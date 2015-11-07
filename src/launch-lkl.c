/* libguestfs
 * Copyright (C) 2009-2016 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <libintl.h>

#include "cloexec.h"

#include "guestfs.h"
#include "guestfs-internal.h"
#include "guestfs_protocol.h"

/* Per-handle data. */
struct backend_lkl_data {
  pid_t pid;                    /* daemon PID. */
  pid_t recoverypid;            /* Recovery process PID. */
};

static void print_daemon_command_line (guestfs_h *g, char **argv);

/* Test for features which are not supported by the LKL backend.
 * Possibly some of these should just be warnings, not errors.
 */
static bool
lkl_supported (guestfs_h *g)
{
  size_t i;
  struct drive *drv;

  if (g->enable_network) {
    error (g, _("lkl backend does not support networking"));
    return false;
  }
  if (g->smp > 1) {
    error (g, _("lkl backend does not support SMP"));
    return false;
  }

  ITER_DRIVES (g, i, drv) {
    if (drv->src.protocol != drive_protocol_file) {
      error (g, _("lkl backend does not support remote drives"));
      return false;
    }
    if (drv->src.format && STRNEQ (drv->src.format, "raw")) {
      error (g, _("lkl backend does not support non-raw-format drives"));
      return false;
    }
    if (drv->iface) {
      error (g,
             _("lkl backend does not support drives with 'iface' parameter"));
      return false;
    }
    if (drv->disk_label) {
      error (g,
             _("lkl backend does not support drives with 'label' parameter"));
      return false;
    }
    /* Note that discard == "besteffort" is fine. */
    if (drv->discard == discard_enable) {
      error (g,
             _("lkl backend does not support drives with 'discard' parameter set to 'enable'"));
      return false;
    }
  }

  return true;
}

static int
launch_lkl (guestfs_h *g, void *datav, const char *arg)
{
  struct backend_lkl_data *data = datav;
  CLEANUP_FREE_STRINGSBUF DECLARE_STRINGSBUF (cmdline);
  int console_sock = -1, daemon_sock = -1;
  int r;
  int csv[2], dsv[2];
  uint32_t size;
  CLEANUP_FREE void *buf = NULL;
  struct drive *drv;
  size_t i;

  if (!lkl_supported (g))
    return -1;

  if (!g->nr_drives) {
    error (g, _("you must call guestfs_add_drive before guestfs_launch"));
    return -1;
  }

  /* The socket that the daemon will talk to us on. */
  if (socketpair (AF_LOCAL, SOCK_STREAM|SOCK_CLOEXEC, 0, dsv) == -1) {
    perrorf (g, "socketpair");
    goto cleanup0;
  }

  /* The console socket. */
  if (!g->direct_mode) {
    if (socketpair (AF_LOCAL, SOCK_STREAM|SOCK_CLOEXEC, 0, csv) == -1) {
      perrorf (g, "socketpair");
      close (dsv[0]);
      close (dsv[1]);
      goto cleanup0;
    }
  }

  /* Construct the guestfsd-lkl command line.  We have to do this
   * before forking, because after fork we are not allowed to use
   * non-signal-safe functions such as malloc.
   */
#define ADD_CMDLINE(str)			\
  guestfs_int_add_string (g, &cmdline, (str))
#define ADD_CMDLINE_PRINTF(fs,...)				\
  guestfs_int_add_sprintf (g, &cmdline, (fs), ##__VA_ARGS__)

  ADD_CMDLINE (g->hv);

  if (g->verbose)
    ADD_CMDLINE ("-v");

  /* Must use standlone mode. */
  ADD_CMDLINE ("-r");

#if 0 /* XXX Implement a --lkl-memsize flag in the daemon. */
  /* Set memory size. */
  ADD_CMDLINE_PRINTF ("--lkl-memsize=%dM", g->memsize);
#endif

#if 0 /* XXX Implement a --lkl--append flag in the daemon. */
  if (g->append)
    ADD_CMDLINE_PRINTF ("--lkl-append=%s", g->append);
#endif

  /* Add the drives. */
  ITER_DRIVES (g, i, drv) {
    /* XXX This currently ignores all the per-drive flags.  We should
     * pass the ones which make sense through to the daemon,
     * esp. readonly.
     */
    /* if (!drv->overlay) ADD_CMDLINE (drv->overlay); else ... */
    ADD_CMDLINE (drv->src.u.path);
  }

  /* Create the daemon socket. */
  ADD_CMDLINE_PRINTF ("--channel=fd:%d", dsv[1]);

  /* XXX Pass console socket to daemon. */

  /* Finish off the command line. */
  guestfs_int_end_stringsbuf (g, &cmdline);

  r = fork ();
  if (r == -1) {
    perrorf (g, "fork");
    if (!g->direct_mode) {
      close (csv[0]);
      close (csv[1]);
    }
    close (dsv[0]);
    close (dsv[1]);
    goto cleanup0;
  }

  if (r == 0) {                 /* Child (guestfsd-lkl). */
    /* Set up the daemon socket for the child. */
    close (dsv[0]);
    set_cloexec_flag (dsv[1], 0); /* so it doesn't close across exec */

    if (!g->direct_mode) {
      /* Set up stdin, stdout, stderr. */
      close (0);
      close (1);
      close (csv[0]);

      /* We set the FD_CLOEXEC flag on the socket above, but now (in
       * the child) it's safe to unset this flag so daemon can use the
       * socket.
       */
      set_cloexec_flag (csv[1], 0);

      /* Stdin. */
      if (dup (csv[1]) == -1) {
      dup_failed:
        perror ("dup failed");
        _exit (EXIT_FAILURE);
      }
      /* Stdout. */
      if (dup (csv[1]) == -1)
        goto dup_failed;

      /* Send stderr to the pipe as well. */
      close (2);
      if (dup (csv[1]) == -1)
        goto dup_failed;

      close (csv[1]);

      /* RHBZ#1123007 */
      close_file_descriptors (fd > 2 && fd != dsv[1]);
    }

    /* Dump the command line (after setting up stderr above). */
    if (g->verbose)
      print_daemon_command_line (g, cmdline.argv);

    /* Put daemon in a new process group. */
    if (g->pgroup)
      setpgid (0, 0);

    setenv ("LC_ALL", "C", 1);

    execv (g->hv, cmdline.argv); /* Run daemon. */
    perror (g->hv);
    _exit (EXIT_FAILURE);
  }

  /* Parent (library). */
  data->pid = r;

  /* Fork the recovery process off which will kill the daemon if the
   * parent process fails to do so (eg. if the parent segfaults).
   */
  data->recoverypid = -1;
  if (g->recovery_proc) {
    r = fork ();
    if (r == 0) {
      int i;
      struct sigaction sa;
      pid_t daemon_pid = data->pid;
      pid_t parent_pid = getppid ();

      /* Remove all signal handlers.  See the justification here:
       * https://www.redhat.com/archives/libvir-list/2008-August/msg00303.html
       * We don't mask signal handlers yet, so this isn't completely
       * race-free, but better than not doing it at all.
       */
      memset (&sa, 0, sizeof sa);
      sa.sa_handler = SIG_DFL;
      sa.sa_flags = 0;
      sigemptyset (&sa.sa_mask);
      for (i = 1; i < NSIG; ++i)
        sigaction (i, &sa, NULL);

      /* Close all other file descriptors.  This ensures that we don't
       * hold open (eg) pipes from the parent process.
       */
      close_file_descriptors (1);

      /* It would be nice to be able to put this in the same process
       * group as daemon (ie. setpgid (0, daemon_pid)).  However
       * this is not possible because we don't have any guarantee here
       * that the daemon process has started yet.
       */
      if (g->pgroup)
        setpgid (0, 0);

      /* Writing to argv is hideously complicated and error prone.  See:
       * http://git.postgresql.org/gitweb/?p=postgresql.git;a=blob;f=src/backend/utils/misc/ps_status.c;hb=HEAD
       */

      /* Loop around waiting for one or both of the other processes to
       * disappear.  It's fair to say this is very hairy.  The PIDs that
       * we are looking at might be reused by another process.  We are
       * effectively polling.  Is the cure worse than the disease?
       */
      for (;;) {
        if (kill (daemon_pid, 0) == -1)
          /* daemon's gone away, we aren't needed */
          _exit (EXIT_SUCCESS);
        if (kill (parent_pid, 0) == -1) {
          /* Parent's gone away, daemon still around, so kill daemon. */
          kill (data->pid, SIGKILL);
          _exit (EXIT_SUCCESS);
        }
        sleep (2);
      }
    }

    /* Don't worry, if the fork failed, this will be -1.  The recovery
     * process isn't essential.
     */
    data->recoverypid = r;
  }

  if (!g->direct_mode) {
    /* Close the other end of the console socketpair. */
    close (csv[1]);

    console_sock = csv[0];      /* stdin of child */
    csv[0] = -1;
  }

  daemon_sock = dsv[0];
  close (dsv[1]);
  dsv[0] = -1;

  g->state = LAUNCHING;

  /* Wait for daemon to start and to connect back to us via
   * virtio-serial and send the GUESTFS_LAUNCH_FLAG message.
   */
  g->conn =
    guestfs_int_new_conn_socket_connected (g, daemon_sock, console_sock);
  if (!g->conn)
    goto cleanup1;

  /* g->conn now owns these sockets. */
  daemon_sock = console_sock = -1;

  /* We now have to wait for daemon to start up, the daemon to start
   * running, and for it to send the GUESTFS_LAUNCH_FLAG to us.
   */
  r = guestfs_int_recv_from_daemon (g, &size, &buf);

  if (r == -1) {
    guestfs_int_launch_failed_error (g);
    goto cleanup1;
  }

  if (size != GUESTFS_LAUNCH_FLAG) {
    guestfs_int_launch_failed_error (g);
    goto cleanup1;
  }

  debug (g, "appliance is up");

  /* This is possible in some really strange situations, such as
   * guestfsd starts up OK but then daemon immediately exits.  Check
   * for it because the caller is probably expecting to be able to
   * send commands after this function returns.
   */
  if (g->state != READY) {
    error (g, _("daemon launched and contacted daemon, but state != READY"));
    goto cleanup1;
  }

  return 0;

 cleanup1:
  if (!g->direct_mode && csv[0] >= 0)
    close (csv[0]);
  if (dsv[0] >= 0)
    close (dsv[0]);
  if (data->pid > 0) kill (data->pid, SIGKILL);
  if (data->recoverypid > 0) kill (data->recoverypid, SIGKILL);
  if (data->pid > 0) waitpid (data->pid, NULL, 0);
  if (data->recoverypid > 0) waitpid (data->recoverypid, NULL, 0);
  data->pid = 0;
  data->recoverypid = 0;
  memset (&g->launch_t, 0, sizeof g->launch_t);

 cleanup0:
  if (daemon_sock >= 0)
    close (daemon_sock);
  if (console_sock >= 0)
    close (console_sock);
  if (g->conn) {
    g->conn->ops->free_connection (g, g->conn);
    g->conn = NULL;
  }
  g->state = CONFIG;
  return -1;
}

/* This is called from the forked subprocess just before the daemon
 * runs, so it can just print the message straight to stderr, where it
 * will be picked up and funnelled through the usual appliance event
 * API.
 */
static void
print_daemon_command_line (guestfs_h *g, char **argv)
{
  size_t i = 0;
  int needs_quote;

  struct timeval tv;
  gettimeofday (&tv, NULL);
  fprintf (stderr, "[%05" PRIi64 "ms] ",
           guestfs_int_timeval_diff (&g->launch_t, &tv));

  while (argv[i]) {
    if (i > 0) fputc (' ', stderr);

    /* Does it need shell quoting?  This only deals with simple cases. */
    needs_quote = strcspn (argv[i], " ") != strlen (argv[i]);

    if (needs_quote) fputc ('\'', stderr);
    fprintf (stderr, "%s", argv[i]);
    if (needs_quote) fputc ('\'', stderr);
    i++;
  }

  fputc ('\n', stderr);
}

static int
shutdown_lkl (guestfs_h *g, void *datav, int check_for_errors)
{
  struct backend_lkl_data *data = datav;
  int ret = 0;
  int status;

  if (data->pid > 0) {
    /* Tell the daemon to exit cleanly. */
    guestfs_internal_exit (g);

#if 0
    /* Signal daemon to shutdown.  */
    debug (g, "sending SIGTERM to process %d", data->pid);
    kill (data->pid, SIGTERM);
#endif
  }

  /* Kill the recovery process. */
  if (data->recoverypid > 0) kill (data->recoverypid, 9);

  /* Wait for subprocess(es) to exit. */
  if (data->pid > 0) {
    if (waitpid (data->pid, &status, 0) == -1) {
      perrorf (g, "waitpid (daemon)");
      ret = -1;
    }
    /* Note it's normal for the pre-3.11 daemon process to exit with
     * status "killed by signal 15" (where 15 == SIGTERM).  Post 3.11
     * the exit status can normally be 1.
     *
     * So don't consider those to be an error.
     */
    else if (!WIFEXITED (status) || WEXITSTATUS (status) != 0) {
      guestfs_int_external_command_failed (g, status, g->hv, NULL);
      ret = -1;
    }
  }
  if (data->recoverypid > 0) waitpid (data->recoverypid, NULL, 0);

  data->pid = data->recoverypid = 0;

  return ret;
}

static int
get_pid_lkl (guestfs_h *g, void *datav)
{
  struct backend_lkl_data *data = datav;

  if (data->pid > 0)
    return data->pid;
  else {
    error (g, "get_pid: no daemon subprocess");
    return -1;
  }
}

/* XXX Unknown.  It's unlikely to be unlimited, as presumably disks
 * still consume minor numbers in the linked kernel.
 */
static int
max_disks_lkl (guestfs_h *g, void *datav)
{
  return 15;
}

static struct backend_ops backend_lkl_ops = {
  .data_size = sizeof (struct backend_lkl_data),
  /* .create_cow_overlay_lkl = ??? */
  .launch = launch_lkl,
  .shutdown = shutdown_lkl,
  .get_pid = get_pid_lkl,
  .max_disks = max_disks_lkl,
};

void
guestfs_int_init_lkl_backend (void)
{
  guestfs_int_register_backend ("lkl", &backend_lkl_ops);
}
