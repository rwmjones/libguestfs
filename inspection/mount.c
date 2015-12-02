/* guestfs-inspection
 * Copyright (C) 2009-2015 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <errno.h>
#include <error.h>

#include "c-ctype.h"

#include "guestfs-internal-all.h"

#include "inspection.h"
#include "cleanups.h"
#include "command.h"
#include "stringsbuf.h"

/* If root device is an ext2 filesystem, this is the major and minor.
 * This is so we can ignore this device from the point of view of the
 * user, eg. in guestfs_list_devices and many other places.
 */
static dev_t root_device = 0;

/* A temporary directory where we place all the mountpoints. */
static char mountpoints[] = "/tmp/mp.XXXXXX";

static void init_mount (void) __attribute__((constructor));

static void
init_mount (void)
{
  struct stat statbuf;

  if (stat ("/", &statbuf) == 0)
    root_device = statbuf.st_dev;

  if (mkdtemp (mountpoints) == NULL)
    perror ("mkdtemp");
}

static void free_mount (void) __attribute__((destructor));

static void
free_mount (void)
{
  DIR *dir;
  struct dirent *d;

  /* Unmount all mountpoints in the temporary directory, then
   * delete those directories and the parent.
   */
  dir = opendir (mountpoints);
  if (!dir) {
    perror (mountpoints);
    return;
  }

  for (;;) {
    errno = 0;
    d = readdir (dir);
    if (!d) break;

    if (d->d_name[0] != '.') {
      CLEANUP_FREE char *mp;

      if (asprintf (&mp, "%s/%s", mountpoints, d->d_name) == -1) {
        perror ("asprintf");
        continue;
      }

      if (umount2 (mp, MNT_DETACH) == -1) /* lazy umount */
        perror (mp);

      if (rmdir (mp) == -1)
        perror (mp);
    }
  }

  /* Check readdir didn't fail */
  if (errno != 0) {
    perror ("readdir");
    return;
  }

  /* Close the directory handle */
  if (closedir (dir) == -1) {
    perror ("closedir");
    return;
  }

  if (rmdir (mountpoints) == -1)
    perror (mountpoints);
}

/* Return true iff device is the root device (and therefore should be
 * ignored from the point of view of user calls).
 */
static int
is_root_device_stat (struct stat *statbuf)
{
  if (statbuf->st_rdev == root_device) return 1;
  return 0;
}

static int
is_root_device (const char *device)
{
  struct stat statbuf;

  if (stat (device, &statbuf) == -1) {
    perror (device);
    return 0;
  }

  return is_root_device_stat (&statbuf);
}

typedef int (*block_dev_func_t) (const char *dev, struct stringsbuf *r);

/* Execute a given function for each discovered block device */
static char **
foreach_block_device (block_dev_func_t func)
{
  DECLARE_STRINGSBUF (r);
  DIR *dir;
  struct dirent *d;
  char dev_path[256];
  int fd;
  bool err = false;

  dir = opendir ("/sys/block");
  if (!dir) {
    perror ("opendir: /sys/block");
    return NULL;
  }

  for (;;) {
    errno = 0;
    d = readdir (dir);
    if (!d) break;

    if (STREQLEN (d->d_name, "sd", 2) ||
        STREQLEN (d->d_name, "hd", 2) ||
        STREQLEN (d->d_name, "ubd", 3) ||
        STREQLEN (d->d_name, "vd", 2) ||
        STREQLEN (d->d_name, "sr", 2)) {
      snprintf (dev_path, sizeof dev_path, "/dev/%s", d->d_name);

      /* Ignore the root device. */
      if (is_root_device (dev_path))
        continue;

      /* RHBZ#514505: Some versions of qemu <= 0.10 add a
       * CD-ROM device even though we didn't request it.  Try to
       * detect this by seeing if the device contains media.
       */
      fd = open (dev_path, O_RDONLY|O_CLOEXEC);
      if (fd == -1) {
        perror (dev_path);
        continue;
      }
      close (fd);

      /* Call the map function for this device */
      if ((*func)(d->d_name, &r) != 0) {
        err = true;
        break;
      }
    }
  }

  /* Check readdir didn't fail */
  if (errno != 0) {
    perror ("readdir: /sys/block");
    free_stringslen (r.argv, r.size);
    closedir (dir);
    return NULL;
  }

  /* Close the directory handle */
  if (closedir (dir) == -1) {
    perror ("closedir: /sys/block");
    free_stringslen (r.argv, r.size);
    return NULL;
  }

  /* Free the result list on error */
  if (err) {
    free_stringslen (r.argv, r.size);
    return NULL;
  }

  /* Sort the devices. */
  if (r.size > 0)
    sort_device_names (r.argv, r.size);

  /* NULL terminate the list */
  end_stringsbuf (&r);

  return r.argv;
}

/* Add a device to the list of devices */
static int
add_device (const char *device, struct stringsbuf *r)
{
  char dev_path[256];
  snprintf (dev_path, sizeof dev_path, "/dev/%s", device);

  add_string (r, dev_path);

  return 0;
}

char **
get_all_block_devices (void)
{
  return foreach_block_device (add_device);
}

static int
add_partitions (const char *device, struct stringsbuf *r)
{
  char devdir[256];

  /* Open the device's directory under /sys/block */
  snprintf (devdir, sizeof devdir, "/sys/block/%s", device);

  DIR *dir = opendir (devdir);
  if (!dir) {
    perror (devdir);
    free_stringslen (r->argv, r->size);
    return -1;
  }

  /* Look in /sys/block/<device>/ for entries starting with <device>
   * e.g. /sys/block/sda/sda1
   */
  errno = 0;
  struct dirent *d;
  while ((d = readdir (dir)) != NULL) {
    if (STREQLEN (d->d_name, device, strlen (device))) {
      char part[256];
      snprintf (part, sizeof part, "/dev/%s", d->d_name);

      add_string (r, part);
    }
  }

  /* Check if readdir failed */
  if (0 != errno) {
    perror (devdir);
    free_stringslen (r->argv, r->size);
    closedir (dir);
    return -1;
  }

  /* Close the directory handle */
  if (closedir (dir) == -1) {
    perror (device);
    free_stringslen (r->argv, r->size);
    return -1;
  }

  return 0;
}

char **
get_all_partitions (void)
{
  return foreach_block_device (add_partitions);
}

char **
get_all_mddevs (void)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return NULL;
}

char **
get_all_lvs (void)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return NULL;
}

char **
get_all_ldmvols (void)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return NULL;
}

char **
get_all_ldmparts (void)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return NULL;
}

char **
get_all_btrfs_subvolumes (const char *fs)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return NULL;
}

static char *
get_blkid_tag (const char *device, const char *tag)
{
  char *out;
  CLEANUP_FREE char *err = NULL;
  int r;
  size_t len;

  r = commandr (&out, &err,
                "blkid",
                /* Adding -c option kills all caching, even on RHEL 5. */
                "-c", "/dev/null",
                "-o", "value", "-s", tag, device, NULL);
  if (r != 0 && r != 2) {
    if (r >= 0)
      fprintf (stderr, "%s: %s (blkid returned %d)\n", device, err, r);
    else
      fprintf (stderr, "%s: %s\n", device, err);
    free (out);
    return NULL;
  }

  if (r == 2) {                 /* means UUID etc not found */
    free (out);
    out = strdup ("");
    if (out == NULL)
      perror ("strdup");
    return out;
  }

  /* Trim trailing \n if present. */
  len = strlen (out);
  if (len > 0 && out[len-1] == '\n')
    out[len-1] = '\0';

  return out;                   /* caller frees */
}

char *
get_vfs_type (const char *fs)
{
  return get_blkid_tag (fs, "TYPE");
}

/* Test if sfdisk is recent enough to have --part-type, to be used instead
 * of --print-id and --change-id.
 */
static int
test_sfdisk_has_part_type (void)
{
  static int tested = -1;
  int r;
  CLEANUP_FREE char *out = NULL, *err = NULL;

  if (tested != -1)
    return tested;

  r = command (&out, &err, "sfdisk", "--help", NULL);
  if (r == -1) {
    fprintf (stderr, "%s: %s\n", "sfdisk --help", err);
    return -1;
  }

  tested = strstr (out, "--part-type") != NULL;
  return tested;
}

static char *
part_to_dev (const char *part)
{
  int err = 1;
  size_t n = strlen (part);
  char *r;

  while (n >= 1 && c_isdigit (part[n-1])) {
    err = 0;
    n--;
  }

  if (err) {
    fprintf (stderr, "device name is not a partition\n");
    return NULL;
  }

  r = strndup (part, n);
  if (r == NULL) {
    perror ("strdup");
    return NULL;
  }

  return r;
}

static int
part_to_partnum (const char *part)
{
  int err = 1;
  size_t n = strlen (part);
  int r;

  while (n >= 1 && c_isdigit (part[n-1])) {
    err = 0;
    n--;
  }

  if (err) {
    fprintf (stderr, "device name is not a partition\n");
    return -1;
  }

  if (sscanf (&part[n], "%d", &r) != 1) {
    fprintf (stderr, "could not parse number\n");
    return -1;
  }

  return r;
}

int
get_partition_mbr_id (const char *fs)
{
  CLEANUP_FREE char *device;
  int partnum;
  char partnum_str[16];
  const char *param =
    test_sfdisk_has_part_type () ? "--part-type" : "--print-id";
  CLEANUP_FREE char *out = NULL, *err = NULL;
  int r;
  unsigned id;

  /* Get the block device and partition number from the filesystem
   * string.
   */
  device = part_to_dev (fs);
  if (device == NULL)
    return -1;
  partnum = part_to_partnum (fs);
  if (partnum == -1)
    return -1;
  snprintf (partnum_str, sizeof partnum_str, "%d", partnum);

  r = command (&out, &err, "sfdisk", param, device, partnum_str, NULL);
  if (r == -1) {
    fprintf (stderr, "sfdisk %s: %s\n", param, err);
    return -1;
  }

  /* It's printed in hex ... */
  if (sscanf (out, "%x", &id) != 1) {
    fprintf (stderr, "sfdisk --print-id: cannot parse output: %s\n", out);
    return -1;
  }

  return id;
}

/* When mounting filesystems, we place them in temporary directories
 * under 'mountpoints'.  We name the temporary directory after the
 * device name, but since device names contain '/' characters, we have
 * to mangle the name.
 */
static char *
get_mount_name (const char *fs)
{
  char *ret;
  size_t i;

  if (asprintf (&ret, "%s/%s", mountpoints, fs) == -1) {
    perror ("asprintf");
    return NULL;
  }

  for (i = strlen (mountpoints) + 1; i < strlen (ret); ++i) {
    if (ret[i] == '/')
      ret[i] = '_';
  }

  return ret;                   /* caller frees */
}

int
is_mountable (const char *fs)
{
  CLEANUP_FREE char *mp = NULL;
  struct stat statbuf;
  int r;
  CLEANUP_FREE char *err = NULL;

  mp = get_mount_name (fs);
  if (mp == NULL)
    return -1;

  if (stat (mp, &statbuf) == 0 && S_ISDIR (statbuf.st_mode))
    return 1;                   /* mountable, and mounted already */

  /* Try to create the mountpoint. */
  if (mkdir (mp, 0700) == -1) {
    perror (mp);
    return -1;
  }

  /* Try to mount the filesystem. */
  r = command (NULL, &err,
               "mount", "-o", "ro", fs, mp, NULL);
  if (r == -1) {
    fprintf (stderr, "mount: %s: %s\n", fs, err);

    /* Now hack things for the *BSDs. */
    /* FreeBSD fs is a variant of ufs called ufs2 ... */
    free (err); err = NULL;
    r = command (NULL, &err,
                 "mount", "-o", "ro,ufstype=ufs2", fs, mp, NULL);
    if (r == -1) {
      fprintf (stderr, "mount [ufs2]: %s: %s\n", fs, err);

      /* while NetBSD and OpenBSD use another variant labeled 44bsd */
      free (err); err = NULL;
      r = command (NULL, &err,
                   "mount", "-o", "ro,ufstype=44bsd", fs, mp, NULL);
      if (r == -1) {
        fprintf (stderr, "mount [44bsd]: %s: %s\n", fs, err);

        /* Mount failed, so remove the mountpoint. */
        rmdir (mp);
        return 0;
      }
    }
  }

  /* Mount succeeded. */
  return 1;
}

int
get_mount (const char *fs, const char *filename, char **relative_filename)
{
  CLEANUP_FREE char *mp = NULL;
  int r;

  if (filename[0] != '/') {
    fprintf (stderr, "get_mount: filename is not an absolute path: %s\n",
             filename);
    return -1;
  }

  r = is_mountable (fs);
  if (r == -1)
    return -1;
  if (r == 0) {
    fprintf (stderr, "get_mount: called on non-mountable filesystem: %s\n",
             fs);
    return -1;
  }

  mp = get_mount_name (fs);

  /* Construct the filename relative to the mountpoint. */
  if (asprintf (relative_filename, "%s%s", mp, filename) == -1) {
    perror ("asprintf");
    return -1;
  }

  return 0;
}
