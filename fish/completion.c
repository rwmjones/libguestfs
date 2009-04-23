/* libguestfs generated file
 * WARNING: THIS FILE IS GENERATED BY 'src/generator.ml'.
 * ANY CHANGES YOU MAKE TO THIS FILE WILL BE LOST.
 *
 * Copyright (C) 2009 Red Hat Inc.
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
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#endif

#include "fish.h"

#ifdef HAVE_LIBREADLINE

static const char *commands[] = {
  "add",
  "add-cdrom",
  "add-drive",
  "aug-close",
  "aug-defnode",
  "aug-defvar",
  "aug-get",
  "aug-init",
  "aug-insert",
  "aug-load",
  "aug-ls",
  "aug-match",
  "aug-mv",
  "aug-rm",
  "aug-save",
  "aug-set",
  "autosync",
  "blockdev-flushbufs",
  "blockdev-getbsz",
  "blockdev-getro",
  "blockdev-getsize64",
  "blockdev-getss",
  "blockdev-getsz",
  "blockdev-rereadpt",
  "blockdev-setbsz",
  "blockdev-setro",
  "blockdev-setrw",
  "cat",
  "cdrom",
  "checksum",
  "chmod",
  "chown",
  "command",
  "command-lines",
  "config",
  "debug",
  "download",
  "exists",
  "file",
  "get-autosync",
  "get-path",
  "get-qemu",
  "get-state",
  "get-verbose",
  "is-busy",
  "is-config",
  "is-dir",
  "is-file",
  "is-launching",
  "is-ready",
  "kill-subprocess",
  "launch",
  "list-devices",
  "list-partitions",
  "ll",
  "ls",
  "lstat",
  "lvcreate",
  "lvm-remove-all",
  "lvs",
  "lvs-full",
  "mkdir",
  "mkdir-p",
  "mkfs",
  "mount",
  "mount-options",
  "mount-ro",
  "mount-vfs",
  "mounts",
  "path",
  "pvcreate",
  "pvs",
  "pvs-full",
  "qemu",
  "read-lines",
  "rm",
  "rm-rf",
  "rmdir",
  "run",
  "set-autosync",
  "set-path",
  "set-qemu",
  "set-verbose",
  "sfdisk",
  "stat",
  "statvfs",
  "sync",
  "tar-in",
  "tar-out",
  "tgz-in",
  "tgz-out",
  "touch",
  "tune2fs-l",
  "umount",
  "umount-all",
  "unmount",
  "unmount-all",
  "upload",
  "verbose",
  "vgcreate",
  "vgs",
  "vgs-full",
  "write-file",
  NULL
};

static char *
generator (const char *text, int state)
{
  static int index, len;
  const char *name;

  if (!state) {
    index = 0;
    len = strlen (text);
  }

  while ((name = commands[index]) != NULL) {
    index++;
    if (strncasecmp (name, text, len) == 0)
      return strdup (name);
  }

  return NULL;
}

#endif /* HAVE_LIBREADLINE */

char **do_completion (const char *text, int start, int end)
{
  char **matches = NULL;

#ifdef HAVE_LIBREADLINE
  if (start == 0)
    matches = rl_completion_matches (text, generator);
#endif

  return matches;
}
