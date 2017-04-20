/* libguestfs - the guestfsd daemon
 * Copyright (C) 2012-2017 Red Hat Inc.
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
#include <string.h>

#include "c-ctype.h"

#include "daemon.h"
#include "actions.h"

/* Disk labels are stored in a simple array, indexed by the drive
 * number.  NULL means no label.  If the array is shorter than the
 * number of disks, then that also means no label.
 *
 * When looking up a disk label, we have to do a linear scan.  Most
 * guests will only have one or two disks.
 */
static char **disk_labels = NULL;
static size_t dl_alloc = 0;

/* This is just meant to limit the size of the above array to
 * something not insane, and deal with overflows in the index.
 */
#define MAX_INDEX 1000000

int
do_internal_set_disk_label (int index, const char *label)
{
  size_t i, len;
  char *p;

  if (index < 0 || index > MAX_INDEX) {
    reply_with_error ("index out of range");
    return -1;
  }

  /* This was checked in the library, and this is an internal call so
   * it "cannot" be wrong, but check it again to be on the safe side.
   */
  len = strlen (label);
  if (len == 0 || len > 20) {
  invalid_disk_label:
    reply_with_error ("invalid disk label");
    return -1;
  }
  for (i = 0; i < len; ++i) {
    if (!c_isalpha (label[i]))
      goto invalid_disk_label;
  }

  /* Extend the array if necessary. */
  if ((size_t) index >= dl_alloc) {
    char **pp;

    pp = realloc (disk_labels, (index + 1) * sizeof (char *));
    if (pp == NULL) {
      reply_with_perror ("realloc");
      return -1;
    }
    disk_labels = pp;
    while (dl_alloc < (size_t)index+1)
      disk_labels[dl_alloc++] = NULL;
    dl_alloc = index+1;
  }

  p = strdup (label);
  if (p == NULL) {
    reply_with_perror ("strdup");
    return -1;
  }
  free (disk_labels[index]);
  disk_labels[index] = p;

  return 0;
}

int
do_internal_clear_disk_label (int index)
{
  if (index < 0 || index > MAX_INDEX) {
    reply_with_error ("index out of range");
    return -1;
  }

  if ((size_t) index < dl_alloc) {
    free (disk_labels[index]);
    disk_labels[index] = NULL;
  }

  return 0;
}

/* The API is (unfortunately) defined to return both devices and
 * partitions, because that was simpler with the previous (udev-based)
 * implementation.  If it just had to return devices, then we could
 * iterate over the disk_labels array and be done.
 */
char **
do_list_disk_labels (void)
{
  CLEANUP_FREE_STRINGSBUF DECLARE_STRINGSBUF (ret);
  size_t i;

  for (i = 0; i < dl_alloc; ++i) {
    if (disk_labels[i] != NULL) {
      char dev[64] = "/dev/sd";

      if (add_string (&ret, disk_labels[i]) == -1)
        return NULL;

      drive_name (i, &dev[7]);
      if (add_string (&ret, dev) == -1)
        return NULL;
    }
  }

  if (end_stringsbuf (&ret) == -1)
    return NULL;

  return take_stringsbuf (&ret);              /* caller frees */
}

ssize_t
find_disk_label (const char *label)
{
  size_t i;

  for (i = 0; i < dl_alloc; ++i)
    if (disk_labels[i] && STREQ (disk_labels[i], label))
      return i;

  return -1;                    /* Not found. */
}
