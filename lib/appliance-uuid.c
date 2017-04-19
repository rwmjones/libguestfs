/* libguestfs
 * Copyright (C) 2010-2017 Red Hat Inc.
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

/**
 * Calculate the root=UUID=... kernel parameter for the appliance.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "guestfs.h"
#include "guestfs-internal.h"

/**
 * Calculate the volume UUID of an ext4 filesystem (in a file).  This
 * is quite simple as it is stored at a known offset.  See
 * L<https://ext4.wiki.kernel.org/index.php/Ext4_Disk_Layout#The_Super_Block>.
 */
char *
guestfs_int_ext4fs_uuid (guestfs_h *g, const char *filename)
{
  int fd;
  unsigned char magic[2];
  unsigned char raw_uuid[16];
  char uuid[37];

  fd = open (filename, O_RDONLY);
  if (fd == -1) {
    perrorf (g, "%s", filename);
    return NULL;
  }

  /* Check it's really an ext4 superblock at offset 0x400. */
  if (pread (fd, magic, sizeof magic, 0x438) != sizeof magic) {
    error (g, "ext4fs: could not read magic from ext4 file: %s", filename);
    close (fd);
    return NULL;
  }
  if (magic[0] != 0x53 || magic[1] != 0xef) {
    error (g, "ext4fs: not an ext4 file: %s", filename);
    close (fd);
    return NULL;
  }

  /* Read the raw bytes of the UUID (128 bits / 8 == 16 bytes). */
  if (pread (fd, raw_uuid, sizeof raw_uuid, 0x468) != sizeof raw_uuid) {
    error (g, "ext4fs: could not read volume UUID from ext4 superblock: %s",
           filename);
    close (fd);
    return NULL;
  }
  close (fd);

  /* Check for sanity. */
  if (is_zero ((char *) raw_uuid, sizeof raw_uuid)) {
    error (g, "ext4fs: UUID is all zeroes: %s", filename);
    return NULL;
  }

  /* Convert to a string UUID.  The same format that blkid produces. */
  snprintf (uuid, sizeof uuid,
            "%02x%02x%02x%02x-"
            "%02x%02x-"
            "%02x%02x-"
            "%02x%02x-"
            "%02x%02x%02x%02x%02x%02x",
            raw_uuid[0], raw_uuid[1], raw_uuid[2], raw_uuid[3],
            raw_uuid[4], raw_uuid[5],
            raw_uuid[6], raw_uuid[7],
            raw_uuid[8], raw_uuid[9],
            raw_uuid[10], raw_uuid[11], raw_uuid[12], raw_uuid[13],
            raw_uuid[14], raw_uuid[15]);

  return safe_strdup (g, uuid); /* caller frees */
}
