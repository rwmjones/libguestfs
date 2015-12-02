/* guestfs-inspection
 * Copyright (C) 2009-2016 Red Hat Inc.
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

#ifndef GUESTFS_INSPECTION_H
#define GUESTFS_INSPECTION_H

#include <pcre.h>

/* detect.c - used by rules */
extern int get_distro_from_os_release (const char *fs, char **distro);
extern int get_version_from_os_release (const char *fs, char **major, char **minor);
extern int get_product_name_from_os_release (const char *fs, char **product_name);
extern int get_distro_from_lsb_release (const char *fs, char **distro);
extern int get_version_from_lsb_release (const char *fs, char **major, char **minor);
extern int get_product_name_from_lsb_release (const char *fs, char **product_name);
extern int get_version_from_oracle_release (const char *fs, char **major, char **minor);
extern int get_version_from_centos_release (const char *fs, char **major, char **minor);
extern int get_version_from_altlinux_release (const char *fs, char **major, char **minor);
extern int match_redhat_release_fedora (const char *fs);
extern int match_redhat_release_rhel (const char *fs);
extern int match_redhat_release_centos (const char *fs);
extern int match_redhat_release_scientific_linux (const char *fs);
extern int get_version_from_redhat_release (const char *fs, char **major, char **minor);

/* match.c */
extern char *match1 (const char *str, const pcre *re);
extern int match2 (const char *str, const pcre *re, char **ret1, char **ret2);

/* mount.c - used by rules */
extern char **get_all_block_devices (void);
extern char **get_all_partitions (void);
extern char **get_all_mddevs (void);
extern char **get_all_lvs (void);
extern char **get_all_ldmvols (void);
extern char **get_all_ldmparts (void);
extern char **get_all_btrfs_subvolumes (const char *fs);
extern char *get_vfs_type (const char *fs);
extern int get_partition_mbr_id (const char *fs);
extern int is_mountable (const char *fs);
extern int get_mount (const char *fs, const char *filename, char **relative_filename);

/* utils.c */
extern char *first_line_of_file (const char *fs, const char *filename);

#endif /* GUESTFS_INSPECTION_H */
