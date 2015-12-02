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
#include <string.h>
#include <unistd.h>
#include <error.h>
#include <errno.h>

#include "ignore-value.h"

#include <pcre.h>

#include "guestfs-internal-all.h"
#include "cleanups.h"
#include "inspection.h"

int
get_distro_from_os_release (const char *fs, char **distro)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_version_from_os_release (const char *fs, char **major, char **minor)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_product_name_from_os_release (const char *fs, char **product_name)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_distro_from_lsb_release (const char *fs, char **distro)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_version_from_lsb_release (const char *fs, char **major, char **minor)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_product_name_from_lsb_release (const char *fs, char **product_name)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_version_from_oracle_release (const char *fs, char **major, char **minor)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

COMPILE_REGEXP (re_centos_old, "CentOS.*release (\\d+).*Update (\\d+)", 0)
COMPILE_REGEXP (re_centos, "CentOS.*release (\\d+)\\.(\\d+)", 0)
COMPILE_REGEXP (re_centos_no_minor, "CentOS.*release (\\d+)", 0)

#define CENTOS_RELEASE_FILE "/etc/centos-release"

int
get_version_from_centos_release (const char *fs, char **major, char **minor)
{
  CLEANUP_FREE char *line = first_line_of_file (fs, CENTOS_RELEASE_FILE);

  if (match2 (line, re_centos_old, major, minor) ||
      match2 (line, re_centos, major, minor))
    return 0;

  if ((*major = match1 (line, re_centos_no_minor)) != NULL) {
    *minor = strdup ("0");
    if (*minor == NULL)
      error (EXIT_FAILURE, errno, "strdup");
    return 0;
  }

  fprintf (stderr, "%s: cannot parse major/minor version from file",
           CENTOS_RELEASE_FILE);
  return -1;
}

int
get_version_from_altlinux_release (const char *fs, char **major, char **minor)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
match_redhat_release_fedora (const char *fs)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
match_redhat_release_rhel (const char *fs)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
match_redhat_release_centos (const char *fs)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
match_redhat_release_scientific_linux (const char *fs)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}

int
get_version_from_redhat_release (const char *fs, char **major, char **minor)
{
  error (EXIT_FAILURE, 0, "%s: not implemented", __func__);
  return -1;
}
