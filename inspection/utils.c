/* libguestfs
 * Copyright (C) 2016 Red Hat Inc.
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
#include <error.h>
#include <errno.h>

#include <pcre.h>

#include "cleanups.h"
#include "inspection.h"

/* Get the first line of the file, without any trailing newline
 * character.  The caller must free the returned string.
 *
 * If the file is completely empty or begins with '\n' this returns an
 * empty string.
 *
 * This function never returns NULL.  If the file does not exist or
 * there is some other error, it exits with an error.
 */
char *
first_line_of_file (const char *fs, const char *filename)
{
  CLEANUP_FREE char *relative_filename = NULL;
  FILE *fp;
  ssize_t r;
  size_t n = 0;
  char *line = NULL;

  if (get_mount (fs, filename, &relative_filename) == -1)
    error (EXIT_FAILURE, 0, "%s: failed to get mountpoint: %s %s",
           __func__, fs, filename);

  fp = fopen (relative_filename, "r");
  if (fp == NULL)
    error (EXIT_FAILURE, errno, "%s: open: %s", __func__, relative_filename);
  r = getline (&line, &n, fp);
  if (r == -1)
    error (EXIT_FAILURE, errno, "%s: getline: %s", __func__, relative_filename);
  fclose (fp);

  if (r > 0 && line[r-1] == '\n')
    line[r-1] = '\0';

  return line;                  /* caller frees */
}
