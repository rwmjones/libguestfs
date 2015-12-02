/* libguestfs
 * Copyright (C) 2010-2016 Red Hat Inc.
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

#include "inspection.h"

static char *
safe_strndup (const char *str, size_t len)
{
  char *ret = strndup (str, len);
  if (ret == NULL)
    error (EXIT_FAILURE, errno, "strndup");
  return ret;
}

#if 0
/* Match a regular expression which contains no captures.  Returns
 * true if it matches or false if it doesn't.
 */
int
match (const char *str, const pcre *re)
{
  size_t len = strlen (str);
  int vec[30], r;

  r = pcre_exec (re, NULL, str, len, 0, 0, vec, sizeof vec / sizeof vec[0]);
  if (r == PCRE_ERROR_NOMATCH)
    return 0;

  return 1;
}
#endif

/* Match a regular expression which contains exactly one capture.  If
 * the string matches, return the capture, otherwise return NULL.  The
 * caller must free the result.
 */
char *
match1 (const char *str, const pcre *re)
{
  size_t len = strlen (str);
  int vec[30], r;

  r = pcre_exec (re, NULL, str, len, 0, 0, vec, sizeof vec / sizeof vec[0]);
  if (r == PCRE_ERROR_NOMATCH)
    return NULL;

  return r == 2 ? safe_strndup (&str[vec[2]], vec[3]-vec[2]) : NULL;
}

/* Match a regular expression which contains exactly two captures. */
int
match2 (const char *str, const pcre *re, char **ret1, char **ret2)
{
  size_t len = strlen (str);
  int vec[30], r;

  r = pcre_exec (re, NULL, str, len, 0, 0, vec, 30);
  if (r == PCRE_ERROR_NOMATCH)
    return 0;

  *ret1 = NULL;
  *ret2 = NULL;

  if (r > 1) *ret1 = safe_strndup (&str[vec[2]], vec[3]-vec[2]);
  if (r > 2) *ret2 = safe_strndup (&str[vec[4]], vec[5]-vec[4]);

  return 1;
}

#if 0
/* Match a regular expression which contains exactly three captures. */
int
match3 (const char *str, const pcre *re,
        char **ret1, char **ret2, char **ret3)
{
  size_t len = strlen (str);
  int vec[30], r;

  r = pcre_exec (re, NULL, str, len, 0, 0, vec, 30);
  if (r == PCRE_ERROR_NOMATCH)
    return 0;

  *ret1 = NULL;
  *ret2 = NULL;
  *ret3 = NULL;

  if (r > 1) *ret1 = safe_strndup (&str[vec[2]], vec[3]-vec[2]);
  if (r > 2) *ret2 = safe_strndup (&str[vec[4]], vec[5]-vec[4]);
  if (r > 3) *ret3 = safe_strndup (&str[vec[6]], vec[7]-vec[6]);

  return 1;
}

/* Match a regular expression which contains exactly four captures. */
int
match4 (const char *str, const pcre *re,
        char **ret1, char **ret2, char **ret3, char **ret4)
{
  size_t len = strlen (str);
  int vec[30], r;

  r = pcre_exec (re, NULL, str, len, 0, 0, vec, 30);
  if (r == PCRE_ERROR_NOMATCH)
    return 0;

  *ret1 = NULL;
  *ret2 = NULL;
  *ret3 = NULL;
  *ret4 = NULL;

  if (r > 1) *ret1 = safe_strndup (&str[vec[2]], vec[3]-vec[2]);
  if (r > 2) *ret2 = safe_strndup (&str[vec[4]], vec[5]-vec[4]);
  if (r > 3) *ret3 = safe_strndup (&str[vec[6]], vec[7]-vec[6]);
  if (r > 4) *ret4 = safe_strndup (&str[vec[8]], vec[9]-vec[8]);

  return 1;
}

/* Match a regular expression which contains exactly six captures. */
int
match6 (const char *str, const pcre *re,
        char **ret1, char **ret2, char **ret3, char **ret4,
        char **ret5, char **ret6)
{
  size_t len = strlen (str);
  int vec[30], r;

  r = pcre_exec (re, NULL, str, len, 0, 0, vec, 30);
  if (r == PCRE_ERROR_NOMATCH)
    return 0;

  *ret1 = NULL;
  *ret2 = NULL;
  *ret3 = NULL;
  *ret4 = NULL;
  *ret5 = NULL;
  *ret6 = NULL;

  if (r > 1) *ret1 = safe_strndup (&str[vec[2]], vec[3]-vec[2]);
  if (r > 2) *ret2 = safe_strndup (&str[vec[4]], vec[5]-vec[4]);
  if (r > 3) *ret3 = safe_strndup (&str[vec[6]], vec[7]-vec[6]);
  if (r > 4) *ret4 = safe_strndup (&str[vec[8]], vec[9]-vec[8]);
  if (r > 5) *ret5 = safe_strndup (&str[vec[10]], vec[11]-vec[10]);
  if (r > 6) *ret6 = safe_strndup (&str[vec[12]], vec[13]-vec[12]);

  return 1;
}
#endif
