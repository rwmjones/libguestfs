/* libguestfs - the guestfsd daemon
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
#include <errno.h>
#include <error.h>
#include <assert.h>

#include "guestfs-internal-all.h"

#include "stringsbuf.h"

void
add_string_nodup (struct stringsbuf *sb, char *str)
{
  char **new_argv;

  if (sb->size >= sb->alloc) {
    sb->alloc += 64;
    new_argv = realloc (sb->argv, sb->alloc * sizeof (char *));
    if (new_argv == NULL)
      error (EXIT_FAILURE, errno, "realloc");
    sb->argv = new_argv;
  }

  sb->argv[sb->size] = str;
  sb->size++;
}

void
add_string (struct stringsbuf *sb, const char *str)
{
  char *new_str = NULL;

  if (str) {
    new_str = strdup (str);
    if (new_str == NULL)
      error (EXIT_FAILURE, errno, "strdup");
  }

  add_string_nodup (sb, new_str);
}

void
add_sprintf (struct stringsbuf *sb, const char *fs, ...)
{
  va_list args;
  char *str;
  int r;

  va_start (args, fs);
  r = vasprintf (&str, fs, args);
  va_end (args);
  if (r == -1)
    error (EXIT_FAILURE, errno, "vasprintf");

  add_string_nodup (sb, str);
}

void
end_stringsbuf (struct stringsbuf *sb)
{
  add_string_nodup (sb, NULL);
}

void
free_stringsbuf (struct stringsbuf *sb)
{
  if (sb->argv != NULL)
    free_stringslen (sb->argv, sb->size);
}

size_t
count_strings (char *const *argv)
{
  size_t argc;

  for (argc = 0; argv[argc] != NULL; ++argc)
    ;
  return argc;
}

static int
compare (const void *vp1, const void *vp2)
{
  char * const *p1 = (char * const *) vp1;
  char * const *p2 = (char * const *) vp2;
  return strcmp (*p1, *p2);
}

void
sort_strings (char **argv, size_t len)
{
  qsort (argv, len, sizeof (char *), compare);
}

void
free_strings (char **argv)
{
  size_t argc;

  if (!argv)
    return;

  for (argc = 0; argv[argc] != NULL; ++argc)
    free (argv[argc]);
  free (argv);
}

void
free_stringslen (char **argv, size_t len)
{
  size_t i;

  if (!argv)
    return;

  for (i = 0; i < len; ++i)
    free (argv[i]);
  free (argv);
}

/* Compare device names (including partition numbers if present).
 * https://rwmj.wordpress.com/2011/01/09/how-are-linux-drives-named-beyond-drive-26-devsdz/
 */
int
compare_device_names (const char *a, const char *b)
{
  size_t alen, blen;
  int r;
  int a_partnum, b_partnum;

  /* Skip /dev/ prefix if present. */
  if (STRPREFIX (a, "/dev/"))
    a += 5;
  if (STRPREFIX (b, "/dev/"))
    b += 5;

  /* Skip sd/hd/ubd/vd. */
  alen = strcspn (a, "d");
  blen = strcspn (b, "d");
  assert (alen > 0 && alen <= 2);
  assert (blen > 0 && blen <= 2);
  a += alen + 1;
  b += blen + 1;

  /* Get device name part, that is, just 'a', 'ab' etc. */
  alen = strcspn (a, "0123456789");
  blen = strcspn (b, "0123456789");

  /* If device name part is longer, it is always greater, eg.
   * "/dev/sdz" < "/dev/sdaa".
   */
  if (alen != blen)
    return alen - blen;

  /* Device name parts are the same length, so do a regular compare. */
  r = strncmp (a, b, alen);
  if (r != 0)
    return r;

  /* Compare partitions numbers. */
  a += alen;
  b += alen;

  /* If no partition numbers, bail -- the devices are the same.  This
   * can happen in one peculiar case: where you have a mix of devices
   * with different interfaces (eg. /dev/sda and /dev/vda).
   * (RHBZ#858128).
   */
  if (!*a && !*b)
    return 0;

  r = sscanf (a, "%d", &a_partnum);
  assert (r == 1);
  r = sscanf (b, "%d", &b_partnum);
  assert (r == 1);

  return a_partnum - b_partnum;
}

static int
compare_device_names_vp (const void *vp1, const void *vp2)
{
  char * const *p1 = (char * const *) vp1;
  char * const *p2 = (char * const *) vp2;
  return compare_device_names (*p1, *p2);
}

void
sort_device_names (char **argv, size_t len)
{
  qsort (argv, len, sizeof (char *), compare_device_names_vp);
}

char *
concat_strings (char *const *argv)
{
  return join_strings ("", argv);
}

char *
join_strings (const char *separator, char *const *argv)
{
  size_t i, len, seplen, rlen;
  char *r;

  seplen = strlen (separator);

  len = 0;
  for (i = 0; argv[i] != NULL; ++i) {
    if (i > 0)
      len += seplen;
    len += strlen (argv[i]);
  }
  len++; /* for final \0 */

  r = malloc (len);
  if (r == NULL)
    error (EXIT_FAILURE, errno, "malloc");

  rlen = 0;
  for (i = 0; argv[i] != NULL; ++i) {
    if (i > 0) {
      memcpy (&r[rlen], separator, seplen);
      rlen += seplen;
    }
    len = strlen (argv[i]);
    memcpy (&r[rlen], argv[i], len);
    rlen += len;
  }
  r[rlen] = '\0';

  return r;
}
