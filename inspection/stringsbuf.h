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
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef GUESTFS_INSPECTION_STRINGSBUF_H
#define GUESTFS_INSPECTION_STRINGSBUF_H

/* Growable strings buffer. */
struct stringsbuf {
  char **argv;
  size_t size;
  size_t alloc;
};
#define DECLARE_STRINGSBUF(v) \
  struct stringsbuf (v) = { .argv = NULL, .size = 0, .alloc = 0 }

/* Append a string to the strings buffer.
 *
 * add_string_nodup: don't copy the string.
 * add_string: copy the string.
 * end_stringsbuf: NULL-terminate the buffer.
 */
extern void add_string_nodup (struct stringsbuf *sb, char *str);
extern void add_string (struct stringsbuf *sb, const char *str);
extern void add_sprintf (struct stringsbuf *sb, const char *fs, ...)
  __attribute__((format (printf,2,3)));
extern void end_stringsbuf (struct stringsbuf *sb);
extern void free_stringsbuf (struct stringsbuf *sb);

extern size_t count_strings (char *const *argv);
extern void sort_strings (char **argv, size_t len);
extern void free_strings (char **argv);
extern void free_stringslen (char **argv, size_t len);

extern void sort_device_names (char **argv, size_t len);
extern int compare_device_names (const char *a, const char *b);

/* Concatenate strings, optionally with a separator string between each. */
extern char *concat_strings (char *const *argv);
extern char *join_strings (const char *separator, char *const *argv);

#endif /* GUESTFS_INSPECTION_STRINGSBUF_H */
