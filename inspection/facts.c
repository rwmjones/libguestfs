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

/* Handle the true and false facts sets, and other helper functions. */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <error.h>
#include <errno.h>
#include <alloca.h>

#include "gl_oset.h"
#include "gl_xoset.h"
#include "gl_array_oset.h"

#include "rules.h"

/* True and false facts sets.  See guestfs-inspection(8)/WRITING RULES
 * to understand why these are used.  The fact structs themselves are
 * identical, we only know that a fact is false or true based on which
 * list it appears on.
 */
static gl_oset_t true_facts = NULL;
static gl_oset_t false_facts = NULL;

static int
compare_facts (const void *vf1, const void *vf2)
{
  size_t i;
  int r;
  const fact *f1 = vf1;
  const fact *f2 = vf2;

  r = strcmp (f1->term_name, f2->term_name);
  if (r != 0) return r;

  /* If term names are equal, they are supposed to have the same
   * number of arguments.  We type-checked that when reading the
   * source.  However, better check.
   */
  assert (f1->nr_term_args == f2->nr_term_args);

  for (i = 0; i < f1->nr_term_args; ++i) {
    r = strcmp (f1->term_arg[i], f2->term_arg[i]);
    if (r != 0) return r;
  }

  return 0;
}

static void
free_fact (const void *vf)
{
  /* Why is the parameter const?
   * See Bruno Haible's explanation here:
   * https://www.mail-archive.com/bug-gnulib@gnu.org/msg08619.html
   */
  fact *f = (fact *) vf;
  size_t i;

  free (f->term_name);
  for (i = 0; i < f->nr_term_args; ++i)
    free (f->term_arg[i]);
  free (f);
}

static void init_facts (void) __attribute__((constructor));

static void
init_facts (void)
{
  true_facts = gl_oset_create_empty (GL_ARRAY_OSET, compare_facts, free_fact);
  false_facts = gl_oset_create_empty (GL_ARRAY_OSET, compare_facts, free_fact);
}

static void free_facts (void) __attribute__((destructor));

static void
free_facts (void)
{
  gl_oset_free (true_facts);
  gl_oset_free (false_facts);
}

static void
clear_set (gl_oset_t set)
{
  const void *f;
  gl_oset_iterator_t iter = gl_oset_iterator (set);

  while (gl_oset_iterator_next (&iter, &f)) {
    gl_oset_remove (set, (void *) f);
  }
  gl_oset_iterator_free (&iter);
}

void
clear_true_facts (void)
{
  clear_set (true_facts);
}

void
clear_false_facts (void)
{
  clear_set (false_facts);
}

size_t
count_true_facts (void)
{
  return gl_oset_size (true_facts);
}

/* This is just for debugging facts. */
void
print_fact (bool is_true, const fact *f, FILE *fp)
{
  size_t i;

  if (!is_true)
    fputc ('!', fp);
  fputs (f->term_name, fp);
  if (f->nr_term_args > 0)
    fputc ('(', fp);
  for (i = 0; i < f->nr_term_args; ++i) {
    fputc ('"', fp);
    fputs (f->term_arg[i], fp);
    fputc ('"', fp);
    if (i+1 != f->nr_term_args)
      fputs (", ", fp);
  }
  if (f->nr_term_args > 0)
    fputc (')', fp);
}

static void
print_set (bool is_true, gl_oset_t set)
{
  const void *vf;
  gl_oset_iterator_t iter = gl_oset_iterator (set);

  while (gl_oset_iterator_next (&iter, &vf)) {
    fact *f = (fact *) vf;
    print_fact (is_true, f, stdout);
    printf ("\n");
  }
  gl_oset_iterator_free (&iter);
}

void
print_true_facts (void)
{
  print_set (true, true_facts);
}

void
print_false_facts (void)
{
  print_set (false, false_facts);
}

/* Look for every string parameter of every fact we know about, and
 * add all those strings to the set.
 */
void
add_all_fact_strings (gl_oset_t set)
{
  const void *vf;
  fact *f;
  gl_oset_iterator_t iter;
  size_t i;

  iter = gl_oset_iterator (true_facts);
  while (gl_oset_iterator_next (&iter, &vf)) {
    f = (fact *) vf;
    for (i = 0; i < f->nr_term_args; ++i)
      gl_oset_add (set, f->term_arg[i]);
  }
  gl_oset_iterator_free (&iter);

  iter = gl_oset_iterator (false_facts);
  while (gl_oset_iterator_next (&iter, &vf)) {
    f = (fact *) vf;
    for (i = 0; i < f->nr_term_args; ++i)
      gl_oset_add (set, f->term_arg[i]);
  }
  gl_oset_iterator_free (&iter);
}

/* Look for every string parameter in the specific argument position
 * of the specific term name (both true and false), and add those
 * strings only to the set.
 */
void
add_strings_from_facts (gl_oset_t set, const char *term_name, size_t arg_i)
{
  const void *vf;
  fact *f;
  gl_oset_iterator_t iter;

  iter = gl_oset_iterator (true_facts);
  while (gl_oset_iterator_next (&iter, &vf)) {
    f = (fact *) vf;
    if (strcmp (f->term_name, term_name) == 0)
      gl_oset_add (set, f->term_arg[arg_i]);
  }
  gl_oset_iterator_free (&iter);

  iter = gl_oset_iterator (false_facts);
  while (gl_oset_iterator_next (&iter, &vf)) {
    f = (fact *) vf;
    if (strcmp (f->term_name, term_name) == 0)
      gl_oset_add (set, f->term_arg[arg_i]);
  }
  gl_oset_iterator_free (&iter);
}

/* NB: This does not make a deep copy of the strings.  However before
 * we add the fact to the true_facts or false_facts arrays, we do call
 * deep_copy to copy the strings.
 */
fact *
create_fact (const char *term_name, size_t n, ...)
{
  fact *f;
  size_t i;
  va_list args;

  f = malloc (sizeof (*f) + n * sizeof (char *));
  if (f == NULL)
    error (EXIT_FAILURE, errno, "malloc");
  va_start (args, term_name);
  for (i = 0; i < n; ++i) {
    const char *p = va_arg (args, const char *);
    f->term_arg[i] = (char *) p;
  }
  va_end (args);
  f->term_name = (char *) term_name;
  f->nr_term_args = n;
  return f;                     /* caller must free only the struct */
}

static fact *
deep_copy_fact (const fact *f)
{
  fact *ret;
  size_t i;

  ret = malloc (sizeof (*ret) + f->nr_term_args * sizeof (char *));
  if (ret == NULL)
    error (EXIT_FAILURE, errno, "malloc");

  ret->term_name = strdup (f->term_name);
  if (ret->term_name == NULL)
    error (EXIT_FAILURE, errno, "strdup");

  ret->nr_term_args = f->nr_term_args;

  for (i = 0; i < f->nr_term_args; ++i) {
    ret->term_arg[i] = strdup (f->term_arg[i]);
    if (ret->term_arg[i] == NULL)
      error (EXIT_FAILURE, errno, "strdup");
  }

  return ret;
}

bool
is_fact (bool is_true, const fact *f)
{
  return gl_oset_search (is_true ? true_facts : false_facts, f);
}

bool
add_fact (bool is_true, const fact *f)
{
  fact *f2 = deep_copy_fact (f);
  bool ret;

  ret = gl_oset_add (is_true ? true_facts : false_facts, f2);

  /* Didn't add it, so we must free the deep copy. */
  if (!ret)
    free_fact (f2);

  return ret;
}
