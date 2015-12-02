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

#ifndef GUESTFS_RULES_H
#define GUESTFS_RULES_H

#include <stdbool.h>

#include "gl_oset.h"

extern int verbose;

/* facts.c */
struct fact {
  char *term_name;
  size_t nr_term_args;
  char *term_arg[];
};
typedef struct fact fact;

/* Create a fact on the heap.  This doesn't copy the strings, but they
 * are deep copied when we call add_fact.
 */
extern fact *create_fact (const char *term_name, size_t n, ...);

/* Create a fact on the stack and set 'var' to be a const pointer to
 * it.  This doesn't copy the strings, but they are deep copied when
 * we call add_fact.  Because of stupidity in C99, this is way more
 * complex than it needs to be.
 */
#define CREATE_FACT(var,name,n,...)                     \
  struct {                                              \
    const char *term_name;                              \
    size_t nr_term_args;                                \
    const char *term_arg[n];                            \
  } var##_tmp_fact = { (name), (n), { __VA_ARGS__ } };  \
  const fact *var = (struct fact *) &var##_tmp_fact

extern void clear_true_facts (void);
extern void clear_false_facts (void);
extern size_t count_true_facts (void);
extern void print_fact (bool is_true, const fact *f, FILE *fp);
extern void print_true_facts (void);
extern void print_false_facts (void);
extern void add_all_fact_strings (gl_oset_t set);
extern void add_strings_from_facts (gl_oset_t set, const char *term_name, size_t arg_i);
extern bool is_fact (bool is_true, const fact *);
extern bool add_fact (bool is_true, const fact *);

/* rules.c - generated code */
extern const char *all_strings[];
extern void rules (void);

#endif /* GUESTFS_RULES_H */
