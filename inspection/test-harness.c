/* guestfs-inspection tests
 * Copyright (C) 2015-2016 Red Hat Inc.
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
#include <assert.h>
#include <errno.h>

#include "cleanups.h"
#include "rules.h"

int verbose = 1;

int
main (int argc, char *argv[])
{
  /* Run the rules. */
  rules ();

  /* For debugging. */
  print_true_facts ();

  /* The tests should contain a TestOK rule which verifies that all
   * true and false facts are what we expect.  So we just need to
   * check that TestOK is true and we're done.
   */
  CLEANUP_FREE fact *f = create_fact ("TestOK", NULL);
  if (!is_fact (true, f)) {
    fprintf (stderr, "%s: test failed, see debugging information above\n",
             argv[0]);
    exit (EXIT_FAILURE);
  }

  exit (EXIT_SUCCESS);
}
