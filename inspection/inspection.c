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
#include <inttypes.h>
#include <getopt.h>
#include <unistd.h>

#include "rules.h"

int verbose = 0;
const char *sysroot = NULL;
const size_t sysroot_len = 0;

/* Required by the gnulib 'error' module. */
const char *program_name = "guestfs-inspection";

static void __attribute__((noreturn))
usage (int status)
{
  if (status != EXIT_SUCCESS)
    fprintf (stderr, "Try `%s --help' for more information.\n",
             program_name);
  else {
    printf ("%s: guestfs inspection\n"
            "Copyright (C) 2009-2015 Red Hat Inc.\n"
            "Usage:\n"
            "Options:\n"
            "  --help               Display brief help\n"
            "  -v|--verbose         Verbose messages\n"
            "  -V|--version         Display version and exit\n"
            "For more information, see the manpage %s(8).\n",
            program_name, program_name);
  }
  exit (status);
}

int
main (int argc, char *argv[])
{
  enum { HELP_OPTION = CHAR_MAX + 1 };

  static const char *options = "vV";
  static const struct option long_options[] = {
    { "help", 0, 0, HELP_OPTION },
    { "verbose", 0, 0, 'v' },
    { "version", 0, 0, 'V' },
    { 0, 0, 0, 0 }
  };
  int c;
  int option_index;

  for (;;) {
    c = getopt_long (argc, argv, options, long_options, &option_index);
    if (c == -1) break;

    switch (c) {
    case 0:			/* options which are long only */
      fprintf (stderr, "%s: unknown long option: %s (%d)\n",
               program_name,
               long_options[option_index].name, option_index);
      exit (EXIT_FAILURE);

    case 'v':
      verbose++;
      break;

    case 'V':
      printf ("%s %s\n", program_name, PACKAGE_VERSION_FULL);
      exit (EXIT_SUCCESS);

    case HELP_OPTION:
      usage (EXIT_SUCCESS);

    default:
      usage (EXIT_FAILURE);
    }
  }

  /* Run the rules. */
  rules ();

  /* Print the true facts.  XXX Output XXX */
  print_true_facts ();

  exit (EXIT_SUCCESS);
}
