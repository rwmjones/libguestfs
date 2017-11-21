/* Live conversion client
 * Copyright (C) 2017 Red Hat Inc.
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
#include <limits.h>
#include <getopt.h>
#include <error.h>
#include <errno.h>
#include <locale.h>
#include <libintl.h>

#include "getprogname.h"

#include "live.h"

int verbose = 0;

enum { HELP_OPTION = CHAR_MAX + 1 };
static const char options[] = "Vv";
static const struct option long_options[] = {
  { "help", 0, 0, HELP_OPTION },
  { "long-options", 0, 0, 0 },
  { "short-options", 0, 0, 0 },
  { "verbose", 0, 0, 'v' },
  { "version", 0, 0, 'V' },
  { 0, 0, 0, 0 }
};

static void __attribute__((noreturn))
usage (int status)
{
  if (status != EXIT_SUCCESS)
    fprintf (stderr, _("Try ‘%s --help’ for more information.\n"),
             getprogname ());
  else {
    printf (_("%s: Perform live conversion on a physical or virtual\n"
              "machine.\n"
              "Copyright (C) 2017 Red Hat Inc.\n"
              "Usage:\n"
              "  %s [--options] [server token]\n"
              "Options:\n"
              "  --help                 Display brief help\n"
              "  -v|--verbose           Verbose messages\n"
              "  -V|--version           Display version and exit\n"
              "For more information, see the manpage %s(1).\n"),
            getprogname (), getprogname (),
            getprogname ());
  }
  exit (status);
}

/* XXX Copied from fish/options.c. */
static void
display_short_options (const char *format)
{
  while (*format) {
    if (*format != ':')
      printf ("-%c\n", *format);
    ++format;
  }
  exit (EXIT_SUCCESS);
}

static void
display_long_options (const struct option *long_options)
{
  while (long_options->name) {
    if (STRNEQ (long_options->name, "long-options") &&
        STRNEQ (long_options->name, "short-options"))
      printf ("--%s\n", long_options->name);
    long_options++;
  }
  exit (EXIT_SUCCESS);
}

int
main (int argc, char *argv[])
{
  int c;
  int option_index;

  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEBASEDIR);
  textdomain (PACKAGE);

  for (;;) {
    c = getopt_long (argc, argv, options, long_options, &option_index);
    if (c == -1) break;

    switch (c) {
    case 0:			/* options which are long only */
      if (STREQ (long_options[option_index].name, "long-options")) {
        display_long_options (long_options);
      }
      else if (STREQ (long_options[option_index].name, "short-options")) {
        display_short_options (options);
      }
      else
        error (EXIT_FAILURE, 0,
               _("unknown long option: %s (%d)"),
               long_options[option_index].name, option_index);
      break;

    case 'v':
      verbose = 1;
      break;

    case 'V':
      printf ("%s %s\n", getprogname (), PACKAGE_VERSION_FULL);
      exit (EXIT_SUCCESS);

    case HELP_OPTION:
      usage (EXIT_SUCCESS);

    default:
      usage (EXIT_FAILURE);
    }
  }

  /* There should be either 0 (interactive) or 2 extra arguments
   * on the command line.  Anything else is an error.
   */
  if (optind == argc) {
    /* XXX interactive */
    fprintf (stderr, "XXX interactive not implemented yet\n");
    exit (EXIT_FAILURE);
  }
  else if (optind == argc-2) {
    do_live_conversion (argv[optind], argv[optind+1]);
    exit (EXIT_SUCCESS);
  }
  else {
    fprintf (stderr, _("%s: unused arguments on the command line\n"),
             getprogname ());
    usage (EXIT_FAILURE);
  }
}
