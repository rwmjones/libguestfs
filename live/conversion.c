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
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <locale.h>
#include <libintl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include "full-read.h"
#include "full-write.h"
#include "getprogname.h"

#include "live.h"

static int sock;
static XDR xdr;

static int read_data (void *unused, void *buf, int count);
static int write_data (void *unused, void *buf, int count);

/**
 * Perform live conversion to the named conversion server.
 * The token is used both for authentication and to find
 * the port number to connect on.
 *
 * XXX Allow conversions over SSH.
 */
void
do_live_conversion (const char *server, const char *full_token)
{
  char token[TOKEN_LENGTH+1];
  int port;
  size_t i;
  struct addrinfo hints;
  struct addrinfo *results, *rp;
  char port_str[16];
  int r;
  header header;
  header_reply header_reply;

  /* The full token should be /^[A-Z]+-[0-9]+$/.  We expect the
   * alphabetic part to be exactly TOKEN_LENGTH characters long.  The
   * numeric part is a port number on the conversion server.
   */
  for (i = 0; i < TOKEN_LENGTH; ++i) {
    if (full_token[i] < 'A' || full_token[i] > 'Z')
      goto token_error;
    token[i] = full_token[i];
  }
  token[TOKEN_LENGTH] = '\0';
  if (full_token[i] != '-')
    goto token_error;
  if (sscanf (&full_token[i+1], "%d", &port) != 1) {
  token_error:
    fprintf (stderr, _("%s: invalid token.\nPlease check the token was pasted correctly from the virt-v2v server.\n"),
             getprogname ());
    exit (EXIT_FAILURE);
  }

  snprintf (port_str, sizeof port_str, "%d", port);

  /* Open the connection. */
  memset (&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;     /* Allow IPv4 or IPv6 connections. */
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_NUMERICSERV; /* Numeric dest port number. */
  hints.ai_protocol = 0;           /* Any protocol. */

  r = getaddrinfo (server, port_str, &hints, &results);
  if (r != 0) {
    fprintf (stderr, "%s: getaddrinfo: %s:%s: %s\n",
             getprogname (), server, port_str, gai_strerror (r));
    exit (EXIT_FAILURE);
  }

  sock = -1;
  for (rp = results; rp != NULL; rp = rp->ai_next) {
    sock = socket (rp->ai_family, rp->ai_socktype | SOCK_CLOEXEC,
                   rp->ai_protocol);
    if (sock == -1)
      continue;

    if (connect (sock, rp->ai_addr, rp->ai_addrlen) == -1) {
      fprintf (stderr, "%s: connect: %s:%s: %m\n",
               getprogname (), server, port_str);
      close (sock);
      sock = -1;
      continue;
    }

    break;
  }

  freeaddrinfo (results);

  if (sock == -1) {
    fprintf (stderr, _("%s: could not connect to %s:%s\n"),
             getprogname (), server, port_str);
    exit (EXIT_FAILURE);
  }

  if (verbose)
    printf ("connected to %s:%s\n", server, port_str);

  /* glibc rpc and libtirpc use different prototypes for the read and
   * write functions.  Casting them to void* prevents warnings.
   */
  xdrrec_create (&xdr, 0, 0, NULL, (void *) read_data, (void *) write_data);
  xdrrec_skiprecord (&xdr);

  /* Header Exchange Phase. */
  memcpy (header.magic, HEADER_MAGIC, sizeof header.magic);
  header.ver = HEADER_VERSION;
  memcpy (header.token, token, sizeof header.token);
  header.client_features = 0;
  header.client_features_must = 0;

  xdr.x_op = XDR_ENCODE;
  if (!xdr_header (&xdr, &header) || !xdrrec_endofrecord (&xdr, 1)) {
    fprintf (stderr, _("%s: failed to send header\n"),
             getprogname ());
    exit (EXIT_FAILURE);
  }

  /* Read the reply. */
  xdr.x_op = XDR_DECODE;
  if (!xdr_header_reply (&xdr, &header_reply)) {
    fprintf (stderr, _("%s: failed to read header reply\n"),
             getprogname ());
    exit (EXIT_FAILURE);
  }
  if (memcmp (header_reply.magic, HEADER_MAGIC, strlen (HEADER_MAGIC)) != 0) {
    fprintf (stderr, _("%s: invalid magic string in header reply\n"),
             getprogname ());
    exit (EXIT_FAILURE);
  }
  if (header_reply.ver != HEADER_VERSION) {
    fprintf (stderr, _("%s: unknown protocol version %d from server: this client can only handle protocol version %d\n"),
             getprogname (), header_reply.ver, HEADER_VERSION);
    exit (EXIT_FAILURE);
  }

  /* Error from server? */
  if (header_reply.error != 0) {
    /* XXX translate error */
    fprintf (stderr, _("%s: server error: %s\n"),
             getprogname (), live_strerror (header_reply.error));
    exit (EXIT_FAILURE);
  }

  /* We currently don't understand any server features, but we must
   * check server_features_must == 0.
   */
  if (header_reply.server_features_must != 0) {
    fprintf (stderr, _("%s: unknown server feature 0x%" PRIx64 " that the client is required to understand\n"),
             getprogname (),
             (uint64_t) header_reply.server_features_must);
    exit (EXIT_FAILURE);
  }

  xdr_free ((xdrproc_t) xdr_header_reply, (char *) &header_reply);

  if (verbose)
    printf ("header exchange completed successfully\n");

  /* Option Negotiation Phase. */


  xdr_destroy (&xdr);
  close (sock);
}

static int
read_data (void *unused, void *buf, int count)
{
  size_t len = (size_t) count, r;

  errno = 0;
  r = full_read (sock, buf, len);
  if (r < len) {
    if (errno == 0)
      /* This is the EOF case, but if it happens any time we are
       * reading a message then it's an error.  Also the xdrrec
       * layer just keeps reading in a loop forever if you return
       * 0 here.
       */
      errno = EILSEQ;
    return -1;
  }
  return r;
}

static int
write_data (void *unused, void *buf, int count)
{
  size_t len = (size_t) count, r;

  r = full_write (sock, buf, len);
  if (r < len)
    return -1; /* error */
  return r;
}
