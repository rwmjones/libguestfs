(* virt-v2v
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
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

(** The protocol used by live conversion (server side).

    For the client side, see [live/*.c].  For documentation
    and description of the protocol see [live/*.x] and
    [generator/live_protocol.ml]. *)

type t

val create : unit -> t
(** Create the listening socket on a random port. *)

val close : t -> unit
(** Close the connection.  It is also cleaned up properly if the
    connection is garbage collected.  This just allows you to
    do an explicit close. *)

val negotiate : t -> string -> unit
(** Accept a connection on the socket.  The string parameter is
    the token that the client is expected to present.

    This only returns when a client has successfully finished
    the negotiation phase.  (If clients connect and are unsuccessful
    then it waits for further connections).

    The socket is {i not} owned by this module.  It must be
    closed by the caller. *)

val port : t -> int
val peername : t -> string
(** Return the local listening port or printable peer address of
    the connection.  Note that [peername] will fail unless
    {!negotiate} has been called and returned successfully
    (ie. we are connected to a client). *)
