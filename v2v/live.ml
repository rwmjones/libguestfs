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

open Unix
open Printf

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Live_protocol_wrapper

let no_error = ERR_0

type t = {
  mutable closed : bool;
  listening_sock : file_descr;
  mutable sock : file_descr option;
}

let port { closed; listening_sock } =
  assert (not closed);
  let port =
    match getsockname listening_sock with
    | ADDR_INET (_, port) -> port
    | _ -> assert false in
  assert (port > 0);
  port

let rec peername { closed; sock } =
  assert (not closed);
  match sock with
  | None -> assert false (* didn't call negotiate first *)
  | Some sock ->
     peer_string_of_socket sock

and peer_string_of_socket sock =
  match getpeername sock with
  | ADDR_UNIX str -> str
  | ADDR_INET (addr, port) -> sprintf "%s:%d" (string_of_inet_addr addr) port

let rec create () =
  (* Create the listening socket, and get the kernel to assign
   * a random port number.
   * XXX Listen on IPv6 too.
   * XXX Allow user choice of loopback or any addr or specific interface.
   *)
  let listening_sock = socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  bind listening_sock (ADDR_INET (inet_addr_any, 0));
  listen listening_sock 4;
  let t = { closed = false; listening_sock; sock = None } in
  Gc.finalise close t;
  t

and close ({ closed; listening_sock; sock } as t) =
  if not closed then (
    Unix.close listening_sock;
    match sock with
    | None -> ()
    | Some sock -> Unix.close sock
  );
  t.closed <- true

let rec negotiate ({ closed; listening_sock } as t) token =
  assert (not closed);

  let sock, _ = accept ~cloexec:true listening_sock in

  (try do_negotiation sock token
   with exn ->
     warning (f_"failed connection from %s: %s")
             (peer_string_of_socket sock)
             (match exn with
              | Failure s -> s
              | End_of_file -> (s_"client closed the connection unexpectedly")
              | exn -> Printexc.to_string exn);
     Unix.close sock;
     negotiate t token
  );

  t.sock <- Some sock

(* Perform the header exchange and negotiation phases of the protocol. *)
and do_negotiation sock token =
  (* The phases of the protocol. *)
  do_header_exchange sock token;
  do_option_negotiation sock;
  (* XXX *)
  ()

(* Header Exchange Phase *)
and do_header_exchange sock token =
  let header = xdr_get_header sock in

  let header_reply = {
    magic = header_magic;
    ver = header_version;
    server_features = 0L;
    server_features_must = 0L;
    error = no_error;
  } in

  if header.magic <> header_magic then (
    let header_reply = { header_reply with error = ERR_INVALID_MAGIC } in
    xdr_put_header_reply sock header_reply;
    failwith (s_"invalid magic string in header")
  );
  if header.ver <> header_version then (
    let header_reply = { header_reply with error = ERR_UNKNOWN_VERSION } in
    xdr_put_header_reply sock header_reply;
    failwithf (f_"unknown protocol version %ld: this server can only handle protocol version %ld")
              header.ver header_version
  );
  if header.token <> String.sub token 0 token_length then (
    let header_reply = { header_reply with error = ERR_INVALID_TOKEN } in
    xdr_put_header_reply sock header_reply;
    failwith (s_"invalid token from client")
  );

  (* We currently don't understand any client features, so ... *)
  if header.client_features_must <> 0L then (
    let header_reply = { header_reply with
                         error = ERR_UNKNOWN_CLIENT_FEATURE } in
    xdr_put_header_reply sock header_reply;
    failwithf (f_"unknown client feature 0x%Lx that the server is required to understand")
              header.client_features_must
  );
  (* Ignore the optional header.client_features for now. *)

  (* Send the acknowledgement packet. *)
  xdr_put_header_reply sock header_reply

(* Option Negotiation Phase. *)
and do_option_negotiation sock =

  (* XXX *)
  ()
