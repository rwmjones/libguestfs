(* libguestfs
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

open Docstrings
open Pr

open Printf

let generate_header = generate_header ~inputs:["generator/live_protocol.ml"]

let header_magic = "LV2V"
let magic_length = String.length header_magic
let header_version = 1

(* The length of the alphabetic part of the token.  This is embedded
 * in the protocol so cannot be changed.
 *)
let token_length = 6

(* This is the maximum block size supported by the protocol.  It is
 * possible in future to negotiate this using options.
 *)
let max_block_size = 65536

(* Maximum message size in the protocol.  This does not include the
 * oversided data packets which are sent outside the XDR.
 *)
let max_message = 4096

type ftype =
  | Byte
  | Char
  | String
  | Int
  | Int64
  | Bool
  | Enum of string
  | Array of ftype * int        (* fixed length array *)
  | VarArray of ftype           (* variable length array *)
  | VarMaxArray of ftype * int  (* variable length array with max length *)

(* List of errors.
 * Note: Add new errors at the end to avoid renumbering errors.
*)
let errors = [
  "INVALID_MAGIC",    "invalid magic string in header";
  "UNKNOWN_VERSION",  "server does not understand this protocol version";
  "INVALID_TOKEN",    "invalid token sent by the client";
  "UNKNOWN_CLIENT_FEATURE", "server does not understand a ‘MUST’ client feature";
]

(* Enumerated types. *)
let enums = [
   "err", "Protocol errors sent by the server",
   "ERR_", 1, errors;
   "opt", "Options requested by the client",
   "OPT_", 0,
   [
     "END",              "indicates end of client options";
     "UPGRADE_TLS",      "upgrade connection to TLS";
     "DISK",             "describe a client disk";
     "HASH_NONE",        "checksum field is not populated";
     "HASH_MD5",         "use MD5 format for block checksums";
     "HASH_SHA256",      "use SHA256 format for block checksums";
     "MAX_BLOCK_65536",  "maximum block size supported is 65536 bytes";
   ];
   "transfer", "Transfer message type",
   "TRANSFER_", 1,
   [
     "DATA",             "data block (struct data)";
     "NEAR_CONVERGENCE", "client reached near convergence state";
     "CONVERGED",        "client reached converged state";
   ];
]

(* Protocol structures sent over the wire. *)
let structs = [
  (* Header Exchange Phase *)
  "header", "Protocol header, sent by the client when it connects",
  [
    "magic", Array (Char, magic_length),
      sprintf "The characters: %S" header_magic;
    "ver", Int, "Always 1 in the current implementation";
    "token", Array (Char, token_length),
      "The token that the client must present";
    "client_features", Int64, "Bitmap of client features (not used)";
    "client_features_must", Int64, "Bitmap of client features server must understand (not used)";
  ];
  "header_reply", "Protocol header reply, sent by the server",
  [
    "magic", Array (Char, magic_length), "Server repeats the magic string";
    "ver", Int, "Always 1 in the current implementation";
    "server_features", Int64, "Bitmap of server features (not used)";
    "server_features_must", Int64, "Bitmap of server features client must understand (not used)";
    "error", Enum "err", "If non-zero, there was a protocol error";
  ];

  (* Option Negotiation Phase *)
  "option_request", "Option request, sent by the client",
  [
    "option", Enum "opt", "Requested option"
    (* One of the option_* structures may follow here depending
     * on the option field.
     *)
  ];
  "option_reply", "Option request, sent by the client",
  [
    "error", Enum "err", "If non-zero, there was an error"
    (* One of the option_reply_* structures may follow here depending
     * on the option field.
     *)
  ];
  "option_disk", "OPTION_DISK request structure",
  [
    "index", Int, "Disk index (starting from 0)";
    "size", Int64, "Size of disk in bytes";
    "device", String, "Local device name (eg. \"sda\")";
  ];

  (* Data Transfer Phase *)
  "transfer_header", "Transfer header followed by data or convergence messages",
  [
    "transfer", Enum "transfer", "Transfer message type";
  ];
  "data", "Data block, sent by client",
  [
    "disk", Int, "Disk index";
    "offset", Int64, "Offset of this block in bytes";
    "size", Int, "Size of the block in bytes";
    "checksum", VarArray Byte, "Checksum of the block";
    (* This is followed by the data block which is not encoded
     * using XDR but will be exactly ‘size’ bytes.
     *)
  ];
  (* There are no server replies to data blocks, but the server may
   * disconnect if there is some error.
   *)

  (* Convergence Phase *)
  "near_convergence_reply", "Reply to near convergence message",
  [
    "transfer", Enum "transfer", "Always TRANSFER_NEAR_CONVERGENCE";
    "error", Enum "err", "If non-zero, there was a server error";
    "proceed", Bool, "If true, proceed with single user mode convergence"
  ];

  "converged_reply", "Reply to converged message",
  [
    "transfer", Enum "transfer", "Always TRANSFER_CONVERGED";
    "error", Enum "err", "If non-zero, there was a server error"
  ];

  (* Metadata Phase *)
  "metadata", "Metadata message, sent by client",
  [
    "metadata", String, "Guest metadata (libvirt XML)";
  ];
  "metadata_reply", "Reply to metadata message",
  [
    "error", Enum "err", "If non-zero, there was a server error"
  ];
]

let rec xdr_field_of_ftype fname = function
  | Byte -> sprintf "opaque %s" fname
  | Char -> sprintf "char %s" fname
  | String -> sprintf "string %s<>" fname
  | Int -> sprintf "int %s" fname
  | Int64 -> sprintf "int64_t %s" fname
  | Bool -> sprintf "bool %s" fname
  | Enum t -> sprintf "%s %s" t fname
  | Array (ftype, size) ->
     sprintf "%s[%d]" (xdr_field_of_ftype fname ftype) size
  | VarArray ftype ->
     sprintf "%s<>" (xdr_field_of_ftype fname ftype)
  | VarMaxArray (ftype, max_size) ->
     sprintf "%s<%d>" (xdr_field_of_ftype fname ftype) max_size

let generate_live_protocol_x () =
  generate_header ~emacs_mode:"c" CStyle GPLv2plus;

  pr "%%#include <config.h>\n";
  pr "\n";

  List.iter (
    fun (name, comment, prefix, offset, fields) ->
      let nr_fields = List.length fields in
      pr "/* %s */\n" comment;
      pr "enum %s {\n" name;
      List.iteri (
        fun i (str, comment) ->
          let comma, space =
            if i < nr_fields-1 then ",", "" else "", " " in
          pr "  %s%-24s = %d%s" prefix str (i+offset) comma;
          if String.length comment > 35 then
            pr "\n\t\t/* %s */\n"  comment
          else
            pr "%s /* %s */\n" space comment
      ) fields;
      pr "};\n";
      pr "\n";
  ) enums;

  List.iter (
    fun (name, comment, fields) ->
      pr "/* %s */\n" comment;
      pr "struct %s {\n" name;
      List.iter (
        fun (fname, ftype, fcomment) ->
          let field = xdr_field_of_ftype fname ftype in
          pr "  %-24s /* %s */\n" (field ^ ";") fcomment
      ) fields;
      pr "};\n";
      pr "\n";
  ) structs

let generate_live_protocol_extra_h () =
  generate_header CStyle GPLv2plus;

  pr "\
#ifndef GUESTFS_LIVE_PROTOCOL_EXTRA_H_
#define GUESTFS_LIVE_PROTOCOL_EXTRA_H_

#include <assert.h>

#include \"live_protocol.h\"

/* The magic protocol string. */
#define HEADER_MAGIC %S

/* The current protocol version. */
#define HEADER_VERSION %d

/* The length of the alphabetic part of the token. */
#define TOKEN_LENGTH %d

/* Maximum message size in the protocol. */
#define MAX_MESSAGE %d

" header_magic header_version token_length max_message;

  pr "/* Server errors as strings. */\n";
  pr "static const char *live_errors[] = {\n";
  List.iter (
    fun (sym, str) ->
      pr "  [ERR_%s] =\n" sym;
      pr "    \"%s\",\n" str
  ) errors;
  pr "};\n";
  pr "\n";

  pr "\
#define MAX_ERROR %d

static inline const char *
live_strerror (int errnum)
{
  /* Internal error.  The client should be checking that errnum != 0
   * before calling this function.
   */
  assert (errnum > 0);

  /* This might happen if an upgraded server sends a new error which
   * we don't know anything about.  We don't want to fail here.  This
   * isn't thread safe (XXX).
   */
  if (errnum > MAX_ERROR) {
    static char errstr[64];
    snprintf (errstr, sizeof errstr, \"unknown error: ERR_%%d\", errnum);
    return errstr;
  }

  return live_errors[errnum];
}
" (List.length errors);

  pr "\n";
  pr "#endif /* GUESTFS_LIVE_PROTOCOL_EXTRA_H_ */\n"

let rec ocaml_type_of_ftype = function
  | Byte | Char -> "char"
  | String -> "string"
  | Int -> "int32"
  | Int64 -> "int64"
  | Bool -> "bool"
  | Enum t -> t
  | Array (Byte, _) | VarArray Byte | VarMaxArray (Byte, _)
  | Array (Char, _) | VarArray Char | VarMaxArray (Char, _) -> "string"
  | Array (ftype, _)
  | VarArray ftype
  | VarMaxArray (ftype, _) ->
     ocaml_type_of_ftype ftype ^ " array"

let generate_live_protocol_wrapper_mli () =
  generate_header OCamlStyle GPLv2plus;

  pr "\
val header_magic : string
(** The magic protocol string. *)

val header_version : int32
(** The current protocol version. *)

val token_length : int
(** The length of the alphabetic part of the token. *)

";

  List.iter (
    fun (name, comment, prefix, offset, fields) ->
      pr "type %s =\n" name;
      for i = 0 to offset-1 do
        pr "| %s%d\n" prefix i
      done;
      List.iteri (
        fun i (str, comment) ->
          pr "| %s%-24s (** %s *)\n" prefix str comment
      ) fields;
      pr "(** %s *)\n" comment;
      pr "\n";
  ) enums;

  List.iter (
    fun (name, comment, fields) ->
      pr "type %s = {\n" name;
      List.iter (
        fun (fname, ftype, fcomment) ->
          let ftype = ocaml_type_of_ftype ftype in
          let fname_ftype = sprintf "%s : %s;" fname ftype in
          pr "  %-24s (** %s *)\n" fname_ftype fcomment
      ) fields;
      pr "}\n";
      pr "(** %s *)\n" comment;
      pr "\n";
      pr "val xdr_get_%s : Unix.file_descr -> %s\n" name name;
      pr "val xdr_put_%s : Unix.file_descr -> %s -> unit\n" name name;
      pr "(** Receive and send %s message. *)\n" name;
      pr "\n";
  ) structs

let generate_live_protocol_wrapper_ml () =
  generate_header OCamlStyle GPLv2plus;

  pr "\
let header_magic = %S
let header_version = %d_l
let token_length = %d

" header_magic header_version token_length;

  List.iter (
    fun (name, comment, prefix, offset, fields) ->
      pr "type %s =\n" name;
      for i = 0 to offset-1 do
        pr "| %s%d\n" prefix i
      done;
      List.iteri (
        fun i (str, comment) ->
          pr "| %s%s\n" prefix str
      ) fields;
  ) enums;
  pr "\n";

  List.iter (
    fun (name, comment, fields) ->
      pr "type %s = {\n" name;
      List.iter (
        fun (fname, ftype, fcomment) ->
          let ftype = ocaml_type_of_ftype ftype in
          pr "  %s : %s;\n" fname ftype
      ) fields;
      pr "}\n";
      pr "external xdr_get_%s : Unix.file_descr -> %s = \"guestfs_int_live_xdr_get_%s\"\n"
         name name name;
      pr "external xdr_put_%s : Unix.file_descr -> %s -> unit = \"guestfs_int_live_xdr_put_%s\"\n"
         name name name;
  ) structs

let generate_live_protocol_wrapper_c () =
  generate_header CStyle GPLv2plus;

  pr "\
#include <config.h>

#include <stdio.h>
#include <errno.h>

#include <rpc/xdr.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include \"full-read.h\"
#include \"full-write.h\"

#include \"live_protocol.h\"

/* Stops GCC complaining about missing prototypes of OCaml external
 * functions.
 */
#pragma GCC diagnostic ignored \"-Wmissing-prototypes\"

/* This buffer is used for serializing and deserializing XDR
 * messages.  If we ever have to worry about threads then we
 * will need to stop using a global buffer for this. (XXX)
 */
static char buf[%d];

" max_message;

  List.iter (
    fun (name, comment, fields) ->
      pr "\
value
guestfs_int_live_xdr_get_%s (value sockv)
{
  CAMLparam1 (sockv);
  /* Note that Unix.file_descr is really just an int. */
  int sock = Int_val (sockv);
  CAMLlocal2 (rv, tv);
  XDR xdr;
  uint32_t len;
  int r;
  %s v;

  if (full_read (sock, buf, 4) != 4) {
    if (errno == 0) caml_raise_end_of_file ();
    else unix_error (errno, (char *) \"xdr_get_%s: read\", Nothing);
  }
  xdrmem_create (&xdr, buf, 4, XDR_DECODE);
  xdr_u_int (&xdr, &len);
  xdr_destroy (&xdr);
  if (len > sizeof buf)
    caml_failwith (\"xdr_get_%s failed: invalid length or oversized struct\");
  if (full_read (sock, buf, len) != len) {
    if (errno == 0) caml_raise_end_of_file ();
    else unix_error (errno, (char *) \"xdr_get_%s: read\", Nothing);
  }
  xdrmem_create (&xdr, buf, len, XDR_DECODE);
  r = xdr_%s (&xdr, &v);
  xdr_destroy (&xdr);
  if (!r)
    caml_failwith (\"xdr_get_%s failed to decode the structure\");

  /* Convert the C structure into an OCaml structure. */
  rv = caml_alloc_tuple (%d);
" name name name name name name name (List.length fields);

      List.iteri (
        fun i (fname, ftype, _) ->
          match ftype with
          | Byte | Char ->
             pr "  Store_field (rv, %d, Val_int (v.%s));\n" i fname
          | String ->
             pr "  tv = caml_copy_string (v.%s);\n" fname;
             pr "  Store_field (rv, %d, tv);\n" i
          | Int ->
             pr "  tv = caml_copy_int32 (v.%s);\n" fname;
             pr "  Store_field (rv, %d, tv);\n" i
          | Int64 ->
             pr "  tv = caml_copy_int64 (v.%s);\n" fname;
             pr "  Store_field (rv, %d, tv);\n" i
          | Bool ->
             pr "  Store_field (rv, %d, Val_bool (v.%s));\n" i fname
          | Enum _ ->
             pr "  Store_field (rv, %d, Val_int (v.%s));\n" i fname
          | Array (Char, size) ->
             pr "  tv = caml_alloc_string (%d);\n" size;
             pr "  memcpy (String_val (tv), v.%s, %d);\n" fname size;
             pr "  Store_field (rv, %d, tv);\n" i
          | VarArray Byte
          | VarMaxArray (Byte, _) ->
             pr "  tv = caml_alloc_string (v.%s.%s_len);\n" fname fname;
             pr "  memcpy (String_val (tv), v.%s.%s_val, v.%s.%s_len);\n"
                fname fname fname fname;
             pr "  Store_field (rv, %d, tv);\n" i
          | Array _
          | VarArray _
          | VarMaxArray _ -> assert false
      ) fields;

      pr "
  xdr_free ((xdrproc_t) xdr_%s, (char *) &v);
  CAMLreturn (rv);
}

value
guestfs_int_live_xdr_put_%s (value sockv, value vv)
{
  CAMLparam2 (sockv, vv);
  /* Note that Unix.file_descr is really just an int. */
  int sock = Int_val (sockv);
  XDR xdr;
  uint32_t len;
  char lenbuf[4];
  int r;
  %s v;

  /* Convert the OCaml structure into a C structure. */
" name name name;

      List.iteri (
        fun i (fname, ftype, _) ->
          match ftype with
          | Byte | Char ->
             pr "  v.%s = Int_val (Field (vv, %d));\n" fname i
          | String ->
             pr "  v.%s = String_val (Field (vv, %d));\n" fname i
          | Int ->
             pr "  v.%s = Int32_val (Field (vv, %d));\n" fname i
          | Int64 ->
             pr "  v.%s = Int64_val (Field (vv, %d));\n" fname i
          | Bool ->
             pr "  v.%s = Bool_val (Field (vv, %d));\n" fname i
          | Enum _ ->
             pr "  v.%s = Int_val (Field (vv, %d));\n" fname i
          | Array (Char, size) ->
             pr "  memcpy (&v.%s, String_val (Field (vv, %d)), %d);\n"
                fname i size
          | VarArray Byte
          | VarMaxArray (Byte, _) ->
             pr "  v.%s.%s_len = caml_string_length (Field (vv, %d));\n"
                fname fname i;
             pr "  v.%s.%s_val = String_val (Field (vv, %d));\n"
                fname fname i
          | Array _
          | VarArray _
          | VarMaxArray _ -> assert false
      ) fields;

      pr "
  xdrmem_create (&xdr, buf, sizeof buf, XDR_ENCODE);
  r = xdr_%s (&xdr, &v);
  len = xdr_getpos (&xdr);
  xdr_destroy (&xdr);
  if (!r)
    caml_failwith (\"xdr_put_%s failed to encode the structure\");

  xdrmem_create (&xdr, lenbuf, sizeof lenbuf, XDR_ENCODE);
  xdr_u_int (&xdr, &len);
  xdr_destroy (&xdr);

  if (full_write (sock, lenbuf, sizeof lenbuf) != sizeof lenbuf ||
      full_write (sock, buf, len) != len)
    unix_error (errno, (char *) \"xdr_put_%s: write\", Nothing);

  CAMLreturn (Val_unit);
}
" name name name;
  ) structs
