(* virt-v2v
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
 *)

(** Handle Windows driver [*.inf] files. *)

type t = section list
(** Type of a parsed Windows driver [*.inf] file. *)

and section = string * data list
(** A single section consists of a header and a list of lines.  If
    the file doesn't start with a section header, then the initial
    section has a dummy header name [""].

    The section header is always normalized to lowercase ASCII. *)

and data = string * string
(** A [key = value] pair appearing within a section body.  The key
    (but {i not} the value) is always normalized to lowercase ASCII. *)

val of_string : string -> t
(** Parse an [*.inf] file from the string.  No parse errors are
    possible since this parser accepts anything as a possible [*.inf]
    file. *)

val load : string -> t
(** Same as {!of_string} except we load the content from
    a host file. *)

val to_string : t -> string
(** Convert an inf file back to a string.  This should probably only
    be used for debugging, since we don't preserve comments and it's
    not tested that Windows would be able to parse what we write out. *)

val find_section : t -> string -> data list
(** [find_section t section_name] finds and returns a section by name.

    Raises [Not_found] if not found. *)

val find_key : t -> string -> string -> string
(** [find_key t section_name key_name] finds and returns a key within
    a particular section.

    Raises [Not_found] if not found. *)
