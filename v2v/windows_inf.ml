(* virt-v2v
 * Copyright (C) 2015 Red Hat Inc.
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

open Printf

open Common_utils

type t = section list
and section = string * data list
and data = string * string

let crlf_rex = Str.regexp "\r?\n"

(* Match [[header]] in a Windows [*.inf] file. *)
let section_header_rex =
  Str.regexp "^[ \t]*\\[[ \t]*\\(.*\\)[ \t]*\\][ \t]*$"

let match_section_header line = Str.string_match section_header_rex line 0
let not_section_header line = not (match_section_header line)

(* Match [key = value] in a Windows [*.inf] file. *)
let key_value_rex =
  Str.regexp_case_fold
    "^[ \t]*\\([a-z0-9%_.]+\\)[ \t]*=[ \t]*\\(.*\\)[ \t]*$"

(* Match comment preceeded by whitespace (so comments can be removed). *)
let comment_rex = Str.regexp "[ \t]*;.*"

(* Parse a Windows [*.inf] file into headers and section lines. *)
let of_string content =
  (* Split up the inf file (possibly with DOS line endings) into lines. *)
  let lines = Str.split crlf_rex content in

  (* Split the file into section headers + section content. *)
  let rec loop = function
    | [] -> []
    | header :: xs when match_section_header header ->
       let header = Str.matched_group 1 header in
       let lines = takewhile not_section_header xs in
       let ys = dropwhile not_section_header xs in
       (header, lines) :: loop ys
    | xs ->
       (* Put all initial lines before the first section into a
        * section with no name.
        *)
       let lines = takewhile not_section_header xs in
       let ys = dropwhile not_section_header xs in
       ("", lines) :: loop ys
  in
  let sections = loop lines in

  (* Split the lines that match "key = value" into [(key, value)] pairs.
   * Ignore any other lines.
   *)
  let sections = List.map (
    fun (header, lines) ->
      let lines = filter_map (
        fun line ->
          if Str.string_match key_value_rex line 0 then (
            let key = Str.matched_group 1 line in
            let value = Str.matched_group 2 line in
            Some (key, value)
          )
          else None (* ignore the non-matching line *)
      ) lines in
      header, lines
  ) sections in

  (* If the dummy section at the beginning is now completely empty,
   * remove it.
   *)
  let sections =
    match sections with
    | ("", []) :: sections -> sections
    | sections -> sections in

  (* Remove any comments from values, conservatively though because
   * we don't really understand the value format.
   *)
  let sections = List.map (
    fun (header, lines) ->
      let lines = List.map (
        fun (key, value) ->
          let value =
            if String.contains value '"' then value
            else Str.replace_first comment_rex "" value in
          key, value
      ) lines in
      header, lines
  ) sections in

  (* Normalize (by lowercasing) the section headers and keys (but not
   * the values).
   *)
  let sections = List.map (
    fun (header, lines) ->
      let header = String.lowercase_ascii header in
      let lines = List.map (
        fun (key, value) ->
          String.lowercase_ascii key, value
      ) lines in
      header, lines
  ) sections in

  sections

let find_section t section_name =
  let section_name = String.lowercase_ascii section_name in
  List.assoc section_name t

let find_key t section_name key_name =
  let data = find_section t section_name in
  let key_name = String.lowercase_ascii key_name in
  List.assoc key_name data

let load filename =
  of_string (read_whole_file filename)

let rec to_string sections =
  String.concat "\n" (List.map string_of_section sections)

and string_of_section (header, body) =
  let header = sprintf "[%s]" header in
  let body = List.map string_of_key_value body in
  String.concat "\n" (header :: body)

and string_of_key_value (key, value) =
  sprintf "%s = %s" key value
