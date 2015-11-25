(* libguestfs -*- text -*-
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

{
open Rules_parser

let string_of_lexbuf = Lexing.lexeme

(* Errors raised by the lexer. *)
exception Error of string * string * int * int

let raise_error lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  raise (Error (msg, p.Lexing.pos_fname,
                     p.Lexing.pos_lnum,
                     p.Lexing.pos_cnum - p.Lexing.pos_bol))

(* Store "..." strings. *)
let string_buf = Buffer.create 256
let reset_string_buffer () = Buffer.clear string_buf
let store_string_char c = Buffer.add_char string_buf c
let get_string_buffer () = Buffer.contents string_buf

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

(* Store {{ CODE }} sections. *)
let code_buf = Buffer.create 256
let reset_code_buffer () = Buffer.clear code_buf
let store_code_char c = Buffer.add_char code_buf c
let get_code_buffer () = Buffer.contents code_buf
}

(* Characters that can appear within an identifier (after the first
 * character which is treated specially below).
 *)
let id_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']

(* Whitespace. *)
let ws = [' ' '\t']

(* Backslash escapes within strings. *)
let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r']

rule token = parse
        | "/*"       { comment lexbuf; token lexbuf }
        | '('        { LPAREN }
        | ')'        { RPAREN }
        | '*'        { STAR }
        | '?'        { QUESTION }
        | '+'        { PLUS }
        | '.'        { DOT }
        | ":-"       { IMPLIC }
        | ','        { COMMA }
        | ';'        { SEMI }
        | '!'        { NOT }
        | '='        { EQUALS }
        | '"'        { reset_string_buffer ();
                       string lexbuf;
                       STRING (get_string_buffer ()) }
        | "{{"       { reset_code_buffer ();
                       code lexbuf;
                       CODE (get_code_buffer ()) }
        | "true"     { TRUE }
        | "false"    { FALSE }
        | ['A'-'Z'] id_char* { UID (string_of_lexbuf lexbuf) }
        | ['a'-'z' '_'] id_char* { LID (string_of_lexbuf lexbuf) }
        | '\n'       { Lexing.new_line lexbuf; token lexbuf }
        | ws         { token lexbuf }
        | eof        { raise End_of_file }
        | _          { raise_error lexbuf "unexpected character in input" }

(* Discard C-style comments. *)
and comment = parse
        | "*/"       { () }
        | eof        { raise_error lexbuf "unterminated comment" }
        | '\n'       { Lexing.new_line lexbuf; comment lexbuf }
        | _          { comment lexbuf }

(* Store "..." strings. *)
and string = parse
        | '"'        { () }
        | eof        { raise_error lexbuf "unterminated string" }
        | '\n'       { raise_error lexbuf "strings cannot contain newline characters" }
        | '\\' (backslash_escapes as c)
                     { store_string_char (char_for_backslash c); string lexbuf }
        | _ as c     { store_string_char c; string lexbuf }

(* Store {{ ... }} (CODE) sections containing C code. *)
and code = parse
        | "}}"       { () }
        | eof        { raise_error lexbuf "unterminated code section" }
        | '\n' as c  { Lexing.new_line lexbuf; store_code_char c; code lexbuf }
        | _ as c     { store_code_char c; code lexbuf }
