(* guestfs-inspection
 * Copyright (C) 2009-2017 Red Hat Inc.
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

(* Command line argument parsing. *)

open Printf

open Common_utils

let sysroot = ref "/sysroot"

let argspec = [
  "--sysroot", Arg.Set_string sysroot, "DIR Set mountpoint directory.";
  "--verbose", Arg.Unit set_verbose, " Enable verbose messages.";
]
let argspec = Arg.align argspec

let usage_msg = sprintf "\
%s: perform guestfs inspection

Normally you should NOT run this program by hand.

A short summary of the options is given below.  For detailed help please
read the man page guestfs-inspection(8).
" prog

let parse_cmdline () =
  Arg.parse argspec (fun s -> raise (Arg.Bad ("unknown extra parameter: " ^ s)))
            usage_msg

let sysroot () = !sysroot
