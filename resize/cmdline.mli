(* virt-resize
 * Copyright (C) 2010-2015 Red Hat Inc.
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

(** Command line argument parsing. *)

type align_first_t = [ `Never | `Always | `Auto ]

type unknown_filesystems_mode =
  | UnknownFsIgnore
  | UnknownFsWarn
  | UnknownFsError

type cmdline = {
  infile : string;
  infile_uri : URI.uri;
  outfile : string;
  align_first : align_first_t;
  alignment : int64;
  copy_boot_loader : bool;
  deletes : string list;
  dryrun : bool;
  expand : string option;
  expand_content : bool;
  extra_partition : bool;
  format : string option;
  ignores : string list;
  lv_expands : string list;
  machine_readable : bool;
  ntfsresize_force : bool;
  output_format : string option;
  resizes : string list;
  resizes_force : string list;
  shrink : string option;
  sparse : bool;
  unknown_fs_mode : unknown_filesystems_mode;
}

val parse_cmdline : unit -> cmdline
