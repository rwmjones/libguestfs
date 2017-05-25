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

open Scanf

open Common_utils

open Utils

(* Test if [sfdisk] is recent enough to have [--part-type], to be used
 * instead of [--print-id] and [--change-id].
 *)
let test_sfdisk_has_part_type = lazy (
  let out = command "sfdisk" ["--help"] in
  String.find out "--part-type" >= 0
)

let part_get_mbr_id device partnum =
  if partnum <= 0 then
    failwith "partition number must be >= 1";

  let param =
    if Lazy.force test_sfdisk_has_part_type then
      "--part-type"
    else
      "--print-id" in

  udev_settle ();
  let out =
    command "sfdisk" [param; device; string_of_int partnum] in
  udev_settle ();

  (* It's printed in hex ... *)
  sscanf out "%x" identity

let print_partition_table ~add_m_option device =
  udev_settle ();

  let args = ref [] in
  if add_m_option then push_back args "-m";
  push_back args "-s";
  push_back args "--";
  push_back args device;
  push_back args "unit";
  push_back args "b";
  push_back args "print";

  try
    let out = command "parted" !args in
    udev_settle ();
    out
  with
    (* Translate "unrecognised disk label" into an errno code. *)
    Failure str when String.find str "unrecognised disk label" >= 0 ->
      raise (Unix.Unix_error (Unix.EINVAL, "parted", device ^ ": " ^ str))

let nr_partitions device =
  let lines = print_partition_table ~add_m_option:true device in
  let lines = String.nsplit "\n" lines in

  (* lines[0] is "BYT;", lines[1] is the device line which we ignore,
   * lines[2..] are the partitions themselves.
   *)
  let n = List.length lines - 2 in
  assert (n >= 0);
  n
