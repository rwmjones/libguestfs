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

open Printf

open Common_gettext.Gettext
open Common_utils

open Utils

(* Detect anti-virus (AV) software installed in Windows guests. *)
let rex_virus     = Str.regexp_case_fold "virus" (* generic *)
let rex_kaspersky = Str.regexp_case_fold "kaspersky"
let rex_mcafee    = Str.regexp_case_fold "mcafee"
let rex_norton    = Str.regexp_case_fold "norton"
let rex_sophos    = Str.regexp_case_fold "sophos"
let rex_avg_tech  = Str.regexp_case_fold "avg technologies" (* RHBZ#1261436 *)

let rec detect_antivirus { Types.i_type = t; i_apps = apps } =
  assert (t = "windows");
  List.exists check_app apps

and check_app { Guestfs.app2_name = name;
                app2_publisher = publisher } =
  name      =~ rex_virus     ||
  name      =~ rex_kaspersky ||
  name      =~ rex_mcafee    ||
  name      =~ rex_norton    ||
  name      =~ rex_sophos    ||
  publisher =~ rex_avg_tech

and (=~) str rex =
  try ignore (Str.search_forward rex str 0); true with Not_found -> false

(* Copy the matching drivers to the driverdir; return true if any have
 * been copied.
 *)
type virtio_win_source =
  | Virtio_Win_Directory
  | Virtio_Win_ISO of Guestfs.guestfs

let rec copy_virtio_drivers g inspect virtio_win driverdir =
  (* Does $VIRTIO_WIN point to a directory or an ISO file? *)
  let virtio_win_source =
    if is_directory virtio_win then
      Some Virtio_Win_Directory
    else if is_regular_file virtio_win then (
      try
        let g2 = open_guestfs ~identifier:"virtio_win" () in
        g2#add_drive_opts virtio_win ~readonly:true;
        g2#launch ();
        g2#mount_ro "/dev/sda" "/";
        Some (Virtio_Win_ISO g2)
      with Guestfs.Error msg ->
        error (f_"%s: cannot open virtio-win ISO file: %s") virtio_win msg
    ) else
      None in

  match virtio_win_source with
  | None ->
     (* [$VIRTIO_WIN] does not point to a directory or regular file.  This
      * is not an error, but at the same time, no drivers were copied
      * so we return [false] here.
      *)
     false

  | Some virtio_win_source ->
     (* Find and load all the *.inf files from the virtio-win directory
      * or ISO.  Returns a list of pairs [(inf_content, inf_name, directory)]
      * where [inf_content] is the unparsed inf file, [inf_name] is a debug
      * string we can use in error messages to refer to the inf file,
      * and [directory] is a string used to track the directory containing
      * the inf file.
      *)
     let inf_files : (string * string * string) list =
       match virtio_win_source with
       | Virtio_Win_Directory ->
          let cmd =
            sprintf "find %s -name '*.inf' -type f" (quote virtio_win) in
          let paths = external_command cmd in
          List.map (fun path ->
                      read_whole_file path, path, Filename.dirname path) paths
       | Virtio_Win_ISO g2 ->
          let paths = g2#find "/" in
          let paths = Array.to_list paths in
          let paths =
            List.filter (
              fun path ->
                String.is_suffix path ".inf" &&
                  g2#is_file path ~followsymlinks:false
            ) paths in
          List.map (
            fun path ->
              let path = "/" ^ path in
              let i = String.rindex path '/' in
              let dir = String.sub path 0 i in
              g2#read_file path, sprintf "%s:%s" virtio_win path, dir
          ) paths in

     (* Get only the *.inf files which match the operating system. *)
     let inf_files =
       List.filter (
         fun (inf_content, inf_name, directory) ->
           virtio_inf_matches_guest_os inf_content inf_name inspect
       ) inf_files in

     (* If a directory contains any matching *.inf file, then we will
      * copy all the files from that directory.  So get the unique list
      * of directories that we will copy.
      *)
     let directories = List.map (fun (_,_,dir) -> dir) inf_files in
     let directories = sort_uniq directories in

     (* Copy the directories. *)
     List.iter (
       fun dir ->
         match virtio_win_source with
         | Virtio_Win_Directory -> copy_host_directory dir g driverdir
         | Virtio_Win_ISO g2 -> copy_iso_directory g2 dir g driverdir
     ) directories;

     (* Return true if some drivers were copied. *)
     directories <> []

(* Copy host files in same directory as inf_path to driverdir. *)
and copy_host_directory dir g driverdir =
  g#copy_in dir driverdir

(* Copy files from ISO in same directory as inf_path to driverdir. *)
and copy_iso_directory g2 dir g driverdir =
  let files = g2#find dir in
  let files = Array.to_list files in
  assert (files <> []); (* at least the .inf file must be here *)

  List.iter (
    fun filename ->
      let content = g2#read_file (dir ^ filename) in
      g#write (driverdir ^ "/" ^ filename) content
  ) files

(* Given the content of a [*.inf] file from the virtio-win drivers,
 * figure out if it's suitable for the specific Windows flavor of the
 * current guest.
 *)
and virtio_inf_matches_guest_os inf_content inf_name inspect =
  let sections = Windows_inf.of_string inf_content in

  (* Try to find the [Version] / DriverVer. *)
  let driver_ver = parse_driver_ver inf_name sections in

  (* Try to find the [Manufacturer] line. *)
  let driver_arch = parse_manufacturer inf_name sections in

  (* If we got both, we can continue, else give up. *)
  match driver_ver, driver_arch with
  | None, None | Some _, None | None, Some _ -> false
  | Some driver_ver, Some driver_arch ->
     (* XXX This ignores i_product_variant.  However that may not matter.
      * There appears to be no material difference in the inf file.
      *)
     let { Types.i_major_version = major; i_minor_version = minor;
           i_arch = arch } = inspect in

     let ver = major * 10 + minor in

     driver_ver = ver && driver_arch = arch

(* Find the [Version] section in the [*.inf] file, and find the
 * [DriverVer] from that, and parse it.  Returns [None] if we couldn't
 * find / parse it.
 * Reference: https://www.redhat.com/archives/libguestfs/2015-October/msg00352.html
 *)
and parse_driver_ver inf_name sections =
  try
    let driver_ver = Windows_inf.find_key sections "Version" "DriverVer" in
    if Str.string_match driver_ver_rex driver_ver 0 then
      Some (int_of_string (Str.matched_group 1 driver_ver))
    else
      raise Not_found
  with
    Not_found ->
      warning (f_"%s: could not find or parse the [Version] DriverVer key in the Windows inf file")
              inf_name;
      None

and driver_ver_rex =
  Str.regexp "[0-9/]+,\\([0-9]+\\)"

(* Find the [Manufacturer] section and try to find the first line.
 * There is no consistent naming of this line unfortunately.
 * Reference: https://www.redhat.com/archives/libguestfs/2015-November/msg00065.html
 *)
and parse_manufacturer inf_name sections =
  try
    let lines = Windows_inf.find_section sections "Manufacturer" in
    match lines with
    | [] -> raise Not_found
    | (_, manufacturer) :: _ ->
       if Str.string_match manufacturer_rex manufacturer 0 then (
         let arch = Str.matched_group 1 manufacturer in
         if arch = "x86" || arch = "X86" then Some "i386" else Some "x86_64"
       )
       else
         raise Not_found
  with
    Not_found ->
      warning (f_"%s: could not find or parse the [Manufacturer] section in the Windows inf file")
              inf_name;
      None

and manufacturer_rex =
  Str.regexp_case_fold ".*,NT\\(x86\\|amd64\\)"

(* The following function is only exported for unit tests. *)
module UNIT_TESTS = struct
  let virtio_inf_matches_guest_os = virtio_inf_matches_guest_os
end

(* This is a wrapper that handles opening and closing the hive
 * properly around a function [f].  If [~write] is [true] then the
 * hive is opened for writing and committed at the end if the
 * function returned without error.
 *)
type ('a, 'b) maybe = Either of 'a | Or of 'b

let with_hive (g : Guestfs.guestfs) hive_filename ~write f =
  let verbose = verbose () in
  g#hivex_open ~write ~verbose (* ~debug:verbose *) hive_filename;
  let r =
    try
      let root = g#hivex_root () in
      let ret = f root in
      if write then g#hivex_commit None;
      Either ret
    with exn ->
      Or exn in
  g#hivex_close ();
  match r with Either ret -> ret | Or exn -> raise exn

(* Find the given node in the current hive, relative to the starting
 * point.  Returns [None] if the node is not found.
 *)
let rec get_node (g : Guestfs.guestfs) node = function
  | [] -> Some node
  | x :: xs ->
     let node = g#hivex_node_get_child node x in
     if node = 0L then None
     else get_node g node xs
