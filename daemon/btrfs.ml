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

open Printf
open Scanf
open Unix

open Common_utils

open Mountable
open Utils

include Structs

(* In order to examine subvolumes, quota and other things, the btrfs
 * filesystem has to be mounted.  However we're passed a mountable
 * in these cases, so we must mount the filesystem.  But we cannot
 * mount it under the sysroot, as something else might be mounted
 * there so this function mounts the filesystem on a temporary
 * directory and ensures it is always unmounted afterwards.
 *)
let with_mounted mountable f =
  let tmpdir = sprintf "/tmp/%s" (String.random8 ()) in
  (* This is the cleanup function which is called to unmount and
   * remove the temporary directory.  This is called on error and
   * ordinary exit paths.
   *)
  let finally () =
    ignore (Sys.command (sprintf "umount %s" (quote tmpdir)));
    rmdir tmpdir
  in

  match mountable.m_type with
  | MountablePath ->
     (* This corner-case happens for Mountable_or_Path parameters, where
      * a path was supplied by the caller.  The path (the m_device
      * field) is relative to the sysroot.
      *)
     f (Sysroot.sysroot () // mountable.m_device)

  | MountableDevice ->
     protect ~finally ~f:(
       fun () ->
         mkdir tmpdir 0o700;
         ignore (command "mount" [mountable.m_device; tmpdir]);
         f tmpdir
     )

  | MountableBtrfsVol subvol ->
     protect ~finally ~f:(
       fun () ->
         mkdir tmpdir 0o700;
         ignore (command "mount" ["-o"; "subvol=" ^ subvol (* XXX quoting? *);
                                  mountable.m_device; tmpdir]);
         f tmpdir
     )

let re_btrfs_subvolume_list =
  Str.regexp ("ID[ \t]+\\([0-9]+\\).*[ \t]" ^
              "top level[ \t]+\\([0-9]+\\).*[ \t]" ^
              "path[ \t]+\\(.*\\)")

let btrfs_subvolume_list mountable =
  (* Execute 'btrfs subvolume list <fs>', and split the output into lines *)
  let lines =
    with_mounted mountable (
      fun mp -> command "btrfs" ["subvolume"; "list"; mp]
    ) in
  let lines = String.nsplit "\n" lines in

  (* Output is:
   *
   * ID 256 gen 30 top level 5 path test1
   * ID 257 gen 30 top level 5 path dir/test2
   * ID 258 gen 30 top level 5 path test3
   *
   * "ID <n>" is the subvolume ID.
   * "gen <n>" is the generation when the root was created or last
   * updated.
   * "top level <n>" is the top level subvolume ID.
   * "path <str>" is the subvolume path, relative to the top of the
   * filesystem.
   *
   * Note that the order that each of the above is fixed, but
   * different versions of btrfs may display different sets of data.
   * Specifically, older versions of btrfs do not display gen.
   *)
  filter_map (
    fun line ->
      if line = "" then None
      else if Str.string_match re_btrfs_subvolume_list line 0 then (
        let id = Int64.of_string (Str.matched_group 1 line)
        and top_level_id = Int64.of_string (Str.matched_group 2 line)
        and path = Str.matched_group 3 line in

        Some {
          btrfssubvolume_id = id;
          btrfssubvolume_top_level_id = top_level_id;
          btrfssubvolume_path = path
        }
      )
      else
        failwithf "unexpected output from 'btrfs subvolume list' command: %s"
                  line
  ) lines

let btrfs_subvolume_get_default mountable =
  let out =
    with_mounted mountable (
      fun mp -> command "btrfs" ["subvolume"; "get-default"; mp]
    ) in
  sscanf out "ID %Ld" identity
