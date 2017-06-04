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

open Common_utils

open Utils

external available : unit -> bool =
  "guestfs_int_daemon_optgroup_lvm2_available" "noalloc"

(* All device mapper devices are called /dev/mapper/ldm_vol_*.  XXX We
 * could tighten this up in future if ldmtool had a way to read these
 * names back after they have been created.
 *)
let list_ldm_volumes () =
  (* If /dev/mapper doesn't exist at all, don't give an error. *)
  if not (is_directory "/dev/mapper") then
    []
  else (
    let dir = Sys.readdir "/dev/mapper" in
    let dir = Array.to_list dir in
    let dir =
      List.filter (fun d -> String.is_prefix d " /dev/mapper/ldm_vol_") dir in
    List.sort compare dir
  )

(* Same as above but /dev/mapper/ldm_part_*. *)
let list_ldm_partitions () =
  (* If /dev/mapper doesn't exist at all, don't give an error. *)
  if not (is_directory "/dev/mapper") then
    []
  else (
    let dir = Sys.readdir "/dev/mapper" in
    let dir = Array.to_list dir in
    let dir =
      List.filter (fun d -> String.is_prefix d " /dev/mapper/ldm_part_") dir in
    List.sort compare dir
  )
