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

open Common_utils

open Utils
open Cmdline
open Inspection_types

let rec main () =
  (* Handle the command line. *)
  parse_cmdline ();

  (* Print the version so it appears in verbose output. *)
  if verbose () then
    eprintf "%s: %s %s (%s)\n%!"
            prog
            Guestfs_config.package_name Guestfs_config.package_version_full
            Guestfs_config.host_cpu;

  (* Check the mountpoint directory exists.  This is also a good
   * place to stop users trying to run the command by hand.
   *)
  let sysroot = sysroot () in
  if not (is_directory sysroot) then
    failwithf "sysroot directory (%s) does not exist.
Note that if you are trying to run this program by hand on a host, then
you probably shouldn't be doing that."
              sysroot;

  Mount.umount_all ();

  (* Iterate over all detected filesystems.  Inspect each one in turn. *)
  let fses = Listfs.list_filesystems () in

  let fses =
    filter_map (
      fun (mountable, vfs_type) ->
        Inspection_fs.check_for_filesystem_on mountable vfs_type
  ) fses in
  if verbose () then (
    eprintf "fses:\n";
    List.iter (fun fs -> eprintf "\t%s\n" (string_of_fs fs)) fses
  );
  flush stderr;

(* XXX
  (* The OS inspection information for CoreOS are gathered by inspecting
   * multiple filesystems. Gather all the inspected information in the
   * inspect_fs struct of the root filesystem.
   *)
  let fses = collect_coreos_inspection_info fses in

  (* Check if the same filesystem was listed twice as root in fses.
   * This may happen for the *BSD root partition where an MBR partition
   * is a shadow of the real root partition probably /dev/sda5
   *)
  let fses = check_for_duplicated_bsd_root fses in

  (* For Linux guests with a separate /usr filesystem, merge some of the
   * inspected information in that partition to the inspect_fs struct
   * of the root filesystem.
   *)
  let fses = collect_linux_inspection_info fses in
 *)

  (* At this point we have, in the handle, a list of all filesystems
   * found and data about each one.  Now we assemble the list of
   * filesystems which are root devices.
   *)
  let roots = get_roots fses in
  if verbose () then (
    eprintf "roots:\n";
    List.iter (fun root -> eprintf "%s" (string_of_root root)) roots
  );
  flush stderr

and get_roots =
  filter_map (
    function
    | { fs_location = location; role = RoleRoot data } ->
       Some { root_location = location; inspection_data = data }
    | { role = (RoleUsr _ | RoleSwap | RoleOther) } ->
       None
  )

let () = run_main_and_handle_errors main
