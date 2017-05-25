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
open Unix

open Cmdline

let rec is_file ?(followsymlinks = false) path =
  let sysroot = sysroot () in
  let chroot = Chroot.create sysroot ~name:(sprintf "is_file: %s" path) in
  Chroot.f chroot get_kind (path, followsymlinks) = Some S_REG

and is_dir ?(followsymlinks = false) path =
  let sysroot = sysroot () in
  let chroot = Chroot.create sysroot ~name:(sprintf "is_dir: %s" path) in
  Chroot.f chroot get_kind (path, followsymlinks) = Some S_DIR

and is_symlink path =
  let sysroot = sysroot () in
  let chroot = Chroot.create sysroot ~name:(sprintf "is_symlink: %s" path) in
  Chroot.f chroot get_kind (path, false) = Some S_LNK

and get_kind (path, followsymlinks) =
  let statfun = if followsymlinks then stat else lstat in
  try
    let statbuf = statfun path in
    Some statbuf.st_kind
  with
    Unix_error ((ENOENT|ENOTDIR), _, _) ->
      None  (* File doesn't exist => return None *)
