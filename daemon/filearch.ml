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

open Unix
open Printf

open Common_utils

open Utils

let re_file_elf =
  Str.regexp "ELF \\([0-9]+\\)-bit \\(MSB\\|LSB\\).*\\(executable\\|shared object\\|relocatable\\), \\([^,]+\\),"

let re_file_elf_ppc64 = Str.regexp ".*64.*PowerPC"

let initrd_binaries = [
  "bin/ls";
  "bin/rm";
  "bin/modprobe";
  "sbin/modprobe";
  "bin/sh";
  "bin/bash";
  "bin/dash";
  "bin/nash";
]

let rec file_architecture orig_path =
  (* Get the output of the "file" command.  Note that because this
   * is running in the daemon, LANG=C so it's in English.
   *)
  let magic = File.file orig_path in
  file_architecture_of_magic magic orig_path orig_path

and file_architecture_of_magic magic orig_path path =
  if Str.string_match re_file_elf magic 0 then (
    let bits = Str.matched_group 1 magic in
    let endianness = Str.matched_group 2 magic in
    let elf_arch = Str.matched_group 4 magic in
    canonical_elf_arch bits endianness elf_arch
  )
  else if String.find magic "PE32 executable" >= 0 then
    "i386"
  else if String.find magic "PE32+ executable" >= 0 then
    "x86_64"
  else if String.find magic "cpio archive" >= 0 then
    cpio_arch magic orig_path path
  else
    failwithf "unknown architecture: %s" path

(* Convert output from 'file' command on ELF files to the canonical
 * architecture string.  Caller must free the result.
 *)
and canonical_elf_arch bits endianness elf_arch =
  let substr s = String.find elf_arch s >= 0 in
  if substr "Intel 80386" || substr "Intel 80486" then
    "i386"
  else if substr "x86-64" || substr "AMD x86-64" then
    "x86_64"
  else if substr "SPARC32" then
    "sparc"
  else if substr "SPARC V9" then
    "sparc64"
  else if substr "IA-64" then
    "ia64"
  else if Str.string_match re_file_elf_ppc64 elf_arch 0 then (
    match endianness with
    | "MSB" -> "ppc64"
    | "LSB" -> "ppc64le"
    | _ -> failwithf "unknown endianness '%s'" endianness
  )
  else if substr "PowerPC" then
    "ppc"
  else if substr "ARM aarch64" then
    "aarch64"
  else if substr "ARM" then
    "arm"
  else if substr "UCB RISC-V" then
    sprintf "riscv%s" bits
  else if substr "IBM S/390" then (
    match bits with
    | "32" -> "s390"
    | "64" -> "s390x"
    | _ -> failwithf "unknown S/390 bit size: %s" bits
  )
  else
    elf_arch

and cpio_arch magic orig_path path =
  let sysroot = Sysroot.sysroot () in

  let zcat =
    if String.find magic "gzip" >= 0 then "zcat"
    else if String.find magic "bzip2" >= 0 then "bzcat"
    else if String.find magic "XZ compressed" >= 0 then "xzcat"
    else "cat" in

  let tmpdir = sprintf "/tmp/%s" (String.random8 ()) in
  mkdir tmpdir 0o700;

  (* Construct a command to extract named binaries from the initrd file. *)
  let cmd =
    sprintf "cd %s && %s %s | cpio --quiet -id %s"
            tmpdir zcat (quote (sysroot // path))
            (String.concat " " (List.map quote initrd_binaries)) in
  if verbose () then eprintf "%s\n%!" cmd;
  if Sys.command cmd <> 0 then
    failwith "cpio command failed";

  (* See if any of the binaries were present in the output. *)
  let rec loop = function
    | bin :: bins ->
       let bin_path = tmpdir // bin in
       if is_regular_file bin_path then (
         let out = command "file" ["-zb"; bin_path] in
         file_architecture_of_magic out orig_path bin_path
       )
       else
         loop bins
    | [] ->
       failwithf "could not determine architecture of cpio archive: %s" path
  in
  loop initrd_binaries
