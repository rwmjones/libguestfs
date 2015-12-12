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

(* Command line argument parsing. *)

open Common_utils
open Common_gettext.Gettext

open Printf

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

let parse_cmdline () =
  let add xs s = xs := s :: !xs in

  let align_first = ref "auto" in
  let alignment = ref 128 in
  let copy_boot_loader = ref true in
  let deletes = ref [] in
  let dryrun = ref false in
  let expand = ref "" in
  let set_expand s =
    if s = "" then error (f_"empty --expand option")
    else if !expand <> "" then error (f_"--expand option given twice")
    else expand := s
  in
  let expand_content = ref true in
  let extra_partition = ref true in
  let format = ref "" in
  let ignores = ref [] in
  let lv_expands = ref [] in
  let machine_readable = ref false in
  let ntfsresize_force = ref false in
  let output_format = ref "" in
  let resizes = ref [] in
  let resizes_force = ref [] in
  let shrink = ref "" in
  let set_shrink s =
    if s = "" then error (f_"empty --shrink option")
    else if !shrink <> "" then error (f_"--shrink option given twice")
    else shrink := s
  in
  let sparse = ref true in
  let unknown_fs_mode = ref "warn" in

  let ditto = " -\"-" in
  let argspec = [
      "--align-first", Arg.Set_string align_first, s_"never|always|auto" ^ " " ^ s_"Align first partition (default: auto)";
      "--alignment", Arg.Set_int alignment,   s_"sectors" ^ " " ^ s_"Set partition alignment (default: 128 sectors)";
      "--no-copy-boot-loader", Arg.Clear copy_boot_loader, " " ^ s_"Don't copy boot loader";
      "-d",        Arg.Unit set_verbose,      " " ^ s_"Enable debugging messages";
      "--debug",   Arg.Unit set_verbose,      ditto;
      "--delete",  Arg.String (add deletes),  s_"part" ^ " " ^ s_"Delete partition";
      "--expand",  Arg.String set_expand,     s_"part" ^ " " ^ s_"Expand partition";
      "--no-expand-content", Arg.Clear expand_content, " " ^ s_"Don't expand content";
      "--no-extra-partition", Arg.Clear extra_partition, " " ^ s_"Don't create extra partition";
      "--format",  Arg.Set_string format,     s_"format" ^ " " ^ s_"Format of input disk";
      "--ignore",  Arg.String (add ignores),  s_"part" ^ " " ^ s_"Ignore partition";
      "--lv-expand", Arg.String (add lv_expands), s_"lv" ^ " " ^ s_"Expand logical volume";
      "--LV-expand", Arg.String (add lv_expands), s_"lv" ^ ditto;
      "--lvexpand", Arg.String (add lv_expands), s_"lv" ^ ditto;
      "--LVexpand", Arg.String (add lv_expands), s_"lv" ^ ditto;
      "--machine-readable", Arg.Set machine_readable, " " ^ s_"Make output machine readable";
      "-n",        Arg.Set dryrun,            " " ^ s_"Don't perform changes";
      "--dry-run", Arg.Set dryrun,            " " ^ s_"Don't perform changes";
      "--dryrun",  Arg.Set dryrun,            ditto;
      "--ntfsresize-force", Arg.Set ntfsresize_force, " " ^ s_"Force ntfsresize";
      "--output-format", Arg.Set_string output_format, s_"format" ^ " " ^ s_"Format of output disk";
      "--resize",  Arg.String (add resizes),  s_"part=size" ^ " " ^ s_"Resize partition";
      "--resize-force", Arg.String (add resizes_force), s_"part=size" ^ " " ^ s_"Forcefully resize partition";
      "--shrink",  Arg.String set_shrink,     s_"part" ^ " " ^ s_"Shrink partition";
      "--no-sparse", Arg.Clear sparse,        " " ^ s_"Turn off sparse copying";
      "--unknown-filesystems", Arg.Set_string unknown_fs_mode,
                                              s_"ignore|warn|error" ^ " " ^ s_"Behaviour on expand unknown filesystems (default: warn)";
    ] in
  let argspec = set_standard_options argspec in
  let disks = ref [] in
  let anon_fun s = disks := s :: !disks in
  let usage_msg =
    sprintf (f_"\
%s: resize a virtual machine disk

A short summary of the options is given below.  For detailed help please
read the man page virt-resize(1).
")
            prog in
  Arg.parse argspec anon_fun usage_msg;

  if verbose () then (
    printf "command line:";
    List.iter (printf " %s") (Array.to_list Sys.argv);
    print_newline ()
  );

  (* Dereference the rest of the args. *)
  let alignment = !alignment in
  let copy_boot_loader = !copy_boot_loader in
  let deletes = List.rev !deletes in
  let dryrun = !dryrun in
  let expand = match !expand with "" -> None | str -> Some str in
  let expand_content = !expand_content in
  let extra_partition = !extra_partition in
  let format = match !format with "" -> None | str -> Some str in
  let ignores = List.rev !ignores in
  let lv_expands = List.rev !lv_expands in
  let machine_readable = !machine_readable in
  let ntfsresize_force = !ntfsresize_force in
  let output_format = match !output_format with "" -> None | str -> Some str in
  let resizes = List.rev !resizes in
  let resizes_force = List.rev !resizes_force in
  let shrink = match !shrink with "" -> None | str -> Some str in
  let sparse = !sparse in
  let unknown_fs_mode = !unknown_fs_mode in

  if alignment < 1 then
    error (f_"alignment cannot be < 1");
  let alignment = Int64.of_int alignment in

  let align_first =
    match !align_first with
    | "never" -> `Never
    | "always" -> `Always
    | "auto" -> `Auto
    | _ ->
       error (f_"unknown --align-first option: use never|always|auto") in

  let unknown_fs_mode =
    match unknown_fs_mode with
    | "ignore" -> UnknownFsIgnore
    | "warn" -> UnknownFsWarn
    | "error" -> UnknownFsError
    | _ ->
       error (f_"unknown --unknown-filesystems: use ignore|warn|error") in

  (* No arguments and machine-readable mode?  Print out some facts
   * about what this binary supports.  We only need to print out new
   * things added since this option, or things which depend on features
   * of the appliance.
   *)
  if !disks = [] && machine_readable then (
    printf "virt-resize\n";
    printf "ntfsresize-force\n";
    printf "32bitok\n";
    printf "128-sector-alignment\n";
    printf "alignment\n";
    printf "align-first\n";
    printf "infile-uri\n";
    let g = open_guestfs () in
    g#add_drive "/dev/null";
    g#launch ();
    if g#feature_available [| "ntfsprogs"; "ntfs3g" |] then
      printf "ntfs\n";
    if g#feature_available [| "btrfs" |] then
      printf "btrfs\n";
    if g#feature_available [| "xfs" |] then
      printf "xfs\n";
    exit 0
  );

  (* Verify we got exactly 2 disks. *)
  let infile, outfile =
    match List.rev !disks with
    | [infile; outfile] -> infile, outfile
    | _ ->
       error (f_"usage is: %s [--options] indisk outdisk") prog in

  (* Simple-minded check that the user isn't trying to use the
   * same disk for input and output.
   *)
  if infile = outfile then
    error (f_"you cannot use the same disk image for input and output");

  (* infile can be a URI. *)
  let infile, infile_uri =
    try infile, URI.parse_uri infile
    with Invalid_argument "URI.parse_uri" ->
      error (f_"error parsing URI '%s'. Look for error messages printed above.")
            infile in

  {
    infile = infile;
    infile_uri = infile_uri;
    outfile = outfile;
    align_first = align_first;
    alignment = alignment;
    copy_boot_loader = copy_boot_loader;
    deletes = deletes;
    dryrun = dryrun;
    expand = expand;
    expand_content = expand_content;
    extra_partition = extra_partition;
    format = format;
    ignores = ignores;
    lv_expands = lv_expands;
    machine_readable = machine_readable;
    ntfsresize_force = ntfsresize_force;
    output_format = output_format;
    resizes = resizes;
    resizes_force = resizes_force;
    shrink = shrink;
    sparse = sparse;
    unknown_fs_mode = unknown_fs_mode;
  }
