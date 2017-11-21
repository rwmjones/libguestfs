(* virt-v2v
 * Copyright (C) 2017 Red Hat Inc.
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

open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext

open Types

let random_token =
  (* This is meant to be typed, so omit some homoglyphs.  There are
   * about 100 million combinations, and an attacker would have to
   * be on the same network.
   *)
  let chars = "ABCDEFGHJKLMNPRSTUWXYZ" in
  let len = Live_protocol_wrapper.token_length in
  fun () -> Urandom.urandom_uniform len chars

class input_live =
  (* Create a temporary directory where we will spool the disk.
   * XXX In the final design we will allow this directory to be
   * specified on the command line so that we can do in-place
   * conversions efficiently.
   *)
(*
  let spooldir =
    let base_dir = (open_guestfs ())#get_cachedir () in
    let t = Mkdtemp.temp_dir ~base_dir "live." in
    rmdir_on_exit t;
    t in
 *)

object
  inherit input

  method as_options = "-i live"

  method source () =
    let conn = Live.create () in
    let port = Live.port conn in

    (* Create the short random token that the user must type on the
     * client.  The port number is encoded in the token so that the
     * client knows which port to try to connect on.
     * XXX Allow the token & port to be passed on the command line.
     *)
    let token = sprintf "%s-%d" (random_token ()) port in
    printf "Token: %s\n" token;
    info (f_"Start the live conversion client on the source and at the prompt enter the IP address or host name of this server and the token.");

    info (f_"Waiting for a connection ...");
    Live.negotiate conn token;

    info (f_"Negotiated connection with %s") (Live.peername conn);







    let source = {
      (* XXX SET THIS FROM CLIENT *)
      s_hypervisor = UnknownHV;
      s_name = "name"; s_orig_name = "name";
      s_memory = 2048L *^ 1024L *^ 1024L; (* 2048 MB *)
      s_vcpu = 1;                         (* 1 vCPU is a safe default *)
      s_cpu_vendor = None;
      s_cpu_model = None;
      s_cpu_sockets = None;
      s_cpu_cores = None;
      s_cpu_threads = None;
      s_features = [ "acpi"; "apic"; "pae" ];
      s_firmware = UnknownFirmware;       (* causes virt-v2v to autodetect *)
      s_display = None;
      s_video = None;
      s_sound = None;
      s_disks = [];
      s_removables = [];
      s_nics = [];
    } in

    source

end

let input_live () = new input_live
let () = Modules_list.register_input_module "live"
