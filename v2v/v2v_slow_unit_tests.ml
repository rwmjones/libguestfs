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

(* Test individual virt-v2v utility functions for tests which
 * are long running.
 *)

open Printf

open OUnit2

open Std_utils
open Tools_utils

open Types

let get_g_i filename =
  let g = open_guestfs () in
  g#add_drive filename ~readonly:true;
  g#launch ();
  let inspect = Inspect_source.inspect_source SingleRoot g in
  g, inspect

let fedora_g, fedora_i = get_g_i "v2v-slow-unit-tests-fedora-26.img"
let debian_g, debian_i = get_g_i "v2v-slow-unit-tests-debian-9.img"

let test_is_file_owned ctx =
  let test g i filename expected =
    let printer = string_of_bool in
    let msg =
      sprintf "Linux.is_file_owned %S (expecting %b)" filename expected in
    assert_equal ~printer ~msg
                 expected (Linux.is_file_owned g i filename)
  in

  test fedora_g fedora_i "/etc/passwd" true;
  (* /etc/passwd is not in fact owned in Debian. *)
  test debian_g debian_i "/etc/debian_version" true;
  test fedora_g fedora_i "/var/log" true;
  test debian_g debian_i "/var/log" true;

  test fedora_g fedora_i "/tmp/notowned" false;
  test debian_g debian_i "/tmp/notowned" false;
  test fedora_g fedora_i "/tmp/notowned2" false;
  test debian_g debian_i "/tmp/notowned2" false;

  ()

(* Suites declaration. *)
let suite =
  "virt-v2v" >:::
    [
      "Linux.is_file_owned" >:: test_is_file_owned;
    ]

let () =
  run_test_tt_main suite
