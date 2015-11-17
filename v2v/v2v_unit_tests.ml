(* virt-v2v
 * Copyright (C) 2011-2015 Red Hat Inc.
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

(* This file tests individual virt-v2v functions. *)

open Printf
open OUnit2

open Common_utils

open Types

external identity : 'a -> 'a = "%identity"

let (//) = Filename.concat

let srcdir =
  try Sys.getenv "srcdir"
  with Not_found -> failwith "environment variable $srcdir must be set"

let inspect_defaults = {
  i_type = ""; i_distro = ""; i_arch = "";
  i_major_version = 0; i_minor_version = 0;
  i_root = ""; i_package_format = ""; i_package_management = "";
  i_product_name = ""; i_product_variant = ""; i_mountpoints = [];
  i_apps = []; i_apps_map = StringMap.empty; i_uefi = false
}

let test_get_ostype ctx =
  let printer = identity in
  assert_equal ~printer "RHEL6"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "linux"; i_distro = "rhel";
                                 i_major_version = 6;
                                 i_minor_version = 0;
                                 i_arch = "i386" });
  assert_equal ~printer "RHEL6x64"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "linux"; i_distro = "rhel";
                                 i_major_version = 6;
                                 i_minor_version = 0;
                                 i_arch = "x86_64" });
  assert_equal ~printer "rhel_7x64"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "linux"; i_distro = "rhel";
                                 i_major_version = 7;
                                 i_minor_version = 0;
                                 i_arch = "x86_64" });
  assert_equal ~printer "Windows7"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "windows";
                                 i_major_version = 6;
                                 i_minor_version = 1;
                                 i_product_variant = "Client";
                                 i_arch = "i386" });
  assert_equal ~printer "Windows7x64"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "windows";
                                 i_major_version = 6;
                                 i_minor_version = 1;
                                 i_product_variant = "Client";
                                 i_arch = "x86_64" });
  assert_equal ~printer "windows_8"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "windows";
                                 i_major_version = 6;
                                 i_minor_version = 2;
                                 i_product_variant = "Client";
                                 i_arch = "i386" });
  assert_equal ~printer "windows_8x64"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "windows";
                                 i_major_version = 6;
                                 i_minor_version = 2;
                                 i_product_variant = "Client";
                                 i_arch = "x86_64" });
  assert_equal ~printer "windows_2012x64"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "windows";
                                 i_major_version = 6;
                                 i_minor_version = 2;
                                 i_product_variant = "Server";
                                 i_arch = "x86_64" });
  assert_equal ~printer "windows_2012R2x64"
               (OVF.get_ostype { inspect_defaults with
                                 i_type = "windows";
                                 i_major_version = 6;
                                 i_minor_version = 3;
                                 i_product_variant = "Server";
                                 i_arch = "x86_64" })

let test_drive_name ctx =
  let printer = identity in
  assert_equal ~printer "a" (Utils.drive_name 0);
  assert_equal ~printer "z" (Utils.drive_name 25);
  assert_equal ~printer "aa" (Utils.drive_name 26);
  assert_equal ~printer "ab" (Utils.drive_name 27);
  assert_equal ~printer "az" (Utils.drive_name 51);
  assert_equal ~printer "ba" (Utils.drive_name 52);
  assert_equal ~printer "zz" (Utils.drive_name 701);
  assert_equal ~printer "aaa" (Utils.drive_name 702);
  assert_equal ~printer "zzz" (Utils.drive_name 18277)

let test_drive_index ctx =
  let printer = string_of_int in
  assert_equal ~printer 0 (Utils.drive_index "a");
  assert_equal ~printer 25 (Utils.drive_index "z");
  assert_equal ~printer 26 (Utils.drive_index "aa");
  assert_equal ~printer 27 (Utils.drive_index "ab");
  assert_equal ~printer 51 (Utils.drive_index "az");
  assert_equal ~printer 52 (Utils.drive_index "ba");
  assert_equal ~printer 701 (Utils.drive_index "zz");
  assert_equal ~printer 702 (Utils.drive_index "aaa");
  assert_equal ~printer 18277 (Utils.drive_index "zzz");
  let exn = Invalid_argument "drive_index: invalid parameter" in
  assert_raises exn (fun () -> Utils.drive_index "");
  assert_raises exn (fun () -> Utils.drive_index "abc123");
  assert_raises exn (fun () -> Utils.drive_index "123");
  assert_raises exn (fun () -> Utils.drive_index "Z");
  assert_raises exn (fun () -> Utils.drive_index "aB")

(* Test parsing a [*.inf] file. *)
let test_windows_inf_of_string ctx =
  let printer = Windows_inf.to_string in

  (* There is nothing special about this choice.  It is just a driver
   * [*.inf] file picked at random.
   *)
  let path = srcdir // ".." // "test-data" // "fake-virtio-win" //
               "cd" // "Balloon" // "2k12" // "amd64" // "balloon.inf" in

  let sections = Windows_inf.load path in

  let expected = [
    "version", [
      "signature", "\"$WINDOWS NT$\"";
      "class", "System";
      "classguid", "{4d36e97d-e325-11ce-bfc1-08002be10318}";
      "provider", "%RHEL%";
      "driverver", "12/04/2014,62.71.104.9600";
      "catalogfile", "Balloon.cat";
      "driverpackagetype", "PlugAndPlay";
      "driverpackagedisplayname", "%BALLOON.DeviceDesc%";
      "pnplockdown", "1";
    ];
    "destinationdirs", [
      "defaultdestdir", "12";
    ];
    "sourcedisksnames", [
      "1", "%DiskId1%,,,\"\"";
    ];
    "sourcedisksfiles", [
      "balloon.sys", "1,,";
    ];
    "manufacturer", [
      "%rhel%", "Standard,NTamd64";
    ];
    "standard", [
      "%balloon.devicedesc%", "BALLOON_Device, PCI\\VEN_1AF4&DEV_1002&SUBSYS_00051AF4&REV_00";
    ];
    "standard.ntamd64", [
      "%balloon.devicedesc%", "BALLOON_Device, PCI\\VEN_1AF4&DEV_1002&SUBSYS_00051AF4&REV_00";
    ];
    "balloon_device.nt", [
      "copyfiles", "Drivers_Dir";
    ];
    "drivers_dir", [];
    "balloon_device.nt.services", [
      "addservice", "BALLOON,%SPSVCINST_ASSOCSERVICE%, BALLOON_Service_Inst, BALLOON_Logging_Inst";
    ];
    "balloon_service_inst", [
      "displayname", "%BALLOON.SVCDESC%";
      "servicetype", "1";
      "starttype", "3";
      "errorcontrol", "1";
      "servicebinary", "%12%\\balloon.sys";
    ];
    "balloon_logging_inst", [
      "addreg", "BALLOON_Logging_Inst_AddReg";
    ];
    "balloon_logging_inst_addreg", [];
    "destinationdirs", [
      "balloon_device_coinstaller_copyfiles", "11";
    ];
    "balloon_device.nt.coinstallers", [
      "addreg", "BALLOON_Device_CoInstaller_AddReg";
      "copyfiles", "BALLOON_Device_CoInstaller_CopyFiles";
    ];
    "balloon_device_coinstaller_addreg", [];
    "balloon_device_coinstaller_copyfiles", [];
    "sourcedisksfiles", [
      "wdfcoinstaller01011.dll", "1";
    ];
    "balloon_device.nt.wdf", [
      "kmdfservice", "BALLOON, BALLOON_wdfsect";
    ];
    "balloon_wdfsect", [
      "kmdflibraryversion", "1.11";
    ];
    "strings", [
      "spsvcinst_assocservice", "0x00000002";
      "rhel", "\"Red Hat, Inc.\"";
      "diskid1", "\"VirtIO Balloon Installation Disk #1\"";
      "balloon.devicedesc", "\"VirtIO Balloon Driver\"";
      "balloon.svcdesc", "\"VirtIO Balloon Service\"";
      "classname", "\"VirtIO Balloon Device\"";
    ];
  ] in

  assert_equal ~printer expected sections

(* Test the code which matches [*.inf] files to Windows guests. *)
let test_virtio_inf_matches_guest_os ctx =
  (* Windows OSes fake inspection data. *)
  let make_win name major minor variant arch = {
    inspect_defaults with
    i_product_name = name; i_product_variant = variant;
    i_major_version = major; i_minor_version = minor; i_arch = arch;
  } in
  let winxp_32 =     make_win "winxp_32"     5 1 "Client" "i386" in
  let winxp_64 =     make_win "winxp_64"     5 1 "Client" "x86_64" in
  let win2k3_32 =    make_win "win2k3_32"    5 2 "Server" "i386" in
  let win2k3_64 =    make_win "win2k3_64"    5 2 "Server" "x86_64" in
  let winvista_32 =  make_win "winvista_32"  6 0 "Client" "i386" in
  let winvista_64 =  make_win "winvista_64"  6 0 "Client" "x86_64" in
  let win2k8_32 =    make_win "win2k8_32"    6 0 "Server" "i386" in
  let win2k8_64 =    make_win "win2k8_64"    6 0 "Server" "x86_64" in
  let win7_32 =      make_win "win7_32"      6 1 "Client" "i386" in
  let win7_64 =      make_win "win7_64"      6 1 "Client" "x86_64" in
  let win2k8r2_32 =  make_win "win2k8r2_32"  6 1 "Server" "i386" in
  let win2k8r2_64 =  make_win "win2k8r2_64"  6 1 "Server" "x86_64" in
  let win8_32 =      make_win "win8_32"      6 2 "Client" "i386" in
  let win8_64 =      make_win "win8_64"      6 2 "Client" "x86_64" in
  let win2k12_32 =   make_win "win2k12_32"   6 2 "Server" "i386" in
  let win2k12_64 =   make_win "win2k12_64"   6 2 "Server" "x86_64" in
  let win8_1_32 =    make_win "win8_1_32"    6 3 "Client" "i386" in
  let win8_1_64 =    make_win "win8_1_64"    6 3 "Client" "x86_64" in
  let win2k12r2_32 = make_win "win2k12r2_32" 6 3 "Server" "i386" in
  let win2k12r2_64 = make_win "win2k12r2_64" 6 3 "Server" "x86_64" in
  let win10_32 =     make_win "win10_32"     10 0 "Client" "i386" in
  let win10_64 =     make_win "win10_64"     10 0 "Client" "x86_64" in
  let all_windows = [
    winxp_32; win2k3_32; winvista_32; win2k8_32; win7_32; win2k8r2_32;
    win8_32; win2k12_32; win8_1_32; win2k12r2_32; win10_32;
    winxp_64; win2k3_64; winvista_64; win2k8_64; win7_64; win2k8r2_64;
    win8_64; win2k12_64; win8_1_64; win2k12r2_64; win10_64
  ] in

  let paths = [
    (* Paths relative to $srcdir/test-data/fake-virtio-win. *)
    "cd/Balloon/2k12/amd64/balloon.inf", Some win2k12_64;
    "cd/Balloon/2k12R2/amd64/balloon.inf", Some win2k12r2_64;
    "cd/Balloon/2k3/amd64/balloon.inf", Some win2k3_64;
    "cd/Balloon/2k3/x86/balloon.inf", Some win2k3_32;
    "cd/Balloon/2k8/amd64/balloon.inf", Some win2k8_64;
    "cd/Balloon/2k8/x86/balloon.inf", Some win2k8_32;
    "cd/Balloon/2k8R2/amd64/balloon.inf", Some win2k8r2_64;
    "cd/Balloon/w7/amd64/balloon.inf", Some win7_64;
    "cd/Balloon/w7/x86/balloon.inf", Some win7_32;
    "cd/Balloon/w8.1/amd64/balloon.inf", Some win8_1_64;
    "cd/Balloon/w8.1/x86/balloon.inf", Some win8_1_32;
    "cd/Balloon/w8/amd64/balloon.inf", Some win8_64;
    "cd/Balloon/w8/x86/balloon.inf", Some win8_32;
    "cd/Balloon/xp/x86/balloon.inf", Some winxp_32;
    "cd/NetKVM/2k12/amd64/netkvm.inf", Some win2k12_64;
    "cd/NetKVM/2k12R2/amd64/netkvm.inf", Some win2k12r2_64;
    "cd/NetKVM/2k3/amd64/netkvm.inf", Some win2k3_64;
    "cd/NetKVM/2k3/x86/netkvm.inf", Some win2k3_32;
    "cd/NetKVM/2k8/amd64/netkvm.inf", Some win2k8_64;
    "cd/NetKVM/2k8/x86/netkvm.inf", Some win2k8_32;
    "cd/NetKVM/2k8R2/amd64/netkvm.inf", Some win2k8r2_64;
    "cd/NetKVM/w7/amd64/netkvm.inf", Some win7_64;
    "cd/NetKVM/w7/x86/netkvm.inf", Some win7_32;
    "cd/NetKVM/w8.1/amd64/netkvm.inf", Some win8_1_64;
    "cd/NetKVM/w8.1/x86/netkvm.inf", Some win8_1_32;
    "cd/NetKVM/w8/amd64/netkvm.inf", Some win8_64;
    "cd/NetKVM/w8/x86/netkvm.inf", Some win8_32;
    "cd/NetKVM/xp/x86/netkvm.inf", Some winxp_32;
    "cd/qemupciserial/qemupciserial.inf", None;
    "cd/viorng/2k12/amd64/viorng.inf", Some win2k12_64;
    "cd/viorng/2k12R2/amd64/viorng.inf", Some win2k12r2_64;
    "cd/viorng/2k8/amd64/viorng.inf", Some win2k8_64;
    "cd/viorng/2k8/x86/viorng.inf", Some win2k8_32;
    "cd/viorng/2k8R2/amd64/viorng.inf", Some win2k8r2_64;
    "cd/viorng/w7/amd64/viorng.inf", Some win7_64;
    "cd/viorng/w7/x86/viorng.inf", Some win7_32;
    "cd/viorng/w8.1/amd64/viorng.inf", Some win8_1_64;
    "cd/viorng/w8.1/x86/viorng.inf", Some win8_1_32;
    "cd/viorng/w8/amd64/viorng.inf", Some win8_64;
    "cd/viorng/w8/x86/viorng.inf", Some win8_32;
    "cd/vioscsi/2k12/amd64/vioscsi.inf", Some win2k12_64;
    "cd/vioscsi/2k12R2/amd64/vioscsi.inf", Some win2k12r2_64;
    "cd/vioscsi/2k8/amd64/vioscsi.inf", Some win2k8_64;
    "cd/vioscsi/2k8/x86/vioscsi.inf", Some win2k8_32;
    "cd/vioscsi/2k8R2/amd64/vioscsi.inf", Some win2k8r2_64;
    "cd/vioscsi/w7/amd64/vioscsi.inf", Some win7_64;
    "cd/vioscsi/w7/x86/vioscsi.inf", Some win7_32;
    "cd/vioscsi/w8.1/amd64/vioscsi.inf", Some win8_1_64;
    "cd/vioscsi/w8.1/x86/vioscsi.inf", Some win8_1_32;
    "cd/vioscsi/w8/amd64/vioscsi.inf", Some win8_64;
    "cd/vioscsi/w8/x86/vioscsi.inf", Some win8_32;
    "cd/vioserial/2k12/amd64/vioser.inf", Some win2k12_64;
    "cd/vioserial/2k12R2/amd64/vioser.inf", Some win2k12r2_64;
    "cd/vioserial/2k3/amd64/vioser.inf", Some win2k3_64;
    "cd/vioserial/2k3/x86/vioser.inf", Some win2k3_32;
    "cd/vioserial/2k8/amd64/vioser.inf", Some win2k8_64;
    "cd/vioserial/2k8/x86/vioser.inf", Some win2k8_32;
    "cd/vioserial/2k8R2/amd64/vioser.inf", Some win2k8r2_64;
    "cd/vioserial/w7/amd64/vioser.inf", Some win7_64;
    "cd/vioserial/w7/x86/vioser.inf", Some win7_32;
    "cd/vioserial/w8.1/amd64/vioser.inf", Some win8_1_64;
    "cd/vioserial/w8.1/x86/vioser.inf", Some win8_1_32;
    "cd/vioserial/w8/amd64/vioser.inf", Some win8_64;
    "cd/vioserial/w8/x86/vioser.inf", Some win8_32;
    "cd/vioserial/w8/x86/vioser.pdb", Some win8_32;
    "cd/vioserial/xp/x86/vioser.inf", Some winxp_32;
    "cd/viostor/2k12/amd64/viostor.inf", Some win2k12_64;
    "cd/viostor/2k12R2/amd64/viostor.inf", Some win2k12r2_64;
    "cd/viostor/2k3/amd64/viostor.inf", Some win2k3_64;
    "cd/viostor/2k3/x86/viostor.inf", Some win2k3_32;
    "cd/viostor/2k8/amd64/viostor.inf", Some win2k8_64;
    "cd/viostor/2k8/x86/viostor.inf", Some win2k8_32;
    "cd/viostor/2k8R2/amd64/viostor.inf", Some win2k8r2_64;
    "cd/viostor/w7/amd64/viostor.inf", Some win7_64;
    "cd/viostor/w7/x86/viostor.inf", Some win7_32;
    "cd/viostor/w8.1/amd64/viostor.inf", Some win8_1_64;
    "cd/viostor/w8.1/x86/viostor.inf", Some win8_1_32;
    "cd/viostor/w8/amd64/viostor.inf", Some win8_64;
    "cd/viostor/w8/x86/viostor.inf", Some win8_32;
    "cd/viostor/xp/x86/viostor.inf", Some winxp_32;

    "drivers/i386/Win8.1/viostor.inf", Some win8_1_32;
    "drivers/i386/Win8.1/netkvm.inf", Some win8_1_32;
    "drivers/i386/Win8.1/vioscsi.inf", Some win8_1_32;
    "drivers/i386/Win2008/viostor.inf", Some win2k8_32;
    "drivers/i386/Win2008/netkvm.inf", Some win2k8_32;
    "drivers/i386/Win2008/vioscsi.inf", Some win2k8_32;
    "drivers/i386/Win7/viostor.inf", Some win7_32;
    "drivers/i386/Win7/netkvm.inf", Some win7_32;
    "drivers/i386/Win7/qxl.inf", Some win7_32;
    "drivers/i386/Win7/vioscsi.inf", Some win7_32;
    "drivers/i386/Win2003/viostor.inf", Some win2k3_32;
    "drivers/i386/Win2003/netkvm.inf", Some win2k3_32;
    "drivers/i386/Win8/viostor.inf", Some win8_32;
    "drivers/i386/Win8/netkvm.inf", Some win8_32;
    "drivers/i386/Win8/vioscsi.inf", Some win8_32;
    "drivers/i386/WinXP/viostor.inf", Some winxp_32;
    "drivers/i386/WinXP/netkvm.inf", Some winxp_32;
    "drivers/i386/WinXP/qxl.inf", Some winxp_32;
    "drivers/amd64/Win8.1/viostor.inf", Some win8_1_64;
    "drivers/amd64/Win8.1/netkvm.inf", Some win8_1_64;
    "drivers/amd64/Win8.1/vioscsi.inf", Some win8_1_64;
    "drivers/amd64/Win2008/viostor.inf", Some win2k8_64;
    "drivers/amd64/Win2008/netkvm.inf", Some win2k8_64;
    "drivers/amd64/Win2008/vioscsi.inf", Some win2k8_64;
    "drivers/amd64/Win7/viostor.inf", Some win7_64;
    "drivers/amd64/Win7/netkvm.inf", Some win7_64;
    "drivers/amd64/Win7/qxl.inf", Some win7_64;
    "drivers/amd64/Win7/vioscsi.inf", Some win7_64;
    "drivers/amd64/Win2003/viostor.inf", Some win2k3_64;
    "drivers/amd64/Win2003/netkvm.inf", Some win2k3_64;
    "drivers/amd64/Win8/viostor.inf", Some win8_64;
    "drivers/amd64/Win8/netkvm.inf", Some win8_64;
    "drivers/amd64/Win8/vioscsi.inf", Some win8_64;
    "drivers/amd64/Win2012/viostor.inf", Some win2k12_64;
    "drivers/amd64/Win2012/netkvm.inf", Some win2k12_64;
    "drivers/amd64/Win2012/vioscsi.inf", Some win2k12_64;
    "drivers/amd64/Win2008R2/viostor.inf", Some win2k8r2_64;
    "drivers/amd64/Win2008R2/netkvm.inf", Some win2k8r2_64;
    "drivers/amd64/Win2008R2/qxl.inf", Some win2k8r2_64;
    "drivers/amd64/Win2008R2/vioscsi.inf", Some win2k8r2_64;
    "drivers/amd64/Win2012R2/viostor.inf", Some win2k12r2_64;
    "drivers/amd64/Win2012R2/netkvm.inf", Some win2k12r2_64;
    "drivers/amd64/Win2012R2/vioscsi.inf", Some win2k12r2_64;
  ] in

  (* Test each inf file against each version of Windows. *)
  let printer = string_of_bool in

  List.iter (
    fun (path, correct_windows) ->
      let path = srcdir // ".." // "test-data" // "fake-virtio-win" // path in

      match correct_windows with
      | None ->
         List.iter (
           fun win ->
             let msg = sprintf "path %s should not match %s"
                               path win.i_product_name in
             let content = read_whole_file path in
             assert_equal ~printer ~msg false
               (Windows.UNIT_TESTS.virtio_inf_matches_guest_os content path win)
         ) all_windows
      | Some correct_windows ->
         List.iter (
           fun win ->
             let expected = win = correct_windows in
             let msg =
               if expected then
                 sprintf "path %s should match %s"
                         path win.i_product_name
               else
                 sprintf "path %s should not match %s"
                         path win.i_product_name in
             let content = read_whole_file path in
             assert_equal ~printer ~msg expected
               (Windows.UNIT_TESTS.virtio_inf_matches_guest_os content path win)
         ) all_windows
  ) paths

(* Suites declaration. *)
let suite =
  "virt-v2v" >:::
    [
      "OVF.get_ostype" >:: test_get_ostype;
      "Utils.drive_name" >:: test_drive_name;
      "Utils.drive_index" >:: test_drive_index;
      "Windows_inf.of_string" >:: test_windows_inf_of_string;
      "Windows.virtio_inf_matches_guest_os" >::
        test_virtio_inf_matches_guest_os;
    ]

let () =
  run_test_tt_main suite
