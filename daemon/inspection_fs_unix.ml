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

let re_major_minor = Str.regexp "\\([0-9]+\\)\\.\\([0-9]+\\)"
let re_major_no_minor = Str.regexp "\\([0-9]+\\)"

let re_fedora = Str.regexp "Fedora release \\([0-9]+\\)"
let re_rhel_old = Str.regexp "Red Hat.*release \\([0-9]+\\).*Update \\([0-9]+\\)"
let re_rhel = Str.regexp "Red Hat.*release \\([0-9]+\\)\\.\\([0-9]+\\)"
let re_rhel_no_minor = Str.regexp "Red Hat.*release \\([0-9]+\\)"
let re_centos_old = Str.regexp "CentOS.*release \\([0-9]+\\).*Update \\([0-9]+\\)"
let re_centos = Str.regexp "CentOS.*release \\([0-9]+\\)\\.\\([0-9]+\\)"
let re_centos_no_minor = Str.regexp "CentOS.*release \\([0-9]+\\)"
let re_scientific_linux_old =
  Str.regexp "Scientific Linux.*release \\([0-9]+\\).*Update \\([0-9]+\\)"
let re_scientific_linux =
  Str.regexp "Scientific Linux.*release \\([0-9]+\\)\\.\\([0-9]+\\)"
let re_scientific_linux_no_minor =
  Str.regexp "Scientific Linux.*release \\([0-9]+\\)"
let re_oracle_linux_old =
  Str.regexp "Oracle Linux.*release \\([0-9]+\\).*Update \\([0-9]+\\)"
let re_oracle_linux =
  Str.regexp "Oracle Linux.*release \\([0-9]+\\)\\.\\([0-9]+\\)"
let re_oracle_linux_no_minor = Str.regexp "Oracle Linux.*release \\([0-9]+\\)"
let re_netbsd = Str.regexp "^NetBSD \\([0-9]+\\)\\.\\([0-9]+\\)"
let re_opensuse = Str.regexp "^\\(openSUSE|SuSE Linux|SUSE LINUX\\) "
let re_sles = Str.regexp "^SUSE \\(Linux|LINUX\\) Enterprise "
let re_nld = Str.regexp "^Novell Linux Desktop "
let re_sles_version = Str.regexp "^VERSION = \\([0-9]+\\)"
let re_sles_patchlevel = Str.regexp "^PATCHLEVEL = \\([0-9]+\\)"
let re_minix = Str.regexp "^\\([0-9]+\\)\\.\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?"
let re_openbsd = Str.regexp "^OpenBSD \\([0-9]+|\\?\\)\\.\\([0-9]+|\\?\\)"
let re_frugalware = Str.regexp "Frugalware \\([0-9]+\\)\\.\\([0-9]+\\)"
let re_pldlinux = Str.regexp "\\([0-9]+\\)\\.\\([0-9]+\\) PLD Linux"

let rec check_linux_root data =
  let data = {
    data with
      os_type = Some OS_TYPE_LINUX
  } in

  let tests = [
    (* systemd distros include /etc/os-release which is reasonably
     * standardized.  This entry should be first.
     *)
    "/etc/os-release",     parse_os_release;
    (* LSB is also a reasonable standard.  This entry should be second. *)
    "/etc/lsb-release",    parse_lsb_release;

    (* Now we enter the Wild West ... *)

    (* RHEL-based distros include a [/etc/redhat-release] file, hence their
     * checks need to be performed before the Red-Hat one.
     *)
    "/etc/oracle-release", parse_generic ~rex:re_oracle_linux_old
                                         DISTRO_ORACLE_LINUX;
    "/etc/oracle-release", parse_generic ~rex:re_oracle_linux
                                         DISTRO_ORACLE_LINUX;
    "/etc/oracle-release", parse_generic ~rex:re_oracle_linux_no_minor
                                         DISTRO_ORACLE_LINUX;
    "/etc/centos-release", parse_generic ~rex:re_centos_old
                                         DISTRO_CENTOS;
    "/etc/centos-release", parse_generic ~rex:re_centos
                                         DISTRO_CENTOS;
    "/etc/centos-release", parse_generic ~rex:re_centos_no_minor
                                         DISTRO_CENTOS;
    "/etc/altlinux-release", parse_generic DISTRO_ALTLINUX;
    "/etc/redhat-release", parse_generic ~rex:re_fedora
                                         DISTRO_FEDORA;
    "/etc/redhat-release", parse_generic ~rex:re_rhel_old
                                         DISTRO_RHEL;
    "/etc/redhat-release", parse_generic ~rex:re_rhel
                                         DISTRO_RHEL;
    "/etc/redhat-release", parse_generic ~rex:re_rhel_no_minor
                                         DISTRO_RHEL;
    "/etc/redhat-release", parse_generic ~rex:re_centos_old
                                         DISTRO_CENTOS;
    "/etc/redhat-release", parse_generic ~rex:re_centos
                                         DISTRO_CENTOS;
    "/etc/redhat-release", parse_generic ~rex:re_centos_no_minor
                                         DISTRO_CENTOS;
    "/etc/redhat-release", parse_generic ~rex:re_scientific_linux_old
                                         DISTRO_SCIENTIFIC_LINUX;
    "/etc/redhat-release", parse_generic ~rex:re_scientific_linux
                                         DISTRO_SCIENTIFIC_LINUX;
    "/etc/redhat-release", parse_generic ~rex:re_scientific_linux_no_minor
                                         DISTRO_SCIENTIFIC_LINUX;

    (* If there's an /etc/redhat-release file, but nothing above
     * matches, then it is a generic Red Hat-based distro.
     *)
    "/etc/redhat-release", parse_generic DISTRO_REDHAT_BASED;
    "/etc/redhat-release",
      (fun _ data -> { data with distro = Some DISTRO_REDHAT_BASED });

    "/etc/debian_version", parse_generic DISTRO_DEBIAN;
    "/etc/pardus-release", parse_generic DISTRO_PARDUS;

    (* /etc/arch-release file is empty and I can't see a way to
     * determine the actual release or product string.
     *)
    "/etc/arch-release",
      (fun _ data -> { data with distro = Some DISTRO_ARCHLINUX });

    "/etc/gentoo-release", parse_generic DISTRO_GENTOO;
    "/etc/meego-release", parse_generic DISTRO_MEEGO;
    "/etc/slackware-version", parse_generic DISTRO_SLACKWARE;
    "/etc/ttylinux-target", parse_generic DISTRO_TTYLINUX;

    "/etc/SuSE-release", parse_suse_release;
    "/etc/SuSE-release",
      (fun _ data -> { data with distro = Some DISTRO_SUSE_BASED });

    "/etc/cirros/version", parse_generic DISTRO_CIRROS;
    "/etc/br-version",
      (fun release_file data ->
        let distro =
          if Is.is_file ~followsymlinks:true "/usr/share/cirros/logo" then
            DISTRO_CIRROS
          else
            DISTRO_BUILDROOT in
        (* /etc/br-version has the format YYYY.MM[-git/hg/svn release] *)
        parse_generic distro release_file data);

    "/etc/alpine-release", parse_generic DISTRO_ALPINE_LINUX;
    "/etc/frugalware-release", parse_generic ~rex:re_frugalware
                                             DISTRO_FRUGALWARE;
    "/etc/pld-release", parse_generic ~rex:re_pldlinux
                                      DISTRO_PLD_LINUX;
  ] in

  let rec loop = function
    | (release_file, parse_fun) :: tests ->
       if verbose () then
         eprintf "check_linux_root: checking %s\n%!" release_file;
       (try
          if not (Is.is_file ~followsymlinks:true release_file) then
            raise Not_found;
          parse_fun release_file data
        with
          Not_found -> loop tests)
    | [] -> data
  in
  let data = loop tests in

  let data = {
    data with
      arch = check_architecture ();
      fstab = check_fstab ();
      hostname = check_hostname_linux ();
  } in

  data

(* Parse a os-release file.
 *
 * Only few fields are parsed, falling back to the usual detection if we
 * cannot read all of them.
 *
 * For the format of os-release, see also:
 * http://www.freedesktop.org/software/systemd/man/os-release.html
 *)
and parse_os_release release_file data =
  let chroot = Chroot.create (sysroot ()) in
  let lines =
    Chroot.f chroot (
      fun () ->
        if not (is_small_file release_file) then (
          eprintf "%s: not a regular file or too large\n" release_file;
          raise Not_found
        );
        read_whole_file release_file
  ) () in
  let lines = String.nsplit "\n" lines in

  let data = List.fold_left (
    fun data line ->
      let line = String.trim line in
      if line = "" || line.[0] = '#' then
        data
      else (
        let key, value = String.split "=" line in
        let value =
          let n = String.length value in
          if n >= 2 && value.[0] = '"' && value.[n-1] = '"' then
            String.sub value 1 (n-2)
          else
            value in
        if key = "ID" then (
          let distro = distro_of_os_release_id value in
          match distro with
          | Some _ as distro -> { data with distro = distro }
          | None -> data
        )
        else if key = "PRETTY_NAME" then
          { data with product_name = Some value }
        else if key = "VERSION_ID" then
          parse_version_from_major_minor value data
        else
          data
      )
  ) data lines in

  (* os-release in Debian and CentOS does not provide the full
   * version number (VERSION_ID), just the major part of it.  If
   * we detect that situation then bail out and use the release
   * files instead.
   *)
  (match data with
   | { distro = Some (DISTRO_DEBIAN|DISTRO_CENTOS); version = Some (_, 0) } ->
      raise Not_found
   | _ -> ()
  );

  data

(* ID="fedora" => Some DISTRO_FEDORA *)
and distro_of_os_release_id = function
  | "alpine" -> Some DISTRO_ALPINE_LINUX
  | "altlinux" -> Some DISTRO_ALTLINUX
  | "arch" -> Some DISTRO_ARCHLINUX
  | "centos" -> Some DISTRO_CENTOS
  | "coreos" -> Some DISTRO_COREOS
  | "debian" -> Some DISTRO_DEBIAN
  | "fedora" -> Some DISTRO_FEDORA
  | "frugalware" -> Some DISTRO_FRUGALWARE
  | "mageia" -> Some DISTRO_MAGEIA
  | "opensuse" -> Some DISTRO_OPENSUSE
  | "pld" -> Some DISTRO_PLD_LINUX
  | "rhel" -> Some DISTRO_RHEL
  | "sles" | "sled" -> Some DISTRO_SLES
  | "ubuntu" -> Some DISTRO_UBUNTU
  | "void" -> Some DISTRO_VOID_LINUX
  | value ->
     eprintf "/etc/os-release: unknown ID=%s\n" value;
     None

(* Ubuntu has /etc/lsb-release containing:
 *   DISTRIB_ID=Ubuntu                                # Distro
 *   DISTRIB_RELEASE=10.04                            # Version
 *   DISTRIB_CODENAME=lucid
 *   DISTRIB_DESCRIPTION="Ubuntu 10.04.1 LTS"         # Product name
 *
 * [Ubuntu-derived ...] Linux Mint was found to have this:
 *   DISTRIB_ID=LinuxMint
 *   DISTRIB_RELEASE=10
 *   DISTRIB_CODENAME=julia
 *   DISTRIB_DESCRIPTION="Linux Mint 10 Julia"
 * Linux Mint also has /etc/linuxmint/info with more information,
 * but we can use the LSB file.
 *
 * Mandriva has:
 *   LSB_VERSION=lsb-4.0-amd64:lsb-4.0-noarch
 *   DISTRIB_ID=MandrivaLinux
 *   DISTRIB_RELEASE=2010.1
 *   DISTRIB_CODENAME=Henry_Farman
 *   DISTRIB_DESCRIPTION="Mandriva Linux 2010.1"
 * Mandriva also has a normal release file called /etc/mandriva-release.
 *
 * CoreOS has a /etc/lsb-release link to /usr/share/coreos/lsb-release containing:
 *   DISTRIB_ID=CoreOS
 *   DISTRIB_RELEASE=647.0.0
 *   DISTRIB_CODENAME="Red Dog"
 *   DISTRIB_DESCRIPTION="CoreOS 647.0.0"
 *)
and parse_lsb_release release_file data =
  let chroot = Chroot.create (sysroot ()) in
  let lines =
    Chroot.f chroot (
      fun () ->
        if not (is_small_file release_file) then (
          eprintf "%s: not a regular file or too large\n" release_file;
          raise Not_found
        );
        read_whole_file release_file
  ) () in
  let lines = String.nsplit "\n" lines in

  let data = List.fold_left (
    fun data line ->
      if data.distro = None && line = "DISTRIB_ID=Ubuntu" then
        { data with distro = Some DISTRO_UBUNTU }
      else if data.distro = None && line = "DISTRIB_ID=LinuxMint" then
        { data with distro = Some DISTRO_LINUX_MINT }
      else if data.distro = None && line = "DISTRIB_ID=\"Mageia\"" then
        { data with distro = Some DISTRO_MAGEIA }
      else if data.distro = None && line = "DISTRIB_ID=CoreOS" then
        { data with distro = Some DISTRO_COREOS }
      else if String.is_prefix line "DISTRIB_RELEASE=" then
        parse_version_from_major_minor line data
      else if String.is_prefix line "DISTRIB_DESCRIPTION=\"" ||
                String.is_prefix line "DISTRIB_DESCRIPTION='" then (
        let n = String.length line in
        let product_name = String.sub line 21 (n-20) in
        { data with product_name = Some product_name }
      )
      else if String.is_prefix line "DISTRIB_DESCRIPTION=" then (
        let n = String.length line in
        let product_name = String.sub line 20 (n-20) in
        { data with product_name = Some product_name }
      )
      else
        data
  ) data lines in

  data

and parse_suse_release release_file data =
  let chroot = Chroot.create (sysroot ()) in
  let lines =
    Chroot.f chroot (
      fun () ->
        if not (is_small_file release_file) then (
          eprintf "%s: not a regular file or too large\n" release_file;
          raise Not_found
        );
        read_whole_file release_file
  ) () in
  let lines = String.nsplit "\n" lines in

  if lines = [] then raise Not_found;

  (* First line is dist release name. *)
  let product_name = List.hd lines in
  let data = {
    data with
      product_name = Some product_name
  } in

  (* Match SLES first because openSuSE regex overlaps some SLES
   * release strings.
   *)
  if Str.string_match re_sles product_name 0 ||
     Str.string_match re_nld product_name 0 then (
    (* Second line contains version string. *)
    let major =
      if List.length lines >= 2 then (
        let line = List.nth lines 1 in
        if Str.string_match re_sles_version line 0 then
          Some (int_of_string (Str.matched_group 1 line))
        else None
      )
      else None in

    (* Third line contains service pack string. *)
    let minor =
      if List.length lines >= 3 then (
        let line = List.nth lines 2 in
        if Str.string_match re_sles_patchlevel line 0 then
          Some (int_of_string (Str.matched_group 1 line))
        else None
      )
      else None in

    let version =
      match major, minor with
      | Some major, Some minor -> Some (major, minor)
      | Some major, None -> Some (major, 0)
      | None, Some _ | None, None -> None in

    { data with
        distro = Some DISTRO_SLES;
        version = version }
  )
  else if Str.string_match re_opensuse product_name 0 then (
    (* Second line contains version string. *)
    let data =
      if List.length lines >= 2 then (
        let line = List.nth lines 1 in
        parse_version_from_major_minor line data
      )
      else data in

    { data with distro = Some DISTRO_OPENSUSE }
  )
  else
    data

(* Parse any generic /etc/x-release file.
 *
 * The optional regular expression which may match 0, 1 or 2
 * substrings, which are used as the major and minor numbers.
 *
 * The fixed distro is always set, and the product name is
 * set to the first line of the release file.
 *)
and parse_generic ?rex distro release_file data =
  let chroot = Chroot.create (sysroot ()) in
  let product_name =
    Chroot.f chroot (
      fun () ->
        if not (is_small_file release_file) then (
          eprintf "%s: not a regular file or too large\n" release_file;
          raise Not_found
        );
        read_first_line_from_file release_file
  ) () in
  if product_name = "" then
    raise Not_found;

  let data =
    { data with product_name = Some product_name;
                distro = Some distro } in

  match rex with
  | Some rex ->
     (* If ~rex was supplied, then it must match the release file,
      * else the parsing fails.
      *)
     if not (Str.string_match rex product_name 0) then
       raise Not_found;

    (* Although it's not documented, matched_group raises
     * Invalid_argument if called with an unknown group number.
     *)
    let major =
      try Some (int_of_string (Str.matched_group 1 product_name))
      with Not_found | Invalid_argument _ | Failure _ -> None in
    let minor =
      try Some (int_of_string (Str.matched_group 2 product_name))
      with Not_found | Invalid_argument _ | Failure _ -> None in
    (match major, minor with
     | None, None -> data
     | None, Some _ -> data
     | Some major, None -> { data with version = Some (major, 0) }
     | Some major, Some minor -> { data with version = Some (major, minor) }
    )

  | None ->
     (* However if no ~rex was supplied, then we make a best
      * effort attempt to parse a version number, but don't
      * fail if one cannot be found.
      *)
     parse_version_from_major_minor product_name data

(* Make a best effort attempt to parse either X or X.Y from a string,
 * usually the product_name string.
 *)
and parse_version_from_major_minor str data =
  if Str.string_match re_major_minor str 0 ||
     Str.string_match re_major_no_minor str 0 then (
    let major =
      try Some (int_of_string (Str.matched_group 1 str))
      with Not_found | Invalid_argument _ | Failure _ -> None in
    let minor =
      try Some (int_of_string (Str.matched_group 2 str))
      with Not_found | Invalid_argument _ | Failure _ -> None in
    match major, minor with
    | None, None -> data
    | None, Some _ -> data
    | Some major, None -> { data with version = Some (major, 0) }
    | Some major, Some minor -> { data with version = Some (major, minor) }
  )
  else (
    eprintf "parse_version_from_major_minor: cannot parse version from ‘%s’\n"
            str;
    data
  )

and check_architecture () =
  (* XXX *) None

and check_fstab () =
  (* XXX *) []

and check_hostname_linux () =
  (* XXX *) None

let check_linux_usr data =
  (* XXX *) data

let check_coreos_root data =
  (* XXX *) data

let check_coreos_usr data =
  (* XXX *) data

let check_freebsd_root data =
  (* XXX *) data

and check_hostname_freebsd () =
  (* XXX *) None

let check_netbsd_root data =
  (* XXX *) data

let rec check_openbsd_root data =
  (* XXX *) data

and check_hostname_openbsd () =
  (* XXX *) None

let check_hurd_root data =
  (* XXX *) data

let rec check_minix_root data =
  (* XXX *) data

and check_hostname_minix () =
  (* XXX *) None
