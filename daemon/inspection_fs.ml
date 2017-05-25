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

open Cmdline
open Mountable
open Inspection_types

let rec check_for_filesystem_on mountable vfs_type =
  if verbose () then
    eprintf "check_for_filesystem_on: %s (%s)\n%!"
            (Mountable.to_string mountable) vfs_type;

  let role =
    let is_swap = vfs_type = "swap" in
    if is_swap then
      Some RoleSwap
    else (
      (* If it's a whole device, see if it is an install ISO. *)
      let is_whole_device = Devsparts.is_whole_device mountable.m_device in
      let installer_role =
        if is_whole_device then
          Inspection_fs_cd.check_installer_iso mountable.m_device
        else
          None in
      match installer_role with
      | Some _ as role -> role
      | None ->
         (* Try mounting the device.  Ignore errors if we can't do this. *)
         let mounted =
           if vfs_type = "ufs" then ( (* Hack for the *BSDs. *)
             (* FreeBSD fs is a variant of ufs called ufs2 ... *)
             try
               Mount.mount_vfs (Some "ro,ufstype=ufs2") (Some "ufs")
                               mountable "/";
               true
             with _ ->
               (* while NetBSD and OpenBSD use another variant labeled 44bsd *)
               try
                 Mount.mount_vfs (Some "ro,ufstype=44bsd") (Some "ufs")
                                 mountable "/";
                 true
               with _ -> false
           ) else (
             try Mount.mount_ro mountable "/";
                 true
             with _ -> false
           ) in
         if not mounted then None
         else (
           let role = check_filesystem mountable is_whole_device in
           Mount.umount_all ();
           role
         )
    ) in

  match role with
  | None -> None
  | Some role ->
     Some { fs_location = { mountable = mountable; vfs_type = vfs_type };
            role = role }

(* When this function is called, the filesystem is mounted on sysroot (). *)
and check_filesystem mountable is_whole_device =
  let is_only_partition =
    if is_whole_device then false
    else is_only_partition mountable in

  let role = ref `Other in
  let data = ref null_inspection_data in

  (* Grub /boot? *)
  if Is.is_file "/grub/menu.lst" ||
     Is.is_file "/grub/grub.conf" ||
     Is.is_file "/grub2/grub.cfg" then
    ()
  (* FreeBSD root? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/bin" &&
          Is.is_file "/etc/freebsd-update.conf" &&
          Is.is_file "/etc/fstab" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_freebsd_root !data
  )
  (* NetBSD root? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/bin" &&
          Is.is_file "/netbsd" &&
          Is.is_file "/etc/fstab" &&
          Is.is_file "/etc/release" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_netbsd_root !data;
  )
  (* OpenBSD root? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/bin" &&
          Is.is_file "/bsd" &&
          Is.is_file "/etc/fstab" &&
          Is.is_file "/etc/motd" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_openbsd_root !data;
  )
  (* Hurd root? *)
  else if Is.is_file "/hurd/console" &&
          Is.is_file "/hurd/hello" &&
          Is.is_file "/hurd/null" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_hurd_root !data;
  )
  (* Minix root? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/bin" &&
          Is.is_file "/service/vm" &&
          Is.is_file "/etc/fstab" &&
          Is.is_file "/etc/version" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_minix_root !data;
  )
  (* Linux root? *)
  else if Is.is_dir "/etc" &&
          (Is.is_dir "/bin" ||
           is_symlink_to "/bin" "usr/bin") &&
          (Is.is_file "/etc/fstab" ||
           Is.is_file "/etc/hosts") then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_linux_root !data;
  )
  (* CoreOS root? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/root" &&
          Is.is_dir "/home" &&
          Is.is_dir "/usr" &&
          Is.is_file "/etc/coreos/update.conf" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_unix.check_coreos_root !data;
  )
  (* Linux /usr/local? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/bin" &&
          Is.is_dir "/share" &&
          not (Is.is_dir "/local") &&
          not (Is.is_file "/etc/fstab") then
    ()
  (* Linux /usr? *)
  else if Is.is_dir "/etc" &&
          Is.is_dir "/bin" &&
          Is.is_dir "/share" &&
          Is.is_dir "/local" &&
          not (Is.is_file "/etc/fstab") then (
    data := Inspection_fs_unix.check_linux_usr !data;
  )
  (* CoreOS /usr? *)
  else if Is.is_dir "/bin" &&
          Is.is_dir "/share" &&
          Is.is_dir "/local" &&
          Is.is_dir "/share/coreos" then (
    data := Inspection_fs_unix.check_coreos_usr !data;
  )
  (* Linux /var? *)
  else if Is.is_dir "/log" &&
          Is.is_dir "/run" &&
          Is.is_dir "/spool" then
    ()
  (* Windows root? *)
  else if Inspection_fs_windows.is_windows_systemroot () then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLED };
    data := Inspection_fs_windows.check_windows_root !data;
  )
  (* Windows volume with installed applications (but not root)? *)
  else if is_dir_nocase "/System Volume Information" &&
          is_dir_nocase "/Program Files" then
    ()
  (* Windows volume (but not root)? *)
  else if is_dir_nocase "/System Volume Information" then
    ()
  (* FreeDOS? *)
  else if is_dir_nocase "/FDOS" &&
          is_file_nocase "/FDOS/FREEDOS.BSS" then (
    role := `Root;
    data := { !data with
              format = Some FORMAT_INSTALLED;
              os_type = Some OS_TYPE_DOS;
              distro = Some DISTRO_FREEDOS;
              (* FreeDOS is a mix of 16 and 32 bit, but
               * assume it requires a 32 bit i386 processor.
               *)
              arch = Some "i386" }
  )
  (* Install CD/disk?
   *
   * Note that we checked (above) for an install ISO, but there are
   * other types of install image (eg. USB keys) which that check
   * wouldn't have picked up.
   *
   * Skip these checks if it's not a whole device (eg. CD) or the
   * first partition (eg. bootable USB key).
   *)
  else if (is_whole_device || is_only_partition) &&
          Is.is_file "/isolinux/isolinux.cfg" ||
          Is.is_dir "/EFI/BOOT" ||
          Is.is_file "/images/install.img" ||
          Is.is_dir "/.disk" ||
          Is.is_file "/.discinfo" ||
          Is.is_file "/i386/txtsetup.sif" ||
          Is.is_file "/amd64/txtsetup.sif" ||
          Is.is_file "/freedos/freedos.ico" ||
          Is.is_file "/boot/loader.rc" then (
    role := `Root;
    data := { !data with format = Some FORMAT_INSTALLER };
    data := Inspection_fs_cd.check_installer_root !data;
  );

  (* The above code should have set [data.os_type] and [data.distro]
   * fields, so we can now guess the package management system.
   *)
  let data = !data in
  let data = { data with
               package_format = check_package_format data;
               package_management = check_package_management data } in
  match !role with
  | `Root -> Some (RoleRoot data)
  | `Usr -> Some (RoleUsr data)
  | `Other -> Some RoleOther

(* The mountable is the first and only partition on a device
 * with a single device.
 *)
and is_only_partition = function
  | { m_type = MountablePath | MountableBtrfsVol _ } -> false
  | { m_type = MountableDevice; m_device = device } ->
     let partnum, nr_partitions = get_partition_context device in
     partnum = 1 && nr_partitions = 1

and get_partition_context partition =
  let partnum = Devsparts.part_to_partnum partition in
  let device = Devsparts.part_to_dev partition in
  let nr_partitions = Parted.nr_partitions device in
  partnum, nr_partitions

and is_symlink_to file wanted_target =
  if not (Is.is_symlink file) then false
  else Link.readlink file = wanted_target

and is_file_nocase path =
  let path =
    try Some (Realpath.case_sensitive_path path)
    with _ -> None in
  match path with
  | None -> false
  | Some path -> Is.is_file path

and is_dir_nocase path =
  let path =
    try Some (Realpath.case_sensitive_path path)
    with _ -> None in
  match path with
  | None -> false
  | Some path -> Is.is_dir path

(* At the moment, package format and package management are just a
 * simple function of the [distro] and [version[0]] fields, so these
 * can never return an error.  We might be cleverer in future.
 *)
and check_package_format { distro = distro } =
  match distro with
  | None -> None
  | Some DISTRO_FEDORA
  | Some DISTRO_MEEGO
  | Some DISTRO_REDHAT_BASED
  | Some DISTRO_RHEL
  | Some DISTRO_MAGEIA
  | Some DISTRO_MANDRIVA
  | Some DISTRO_SUSE_BASED
  | Some DISTRO_OPENSUSE
  | Some DISTRO_SLES
  | Some DISTRO_CENTOS
  | Some DISTRO_SCIENTIFIC_LINUX
  | Some DISTRO_ORACLE_LINUX
  | Some DISTRO_ALTLINUX ->
     Some PACKAGE_FORMAT_RPM
  | Some DISTRO_DEBIAN
  | Some DISTRO_UBUNTU
  | Some DISTRO_LINUX_MINT ->
     Some PACKAGE_FORMAT_DEB
  | Some DISTRO_ARCHLINUX ->
     Some PACKAGE_FORMAT_PACMAN
  | Some DISTRO_GENTOO ->
     Some PACKAGE_FORMAT_EBUILD
  | Some DISTRO_PARDUS ->
     Some PACKAGE_FORMAT_PISI
  | Some DISTRO_ALPINE_LINUX ->
     Some PACKAGE_FORMAT_APK
  | Some DISTRO_VOID_LINUX ->
     Some PACKAGE_FORMAT_XBPS
  | Some DISTRO_SLACKWARE
  | Some DISTRO_TTYLINUX
  | Some DISTRO_COREOS
  | Some DISTRO_WINDOWS
  | Some DISTRO_BUILDROOT
  | Some DISTRO_CIRROS
  | Some DISTRO_FREEDOS
  | Some DISTRO_FREEBSD
  | Some DISTRO_NETBSD
  | Some DISTRO_OPENBSD
  | Some DISTRO_FRUGALWARE
  | Some DISTRO_PLD_LINUX ->
     None

and check_package_management { distro = distro; version = version } =
  let major = match version with None -> 0 | Some (major, _) -> major in
  match distro with
  | None -> None

  | Some DISTRO_MEEGO ->
     Some PACKAGE_MANAGEMENT_YUM

  | Some DISTRO_FEDORA ->
    (* If Fedora >= 22 and dnf is installed, say "dnf". *)
     if major >= 22 && Is.is_file ~followsymlinks:true "/usr/bin/dnf" then
       Some PACKAGE_MANAGEMENT_DNF
     else if major >= 1 then
       Some PACKAGE_MANAGEMENT_YUM
     else
       (* Probably parsing the release file failed, see RHBZ#1332025. *)
       None

  | Some DISTRO_REDHAT_BASED
  | Some DISTRO_RHEL
  | Some DISTRO_CENTOS
  | Some DISTRO_SCIENTIFIC_LINUX
  | Some DISTRO_ORACLE_LINUX ->
     if major >= 8 then
       Some PACKAGE_MANAGEMENT_DNF
     else if major >= 5 then
       Some PACKAGE_MANAGEMENT_YUM
     else if major >= 2 then
       Some PACKAGE_MANAGEMENT_UP2DATE
     else
       (* Probably parsing the release file failed, see RHBZ#1332025. *)
       None

  | Some DISTRO_DEBIAN
  | Some DISTRO_UBUNTU
  | Some DISTRO_LINUX_MINT
  | Some DISTRO_ALTLINUX ->
     Some PACKAGE_MANAGEMENT_APT

  | Some DISTRO_ARCHLINUX ->
     Some PACKAGE_MANAGEMENT_PACMAN

  | Some DISTRO_GENTOO ->
     Some PACKAGE_MANAGEMENT_PORTAGE

  | Some DISTRO_PARDUS ->
     Some PACKAGE_MANAGEMENT_PISI

  | Some DISTRO_MAGEIA
  | Some DISTRO_MANDRIVA ->
     Some PACKAGE_MANAGEMENT_URPMI

  | Some DISTRO_SUSE_BASED
  | Some DISTRO_OPENSUSE
  | Some DISTRO_SLES ->
     Some PACKAGE_MANAGEMENT_ZYPPER

  | Some DISTRO_ALPINE_LINUX ->
     Some PACKAGE_MANAGEMENT_APK

  | Some DISTRO_VOID_LINUX ->
     Some PACKAGE_MANAGEMENT_XBPS;

  | Some DISTRO_SLACKWARE
  | Some DISTRO_TTYLINUX
  | Some DISTRO_COREOS
  | Some DISTRO_WINDOWS
  | Some DISTRO_BUILDROOT
  | Some DISTRO_CIRROS
  | Some DISTRO_FREEDOS
  | Some DISTRO_FREEBSD
  | Some DISTRO_NETBSD
  | Some DISTRO_OPENBSD
  | Some DISTRO_FRUGALWARE
  | Some DISTRO_PLD_LINUX ->
    None

