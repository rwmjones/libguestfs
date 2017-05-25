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

type fs = {
  fs_location : location;
  role : role;             (** Special cases: root filesystem or /usr *)
}
and root = {
  root_location : location;
  inspection_data : inspection_data;
}
and location = {
  mountable : Mountable.t; (** The device name or other mountable object.*)
  vfs_type : string;       (** Returned from [vfs_type] API. *)
}

and role =
  | RoleRoot of inspection_data
  | RoleUsr of inspection_data
  | RoleSwap
  | RoleOther
and inspection_data = {
  format : format option;
  os_type : os_type option;
  distro : distro option;
  package_format : package_format option;
  package_management : package_management option;
  product_name : string option;
  product_variant : string option;
  version : version option;
  arch : string option;
  hostname : string option;
  fstab : fstab_entry list;
  windows_systemroot : string option;
  windows_software_hive : string option;
  windows_system_hive : string option;
  windows_current_control_set : string option;
  drive_mappings : drive_mapping list;
  is_live_disk : bool;
  is_netinst_disk : bool;
  is_multipart_disk : bool;
}
and format =
  | FORMAT_INSTALLED
  | FORMAT_INSTALLER
  (* in future: supplemental install disks *)
and os_type =
  | OS_TYPE_DOS
  | OS_TYPE_FREEBSD
  | OS_TYPE_HURD
  | OS_TYPE_LINUX
  | OS_TYPE_MINIX
  | OS_TYPE_NETBSD
  | OS_TYPE_OPENBSD
  | OS_TYPE_WINDOWS
and distro =
  | DISTRO_ALPINE_LINUX
  | DISTRO_ALTLINUX
  | DISTRO_ARCHLINUX
  | DISTRO_BUILDROOT
  | DISTRO_CENTOS
  | DISTRO_CIRROS
  | DISTRO_COREOS
  | DISTRO_DEBIAN
  | DISTRO_FEDORA
  | DISTRO_FREEBSD
  | DISTRO_FREEDOS
  | DISTRO_FRUGALWARE
  | DISTRO_GENTOO
  | DISTRO_LINUX_MINT
  | DISTRO_MAGEIA
  | DISTRO_MANDRIVA
  | DISTRO_MEEGO
  | DISTRO_NETBSD
  | DISTRO_OPENBSD
  | DISTRO_OPENSUSE
  | DISTRO_ORACLE_LINUX
  | DISTRO_PARDUS
  | DISTRO_PLD_LINUX
  | DISTRO_REDHAT_BASED
  | DISTRO_RHEL
  | DISTRO_SCIENTIFIC_LINUX
  | DISTRO_SLACKWARE
  | DISTRO_SLES
  | DISTRO_SUSE_BASED
  | DISTRO_TTYLINUX
  | DISTRO_UBUNTU
  | DISTRO_VOID_LINUX
  | DISTRO_WINDOWS
and package_format =
  | PACKAGE_FORMAT_APK
  | PACKAGE_FORMAT_DEB
  | PACKAGE_FORMAT_EBUILD
  | PACKAGE_FORMAT_PACMAN
  | PACKAGE_FORMAT_PISI
  | PACKAGE_FORMAT_PKGSRC
  | PACKAGE_FORMAT_RPM
  | PACKAGE_FORMAT_XBPS
and package_management =
  | PACKAGE_MANAGEMENT_APK
  | PACKAGE_MANAGEMENT_APT
  | PACKAGE_MANAGEMENT_DNF
  | PACKAGE_MANAGEMENT_PACMAN
  | PACKAGE_MANAGEMENT_PISI
  | PACKAGE_MANAGEMENT_PORTAGE
  | PACKAGE_MANAGEMENT_UP2DATE
  | PACKAGE_MANAGEMENT_URPMI
  | PACKAGE_MANAGEMENT_XBPS
  | PACKAGE_MANAGEMENT_YUM
  | PACKAGE_MANAGEMENT_ZYPPER
and version = int * int
and fstab_entry = string * string (* mountable, mountpoint *)
and drive_mapping = string * string (* drive name, mountable *)

val merge_inspection_data : inspection_data -> inspection_data -> inspection_data
(** [merge_inspection_data child parent] merges two sets of inspection
    data into a single set.  The parent inspection data fields, if
    present, take precedence over the child inspection data fields.

    It's intended that you merge upwards, ie.
    [merge_inspection_data usr root] *)

val string_of_fs : fs -> string
(** Convert [fs] into a single line string, for debugging only. *)

val string_of_root : root -> string
(** Convert [root] into a multi-line string, for debugging only. *)

val string_of_location : location -> string
(** Convert [location] into a string, for debugging only. *)

val string_of_inspection_data : inspection_data -> string
(** Convert [inspection_data] into a multi-line string, for debugging only. *)

val string_of_format : format -> string
(** Convert [format] to a string.
    The string is part of the public API. *)

val string_of_os_type : os_type -> string
(** Convert [os_type] to a string.
    The string is part of the public API. *)

val string_of_distro : distro -> string
(** Convert [distro] to a string.
    The string is part of the public API. *)

val string_of_package_format : package_format -> string
(** Convert [package_format] to a string.
    The string is part of the public API. *)

val string_of_package_management : package_management -> string
(** Convert [package_management] to a string.
    The string is part of the public API. *)

val null_inspection_data : inspection_data
(** {!inspection_data} structure with all fields set to [None]. *)
