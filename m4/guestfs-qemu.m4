# libguestfs
# Copyright (C) 2009-2017 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

dnl Check for QEMU for running binaries on this $host_cpu, fall
dnl back to basic 'qemu'.  Allow the user to override it.

AS_CASE([$host_cpu],
        [i@<:@456@:>@86],[qemu_cpu=i386],
        [arm*],[qemu_cpu=arm],
        [amd64],[qemu_cpu=x86_64],
        [powerpc64 | ppc64le | powerpc64le],[qemu_cpu=ppc64],
        [qemu_cpu=$host_cpu])
default_qemu="qemu-kvm kvm qemu-system-$qemu_cpu qemu"

AC_ARG_WITH([qemu],
    [AS_HELP_STRING([--with-qemu=/path/to/qemu],
        [set default QEMU binary @<:@default=search@:>@])],
    dnl --with-qemu or --without-qemu:
    [],
    dnl neither option was given:
    [with_qemu=search]
)

AS_CASE([$with_qemu],
    [no],[
        AC_MSG_WARN([qemu was disabled, libguestfs may not work at all])
        QEMU=no
    ],
    [search],[
        AC_PATH_PROGS([QEMU],[$default_qemu],[no],
            [$PATH$PATH_SEPARATOR/usr/sbin$PATH_SEPARATOR/sbin$PATH_SEPARATOR/usr/libexec])
    test "x$QEMU" = "xno" && AC_MSG_ERROR([qemu not found])

    AC_DEFINE_UNQUOTED([QEMU],["$QEMU"],[Location of qemu binary.])

    # Only check that -help and -version are available and look
    # reasonable.  Real checks will be performed at runtime.

    AC_MSG_CHECKING([that $QEMU -help works])
    if $QEMU -help >&AS_MESSAGE_LOG_FD 2>&1; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
        AC_MSG_FAILURE(
[$QEMU -help: command failed.

This could be a very old version of qemu, or qemu might not be
working.
])
    fi

    AC_MSG_CHECKING([that $QEMU -version works])
    if $QEMU -version >&AS_MESSAGE_LOG_FD 2>&1; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
        AC_MSG_FAILURE(
[$QEMU -version: command failed.

This could be a very old version of qemu, or qemu might not be
working.
])
    fi

    AC_MSG_CHECKING([for $QEMU version >= 1])
    if $QEMU -version | grep -sq 'version @<:@1-9@:>@'; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
        AC_MSG_FAILURE([$QEMU version must be >= 1.0.])
    fi
    ],[
    # AS_CASE default:
    # Use the binary supplied without question, but check
    # that a full path was given.
    QEMU="$with_qemu"
    AC_MSG_CHECKING([that $QEMU is an absolute path])
    case "$QEMU" in
    /*)
        AC_MSG_RESULT([yes])
        ;;
    *)
        AC_MSG_RESULT([no])
        AC_MSG_ERROR([--with-qemu=$QEMU must be an absolute path])
        ;;
    esac
    AC_DEFINE_UNQUOTED([QEMU],["$QEMU"],[Location of qemu binary.])
])
