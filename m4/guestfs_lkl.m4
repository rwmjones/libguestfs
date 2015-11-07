# libguestfs
# Copyright (C) 2009-2015 Red Hat Inc.
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

dnl LKL (Linux Kernel compiled as a Library).
dnl
dnl You have to use --with-lkl=/path/to/linux
dnl
dnl where '/path/to/linux' is the absolute path to the Linux sources
dnl (https://github.com/lkl/linux), compiled.  See Documentation/lkl.txt
dnl for how to build LKL.

AC_MSG_CHECKING([for LKL (Linux Kernel Library)])
AC_ARG_WITH([lkl],
    [AS_HELP_STRING([--with-lkl=/path/to/linux],
        [build the LKL (Linux Kernel Library) backend])],
    [
    LKL_PATH="$withval"

    # Does it contain the compiled static library?
    if ! test -f "$LKL_PATH/tools/lkl/lib/liblkl.a"; then
        AC_MSG_FAILURE([--with-lkl parameter does not point to compiled LKL])
    fi

    # Does it contain the header file?
    if ! test -f "$LKL_PATH/tools/lkl/include/lkl.h"; then
        AC_MSG_FAILURE([--with-lkl parameter does not point to LKL header file])
    fi

    ],
    [LKL_PATH=no])
AC_MSG_RESULT([$LKL_PATH])

AC_SUBST([LKL_PATH])

AM_CONDITIONAL([HAVE_LKL], [test "x$LKL_PATH" != "xno"])
