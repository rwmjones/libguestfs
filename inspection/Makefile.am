# libguestfs
# Copyright (C) 2015-2016 Red Hat Inc.
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

include $(top_srcdir)/subdir-rules.mk

generator_built = \
	rules.c \
	test1.c \
	test2.c

BUILT_SOURCES = \
	$(generator_built)

CLEANFILES = \
	stamp-guestfs-inspection.pod \
	guestfs-inspection.8

# Build the inspection program.

if INSTALL_DAEMON
sbin_PROGRAMS = guestfs-inspection
else
noinst_PROGRAMS = guestfs-inspection
endif

guestfs_inspection_SOURCES = \
	../daemon/cleanups.c \
	../daemon/cleanups.h \
	../daemon/command.c \
	../daemon/command.h \
	detect.c \
	facts.c \
	inspection.c \
	inspection.h \
	match.c \
	mount.c \
	rules.c \
	rules.h \
	stringsbuf.c \
	stringsbuf.h \
	utils.c

guestfs_inspection_LDADD = \
	$(AUGEAS_LIBS) \
	$(HIVEX_LIBS) \
	$(PCRE_LIBS) \
	$(top_builddir)/gnulib/lib/.libs/libgnu.a

guestfs_inspection_CPPFLAGS = \
	-I$(top_srcdir)/gnulib/lib \
	-I$(top_builddir)/gnulib/lib \
	-I$(top_srcdir)/daemon \
	-I$(top_builddir)/daemon \
	-I$(top_srcdir)/src \
	-I$(top_builddir)/src

guestfs_inspection_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(AUGEAS_CFLAGS) \
	$(HIVEX_CFLAGS) \
	$(PCRE_CFLAGS)

# Manual pages and HTML files for the website.
if INSTALL_DAEMON
man_MANS = guestfs-inspection.8
else
noinst_MANS = guestfs-inspection.8
endif
noinst_DATA = $(top_builddir)/website/guestfs-inspection.8.html

guestfs-inspection.8 $(top_builddir)/website/guestfs-inspection.8.html: stamp-guestfs-inspection.pod

stamp-guestfs-inspection.pod: guestfs-inspection.pod
	$(PODWRAPPER) \
	  --section 8 \
	  --man guestfs-inspection.8 \
	  --html $(top_builddir)/website/guestfs-inspection.8.html \
	  --license GPLv2+ \
	  $<
	touch $@

# Tests.

LOG_COMPILER = $(VG)
TESTS_ENVIRONMENT = $(top_builddir)/run --test

TESTS = test1 test2
check_PROGRAMS = $(TESTS)

test1_SOURCES = \
	../daemon/cleanups.c \
	../daemon/cleanups.h \
	facts.c \
	rules.h \
	stringsbuf.c \
	stringsbuf.h \
	test-harness.c \
	test1.c

test1_LDADD = $(guestfs_inspection_LDADD)
test1_CPPFLAGS = $(guestfs_inspection_CPPFLAGS)
test1_CFLAGS = $(guestfs_inspection_CFLAGS)

test2_SOURCES = \
	../daemon/cleanups.c \
	../daemon/cleanups.h \
	facts.c \
	rules.h \
	stringsbuf.c \
	stringsbuf.h \
	test-harness.c \
	test2.c

test2_LDADD = $(guestfs_inspection_LDADD)
test2_CPPFLAGS = $(guestfs_inspection_CPPFLAGS)
test2_CFLAGS = $(guestfs_inspection_CFLAGS)

check-valgrind:
	$(MAKE) VG="@VG@" check
