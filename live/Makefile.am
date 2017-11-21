# libguestfs live conversion client
# Copyright (C) 2017 Red Hat Inc.
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
	live_protocol.x \
	live-protocol-extra.h

BUILT_SOURCES = \
	live_protocol.c \
	live_protocol.h \
	live-protocol-extra.h

EXTRA_DIST = \
	$(generator_built) \
	live_protocol.c \
	live_protocol.h

bin_PROGRAMS = live-conversion-client

live_conversion_client_SOURCES = \
	conversion.c \
	live.h \
	main.c

# -I lib only appears here so we can pick up <guestfs-internal-all.h>
live_conversion_client_CPPFLAGS = \
	-DLOCALEBASEDIR=\""$(datadir)/locale"\" \
	-I$(top_srcdir)/common/utils -I$(top_builddir)/common/utils \
	-I$(top_srcdir)/lib -I$(top_builddir)/lib \
	-I$(srcdir)/../gnulib/lib -I../gnulib/lib

live_conversion_client_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS)

live_conversion_client_LDADD = \
	libliveprotocol.la \
	$(top_builddir)/common/utils/libutils.la \
	../gnulib/lib/libgnu.la

noinst_LTLIBRARIES = libliveprotocol.la

# Because rpcgen generates archaic code, we cannot use ordinary
# warnings here.
libliveprotocol_la_SOURCES = live_protocol.c  live_protocol.h
libliveprotocol_la_CFLAGS = \
	-Wall -Wno-unused -fno-strict-aliasing $(GCC_VISIBILITY_HIDDEN) \
	$(RPC_CFLAGS)

if HAVE_RPCGEN

live_protocol.c: live_protocol.x
	rm -f $@-t $@-t2
	$(RPCGEN) -c -o $@-t $<
	$(SED) 's,\.\./\(\.\./\)*lib,.,' < $@-t > $@-t2
	rm $@-t
	mv $@-t2 $@

live_protocol.h: live_protocol.x
	rm -f $@-t
	$(RPCGEN) -h -o $@-t $<
	mv $@-t $@

endif

# Manual pages and HTML files for the website.

# Tests.
