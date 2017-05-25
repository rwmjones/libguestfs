# libguestfs-daemon
# Copyright (C) 2011 Red Hat Inc.
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
	actions.h \
	dispatch.c \
	names.c \
	lvm-tokenization.c \
	structs-cleanups.c \
	structs-cleanups.h \
	stubs-0.c \
	stubs-1.c \
	stubs-2.c \
	stubs-3.c \
	stubs-4.c \
	stubs-5.c \
	stubs-6.c \
	stubs.h

BUILT_SOURCES = \
	$(generator_built)

EXTRA_DIST = \
	$(BUILT_SOURCES) \
	$(SOURCES_MLI) $(SOURCES_ML) $(SOURCES_C) \
	guestfsd.pod \
	guestfs-inspection.pod

if INSTALL_DAEMON
sbin_PROGRAMS = guestfsd guestfs-inspection
else
noinst_PROGRAMS = guestfsd guestfs-inspection
endif

guestfsd_SOURCES = \
	../common/errnostring/errnostring.h \
	../common/protocol/guestfs_protocol.h \
	9p.c \
	acl.c \
	actions.h \
	available.c \
	augeas.c \
	base64.c \
	blkdiscard.c \
	blkid.c \
	blockdev.c \
	btrfs.c \
	cap.c \
	checksum.c \
	cleanups.c \
	cleanups.h \
	cmp.c \
	command.c \
	command.h \
	compress.c \
	copy.c \
	cpio.c \
	cpmv.c \
	daemon.h \
	dd.c \
	debug.c \
	debug-bmap.c \
	device-name-translation.c \
	devsparts.c \
	df.c \
	dir.c \
	dispatch.c \
	dmesg.c \
	dropcaches.c \
	du.c \
	echo-daemon.c \
	ext2.c \
	fallocate.c \
	file.c \
	findfs.c \
	fill.c \
	find.c \
	format.c \
	fs-min-size.c \
	fsck.c \
	fstrim.c \
	glob.c \
	grep.c \
	grub.c \
	guestfsd.c \
	headtail.c \
	hexdump.c \
	hotplug.c \
	hivex.c \
	htonl.c \
	initrd.c \
	inotify.c \
	internal.c \
	is.c \
	isoinfo.c \
	journal.c \
	labels.c \
	ldm.c \
	link.c \
	ls.c \
	luks.c \
	lvm.c \
	lvm-filter.c \
	lvm-tokenization.c \
	md.c \
	mkfs.c \
	mknod.c \
	mktemp.c \
	modprobe.c \
	mount.c \
	mountable.c \
	names.c \
	ntfs.c \
	ntfsclone.c \
	optgroups.c \
	optgroups.h \
	parted.c \
	pingdaemon.c \
	proto.c \
	readdir.c \
	realpath.c \
	rename.c \
	rsync.c \
	scrub.c \
	selinux.c \
	selinux-relabel.c \
	sfdisk.c \
	sh.c \
	sleep.c \
	sleuthkit.c \
	squashfs.c \
	stat.c \
	statvfs.c \
	strings.c \
	structs-cleanups.c \
	structs-cleanups.h \
	stubs-0.c \
	stubs-1.c \
	stubs-2.c \
	stubs-3.c \
	stubs-4.c \
	stubs-5.c \
	stubs-6.c \
	stubs.h \
	stubs-macros.h \
	swap.c \
	sync.c \
	syslinux.c \
	tar.c \
	tsk.c \
	truncate.c \
	umask.c \
	upload.c \
	utimens.c \
	utsname.c \
	uuids.c \
	wc.c \
	xattr.c \
	xfs.c \
	yara.c \
	zero.c \
	zerofree.c

guestfsd_LDADD = \
	../common/errnostring/liberrnostring.la \
	../common/protocol/libprotocol.la \
	$(ACL_LIBS) \
	$(CAP_LIBS) \
	$(YAJL_LIBS) \
	$(SELINUX_LIBS) \
	$(AUGEAS_LIBS) \
	$(HIVEX_LIBS) \
	$(SD_JOURNAL_LIBS) \
	$(top_builddir)/gnulib/lib/.libs/libgnu.a \
	$(GETADDRINFO_LIB) \
	$(HOSTENT_LIB) \
	$(INET_NTOP_LIB) \
	$(LIBSOCKET) \
	$(LIB_CLOCK_GETTIME) \
	$(LIBINTL) \
	$(SERVENT_LIB) \
	$(PCRE_LIBS) \
	$(TSK_LIBS) \
	$(RPC_LIBS) \
	$(YARA_LIBS)

guestfsd_CPPFLAGS = \
	-I$(top_srcdir)/gnulib/lib \
	-I$(top_builddir)/gnulib/lib \
	-I$(top_srcdir)/lib \
	-I$(top_builddir)/lib \
	-I$(top_srcdir)/common/errnostring \
	-I$(top_builddir)/common/errnostring \
	-I$(top_srcdir)/common/protocol \
	-I$(top_builddir)/common/protocol
guestfsd_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS) \
	$(RPC_CFLAGS) \
	$(AUGEAS_CFLAGS) \
	$(HIVEX_CFLAGS) \
	$(SD_JOURNAL_CFLAGS) \
	$(YAJL_CFLAGS) \
	$(PCRE_CFLAGS)

SOURCES_MLI = \
	blkid.mli \
	btrfs.mli \
	chroot.mli \
	common_utils.mli \
	devsparts.mli \
	inspection_fs.mli \
	inspection_fs_cd.mli \
	inspection_fs_unix.mli \
	inspection_fs_windows.mli \
	inspection_types.mli \
	is.mli \
	ldm.mli \
	link.mli \
	listfs.mli \
	lvm2.mli \
	md.mli \
	mount.mli \
	mountable.mli \
	parted.mli \
	realpath.mli \
	unix_utils.mli \
	utils.mli

SOURCES_ML = \
	unix_utils.ml \
	guestfs_config.ml \
	common_utils.ml \
	mountable.ml \
	chroot.ml \
	utils.ml \
	cmdline.ml \
	blkid.ml \
	parted.ml \
	devsparts.ml \
	ldm.ml \
	lvm2.ml \
	md.ml \
	btrfs.ml \
	is.ml \
	link.ml \
	realpath.ml \
	listfs.ml \
	mount.ml \
	inspection_types.ml \
	inspection_fs_cd.ml \
	inspection_fs_unix.ml \
	inspection_fs_windows.ml \
	inspection_fs.ml \
	inspection.ml

SOURCES_C = \
	unix_utils-c.c

guestfs_inspection_SOURCES = $(SOURCES_C)
guestfs_inspection_CPPFLAGS = \
	-I. \
	-I$(top_builddir) \
	-I$(shell $(OCAMLC) -where)
guestfs_inspection_CFLAGS = \
	$(WARN_CFLAGS) $(WERROR_CFLAGS)

BOBJECTS = \
	$(SOURCES_ML:.ml=.cmo)
XOBJECTS = $(BOBJECTS:.cmo=.cmx)

# -I $(top_builddir)/lib/.libs is a hack which forces corresponding -L
# option to be passed to gcc, so we don't try linking against an
# installed copy of libguestfs.
OCAMLPACKAGES = \
	-package str,unix,hivex

OCAMLCLIBS =

OCAMLFLAGS = $(OCAML_FLAGS) $(OCAML_WARN_ERROR)

if !HAVE_OCAMLOPT
OBJECTS = $(BOBJECTS)
else
OBJECTS = $(XOBJECTS)
endif

OCAMLLINKFLAGS = $(LINK_CUSTOM_OCAMLC_ONLY)

guestfs_inspection_DEPENDENCIES = $(OBJECTS) $(top_srcdir)/ocaml-link.sh
guestfs_inspection_LINK = \
	$(top_srcdir)/ocaml-link.sh -cclib '$(OCAMLCLIBS)' -- \
	  $(OCAMLFIND) $(BEST) $(OCAMLFLAGS) $(OCAMLPACKAGES) $(OCAMLLINKFLAGS) \
	  $(OBJECTS) -o $@

# We share common_utils.ml{,i}, guestfs_config.ml and unix_utils*
# with the mllib directory.
#
# For common_utils we have to remove functions which depend on any
# modules which are not part of the OCaml stdlib.
common_utils.ml: $(top_srcdir)/mllib/common_utils.ml
	rm -f $@ $@-t
	echo '(* This file is generated from mllib/common_utils.ml *)' > $@-t
	sed -n '/^(\*<stdlib>\*)$$/,/^(\*<\/stdlib>\*)$$/p' $< >> $@-t
	mv $@-t $@
common_utils.mli: $(top_srcdir)/mllib/common_utils.mli
	rm -f $@ $@-t
	echo '(* This file is generated from mllib/common_utils.mli *)' > $@-t
	sed -n '/^(\*<stdlib>\*)$$/,/^(\*<\/stdlib>\*)$$/p' $< >> $@-t
	mv $@-t $@
guestfs_config.ml: ../mllib/guestfs_config.ml
	cp $< $@
unix_utils-c.c: ../mllib/unix_utils-c.c
	cp $< $@
unix_utils.ml: ../mllib/unix_utils.ml
	cp $< $@
unix_utils.mli: ../mllib/unix_utils.mli
	cp $< $@

# Manual pages and HTML files for the website.
if INSTALL_DAEMON
man_MANS = guestfsd.8 guestfs-inspection.8
noinst_DATA =
else
# It should be noinst_MANS but that doesn't work.
noinst_DATA = guestfsd.8 guestfs-inspection.8
endif
noinst_DATA += \
	$(top_builddir)/website/guestfsd.8.html \
	$(top_builddir)/website/guestfs-inspection.8.html

guestfsd.8 $(top_builddir)/website/guestfsd.8.html: stamp-guestfsd.pod

stamp-guestfsd.pod: guestfsd.pod
	$(PODWRAPPER) \
	  --section 8 \
	  --man guestfsd.8 \
	  --html $(top_builddir)/website/guestfsd.8.html \
	  --license GPLv2+ \
	  $<
	touch $@

guestfs-inspection.8 $(top_builddir)/website/guestfs-inspection.8.html: stamp-guestfs-inspection.pod

stamp-guestfs-inspection.pod: guestfs-inspection.pod
	$(PODWRAPPER) \
	  --section 8 \
	  --man guestfs-inspection.8 \
	  --html $(top_builddir)/website/guestfs-inspection.8.html \
	  --license GPLv2+ \
	  $<
	touch $@

# OCaml dependencies.
depend: .depend

.depend: $(wildcard $(abs_srcdir)/*.mli) $(wildcard $(abs_srcdir)/*.ml) common_utils.ml common_utils.mli guestfs_config.ml
	rm -f $@ $@-t
	$(OCAMLFIND) ocamldep -I ../ocaml -I $(abs_srcdir) -I $(abs_top_builddir)/mllib -I $(abs_top_builddir)/customize $^ | \
	  $(SED) 's/ *$$//' | \
	  $(SED) -e :a -e '/ *\\$$/N; s/ *\\\n */ /; ta' | \
	  $(SED) -e 's,$(abs_srcdir)/,$(builddir)/,g' | \
	  sort > $@-t
	mv $@-t $@

-include .depend

.PHONY: depend docs
