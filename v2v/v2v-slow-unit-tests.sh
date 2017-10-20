#!/bin/bash -
# libguestfs virt-v2v test script
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

# Wrapper around v2v_slow_unit_tests (OCaml test program).

set -e

$TEST_FUNCTIONS
slow_test
skip_if_skipped

rm -f v2v-slow-unit-tests-*.img

# These images are used for testing.
virt-builder fedora-26 \
             -o v2v-slow-unit-tests-fedora-26.img \
             --mkdir /tmp/notowned \
             --touch /tmp/notowned2
virt-builder debian-9 \
             -o v2v-slow-unit-tests-debian-9.img \
             --mkdir /tmp/notowned \
             --touch /tmp/notowned2

# Run the actual tests.
./v2v_slow_unit_tests

rm v2v-slow-unit-tests-*.img
