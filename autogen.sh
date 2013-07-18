#!/bin/sh
### autogen.sh - tool to help build BBDB from a git checkout

## Copyright (C) 2013 Christian Egli <christian.egli@sbs.ch>
##               and Roland Winkler <winkler@gnu.org>
##
## This file is part of the Insidious Big Brother Database (aka BBDB),
##
## BBDB is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## BBDB is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## The BBDB git repository does not include the configure script
## (and associated helpers).  The first time you fetch BBDB from git,
## run this script to generate the necessary files.

### Code:

set -e

# Refresh GNU autotools toolchain.
echo "Cleaning autotools files..."
find -type d -name autom4te.cache -print0 | xargs -0 rm -rf \;
find -type f \( -name missing -o -name install-sh -o -name mkinstalldirs \
	-o -name depcomp -o -name ltmain.sh -o -name configure \
	-o -name config.sub -o -name config.guess -o -name config.h.in \
        -o -name mdate-sh -o -name texinfo.tex \
	-o -name Makefile.in -o -name aclocal.m4 \) -print0 | xargs -0 rm -f

echo "Running autoreconf..."
autoreconf --install --verbose --warnings=all

echo "You can now run \`./configure'."

exit 0
