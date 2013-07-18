#                                               -*- Autoconf -*-
# configuration setup for BBDB

# Author:        Didier Verna <didier@xemacs.org>
# Maintainer:    Roland Winkler <winkler@gnu.org>
# Created:       Tue Nov 14 18:28:52 2000
# Last Revision: $Date: 2013/07/16 15:01:37 $
# 
# Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>
# Copyright (C) 2011-2013 Roland Winkler <winkler@gnu.org>
# 
# BBDB is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# BBDB is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with BBDB.  If not, see <http://www.gnu.org/licenses/>.
# 
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.69)

AC_INIT([BBDB],[3.02],[bbdb-info@lists.sourceforge.net])
# We need GNU Automake 1.14 for AM_ELCFLAGS.
AM_INIT_AUTOMAKE([1.14])

# Checks for programs.
AC_PROG_INSTALL

# Search for Emacs
AM_PATH_LISPDIR

# Checks for libraries.
AC_ARG_WITH([vm-dir],
AS_HELP_STRING([--with-vm-dir=DIR], [where to find VM's lisp directory]),
# if VM was requested, make sure we have access to the source
[if test "x$with_vm_dir" != "x"; then
    AC_MSG_CHECKING([for VM files])
    # convert path to absolute and canonicalize it.
    VMDIR=`${EMACS} -batch --quick -eval "(message \"%s\" (expand-file-name \"${with_vm_dir}\"))" 2>&1`
    BBDB_VM=`${EMACS} -batch --quick --directory=${VMDIR} -eval "(if (or (locate-library \"vm-autoload\") (locate-library \"vm-autoloads\")) (message \"vm\"))" 2>&1`
    if test "x$BBDB_VM" = "x"; then
       AC_MSG_ERROR([*** VM's vm-autoloads.el must exist in directory passed to `--with-vm-dir'.])
    fi
    AC_MSG_RESULT($VMDIR)
    AM_ELCFLAGS=--directory=$VMDIR
    AC_SUBST([AM_ELCFLAGS])
 fi])
AM_CONDITIONAL([VM], [test x$VMDIR != x])

AC_CONFIG_FILES([Makefile
                 lisp/Makefile
                 doc/Makefile
                 tex/Makefile])

AC_OUTPUT

