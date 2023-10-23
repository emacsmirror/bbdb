### emacs_vm.m4

## Copyright (C) 2010-2022  Free Software Foundation, Inc.
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

AC_DEFUN([EMACS_NOTMUCH],
[
AC_ARG_WITH([notmuch-dir],
AS_HELP_STRING([--with-notmuch-dir=DIR], [where to find NOTMUCH lisp directory]),
# if NOTMUCH was requested, make sure we have access to the source
[if test "x$with_notmuch_dir" != xno -a "x$with_notmuch_dir" != "x"; then
    AC_MSG_CHECKING([for NOTMUCH files])
    # convert path to absolute and canonicalize it.
    NOTMUCHDIR=$(${EMACS} -batch --quick -eval "(message \"%s\" (expand-file-name \"${with_notmuch_dir}\"))" 2>&1)
    NOTMUCH_LOCATE=$(${EMACS} -batch --quick --directory="${NOTMUCHDIR}" -eval "(if (locate-library \"notmuch\") (message \"notmuch\"))" 2>&1)
    if test "x$NOTMUCH_LOCATE" = "x"; then
       AC_MSG_ERROR([*** NOTMUCH notmuch.el must exist in directory passed to --with-notmuch-dir.])
    fi
    AC_MSG_RESULT($NOTMUCHDIR)
    # append NOTMUCHDIR to AM_ELCFLAGS
    AM_ELCFLAGS="--directory=$NOTMUCHDIR $AM_ELCFLAGS"
 fi])
# New conditional NOTMUCH
AM_CONDITIONAL([NOTMUCH], [test x$NOTMUCHDIR != x])
])
