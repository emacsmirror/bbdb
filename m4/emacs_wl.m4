### emacs_wl.m4

## Copyright (C) 2016-2017 Roland Winkler <winkler@gnu.org>
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

AC_DEFUN([EMACS_WL],
[
AC_ARG_WITH([wl-dir],
AS_HELP_STRING([--with-wl-dir=DIR], [where to find Wl lisp directory]),
# if Wl was requested, make sure we have access to the source
[if test "x$with_wl_dir" != xno -a "x$with_wl_dir" != "x"; then
    AC_MSG_CHECKING([for Wl files])
    # convert path to absolute and canonicalize it.
    WLDIR=$(${EMACS} -batch --quick -eval "(message \"%s\" (expand-file-name \"${with_wl_dir}\"))" 2>&1)
    WL_LOCATE=$(${EMACS} -batch --quick --directory="${WLDIR}" -eval "(if (locate-library \"wl-vars\") (message \"wl\"))" 2>&1)
    if test "x$WL_LOCATE" = "x"; then
       AC_MSG_ERROR([*** WL wl-vars.el must exist in directory passed to --with-wl-dir.])
    fi
    AC_MSG_RESULT($WLDIR)
    # append WLDIR to AM_ELCFLAGS
    AM_ELCFLAGS="--directory=$WLDIR $AM_ELCFLAGS"
 fi])
# New conditional WL
AM_CONDITIONAL([WL], [test x$WLDIR != x])
])
