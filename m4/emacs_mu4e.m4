### emacs_mu4e.m4

## Copyright (C) 2015-2017 Roland Winkler <winkler@gnu.org>
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

AC_DEFUN([EMACS_MU4E],
[
AC_ARG_WITH([mu4e-dir],
AS_HELP_STRING([--with-mu4e-dir=DIR], [where to find Mu4e lisp directory]),
# if Mu4e was requested, make sure we have access to the source
[if test "x$with_mu4e_dir" != xno -a "x$with_mu4e_dir" != "x"; then
    AC_MSG_CHECKING([for Mu4e files])
    # convert path to absolute and canonicalize it.
    MU4EDIR=$(${EMACS} -batch --quick -eval "(message \"%s\" (expand-file-name \"${with_mu4e_dir}\"))" 2>&1)
    MU4E_LOCATE=$(${EMACS} -batch --quick --directory="${MU4EDIR}" -eval "(if (locate-library \"mu4e-vars\") (message \"mu4e\"))" 2>&1)
    if test "x$MU4E_LOCATE" = "x"; then
       AC_MSG_ERROR([*** MU4E mu4e-vars.el must exist in directory passed to --with-mu4e-dir.])
    fi
    AC_MSG_RESULT($MU4EDIR)
    # append MU4EDIR to AM_ELCFLAGS
    AM_ELCFLAGS="--directory=$MU4EDIR $AM_ELCFLAGS"
 fi])
# New conditional MU4E
AM_CONDITIONAL([MU4E], [test x$MU4EDIR != x])
])
