dnl aclocal.m4 --- More autoconf macros for BBDB

dnl Author:        Didier Verna <didier@xemacs.org>
dnl Maintainer:    Didier Verna <didier@xemacs.org>
dnl Created:       Tue Nov 14 18:28:52 2000
dnl Last Revision: Tue Jan  2 16:53:50 2001

dnl Copyright (C) 2000-2001 Didier Verna

dnl BBDB is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU Library General Public License as published
dnl by the Free Software Foundation; either version 2 of the License, or (at
dnl your option) any later version.

dnl BBDB is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU Library General Public License for more details.

dnl You should have received a copy of the GNU Library General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


dnl Transforms a colon separated list into a space separated one:
AC_DEFUN([BBDB_COLON_TO_SPACE],
[ case "$$1" in *:*)
    $1="`echo $$1 | sed -e 's/:/ /g'`";;
  esac ])

dnl Find GNU tar:
AC_DEFUN([BBDB_PROG_GNU_TAR],
  [ AC_CHECK_PROGS(TAR, gtar tar)
    if test "x${TAR}" = "xtar" ; then
      AC_MSG_CHECKING([that tar is GNU tar])
      ${TAR} --version > /dev/null 2>&1 || TAR=
      if test "x${TAR}" = "x" ; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT(yes)
      fi
    fi
    if test "x${TAR}" = "x" ; then
      AC_MSG_WARN([*** No GNU tar program found.])
      AC_MSG_WARN([*** Some targets will be unavailable.])
    fi ])

dnl Choose a compression program:
AC_DEFUN([BBDB_PROG_COMPRESS],
  [ AC_CHECK_PROGS(COMPRESS, gzip compress)
    AC_SUBST(COMPEXT)
    if test "x${COMPRESS}" = "x" ; then
      AC_MSG_WARN([*** No compression program found.])
      AC_MSG_WARN([*** Tarballs will not be compressed.])
      COMPEXT=
    elif test "x${COMPRESS}" = "xgzip" ; then
      COMPRESS="gzip --verbose --best"
      COMPEXT=gz
    else
      COMPEXT=Z
    fi ])

dnl Find makeinfo:
AC_DEFUN([BBDB_PROG_MAKEINFO],
  [ AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo)
    if test "x${MAKEINFO}" = "x" ; then
      AC_MSG_WARN([*** No makeinfo program found.])
      AC_MSG_WARN([*** Info files will not be built.])
    fi ])

dnl Find texi2dvi:
AC_DEFUN([BBDB_PROG_TEXI2DVI],
  [ AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi)
    if test "x${TEXI2DVI}" = "x" ; then
      AC_MSG_WARN([*** No texi2dvi program found.])
      AC_MSG_WARN([*** DVI and PDF files will not be built.])
    fi ])

dnl Find etags:
AC_DEFUN([BBDB_PROG_ETAGS],
  [ AC_CHECK_PROG(ETAGS, etags, etags)
    if test "x${ETAGS}" = "x" ; then
      AC_MSG_WARN([*** No etags program found.])
      AC_MSG_WARN([*** Tags file will not be built.])
    fi ])

dnl Choose an Emacs flavor:
dnl If I were pedantic, I'd check that the user-specified executable is
dnl actually working. I might do that someday.
AC_DEFUN([BBDB_PROG_EMACS],
  [ AC_SUBST(EMACS)
    AC_ARG_WITH([emacs],
                [  --with-emacs=PROG       which flavor of Emacs to use],
                [ EMACS="${withval}" ],
                [ AC_CHECK_PROGS(EMACS, emacs xemacs) ])
    if test "x${EMACS}" = "x" ; then
      dnl This is critical enough to generate an error and not a warning...
      AC_MSG_ERROR([*** No Emacs program found.])
    fi ])

dnl aclocal.m4 ends here
