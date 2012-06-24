dnl configure.ac --- configuration setup for BBDB

dnl Author:        Didier Verna <didier@xemacs.org>
dnl Maintainer:    Roland Winkler <winkler@gnu.org>
dnl Created:       Tue Nov 14 18:28:52 2000
dnl Last Revision: $Date: 2012/06/24 08:13:19 $

dnl Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>
dnl Copyright (C) 2011-2012 Roland Winkler <winkler@gnu.org>

dnl BBDB is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl BBDB is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

dnl Process this file with autoconf to produce a new configure script

AC_PREREQ(2.65)

BBDB_PRE_INIT
AC_INIT([BBDB],[BBDB_VERSION],[bbdb-info@lists.sourceforge.net])

AC_COPYRIGHT([Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>.
Copyright (C) 2011 Roland Winkler <winkler@gnu.org>.])
AC_REVISION([$Revision: 1.13 $])

AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_FILES([Makefile
                 lisp/Makefile
                 doc/Makefile
                 tex/Makefile])

dnl Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(GREP, grep, /bin/grep)

AC_PATH_PROG(RM, rm, /bin/rm)
dnl If RM really /is/ rm (i.e. we're not on DOS, for example)
dnl then make it 'quiet'.
if test "${RM}" = "/bin/rm" -o "${RM}" = "/usr/bin/rm"; then
  RM="${RM} -f"
fi

dnl External programs checking:
BBDB_PROG_GNU_TAR
BBDB_PROG_COMPRESS
BBDB_PROG([MAKEINFO],[makeinfo],[*** Use Emacs to build info files.])
BBDB_PROG([TEXI2DVI],[texi2dvi],[*** DVI files will not be built.])
BBDB_PROG([TEXI2PDF],[texi2pdf],[*** PDF files will not be built.])
BBDB_PROG([DVIPS],[dvips],[*** PS files will not be built.])
BBDB_PROG([ETAGS],[etags],[*** Tags file will not be built.])
BBDB_PROG_EMACS
BBDB_WITH_VM

BBDB_PATH_LISPDIR
BBDB_PATH_TEXMFDIR

dnl For now this only allows you to see the compile commands
AC_ARG_ENABLE(developer,
              AS_HELP_STRING([--enable-developer],
                             [enable developer hooks [no]]),
              [if test "x$enableval" = "xyes"; then HUSHMAKE=; else HUSHMAKE=@; fi],
              [HUSHMAKE=@])
AC_SUBST([HUSHMAKE])

AC_OUTPUT

dnl configure.ac ends here
