dnl configure.ac --- configuration setup for BBDB

dnl Author:        Didier Verna <didier@xemacs.org>
dnl Maintainer:    Didier Verna <didier@xemacs.org>
dnl Created:       Tue Nov 14 18:28:52 2000
dnl Last Revision: Tue Jan  2 16:47:01 2001

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


dnl Process this file with autoconf to produce a new configure script

AC_PREREQ([2.50])

BBDB_PRE_INIT
AC_INIT([BBDB], [BBDB_VERSION], [bbdb-info@lists.sourceforge.net])

AC_COPYRIGHT([Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>.])
AC_REVISION([$Revision$])

AC_CONFIG_SRCDIR([configure.ac])
dnl We only have install-sh right now, so it's not worth adding an aux
dnl directory just for that file.
dnl AC_ CONFIG_AUX_DIR
dnl The following macro is incompatible with the current Automake, but we
dnl don't use it for now.
AC_CONFIG_FILES([Makefile
                 lisp/Makefile
                 texinfo/Makefile
                 tex/Makefile
                 utils/Makefile])

dnl Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(RM, rm, /bin/rm)

dnl ah ah ah.
AC_MSG_CHECKING([for my keys])
AC_MSG_RESULT([not found])
echo "Damn! I'm locked outside :-("

dnl External programs checking:
BBDB_PROG_GNU_TAR
BBDB_PROG_COMPRESS
BBDB_PROG_MAKEINFO
BBDB_PROG_TEXI2DVI
BBDB_PROG_ETAGS
BBDB_PROG_EMACS

dnl Some customizations:
BBDB_ARG_SUBST([GNUSDIR], [gnus-dir], [DIR], [set the location of Gnus to DIR])
BBDB_ARG_SUBST([MHEDIR],  [mhe-dir],  [DIR], [set the location of Mh-E to DIR])
BBDB_ARG_SUBST([VMDIR],   [vm-dir],   [DIR], [set the location of VM   to DIR])

BBDB_ARG_SUBST([OTHERDIRS], [other-dirs], [DIRS],
  [set other needed directories (a list of space or colon separated paths)],
  [],
  dnl #### WARNING: pay attention to the quoting of ACTION if given !!!!!
  [[ BBDB_COLON_TO_SPACE(withval) ]])

BBDB_ARG_SUBST([PACKAGEDIR], [package-dir], [DIR],
  [set the XEmacs package directory to DIR],
  [/usr/local/lib/xemacs/site-packages])

BBDB_ARG_SUBST([SYMLINKS], [symlinks], [],
  [install BBDB by linking instead of copying], [no])

BBDB_ARG_SUBST([LINKPATH], [linkpath], [PATH],
  [path to symlink from if `pwd' does not work])

AC_OUTPUT

dnl configure.ac ends here
