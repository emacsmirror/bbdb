dnl Process this file with autoconf to produce a new configure script

AC_PREREQ([2.64])

BBDB_PRE_INIT
AC_INIT([BBDB],[BBDB_VERSION],[bbdb-info@lists.sourceforge.net])

AC_COPYRIGHT([Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>.
Copyright (C) 2011 Roland Winkler <winkler@gnu.org>.])

AC_CONFIG_SRCDIR([lisp/bbdb.el])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([foreign])

AM_SILENT_RULES

BBDB_WITH_VM
if test "$VMDIR" = no; then
  AC_MSG_WARN([VM mailer sources not found.
      Will not create autoloads for, compile, or install VM support.])
fi
AM_CONDITIONAL([HAVE_VM], test "$VMDIR" != no)

if test x"$texmfdir" = x; then
  AC_MSG_WARN([texmfdir not set, TeX files will not be installed.
      For potential values for texmfdir, run: kpsewhich -show-path .tex])
fi
AC_SUBST(texmfdir)
AM_CONDITIONAL([HAVE_TEXMF], test x"${texmfdir}" != x)

AM_PATH_LISPDIR

AC_CONFIG_FILES([Makefile
                 lisp/Makefile
                 doc/Makefile
                 tex/Makefile])

AC_OUTPUT
