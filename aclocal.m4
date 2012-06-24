dnl aclocal.m4 --- autoconf macros for BBDB

dnl Author:        Didier Verna <didier@xemacs.org>
dnl Maintainer:    Roland Winkler <winkler@gnu.org>
dnl Created:       Tue Nov 14 18:28:52 2000
dnl Last Revision: $Date: 2012/06/24 08:13:14 $

dnl Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>
dnl Copyright (C) 2011-2012 Roland Winkler <winkler@gnu.org>

dnl This file is part of the Insidious Big Brother Database (aka BBDB),

dnl BBDB is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl BBDB is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with BBDB.  If not, see <http://www.gnu.org/licenses/>.


dnl BBDB_PRE_INIT
dnl Find BBDB version number and put it in the m4 macro BBDB_VERSION

m4_define([BBDB_PRE_INIT],
[ m4_define([BBDB_VERSION],
             m4_esyscmd([sed -n 's/^(defconst bbdb-version "\([^"]*\)".*)/\1/p' lisp/bbdb.el | xargs printf '%s'
                        ]))])

AC_DEFUN([BBDB_WITH_VM],
  [ AC_ARG_WITH([vm-dir],
                AS_HELP_STRING([--with-vm-dir=DIR],
                               [where to find VM's lisp directory]))
    dnl if VM was requested, make sure we have access to the source
    if test "x$with_vm_dir" != "x"; then
       BBDB_VM=`${EMACS_PROG} -batch --no-site-file --no-init-file -eval "(push \"${with_vm_dir}\" load-path)" -eval "(if (locate-library \"vm-autoload\") (message \"vm\"))" 2>&1`
       if test "x$BBDB_VM" = "x"; then
            AC_MSG_ERROR([*** Cannot build VM support without VM's source.])
       fi
    fi
    AC_SUBST([VMDIR], "${with_vm_dir}")
    AC_SUBST([BBDB_VM])])

dnl BBDB_PROG_GNU_TAR
dnl
dnl Find a (g)tar program and make sure it is GNU one. A failure is not fatal
dnl since tar is needed for non critical targets only.
AC_DEFUN([BBDB_PROG_GNU_TAR],
  [ AC_CHECK_PROGS(TAR, gtar tar)
    if test "x${TAR}" != "x" ; then
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

dnl BBDB_PROG_COMPRESS
dnl
dnl Find a gzip compression program. A failure is not fatal, only
dnl tarballs won't be compressed.
AC_DEFUN([BBDB_PROG_COMPRESS],
  [ AC_CHECK_PROG(COMPRESS, gzip, gzip)
    if test "x${COMPRESS}" = "xgzip" ; then
      COMPRESS="gzip --verbose --best"
    else
      AC_MSG_WARN([*** Tarballs will not be compressed.])
    fi ])

dnl BBDB_PROG
dnl
dnl Find a $1,$2 program. Warning $3
AC_DEFUN([BBDB_PROG],
  [ AC_CHECK_PROG([$1],[$2],[$2])
    if test "x$1" = "x" ; then
      AC_MSG_WARN([$3])
    fi ])

dnl BBDB_PROG_EMACS
dnl
dnl Choose an Emacs flavor according to the --with-emacs user option,
dnl or try emacs.
dnl We use EMACS_PROG instead of EMACS to avoid colliding with Emacs'
dnl own internal environment which sets the EMACS environment variable
dnl to 't' if you run a shell window in Emacs.
AC_DEFUN([BBDB_PROG_EMACS],
  [ AC_ARG_WITH([emacs],
                AS_HELP_STRING([--with-emacs=PROG],
                               [which Emacs to use]),
                [ AC_PATH_PROG(EMACS_PROG, ${withval}, ${withval}) ],
                [ AC_CHECK_PROG(EMACS_PROG, emacs, emacs) ])
    if test "x${EMACS_PROG}" = "x" ; then
       AC_MSG_ERROR([*** No Emacs program found.])
    elif test `${EMACS_PROG} --version | head -1 | sed 's/^GNU Emacs \(.*\)$/\1/'` "<" 23; then
       AC_MSG_ERROR([*** Need at least GNU Emacs 23.])
    fi
  ])

AC_DEFUN(BBDB_PATH_LISPDIR,
   [ AC_ARG_WITH([lispdir],
                 AS_HELP_STRING([--with-lispdir=DIR],
                                [where to install lisp files]))
     AC_MSG_CHECKING([where .elc files should go])
     if test "x$with_lispdir" = "x"; then
        theprefix=$prefix
        if test "x$theprefix" = "xNONE"; then
           theprefix=$ac_default_prefix
        fi
        with_lispdir="\$(datadir)/emacs/site-lisp/bbdb"
        for dir in share lib; do
           if test -d "${theprefix}/${dir}/emacs/site-lisp"; then
              with_lispdir="${theprefix}/${dir}/emacs/site-lisp/bbdb"
              break
           fi
        done
     fi
     AC_MSG_RESULT($with_lispdir)
     AC_SUBST(lispdir, "$with_lispdir")
   ])

AC_DEFUN(BBDB_PATH_TEXMFDIR,
   [ AC_ARG_WITH(texmf-dir,
                 AS_HELP_STRING([--with-texmf-dir=DIR],
                                [where to install tex files]))
     dnl If the user does not help us, finding the right location
     dnl for texmf-dir can be tricky. So we try to use kpsexpand,
     dnl then we give up.
     if test "x${with_texmf_dir}" = "x" ; then
       AC_CHECK_PROG(KPSEXPAND, kpsexpand, kpsexpand)
       if test "x${KPSEXPAND}" != "x" ; then
        with_texmf_dir="`kpsexpand \\$TEXMFLOCAL`"
       fi
     fi
     if test "x${with_texmf_dir}" = "x"; then
      AC_MSG_WARN([no directory for installing .tex files])
     else
      AC_MSG_CHECKING([where .tex files should go])
      if test ! -d "${with_texmf_dir}" ; then
        AC_MSG_ERROR(["${with_texmf_dir}": Directory does not exist])
      fi
      with_texmf_dir="${with_texmf_dir}/bbdb"
     fi
     AC_MSG_RESULT($with_texmf_dir)
     AC_SUBST(texmf_dir, "$with_texmf_dir")
   ])

dnl aclocal.m4 ends here
