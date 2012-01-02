AC_DEFUN([BBDB_WITH_VM],
  [ AC_ARG_WITH([vm-dir],
                AS_HELP_STRING([--with-vm-dir=DIR],
                               [where to find VM's lisp directory]))
    dnl if VM was requested, make sure we have access to the source
    if test "x$with_vm_dir" != "x"; then
       BBDB_VM=$(${EMACS_PROG} -batch --no-site-file --no-init-file -eval "(push \"${with_vm_dir}\" load-path)" -eval "(if (locate-library \"vm-autoload\") (message \"vm\"))" 2>&1)
       if test "x$BBDB_VM" = "x"; then
            AC_MSG_ERROR([*** Cannot build VM support without VM's source.])
       fi
    fi
    AC_SUBST([VMDIR], "${with_vm_dir}")
    AC_SUBST([BBDB_VM])])
