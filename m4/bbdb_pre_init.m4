dnl BBDB_PRE_INIT
dnl Find BBDB version number and put it in the m4 macro BBDB_VERSION

m4_define([BBDB_PRE_INIT],
[ m4_define([BBDB_VERSION],
             m4_esyscmd([sed -n 's/^(defconst bbdb-version "\([^"]*\)".*)/\1/p' lisp/bbdb.el | xargs printf '%s'
                        ]))])
