# Copyright (C) 2010, 2011 Roland Winkler <winkler@gnu.org>
# 
# This file is part of the Insidious Big Brother Database (aka BBDB),
# 
# BBDB is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# BBDB is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with BBDB.  If not, see <http://www.gnu.org/licenses/>.



INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

RM   = /bin/rm -f
LN_S = ln -s

EMACS = emacs

# Command line flags for Emacs.
EMACSOPT = -batch --no-site-file --no-init-file

# The actual Emacs command run in the targets below.
emacs = LC_ALL=C $(EMACS) $(EMACSOPT)

GNUSDIR = 
# This should point to your vm/lisp directory.
# If you do not use vm, set this to null and remove vm from the list
# of dependencies of all.
VMDIR = ~/emacs/site-lisp/vm/lisp
OTHERDIRS = 

PACKAGEDIR = @PACKAGEDIR@
SYMLINKS = no
LINKPATH = 

VM	= -eval '(if (not (string-match "$(VMDIR)" "")) (setq load-path (cons "$(VMDIR)" load-path)))'

GNUS	= -eval '(if (not (string-match "$(GNUSDIR)" "")) (setq load-path (cons "$(GNUSDIR)" load-path)))'

PUSHPATH = -eval '(push "./" load-path)'

.SUFFIXES: .elc .el .tar .Z .gz .uu

DEPSRCS=	bbdb-com.el bbdb-print.el bbdb-migrate.el \
		bbdb-mua.el bbdb-message.el bbdb-rmail.el \
		bbdb-gnus.el bbdb-mhe.el bbdb-vm.el

DEPBINS=	${DEPSRCS:.el=.elc}
SRCS=		bbdb.el  $(DEPSRCS)
BINS=		bbdb.elc $(DEPBINS)

all: Makefile bbdb autoloadsc vm

# Makefile: Makefile.in
# 	@echo "Makefile.in has changed, rerun configure!"
# 	exit 1

install-pkg: uninstall-pkg bbdb autoloadsc
	@if test "x$(SYMLINKS)" = "xno" ; then                 \
          mkdir -p -m 0755 $(PACKAGEDIR)/lisp/bbdb;            \
          for i in `ls *.elc` ; do                             \
            $(INSTALL_DATA) `echo $$i | sed 's/c$$//g'`        \
                            $(PACKAGEDIR)/lisp/bbdb ;          \
            $(INSTALL_DATA) $$i $(PACKAGEDIR)/lisp/bbdb ;      \
          done ;                                               \
        else                                                   \
          if test "x$(LINKPATH)" = "x" ; then                  \
            $(LN_S) `pwd` $(PACKAGEDIR)/lisp/bbdb ;            \
          else                                                 \
            $(LN_S) $(LINKPATH)/lisp $(PACKAGEDIR)/lisp/bbdb ; \
          fi ;                                                 \
        fi

uninstall-pkg:
	-$(RM) -r $(PACKAGEDIR)/lisp/bbdb

bbdb-autoloads.el: $(SRCS)
	@-$(RM) $@;
	@echo "(provide 'bbdb-autoloads)" > $@;
	@echo "(if (and load-file-name (file-name-directory load-file-name))" >> $@;
	@echo "    (add-to-list 'load-path (file-name-directory load-file-name)))" >> $@;
	@echo "" >> $@;
	@$(emacs) -batch -l autoload \
		--eval '(setq generated-autoload-file "'`pwd`'/$@")' \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq autoload-package-name "bbdb")' \
		-f batch-update-autoloads `pwd`

bbdb-autoloads.elc: bbdb-autoloads.el
	@$(emacs) -batch -f batch-byte-compile ./$^

.el.elc:
	@$(emacs) -batch $(PUSHPATH) -f batch-byte-compile $<

bbdb.elc:	bbdb.el
	@$(emacs) -batch -f batch-byte-compile $(@:.elc=.el)

bbdb-com.elc:	bbdb.elc bbdb-com.el
	@$(emacs) -batch $(PUSHPATH) \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-mua.elc:  bbdb.elc bbdb-com.elc bbdb-mua.el
	@$(emacs) -batch $(PUSHPATH) \
	-eval '(unless (string= "$(VMDIR)" "") (push "$(VMDIR)" load-path) (load "vm" t t))' \
	-f batch-byte-compile $(@:.elc=.el)
bbdb-rmail.elc:	bbdb.elc bbdb-com.elc bbdb-mua.elc bbdb-rmail.el
	@$(emacs) -batch $(PUSHPATH) \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-gnus.elc:	bbdb.elc bbdb-com.elc bbdb-mua.elc bbdb-gnus.el
	@$(emacs) -batch $(PUSHPATH) \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-mhe.elc:	bbdb.elc bbdb-com.elc bbdb-mua.elc bbdb-mhe.el
	@$(emacs) -batch $(PUSHPATH) \
		-f batch-byte-compile $(@:.elc=.el)
# VM is not part of GNU Emacs
bbdb-vm.elc:	bbdb.elc bbdb-com.elc bbdb-mua.elc bbdb-vm.el
	@$(emacs) -batch $(PUSHPATH) $(VM) \
		-f batch-byte-compile $(@:.elc=.el)

bbdb-print.elc:	bbdb.elc bbdb-com.elc bbdb-print.el
	@$(emacs) -batch $(PUSHPATH) \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-migrate.elc:	bbdb.elc bbdb-migrate.el
	@$(emacs) -batch $(PUSHPATH) \
		-f batch-byte-compile $(@:.elc=.el)

autoloadsc: bbdb-autoloads.elc

bbdb:	bbdb.elc bbdb-com.elc autoloadsc bbdb-print.elc bbdb-migrate.elc \
	bbdb-mua.elc bbdb-message.elc bbdb-rmail.elc bbdb-gnus.elc \
	bbdb-mhe.elc
vm:	bbdb-vm.elc

# Assorted clean-up targets
clean:
	-$(RM) bbdb*.elc

distclean: clean

cvsclean: distclean
	-$(RM) bbdb-autoloads.el  # Generated file
	-$(RM) Makefile
