# Makefile for the Insidious Big Brother Database.
# Original Author: Jamie Zawinski (jwz@netscape.com)
#
# $Id$
#
# $Log$
# Revision 1.5  1998/01/06 06:48:50  simmonmt
# Fixed bug
#
# Revision 1.4  1998/01/06 06:45:39  simmonmt
# Added deploy target.  Added migrate.el
#
# Revision 1.3  1997/11/02 06:32:38  simmonmt
# Support for bbdb-sc.el
#
# Revision 1.2  1997/10/12 00:24:41  simmonmt
# Changed VM, GNUS, and MHE definitions.  If the corresponding DIR
# variables are set, the (the DIR vars) are added to load-path.  The
# relevant el files are then loaded using the load-path.  This allows
# VMDIR, et al to be unset if the packages live in load-path by
# default.  Added bbdb-snarf and bbdb-w3.  Made the bbdb-srv and
# bbdb-reportmail skip messages more informative for the poor souls (FSF
# Emacs users) who can't use them.  Added a check for itimer, since
# apparently some FSF Emacs users have installed gnuserv - without
# itimer, bbdb-srv still won't compile.
#
# Revision 1.1  1997/10/06 01:16:00  simmonmt
# Initial revision
#
#

# this is lovely, isn't it?  Surprisingly enough, it seems to work...
VM	= -eval '(progn (if (not (string-match "$(VMDIR)" ""))            \
			    (setq load-path (cons "$(VMDIR)" load-path))) \
			(if (load "vm-version" t)                         \
			    (cond ((> (string-to-number vm-version) 5.31) \
                                   (load "vm"))                           \
                                  (t (load "vm-vars") (load "vm")))))'

GNUS	= -eval '(progn (if (not (string-match "$(GNUSDIR)" ""))            \
		            (setq load-path (cons "$(GNUSDIR)" load-path))) \
			(load "nntp") (load "gnus"))'
MHE	= -eval '(progn (if (not (string-match "$(MHEDIR)" ""))            \
			    (setq load-path (cons "$(MHEDIR)" load-path))) \
			(load "mh-e"))'

        EMACS = xemacs
     MAKEINFO = makeinfo

.SUFFIXES: .elc .el .tar .Z .gz .uu

DEPSRCS=	bbdb-com.el  bbdb-hooks.el  bbdb-gnus.el  bbdb-mhe.el \
		bbdb-rmail.el bbdb-vm.el bbdb-ftp.el bbdb-whois.el \
		bbdb-xemacs.el bbdb-print.el bbdb-srv.el bbdb-reportmail.el \
		bbdb-migrate.el

DEPBINS=	${DEPSRCS:.el=.elc}
SRCS=		bbdb.el  $(DEPSRCS)
BINS=		bbdb.elc $(DEPBINS)

PUSHPATH=	-eval '(setq load-path (cons "." load-path))'

syntax:
	@echo "" ;\
	echo "*** make one or more of: rmail vm mhe gnus all bbdb" ;\
	echo "" ;\

all:	rmail gnus vm mhe bbdb info

auto-autoloads.elc: auto-autoloads.el
	$(EMACS) -batch -q -f batch-byte-compile ./auto-autoloads.el

bbdb.elc:            bbdb.el
bbdb-com.elc:        bbdb.elc bbdb-com.el
bbdb-ftp.elc:        bbdb.elc bbdb-ftp.el
bbdb-migrate.elc:    bbdb.elc bbdb-migrate.el
bbdb-print.elc:      bbdb.elc bbdb-print.el
bbdb-snarf.elc:      bbdb.elc bbdb-snarf.el
bbdb-whois.elc:      bbdb.elc bbdb-whois.el
bbdb-w3.elc:         bbdb.elc bbdb-w3.el
bbdb-xemacs.elc:     bbdb.elc bbdb-xemacs.el

.el.elc:
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -f batch-byte-compile $<

bbdb.elc:	bbdb.el
	$(EMACS) -batch -q -f batch-byte-compile ./bbdb.el

bbdb-gnus.elc:	bbdb.elc bbdb-gnus.el
	$(EMACS) -batch -q -l ./bbdb.elc $(GNUS) -f batch-byte-compile $(@:.elc=.el)
bbdb-mhe.elc:	bbdb.elc bbdb-mhe.el
	$(EMACS) -batch -q -l ./bbdb.elc $(MHE) -f batch-byte-compile $(@:.elc=.el)
bbdb-rmail.elc:	bbdb.elc bbdb-rmail.el
	$(EMACS) -batch -q -l ./bbdb.elc $(RMAIL) -f batch-byte-compile $(@:.elc=.el)
bbdb-vm.elc:	bbdb.elc bbdb-vm.el
	$(EMACS) -batch -q -l ./bbdb.elc $(VM) -f batch-byte-compile $(@:.elc=.el)

bbdb-srv.elc:   bbdb.elc bbdb-srv.el
	@$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval '(if (and (locate-library "gnuserv") (locate-library "itimer")) (byte-compile-file "bbdb-srv.el") (message "Optional package bbdb-srv skipped - gnuserv not found"))'

bbdb-reportmail.elc: bbdb.elc bbdb-reportmail.el
	@$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval '(if (locate-library "reportmail") (byte-compile-file "bbdb-reportmail.el") (message "Optional package bbdb-reportmail skipped - reportmail not found"))'

bbdb-sc.elc: bbdb.elc bbdb-sc.el
	@$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval '(if (locate-library "supercite") (byte-compile-file "bbdb-sc.el") (message "Optional package bbdb-sc skipped - supercite not found"))'

# bbdb-hooks uses VM macros if it can find VM.  If you don't have VM,
# then the $(VM) makefile variable should be undefined or empty.
bbdb-hooks.elc:  bbdb.elc bbdb-hooks.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(VM) \
	-f batch-byte-compile $(@:.elc=.el)

autoloads: auto-autoloads.elc

extras: bbdb-print.elc bbdb-ftp.elc bbdb-whois.elc bbdb-xemacs.elc  \
	bbdb-srv.elc bbdb-reportmail.elc bbdb-snarf.elc bbdb-w3.elc \
	bbdb-sc.elc bbdb-migrate.el
bbdb:	bbdb.elc bbdb-com.elc bbdb-hooks.elc extras
rmail:	bbdb bbdb-rmail.elc
vm:	bbdb bbdb-vm.elc
mhe:	bbdb bbdb-mhe.elc
gnus:	bbdb bbdb-gnus.elc
# aliases
mh:	mhe
mh-e:	mhe

deploy:
	@for fname in `cd RCS; ls |sed 's/,v$$//g'` ; do \
	  if [ -w "$$fname" ] ; then \
	    echo "File $$fname needs to be checked in" ; \
	    exit 1 ; \
	  fi ; \
	done ; \
	if [ -z "$(REVTAG)" ] ; then \
	  echo "REVTAG needs to be set" ; \
	  exit 1 ; \
	fi ; \
	rcs "-n$(REVTAG)": RCS/* ; \
	co -kv RCS/*

clean:
	$(RM) bbdb.elc bbdb-*.elc bbdb.info auto-autoloads.elc

