# Makefile for the Insidious Big Brother Database.
# Original Author: Jamie Zawinski (jwz@netscape.com)
#
# $Id$
#
# $Log$
# Revision 1.1  1997/10/06 01:16:00  simmonmt
# Initial revision
#
#

# this is lovely, isn't it?  Surprisingly enough, it seems to work...
VM	= -eval '(cond ((not (string-match "$(VMDIR)" ""))            \
                        (progn (load-file "$(VMDIR)/vm-version.elc")  \
			(cond ((> (string-to-number vm-version) 5.31) \
		        (load-file "$(VMDIR)/vm.elc"))                \
                       (t (load-file "$(VMDIR)/vm-vars.elc")          \
                          (load-file "$(VMDIR)/vm.elc"))))))'

GNUS	= -eval '(setq load-path (cons "$(GNUSDIR)" load-path))' \
	  -l $(GNUSDIR)/nntp.elc -l $(GNUSDIR)/gnus.elc
MHE	= -l $(MHEDIR)/mh-e.elc

        EMACS = xemacs
     MAKEINFO = makeinfo

.SUFFIXES: .elc .el .tar .Z .gz .uu

DEPSRCS=	bbdb-com.el  bbdb-hooks.el  bbdb-gnus.el  bbdb-mhe.el \
		bbdb-rmail.el bbdb-vm.el bbdb-ftp.el bbdb-whois.el \
		bbdb-xemacs.el bbdb-print.el bbdb-srv.el bbdb-reportmail.el

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
bbdb-print.elc:      bbdb.elc bbdb-print.el
bbdb-whois.elc:      bbdb.elc bbdb-whois.el
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
	@$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval \
	'(if (locate-library "gnuserv") (byte-compile-file "bbdb-srv.el") (message "bbdb-srv skipped"))'

bbdb-reportmail.elc: bbdb.elc bbdb-reportmail.el
	@$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval \
	'(if (locate-library "reportmail") (byte-compile-file "bbdb-reportmail.el") (message "bbdb-reportmail skipped"))'

# bbdb-hooks uses VM macros if it can find VM.  If you don't have VM,
# then the $(VM) makefile variable should be undefined or empty.
bbdb-hooks.elc:  bbdb.elc bbdb-hooks.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(VM) \
	-f batch-byte-compile $(@:.elc=.el)

autoloads: auto-autoloads.elc

extras: bbdb-print.elc bbdb-ftp.elc bbdb-whois.elc bbdb-xemacs.elc bbdb-srv.elc \
	bbdb-reportmail.elc
bbdb:	bbdb.elc bbdb-com.elc bbdb-hooks.elc extras
rmail:	bbdb bbdb-rmail.elc
vm:	bbdb bbdb-vm.elc
mhe:	bbdb bbdb-mhe.elc
gnus:	bbdb bbdb-gnus.elc
# aliases
mh:	mhe
mh-e:	mhe

clean:
	$(RM) bbdb.elc bbdb-*.elc bbdb.info auto-autoloads.elc

