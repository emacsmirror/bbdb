# Makefile for the Insidious Big Brother Database.
# Original Author: Jamie Zawinski (jwz@netscape.com)
#
# $Id$
#
# $Log$
# Revision 1.10  2000/08/01 14:09:38  fenk
# Added setting of variable autoload-package-name and deletion of
# bbdb-autoloads.el file before creation in order to create it with the right
# featurep and contents.
# Otherwise it was not working correctly with my Xemacs.
#
# Revision 1.9  2000/07/25 17:47:19  sds
# use auto-generated autoloads consistently
#
# Revision 1.8  2000/06/30 21:58:11  kuepper
# Build bbdb-merge.elc
#
# Revision 1.7  1998/03/10 07:33:12  simmonmt
# Finally fixed that damn 19.34 :link problem - needed to put OTHERDIR
# stuff first in the load-path before loading bbdb.el.
#
# Revision 1.6  1998/02/23 07:23:23  simmonmt
# Support for OTHERDIR, rearranged flags to Emacs so we can use
# bbdb-split-string (19.34 doesn't have split-string)
#
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

GNUS	= -eval '(if (not (string-match "$(GNUSDIR)" ""))             \
		     (setq load-path (cons "$(GNUSDIR)" load-path)))'

MHE	= -eval '(progn (if (not (string-match "$(MHEDIR)" ""))            \
			    (setq load-path (cons "$(MHEDIR)" load-path))) \
			(load "mh-e"))'

# This is hideous and sick, but FSF 19.34 doesn't ship with split, and we
# can't load bbdb.el to get bbdb-split because we have to put OTHERDIR
# at the front of the load-path before loading bbdb.el.  This makes my
# head hurt.
PUSHPATH= -eval "`\
           dir=\". $(OTHERDIR)\"; \
	   echo \(setq load-path \(append \(list ; \
           for i in $$dir ; do \
              echo \\"$$i\\"\ ; \
	   done ;\
	   echo \) load-path\)\) ; \
           `"

#foo:
#	@echo $(PUSHPATH)
#	echo
#	$(EMACS) -batch -q $(PUSHPATH) -eval '(message (prin1-to-string load-path))'

#-eval '(setq load-path                                        \
#                     (append (bbdb-split (if (/= 0 (length "$(OTHERDIR)")) \
#                                              (concat ". " "$(OTHERDIR)")   \
#                                              ".") " ")                     \
#					       load-path))'

        EMACS = xemacs
     MAKEINFO = makeinfo

.SUFFIXES: .elc .el .tar .Z .gz .uu

DEPSRCS=	bbdb-com.el  bbdb-hooks.el  bbdb-gnus.el  bbdb-mhe.el \
		bbdb-rmail.el bbdb-vm.el bbdb-ftp.el bbdb-whois.el \
		bbdb-xemacs.el bbdb-print.el bbdb-srv.el bbdb-reportmail.el \
		bbdb-merge.el bbdb-migrate.el

DEPBINS=	${DEPSRCS:.el=.elc}
SRCS=		bbdb.el  $(DEPSRCS)
BINS=		bbdb.elc $(DEPBINS)

syntax:
	@echo "" ;\
	echo "*** Make should be run from the `cd ..;pwd` directory" ;\
	echo "" ;\

all:	rmail gnus vm mhe bbdb info autoloads

bbdb-autoloads.el: $(DEPSRCS)
	@rm -f $@;
	@echo "" > $@;
	$(EMACS) -batch -q -l autoload \
		--eval '(setq generated-autoload-file "'`pwd`'/$@")' \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq autoload-package-name "bbdb")' \
		--eval '(delete-file generated-autoload-file)' \
		-f batch-update-autoloads `pwd`

bbdb-autoloads.elc: bbdb-autoloads.el
	$(EMACS) -batch -q -f batch-byte-compile ./$^

bbdb.elc:            bbdb.el
bbdb-com.elc:        bbdb.elc bbdb-com.el
bbdb-ftp.elc:        bbdb.elc bbdb-ftp.el
bbdb-merge.elc:      bbdb-merge.el
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
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(GNUS)   \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-mhe.elc:	bbdb.elc bbdb-mhe.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(MHE)    \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-rmail.elc:	bbdb.elc bbdb-rmail.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(RMAIL)  \
		-f batch-byte-compile $(@:.elc=.el)
bbdb-vm.elc:	bbdb.elc bbdb-vm.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(VM)     \
		-f batch-byte-compile $(@:.elc=.el)

bbdb-srv.elc:   bbdb.elc bbdb-srv.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval '(if (and (locate-library "gnuserv") (locate-library "itimer")) (byte-compile-file "bbdb-srv.el") (message "Optional package bbdb-srv skipped - gnuserv not found"))'

bbdb-reportmail.elc: bbdb.elc bbdb-reportmail.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval '(if (locate-library "reportmail") (byte-compile-file "bbdb-reportmail.el") (message "Optional package bbdb-reportmail skipped - reportmail not found"))'

bbdb-sc.elc: bbdb.elc bbdb-sc.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc -eval '(if (locate-library "supercite") (byte-compile-file "bbdb-sc.el") (message "Optional package bbdb-sc skipped - supercite not found"))'

# bbdb-hooks uses VM macros if it can find VM.  If you don't have VM,
# then the $(VM) makefile variable should be undefined or empty.
bbdb-hooks.elc:  bbdb.elc bbdb-hooks.el
	$(EMACS) -batch -q $(PUSHPATH) -l ./bbdb.elc $(VM) \
	-f batch-byte-compile $(@:.elc=.el)

autoloads: bbdb-autoloads.elc

extras: bbdb-print.elc bbdb-ftp.elc bbdb-whois.elc bbdb-xemacs.elc  \
	bbdb-srv.elc bbdb-reportmail.elc bbdb-snarf.elc bbdb-w3.elc \
	bbdb-sc.elc bbdb-merge.elc bbdb-migrate.elc
bbdb:	bbdb.elc bbdb-com.elc bbdb-hooks.elc autoloads extras
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
	$(RM) bbdb.elc bbdb-*.elc bbdb.info bbdb-autoloads.elc

