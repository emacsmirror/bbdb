# Makefile for the Insidious Big Brother Database.
# last change 21-may-96. jwz.

# If the VM, GNUS, or MH-E source is not in the standard emacs library 
# (that is, it's not on the load-path by default in a -q emacs) then
# set these variables to point at them.  You need to do this because
# otherwise "require" won't work in the batch emacs.
#
# If you don't have VM, the "VM=" line *must* be commented out.

VMDIR   	= /p/local/xemacs-20.3/lib/xemacs-20.3-b24/lisp/vm
GNUSDIR 	= /home/simmonmt/gnus/lisp
MHEDIR 		= /p/local/xemacs-20.3/lib/xemacs-20.3-b24/lisp/mh-e

# use this line for VM versions 5.31 and earlier
#VM	= -l $(VMDIR)/vm-version.elc -l $(VMDIR)/vm-vars.elc -l $(VMDIR)/vm.elc

# use this line for VM versions 5.32 and later
VM	= -l $(VMDIR)/vm.elc

GNUS	= -eval '(setq load-path (cons "$(GNUSDIR)" load-path))' \
	  -l $(GNUSDIR)/nntp.elc -l $(GNUSDIR)/gnus.elc
MHE	= -l $(MHEDIR)/mh-e.elc

        EMACS = xemacs
     MAKEINFO = makeinfo
          TAR = tar
     COMPRESS = gzip --verbose --best
 COMPRESS_EXT = gz
#    COMPRESS = compress
#COMPRESS_EXT = Z

# You shouldn't need to change anything after this point.

.SUFFIXES: .elc .el .tar .Z .gz .uu

DEPSRCS=	bbdb-com.el  bbdb-hooks.el  bbdb-gnus.el  bbdb-mhe.el \
		bbdb-rmail.el bbdb-vm.el bbdb-ftp.el bbdb-whois.el \
		bbdb-xemacs.el bbdb-print.el bbdb-srv.el bbdb-reportmail.el

DEPBINS=	${DEPSRCS:.el=.elc}
SRCS=		bbdb.el  $(DEPSRCS)
BINS=		bbdb.elc $(DEPBINS)

syntax:
	@echo "" ;\
	echo "*** make one or more of: rmail vm mhe gnus all bbdb" ;\
	echo "" ;\

all:	rmail gnus vm mhe info

info:	bbdb.info

bbdb.info: bbdb.texinfo
	$(MAKEINFO) bbdb.texinfo

auto-autoloads.elc: auto-autoloads.el
	$(EMACS) -batch -q -f batch-byte-compile ./auto-autoloads.el

install-pkg: all auto-autoloads.elc bbdb.info
	mkdir -p ../etc/bbdb

bbdb.elc:            bbdb.el
bbdb-com.elc:        bbdb.elc bbdb-com.el
bbdb-ftp.elc:        bbdb.elc bbdb-ftp.el
bbdb-print.elc:      bbdb.elc bbdb-print.el
bbdb-reportmail.elc: bbdb.elc bbdb-reportmail.el
bbdb-srv.elc:        bbdb.elc bbdb-srv.el
bbdb-whois.elc:      bbdb.elc bbdb-whois.el
bbdb-xemacs.elc:     bbdb.elc bbdb-xemacs.el

.el.elc:
	$(EMACS) -batch -q -l ./bbdb.elc -f batch-byte-compile $<

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

# bbdb-hooks uses VM macros if it can find VM.  If you don't have VM,
# then the $(VM) makefile variable should be undefined or empty.
bbdb-hooks.elc:  bbdb.elc bbdb-hooks.el
	$(EMACS) -batch -q -l ./bbdb.elc $(VM) -f batch-byte-compile $(@:.elc=.el)


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

TARFILES=	bbdb-Makefile bbdb.texinfo bbdb.el $(DEPSRCS) \
		bbdb-print.tex multicol.tex

tar: $(TARFILES)
	@NAME=`sed -n							     \
  's/^(defconst bbdb-version "\([0-9]\.[0-9][0-9]*\).*/bbdb-\1/p' bbdb.el` ; \
  rm -f $$NAME ; ln -s . $$NAME ;					    \
  echo creating tar file $${NAME}.tar.$(COMPRESS_EXT)... ;		    \
   $(TAR) -vchf - `echo $(TARFILES)				    	    \
   | sed "s|^|$$NAME/|g; s| | $$NAME/|g" `				    \
   | $(COMPRESS) > $${NAME}.tar.$(COMPRESS_EXT) ;			    \
  rm $$NAME
