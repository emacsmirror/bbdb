# Makefile for the Insidious Big Brother Database.
# last change 21-may-96. jwz.

# If the VM, GNUS, or MH-E source is not in the standard emacs library 
# (that is, it's not on the load-path by default in a -q emacs) then
# set these variables to point at them.  You need to do this because
# otherwise "require" won't work in the batch emacs.
#
# If you don't have VM, the "VM=" line *must* be commented out.

VMDIR   	= /usr/local/lib/xemacs-19.14/lisp/vm/
GNUSDIR 	= /usr/local/lib/xemacs-19.14/lisp/gnus/
MHEDIR 		= /usr/local/lib/xemacs-19.14/lisp/mh-e/

# use this line for VM versions 5.31 and earlier
#VM	= -l $(VMDIR)vm-version.elc -l $(VMDIR)vm-vars.elc -l $(VMDIR)vm.elc

# use this line for VM versions 5.32 and later
VM	= -l $(VMDIR)vm.elc

GNUS	= -l $(GNUSDIR)nntp.elc -l $(GNUSDIR)gnus.elc
MHE	= -l $(MHEDIR)mh-e.elc

        EMACS = xemacs
          TAR = tar
     COMPRESS = gzip --verbose --best
 COMPRESS_EXT = gz
#    COMPRESS = compress
#COMPRESS_EXT = Z

# You shouldn't need to change anything after this point.

.SUFFIXES: .elc .el .tar .Z .gz .uu

DEPSRCS=	bbdb-com.el  bbdb-hooks.el  bbdb-gnus.el  bbdb-mhe.el \
		bbdb-rmail.el  bbdb-vm.el bbdb-415-510.el bbdb-213-310.el \
		bbdb-ftp.el  bbdb-whois.el  bbdb-xemacs.el  bbdb-print.el \
		bbdb-srv.el
DEPBINS=	bbdb-com.elc bbdb-hooks.elc bbdb-gnus.elc bbdb-mhe.elc \
		bbdb-rmail.elc bbdb-vm.elc bbdb-415-510.elc bbdb-213-310.elc \
		bbdb-ftp.elc bbdb-whois.elc bbdb-xemacs.elc bbdb-print.elc \
		bbdb-srv.elc
SRCS=		bbdb.el  $(DEPSRCS) mail-abbrevs.el \
		mail-extr.el advice.el
BINS=		bbdb.elc $(DEPBINS) mail-abbrevs.elc \
		mail-extr.elc

syntax:
	@echo "" ;\
	echo "*** make one or more of: rmail vm mhe gnus all bbdb" ;\
	echo "" ;\
	exit 1

all:	rmail gnus vm mhe

mail-extr.elc: mail-extr.el
	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)

mail-abbrevs.elc: mail-abbrevs.el
	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)

advice.elc: advice.el
	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)

bbdb.elc:	bbdb.el
	$(EMACS) -batch -q -f batch-byte-compile ./bbdb.el


#$(DEPBINS):
#	$(EMACS) -batch -q -l ./bbdb.elc $(OLOADS) -f batch-byte-compile $(@:.elc=.el)


bbdb-com.elc:	bbdb.elc bbdb-com.el
	$(EMACS) -batch -q -l ./bbdb.elc -f batch-byte-compile $(@:.elc=.el)

bbdb-gnus.elc:	bbdb.elc bbdb-gnus.el
	$(EMACS) -batch -q -l ./bbdb.elc $(GNUS) -f batch-byte-compile $(@:.elc=.el)
bbdb-mhe.elc:	bbdb.elc bbdb-mhe.el
	$(EMACS) -batch -q -l ./bbdb.elc $(MHE) -f batch-byte-compile $(@:.elc=.el)
bbdb-rmail.elc:	bbdb.elc bbdb-rmail.el
	$(EMACS) -batch -q -l ./bbdb.elc $(RMAIL) -f batch-byte-compile $(@:.elc=.el)
bbdb-vm.elc:	bbdb.elc bbdb-vm.el
	$(EMACS) -batch -q -l ./bbdb.elc $(VM) -f batch-byte-compile $(@:.elc=.el)

bbdb-xemacs.elc: bbdb.elc bbdb-com.elc bbdb-xemacs.el
	$(EMACS) -batch -q -l ./bbdb.elc -l ./bbdb-com.elc -f batch-byte-compile $(@:.elc=.el)
bbdb-print.elc:	bbdb.elc bbdb-com.elc bbdb-print.el
	$(EMACS) -batch -q -l ./bbdb.elc -l ./bbdb-com.elc -f batch-byte-compile $(@:.elc=.el)
bbdb-ftp.elc:	bbdb.elc bbdb-com.elc bbdb-ftp.el
	$(EMACS) -batch -q -l ./bbdb.elc -l ./bbdb-com.elc -f batch-byte-compile $(@:.elc=.el)
bbdb-whois.elc:	bbdb.elc bbdb-com.elc bbdb-whois.el
	$(EMACS) -batch -q -l ./bbdb.elc -l ./bbdb-com.elc -f batch-byte-compile $(@:.elc=.el)
bbdb-srv.elc:	bbdb.elc bbdb-com.elc bbdb-srv.el
	$(EMACS) -batch -q -l ./bbdb.elc -l ./bbdb-com.elc -f batch-byte-compile $(@:.elc=.el)

# bbdb-hooks uses VM macros if it can find VM.  If you don't have VM,
# then the $(VM) makefile variable should be undefined or empty.
bbdb-hooks.elc:  bbdb.elc bbdb-hooks.el
	$(EMACS) -batch -q -l ./bbdb.elc $(VM) -f batch-byte-compile $(@:.elc=.el)


extras: bbdb-print.elc bbdb-ftp.elc bbdb-whois.elc mail-abbrevs.elc bbdb-xemacs.elc bbdb-srv.elc
bbdb:	bbdb.elc bbdb-com.elc bbdb-hooks.elc mail-extr.elc extras
rmail:	bbdb bbdb-rmail.elc
vm:	bbdb bbdb-vm.elc
mhe:	bbdb bbdb-mhe.elc
gnus:	bbdb bbdb-gnus.elc
# aliases
mh:	mhe
mh-e:	mhe

clean:
	$(RM) bbdb.elc bbdb-*.elc mail-extr.elc mail-abbrevs.elc

TARFILES=	bbdb-Makefile bbdb.texinfo bbdb.el $(DEPSRCS) \
		mail-abbrevs.el mail-extr.el bbdb-print.tex \
		multicol.tex

tar: $(TARFILES)
	@NAME=`sed -n							     \
  's/^(defconst bbdb-version "\([0-9]\.[0-9][0-9]*\).*/bbdb-\1/p' bbdb.el` ; \
  rm -f $$NAME ; ln -s . $$NAME ;					    \
  echo creating tar file $${NAME}.tar.$(COMPRESS_EXT)... ;		    \
   $(TAR) -vchf - `echo $(TARFILES)				    	    \
   | sed "s|^|$$NAME/|g; s| | $$NAME/|g" `				    \
   | $(COMPRESS) > $${NAME}.tar.$(COMPRESS_EXT) ;			    \
  rm $$NAME
