# Makefile for the Insidious Big Brother Database.
# Original Author: Jamie Zawinski (jwz@netscape.com)
#
# $Id$
#
# $Log$
# Revision 1.60  2000/04/12 23:37:06  waider
# Cleanup work
#
# Revision 1.59  1998/04/11 07:23:03  simmonmt
# Fix for compatibility with more makes
#
# Revision 1.58  1998/03/10 07:27:02  simmonmt
# Removed my paths, added comments about the Custom requirement for
# building Gnus support under 19.34
#
# Revision 1.57  1998/02/23 07:08:48  simmonmt
# Alphabetized MUA directory variables, added OTHERDIR variable
#
# Revision 1.56  1997/11/02 06:35:43  simmonmt
# Fixed package install - `foo 'bar` (as opposed to `foo 'bar'`) works
# in Bourne shell, but not in others.  That and it's not exactly
# intuitive.
#
# Revision 1.55  1997/10/26 05:11:20  simmonmt
# Installation timing change: .el before .elc.  Tried to optimize
# install
#
# Revision 1.54  1997/10/11 23:48:40  simmonmt
# Removed my paths from VMDIR and MHEDIR.  Seems I had broken the
# documented 'if these are blank and the packages live on load-path,
# Emacs will find them' behavior by dying if VMDIR, MHEDIR and GNUSDIR
# weren't set.  It's fixed now.
#
# Revision 1.53  1997/10/06 01:15:13  simmonmt
# Fixed for new grand reorg.  Rewrote XEmacs package installation code.
#
#

# ************************
# MUA-specific definitions
# ************************

# If the VM, GNUS, or MH-E source is not in the standard emacs library 
# (that is, it's not on the load-path by default in a -q emacs) then
# set these variables to point at them.  You need to do this because
# otherwise "require" won't work in the batch emacs.
#
# Set the ones you've got, and comment out the rest.
GNUSDIR         = 
MHEDIR 	        = 
VMDIR           = 

# ***************************
# Locations of other packages
# ***************************
#
# Add the directories (if any) containing the optional packages you
# will use with the BBDB (see the "Byte Compiling the Lisp files"
# section of the BBDB manual for more information).  If more than one
# directory is to be used, separate the directories with spaces.  Do
# not quote the directory names.  GNU Emacs 19.34 should put the path
# to Custom 1.9962 here if they plan to build Gnus support.
OTHERDIR	=

# **************************
# XEmacs Packagization (sp?)
# **************************

# If you're installing this as an XEmacs package, either set the following
# variable to point to the root of the package directory, or set it on the
# command line ( 'make PACKAGEROOT=foo' )
#PACKAGEROOT=

# Uncomment this definition if you want the lisp and info files to be linked
# in (as opposed to being copied)
#LINKTOPACKAGE=yes

# If you uncommented the above, the lisp and info directories will be linked
# from this directory to PACKAGEROOT.  For example, lisp will be linked with:
#
#   ln -s `pwd`/lisp $(PACKAGEROOT)/bbdb/lisp
#
# If `pwd` will not return the correct path, set LINKPATH below to the correct
# path.
#LINKPATH=

# *******************************
# Other important things to check
# *******************************

        EMACS = xemacs -no-site-file -no-init-file
     MAKEINFO = makeinfo

# Uncomment one of the below
#  SYSVINSTALL = /usr/sbin/install
   BSDINSTALL = /usr/ucb/install

          TAR = tar
     COMPRESS = gzip --verbose --best
 COMPRESS_EXT = gz

   BUILDFLAGS = $(MAKEFLAGS) "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" \
		"VMDIR=$(VMDIR)" "GNUSDIR=$(GNUSDIR)" "MHEDIR=$(MHEDIR)" \
		"OTHERDIR=$(OTHERDIR)"

#    COMPRESS = compress
#COMPRESS_EXT = Z

# You shouldn't need to change anything after this point.

syntax:
	@echo "" ;\
	echo "*** make one or more of: rmail vm mhe gnus all bbdb" ;\
	echo "" 

all: bbdb rmail vm mhe gnus info

bbdb:
	cd lisp; $(MAKE) $(BUILDFLAGS) bbdb

rmail:
	cd lisp; $(MAKE) $(BUILDFLAGS) rmail

vm:
	cd lisp; $(MAKE) $(BUILDFLAGS) vm

mhe:
	cd lisp; $(MAKE) $(BUILDFLAGS) mhe

gnus:
	cd lisp; $(MAKE) $(BUILDFLAGS) gnus

autoloads:
	cd lisp; $(MAKE) $(BUILDFLAGS) autoloads

install-pkg: bbdb autoloads info
	if [ -z "$(PACKAGEROOT)" ] ; then \
	   echo "You must specify PACKAGEROOT (see Makefile)"; \
	   exit 1 ; \
	else \
	   rm -fr $(PACKAGEROOT)/lisp/bbdb $(PACKAGEROOT)/info/bbdb \
		  $(PACKAGEROOT)/etc/bbdb; \
           if [ -z "$(LINKTOPACKAGE)" ] ; then \
	      mkdir -p -m 0755 $(PACKAGEROOT)/lisp/bbdb; \
	      if [ -z "$(SYSVINSTALL)" ] ; then \
		for i in `ls lisp/*.elc` ; do \
		   $(BSDINSTALL) -c -m 0644 `echo $$i | sed 's/c$$//g'` \
			$(PACKAGEROOT)/lisp/bbdb ; \
		   $(BSDINSTALL) -c -m 0644 $$i $(PACKAGEROOT)/lisp/bbdb ; \
		done ; \
	      else \
		for i in `ls lisp/*.elc` ; do \
		   $(SYSVINSTALL) -c $(PACKAGEROOT)/lisp/bbdb -s -m 0644 \
			`echo $$i | sed 's/c$$//g'` $(PACKAGEROOT)/lisp/bbdb ; \
		   $(SYSVINSTALL) -c $(PACKAGEROOT)/lisp/bbdb -s -m 0644 $$i ; \
		done ; \
	      fi ; \
	      mkdir -p -m 0755 $(PACKAGEROOT)/info/bbdb ; \
	      if [ -z "$(SYSVINSTALL)" ] ; then \
		for i in `ls texinfo/*.info* ` ; do \
		   $(BSDINSTALL) -c -m 0644 $$i $(PACKAGEROOT)/info/bbdb ; \
		done ; \
	      else \
		for i in `ls texinfo/*.info* ` ; do \
		   $(SYSVINSTALL) -c $(PACKAGEROOT)/info/bbdb -s -m 0644 $$i ; \
		done ; \
	      fi ; \
	      mkdir -p -m 0755 $(PACKAGEROOT)/etc/bbdb/tex \
			       $(PACKAGEROOT)/etc/bbdb/utils ; \
	      if [ -z "$(SYSVINSTALL)" ] ; then \
		for i in `ls tex/*.tex` ; do \
		   $(BSDINSTALL) -c -m 0644 $$i $(PACKAGEROOT)/etc/bbdb/tex ; \
		done ; \
		for i in `ls -d utils/* |egrep -v '(RCS|SCCS)'` ; do \
		   $(BSDINSTALL) -c -m 0644 $$i $(PACKAGEROOT)/etc/bbdb/utils ; \
		done ; \
	      else \
		for i in `ls tex/*.tex` ; do \
		   $(SYSVINSTALL) -c $(PACKAGEROOT)/etc/bbdb/tex -s -m 0644 $$i; \
		done ; \
		for i in `ls -d utils/* |egrep -v '(RCS|SCCS)'` ; do \
		   $(SYSVINSTALL) -c $(PACKAGEROOT)/etc/bbdb/utils -s -m 0644 $$i; \
		done ; \
	      fi ; \
	   else \
	      if [ -z "$(LINKPATH)" ] ; then \
		 ln -s `pwd`/lisp $(PACKAGEROOT)/lisp/bbdb ; \
		 ln -s `pwd`/texinfo $(PACKAGEROOT)/info/bbdb ; \
	      else \
		 ln -s $(LINKPATH)/lisp $(PACKAGEROOT)/lisp/bbdb ; \
		 ln -s $(LINKPATH)/texinfo $(PACKAGEROOT)/info/bbdb ; \
	      fi ; \
	   fi ; \
	fi

info:
	cd texinfo; $(MAKE)

clean:
	cd lisp; $(MAKE) clean
	cd texinfo; $(MAKE) clean

reallyclean: clean
	cd texinfo; $(MAKE) reallyclean

# Testing
test: test-setup test-bbdb

test-setup:
	cp $$HOME/.bbdb $$HOME/.bbdb-test

test-bbdb:
	emacs -l $$HOME/bbdb-test.el

# FSF Emacs 19.34

emacs19.34-test: emacs19.34-test-setup emacs19.34-test-bbdb
emacs19.34-test: emacs19.34-test-rmail emacs19.34-test-vm
emacs19.34-test: emacs19.34-test-mhe   emacs19.34-test-gnus
emacs19.34-test: emacs19.34-test-all

emacs19.34-test-setup:
	@echo '--- TESTING BBDB WITH FSF EMACS 19.34 ---'
	@echo
	@echo '** Setting up **'
	make clean
	rm -f /local/downloads/emacs-19.34/*
	cp /home/simmonmt/gnus/lisp/*.el /p/local/elisp-19.34/gnus/lisp

emacs19.34-test-bbdb:
	@echo
	@echo '** Testing build of "bbdb" **'
	@echo
	-make EMACS=emacs-19.34 GNUSDIR=/p/local/elisp-19.34/gnus/lisp \
	      OTHERDIR=/p/local/elisp-19.34/custom-1.9962 bbdb

emacs19.34-test-rmail:
	@echo
	@echo '** Testing build of "rmail" **'
	@echo
	-make EMACS=emacs-19.34 GNUSDIR=/p/local/elisp-19.34/gnus/lisp \
	      OTHERDIR=/p/local/elisp-19.34/custom-1.9962 rmail

emacs19.34-test-vm:
	@echo
	@echo '** Testing build of "vm" **'
	@echo
	-make EMACS=emacs-19.34 GNUSDIR=/p/local/elisp-19.34/gnus/lisp \
	      OTHERDIR=/p/local/elisp-19.34/custom-1.9962 vm

emacs19.34-test-mhe:
	@echo
	@echo '** Testing build of "mhe" **'
	@echo
	-make EMACS=emacs-19.34 GNUSDIR=/p/local/elisp-19.34/gnus/lisp \
	      OTHERDIR=/p/local/elisp-19.34/custom-1.9962 mhe

emacs19.34-test-gnus:
	@echo
	@echo '** Testing build of "gnus" **'
	@echo
	-make EMACS=emacs-19.34 GNUSDIR=/p/local/elisp-19.34/gnus/lisp \
	      OTHERDIR=/p/local/elisp-19.34/custom-1.9962 gnus

emacs19.34-test-all:
	@echo
	@echo '** Testing build of "all" **'
	@echo
	-make clean
	-make EMACS=emacs-19.34 GNUSDIR=/p/local/elisp-19.34/gnus/lisp \
	      OTHERDIR=/p/local/elisp-19.34/custom-1.9962 all

# FSF Emacs 20.2

emacs20.2-test: emacs20.2-test-setup emacs20.2-test-bbdb
emacs20.2-test: emacs20.2-test-rmail emacs20.2-test-vm
emacs20.2-test: emacs20.2-test-mhe   emacs20.2-test-gnus
emacs20.2-test: emacs20.2-test-all

emacs20.2-test-setup:
	@echo '--- TESTING BBDB WITH FSF EMACS 20.2 ---'
	@echo
	@echo '** Setting up **'
	make clean
	rm -f /p/local/elisp-20.2/gnus/lisp/*
	cp /home/simmonmt/gnus/lisp/*.el /p/local/elisp-20.2/gnus/lisp

emacs20.2-test-bbdb:
	@echo
	@echo '** Testing build of "bbdb" **'
	@echo
	-make EMACS=emacs-20.2 GNUSDIR=/p/local/elisp-20.2/gnus/lisp bbdb

emacs20.2-test-rmail:
	@echo
	@echo '** Testing build of "rmail" **'
	@echo
	-make EMACS=emacs-20.2 GNUSDIR=/p/local/elisp-20.2/gnus/lisp rmail

emacs20.2-test-vm:
	@echo
	@echo '** Testing build of "vm" **'
	@echo
	-make EMACS=emacs-20.2 GNUSDIR=/p/local/elisp-20.2/gnus/lisp vm

emacs20.2-test-mhe:
	@echo
	@echo '** Testing build of "mhe" **'
	@echo
	-make EMACS=emacs-20.2 GNUSDIR=/p/local/elisp-20.2/gnus/lisp mhe

emacs20.2-test-gnus:
	@echo
	@echo '** Testing build of "gnus" **'
	@echo
	-make EMACS=emacs-20.2 GNUSDIR=/p/local/elisp-20.2/gnus/lisp gnus

emacs20.2-test-all:
	@echo
	@echo '** Testing build of "all" **'
	@echo
	-make clean
	-make EMACS=emacs-20.2 GNUSDIR=/p/local/elisp-20.2/gnus/lisp all


# Deployment
TARFILES=Makefile ChangeLog INSTALL README lisp misc tex texinfo utils

tar: $(TARFILES)
	@NAME=`sed -n							     \
  's/^(defconst bbdb-version "\([0-9]\.[0-9][0-9]*\).*/bbdb-\1/p' lisp/bbdb.el` ; \
  rm -f $$NAME ; ln -s . $$NAME ;					    \
  echo creating tar file $${NAME}.tar.$(COMPRESS_EXT)... ;		    \
   $(TAR) --exclude=CVS -vchf - `echo $(TARFILES)				    	    \
   | sed "s|^|$$NAME/|g; s| | $$NAME/|g" `				    \
   | $(COMPRESS) > $${NAME}.tar.$(COMPRESS_EXT) ;			    \
  rm $$NAME

dist: clean info tar

TAGS: tags

tags:
	etags */*.el

