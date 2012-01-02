# Command line flags for Emacs.
EMACSOPT = --batch --no-site-file --no-init-file

# The actual Emacs command run in the targets below.
emacs = LC_ALL=C $(EMACS) $(EMACSOPT)

PUSHPATH = -eval '(push "./" load-path)'

el_files_std = bbdb.el bbdb-com.el bbdb-print.el bbdb-anniv.el		\
	   bbdb-migrate.el bbdb-mua.el bbdb-message.el bbdb-rmail.el	\
	   bbdb-gnus.el bbdb-mhe.el

el_files_ver = bbdb-version.el

el_files_vm = bbdb-vm.el

EXTRA_DIST = $(el_files_std) $(el_files_vm)

el_files = $(el_files_std) $(el_files_ver)
if HAVE_VM
el_files += $(el_files_vm)
endif

lisp_DATA = $(el_files) $(el_files:.el=.elc) bbdb-loaddefs.el bbdb-loaddefs.elc

MOSTLYCLEANFILES = $(el_files:.el=.elc) bbdb-loaddefs.el bbdb-loaddefs.elc

# The variable generated-autoload-file must be set to an absolute
# path, but $srcdir can be relative, so we use the current working
# directory explicitly.

bbdb-loaddefs.el: $(el_files)
	@echo "(provide 'bbdb-loaddefs)" > $@;
	@echo "(if (and load-file-name (file-name-directory load-file-name))" >> $@;
	@echo "    (add-to-list 'load-path (file-name-directory load-file-name)))" >> $@;
	@echo "" >> $@;
	$(emacs) -l autoload					\
		--eval '(setq generated-autoload-file "'$$(pwd)/$@'")'	\
		--eval '(setq make-backup-files nil)'			\
		-f batch-update-autoloads $$(pwd)

.el.elc:
	$(emacs) $(PUSHPATH) $(EMACS_EXTRA_FLAGS) -f batch-byte-compile $<

bbdb.elc: bbdb-version.elc

bbdb-com.elc bbdb-migrate.elc bbdb-anniv.elc bbdb-ispell.elc		\
bbdb-mua.elc bbdb-print.elc bbdb-rmail.elc bbdb-gnus.elc bbdb-mhe.elc:	\
bbdb.elc bbdb-version.elc

bbdb-mua.elc bbdb-print.elc bbdb-rmail.elc bbdb-gnus.elc bbdb-mhe.elc:	\
bbdb-com.elc

bbdb-rmail.elc bbdb-gnus.elc bbdb-mhe.elc: bbdb-mua.elc

# VM is not part of GNU Emacs
if HAVE_VM
VM = -eval '(if (not (string-match "$(VMDIR)" "")) (setq load-path (cons "$(VMDIR)" load-path)))'
bbdb-mua.elc: EMACS_EXTRA_FLAGS = -eval '(unless (string= "$(VMDIR)" "") (push "$(VMDIR)" load-path) (load "vm" t t))'
bbdb-vm.elc: bbdb.elc bbdb-version.elc bbdb-com.elc bbdb-mua.elc
bbdb-vm.elc: EMACS_EXTRA_FLAGS = $(VM)
endif

autoloads: bbdb-loaddefs.el

.PHONY: autoloads
