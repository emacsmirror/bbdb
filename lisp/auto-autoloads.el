;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'bbdb-1.52-autoloads))
    (progn

;;;### (autoloads (bbdb-print) "bbdb-print" "bbdb-1.52/bbdb-print.el")

(autoload 'bbdb-print "bbdb-print" "\
Make a TeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is used instead of simply \"\\[bbdb-print]\", then includes only the 
people currently in the *BBDB* buffer.  There are various variables
for customizing the content & format of the printout, see the file
bbdb-print.el for more information." t nil)

;;;***

;;;### (autoloads (bbdb-whois) "bbdb-whois" "bbdb-1.52/bbdb-whois.el")

(autoload 'bbdb-whois "bbdb-whois" nil t nil)

;;;***

(autoload 'bbdb-ftp                 "bbdb-ftp"  "Ftp BBDB Package" t)
(autoload 'bbdb-create-ftp-site     "bbdb-ftp"  "Ftp BBDB Package" t)

(defvar bbdbid "Insidious Big Brother Database autoload")

;; tie it all together...
;;
(autoload 'bbdb		"bbdb-com" bbdbid t)
(autoload 'bbdb-name	"bbdb-com" bbdbid t)
(autoload 'bbdb-company	"bbdb-com" bbdbid t)
(autoload 'bbdb-net	"bbdb-com" bbdbid t)
(autoload 'bbdb-notes	"bbdb-com" bbdbid t)
(autoload 'bbdb-changed	"bbdb-com" bbdbid t)
(autoload 'bbdb-create	"bbdb-com" bbdbid t)
(autoload 'bbdb-dial	"bbdb-com" bbdbid t)
(autoload 'bbdb-finger	"bbdb-com" bbdbid t)
(autoload 'bbdb-info	"bbdb-com" bbdbid t)
(autoload 'bbdb-help	"bbdb-com" bbdbid t)

(autoload 'bbdb-insinuate-vm    "bbdb-vm"    "Hook BBDB into VM")
(autoload 'bbdb-insinuate-rmail "bbdb-rmail" "Hook BBDB into RMAIL")
(autoload 'bbdb-insinuate-mh    "bbdb-mhe"   "Hook BBDB into MH-E")
(autoload 'bbdb-insinuate-gnus  "bbdb-gnus"  "Hook BBDB into GNUS")

(autoload 'bbdb-apply-next-command-to-all-records "bbdb-com" bbdbid t)

(autoload 'bbdb-insert-new-field		"bbdb-com" bbdbid t)
(autoload 'bbdb-edit-current-field		"bbdb-com" bbdbid t)
(autoload 'bbdb-transpose-fields		"bbdb-com" bbdbid t)
(autoload 'bbdb-record-edit-notes		"bbdb-com" bbdbid t)
(autoload 'bbdb-delete-current-field-or-record	"bbdb-com" bbdbid t)
(autoload 'bbdb-delete-current-record		"bbdb-com" bbdbid t)
(autoload 'bbdb-refile-record			"bbdb-com" bbdbid t)
(autoload 'bbdb-elide-record			"bbdb-com" bbdbid t)
(autoload 'bbdb-omit-record			"bbdb-com" bbdbid t)
(autoload 'bbdb-send-mail			"bbdb-com" bbdbid t)
(autoload 'bbdb-complete-name			"bbdb-com" bbdbid t)
(autoload 'bbdb-yank				"bbdb-com" bbdbid t)
(autoload 'bbdb-completion-predicate            "bbdb-com" bbdbid)
(autoload 'bbdb-dwim-net-address                "bbdb-com" bbdbid)
(autoload 'bbdb-redisplay-records		"bbdb-com" bbdbid)
(autoload 'bbdb-define-all-aliases		"bbdb-com" bbdbid)
(autoload 'bbdb-read-addresses-with-completion	"bbdb-com" bbdbid)
(autoload 'bbdb-record-edit-property		"bbdb-com" bbdbid t)

(autoload 'bbdb/vm-show-sender        "bbdb-vm"    bbdbid t)
(autoload 'bbdb/vm-annotate-sender    "bbdb-vm"    bbdbid t)
(autoload 'bbdb/vm-update-record      "bbdb-vm"    bbdbid t)
(autoload 'bbdb/rmail-show-sender     "bbdb-rmail" bbdbid t)
(autoload 'bbdb/rmail-annotate-sender "bbdb-rmail" bbdbid t)
(autoload 'bbdb/rmail-update-record   "bbdb-rmail" bbdbid t)
(autoload 'bbdb/mh-show-sender        "bbdb-mhe"   bbdbid t)
(autoload 'bbdb/mh-annotate-sender    "bbdb-mhe"   bbdbid t)
(autoload 'bbdb/mh-update-record      "bbdb-mhe"   bbdbid t)
(autoload 'bbdb/gnus-show-sender      "bbdb-gnus"  bbdbid t)
(autoload 'bbdb/gnus-annotate-sender  "bbdb-gnus"  bbdbid t)
(autoload 'bbdb/gnus-update-record    "bbdb-gnus"  bbdbid t)
(autoload 'bbdb/gnus-lines-and-from   "bbdb-gnus"  bbdbid nil)

(autoload 'bbdb-extract-field-value          "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-timestamp-hook               "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-ignore-most-messages-hook    "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-ignore-some-messages-hook    "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-auto-notes-hook              "bbdb-hooks" bbdbid nil)
(autoload 'sample-bbdb-canonicalize-net-hook "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-creation-date-hook	     "bbdb-hooks" bbdbid nil)

(autoload 'bbdb-fontify-buffer	"bbdb-xemacs" bbdbid nil)
(autoload 'bbdb-menu		"bbdb-xemacs" bbdbid t)

(makunbound 'bbdbid)

(provide 'bbdb-1.52-autoloads)
))
