;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'bbdb-autoloads))
    (progn

;;;### (autoloads (bbdb-whois) "bbdb-whois" "bbdb/bbdb-whois.el")

(autoload 'bbdb-whois "bbdb-whois" nil t nil)

;;;***

;;;### (autoloads (bbdb-print) "bbdb-print" "lisp/bbdb-print.el")

(autoload 'bbdb-print "bbdb-print" "\
Make a TeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is used instead of simply \"\\[bbdb-print]\", then includes only the 
people currently in the *BBDB* buffer.  With a prefix argument, makes
a brief (one-line-per-entry) printout.

There are various variables for customizing the content & format of
the printout, notably the variables `bbdb-print-alist' and
`bbdb-print-require'.  See the file bbdb-print.el for more information." t nil)

;;;***

;;;### (autoloads (bbdb-snarf) "bbdb-snarf" "lisp/bbdb-snarf.el")

(autoload 'bbdb-snarf "bbdb-snarf" "\
snarf up a bbdb record WHERE the point is.
We assume things are line-broken and paragraph-bounded.
The name comes first and other fields (address, 
phone, email, web pages) are recognized by context.

Requred context:
	addresses end with \"City, State ZIP\" or \"City, State\"
	phones match bbdb-snarf-phone-regexp
		(currently US-style phones)
	e-mail addresses have @'s in them
	web sites are recognized by http:// or www.

Address and phone context are currently US-specific;
patches to internationalize these assumptions are welcome.

\\[bbdb-snarf] is similar to \\[bbdb-whois-sentinel], but less specialized." t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-w3 bbdb-www-grab-homepage bbdb-www) "bbdb-w3" "lisp/bbdb-w3.el")

(autoload 'bbdb-www "bbdb-w3" "\
Visit URL's stored in `www' fields of the current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-www] means to try all records currently visible.
Non-interactively, do all records if arg is nonnil." t nil)

(autoload 'bbdb-www-grab-homepage "bbdb-w3" "\
Grab the current URL and store it in the bbdb database" t nil)

(autoload 'bbdb-insinuate-w3 "bbdb-w3" "\
Call this function to hook BBDB into W3." nil nil)

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

(autoload 'bbdb-insinuate-vm      "bbdb-vm"    "Hook BBDB into VM")
(autoload 'bbdb-insinuate-rmail   "bbdb-rmail" "Hook BBDB into RMAIL")
(autoload 'bbdb-insinuate-mh      "bbdb-mhe"   "Hook BBDB into MH-E")
(autoload 'bbdb-insinuate-gnus    "bbdb-gnus"  "Hook BBDB into GNUS")
(autoload 'bbdb-insinuate-message "bbdb-gnus"  "Hook BBDB into message")

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

(provide 'bbdb-autoloads)
))
