;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'bbdb-autoloads))
    (progn

;;;### (autoloads (bbdb-help bbdb-info bbdb-creation-no-change bbdb-creation-newer bbdb-creation-older bbdb-timestamp-newer bbdb-timestamp-older bbdb-finger bbdb-dial bbdb-define-all-aliases bbdb-yank bbdb-complete-name bbdb-read-addresses-with-completion bbdb-completion-predicate bbdb-show-all-recipients bbdb-send-mail bbdb-dwim-net-address bbdb-refile-record bbdb-omit-record bbdb-elide-record bbdb-delete-current-record bbdb-delete-current-field-or-record bbdb-transpose-fields bbdb-record-edit-property bbdb-record-edit-notes bbdb-edit-current-field bbdb-insert-new-field bbdb-apply-next-command-to-all-records bbdb-create bbdb-redisplay-records bbdb-changed bbdb-notes bbdb-net bbdb-company bbdb-name bbdb) "bbdb-com" "lisp/bbdb-com.el")

(autoload 'bbdb "bbdb-com" "\
Display all entries in the BBDB matching the regexp STRING 
in either the name(s), company, network address, or notes." t nil)

(autoload 'bbdb-name "bbdb-com" "\
Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names)." t nil)

(autoload 'bbdb-company "bbdb-com" "\
Display all entries in BBDB matching STRING in the company field." t nil)

(autoload 'bbdb-net "bbdb-com" "\
Display all entries in BBDB matching regexp STRING in the network address." t nil)

(autoload 'bbdb-notes "bbdb-com" "\
Display all entries in BBDB matching STRING in the named notes field." t nil)

(autoload 'bbdb-changed "bbdb-com" "\
Display all entries in the bbdb database which have been changed since
the database was last saved." t nil)

(autoload 'bbdb-redisplay-records "bbdb-com" "\
Regrinds the contents of the *BBDB* buffer, without scrolling.
If possible, you should call bbdb-redisplay-one-record instead." nil nil)

(autoload 'bbdb-create "bbdb-com" "\
Add a new entry to the bbdb database; prompts for all relevant info
using the echo area, inserts the new record in the db, sorted alphabetically,
and offers to save the db file.  DO NOT call this from a program.  Call
bbdb-create-internal instead." t nil)

(autoload 'bbdb-apply-next-command-to-all-records "bbdb-com" "\
Typing \\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] in the *BBDB* buffer makes the next command operate on all
of the records currently displayed.  (Note that this only works for
certain commands.)" t nil)

(autoload 'bbdb-insert-new-field "bbdb-com" "\
Add a new field to the current record; the field type and contents
are prompted for if not supplied.

If you are inserting a new phone-number field, you can control whether
it is a north american or european phone number by providing a prefix
argument.  A prefix arg of ^U means it's to be a euronumber, and any
other prefix arg means it's to be a a structured north american number.
Otherwise, which style is used is controlled by the variable
bbdb-north-american-phone-numbers-p." t nil)

(autoload 'bbdb-edit-current-field "bbdb-com" "\
Edit the contents of the Insidious Big Brother Database field displayed on 
the current line (this is only meaningful in the \"*BBDB*\" buffer.)   If the 
cursor is in the middle of a multi-line field, such as an address or comments 
section, then the entire field is edited, not just the current line." t nil)

(autoload 'bbdb-record-edit-notes "bbdb-com" nil t nil)

(autoload 'bbdb-record-edit-property "bbdb-com" nil t nil)

(autoload 'bbdb-transpose-fields "bbdb-com" "\
This is like the `transpose-lines' command, but it is for BBDB fields.
If the cursor is on a field of a BBDB record, that field and the previous
field will be transposed.

With argument ARG, takes previous line and moves it past ARG fields.
With argument 0, interchanges field point is in with field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone-number
fields are listed, but you can't use it to make an address appear before a
phone number; the order of field types is fixed.)" t nil)

(autoload 'bbdb-delete-current-field-or-record "bbdb-com" "\
Delete the line which the cursor is on; actually, delete the field which
that line represents from the database.  If the cursor is on the first line
of a database entry (the name/company line) then the entire entry will be
deleted." t nil)

(autoload 'bbdb-delete-current-record "bbdb-com" "\
Delete the entire bbdb database entry which the cursor is within." t nil)

(autoload 'bbdb-elide-record "bbdb-com" "\
Toggle whether the current record is displayed expanded or elided
\(multi-line or one-line display.)  With a numeric argument of 0, the 
current record will unconditionally be made elided; with any other argument,
the current record will unconditionally be shown expanded.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-elide-record]\" is used instead of simply \"\\[bbdb-elide-record]\", then the state of all records will
be changed instead of just the one at point.  In this case, an argument 
of 0 means that all records will unconditionally be made elided; any other
numeric argument means that all of the records will unconditionally be shown
expanded; and no numeric argument means that the records are made to be in
the opposite state of the record under point." t nil)

(autoload 'bbdb-omit-record "bbdb-com" "\
Remove the current record from the display without deleting it from the
database.  With a prefix argument, omit the next N records.  If negative, 
omit backwards." t nil)

(autoload 'bbdb-refile-record "bbdb-com" "\
Merge the current record into some other record; that is, delete the
record under point after copying all of the data within it into some other
record.  this is useful if you realize that somehow a redundant record has
gotten into the database, and you want to merge it with another.

If both records have names and/or companies, you are asked which to use.
Phone numbers, addresses, and network addresses are simply concatenated.
The first record is the record under the point; the second is prompted for.
Completion behaviour is as dictated by the variable `bbdb-completion-type'." t nil)

(autoload 'bbdb-dwim-net-address "bbdb-com" "\
Returns a string to use as the email address of the given record.  The
given address is the address the mail is destined to; this is formatted like
\"Firstname Lastname <addr>\" unless both the first name and last name are
constituents of the address, as in John.Doe@SomeHost, or the address is
already in the form \"Name <foo>\" or \"foo (Name)\", in which case the
address is used as-is." nil nil)

(autoload 'bbdb-send-mail "bbdb-com" "\
Compose a mail message to the person indicated by the current bbdb record.
The first (most-recently-added) address is used if there are more than one.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-send-mail]\" is used instead of simply \"\\[bbdb-send-mail]\", then mail will be sent to all of the
folks listed in the *BBDB* buffer instead of just the person at point." t nil)

(autoload 'bbdb-show-all-recipients "bbdb-com" "\
*Display BBDB records for all recipients of the message in this buffer." t nil)

(autoload 'bbdb-completion-predicate "bbdb-com" "\
For use as the third argument to completing-read, to obey the
semantics of bbdb-completion-type." nil nil)

(autoload 'bbdb-read-addresses-with-completion "bbdb-com" "\
Like read-string, but allows bbdb-complete-name style completion." nil nil)

(autoload 'bbdb-complete-name "bbdb-com" "\
Complete the user full-name or net-address before point (up to the 
preceeding newline, colon, or comma).  If what has been typed is unique,
insert an entry of the form \"User Name <net-addr>\".  If it is a valid
completion but not unique, a list of completions is displayed.  

Completion behaviour can be controlled with 'bbdb-completion-type'." t nil)

(autoload 'bbdb-yank "bbdb-com" "\
Insert the current contents of the *BBDB* buffer at point." t nil)

(autoload 'bbdb-define-all-aliases "bbdb-com" "\
Define mail aliases for some of the records in the database.
Every record which has a `mail-alias' field will have a mail alias
defined for it which is the contents of that field.  If there are 
multiple comma-separated words in the `mail-alias' field, then all
of those words will be defined as aliases for that person.

If multiple entries in the database have the same mail alias, then 
that alias expands to a comma-separated list of the network addresses
of all of those people." nil nil)

(autoload 'bbdb-dial "bbdb-com" "\
On a Sun SparcStation, play the appropriate tones on the builtin 
speaker to dial the phone number corresponding to the current line.
If the point is at the beginning of a record, dial the first phone
number.  Does not dial the extension.  Does not dial the area code if
it is the same as `bbdb-default-area-code' unless a prefix arg is given." t nil)

(autoload 'bbdb-finger "bbdb-com" "\
Finger the network address of a BBDB record. 
If this command is executed from the *BBDB* buffer, finger the network
address of the record at point; otherwise, it prompts for a user.
With a numeric prefix argument, finger the Nth network address of the 
current record; with a prefix argument of ^U, finger all of them.
The *finger* buffer is filled asynchronously, meaning that you don't
have to wait around for it to finish; but fingering another user before
the first finger has finished could have unpredictable results.
\\<bbdb-mode-map>
If this command is executed from the *BBDB* buffer, it may be prefixed
with \"\\[bbdb-apply-next-command-to-all-records]\" (as in \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-finger]\" instead of simply \"\\[bbdb-finger]\"), meaning to finger all of 
the users currently listed in the *BBDB* buffer instead of just the one
at point.  The numeric prefix argument has the same interpretation.

You can define a special network address to \"finger\" by defining a
field `finger-host' (default value of `bbdb-finger-host-field')." t nil)

(autoload 'bbdb-timestamp-older "bbdb-com" "\
*Display records with timestamp older than DATE.  DATE must be in
yyyy-mm-dd format." t nil)

(autoload 'bbdb-timestamp-newer "bbdb-com" "\
*Display records with timestamp newer than DATE.  DATE must be in
yyyy-mm-dd format." t nil)

(autoload 'bbdb-creation-older "bbdb-com" "\
*Display records with creation-date older than DATE.  DATE must be
in yyyy-mm-dd format." t nil)

(autoload 'bbdb-creation-newer "bbdb-com" "\
*Display records with creation-date newer than DATE.  DATE must be
in yyyy-mm-dd format." t nil)

(autoload 'bbdb-creation-no-change "bbdb-com" "\
*Display records that have the same timestamp and creation-date." t nil)

(autoload 'bbdb-info "bbdb-com" nil t nil)

(autoload 'bbdb-help "bbdb-com" nil t nil)

;;;***

;;;### (autoloads (bbdb-create-ftp-site bbdb-ftp) "bbdb-ftp" "lisp/bbdb-ftp.el")

(autoload 'bbdb-ftp "bbdb-ftp" "\
Use ange-ftp to open an ftp-connection to a BBDB record's name.
If this command is executed from the *BBDB* buffer, ftp the site of
the record at point; otherwise, it prompts for an ftp-site.
\\<bbdb-mode-map>" t nil)

(autoload 'bbdb-create-ftp-site "bbdb-ftp" "\
Add a new ftp-site entry to the bbdb database; prompts for all relevant info
using the echo area, inserts the new record in the db, sorted alphabetically." t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-message bbdb-insinuate-gnus bbdb/gnus-score bbdb/gnus-snarf-signature bbdb/gnus-lines-and-from bbdb/gnus-show-sender bbdb/gnus-annotate-sender bbdb/gnus-update-record) "bbdb-gnus" "lisp/bbdb-gnus.el")

(autoload 'bbdb/gnus-update-record "bbdb-gnus" "\
returns the record corresponding to the current GNUS message, creating 
or modifying it as necessary.  A record will be created if 
bbdb/news-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation." nil nil)

(autoload 'bbdb/gnus-annotate-sender "bbdb-gnus" "\
Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)." t nil)

(autoload 'bbdb/gnus-show-sender "bbdb-gnus" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings." t nil)

(autoload 'bbdb/gnus-lines-and-from "bbdb-gnus" "\
Useful as the value of gnus-optional-headers in *GNUS* (not Gnus).
NOTE: This variable no longer seems to be present in Gnus.  It seems
to have been replaced by `message-default-headers', which only takes
strings.  In the future this should change." nil nil)

(autoload 'bbdb/gnus-snarf-signature "bbdb-gnus" "\
Snarf signature from the corresponding *Article* buffer." t nil)

(autoload 'bbdb/gnus-score "bbdb-gnus" "\
This returns a score alist for GNUS.  A score pair will be made for
every member of the net field in records which also have a gnus-score
field.  This allows the BBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile." nil nil)

(autoload 'bbdb-insinuate-gnus "bbdb-gnus" "\
Call this function to hook BBDB into GNUS." nil nil)

(autoload 'bbdb-insinuate-message "bbdb-gnus" "\
Call this function to hook BBDB into message-mode." nil nil)

;;;***

;;;### (autoloads (sample-bbdb-canonicalize-net-hook bbdb-auto-notes-hook bbdb-ignore-some-messages-hook bbdb-ignore-most-messages-hook bbdb-extract-field-value bbdb-creation-date-hook bbdb-timestamp-hook) "bbdb-hooks" "lisp/bbdb-hooks.el")

(autoload 'bbdb-timestamp-hook "bbdb-hooks" "\
For use as a bbdb-change-hook; maintains a notes-field called `timestamp'
for the given record which contains the time when it was last modified.  If
there is such a field there already, it is changed, otherwise it is added." nil nil)

(autoload 'bbdb-creation-date-hook "bbdb-hooks" "\
For use as a bbdb-create-hook; adds a notes-field called `creation-date'
which is the current time string." nil nil)

(autoload 'bbdb-extract-field-value "bbdb-hooks" "\
Given the name of a field (like \"Subject\") this returns the value of
that field in the current message, or nil.  This works whether you're in
GNUS, Rmail, or VM.  This works on multi-line fields, but if more than
one field of the same name is present, only the last is returned.  It is
expected that the current buffer has a message in it, and (point) is at the
beginning of the message headers." nil nil)

(autoload 'bbdb-ignore-most-messages-hook "bbdb-hooks" "\
For use as the value of bbdb/news-auto-create-p or bbdb/mail-auto-create-p.
This will automatically create BBDB entries for messages which match
the bbdb-ignore-some-messages-alist (which see) and *no* others." nil nil)

(autoload 'bbdb-ignore-some-messages-hook "bbdb-hooks" "\
For use as a bbdb/news-auto-create-hook or bbdb/mail-auto-create-hook.
This will automatically create BBDB entries for messages which do *not*
match the bbdb-ignore-some-messages-alist (which see)." nil nil)

(autoload 'bbdb-auto-notes-hook "bbdb-hooks" "\
For use as a bbdb-notice-hook.  This might automatically add some text
to the notes field of the BBDB record corresponding to the current record
based on the header of the current message.  See the documentation for
the variables bbdb-auto-notes-alist and bbdb-auto-notes-ignore." nil nil)

(autoload 'sample-bbdb-canonicalize-net-hook "bbdb-hooks" nil nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-mh bbdb/mh-show-sender bbdb/mh-annotate-sender bbdb/mh-update-record) "bbdb-mhe" "lisp/bbdb-mhe.el")

(autoload 'bbdb/mh-update-record "bbdb-mhe" "\
Returns the record corresponding to the current MH message, creating or
modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation." nil nil)

(autoload 'bbdb/mh-annotate-sender "bbdb-mhe" "\
Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)." t nil)

(autoload 'bbdb/mh-show-sender "bbdb-mhe" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings." t nil)

(autoload 'bbdb-insinuate-mh "bbdb-mhe" "\
Call this function to hook BBDB into MH-E." nil nil)

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

;;;### (autoloads (bbdb-insinuate-reportmail) "bbdb-reportmail" "lisp/bbdb-reportmail.el")

(autoload 'bbdb-insinuate-reportmail "bbdb-reportmail" "\
Call this function to hook BBDB into reportmail." nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-rmail bbdb/rmail-show-sender bbdb/rmail-annotate-sender bbdb/rmail-update-record) "bbdb-rmail" "lisp/bbdb-rmail.el")

(autoload 'bbdb/rmail-update-record "bbdb-rmail" "\
returns the record corresponding to the current RMAIL message, creating or
modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation." nil nil)

(autoload 'bbdb/rmail-annotate-sender "bbdb-rmail" "\
Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)." t nil)

(autoload 'bbdb/rmail-show-sender "bbdb-rmail" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings." t nil)

(autoload 'bbdb-insinuate-rmail "bbdb-rmail" "\
Call this function to hook BBDB into RMAIL." nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-sc) "bbdb-sc" "lisp/bbdb-sc.el")

(autoload 'bbdb-insinuate-sc "bbdb-sc" "\
Call this function to hook BBDB into Supercite." nil nil)

;;;***

;;;### (autoloads (bbdb-snarf-region bbdb-snarf) "bbdb-snarf" "lisp/bbdb-snarf.el")

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

(autoload 'bbdb-snarf-region "bbdb-snarf" "\
snarf up a bbdb record in the current region.  See `bbdb-snarf' for
more details." t nil)

;;;***

;;;### (autoloads (bbdb/srv-auto-create-mail-news-dispatcher) "bbdb-srv" "lisp/bbdb-srv.el")

(fset 'bbdb-srv 'bbdb/srv-handle-headers-with-delay)

(autoload 'bbdb/srv-auto-create-mail-news-dispatcher "bbdb-srv" "\
For use as the value of bbdb/srv-auto-create-p.
This will try to decide if this is a mail message or a news message, and then
run either bbdb/news-auto-create-p or bbdb/mail-auto-create-p as appropriate.
\(The heuristic is that news messages never have a Status or X-Mozilla-Status
header; and that mail messages never have Path headers.)" nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-vm bbdb/vm-show-sender bbdb/vm-annotate-sender bbdb/vm-update-record) "bbdb-vm" "lisp/bbdb-vm.el")

(autoload 'bbdb/vm-update-record "bbdb-vm" "\
Returns the record corresponding to the current VM message, 
creating or modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation." nil nil)

(autoload 'bbdb/vm-annotate-sender "bbdb-vm" "\
Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)." t nil)

(autoload 'bbdb/vm-show-sender "bbdb-vm" "\
Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings." t nil)

(autoload 'bbdb-insinuate-vm "bbdb-vm" "\
Call this function to hook BBDB into VM." nil nil)

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

;;;### (autoloads (bbdb-whois) "bbdb-whois" "lisp/bbdb-whois.el")

(autoload 'bbdb-whois "bbdb-whois" nil t nil)

;;;***

;;;### (autoloads (bbdb-xemacs-display-completion-list bbdb-menu bbdb-fontify-buffer) "bbdb-xemacs" "lisp/bbdb-xemacs.el")

(autoload 'bbdb-fontify-buffer "bbdb-xemacs" nil nil nil)

(autoload 'bbdb-menu "bbdb-xemacs" nil t nil)

(autoload 'bbdb-xemacs-display-completion-list "bbdb-xemacs" "\
Wrapper for display-completion-list.  Allows callbacks on XEmacs
display-completion-list is called with `:activate-callback CALLBACK'
if CALLBACK is non-nil. `:user-data DATA' is also used if DATA is
non-nil.  Neither are used if CALLBACK is nil." nil nil)

;;;***

;;;### (autoloads (bbdb-initialize) "bbdb" "lisp/bbdb.el")

(autoload 'bbdb-initialize "bbdb" "\
*Initialize the BBDB.  One or more of the following symbols can be
passed as arguments to initiate the appropriate insinuations.

 Initialization of mail/news readers:

   Gnus       Initialize BBDB support for the Gnus version 3.14 or
              older.
   gnus       Initialize BBDB support for the Gnus mail/news reader
              version 3.15 or newer.  If you pass the `gnus' symbol,
              you should probably also pass the `message' symbol.
   mh-e       Initialize BBDB support for the MH-E mail reader.
   rmail      Initialize BBDB support for the RMAIL mail reader.
   sendmail   Initialize BBDB support for sendmail (M-x mail).
   vm         Initialize BBDB support for the VM mail reader.
              NOTE: For the VM insinuation to work properly, you must
              either call `bbdb-initialize' with the `vm' symbol from
              within your VM initialization file (\"~/.vm\") or you
              must call `bbdb-insinuate-vm' manually from within your
              VM initialization file.

 Initialization of miscellaneous package:

   message    Initialize BBDB support for Message mode.
   reportmail Initialize BBDB support for the Reportmail mail
              notification package.
   sc         Initialize BBDB support for the Supercite message
              citation package.
   w3         Initialize BBDB support for Web browsers." nil nil)

;;;***


(provide 'bbdb-autoloads)
))
