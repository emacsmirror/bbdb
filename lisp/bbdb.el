;;; bbdb.el --- core of BBDB

;; Copyright (C) 1991, 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.
;; Copyright (C) 2010, 2011 Roland Winkler <winkler@gnu.org>

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This file is the core of the Insidious Big Brother Database (aka BBDB),
;;; See bbdb.texinfo for documentation.
;;;
;;;  ------------------------------------------------------------------------
;;; |  There is a mailing list for discussion of BBDB:                       |
;;; |         bbdb-info@lists.sourceforge.net                                |
;;; |  To join, send mail to bbdb-info-request@lists.sourceforge.net         |
;;; |  (do not forget the "-request" part or you'll look silly in front of   |
;;; |  lots of people who have the ability to remember it indefinitely...)   |
;;; |                                                                        |
;;; |  There is also a second mailing list, to which only bug fixes and      |
;;; |  new version announcements are sent; to be added to it, send mail to   |
;;; |  bbdb-announce-request@lists.sourceforge.net.  This is a very low      |
;;; |  volume list, and if you're using BBDB, you really should be on it.    |
;;; |                                                                        |
;;; |  When joining these lists or reporting bugs, please mention which      |
;;; |  version you have. The preferred method of reporting bugs is to use    |
;;; |  bbdb-submit-bug-report, which will include all useful version         |
;;; |  information plus state information about how you have BBDB set up.    |
;;;  ------------------------------------------------------------------------

(require 'timezone)

;; When running BBDB, we have (require 'bbdb-autoloads)
(eval-when-compile              ; pacify the compiler.
  (autoload 'widget-group-match "wid-edit")
  (autoload 'Electric-pop-up-window "electric")
  (autoload 'Electric-command-loop "electric")
  (autoload 'bbdb-migrate "bbdb-migrate")
  (autoload 'bbdb-do-records "bbdb-com")
  (autoload 'bbdb-create-internal "bbdb-com")
  (autoload 'bbdb-append-display-p "bbdb-com")
  (autoload 'bbdb-toggle-records-layout "bbdb-com")
  (autoload 'bbdb-dwim-mail "bbdb-com")
  (autoload 'bbdb-layout-prefix "bbdb-com")
  (autoload 'bbdb-completing-read-records "bbdb-com")
  (autoload 'mail-position-on-field "sendmail")
  (autoload 'vm-select-folder-buffer "vm-folder")

  ;; cannot use autoload for variables...
  (defvar message-mode-map) ;; message.el
  (defvar mail-mode-map) ;; sendmail.el
  (defvar gnus-article-buffer)) ;; gnus-art.el

(defconst bbdb-version "3.02")
(defconst bbdb-version-date "$Date: 2011/04/16 22:07:00 $")

;; Custom groups

(defgroup bbdb nil
  "The Insidious Big Brother Database."
  :group 'news
  :group 'mail)

(put 'bbdb 'custom-loads '("bbdb-mua" "bbdb-com"))

(defgroup bbdb-record-display nil
  "Variables that affect the display of BBDB records"
  :group 'bbdb)

(defgroup bbdb-record-edit nil
  "Variables that affect the editing of BBDB records"
  :group 'bbdb)

(defgroup bbdb-sendmail nil
  "Variables that affect sending mail."
  :group 'bbdb)

(defgroup bbdb-mua nil
  "Variables that specify the BBDB-MUA interface"
  :group 'bbdb)

(defgroup bbdb-mua-gnus nil
  "Gnus-specific BBDB customizations"
  :group 'bbdb-mua)
(put 'bbdb-mua-gnus 'custom-loads '("bbdb-gnus"))

(defgroup bbdb-mua-gnus-scoring nil
  "Gnus-specific scoring BBDB customizations"
  :group 'bbdb-mua-gnus)
(put 'bbdb-mua-gnus-scoring 'custom-loads '("bbdb-gnus"))

(defgroup bbdb-mua-gnus-splitting nil
  "Gnus-specific splitting BBDB customizations"
  :group 'bbdb-mua-gnus)
(put 'bbdb-mua-gnus-splitting 'custom-loads '("bbdb-gnus"))

(defgroup bbdb-mua-vm nil
  "VM-specific BBDB customizations"
  :group 'bbdb-mua)
(put 'bbdb-mua-vm 'custom-loads '("bbdb-vm"))

(defgroup bbdb-mua-message nil
  "Message-specific BBDB customizations"
  :group 'bbdb-mua)
(put 'bbdb-mua-message 'custom-loads '("bbdb-message"))

(defgroup bbdb-dialing nil
  "BBDB Customizations for phone number dialing"
  :group 'bbdb)
(put 'bbdb-dialing 'custom-loads '("bbdb-com"))

(defgroup bbdb-print nil
  "Customizations for printing the BBDB."
  :group 'bbdb)
(put 'bbdb-print 'custom-loads '("bbdb-print"))

(defgroup bbdb-utilities nil
  "Customizations for BBDB Utilities"
  :group 'bbdb)

(defgroup bbdb-utilities-anniv nil
  "Customizations for BBDB Anniversaries"
  :group 'bbdb-utilities)
(put 'bbdb-anniv 'custom-loads '("bbdb-anniv"))

;;; Customizable variables
(defcustom bbdb-file "~/.bbdb"
  "The name of the Insidious Big Brother Database file."
  :group 'bbdb
  :type 'file)

;; This should be removed, and the following put in place:
;; a hierarchical structure of bbdb files, some perhaps read-only,
;; perhaps caching in the local bbdb. This way one could have, e.g. an
;; organization address book, with each person having access to it, and
;; then a local address book with personal stuff in it.
(defcustom bbdb-file-remote nil
  "The remote file to save the BBDB database to.
When this is non-nil, it should be a file name.
When BBDB reads `bbdb-file', it also checks this file,
and if it is newer than `bbdb-file', it loads this file.
When BBDB writes `bbdb-file', it also writes this file.

This feature allows one to keep the database in one place while using
different computers, thus reducing the need for merging different files."
  :group 'bbdb
  :type '(choice (const :tag "none" nil)
                 (file :tag "remote file name")))

(defcustom bbdb-file-remote-save-always t
  "If t `bbdb-file-remote' is saved automatically when `bbdb-file' is saved.
When nil, ask."
  :group 'bbdb
  :type 'boolean)

(defcustom bbdb-read-only nil
  "If t then BBDB will not modify `bbdb-file'
If you have more than one emacs running at the same time, you might want
to set this to t in all but one of them."
  :group 'bbdb
  :type '(choice (const :tag "Database is read-only" t)
                 (const :tag "Database is writable" nil)))

(defcustom bbdb-auto-revert nil
  "If t revert unchanged database without prompting.
If t and `bbdb-file' has changed on disk, while you have not modified
the database in memory, the database will be automatically reverted
without prompting you first.  Otherwise you will be asked.
But if `bbdb-file' has changed while you have made changes in memory as well,
you will always be asked."
  :group 'bbdb
  :type '(choice (const :tag "Revert unchanged database without prompting" t)
                 (const :tag "Ask before reverting database")))

(defcustom bbdb-check-auto-save-file nil
  "If t BBDB will check its auto-save file.
If this file is newer than `bbdb-file', BBDB will offer to revert."
  :group 'bbdb
  :type '(choice (const :tag "Check auto-save file" t)
                 (const :tag "Do not check auto-save file" nil)))

(defcustom bbdb-before-save-hook nil
  "Hook run before saving `bbdb-file'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-after-save-hook nil
  "Hook run before saving `bbdb-file'."
  :group 'bbdb
  :type 'hook)

;; The default element of `bbdb-create-hook' is `bbdb-creation-date'.
;; The default element of `bbdb-change-hook' is `bbdb-timestamp'.
;; -- We have a problem here:
;; Usually, hook variables are not set, but functions are added
;; by calling `add-hook' and removed by calling `remove-hook'.
;; If the user calls `add-hook' in the init file (usually before loading
;; the package that defines the hook variable), this also initializes
;; the respective hook variable.  Yet this collides with a non-nil
;; default value of a hook variable because a non-nil INITVALUE for `defvar'
;; or `defcustom' has no effect if a variable is already bound.
;; Therefore, the following code calls `add-hook' unconditionally.
;; Yet this is not a perfect solution either for two reasons.
;; First, `defcustom' gets confused if the default is set outside `defcustom'.
;; So we use `defvar', though these are really user variables.
;; Second, if the user wants to remove the default functions from the
;; respective hooks, the current solution requires that he or she calls
;; `remove-hook' *after* loading bbdb.el.  However, it is more likely that
;; users call `add-hook' on these variables in their init files than calling
;; `remove-hook'.  In that sense, the following should work for most people.
;; Nonetheless, suggestions for better solutions are appreciated!
(defvar bbdb-create-hook nil
  "*Hook run each time a new BBDB record is created.
Run with one argument, the new record.  This is called before the record is
added to the database, followed by a call of `bbdb-change-hook'.

If a record has been created by analyzing a mail message, hook functions
can use the variable `bbdb-update-records-address' to determine the header
and class of the mail address according to `bbdb-message-headers'.")
(add-hook 'bbdb-create-hook 'bbdb-creation-date)

(defvar bbdb-change-hook nil
  "*Hook run each time a BBDB record is changed.
Run with one argument, the record.  This is called before the database
is modified.  If a new bbdb record is created, `bbdb-create-hook' is called
first, followed by a call of this hook.")
(add-hook 'bbdb-change-hook 'bbdb-timestamp)

(defcustom bbdb-time-stamp-format "%Y-%m-%d %T %z"
  "The BBDB time stamp format.  See `format-time-string'.
This function is called with arg UNIVERSAL being non-nil."
  :group 'bbdb
  :type 'string)

(defcustom bbdb-after-change-hook nil
  "Hook run each time a BBDB record is changed.
Run with one argument, the record.  This is called after the database
is modified.  So if you want to modify a record when it is created or changed,
use instead `bbdb-create-hook' and / or `bbdb-change-hook'."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-after-read-db-hook nil
  "Hook run (with no arguments) after `bbdb-file' is read.
Note that this can be called more than once if the BBDB is reverted."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-initialize-hook nil
  "Hook run (with no arguments) when the BBDB initialization function
`bbdb-initialize' is run."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-mode-hook nil
  "Hook run when the *BBDB* buffer is created."
  :group 'bbdb
  :type 'hook)

(defcustom bbdb-silent nil
  "If t, BBDB suppresses all its informational messages and queries.
Be very very certain you want to set this to t, because it will suppress
prompting to alter record names, assign names to addresses, etc.
Lisp Hackers: See also `bbdb-silent-internal'."
  :group 'bbdb
  :type '(choice (const :tag "Run silently" t)
                 (const :tag "Disable silent running" nil)))

(defcustom bbdb-info-file nil
  "Location of the bbdb info file, if it's not in the standard place."
  :group 'bbdb
  :type '(choice (const :tag "Standard location" nil)
                 (file :tag "Nonstandard location")))


;;; Record display
(defcustom bbdb-layout-alist
  '((one-line           (order     . (phone mail-alias mail notes))
                        (name-end  . 24)
                        (toggle    . t))
    (multi-line         (omit      . (creation-date timestamp))
                        (toggle    . t)
                        (indentation . 21))
    (pop-up-multi-line  (omit      . (creation-date timestamp))
                        (indentation . 21))
    (full-multi-line    (indentation . 21)))
  "Alist describing each display layout.
The format of an element is (LAYOUT-NAME OPTION-ALIST).

By default there are four different layout types used by BBDB, which are
`one-line', `multi-line', `pop-up-multi-line' (used for pop-ups) and
`full-multi-line' (showing all fields of a record).

OPTION-ALIST specifies the options for the layout.  Valid options are:

                           ------- Availability --------
    Format                  one-line        multi-line        default if unset
------------------------------------------------------------------------------
 (toggle . BOOL)                 +               +              nil
 (order . FIELD-LIST)            +               +              '(phone ...)
 (omit . FIELD-LIST)             +               +              nil
 (name-end . INTEGER)            +               -              40
 (indentation . INTEGER)         -               +              21
 (primary . BOOL)                -               +              nil
 (display-p . FUNCTION)          +               +              nil

- toggle: controls if this layout is included when toggeling the layout
- order: defines a user specific order for the fields, where `t' is a place
  holder for all remaining fields
- omit: is a list of notes fields which should not be displayed
  or `t' to exclude all fields except those listed in the order option
- name-end: sets the column where the name should end in one-line layout.
- indentation: sets the level of indentation for multi-line display.
- primary: controls wether only the primary mail is shown or all are shown.
- display-p: a function controlling wether the record is to be displayed.

When you add a new layout FOO, you can write a corresponding layout
function `bbdb-display-record-layout-FOO'.  If you do not write your own
layout function, the multi-line layout will be used."
  :group 'bbdb-record-display
  :type
  `(repeat
    (cons :tag "Layout Definition"
          (choice :tag "Layout type"
                  (const one-line)
                  (const multi-line)
                  (const pop-up-multi-line)
                  (const full-multi-line)
                  (symbol))
          (set :tag "Properties"
               (cons :tag "Order"
                     (const :tag "List of fields to order by" order)
                     (repeat (choice (const phone)
                                     (const address)
                                     (const mail)
                                     (const AKA)
                                     (const notes)
                                     (symbol :tag "other")
                                     (const :tag "Remaining fields" t))))
               (choice :tag "Omit"
                       :value (omit . nil)
                       (cons :tag "List of fields to omit"
                             (const :tag "Fields not to display" omit)
                             (repeat (choice (const phone)
                                             (const address)
                                             (const mail)
                                             (const AKA)
                                             (const notes)
                                             (symbol :tag "other"))))
                       (const :tag "Exclude all fields except those listed in the order note" t))
               (cons :tag "Indentation"
                     :value (indentation . 14)
                     (const :tag "Level of indentation for multi-line layout"
                            indentation)
                     (number :tag "Column"))
               (cons :tag "End of name field"
                     :value (name-end . 24)
                     (const :tag "The column where the name should end in one-line layout"
                            name-end)
                     (number :tag "Column"))
               (cons :tag "Toggle"
                     (const :tag "The layout is included when toggling layout" toggle)
                     boolean)
               (cons :tag "Primary Mail Only"
                     (const :tag "Only the primary mail address is included" primary)
                     boolean)
               (cons :tag "Display-p"
                     (const :tag "Show only records passing this test" display-p)
                     (choice (const :tag "No test" nil)
                             (function :tag "Predicate")))))))

(defcustom bbdb-layout 'multi-line
  "Default display layout."
  :group 'bbdb-record-display
  :type '(choice (const one-line)
                 (const multi-line)
                 (const full-multi-line)
                 (symbol)))

(defcustom bbdb-pop-up-layout 'pop-up-multi-line
  "Default layout for pop-up BBDB buffers (mail, news, etc.)."
  :group 'bbdb-record-display
  :type '(choice (const one-line)
                 (const multi-line)
                 (const full-multi-line)
                 (symbol)))

(defcustom bbdb-wrap-column nil
  "Wrap column for multi-line display.  If nil do not wrap lines."
  :group 'bbdb-record-display
  :type '(choice (const :tag "No line wrapping" nil)
                 (number :tag "Wrap column")))

(defcustom bbdb-case-fold-search (default-value 'case-fold-search)
  "Value of `case-fold-search' used by BBDB and friends.
This variable lets the case-sensitivity of the BBDB commands
be different from standard commands like `isearch-forward'."
  :group 'bbdb-record-display
  :type 'boolean)

;; See http://en.wikipedia.org/wiki/Postal_address
;; http://www.upu.int/en/activities/addressing/postal-addressing-systems-in-member-countries.html
(defcustom bbdb-address-format-list
  '((("Argentina") "spcSC" "@%s\n@%p, @%c@, %S@\n%C@" "@%c@")
    (("Australia") "scSpC" "@%s\n@%c@ %S@ %p@\n%C@" "@%c@")
    (("Austria" "Germany" "Spain" "Switzerland")
     "spcSC" "@%s\n@%p @%c@ (%S)@\n%C@" "@%c@")
    (("Canada") "scSCp" "@%s\n@%c@, %S@\n%C@ %p@" "@%c@")
    (("China") "scpSC" "@%s\n@%c@\n%p@%S@\n%C@" "@%c@") ; English format
    ; (("China") "CpScs" "@%C @%p\n@%S @%c@ %s@" "@%c@") ; Chinese format
    (("India") "scpSC" "@%s\n@%c@ %p@ (%S)@\n%C@" "@%c@")
    (("USA") "scSpC" "@%s\n@%c@, %S@ %p@\n%C@" "@%c@")
    (t bbdb-edit-address-default bbdb-format-address-default "@%c@"))
  "List of address editing and formatting rules for BBDB.
Each rule is a list (IDENTIFIER EDIT FORMAT FORMAT).
The first rule for which IDENTIFIER matches an address is used for editing
and formatting the address.

IDENTIFIER may be a list of countries.
IDENTIFIER may also be a function that is called with one arg, the address
to be used.  The rule is used if the function returns non-nil.
See `bbdb-address-continental-p' for an example.
If IDENTIFIER is t, EDIT and FORMAT is used for all entries.
Usually, this should be the last rule that becomes a fall-back (default).

EDIT may be a function that is called with one argument, the address.
See `bbdb-edit-address-default' for an example.

EDIT may also be an editting format string.  It is a string containing
the five letters s, c, p, S, and C that specify the order for editing
the five elements of an address:

s  streets
c  city
p  postcode
S  state
C  country

The first FORMAT of each rule is used for multi-line layout, the second FORMAT
is used for one-line layout.

FORMAT may be a function that is called with one argument, the address.
See `bbdb-format-address-default' for an example.

FORMAT may also be a format string.  It consists of formatting elements
separated by a delimiter defined via the first (and last) character of FORMAT.
Each formatting element may contain one of the following format specifiers:

%s  streets (used repeatedly for each street part)
%c  city
%p  postcode
%S  state
%C  country

A formatting element will be applied only if the corresponding part
of the address is a non-empty string.

See also `bbdb-print-address-format-list'."
  :group 'bbdb-record-display
  :type '(repeat (list (choice (const :tag "Default" t)
                               (function :tag "Function")
                               (repeat (string)))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function")))))

(defcustom bbdb-continental-postcode-regexp
  "^\\s *[A-Z][A-Z]?\\s *-\\s *[0-9][0-9][0-9]"
  "Regexp matching continental postcodes.
Used by address format identifier `bbdb-address-continental-p'.
The regexp should match postcodes of the form CH-8052, NL-2300RA,
and SE-132 54."
  :group 'bbdb-record-display
  :type 'regexp)

(defcustom bbdb-default-separator '("[,;]" ", ")
  "The default field separator.  It is a list (SPLIT-RE JOIN).
This is used for fields which do not have an entry in `bbdb-separator-alist'."
  :group 'bbdb-record-display
  :type '(list regexp string))

(defcustom bbdb-separator-alist
  '((organization "[,;]" ", ") (degree "[,;]"  ", ") (aka ";" "; ")
    (mail "[,;]" ", ") (mail-alias "[,;]" ", ") (vm-folder "[,;]" ", ")
    (birthday "\n" "\n") (wedding "\n" "\n") (anniversary "\n" "\n")
    (notes "\n" "\n"))
  "Alist of field separators.
Each element is of the form (FIELD SPLIT-RE JOIN).
For fields lacking an entry here `bbdb-default-separator' is used instead."
  :group 'bbdb-record-display
  :type '(repeat (list symbol regexp string)))

(defcustom bbdb-user-menu-commands nil
  "User defined menu entries which should be appended to the BBDB menu.
This should be a list of menu entries.
When set to a function, it is called with two arguments RECORD and FIELD
and it should either return nil or a list of menu entries.
Used by `bbdb-mouse-menu'."
  :group 'bbdb-record-display
  :type 'sexp)

(defcustom bbdb-electric nil
  "Whether bbdb mode should be `electric' like `electric-buffer-list'."
  :group 'bbdb-record-display
  :type 'boolean)

(defcustom bbdb-display-hook nil
  "Hook run after the *BBDB* is filled in."
  :group 'bbdb-record-display
  :type 'hook)

(defcustom bbdb-multiple-buffers nil
  "When non-nil we create a new buffer of every buffer causing pop-ups.
You can also set this to a function returning a buffer name.
Here a value may be the predefined function `bbdb-multiple-buffers-default'."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Enabled" bbdb-multiple-buffers-default)
                 (function :tag "User defined function")))


;;; Record editing
(defcustom bbdb-lastname-prefixes
 '("von" "de" "di")
  "List of lastname prefixes recognized in name fields.
Used to enhance dividing name strings into firstname and lastname parts.
Case is ignored."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-lastname-suffixes
 '("Jr" "Sr" "II" "III")
  "List of lastname suffixes recognized in name fields.
Used to dividing name strings into firstname and lastname parts.
All suffices are complemented by optional `.'.  Case is ignored."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-default-domain nil
  "Default domain to append when prompting for a new mail address.
If a mail address does not contain `[@%!]', append `@bbdb-default-domain' to it.

The address is not altered if `bbdb-default-domain' is nil
or if a prefix argument is given to the command `bbdb-insert-field'."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "none" nil)
                 (string :tag "Default Domain" :value nil)))

(defcustom bbdb-phone-style 'nanp
  "Phone numbering plan assumed by BBDB.
The value 'nanp refers to the North American Numbering Plan.
The value nil refers to a free-style numbering plan.

You can have both styles of phone number in your database by providing a
prefix argument to the command `bbdb-insert-field'."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "NANP" nanp)
                 (const :tag "none" nil)))

(defcustom bbdb-default-area-code nil
  "Default area code to use when prompting for a new phone number.
This variable also affects dialing."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "none" nil)
                 (integer :tag "Default Area Code"))
  :set (lambda( symb val )
         (if (or (and (stringp val)
                      (string-match "^[0-9]+$" val))
                 (integerp val)
                 (null val))
             (set symb val)
           (error "%s must contain digits only." symb))))

(defcustom bbdb-no-duplicates nil
  "When non-nil BBDB allows records with duplicate names.
This may lead to confusion when doing completion.  If non-nil, it will
prompt the users on how to merge records when duplicates are detected."
  :group 'bbdb-record-edit
  :type 'boolean)

(defcustom bbdb-default-label-list '("home" "work" "other")
  "Default list of labels for Address and Phone fields."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-address-label-list bbdb-default-label-list
  "List of labels for Address field."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-phone-label-list '("home" "work" "cell" "other")
  "List of labels for Phone field."
  :group 'bbdb-record-edit
  :type '(repeat string))

(defcustom bbdb-default-country "Emacs";; what do you mean, it's not a country?
  "Default country to use if none is specified."
  :group 'bbdb-record-edit
  :type '(choice (const :tag "None" nil)
                 (string :tag "Default Country")))

(defcustom bbdb-check-postcode t
  "If non-nil, require legal postcodes when entering an address.
The format of legal postcodes is determined by the variable
`bbdb-legal-postcodes'."
  :group 'bbdb-record-edit
  :type 'boolean)

(defcustom bbdb-legal-postcodes
  '(;; empty string
    "^$"
    ;; Matches 1 to 6 digits.
    "^[ \t\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ \t\n]*$"
    ;; Matches 5 digits and 3 or 4 digits.
    "^[ \t\n]*\\([0-9][0-9][0-9][0-9][0-9]\\)[ \t\n]*-?[ \t\n]*\\([0-9][0-9][0-9][0-9]?\\)[ \t\n]*$"
    ;; Match postcodes for Canada, UK, etc. (result is ("LL47" "U4B")).
    "^[ \t\n]*\\([A-Za-z0-9]+\\)[ \t\n]+\\([A-Za-z0-9]+\\)[ \t\n]*$"
    ;; Match postcodes for continental Europe.  Examples "CH-8057"
    ;; or "F - 83320" (result is ("CH" "8057") or ("F" "83320")).
    ;; Support for "NL-2300RA" added at request from Carsten Dominik
    ;; <dominik@astro.uva.nl>
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+ ?[A-Z]*\\)[ \t\n]*$"
    ;; Match postcodes from Sweden where the five digits are grouped 3+2
    ;; at the request from Mats Lofdahl <MLofdahl@solar.stanford.edu>.
    ;; (result is ("SE" (133 36)))
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+\\)[ \t\n]+\\([0-9]+\\)[ \t\n]*$")
  "List of regexps that match legal postcodes.
Whether this is used at all depends on the variable `bbdb-check-postcode'."
  :group 'bbdb-record-edit
  :type '(repeat regexp))


;;; MUA interface
(defcustom bbdb-update-records-p 'query
  "Return value for `bbdb-select-message' and friends.
This will be passed to `bbdb-update-records', where it controls the handling
of the arg ADDRESS (see there).  Allowed values are:
 nil          Do nothing.
 search       Search for existing records matching ADDRESS.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values."
  ;; Also: Used for communication between `bbdb-update-records'
  ;; and `bbdb-prompt-for-create'.
  :group 'bbdb-mua
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" search)
                 (const :tag "query annotation of all messages" query)
                 (const :tag "annotate all messages" create)
                 (function :tag "User-defined function")))

(defcustom bbdb-mua-update-interactive-p '(search . query)
  "Return values for function `bbdb-mua-update-interactive-p' (see there).
It is a cons pair (WITHOUT-PREFIX . WITH-PREFIX).
The car is used if the command is called without a prefix.
The cdr is used if the command is called with a prefix.
Each element may take the same values as `bbdb-update-records-p'.
If the value is `read then read the value interactively."
  :group 'bbdb-mua
  :type '(cons (choice (const :tag "do nothing" nil)
                       (const :tag "search for existing records" search)
                       (const :tag "query annotation of all messages" query)
                       (const :tag "annotate all messages" create)
                       (function :tag "User-defined function")
                       (const :tag "read arg interactively" read))
               (choice (const :tag "do nothing" nil)
                       (const :tag "search for existing records" search)
                       (const :tag "query annotation of all messages" query)
                       (const :tag "annotate all messages" create)
                       (function :tag "User-defined function")
                       (const :tag "read arg interactively" read))))

(defcustom bbdb-message-headers
  '((sender     "From" "Resent-From" "Reply-To" "Sender")
    (recipients "Resent-To" "Resent-CC" "To" "CC" "BCC"))
  "Alist of headers to search for sender and recipients mail addresses.
Each element is of the form

  (CLASS HEADER ...)

The symbol CLASS defines a class of headers.
The strings HEADER belong to CLASS."
  :group 'bbdb-mua
  :type 'list)

(defcustom bbdb-message-all-addresses nil
  "If t `bbdb-update-records' returns all mail addresses of a message.
Otherwise this function returns only the first mail address of each message.
Changing this variable will show its effect only after clearing the
`bbdb-message-cache' of a folder (e.g., by closing and re-visiting the folder)."
  :group 'bbdb-mua
  :type 'boolean)

(defcustom bbdb-accept-message-alist t
  "Alist describing which messages to automatically create BBDB records for.
The format of this alist is
   ((HEADER-NAME . REGEXP) ...)
For example, if
   ((\"From\" . \"@.*\\.maximegalon\\.edu\")
    (\"Subject\" . \"time travel\"))
BBDB records are only created for messages sent by people at Maximegalon U.,
or people posting about time travel.
A value of t means accept all messages.

See also `bbdb-ignore-message-alist', which has the opposite effect."
  :group 'bbdb-mua
  :type '(repeat (cons
          (string :tag "Header name")
          (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-ignore-message-alist nil
  "Alist describing which messages not to automatically create BBDB records for.
The format of this alist is
   ((HEADER-NAME . REGEXP) ... )
For example, if
   ((\"From\" . \"mailer-daemon\")
    ((\"To\" \"CC\") . \"mailing-list-1\\\\|mailing-list-2\"))
no BBDB records are created for messages from any mailer daemon,
or messages sent to or CCed to either of two mailing lists.
A value of t means ignore all messages.

See also `bbdb-accept-message-alist', which has the opposite effect."
  :group 'bbdb-mua
  :type '(repeat (cons
          (string :tag "Header name")
          (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-accept-name-mismatch nil
  "If non-nil accept name mismatches between messages and BBDB records.
Thus BBDB ignores when the real name in a message differs from the name
in a BBDB record with the same mail address as in \"John Smith <jqs@frob.com>\"
versus \"John Q. Smith <jqs@frob.com>\".
If a number it is the number of seconds BBDB displays the name mismatch.
If nil BBDB asks if you want to change the name.
See also `bbdb-use-alternate-names'."
  :group 'bbdb-mua
  :type '(choice (const :tag "Prompt for name changes" nil)
                 (const :tag "Do not prompt for name changes" t)
         (integer :tag "Instead of prompting, warn for this many seconds")))

(defcustom bbdb-use-alternate-names t
  "If non-nil collect alternate names for a record as AKA.
Thus if BBDB changes the name in a record to the new name found in a message
\(see `bbdb-accept-name-mismatch'), the old name is kept as AKA."
  :group 'bbdb-mua
  :type '(choice (const :tag "Ask to use alternate names field" t)
                 (const :tag "Use alternate names field without asking" nil)))

(defcustom bbdb-user-mail-address-re
  (and (stringp user-mail-address)
       (string-match "\\`\\([^@]*\\)\\(@\\|\\'\\)" user-mail-address)
       (concat "\\<" (regexp-quote (match-string 1 user-mail-address)) "\\>"))
  "A regular expression matching your mail addresses."
  :group 'bbdb-mua
  :type '(regexp :tag "Regexp matching your mail addresses"))

(defcustom bbdb-add-mails 'query
  "How to handle new mail addresses for existing BBDB records.
If t, then when BBDB notices a new mail address for a record,
it will automatically add it to the list of mail addresses.
If it is 'query, query whether to add it.
If it is nil then new mail addresses will never be automatically added
nor the user will be asked.
If set to a function name the function should return one of these values.

See also the variable `bbdb-new-mails-always-primary' for control of whether
the addresses go at the front of the list or the back."
  :group 'bbdb-mua
  :type '(choice (const :tag "Automatically add new mail addresses" t)
                 (const :tag "Ask before adding new mail addresses" query)
                 (const :tag "Never add new mail addresses" nil)))

(defcustom bbdb-new-mails-always-primary nil
  "Controls where to put a new mail addresses in the list of known addresses.
If this is t, then when BBDB adds a new mail address to a record, it will put it
to the front of the list of addresses, making it the primary address.
If it is any other non-nil value, you will be asked.
If nil then the new mail addresses will always be added at the end of the list."
  :group 'bbdb-mua
  :type '(choice (const :tag "New address automatically made primary" t)
                (const :tag "Ask before making new address primary" nil)
                (const :tag "Never make new address primary" never)))

(defcustom bbdb-canonicalize-mail-function nil
  "If non-nil, it should be a function of one arg: a mail address string.
Whenever BBDB \"notices\" a message, the corresponding mail address
will be passed to this function first.  It acts as a kind of \"filter\"
to transform the mail address before it is compared against or added
to the database.
Example: it is the case that CS.CMU.EDU is a valid return address
for all mail originating at a machine in the .CS.CMU.EDU domain.
So, if you wanted all such addresses to be canonically hashed
as user@CS.CMU.EDU, instead of as user@host.CS.CMU.EDU, you might set
this variable to a function like this:

 (setq bbdb-canonicalize-mail-function
       '(lambda (address)
          (cond ((string-match \"\\\\`\\\\([^@]+@\\\\).*\\\\.\\\\(CS\\\\.CMU\\\\.EDU\\\\)\\\\'\"
                               address)
                 (concat (match-string 1 address) (match-string 2 address)))
                (t address))))

See `bbdb-canonicalize-mail-1' for a more complete example.
You could also use this function to rewrite UUCP-style addresses into
domain-style addresses, or any number of things."
  :group 'bbdb-mua
  :type 'function)

(defcustom bbdb-canonicalize-redundant-mails t
  "If non-nil, redundant mail addresses will be ignored.
If a record has an address of the form foo@baz.com, setting this to t
will cause subsequently-noticed addresses like foo@bar.baz.com to be
ignored (since we already have a more general form of that address.)
This is similar in function to one of the possible uses of the variable
`bbdb-canonicalize-mail-function' but is somewhat more automatic.  (This
cannot quite be implemented in terms of the `bbdb-canonicalize-mail-function'
because it needs access to the database to determine whether an address is
redundant, and the `bbdb-canonicalize-mail-function' is purely a textual
manipulation which is performed before any database access.)"
  :group 'bbdb-mua
  :type '(choice (const :tag "Ignore redundant addresses" t)
                 (const :tag "Do not ignore redundant addresses" nil)))

(defcustom bbdb-message-caching t
  "Whether caching of the message->record association should be used
for the interfaces which support it (VM, MH, and RMAIL).  This can speed
things up a lot.  One implication of this variable being t is that the
`bbdb-notice-hook' will not be called each time a message is selected, but
only the first time.  Likewise, if selecting a message would generate a
question (whether to add an address, change the name, etc) you will only
be asked that question the first time the message is selected."
  :group 'bbdb-mua
  :type '(choice (const :tag "Enable caching" t)
                 (const :tag "Disable caching" nil)))

(defcustom bbdb-notice-hook nil
  "Hook run each time a BBDB record is \"noticed\", that is,
each time it is displayed by the MUA interfaces.  Run with
one argument, the new record.  The record need not have been modified for
this to be called - use `bbdb-change-hook' for that.  You can use this to,
for example, add something to the notes field based on the subject of the
current message.  It is up to your hook to determine whether it is running
in Gnus, VM, MH, or RMAIL, and to act appropriately.

Also note that `bbdb-change-hook' will NOT be called as a result of any
modifications you may make to the record inside this hook.

Hook functions can use the variable `bbdb-update-records-address' to determine
the header and class of the mail address according to `bbdb-message-headers'.

Beware that if the variable `bbdb-message-caching' is t (a good idea)
this hook will be called only the first time that message is selected.
Thus when debugging the value of this hook, it can help to set
`bbdb-message-caching' to nil."
  :group 'bbdb-mua
  :type 'hook)

(define-widget 'bbdb-alist-with-header 'group
  "My group"
  :match 'bbdb-alist-with-header-match
  :value-to-internal (lambda (widget value)
                       (if value (list (car value) (cdr value))))
  :value-to-external (lambda (widget value)
                       (if value (append (list (car value)) (cadr value)))))

(defun bbdb-alist-with-header-match (widget value)
  (widget-group-match widget
                      (widget-apply widget :value-to-internal value)))

(defcustom bbdb-auto-notes-rules nil
  "List of rules for adding notes to records of mail addresses of messages.
This automatically annotates the BBDB record of the sender or recipient
of a message based on the value of a header such as the Subject header.
This requires that `bbdb-notice-hook' contains `bbdb-auto-notes' and that
the record already exists or `bbdb/MUA-update-records-p' returns such that
the record will be created.  Messages matching `bbdb-auto-notes-ignore-messages'
are ignored.

The elements of this list are

   (MUA FROM-TO HEADER ANNOTATE ...)
   (FROM-TO HEADER ANNOTATE ...)
   (HEADER ANNOTATE ...)

MUA is the active MUA or a list of MUAs (see `bbdb-mua').
If MUA is missing or t, use this rule for all MUAs.

FROM-TO is a list of headers and/or header classes as in `bbdb-message-headers'.
The record corresponding to a mail address of a message is considered for
annotation if this mail address was found in a header matching FROM-TO.
If FROM-TO is missing or t, records for each mail address are considered
irrespective of where the mail address was found in a message.

HEADER is a message header that is considered for generating the annotation.

ANNOTATE may take the following values:

  (REGEXP . STRING)       [this is equivalent to (REGEXP notes STRING)]
  (REGEXP FIELD STRING)
  (REGEXP FIELD STRING REPLACE)

REGEXP must match the value of HEADER for generating an annotation.
However, if the value of HEADER also matches an element of
`bbdb-auto-notes-ignore-headers' no annotation is generated.

The annotation will be added to FIELD of the respective record.
FIELD defaults to 'notes.

STRING defines a replacement for the match of REGEXP in the value of HEADER.
It may contain \\& or \\N specials used by `replace-match'.
The resulting string becomes the annotation.
If STRING is an integer N, the Nth matching subexpression is used.
If STRING is a function, it will be called with one arg, the value of HEADER.
The return value (which must be a string) is then used.

If REPLACE is t, the resulting string replaces the old contents of FIELD.
If it is nil, the string is appended to the contents of FIELD (unless the
annotation is already part of the content of field).

For example,

   ((\"To\" (\"-vm@\" . \"VM mailing list\"))
    (\"Subject\" (\"sprocket\" . \"mail about sprockets\")
               (\"you bonehead\" . \"called me a bonehead\")))

will cause the text \"VM mailing list\" to be added to the notes field
of the records corresponding to anyone you get mail from via one of the VM
mailing lists.

If multiple clauses match the message, all of the corresponding strings
will be added.

See also variables `bbdb-auto-notes-ignore-messages' and
`bbdb-auto-notes-ignore-headers'.

For speed-up, the function `bbdb-auto-notes' actually use expanded rules
stored in the internal variable `bbdb-auto-notes-rules-expanded'.
If you change the value of `bbdb-auto-notes-rules'
set `bbdb-auto-notes-rules-expanded' to nil, so that the expanded rules
will be re-evaluated."
  :group 'bbdb-mua
  :type '(repeat
          (bbdb-alist-with-header
           (repeat (choice
                    (const sender)
                    (const recipients)))
           (string :tag "Header name")
           (repeat (choice
                    (cons :tag "Value Pair"
                          (regexp :tag "Regexp to match on header value")
                          (string :tag "String for notes if regexp matches"))
                    (list :tag "Replacement list"
                          (regexp :tag "Regexp to match on header value")
                          (choice :tag "Record field"
                                  (const notes :tag "Notes")
                                  (const organization :tag "Organization")
                                  (symbol :tag "Other"))
                          (choice :tag "Regexp match"
                                  (string :tag "Replacement string")
                                  (integer :tag "Subexpression match")
                                  (function :tag "Callback Function"))
                          (choice :tag "Replace previous contents"
                                  (const :tag "No" nil)
                                  (const :tag "Yes" t))))))))

(defcustom bbdb-auto-notes-ignore-messages nil
  "List of rules for ignoring entire messages in `bbdb-auto-notes'.
The elements may have the following values:
  a function  This function is called with one arg, the record
              that would be annotated.
              Ignore this message if the function returns non-nil.
              This function may use `bbdb-update-records-address'.
  MUA         Ignore messages from MUA (see `bbdb-mua').
  (HEADER . REGEXP)  Ignore messages where HEADER matches REGEXP.
              For example,  (\"From\" . bbdb-user-mail-address-re)
              disables any recording of notes for mail addresses
              found in messages coming from yourself, see
              `bbdb-user-mail-address-re'.
  (MUA HEADER REGEXP)  Ignore messages from MUA where HEADER
              matches REGEXP.
See also `bbdb-auto-notes-ignore-headers'."
  :group 'bbdb-mua
  :type '(repeat (cons
          (string :tag "Header name")
          (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-auto-notes-ignore-headers nil
  "Alist of headers and regexps to ignore in `bbdb-auto-notes'.
Each element is of the form

    (HEADER . REGEXP)

For example,

    (\"Organization\" . \"^Gatewayed from\\\\\|^Source only\")

will exclude the phony `Organization:' headers in GNU mailing-lists
gatewayed to gnu.* newsgroups.
See also `bbdb-auto-notes-ignore-messages'."
  :group 'bbdb-mua
  :type '(repeat (cons
          (string :tag "Header name")
          (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb-message-pop-up nil
  "If non-nil, display a continuously updated BBDB window while using a MUA.
If 'horiz, stack the window horizontally if there is room."
  :group 'bbdb-mua
  :type '(choice (const :tag "Automatic BBDB window, stacked vertically" t)
                 (const :tag "Automatic BBDB window, stacked horizontally" 'horiz)
                 (const :tag "No Automatic BBDB window" nil)))

(defcustom bbdb-pop-up-window-size 0.5
  "Vertical size of a MUA pop-up BBDB window (vertical split).
If it is an integer number, it is the number of lines used by BBDB.
If it is a fraction between 0 and 1.0 (inclusive), it is the fraction
of the tallest existing window that BBDB will take over."
  :group 'bbdb-mua
  :type 'number)

(defcustom bbdb-horiz-pop-up-window-size '(112 . 0.3)
  "Horizontal size of a MUA pop-up BBDB window (horizontal split).
It is a cons pair (TOTAL . BBDB-SIZE).
The window that will be considered for horizontal splitting must have
at least TOTAL columns. BBDB-SIZE is the horizontal size of the BBDB window.
If it is an integer number, it is the number of columns used by BBDB.
If it is a fraction between 0 and 1, it is the fraction of the
window width that BBDB will take over."
  :group 'bbdb-mua
  :type '(cons (number) (number)))


;;; Notes processing
(defcustom bbdb-notes-sort-order
  '((notes . 0) (url . 1) (ftp . 2) (gopher . 3) (telnet . 4) (mail-alias . 5)
    (mail-folder . 6) (lpr . 7) (creation-date . 1000) (timestamp . 1001))
  "The order for sorting the notes.
If a note is not in the alist, it is assigned weight 100, so all notes
with weights less then 100 will be in the beginning, and all notes with
weights more than 100 will be in the end."
  :group 'bbdb-mua
  :type 'list)

(defcustom bbdb-merge-notes-function 'bbdb-merge-concat
  "Default function to use for merging BBDB notes records.

If the note field has an entry in `bbdb-merge-notes-function-alist',
that function will be used instead."
  :group 'bbdb-mua
  :type 'function)

(defcustom bbdb-merge-notes-function-alist
  '((creation-date . bbdb-merge-string-least)
    (timestamp . bbdb-merge-string-most))
  "An alist defining specific merging function, based on notes field."
  :group 'bbdb-mua
  :type '(repeat (cons
                  (symbol :tag "Notes filed")
                  (function :tag "Generating function"))))


;;; Sending mail
(defcustom bbdb-mail-user-agent nil
  "Mail user agent used by BBDB.
Allowed values are those allowed for `mail-user-agent'."
  :group 'bbdb-sendmail
  :type '(radio (function-item :tag "Message package"
			       :format "%t\n"
			       message-user-agent)
		(function-item :tag "Mail package"
			       :format "%t\n"
			       sendmail-user-agent)
		(function-item :tag "Emacs interface to MH"
			       :format "%t\n"
			       mh-e-user-agent)
		(function-item :tag "Message with full Gnus features"
			       :format "%t\n"
			       gnus-user-agent)
		(function-item :tag "VM"
			       :format "%t\n"
			       vm-user-agent)
		(function :tag "Other")
                (const :tag "Default" nil)))

(defcustom bbdb-mail-alias-field 'mail-alias
  "Field holding the base alias for a record.
Used by `bbdb-mail-aliases'.  See also `bbdb-mail-alias'."
  :group 'bbdb-sendmail
  :type 'symbol)

(defcustom bbdb-mail-alias 'first
  "Defines which mail aliases are generated for a BBDB record.
first: Generate one alias \"<alias>\" that expands to the first mail address
       of a record.
star:  Generate a second alias \"<alias>*\" that expands to all mail addresses
       of a record.
all:   Generate the aliases \"<alias>\" and \"<alias>*\" (as for 'star)
       and aliases \"<alias>n\" for each mail address, where n is the position
       of the mail address of a record."
  :group 'bbdb-sendmail
  :type '(choice (symbol :tag "Only first" first)
                 (symbol :tag "<alias>* for all mails" star)
                 (symbol :tag "All aliases" all)))

(defcustom bbdb-mail-allow-redundancy nil
  "Mail address to use for BBDB records when sending mail.
If non-nil always use full name in mail address, even if same as mail.
If value is mail-only never use full name."
  :group 'bbdb-sendmail
  :type '(choice (const :tag "Disallow redundancy" nil)
                 (const :tag "Never use full name" 'mail-only)
                 (const :tag "Allow redundancy" t)))

(defcustom bbdb-complete-mail t
  "If t MUA insinuation provides key binding for command `bbdb-complete-mail'."
  :group 'bbdb-sendmail
  :type 'boolean)

(defcustom bbdb-completion-list t
  "Controls the behaviour of `bbdb-complete-mail'.
If a list of symbols, it specifies which fields to complete.  Symbols include
  fl-name (= first and last name)
  lf-name (= last and first name)
  organization
  aka
  mail (= all email addresses of each record)
  primary (= first email address of each record)
If t, completion is done for all of the above.
If nil, no completion is offered."
  ;; These symbols match the fields for which BBDB provides entries in
  ;; `bbdb-hash-table'.
  :group 'bbdb-sendmail
  :type '(choice (const :tag "No Completion" nil)
                 (const :tag "Complete across all fields" t)
                 (repeat :tag "Field"
                         (choice (const fl-name)
                                 (const lf-name)
                                 (const aka)
                                 (const organization)
                                 (const primary)
                                 (const mail)))))

(defcustom bbdb-complete-mail-allow-cycling nil
  "Whether to allow cycling of mail addresses when calling
`bbdb-complete-mail' on a completed address in a composition buffer."
  :group 'bbdb-sendmail
  :type 'boolean)

(defcustom bbdb-complete-mail-hook nil
  "List of functions called after a sucessful completion."
  :group 'bbdb-sendmail
  :type 'hook)

(defcustom bbdb-mail-abbrev-expand-hook nil
  ;; Replacement for function `mail-abbrev-expand-hook'.
  "Function (not hook) run each time an alias is expanded.
The function is called with two args the alias and the list
of corresponding mail addresses."
  :group 'bbdb-sendmail
  :type 'function)

(defcustom bbdb-completion-display-record t
  "Whether `bbdb-complete-mail' displays the BBDB record
whose mail address has just been inserted."
  :group 'bbdb-sendmail
  :type '(choice (const :tag "Update the BBDB buffer" t)
                 (const :tag "Do not update the BBDB buffer" nil)))


;;;Dialing
(defcustom bbdb-dial-local-prefix-alist
  '(((if (integerp bbdb-default-area-code)
         (format "(%03d)" bbdb-default-area-code)
       (or bbdb-default-area-code ""))
     . ""))
  "Mapping to remove local prefixes from numbers.
If this is non-nil, it should be an alist of
\(PREFIX . REPLACEMENT) elements. The first part of a phone number
matching the regexp returned by evaluating PREFIX will be replaced by
the corresponding REPLACEMENT when dialing."
  :group 'bbdb-dialing
  :type 'sexp)

(defcustom bbdb-dial-local-prefix nil
  "Local prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making local calls (for example, if your phone system
requires you to dial 9 before making outside calls.) In BBDB's
opinion, you're dialing a local number if it starts with a 0 after
processing `bbdb-dial-local-prefix-alist'."
  :group 'bbdb-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "9")))

(defcustom bbdb-dial-long-distance-prefix nil
  "Long distance prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making a long distance call (one not in your local
area code).  For example, in some areas you must dial 1 before an area
code. Note that this is used to replace the + sign in phone numbers
when dialling (international dialing prefix.)"
  :group 'bbdb-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "1")))

(defcustom bbdb-sound-player nil
  "The program to be used to play the sounds for the touch-tone digits."
  :group 'bbdb-dialing
  :type '(choice (const :tag "No External Player" nil)
                 (file :tag "Sound Player" "/usr/local/bin/play")))

(defcustom bbdb-sound-files
  (vconcat
   (mapcar (lambda (x) (format "/usr/demo/SOUND/sounds/touchtone.%s.au" x))
           '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "pound" "star")))
  "Vector of sound files to be used for dialing.
They correspond to the 0, 1, 2, ... 9 digits, pound and star, respectively."
  :group 'bbdb-dialing
  :type 'vector)

(defcustom bbdb-modem-dial nil
  "Type of dialing to use.
If this value is nil, the audio device is used for dialing. Otherwise,
this string is fed to the modem before the phone number digits."
  :group 'bbdb-dialing
  :type '(choice (const  :tag "audio" nil)
                 (string :tag "tone dialing" "ATDT ")
                 (string :tag "pulse dialing" "ATDP ")))

(defcustom bbdb-modem-device "/dev/modem"
  "The name of the modem device.
This is only used if `bbdb-modem-dial' is set to something other than nil."
  :group 'bbdb-dialing
  :type 'string)

(defcustom bbdb-sound-volume 50
  "The volume to play back dial tones at. The range is 0 to 100.
This is only used if `bbdb-modem-dial' is set to nil."
  :group 'bbdb-dialing
  :type 'integer)

;;; Internal variables
(eval-and-compile
  (defvar bbdb-debug t
    "Enable debugging if non-nil during compile time.
You really should not disable debugging.  But it will speed things up."))

(defconst bbdb-file-format 7
  "BBDB file format.")

(defconst bbdb-file-coding-system 'utf-8
  "Coding system used for reading and writing `bbdb-file'.")

(defvar bbdb-mail-aliases-need-rebuilt nil)

(defvar bbdb-need-to-sort nil
  "Non-nil if records require sorting after editing.")

(defvar bbdb-suppress-changed-records-recording nil
  "Whether to record changed records in variable `bbdb-changed-records'.

If this is nil, BBDB will cease to remember which records are changed
as the change happens.  It will still remember that records have been changed,
so the file will still be saved, but the changed records list, and the `!!'
in the *BBDB* buffer modeline that it depends on, will no longer be updated.

You should bind this variable, not set it. The `!!' is a useful user interface
feature, and should only be suppressed when changes need to be automatically
made to BBDB records which the user will not care directly about.")

(defvar bbdb-buffer nil)

(defvar bbdb-buffer-name "*BBDB*")

(defvar bbdb-silent-internal nil
  "Bind this to t to quiet things down - do not set it.
See also `bbdb-silent'.")

;; hack hack: a couple of specials that the electric stuff uses for state.
(defvar bbdb-inside-electric-display nil)
(defvar bbdb-electric-execute)
(defvar bbdb-electric-done)

(defvar bbdb-notice-hook-pending nil
  "Internal variable; hands off.
Set to t by BBDB when inside the `bbdb-notice-hook'.
Calls of `bbdb-change-hook' are suppressed when this is non-nil.")

(defvar bbdb-auto-notes-rules-expanded nil
  "Expanded `bbdb-auto-notes-rules'.")

(defvar bbdb-init-forms
  '((gnus                       ; gnus 3.15 or newer
     (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
    (mh-e                       ; MH-E
     (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh))
    (rmail                      ; RMAIL
     (add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail))
    (vm                        ; newer versions of vm do not have `vm-load-hook'
     (eval-after-load "vm" '(bbdb-insinuate-vm)))
    (mail                       ; the standard mail user agent
     (add-hook 'mail-setup-hook 'bbdb-insinuate-mail))
    (sendmail
     (progn (message "BBDB: sendmail insinuation depreciated. Use mail.")
            (add-hook 'mail-setup-hook 'bbdb-insinuate-mail)))
    (message                    ; the gnus mail user agent
     (add-hook 'message-setup-hook 'bbdb-insinuate-message)))
  "Alist mapping features to insinuation forms.")

(defvar bbdb-search-invert nil
  "Bind this variable to t in order to invert the result of `bbdb-search'.")

(defvar bbdb-append-display nil
  "Controls the behavior of the command `bbdb-append-display'.")

(defvar bbdb-complete-mail-saved-window-config nil
  "Store the window configuration before we pop up the completion buffer.")

(defvar bbdb-offer-to-create nil
  "Used for communication between `bbdb-update-records'
 and `bbdb-prompt-for-create'.")

(defvar bbdb-update-records-address nil
  "Used for communication between `bbdb-update-records'
and `bbdb-prompt-for-create'.
It is a list (NAME MAIL HEADER HEADER-CLASS MUA).")

;;; Buffer-local variables for the database.
(defvar bbdb-records nil
  "BBDB records list.
In buffer `bbdb-file' this list includes all records.
In the *BBDB* buffers it includes the records that are actually displayed
and its elements are (RECORD DISPLAY-FORMAT MARKER-POS).")
(make-variable-buffer-local 'bbdb-records)

(defvar bbdb-changed-records nil)

(defvar bbdb-end-marker nil)

;; The value 127 is an arbitrary prime number.
;; see elisp:Creating Symbols
(defvar bbdb-hashtable (make-vector 127 0)
  "Hash table for BBDB records.
Hashes the fields first-last-name, last-first-name, organization, aka,
and mail.")

(defvar bbdb-notes-label-list nil
  "List of labels for Notes fields.")

(defvar bbdb-modified nil
  "Non-nil if the database has been modified.")

(defvar bbdb-message-cache nil
  "Message cache for speeding up the the mail interfaces.
The cache is a buffer-local alist for each MUA or MUA folder.
Its elements are (MESSAGE-KEY RECORDS). MESSAGE-KEY is specific to the MUA.")
(make-variable-buffer-local 'bbdb-message-cache)

(defvar bbdb-modeline-info (make-vector 4 nil)
  "Precalculated mode line info for BBDB commands.
This is a vector [APPEND-M INVERT-M APPEND INVERT].
APPEND-M is the mode line info if `bbdb-append-display' is non-nil.
INVERT-M is the mode line info if `bbdb-search-invert' is non-nil.
APPEND and INVERT appear in the message area.")

;;; Keymap
(defvar bbdb-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "*"          'bbdb-do-all-records)
    (define-key km "+"          'bbdb-append-display)
    (define-key km "!"          'bbdb-search-invert)
    (define-key km "a"          'bbdb-add-mail-alias)
    (define-key km "A"          'bbdb-mail-aliases)
    (define-key km "c"          'bbdb-create)
    (define-key km "e"          'bbdb-edit-field)
    (define-key km "n"          'bbdb-next-record)
    (define-key km "p"          'bbdb-prev-record)
    (define-key km "d"          'bbdb-delete-field-or-record)
    (define-key km "\C-k"       'bbdb-delete-field-or-record)
    (define-key km "i"          'bbdb-insert-field)
    (define-key km "s"          'bbdb-save)
    (define-key km "\C-x\C-s"   'bbdb-save)
    (define-key km "t"          'bbdb-toggle-records-layout)
    (define-key km "T"          'bbdb-display-records-completely)
    (define-key km "o"          'bbdb-omit-record)
    (define-key km "m"          'bbdb-mail)
    (define-key km "M"          'bbdb-mail-address)
    (define-key km "\M-d"       'bbdb-dial)
    (define-key km "h"          'bbdb-info)
    (define-key km "?"          'bbdb-help)
    (define-key km "q"          'bbdb-quit-window)
    (define-key km "\C-x\C-t"   'bbdb-transpose-fields)
    (define-key km "C"          'bbdb-copy-records-as-kill)
    (define-key km "u"          'bbdb-browse-url)
    (define-key km "P"          'bbdb-print)
    (define-key km "="          'delete-other-windows)

    ;; Search keys
    (define-key km "b"          'bbdb)
    (define-key km "S1"         'bbdb-display-records)
    (define-key km "Sn"         'bbdb-search-name)
    (define-key km "So"         'bbdb-search-organization)
    (define-key km "Sp"         'bbdb-search-phone)
    (define-key km "Sa"         'bbdb-search-address)
    (define-key km "Sm"         'bbdb-search-mail)
    (define-key km "SN"         'bbdb-search-notes)
    (define-key km "Sc"         'bbdb-search-changed)
    (define-key km "Sd"         'bbdb-search-duplicates)
    (define-key km "SA"         'bbdb-display-all-records)

    (define-key km [delete]     'scroll-down)
    (define-key km " "          'scroll-up)

    (define-key km [mouse-3]    'bbdb-mouse-menu)
    (define-key km [mouse-2]    (lambda (event)
                                  ;; Toggle record layout
                                  (interactive "e")
                                  (save-excursion
                                    (posn-set-point (event-end event))
                                    (bbdb-toggle-records-layout
                                     (bbdb-do-records t) current-prefix-arg))))
    km)
  "Keymap for Insidious Big Brother Database.
This is a child of `special-mode-map'.")

(easy-menu-define
  bbdb-menu bbdb-mode-map "BBDB Menu"
  '("BBDB"
    ("Display"
     ["Previous record" bbdb-prev-record t]
     ["Next record" bbdb-next-record t]
     "--"
     ["Toggle layout" bbdb-toggle-records-layout t]
     ["Show all fields" bbdb-display-records-completely t]
     ["Omit record" bbdb-omit-record t])
    ("Searching"
     ["General search" bbdb t]
     ["Search one record" bbdb-display-records t]
     ["Search name" bbdb-search-name t]
     ["Search organization" bbdb-search-organization t]
     ["Search phone" bbdb-search-phone t]
     ["Search address" bbdb-search-address t]
     ["Search mail" bbdb-search-mail t]
     ["Search notes" bbdb-search-notes t]
     ["Search changed records" bbdb-search-changed t]
     ["Search duplicates" bbdb-search-duplicates t]
     "--"
     ["Old time stamps" bbdb-timestamp-older t]
     ["New time stamps" bbdb-timestamp-newer t]
     ["Old creation date" bbdb-creation-older t]
     ["New creation date" bbdb-creation-newer t]
     ["Creation date = time stamp" bbdb-creation-no-change t]
     "--"
     ["Append search" bbdb-append-display t]
     ["Invert search" bbdb-search-invert t]
     "--"
     ["Show all records" bbdb-display-all-records t])
    ("Mail"
     ["Send mail" bbdb-mail t]
     ["Save mail address" bbdb-mail-address t]
     "--"
     ["Add mail alias" bbdb-add-mail-alias t]
     ["(Re-)Build mail aliases" bbdb-mail-aliases t])
    ("Use database"
     ["Prefix: do all records" bbdb-do-all-records t]
     "--"
     ["Send mail" bbdb-mail t]
     ["Dial phone number" bbdb-dial t]
     ["Browse URL" bbdb-browse-url t]
     ["Copy records as kill" bbdb-copy-records-as-kill t]
     "--"
     ["Print records" bbdb-print t])
    ("Manipulate database"
     ["Prefix: do all records" bbdb-do-all-records t]
     "--"
     ["Create new record" bbdb-create t]
     ["Edit current field" bbdb-edit-field t]
     ["Insert new field" bbdb-insert-field t]
     ["Transpose fields" bbdb-transpose-fields t]
     ["Delete record or field" bbdb-delete-field-or-record t]
     "--"
     ["Sort addresses" bbdb-sort-addresses t]
     ["Sort phones" bbdb-sort-phones t]
     ["Sort notes" bbdb-sort-notes t]
     ["Merge records" bbdb-merge-records t]
     ["Sort database" bbdb-sort-records t]
     ["Delete duplicate mails" bbdb-delete-duplicate-mails t]
     "--"
     ["Save BBDB" bbdb-save t]
     ["Revert BBDB" revert-buffer t])
    ("Help"
     ["Brief help" bbdb-help t]
     ["BBDB Manual" bbdb-info t])
    "--"
    ["Quit" bbdb-quit-window t]))

(defvar bbdb-completing-read-mails-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " 'self-insert-command)
    (define-key map "\t" 'bbdb-complete-mail)
    (define-key map "\M-\t" 'bbdb-complete-mail)
    map))


;; Faces for font-lock
(defgroup bbdb-faces nil
  "Faces used by BBDB."
  :group 'proced
  :group 'faces)

(defface bbdb-name
  '((t (:inherit font-lock-function-name-face)))
  "Face used for BBDB names."
  :group 'bbdb-faces)

(defface bbdb-organization
  '((t (:inherit font-lock-comment-face)))
  "Face used for BBDB names."
  :group 'bbdb-faces)

(defface bbdb-field-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for BBDB names."
  :group 'bbdb-faces)



;;; Helper functions

(defun bbdb-warn (&rest args)
  (ding t)
  (apply 'message args))

(defsubst bbdb-string-trim (string)
  "Remove leading and trailing whitespace and all properties from STRING.
If STRING is nil return an empty string."
  (if (null string)
      ""
    (if (string-match "\\`[ \t\n]+" string)
        (setq string (substring string (match-end 0))))
    (if (string-match "[ \t\n]+\\'" string)
        (setq string (substring string 0 (match-beginning 0))))
    (substring-no-properties string)))

(defsubst bbdb-string= (str1 str2)
  "Return t if strings STR1 and STR2 are equal, ignoring case."
  (and (stringp str1) (stringp str2)
       (eq t (compare-strings str1 0 nil str2 0 nil t))))

(defun bbdb-split (separator string)
  "Return a list by splitting STRING at SEPARATOR.
SEPARATOR may be a regexp.  SEPARATOR may also be a symbol
\(a field name).  Then look up the value in `bbdb-separator-alist'
or use `bbdb-default-separator'.
Whitespace around SEPARATOR is ignored unless SEPARATOR matches
the string \" \\t\\n\".
Almost the inverse function of `bbdb-concat'."
  (if (symbolp separator)
      (setq separator (car (or (cdr (assq separator bbdb-separator-alist))
                               bbdb-default-separator))))
  (unless (string-match separator " \t\n")
    (setq separator (concat "[ \t\n]*" separator "[ \t\n]*")))
  ;; `split-string' applied to an empty STRING gives nil.
  (split-string (bbdb-string-trim string) separator t))

(defun bbdb-concat (separator &rest strings)
  "Concatenate STRINGS to a string where STRINGS are separated by SEPARATOR.
STRINGS may be strings or lists of strings.  Empty strings are ignored.
SEPARATOR may be a string.
SEPARATOR may also be a symbol (a field name).  Then look up the value
of SEPARATOR in `bbdb-separator-alist' or use `bbdb-default-separator'.
The inverse function of `bbdb-split'."
  (if (symbolp separator)
      (setq separator (nth 1 (or (cdr (assq separator bbdb-separator-alist))
                                 bbdb-default-separator))))
  (mapconcat 'identity
             (delete "" (apply 'append (mapcar (lambda (x) (if (stringp x)
                                                               (list x) x))
                                               strings))) separator))

;; A call of `indent-region' swallows any indentation
;; that might be part of the field itself.  So we indent manually.
(defsubst bbdb-indent-string (string column)
  "Indent nonempty lines in STRING to COLUMN (except first line).
This happens in addition to any pre-defined indentation of STRING."
  (replace-regexp-in-string "\n\\([^\n]\\)"
                            (concat "\n" (make-string column ?\s) "\\1")
                            string))

(defun bbdb-read-string (prompt &optional default collection)
  "Read a string, trimming whitespace and text properties.
DEFAULT appears as initial input, which is convenient for editing
existing BBDB records."
  (bbdb-string-trim
   (if collection
       (completing-read prompt collection nil nil default)
     (read-string prompt default))))

(defun bbdb-current-record (&optional full)
  "Returns the record which point is at.
If FULL is non-nil record includes the display information."
  (unless (eq major-mode 'bbdb-mode)
    (error "This only works while in BBDB buffers."))
  (let ((num (get-text-property (if (and (not (bobp)) (eobp))
                                    (1- (point)) (point))
                                'bbdb-record-number))
        record)
    (unless num (error "Not a BBDB record"))
    (setq record (nth num bbdb-records))
    (if full record (car record))))

(defun bbdb-current-field ()
  (unless (bbdb-current-record) (error "Not a BBDB record"))
  (get-text-property (point) 'bbdb-field))

(defmacro bbdb-debug (&rest body)
  "Turn on debugging if variable `bbdb-debug' is non-nil during compile.
You really should not disable debugging.  But it will speed things up."
  (declare (indent 0))
  (if bbdb-debug ; compile-time switch
      `(let ((debug-on-error t))
         ,@body)))

(defun bbdb-timestamp (record)
  "For use as a `bbdb-change-hook'.
Maintains a notes-field `timestamp' for the given record which contains
the time when it was last modified.  If such a field already exists,
it is changed, otherwise it is added."
  (bbdb-record-set-note record 'timestamp
                       (format-time-string bbdb-time-stamp-format nil t)))

(defun bbdb-creation-date (record)
  "For use as a `bbdb-create-hook'.
Adds a notes-field `creation-date' which is the current time string."
  (bbdb-record-set-note record 'creation-date
                       (format-time-string bbdb-time-stamp-format nil t)))

(defun bbdb-multiple-buffers-default ()
  "Default function for guessing a name for new *BBDB* buffers.
May be used as value of variable `bbdb-multiple-buffers'."
  (save-current-buffer
    (cond ((memq major-mode '(vm-mode vm-summary-mode vm-presentation-mode
                                      vm-virtual-mode))
           (vm-select-folder-buffer)
           (buffer-name))
          ((memq major-mode '(gnus-summary-mode gnus-group-mode))
           (set-buffer gnus-article-buffer)
           (buffer-name))
          ((memq major-mode '(mail-mode vm-mail-mode message-mode))
           "message composition"))))

;; BBDB data structure
(defmacro bbdb-defstruct (name &rest elts)
  "Define two functions to operate on vector NAME for each ELT in ELTS.
The function bbdb-NAME-ELT reads the element ELT in vector NAME.
The function bbdb-NAME-set-ELT sets ELT.
Also define a constant bbdb-NAME-length that holds the number of ELTS
in vector NAME."
  (declare (indent 1))
  (let* ((count 0)
         (sname (symbol-name name))
         (cname (concat "bbdb-" sname "-"))
         body)
    (dolist (elt elts)
      (let* ((selt (symbol-name elt))
             (setname  (intern (concat cname "set-" selt))))
        (push (list 'defsubst (intern (concat cname selt)) '(vector)
                    (format "For BBDB `%s' vector read element %i `%s'."
                            sname count selt)
                    `(aref vector ,count)) body)
        (push (append (list 'defsubst setname '(vector value)
                            (format "For BBDB `%s' vector set element %i `%s'.
Return new value."
                            sname count selt))
                      (if (string= setname "bbdb-record-set-mail")
                          '((unless bbdb-mail-aliases-need-rebuilt
                              (setq bbdb-mail-aliases-need-rebuilt 'edit))))
                      `((aset vector ,count value))) body))
      (setq count (1+ count)))
    (push (list 'defconst (intern (concat cname "length"))
                (length elts)
                (concat "Length of BBDB `" sname "' vector.")) body)
    (cons 'progn body)))

;; Define RECORD:
(bbdb-defstruct record
  firstname lastname degree aka organization phone address mail notes cache)

;; Define PHONE:
(bbdb-defstruct phone
  label area exchange suffix extension)

;; Define ADDRESS:
(bbdb-defstruct address
  label streets city state postcode country)

;; When reading this code, beware that "cache" refers to two things.
;; It refers to the cache slot of record structures, which is
;; used for computed properties of the records; and it also refers
;; to a message-id --> record association list which speeds up
;; the RMAIL, VM, and MH interfaces.

;; Define record CACHE:
;; - fl-name (first and last name of the person referred to by the record),
;; - lf-name (last and first name of the person referred to by the record),
;; - sortkey (the concatenation of the elements used for sorting the record),
;; - marker  (position of beginning of record in `bbdb-file')
;; - deleted-p (a flag).
(bbdb-defstruct cache
  fl-name lf-name sortkey marker deleted-p)

;; `bbdb-hashtable' associates with each FIELD a list of matching records.
(defsubst bbdb-puthash (field record)
  (let ((sym (intern (downcase field) bbdb-hashtable)))
    (if (boundp sym)
        (unless (memq record (symbol-value sym))
          (set sym (cons record (symbol-value sym))))
      (set sym (list record)))))

(defsubst bbdb-gethash (field)
  (if field
      (symbol-value (intern-soft (downcase field) bbdb-hashtable))))

(defsubst bbdb-remhash (field record)
  (let ((sym (intern-soft (downcase field) bbdb-hashtable))
        val)
    (when sym
      (setq val (delq record (symbol-value sym)))
      (if val (set sym val) (unintern sym bbdb-hashtable)))))

(defun bbdb-hash-record (record)
  "Insert the record in `bbdb-hashtable'."
  (bbdb-record-name record)
  (dolist (organization (bbdb-record-organization record))
    (bbdb-puthash organization record))
  (dolist (aka (bbdb-record-aka record))
    (bbdb-puthash aka record))
  (dolist (mail (bbdb-record-mail record))
    (bbdb-puthash mail record)))

(defun bbdb-record-name (record)
  "Record cache function: Return the full name of a record.
If the name is not available in the name cache, the name cache value
is generated and stored."
  (or (bbdb-cache-fl-name (bbdb-record-cache record))
      ;; Build the name cache for a record.
      (bbdb-record-set-name record)))

(defun bbdb-record-set-name (record &optional first last)
  "Record cache function: Set the full name of RECORD.
Set full name in cache and hash.  Return first-last name."
  (if first
      (bbdb-record-set-firstname record first)
    (setq first (bbdb-record-firstname record)))
  (if last
      (bbdb-record-set-lastname record last)
    (setq last (bbdb-record-lastname record)))
  (let ((fl (bbdb-concat " " first last))
        (lf (bbdb-concat " " last first))
        (cache (bbdb-record-cache record)))
    ;; Set cache
    (bbdb-cache-set-fl-name cache fl)
    (bbdb-cache-set-lf-name cache lf)
    ;; Set hash.  For convenience, the hash contains the full name as
    ;; first-last and last-fist.
    (bbdb-puthash fl record)
    (bbdb-puthash lf record)
    fl))

(defun bbdb-record-unset-name (record)
  "Record cache function: Unset the full name of RECORD.
Remove full name in hash."
  (let* ((cache (bbdb-record-cache record))
         (fl-name (bbdb-cache-fl-name cache))
         (lf-name (bbdb-cache-lf-name cache)))
    (unless (or (eq fl-name nil)
                (string= "" fl-name))
      (bbdb-remhash fl-name record))
    (unless (or (eq lf-name nil)
                (string= "" lf-name))
      (bbdb-remhash lf-name record))
    (bbdb-cache-set-fl-name cache nil)
    (bbdb-cache-set-lf-name cache nil)))

(defun bbdb-record-sortkey (record)
  "Record cache function: Return the sortkey for a record.
Build and store it if necessary."
  (or (bbdb-cache-sortkey (bbdb-record-cache record))
      (bbdb-cache-set-sortkey (bbdb-record-cache record)
        (downcase
         (bbdb-concat "" (bbdb-record-lastname record)
                      (bbdb-record-firstname record)
                      (bbdb-record-organization record))))))

(defsubst bbdb-record-set-sortkey (record sortkey)
  "Record cache function: Set and return the sortkey for a record."
  (bbdb-cache-set-sortkey (bbdb-record-cache record) sortkey))

(defsubst bbdb-record-marker (record)
  "Record cache function: Return the marker for a record."
  (bbdb-cache-marker (bbdb-record-cache record)))

(defsubst bbdb-record-set-marker (record marker)
  "Record cache function: Set and return the marker for a record."
  (bbdb-cache-set-marker (bbdb-record-cache record) marker))

(defsubst bbdb-record-deleted-p (record)
  "Record cache function: Return the `deleted' flag for a record."
  (bbdb-cache-deleted-p (bbdb-record-cache record)))

;; `bbdb-record-set-deleted-p' is used exactly once by `bbdb-delete-records'
;; so that this flag is set just before the record is deleted completely.
;; Do we need this?? When would we want to set the deleted flag without
;; actually performing the deletion?
(defsubst bbdb-record-set-deleted-p (record val)
  "Record cache function: Set and return the `deleted' flag for a record."
  (bbdb-cache-set-deleted-p (bbdb-record-cache record) val))

(defsubst bbdb-record-note (record label)
  "For RECORD return value of note LABEL."
  (cdr (assq label (bbdb-record-notes record))))

(defsubst bbdb-record-note-split (record label)
  "For RECORD return value of note LABEL split as a list.
Splitting is based on `bbdb-separator-alist'."
  (let ((val (bbdb-record-note record label)))
    (if val (bbdb-split label val))))

(defun bbdb-record-set-note (record label value)
  "For RECORD set note LABEL to VALUE.
If VALUE is nil, remove note LABEL from RECORD.  Return VALUE."
  (if (and value (string= "" value)) (setq value nil))
  (let ((oldval (assq label (bbdb-record-notes record))))
    ;; Do nothing if both oldval and value are nil.
    (cond ((and oldval value) ; update
           (setcdr oldval value))
          (value ; new field
           (bbdb-record-set-notes record
                                  (append (bbdb-record-notes record)
                                          (list (cons label value)))))
          (oldval ; remove
           (bbdb-record-set-notes record
                                  (delq oldval (bbdb-record-notes record))))))
  value)

(defun bbdb-merge-note (record label value &optional replace)
  "For RECORD merge content of note LABEL with VALUE.
If REPLACE is non-nil, content is replaced by VALUE."
  (let ((oldval (bbdb-string-trim (or (bbdb-record-note record label) "")))
        (value  (bbdb-string-trim value)))
    (unless (or (string= "" value)
                (string-match (regexp-quote value) oldval))
      (unless bbdb-silent
        (if replace
            (if (eq label 'notes)
                (message "Replacing with note \"%s\"" value)
              (message "Replacing field \"%s\" with \"%s\"" label value))
          (if (eq label 'notes)
              (message "Adding note \"%s\"" value)
            (message "Adding \"%s\" to field \"%s\"" value label))))
      (bbdb-record-set-note
       record label
       (if replace
           value
         (bbdb-concat label oldval value))))))

(defun bbdb-phone-string (phone)
  ;; Phone numbers should come in two forms:
  (if (= 2 (length phone))
      ;; (1) ["where" "the number"]
      (if (stringp (aref phone 1))
          (aref phone 1)
        (error "Not a valid phone number: %s" (aref phone 1)))
    ;; (2) ["where" 415 555 1212 99]
    (unless (and (integerp (aref phone 2))
                 (integerp (aref phone 3)))
      (error "Not an NANP number: %s %s"
             (integerp (aref phone 2)) (integerp (aref phone 3))))
    (concat (if (/= 0 (bbdb-phone-area phone))
                (format "(%03d) " (bbdb-phone-area phone))
                "")
            (if (/= 0 (bbdb-phone-exchange phone))
                (format "%03d-%04d"
                        (bbdb-phone-exchange phone) (bbdb-phone-suffix phone))
                "")
            (if (and (bbdb-phone-extension phone)
                     (/= 0 (bbdb-phone-extension phone)))
                (format " x%d" (bbdb-phone-extension phone))
                ""))))

(defsubst bbdb-record-lessp (record1 record2)
  (string< (bbdb-record-sortkey record1)
           (bbdb-record-sortkey record2)))

(defmacro bbdb-error-retry (form)
  `(catch '--bbdb-error-retry--
     (while t
       (condition-case --c--
           (throw '--bbdb-error-retry-- ,form)
         (error (ding)
                (message "Error: %s" (nth 1 --c--))
                (sit-for 2))))))

;;; Completion on labels and field data

(defun bbdb-label-completion-list (field)
  "Figure out a completion list for the specified FIELD label.
This evaluates the variable bbdb-FIELD-label-list, such
as `bbdb-phone-label-list'."
  (let ((sym (intern-soft (format "bbdb-%s-label-list" field))))
    (if (boundp sym)
        (eval sym)
      bbdb-default-label-list)))

(defun bbdb-label-completion-default (field)
  "Figure out a default label from the completion list for FIELD.
This evaluates the variable bbdb-default-FIELD-label, such
as `bbdb-default-phone-label', if it exists, or it takes
the first item from the list of completions for FIELD as
returned by `bbdb-label-completion-list'."
  (let ((sym (intern-soft (format "bbdb-default-%s-label" field))))
    (if (boundp sym)
        (eval sym)
      (car (bbdb-label-completion-list field)))))

;; These are so you can accumulate e.g. mail aliases or organization names
;; and have BBDB offer completion on them.
(defun bbdb-data-completion-list (field)
  "Figure out a completion list for the specified FIELD value.
This evaluates the variable bbdb-FIELD-data-list, such
as `bbdb-mail-alias-data-list', if it exists, or it uses
`bbdb-default-label-list'."
  (let ((sym (intern-soft (format "bbdb-%s-data-list" field))))
    (if (boundp sym)
        (eval sym)
      bbdb-default-label-list)))

(defun bbdb-data-completion-default (field)
  "Figure out a default value from the completion list for FIELD.
This evaluates the variable bbdb-default-FIELD-data, such
as `bbdb-default-mail-alias-data', if it exists, or it takes
the first item from the list of completions for FIELD as
returned by `bbdb-data-completion-list'."
  (let ((sym (intern-soft (format "bbdb-default-%s-data" field))))
    (if (boundp sym)
        (eval sym)
      (nth 0 (bbdb-label-completion-list field)))))

;;;
(defun bbdb-buffer ()
  "Return buffer that visits the BBDB file `bbdb-file'.
Ignore that `bbdb-file' might have changed on disk, which is handled
by `bbdb-records' (if we actually want to write to `bbdb-file').
If `bbdb-file-remote' is non-nil and it is newer than `bbdb-file',
copy it to `bbdb-file'."
  (if (and bbdb-buffer (buffer-live-p bbdb-buffer))
      bbdb-buffer
    (if (and bbdb-file-remote
             (file-newer-than-file-p bbdb-file-remote bbdb-file))
        (let ((coding-system-for-write bbdb-file-coding-system))
          (copy-file bbdb-file-remote bbdb-file t t)))
    ;; If arg NOWARN of `find-file-noselct' is t, it does not warn us
    ;; when the file has changed on disk. But it simply returns the
    ;; buffer that is currently visiting `bbdb-file'.
    ;; Changing files are handled by `bbdb-records'.
    (setq bbdb-buffer (find-file-noselect bbdb-file t))))

(defmacro bbdb-with-db-buffer (&rest body)
  (declare (indent 0))
  (if bbdb-debug ;; compile-time switch
      ;; If we are debugging, and `bbdb-file' is visible in
      ;; a window, temporarilly switch to that window so that
      ;; when we come out, that window has been scrolled to the
      ;; record we have just modified.
      `(let* ((buffer (bbdb-buffer))
              (window (get-buffer-window buffer)))
         (if window
             (with-selected-window window
               ,@body)
           (with-current-buffer buffer
             ,@body)))
    `(with-current-buffer (bbdb-buffer)
       ,@body)))

;; Record formatting:
;; This does not insert anything into the *BBDB* buffer,
;; which is handled in a second step by the display functions.

(defun bbdb-layout-get-option (layout option)
  "For LAYOUT return value of OPTION according to `bbdb-layout-alist'."
  (let ((layout-spec (if (listp layout)
                         layout
                       (assq layout bbdb-layout-alist)))
        option-value)
    (and layout-spec
         (setq option-value (assq option layout-spec))
         (cdr option-value))))

(defun bbdb-address-continental-p (address)
  "Return non-nil if ADDRESS is a continental address.
This is done by comparing the postcode to `bbdb-continental-postcode-regexp'.

This is a possible identifying function for
`bbdb-address-format-list' and `bbdb-print-address-format-list'."
  (string-match bbdb-continental-postcode-regexp
                (bbdb-address-postcode address)))

;; This function can provide some guidance for writing
;; your own address formatting function
(defun bbdb-format-address-default (address)
  "Return formatted ADDRESS as a string.
This is the default format; it is used in the US, for example.
The result looks like this:
       label: street
              street
              ...
              city, state postcode
              country.

This function is a possible formatting function for
`bbdb-address-format-list'."
  (let ((country (bbdb-address-country address))
        (streets (bbdb-address-streets address)))
    (concat (if streets
                (concat (mapconcat 'identity streets "\n") "\n") "")
            (bbdb-concat ", " (bbdb-address-city address)
                         (bbdb-concat " " (bbdb-address-state address)
                                      (bbdb-address-postcode address)))
            (if (string= "" country) ""
              (concat "\n" country)))))

(defun bbdb-format-address (address layout)
  "Format ADDRESS using LAYOUT.  Return result as a string.
The formatting rules are defined in `bbdb-address-format-list'."
  (let ((list bbdb-address-format-list)
        (country (bbdb-address-country address))
        elt string)
    (while (and (not string) (setq elt (pop list)))
      (let ((identifier (car elt))
            (format (nth layout elt))
            ;; recognize case for format identifiers
            case-fold-search str)
        (when (or (eq t identifier) ; default
                  (and (functionp identifier)
                       (funcall identifier address))
                  (and country
                       (listp identifier)
                       ;; ignore case for countries
                       (member-ignore-case country identifier)))
          (cond ((functionp format)
                 (setq string (funcall format address)))
                ((stringp format)
                 (setq string "")
                 (dolist (form (split-string (substring format 1 -1)
                                             (substring format 0 1) t))
                   (cond ((string-match "%s" form)
                          (mapc (lambda (s) (setq string (concat string (format form s))))
                                (bbdb-address-streets address)))
                         ((string-match "%c" form)
                          (unless (string= "" (setq str (bbdb-address-city address)))
                            (setq string (concat string (format (replace-regexp-in-string "%c" "%s" form) str)))))
                         ((string-match "%p" form)
                          (unless (string= ""  (setq str (bbdb-address-postcode address)))
                            (setq string (concat string (format (replace-regexp-in-string "%p" "%s" form) str)))))
                         ((string-match "%S" form)
                          (unless (string= ""  (setq str (bbdb-address-state address)))
                            (setq string (concat string (format (replace-regexp-in-string "%S" "%s" form t) str)))))
                         ((string-match "%C" form)
                          (unless (string= ""  country)
                            (setq string (concat string (format (replace-regexp-in-string "%C" "%s" form t) country)))))
                         (t (error "Malformed address format element %s" form)))))
                (t (error "Malformed address format %s" format))))))
    (unless string
      (error "No match of `bbdb-address-format-list'"))
    string))

;;; Record display:
;;; This inserts formatted (pieces of) records into the BBDB buffer.

(defsubst bbdb-field-property (start field)
  "Set text property bbdb-field of text between START and point to FIELD."
  (put-text-property start (point) 'bbdb-field field))

(defsubst bbdb-display-text (text field &optional face)
  "Insert TEXT at point.  Set its text property bbdb-field to FIELD.
If FACE is non-nil, also add face FACE."
  (let ((start (point)))
    (insert text)
    (bbdb-field-property start field)
    (if face (put-text-property start (point) 'face face))))

(defun bbdb-display-list (list field &optional terminator face indent)
  "Insert elements of LIST at point.
For inserted text, set text property bbdb-field to field.
If TERMINATOR is non-nil use it to terminate the inserted text.
If INDENT and `bbdb-wrap-column' are integers, insert line breaks in between
elements of LIST if otherwise inserted text exceeds `bbdb-wrap-column'."
  ;; `truncate-lines' is fine for one-line layout.  But it is  annyoing
  ;; for records that are displayed with multi-line layout.
  ;; Non-nil `word-wrap' would be much nicer.  How can we switch between
  ;; non-nil `truncate-lines' and non-nil `word-wrap' on a per-record basis?
  ;; The following code is an alternative solution using `bbdb-wrap-column'.
  (let* ((separator (nth 1 (or (cdr (assq field bbdb-separator-alist))
                               bbdb-default-separator)))
         (indent-flag (and (integerp bbdb-wrap-column)
                           (integerp indent)))
         (prefix (if indent-flag
                     (concat separator "\n" (make-string indent ?\s))))
        elt)
    (while (setq elt (pop list))
      (bbdb-display-text elt (list field elt) face)
      (cond ((and list indent-flag
                  (> (+ (current-column) (length (car list)))
                     bbdb-wrap-column))
             (bbdb-display-text prefix (list field) face))
            (list
             (bbdb-display-text separator (list field) face))
            (terminator
             (bbdb-display-text terminator (list field) face))))))

(defun bbdb-display-name-organization (record)
  "Insert name, degree, and organization of RECORD."
  ;; Name
  (bbdb-display-text (or (bbdb-record-name record) "???")
                     '(name) 'bbdb-name)
  ;; Degree
  (let ((degree (bbdb-record-degree record)))
    (when degree
      (insert ", ")
      (bbdb-display-list degree 'degree)))
  ;; Organization
  (let ((organization (bbdb-record-organization record)))
    (when organization
      (insert " - ")
      (bbdb-display-list organization 'organization nil
                         'bbdb-organization))))

(defun bbdb-display-record-one-line (record layout field-list)
  "Record formatting function for the one-line layout.
See `bbdb-layout-alist' for more info."
  ;; Name, degree, and organizations
  (bbdb-display-name-organization record)
  (let ((name-end (or (bbdb-layout-get-option layout 'name-end)
                      40))
        (start (line-beginning-position)))
    (when (> (- (point) start -1) name-end)
      (put-text-property (+ start name-end -4) (point) 'invisible t)
      (insert "..."))
    (indent-to name-end))
  ;; rest of the fields
  (let (formatfun start)
    (dolist (field field-list)
      (cond (;; customized formatting
             (setq formatfun (intern-soft (format "bbdb-display-%s-one-line" field)))
             (funcall formatfun record))
            ;; phone
            ((eq field 'phone)
             (let ((phones (bbdb-record-phone record)) phone)
               (if phones
                   (while (setq phone (pop phones))
                     (bbdb-display-text (format "%s " (aref phone 0))
                                        (list 'phone phone 'field-label)
                                        'bbdb-field-name)
                     (bbdb-display-text (format "%s%s" (aref phone 1)
                                                (if phones " " "; "))
                                        (list 'phone phone))))))
            ;; address
            ((eq field 'address)
             (dolist (address (bbdb-record-address record))
               (setq start (point))
               (insert (bbdb-format-address address 3))
               (bbdb-field-property start (list 'address address))
               (insert "; ")))
            ;; mail
            ((eq field 'mail)
             (let ((mail (bbdb-record-mail record)))
               (if mail
                   (bbdb-display-list (if (bbdb-layout-get-option layout 'primary)
                                          (list (car mail)) mail)
                                      'mail "; "))))
            ;; AKA
            ((eq field 'aka)
             (let ((aka (bbdb-record-aka record)))
               (if aka
                   (bbdb-display-list aka 'aka "; "))))
            ;; notes
            (t
             (let ((note (assq field (bbdb-record-notes record))))
               (if note
                   (bbdb-display-text (concat (replace-regexp-in-string
                                               "\n" "; " (cdr note)) "; ")
                                      (list 'note note)))))))
    ;; delete the trailing "; "
    (if (looking-back "; ")
        (backward-delete-char 2))
    (insert "\n")))

(defun bbdb-display-record-multi-line (record layout field-list)
  "Record formatting function for the multi-line layout.
See `bbdb-layout-alist' for more."
  (bbdb-display-name-organization record)
  (insert "\n")
  (let* ((indent (or (bbdb-layout-get-option layout 'indentation) 21))
         ;; The format string FMT adds three extra characters.
         ;; So we subtract those from the value of INDENT.
         (fmt (format " %%%ds: " (- indent 3)))
         start field formatfun)
    (dolist (field field-list)
      (setq start (point))
      (cond (;; customized formatting
             (setq formatfun (intern-soft (format "bbdb-display-%s-multi-line" field)))
             (funcall formatfun record))
            ;; phone
            ((eq field 'phone)
             (dolist (phone (bbdb-record-phone record))
               (bbdb-display-text (format fmt (concat "phone ("
                                                      (bbdb-phone-label phone)
                                                      ")"))
                                  (list 'phone phone 'field-label)
                                  'bbdb-field-name)
               (bbdb-display-text (concat (bbdb-phone-string phone) "\n")
                                  (list 'phone phone))))
            ;; address
            ((eq field 'address)
             (dolist (address (bbdb-record-address record))
               (bbdb-display-text (format fmt (concat "address ("
                                                      (bbdb-address-label address)
                                                      ")"))
                                  (list 'address address 'field-label)
                                  'bbdb-field-name)
               (setq start (point))
               (insert (bbdb-indent-string (bbdb-format-address address 2) indent)
                       "\n")
               (bbdb-field-property start (list 'address address))))
            ;; mail
            ((eq field 'mail)
             (let ((mail (bbdb-record-mail record)))
               (when mail
                 (bbdb-display-text (format fmt "mail") '(mail nil field-label)
                                    'bbdb-field-name)
                 (bbdb-display-list (if (bbdb-layout-get-option layout 'primary)
                                        (list (car mail)) mail)
                                    'mail "\n" nil indent))))
            ;; AKA
            ((eq field 'aka)
             (let ((aka (bbdb-record-aka record)))
               (when aka
                 (bbdb-display-text (format fmt "AKA") '(aka nil field-label)
                                    'bbdb-field-name)
                 (bbdb-display-list aka 'aka "\n"))))
            ;; notes
            (t
             (let ((note (assq field (bbdb-record-notes record))))
               (when note
                 (bbdb-display-text (format fmt field)
                                    (list 'note note 'field-label)
                                    'bbdb-field-name)
                 (setq start (point))
                 (insert (bbdb-indent-string (cdr note) indent) "\n")
                 (bbdb-field-property start (list 'note note)))))))
    (insert "\n")))

(defalias 'bbdb-display-record-full-multi-line
  'bbdb-display-record-multi-line)

(defalias 'bbdb-display-record-pop-up-multi-line
  'bbdb-display-record-multi-line)

(defun bbdb-display-record (record layout number)
  "Insert a formatted RECORD into the current buffer at point.
LAYOUT can be a symbol describing a layout in `bbdb-layout-alist'.
If it is nil, use `bbdb-layout'.
NUMBER is the number of RECORD among the displayed records.
Move point to the end of the inserted record."
  (bbdb-debug (if (bbdb-record-deleted-p record)
                  (error "Formatting deleted record")))
  (unless layout (setq layout bbdb-layout))
  (unless (assq layout bbdb-layout-alist)
    (error "Unknown layout `%s'" layout))
  (let ((display-p  (bbdb-layout-get-option layout 'display-p))
        (omit-list  (bbdb-layout-get-option layout 'omit)) ; omitted fields
        (order-list (bbdb-layout-get-option layout 'order)); requested field order
        (all-fields (append '(phone address mail aka) ; default field order
                             (mapcar 'car (bbdb-record-notes record))))
        (beg (point))
        format-function field-list)
    (when (or (not display-p)
              (and display-p
                   (funcall display-p)))
      (if (functionp omit-list)
          (setq omit-list (funcall omit-list record layout)))
      (if (functionp order-list)
          (setq order-list (funcall order-list record layout)))
      ;; first omit unwanted fields
      (when (and omit-list (or (not order-list) (memq t order-list)))
        (if (listp omit-list)
            ;; show all fields except those listed here
            (dolist (omit omit-list)
              (setq all-fields (delq omit all-fields)))
          (setq all-fields nil))) ; show nothing
      ;; then order them
      (cond ((not order-list)
             (setq field-list all-fields))
            ((not (memq t order-list))
             (setq field-list order-list))
            (t
             (setq order-list (reverse order-list)
                   all-fields (delq nil (mapcar (lambda (f)
                                                  (unless (memq f order-list)
                                                    f))
                                                all-fields)))
             (dolist (order order-list)
               (if (eq t order)
                   (setq field-list (append all-fields field-list))
                 (push order field-list)))))
      ;; call the actual format function
      (setq format-function
            (intern-soft (format "bbdb-display-record-%s" layout)))
      (if (functionp format-function)
          (funcall format-function record layout field-list)
        (bbdb-display-record-multi-line record layout field-list))
      (put-text-property beg (point) 'bbdb-record-number number))))

(defun bbdb-display-records (&optional records layout append
                                       select horiz-p electric-p)
  "Display RECORDS using LAYOUT."
  (interactive (list (bbdb-completing-read-records "Display records: ")
                     (bbdb-layout-prefix)))
  (let ((bbdb-window (get-buffer-window bbdb-buffer-name)))
    ;; Never be electric if the buffer is already on screen.
    (if (and (not bbdb-window)
             (or bbdb-electric electric-p))
        (progn
          (define-key bbdb-mode-map " " 'bbdb-electric-done)
          (bbdb-electric-display-records records))
      (bbdb-display-records-internal records layout append select horiz-p)
      ;; do not smash keybinding if they invoked `bbdb-display'
      ;; from inside an electric loop.
      (unless bbdb-inside-electric-display
        (define-key bbdb-mode-map " " 'undefined)))))

(defun bbdb-display-records-internal (records &optional layout append
                                              select horiz-p)
  "Low-level function overlooking the setup of the *BBDB* buffer."
  (if (bbdb-append-display-p) (setq append t))

  ;; `bbdb-redisplay-records' calls `bbdb-display-records-internal'
  ;; with display information already amended to RECORDS.
  (unless (or (null records)
              (consp (car records)))
    ;; add layout and a marker to the local list of records
    (setq layout (or layout bbdb-layout)
          records (mapcar (lambda (record)
                            (list record layout (make-marker)))
                          records)))

  (let ((buffer (current-buffer))
        (first-new (caar records))) ; first new record

    ;; If `bbdb-multiple-buffers' is non-nil we create a new BBDB buffer
    ;; when not already within one.  The new buffer name starts with a space,
    ;; i.e. it does not clutter the buffer list.
    (when (and bbdb-multiple-buffers
               (not (assq 'bbdb-buffer-name (buffer-local-variables))))
      (let ((new-name (concat " *BBDB " (if (functionp bbdb-multiple-buffers)
                                            (funcall bbdb-multiple-buffers)
                                          (buffer-name))
                              "*")))
        ;; `bbdb-buffer-name' becomes buffer-local in the current buffer
        ;; as well as in the buffer `bbdb-buffer-name'
        (dolist (buffer (list (current-buffer) (get-buffer-create new-name)))
          (with-current-buffer buffer
            (set (make-local-variable 'bbdb-buffer-name) new-name)))))

    (unless (get-buffer-window bbdb-buffer-name)
      (bbdb-pop-up-window select horiz-p))
    (set-buffer bbdb-buffer-name) ;; *BBDB*

    ;; If we're appending RECORDS to the ones already displayed,
    ;; then first remove any duplicates, and then sort them.
    (if append
        (let ((old-rec (mapcar 'car bbdb-records)))
          (dolist (record records)
            (unless (memq (car record) old-rec)
              (push record bbdb-records)))
          (setq records
                (sort bbdb-records
                      (lambda (x y) (bbdb-record-lessp (car x) (car y)))))))

    (bbdb-mode)
    ;; `bbdb-records' is the only BBDB-specific buffer-local variable
    ;; in the *BBDB* buffer.
    (setq bbdb-records records)

    ;; Formatting happens in the *BBDB* buffer, not the .bbdb buffer.
    (unless (or bbdb-silent-internal bbdb-silent)
      (message "Formatting..."))
    (let ((record-number 0)
          buffer-read-only all-records start)
      (erase-buffer)
      (bbdb-debug (setq all-records (bbdb-records)))
      (dolist (record records)
        (bbdb-debug (unless (memq (car record) all-records)
                      (error "Record %s does not exist" (car record))))
        (setq start (set-marker (nth 2 record) (point)))
        (bbdb-display-record (nth 0 record) (nth 1 record) record-number)
        (setq record-number (1+ record-number)))

      (run-hooks 'bbdb-display-hook))

    (unless (or bbdb-silent-internal bbdb-silent)
      (message "Formatting...done."))

    ;; Put point on first new record in *BBDB* buffer.
    (let ((point (nth 2 (assq first-new bbdb-records)))
          (window (get-buffer-window (current-buffer))))
      (when point ; nil for empty buffer
        (goto-char point)
        (if window (set-window-start window point))))

    (set-buffer-modified-p nil)
    (set-buffer buffer)))

(defun bbdb-undisplay-records ()
  "Undisplay records in `bbdb-buffer-name'."
  (let ((buffer (get-buffer bbdb-buffer-name)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (let (buffer-read-only)
            (erase-buffer))
          (setq bbdb-records nil)
          (set-buffer-modified-p nil)))))

(defun bbdb-redisplay-record (record &optional delete-p)
  "Redisplay one RECORD.
Append RECORD to currently displayed records if necessary.
The *BBDB* buffer must be current when this is called."
  ;; For deletion in the *BBDB* buffer we use the full information
  ;; about the record in the database. Therefore, we need to delete
  ;; the record in the *BBDB* buffer before deleting the record in
  ;; the database.
  (let ((full-record (assq record bbdb-records)))
    (if (null full-record) ; new record
        (bbdb-display-records (list record) nil t)
      (let ((marker (nth 2 full-record))
            (end-marker (nth 2 (car (cdr (memq full-record bbdb-records)))))
            buffer-read-only record-number)
        ;; If point is inside record, put it at the beginning of the record.
        (if (and (<= marker (point))
                 (< (point) (or end-marker (point-max))))
            (goto-char marker))
        (save-excursion
          (goto-char marker)
          (setq record-number (get-text-property (point) 'bbdb-record-number))
          (unless delete-p
            ;; First insert the reformatted record, then delete the old one,
            ;; so that the marker of this record cannot collapse with the
            ;; marker of the subsequent record
            (bbdb-display-record (car full-record) (nth 1 full-record)
                                 record-number))
          (delete-region (point) (or end-marker (point-max)))
          ;; If we deleted a record we need to update the subsequent
          ;; record numbers.
          (if delete-p
              (let* ((markers (append (mapcar (lambda (x) (nth 2 x))
                                              (cdr (memq full-record bbdb-records)))
                                      (list (point-max))))
                     (start (pop markers)))
                (dolist (end markers)
                  (put-text-property start end
                                     'bbdb-record-number record-number)
                  (setq start end
                        record-number (1+ record-number)))))
          (run-hooks 'bbdb-display-hook))))))

(defun bbdb-redisplay-records ()
  "Redisplays the contents of the *BBDB* buffer, without scrolling.
Use this command if multiple records have changed.
Otherwise use `bbdb-redisplay-record'.
The *BBDB* buffer must be current when this is called."
  (let ((point (point))
        (mark (mark t)))
    (goto-char (window-start))
    (let ((p2 (point)))
      (bbdb-display-records-internal bbdb-records)
      (goto-char p2)
      (if mark (set-mark mark)))
    (recenter 0)
    (goto-char point)
    (run-hooks 'bbdb-display-hook)))

(defun bbdb-maybe-update-display (record)
  "If RECORD is currently displayed update display."
  (let ((buffer (get-buffer bbdb-buffer-name)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (memq record (bbdb-records))
              (let ((window (get-buffer-window bbdb-buffer-name)))
                (if window
                    (with-selected-window window
                      (bbdb-redisplay-record record))
                  (bbdb-redisplay-record record))))))))



;;; window configuration hackery
(defun bbdb-pop-up-window (&optional select horiz-p)
  "Find the largest window on the screen, and split it, displaying the
*BBDB* buffer in the bottom `bbdb-pop-up-window-size' lines (unless
the *BBDB* buffer is already visible, in which case do nothing.)
Select this window if SELECT is non-nil.

If `bbdb-message-pop-up' is 'horiz, and the first window matching
HORIZ-P is sufficiently wide (> 112 columns) then the window
will be split vertically rather than horizontally."
  (cond (;; We already have a BBDB window so that nothing needs to be done
         (get-buffer-window bbdb-buffer-name))

        ;; try horizontal split
        ((and (eq bbdb-message-pop-up 'horiz)
              horiz-p
              (>= (frame-width) (car bbdb-horiz-pop-up-window-size))
              (let ((cbuffer (current-buffer))
                    (window-list (window-list))
                    (selected-window (selected-window))
                    (b-width (cdr bbdb-horiz-pop-up-window-size))
                    (search t) window)
                (while (and (setq window (pop window-list))
                            (setq search (funcall horiz-p window))))
                (unless (or search (<= (window-width window)
                                       (car bbdb-horiz-pop-up-window-size)))
                  (select-window window)
                  (condition-case nil ; `split-window-horizontally' might fail
                      (progn
                        (split-window-horizontally
                         (if (integerp b-width)
                             (- (window-width window) b-width)
                           (round (* (- 1 b-width) (window-width window)))))
                        (select-window (next-window window))
                        (let (pop-up-windows)
                          (switch-to-buffer (get-buffer-create bbdb-buffer-name)))
                        (unless select
                          (select-window selected-window)
                          (set-buffer cbuffer))
                        t)
                    (error nil))))))

        (t ;; vertical split
         (let* ((cbuffer (current-buffer))
                (selected-window (selected-window))
                (tallest-window selected-window))
           ;; find the tallest window...
           (dolist (window (window-list))
             (if (> (window-height window) (window-height tallest-window))
                 (setq tallest-window window)))
           (select-window tallest-window)   ; select it and split it...
           (if (eql bbdb-pop-up-window-size 1.0)
               ;; select `bbdb-buffer-name'
               (switch-to-buffer (get-buffer-create bbdb-buffer-name))
             (condition-case nil ; `split-window' might fail
                 (progn
                   (split-window
                    tallest-window
                    (if (integerp bbdb-pop-up-window-size)
                        (- (window-height tallest-window) 1 ; for mode line
                           (max window-min-height bbdb-pop-up-window-size))
                      (round (* bbdb-pop-up-window-size
                                (window-height tallest-window)))))
                   (if (memq major-mode
                             '(gnus-Group-mode gnus-Subject-mode gnus-Article-mode))
                       (goto-char (point-min)))  ; make gnus happy...
                   (select-window (next-window)) ; goto the bottom of the two...
                   (let (pop-up-windows)         ; make it display *BBDB*...
                     (switch-to-buffer (get-buffer-create bbdb-buffer-name))))
               (error (pop-to-buffer (get-buffer-create bbdb-buffer-name))))
             (unless select
               (select-window selected-window) ; original window we were in
               (set-buffer cbuffer)))))))

;;; Electric display stuff

(defun bbdb-electric-display-records (records)
  (require 'electric)
  (let (bbdb-electric-execute)   ; Alert! `bbdb-electric-throw' sets this!
    (let ((bbdb-inside-electric-display t)
          buffer bbdb-electric-done) ; Alert! `bbdb-electric-throw' sets this!
      (save-excursion
        (save-window-excursion
          (save-window-excursion (bbdb-display-records-internal records))
          (setq buffer (window-buffer (Electric-pop-up-window bbdb-buffer-name)))
          (set-buffer buffer)
          (unless bbdb-silent-internal (message "Press Space to bury BBDB list"))
          (catch 'done
            (while t
              (catch 'Blow-off-the-error
                (setq bbdb-electric-done nil)
                (unwind-protect
                    (progn
                      (catch 'electric-bbdb-list-select
                        (Electric-command-loop 'electric-bbdb-list-select
                                               "-> " t))
                      (setq bbdb-electric-done t))
                  ;; protected
                  (if bbdb-electric-done
                      (throw 'done t)
                    (ding)
                    (message "BBDB-Quit")
                    (throw 'Blow-off-the-error t))))))
          (bury-buffer buffer))))
    ;; quit the electric command loop
    (message " ")
    (if bbdb-electric-execute
        (eval bbdb-electric-execute)))
  nil)

(defun bbdb-electric-throw (form)
  "Exit the `electric-command-loop' and evaluate the given form."
  ;; Hack alert!  These variables are bound only within the scope of
  ;; `bbdb-electric-display-records'!
  (unless (boundp 'bbdb-electric-execute)
    (error "BBDB: Electrical short"))
  (setq bbdb-electric-execute form
        bbdb-electric-done t)
  (throw 'electric-bbdb-list-select t))

(defun bbdb-electric-done ()
  (interactive)
  (throw 'electric-bbdb-list-select t))

(defun bbdb-quit-window ()
  (interactive)
  (if bbdb-inside-electric-display
      (bbdb-electric-done)
    (quit-window)))


;;; Reading the BBDB

;;;###autoload
(defun bbdb-records ()
  "Return a list of all BBDB records; read in and parse the db if necessary.
This function also notices if the disk file has been modified."
  (let ((buf (bbdb-buffer)))
    (with-current-buffer buf
      ;; Auto-save file is newer than buffer file
      (when (and bbdb-check-auto-save-file
                 (file-newer-than-file-p (make-auto-save-file-name)
                                         buffer-file-name))
        (if (yes-or-no-p "BBDB auto-save file is newer; recover it? ")
            (progn
              (recover-file buffer-file-name)
              (bury-buffer (current-buffer)) ; `recover-file' selects it
              (auto-save-mode 1) ; turn auto-save back on
              (condition-case nil
                  (delete-file (make-auto-save-file-name))
                (file-error nil)))
          ;; delete auto-save anyway, so we do not keep asking.
          (condition-case nil
              (delete-file (make-auto-save-file-name))
            (file-error nil))))

      ;; Make sure the BBDB in memory is not out of sync with disk.
      (cond ((verify-visited-file-modtime buf))
            ((bbdb-revert-buffer))
            ;; This is the case where `bbdb-file' has changed; the buffer
            ;; has changed as well; and the user has answered "no" to the
            ;; "flush your changes and revert" question.  The only other
            ;; alternative is to save the file right now.  If they answer
            ;; no to the following question, they will be asked the
            ;; preceeding question again and again some large (but finite)
            ;; number of times.  `bbdb-records' is called a lot, you see...
            ((buffer-modified-p buf)
             ;; this prompts
             (bbdb-save t t))
            (t ; Buffer and file are inconsistent, but we let them stay that way
             (message "Continuing with inconsistent BBDB buffers")))

      ;; `bbdb-revert-buffer' kills all local variables.
      (unless (assq 'bbdb-records (buffer-local-variables))
        ;; We are reading / reverting `bbdb-buffer'.
        (set (make-local-variable 'revert-buffer-function)
             'bbdb-revert-buffer)

        (setq buffer-file-coding-system bbdb-file-coding-system
              buffer-read-only bbdb-read-only)
        ;; `bbdb-before-save-hook' and `bbdb-after-save-hook' are user variables.
        ;; To avoid confusion, we hide the hook functions `bbdb-before-save'
        ;; and `bbdb-after-save' from the user as these are essential for BBDB.
        (dolist (hook (cons 'bbdb-before-save bbdb-before-save-hook))
          (add-hook 'before-save-hook hook nil t))
        (dolist (hook (cons 'bbdb-after-save bbdb-after-save-hook))
          (add-hook 'after-save-hook hook nil t))

        (setq bbdb-end-marker nil
              bbdb-changed-records nil
              bbdb-modified nil)

        ;; Flush all caches
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (if bbdb-message-cache
                (setq bbdb-message-cache nil))))
        (fillarray bbdb-hashtable 0)
        (setq bbdb-mail-aliases-need-rebuilt 'parse)

        (if (/= (point-min) (point-max))
            (bbdb-parse-internal) ; normal case
          ;; Empty db: the following does not require `insert-before-markers'
          ;; because there are no db-markers in this buffer.
          (insert (format (concat ";; -*- mode: Emacs-Lisp; coding: %s; -*-"
                                  "\n;;; file-format: %d\n")
                          bbdb-file-coding-system bbdb-file-format))
          (setq bbdb-end-marker (point-marker)))
        (run-hooks 'bbdb-after-read-db-hook))
      ;; return records
      bbdb-records)))

(defun bbdb-revert-buffer (&optional ignore-auto noconfirm)
  "The `revert-buffer-function' for `bbdb-file'.
Return t if the BBDB buffer and file are now consistent
because the reversion was successful (or not needed).
Return nil otherwise."
  (interactive)
  (let ((buf (bbdb-buffer)))
    (with-current-buffer buf
      (cond (;; If nothing has changed do nothing.
             (and (verify-visited-file-modtime buf)
                  (not (buffer-modified-p buf))))
            ((or (and (not (verify-visited-file-modtime buf))
                      ;; File changed on disk
                      (or noconfirm
                          (and bbdb-auto-revert
                               (not (buffer-modified-p buf)))
                          (yes-or-no-p
                           (if (buffer-modified-p buf)
                               "BBDB changed on disk; flush your changes and revert? "
                             "BBDB changed on disk; revert? "))))
                 (and (buffer-modified-p buf)
                      (yes-or-no-p "Flush your changes and revert BBDB? ")))
             (unless (file-exists-p bbdb-file)
               (error "BBDB: file %s no longer exists" bbdb-file))
             (kill-all-local-variables)  ; clear database and caches.
             ;; `revert-buffer-function' has the permanent-local property
             ;; So to avoid looping, we need to bind it to nil explicitly.
             (let (revert-buffer-function)
               (revert-buffer ignore-auto t))
             (bbdb-records)                      ; re-initialize
             (dolist (buffer (buffer-list))
               (with-current-buffer buffer
                 (if (eq major-mode 'bbdb-mode)
                     (bbdb-undisplay-records))))
             t)))))

(defun bbdb-parse-internal ()
  (unless bbdb-silent (message "Parsing BBDB..."))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; Go to where first record begins.  Move to end of file if no records.
      (if (search-forward "\n[" nil 'move)
          (forward-char -1))
      ;; look backwards for file-format, and convert if necessary.
      (let ((file-format (save-excursion
                           (if (re-search-backward
                                "^;+[ \t]*file-\\(format\\|version\\):[ \t]*\\([0-9]+\\)[ \t]*$" nil t)
                               (string-to-number (match-string 2)))))
            records)
        (unless file-format ; current file-format, but no file-format: line.
          (error "BBDB corrupted: no file-format line"))
        (if (> file-format bbdb-file-format)
            (error "BBDB version %s does not understand file format %s."
                   bbdb-version file-format))

        (or (eobp) (looking-at "\\[")
            (error "BBDB corrupted: no following bracket"))

        ;; narrow the buffer to skip over the rubbish before the first record.
        (narrow-to-region (point) (point-max))
        (let ((modp (buffer-modified-p))
              ;; Make sure those parens get cleaned up.
              ;; This code had better stay simple!
              (inhibit-quit t)
              buffer-read-only)
          (goto-char (point-min)) (insert "(\n")
          (goto-char (point-max)) (insert "\n)")
          (goto-char (point-min))
          (setq records (read (current-buffer)))
          (goto-char (point-min)) (delete-char 2)
          (goto-char (point-max)) (delete-char -2)
          (set-buffer-modified-p modp))
        (widen)

        ;; Migrate if `bbdb-file' is outdated.
        (if (= file-format bbdb-file-format)
            (bbdb-parse-frobnicate records)
          (bbdb-parse-frobnicate (bbdb-migrate records file-format))
          (dolist (record bbdb-records)
            (bbdb-overwrite-record-internal record))
          ;; update file format
          (goto-char (point-min))
          (if (re-search-forward (format "^;;; file-\\(version\\|format\\): %d$"
                                         file-format) nil t)
              (replace-match (format ";;; file-format: %d" bbdb-file-format))))

        (unless bbdb-silent (message "Parsing BBDB...done"))))))

(defun bbdb-goto-first-record ()
  "Go to where first record begins,  Move to end of file if no records."
  (goto-char (point-min))
  (unless (looking-at "\\[")
    (search-forward "\n[" nil 'move)
    (forward-char -1)))

(defun bbdb-parse-frobnicate (records)
  ;; now we have to come up with a marker for each record.  Rather than
  ;; calling read for each record, we read them at once (already done) and
  ;; assume that the markers are at each newline.  If this is not the case,
  ;; things can go *very* wrong.
  (bbdb-goto-first-record)
  (setq bbdb-records records)

  (setq bbdb-phone-label-list (bbdb-label-completion-list 'phone)
        bbdb-address-label-list (bbdb-label-completion-list 'address)
        bbdb-notes-label-list nil)
  (let (record label name)
    (while (setq record (pop records))
      (bbdb-cache-set-marker
       (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
       (point-marker))
      (forward-line 1)

      ;; frob the label completion lists
      (dolist (phone (bbdb-record-phone record))
        (unless (member (setq label (bbdb-phone-label phone))
                        bbdb-phone-label-list)
          (push label bbdb-phone-label-list)))
      (dolist (address (bbdb-record-address record))
        (unless (member (setq label (bbdb-address-label address))
                        bbdb-address-label-list)
          (push label bbdb-address-label-list)))
      (dolist (note (bbdb-record-notes record))
        (unless (memq (setq label (car note)) bbdb-notes-label-list)
          (push label bbdb-notes-label-list)))

      (setq name (bbdb-record-name record))
      (if (and bbdb-no-duplicates name
               (bbdb-gethash name))
          ;; Warn the user that there is a duplicate.
          ;; The duplicate record is kept in the database, but it is
          ;; not hashed.
          (message "Duplicate BBDB record encountered: %s" name)
        (bbdb-hash-record record))

      (bbdb-debug
        (if (and records (not (looking-at "\\[")))
            (error "BBDB corrupted: junk between records at %s" (point))))))
  (dolist (label (bbdb-layout-get-option 'multi-line 'omit))
    (setq bbdb-notes-label-list (delq label bbdb-notes-label-list)))
  ;; `bbdb-end-marker' allows us to have comments at the end of `bbdb-file'
  ;; that are ignored.
  (setq bbdb-end-marker (point-marker)))

(defun bbdb-before-save ()
  "Run before saving `bbdb-file' as buffer-local part of `before-save-hook'."
  (when (and bbdb-file-remote
             (or bbdb-file-remote-save-always
                 (y-or-n-p (format "Save the remote BBDB file %s too? "
                                   bbdb-file-remote))))
    ;; Write the current buffer `bbdb-file' into `bbdb-file-remote'.
    (let ((coding-system-for-write bbdb-file-coding-system))
      (write-region (point-min) (point-max) bbdb-file-remote))))

(defun bbdb-after-save ()
  "Run after saving `bbdb-file' as buffer-local part of `after-save-hook'."
  (setq bbdb-modified nil
        bbdb-changed-records nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (eq major-mode 'bbdb-mode)
          (set-buffer-modified-p nil)))))

(defun bbdb-change-record (record &optional need-to-sort new)
  "Update the database after a change of RECORD.
NEED-TO-SORT is t when the name has changed.  You still need to worry
about updating the name hash-table.  If NEW is t treat RECORD as new."
  (or bbdb-notice-hook-pending
      (run-hook-with-args 'bbdb-change-hook record))
  (bbdb-debug (if (bbdb-record-deleted-p record)
                  (error "BBDB: changing deleted record")))
  ;; Do the changing.
  ;; The call of `bbdb-records' checks file synchronization.
  ;; If RECORD refers to an existing record that has been changed,
  ;; yet in the meanwhile we reverted the BBDB file, then RECORD
  ;; no longer refers to a record in `bbdb-records'.  So we are stuck!
  ;; All changes will be lost.
  (cond ((memq record (bbdb-records))
         (if (not need-to-sort) ;; If we do not need to sort, overwrite it.
             (bbdb-overwrite-record-internal record)
           ;; Since we need to sort, delete then insert
           (bbdb-delete-record-internal record)
           (bbdb-insert-record-internal record)))
        ((not new)
         (error "Changes are lost."))
        (t ;; Record is not in database so add it.
         (bbdb-insert-record-internal record)))
  (run-hook-with-args 'bbdb-after-change-hook record)
  record)

(defun bbdb-delete-record-internal (record)
  "Delete RECORD in the database file."
  (unless (bbdb-record-marker record) (error "BBDB: marker absent"))
  (bbdb-with-db-buffer
    (unless (or bbdb-suppress-changed-records-recording
                (memq record bbdb-changed-records))
      (push record bbdb-changed-records))
    (let ((tail (memq record bbdb-records)))
      (unless tail (error "BBDB record absent: %s" record))
      (delete-region (bbdb-record-marker record)
                     (if (cdr tail)
                         (bbdb-record-marker (car (cdr tail)))
                       bbdb-end-marker)))
    (setq bbdb-records (delq record bbdb-records))
    (let ((name (bbdb-record-name record)))
      (if (> (length name) 0)
          (bbdb-remhash name record)))
    (dolist (organization (bbdb-record-organization record))
      (bbdb-remhash organization record))
    (dolist (mail (bbdb-record-mail record))
      (bbdb-remhash mail record))
    (dolist (aka (bbdb-record-aka record))
      (bbdb-remhash aka record))
    (bbdb-record-set-sortkey record nil)
    (setq bbdb-modified t)))

(defun bbdb-insert-record-internal (record)
  "Insert RECORD into the database file.
Do not call this function directly, call instead `bbdb-change-record'
that calls the hooks, too."
  (unless (bbdb-record-marker record)
    (bbdb-record-set-marker record (make-marker)))
  (bbdb-with-db-buffer
    (unless (or bbdb-suppress-changed-records-recording
                (memq record bbdb-changed-records))
      (push record bbdb-changed-records))
    ;; Set the sortkey to nil so that it will automatically be recalculated
    ;; up-to-date for sorting
    (bbdb-record-set-sortkey record nil)
    ;; splice record into `bbdb-records'
    (bbdb-debug (if (memq record bbdb-records)
                    (error "BBDB record not unique: - %s" record)))
    (if (or (not bbdb-records) ; first record in new database
            (bbdb-record-lessp record (car bbdb-records)))
        (push record bbdb-records)
      (let ((records bbdb-records))
        (while (and (cdr records)
                    (bbdb-record-lessp (nth 1 records) record))
          (setq records (cdr records)))
        (setcdr records (cons record (cdr records)))))

    (let ((next (car (cdr (memq record bbdb-records)))))
      (goto-char (if next
                     (bbdb-record-marker next)
                   bbdb-end-marker)))
    ;; Before printing the record, remove the cache (we do not want that
    ;; written to the file.)  After writing, put the cache back and update
    ;; the cache's marker.
    (let ((cache (bbdb-record-cache record))
          (print-escape-newlines t)
          (point (point)))
      (bbdb-debug
        (if (= point (point-min))
            (error "Inserting at point-min (%s)" point))
        (if (and (/= point bbdb-end-marker)
                 (not (looking-at "^\\[")))
            (error "Not inserting before a record (%s)" point)))
      (bbdb-record-set-cache record nil)
      (insert-before-markers (prin1-to-string record) "\n")
      (set-marker (bbdb-cache-marker cache) point)
      (bbdb-record-set-cache record cache)
      (bbdb-hash-record record))
    (setq bbdb-modified t)
    record))

(defun bbdb-overwrite-record-internal (record)
  "Overwrite RECORD in the database file.
Do not call this function directly, call instead `bbdb-change-record'
that calls the hooks, too."
  (bbdb-with-db-buffer
    (unless (or bbdb-suppress-changed-records-recording
                (memq record bbdb-changed-records))
      (push record bbdb-changed-records))
    (let* ((print-escape-newlines t)
           (tail (memq record bbdb-records))
           (_ (unless tail (error "BBDB record absent: %s" record)))
           (cache (bbdb-record-cache record)))
      (bbdb-debug
        (if (<= (bbdb-cache-marker cache) (point-min))
            (error "Cache marker is %s" (bbdb-cache-marker cache))))
      (goto-char (bbdb-cache-marker cache))
      (bbdb-debug
        (if (and (/= (point) bbdb-end-marker)
                 (not (looking-at "\\[")))
            (error "Not inserting before a record (%s)" (point))))

      (bbdb-record-set-cache record nil)
      (insert (prin1-to-string record) "\n")
      (delete-region (point)
                     (if (cdr tail)
                         (bbdb-record-marker (car (cdr tail)))
                       bbdb-end-marker))
      (bbdb-record-set-cache record cache)

      (bbdb-debug
        (if (<= (if (cdr tail)
                    (bbdb-record-marker (car (cdr tail)))
                  bbdb-end-marker)
                (bbdb-record-marker record))
            (error "Overwrite failed")))

      (setq bbdb-modified t)
      record)))

(defun bbdb-set-notes-labels (newval)
  "Set `bbdb-notes-label-list'.
If NEWVAL is a symbol, add it to `bbdb-notes-label-list' if not yet present.
If NEWVAL is a list, it replaces the current value of `bbdb-notes-label-list'."
  (cond ((listp newval)
         (setq bbdb-notes-label-list newval))
        ((and (symbolp newval)
              (not (memq newval bbdb-notes-label-list)))
         (push newval bbdb-notes-label-list)))
  bbdb-notes-label-list)


;;; BBDB mode

;;;###autoload
(define-derived-mode bbdb-mode special-mode "BBDB"
  "Major mode for viewing and editing the Insidious Big Brother Database.
Letters no longer insert themselves.  Numbers are prefix arguments.
You can move around using the usual cursor motion commands.
\\<bbdb-mode-map>
\\[bbdb-add-mail-alias]\t Add new mail alias to visible records or \
remove it.
\\[bbdb-edit-field]\t Edit the field on the current line.
\\[bbdb-delete-field-or-record]\t Delete the field on the \
current line.  If the current line is the\n\t first line of a record, then \
delete the entire record.
\\[bbdb-insert-field]\t Insert a new field into the current record.  \
Note that this\n\t will let you add new fields of your own as well.
\\[bbdb-transpose-fields]\t Swap the field on the current line with the \
previous field.
\\[bbdb-dial]\t Dial the current phone field.
\\[bbdb-next-record], \\[bbdb-prev-record]\t Move to the next or the previous \
displayed record, respectively.
\\[bbdb-create]\t Create a new record.
\\[bbdb-toggle-records-layout]\t Toggle whether the current record is displayed in a \
one-line\n\t listing, or a full multi-line listing.
\\[bbdb-do-all-records]\\[bbdb-toggle-records-layout]\t Do that \
for all displayed records.
\\[bbdb-merge-records]\t Merge the contents of the current record with \
some other, and then\n\t delete the current record.
\\[bbdb-omit-record]\t Remove the current record from the display without \
deleting it from\n\t the database.  This is often a useful thing to do \
before using one\n\t of the `*' commands.
\\[bbdb]\t Search for records in the database (on all fields).
\\[bbdb-search-mail]\t Search for records by mail address.
\\[bbdb-search-organization]\t Search for records by organization.
\\[bbdb-search-notes]\t Search for records by note.
\\[bbdb-search-name]\t Search for records by name.
\\[bbdb-search-changed]\t Display records that have changed since the database \
was saved.
\\[bbdb-mail]\t Compose mail to the person represented by the \
current record.
\\[bbdb-do-all-records]\\[bbdb-mail]\t Compose mail \
to everyone whose record is displayed.
\\[bbdb-save]\t Save the BBDB file to disk.
\\[bbdb-print]\t Create a TeX file containing a pretty-printed version \
of all the\n\t records in the database.
\\[bbdb-do-all-records]\\[bbdb-print]\t Do that for the \
displayed records only.
\\[other-window]\t Move to another window.
\\[bbdb-info]\t Read the Info documentation for BBDB.
\\[bbdb-help]\t Display a one line command summary in the echo area.
\\[bbdb-browse-url]\t Visit Web sites listed in the `url' field(s) of the current \
record.

For address completion using the names and mail addresses in the database:
\t in Sendmail mode, type \\<mail-mode-map>\\[bbdb-complete-mail].
\t in Message mode, type \\<message-mode-map>\\[bbdb-complete-mail].

Important variables:
\t `bbdb-add-mails'
\t `bbdb-auto-revert'
\t `bbdb-canonicalize-redundant-mails'
\t `bbdb-case-fold-search'
\t `bbdb-completion-list'
\t `bbdb-default-area-code'
\t `bbdb-default-domain'
\t `bbdb-electric'
\t `bbdb-layout'
\t `bbdb-file'
\t `bbdb-message-caching'
\t `bbdb-new-mails-always-primary'
\t `bbdb-phone-style'
\t `bbdb-check-auto-save-file'
\t `bbdb-pop-up-layout'
\t `bbdb-pop-up-window-size'
\t `bbdb-accept-name-mismatch'
\t `bbdb-read-only'
\t `bbdb-use-alternate-names'
\t `bbdb-message-pop-up'
\t `bbdb-user-mail-address-re'

There are numerous hooks.  M-x apropos ^bbdb.*hook RET

\\{bbdb-mode-map}"
  (setq truncate-lines t
        default-directory (file-name-directory bbdb-file)
        mode-line-buffer-identification
        (list 24 (buffer-name) "  "
              '(:eval (format "%d/%d/%d"
                              (1+ (or (get-text-property
                                       (point) 'bbdb-record-number) -1))
                              (length bbdb-records)
                              ;; This code gets called a lot.
                              ;; So we keep it as simple as possible.
                              (with-current-buffer bbdb-buffer
                                (length bbdb-records))))
              '(:eval (concat "  "
                              (bbdb-concat " " (elt bbdb-modeline-info 0)
                                           (elt bbdb-modeline-info 1)))))
        mode-line-modified
        '(bbdb-read-only (bbdb-modified "%*" "%%")
                         (bbdb-modified "**" "--")))
  ;; `bbdb-revert-buffer' acts on `bbdb-buffer'.  Yet this command is usually
  ;; called from the *BBDB* buffer.
  (set (make-local-variable 'revert-buffer-function)
       'bbdb-revert-buffer)
  (add-hook 'post-command-hook 'force-mode-line-update nil t))



(defun bbdb-sendmail-menu (record)
  (let ((mails (bbdb-record-mail record)))
    (if (cdr mails)
        (cons "Send mail to..."
              (mapcar (lambda (address)
                        (vector address `(bbdb-compose-mail
                                          ,(bbdb-dwim-mail record address))
                                t))
                      mails))
      (vector (concat "Send mail to " (car mails))
              `(bbdb-compose-mail ,(bbdb-dwim-mail record (car mails)))
              t))))

(defun bbdb-field-menu (record field)
  "Menu items specifically for FIELD of RECORD."
  (let ((type (car field)))
    (nconc
     (list
      (concat "Commands for "
              (cond ((eq type 'note)
                     (concat "\"" (symbol-name (car (nth 1 field)))
                             "\" field:"))
                    ((eq type 'name) "Name field:")
                    ((eq type 'degree) "Degree field:")
                    ((eq type 'organization) "Organization field:")
                    ((eq type 'aka) "Alternate Names field:")
                    ((eq type 'mail) "Mail Addresses field:")
                    ((memq type '(address phone))
                     (concat "\"" (aref (nth 1 field) 0) "\" "
                             (capitalize (symbol-name type)) " field:"))))
      ["Edit Field" bbdb-edit-field t])
     (unless (eq type 'name)
       (list ["Delete Field" bbdb-delete-field-or-record t]))
     (cond ((eq type 'phone)
            (list (vector (concat "Dial " (bbdb-phone-string (nth 1 field)))
                          `(bbdb-dial ',field nil) t)))))))

(defun bbdb-insert-field-menu (record)
  "Submenu for inserting a new field for RECORD."
  (cons "Insert New Field..."
        (mapcar
         (lambda (field)
           (if (stringp field) field
             (vector (symbol-name field)
                     `(bbdb-insert-field
                       ,record ',field (bbdb-prompt-for-new-field ',field))
                     (not (or (and (eq field 'degree) (bbdb-record-degree record))
                              (and (eq field 'organization) (bbdb-record-organization record))
                              (and (eq field 'mail) (bbdb-record-mail record))
                              (and (eq field 'aka) (bbdb-record-aka record))
                              (assq field (bbdb-record-notes record)))))))
         (append '(degree organization aka phone address mail)
                 '("--") bbdb-notes-label-list))))

(defun bbdb-mouse-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ((record (bbdb-current-record))
         (field  (bbdb-current-field))
         (menu (if (and record field (functionp bbdb-user-menu-commands))
                   (funcall bbdb-user-menu-commands record field)
                 bbdb-user-menu-commands)))
    (if record
        (popup-menu
         (append
          (list
           (concat "Commands for record \""
                   (bbdb-record-name record) "\":")
           ["Delete Record" bbdb-delete-records t]
           ["Toggle Record Display Layout" bbdb-toggle-records-layout t]
           (if (and (not (eq 'full-multi-line
                             (nth 1 (assq record bbdb-records))))
                    (bbdb-layout-get-option 'multi-line 'omit))
               ["Fully Display Record" bbdb-display-records-completely t])
           ["Omit Record" bbdb-omit-record t]
           ["Merge Record" bbdb-merge-records t])
          (if (bbdb-record-mail record)
              (list (bbdb-sendmail-menu record)))
          (list "--" (bbdb-insert-field-menu record))
          (if field
              (cons "--" (bbdb-field-menu record field)))
          (if menu
              (append ["--"] ["User Defined Commands"] menu)))))))

(defun bbdb-next-record (p)
  "Move point to the first line of the next BBDB record."
  (interactive "p")
  (if (< p 0)
      (bbdb-prev-record (- p))
    (forward-char)
    (dotimes (i p)
      (unless (re-search-forward "^[^ \t\n]" nil t)
        (beginning-of-line)
        (error "no next record")))
    (beginning-of-line)))

(defun bbdb-prev-record (p)
  "Move point to the first line of the previous BBDB record."
  (interactive "p")
  (if (< p 0)
      (bbdb-next-record (- p))
    (dotimes (i p)
      (unless (re-search-backward "^[^ \t\n]" nil t)
        (error "no previous record")))))

(defun bbdb-save (&optional prompt-first noisy)
  "Save the BBDB if it is modified."
  (interactive (list nil t))
  (bbdb-with-db-buffer
    (if (and (buffer-modified-p)
             (or (null prompt-first)
                 (y-or-n-p
                  (if bbdb-read-only
                      "Save the BBDB, even though it is supposedly read-only? "
                    "Save the BBDB now? "))))
        (save-buffer)
      (if noisy (message "(No BBDB changes need to be saved)")))))

;;;###autoload
(defun bbdb-version (&optional arg)
  "Return string describing the version of BBDB.
With non-nil prefix ARG, insert string at point."
  (interactive "P")
  (let ((version-string (format "BBDB version %s (%s)"
                                bbdb-version bbdb-version-date)))
    (cond (arg (insert (message version-string)))
          ((interactive-p)
           (message version-string))
          (t version-string))))



(defun bbdb-sort-records ()
  "Sort BBDB database.
This is not needed when using BBDB itself.  It might be necessary,
however, after having used other programs to add records to the BBDB."
  (interactive)
  (let* ((records (copy-sequence (bbdb-records))))
    (bbdb-with-db-buffer
      (setq bbdb-records (sort bbdb-records 'bbdb-record-lessp))
      (if (equal records bbdb-records)
          (message "BBDB need not be sorted")
        (message "BBDB was mis-sorted; fixing...")
        (bbdb-goto-first-record)
        (delete-region (point) bbdb-end-marker)
        (let ((print-escape-newlines t)
              (standard-output (current-buffer))
              (inhibit-quit t) ; really, don't mess with this
              cache)
          (dolist (record bbdb-records)
            ;; Before printing the record, remove cache (we do not want that
            ;; written to the file.)  Ater writing, put the cache back
            ;; and update the cache's marker.
            (setq cache (bbdb-record-cache record))
            (set-marker (bbdb-cache-marker cache) (point))
            (bbdb-record-set-cache record nil)
            (prin1 record)
            (bbdb-record-set-cache record cache)
            (insert ?\n)))
        (setq bbdb-modified t)
        (message "BBDB was mis-sorted; fixing...done")))))



;;;###autoload
(defun bbdb-initialize (&rest muas)
  "Initialize BBDB for MUAS.
List MUAS may include the following symbols
to initialize the respective mail/news readers and composers:
  gnus       Gnus mail/news reader.
  mh-e       MH-E mail reader.
  rmail      Rmail mail reader.
  vm         VM mail reader.
  mail       Mail (M-x mail).
  message    Message mode.
See also `bbdb-mua-auto-update-init'.  The latter is a separate function
as this allows one to initialize the auto update feature for some MUAs only,
for example only for outgoing messages."
  (require 'bbdb-autoloads)
  (dolist (mua muas)
    (let ((init (assq mua bbdb-init-forms)))
      (if init
          ;; Should we make sure that each insinuation happens only once?
          (eval (cadr init))
        (bbdb-warn "Do not know how to insinuate `%s'" mua))))
  (run-hooks 'bbdb-initialize-hook))


(provide 'bbdb)
