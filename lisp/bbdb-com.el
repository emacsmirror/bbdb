;;; bbdb-com.el --- user-level commands of BBDB

;; Copyright (C) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
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
;;; This file contains most of the user-level interactive commands for BBDB.
;;; See bbdb.texinfo for documentation.

(require 'bbdb)
(require 'mailabbrev)

(eval-and-compile
  (autoload 'build-mail-aliases "mailalias"))

(defun bbdb-get-records (prompt)
  "If inside the *BBDB* buffer get the current records.
In other buffers ask the user."
  (if (string= bbdb-buffer-name (buffer-name))
      (bbdb-do-records)
    (bbdb-completing-read-records prompt)))

;; Note about the arg RECORDS of various BBDB commands:
;;  - Usually, RECORDS is a list of records.  (Interactively,
;;    this list of records is set up by `bbdb-do-records'.)
;;  - If these commands are used, e.g., in `bbdb-create-hook' or
;;    `bbdb-change-hook', they will be called with one arg, a single record.
;; So depending on context the value of RECORDS will be a single record
;; or a list of records, and we want to handle both cases.
;; So we pass RECORDS to `bbdb-record-list' to handle both cases.
(defun bbdb-record-list (records &optional full)
  "Ensure that RECORDS is a list of records.
If RECORDS is a single record turn it into a list.
If FULL is non-nil, assume that RECORDS include display information."
  (if records
      (if full
          (if (vectorp (car records)) (list records) records)
        (if (vectorp records) (list records) records))))

;; Note about BBDB prefix commands:
;; - `bbdb-do-all-records' is a proper prefix command in the sense
;;   that it must immediately precede the main command.
;; - `bbdb-append-display' and `bbdb-search-invert' are fake prefix
;;   commands. They need not precede the main commands.
;;   Also, `bbdb-append-display' can act on multiple commands.

;;;###autoload
(defun bbdb-do-all-records ()
  "Command prefix for operating on all records currently displayed.
This only works for certain commands."
  (interactive)
  (message (substitute-command-keys
            "\\<bbdb-mode-map>\\[bbdb-do-all-records]"))
  (setq prefix-arg current-prefix-arg
        last-command this-command))

;;;###autoload
(defun bbdb-do-records (&optional full)
  "Return list of records to operate on.
Normally this list includes only the current record.
It includes all currently displayed records if the command prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records] is used.
If FULL is non-nil, the list of records includes display information."
  (if (eq last-command 'bbdb-do-all-records)
      (if full bbdb-records (mapcar 'car bbdb-records))
    (list (bbdb-current-record full))))

;;;###autoload
(defun bbdb-append-display-p ()
  "Return variable `bbdb-append-display' and reset."
  (let ((job (cond ((eq t bbdb-append-display))
                   ((numberp bbdb-append-display)
                    (setq bbdb-append-display (1- bbdb-append-display))
                    (if (zerop bbdb-append-display)
                        (setq bbdb-append-display nil))
                    t)
                   (bbdb-append-display
                    (setq bbdb-append-display nil)
                    t))))
    (cond ((numberp bbdb-append-display)
           (aset bbdb-modeline-info 0
                 (format "(add %dx)" bbdb-append-display)))
          ((not bbdb-append-display)
           (aset bbdb-modeline-info 0 nil)))
    job))

;;;###autoload
(defun bbdb-append-display (&optional arg)
  "Toggle appending next searched records in the *BBDB* buffer.
With prefix ARG \\[universal-argument] always append.
With ARG a positive number append for that many times.
With ARG a negative number do not append."
  (interactive "P")
  (setq bbdb-append-display
        (cond ((and arg (listp arg)) t)
              ((and (numberp arg) (< 1 arg)) arg)
              ((or (and (numberp arg) (< arg 0)) bbdb-append-display) nil)
              (t 'once)))
  (aset bbdb-modeline-info 0
        (cond ((numberp bbdb-append-display)
               (format "(add %dx)" bbdb-append-display))
              ((eq t bbdb-append-display) "Add")
              (bbdb-append-display "add")
              (t nil)))
  (aset bbdb-modeline-info 2
        (if bbdb-append-display
            (substitute-command-keys
             "\\<bbdb-mode-map>\\[bbdb-append-display]")))
  (let ((msg (bbdb-concat " " (elt bbdb-modeline-info 2)
                          (elt bbdb-modeline-info 3))))
    (unless (string= "" msg) (message "%s" msg))))

(defsubst bbdb-layout-prefix ()
  "Set the LAYOUT arg interactively using the prefix arg."
  (cond ((eq current-prefix-arg 0) 'one-line)
        (current-prefix-arg 'multi-line)
        (t bbdb-layout)))

(defun bbdb-editable ()
  "Throw an error if BBDB is read-only."
  (if bbdb-read-only
      (error "The Insidious Big Brother Database is read-only.")))

(defun bbdb-search-invert-p ()
  "Return variable `bbdb-search-invert' and set it to nil.
To set it again, use command `bbdb-search-invert'."
  (let ((result bbdb-search-invert))
    (setq bbdb-search-invert nil)
    (aset bbdb-modeline-info 1 nil)
    (aset bbdb-modeline-info 3 nil)
    result))

;;;###autoload
(defun bbdb-search-invert (&optional arg)
  "Toggle inversion of the next search command.
With prefix ARG a positive number, invert next search.
With prefix ARG a negative number, do not invert next search."
  (interactive "P")
  (if (setq bbdb-search-invert
            (or (and (numberp arg) (< 0 arg))
                (and (not (numberp arg)) (not bbdb-search-invert))))
      (progn
        (aset bbdb-modeline-info 1 "inv")
        (aset bbdb-modeline-info 3
              (substitute-command-keys
               "\\<bbdb-mode-map>\\[bbdb-search-invert]")))
    (aset bbdb-modeline-info 1 nil)
    (aset bbdb-modeline-info 3 nil))
  (message "%s" (bbdb-concat " " (elt bbdb-modeline-info 2)
                             (elt bbdb-modeline-info 3))))

(defmacro bbdb-search (records &optional name organization mail notes
                               phone address)
  "Search RECORDS for fields NAME, ORGANIZATION, MAIL, NOTES, PHONE, ADDRESS.
This macro only generates code for those fields actually being searched for;
literal nils at compile-time cause no code to be generated.

To reverse the search, bind variable `bbdb-search-invert' to t."
  (let (clauses)
    ;; I did not protect these vars from multiple evaluation because that
    ;; actually generates *less efficient code* in elisp, because the extra
    ;; bindings cannot easily be optimized away without lexical scope.  fmh.
    (or (stringp name) (symbolp name) (error "name must be atomic"))
    (or (stringp organization) (symbolp organization) (error "organization must be atomic"))
    (or (stringp mail) (symbolp mail) (error "mail must be atomic"))
    (or (stringp notes) (symbolp notes) (consp notes)
        (error "notes must be atomic or cons"))
    (or (stringp phone) (symbolp phone) (error "phone must be atomic"))
    (when name
      (push `(string-match ,name (or (bbdb-record-name record) "")) clauses)
      (push `(let ((akas (bbdb-record-aka record))
                   aka done)
               (while (and (setq aka (pop akas)) (not done))
                 (setq done (string-match ,name aka)))
               done)
            clauses))
    (if organization
        (push `(let ((organizations (bbdb-record-organization record))
                     org done)
                 (if organizations
                     (while (and (setq org (pop organizations)) (not done))
                       (setq done (string-match ,organization org)))
                   ;; so that "^$" can be used to find records that
                   ;; have no organization
                   (setq done (string-match ,organization "")))
                 done)
              clauses))

    (if phone
        (push `(let ((phones (bbdb-record-phone record))
                     ph done)
                 (if phones
                     (while (and (setq ph (pop phones)) (not done))
                       (setq done (string-match ,phone
                                                (bbdb-phone-string ph))))
                   ;; so that "^$" can be used to find records that
                   ;; have no phones
                   (setq done (string-match ,phone "")))
                 done)
              clauses))
    (if address
        (push `(let ((addresses (bbdb-record-address record))
                     a done)
                 (if addresses
                     (while (and (setq a (pop addresses)) (not done))
                       (setq done (string-match ,address
                                                (bbdb-format-address a 2))))
                   ;; so that "^$" can be used to find records that
                   ;; have no addresses.
                   (setq done (string-match ,address "")))
                 done)
              clauses))
    (if mail
        (push `(let ((mails (bbdb-record-mail record))
                     (bbdb-case-fold-search t) ; there is no case for mails
                     m done)
                 (if mails
                     (while (and (setq m (pop mails)) (not done))
                       (setq done (string-match ,mail m)))
                   ;; so that "^$" can be used to find records that
                   ;; have no mail addresses.
                   (setq done (string-match ,mail "")))
                 done)
              clauses))
    (if notes
        (push `(cond ((stringp ,notes)
                      ;; check notes field `notes'
                      (string-match ,notes
                                    (or (bbdb-record-note record 'notes) "")))
                     ((eq (car ,notes) '*)
                      ;; check all notes fields
                      (let ((labels bbdb-notes-label-list) done tmp)
                        (if (bbdb-record-notes record)
                            (while (and (not done) labels)
                              (setq tmp (bbdb-record-note record (car labels))
                                    done (and tmp (string-match (cdr ,notes)
                                                                tmp))
                                    labels (cdr labels)))
                          ;; so that "^$" can be used to find records that
                          ;; have no notes
                          (setq done (string-match (cdr ,notes) "")))
                        done))
                     (t ; check one field
                      (string-match (cdr ,notes)
                                    (or (bbdb-record-note
                                         record (car ,notes)) ""))))
              clauses))
    `(let ((case-fold-search bbdb-case-fold-search)
           (invert (bbdb-search-invert-p))
           matches)
       (dolist (record ,records)
         (unless (eq (not invert) (not (or ,@clauses)))
           (push record matches)))
       (nreverse matches))))

(defun bbdb-search-prompt (&optional field)
  "Read regexp to search FIELD values of records."
  (read-string (format "Search records%s %smatching regexp: "
                       (if field (concat " with " field) "")
                       (if bbdb-search-invert "not " ""))))

;;;###autoload
(defun bbdb (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP
in either the name(s), organization, address, phone, mail, or notes."
  (interactive (list (bbdb-search-prompt) (bbdb-layout-prefix)))
  (let ((records (bbdb-search (bbdb-records) regexp regexp regexp
                              (cons '* regexp) regexp regexp)))
    (if records
        (bbdb-display-records records layout nil t)
      (message "No records matching '%s'" regexp))))

;;;###autoload
(defun bbdb-search-name (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the name
\(or ``alternate'' names\)."
  (interactive (list (bbdb-search-prompt "names") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) regexp) layout))

;;;###autoload
(defun bbdb-search-organization (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the organization field."
  (interactive (list (bbdb-search-prompt "organization") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) nil regexp) layout))

;;;###autoload
(defun bbdb-search-address (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the address fields."
  (interactive (list (bbdb-search-prompt "address") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) nil nil nil nil nil regexp)
                        layout))

;;;###autoload
(defun bbdb-search-mail (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the mail address."
  (interactive (list (bbdb-search-prompt "mail address") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) nil nil regexp) layout))

;;;###autoload
(defun bbdb-search-phone (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the phones field."
  (interactive (list (bbdb-search-prompt "phone") (bbdb-layout-prefix)))
  (bbdb-display-records
   (bbdb-search (bbdb-records) nil nil nil nil regexp) layout))

;;;###autoload
(defun bbdb-search-notes (field regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the notes FIELD."
  (interactive
   (let ((field (completing-read "Notes field to search (RET for all): "
                                 (mapcar 'list bbdb-notes-label-list) nil t)))
     (list field (bbdb-search-prompt (if (string= field "")
                                         "one field"
                                       field))
           (bbdb-layout-prefix))))
  (let ((notes (if (string= field "")
                   (cons '* regexp)
                 (cons (intern field) regexp))))
    (bbdb-display-records (bbdb-search (bbdb-records) nil nil nil notes)
                          layout)))

;;;###autoload
(defun bbdb-search-changed (&optional layout)
  "Display all records in the bbdb database which have changed since
the database was last saved."
  (interactive (list (bbdb-layout-prefix)))
  (if (bbdb-search-invert-p)
      (let (unchanged-records)
        (dolist (record (bbdb-records))
          (unless (memq record bbdb-changed-records)
            (push record unchanged-records)))
        (bbdb-display-records unchanged-records layout))
    (bbdb-display-records bbdb-changed-records layout)))

(defun bbdb-search-prog (function &optional layout)
  "Search records using FUNCTION.
FUNCTION is called with one argument, the record, and should return
the record to be displayed or nil otherwise."
  (bbdb-display-records (delq nil (mapcar function (bbdb-records))) layout))


;; clean-up functions

;; This need not be restricted to mail field.
;; RW: It seems that, as a minimum. one should always use `add-to-list'
;; to avoid the problem which `bbdb-delete-duplicate-mails' is supposed
;; to solve!
(defun bbdb-delete-duplicate-mails (records)
  "Remove duplicate mails from RECORDS.
These duplicates may occur if we feed BBDB automatically.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records)))
  (dolist (record (bbdb-record-list records))
    (let (cmails)
      (dolist (mail (bbdb-record-mail record))
        (add-to-list 'cmails mail))
      (bbdb-record-set-mail record (nreverse cmails)))))

(defun bbdb-search-duplicates (&optional fields)
  "Search all records that have duplicate entries for FIELDS.
The list FIELDS may contain the symbols `name', `mail', and `aka'.
If FIELDS is nil use all these fields.  With prefix, query for FIELDS.
The search results are displayed in the bbdb buffer."
  (interactive (list (if current-prefix-arg
                         (list (intern (completing-read "Field: "
                                                        '("name" "mail" "aka")
                                                        nil t))))))
  (setq fields (or fields '(name mail aka)))
  (let (hash ret)
    (dolist (record (bbdb-records))

      (when (and (memq 'name fields)
                 (bbdb-record-name record)
                 (setq hash (bbdb-gethash (bbdb-record-name record)))
                 (> (length hash) 1))
        (setq ret (append hash ret))
        (message "BBDB record `%s' causes duplicates, maybe it is equal to an organization name."
                 (bbdb-record-name record))
        (sit-for 0))

      (if (memq 'mail fields)
          (dolist (mail (bbdb-record-mail record))
              (setq hash (bbdb-gethash mail))
              (when (> (length hash) 1)
                (setq ret (append hash ret))
                (message "BBDB record `%s' has duplicate mail `%s'."
                         (bbdb-record-name record) mail)
                (sit-for 0))))

      (if (memq 'aka fields)
          (dolist (aka (bbdb-record-aka record))
            (setq hash (bbdb-gethash aka))
            (when (> (length hash) 1)
              (setq ret (append hash ret))
              (message "BBDB record `%s' has duplicate aka `%s'"
                       (bbdb-record-name record) aka)
              (sit-for 0)))))

    (bbdb-display-records (delete-dups ret))))

;;; Time-based functions

(defmacro bbdb-compare-records (cmpval field compare)
  "Builds a lambda comparison function that takes one argument, RECORD.
RECORD is returned if (COMPARE VALUE CMPVAL) is t, where VALUE
is the value of FIELD of RECORD."
  `(lambda (record)
     (let ((val (bbdb-record-note record ,field)))
       (if (and val (,compare val ,cmpval))
           record))))

(defsubst bbdb-string> (a b)
  (not (or (string= a b)
           (string< a b))))

;;;###autoload
(defun bbdb-timestamp-older (date &optional layout)
  "Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Older than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'timestamp string<) layout))

;;;###autoload
(defun bbdb-timestamp-newer (date &optional layout)
  "Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Newer than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'timestamp bbdb-string>) layout))

;;;###autoload
(defun bbdb-creation-older (date &optional layout)
  "Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Older than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'creation-date string<) layout))

;;;###autoload
(defun bbdb-creation-newer (date &optional layout)
  "Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Newer than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'creation-date bbdb-string>) layout))

;;;###autoload
(defun bbdb-creation-no-change (&optional layout)
  "Display records that have the same timestamp and creation-date."
  (interactive (list (bbdb-layout-prefix)))
  (bbdb-search-prog
   (bbdb-compare-records (bbdb-record-note record 'timestamp)
                         'creation-date string=) layout))

;;; Parsing phone numbers
;;; XXX this needs expansion to handle international prefixes properly
;;; i.e. +353-number without discarding the +353 part. Problem being
;;; that this will necessitate yet another change in the database
;;; format for people who are using north american numbers.

(defsubst bbdb-subint (string num)
  "Used for parsing phone numbers."
  (string-to-number (match-string num string)))

(defun bbdb-parse-phone (string &optional style)
  "Parse a phone number from STRING and return a list of integers the form
\(area-code exchange number extension).
This is both lenient and strict in what it will parse - whitespace may
appear (or not) between any of the groups of digits, parentheses around the
area code are optional, as is a dash between the exchange and number, and
a '1' preceeding the area code; but there must be three digits in the area
code and exchange, and four in the number (if they are present).
All of these are unambigously parsable:

  ( 415 ) 555 - 1212 x123   -> (415 555 1212 123)
  (415)555-1212 123         -> (415 555 1212 123)
  (1-415) 555-1212 123      -> (415 555 1212 123)
  1 (415)-555-1212 123      -> (415 555 1212 123)
  555-1212 123              -> (0 555 1212 123)
  555 1212                  -> (0 555 1212 0)
  415 555 1212              -> (415 555 1212 0)
  1 415 555 1212            -> (415 555 1212 0)
  5551212                   -> (0 555 1212 0)
  4155551212                -> (415 555 1212 0)
  4155551212123             -> (415 555 1212 123)
  5551212x123               -> (0 555 1212 123)
  1234                      -> (0 0 0 1234)

Note that \"4151212123\" is ambiguous; it could be interpreted either as
\"(415) 121-2123\" or as \"415-1212 x123\".

Return a list containing four numbers or one string."

  ;; RW: Missing parts of NANP numbers are replaced by zeros.
  ;; Is this always correct?  What about an extension zero?
  ;; Should we use nil instead of zeros?
  (unless style (setq style bbdb-phone-style))
  (let ((area-regexp (concat "(?[ \t]*\\+?1?[ \t]*[-\(]?[ \t]*[-\(]?[ \t]*"
                             "\\([2-9][0-9][0-9]\\)[ \t]*)?[-./ \t]*"))
        (main-regexp (concat "\\([1-9][0-9][0-9]\\)[ \t]*[-.]?[ \t]*"
                             "\\([0-9][0-9][0-9][0-9]\\)[ \t]*"))
        (ext-regexp "x?[ \t]*\\([0-9]+\\)[ \t]*"))
    (cond ((not (eq style 'nanp))
           (list (bbdb-string-trim string)))
          ((string-match ;; (415) 555-1212 x123
            (concat "^[ \t]*" area-regexp main-regexp ext-regexp "$") string)
           (list (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3) (bbdb-subint string 4)))
          ;; (415) 555-1212
          ((string-match (concat "^[ \t]*" area-regexp main-regexp "$") string)
           (list (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3) 0))
          ;; 555-1212 x123
          ((string-match (concat "^[ \t]*" main-regexp ext-regexp "$") string)
           (list 0 (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3)))
          ;; 555-1212
          ((string-match (concat "^[ \t]*" main-regexp "$") string)
           (list 0 (bbdb-subint string 1) (bbdb-subint string 2) 0))
          ;; x123
          ((string-match (concat "^[ \t]*" ext-regexp "$") string)
           (list 0 0 0 (bbdb-subint string 1)))
          ;; We trust the user she knows what she wants
          (t (list (bbdb-string-trim string))))))

(defun bbdb-message-search (name mail)
  "Return list of BBDB records matching NAME and/or MAIL.
First try to find a record matching both NAME and MAIL.
If this fails try to find a record matching MAIL.
If this fails try to find a record matching NAME."
  (bbdb-records)
  (if name (setq name (downcase name)))
  (let (records)
    ;; (1) records matching NAME and MAIL
    (or (and name
             (dolist (record (bbdb-gethash mail) records)
               (if (string= name (downcase (bbdb-record-name record)))
                   (push record records))))
        ;; (2) records matching MAIL
        (bbdb-gethash mail)
        ;; (3) records matching NAME
        (bbdb-gethash name))))

;;; Parsing other things

(defun bbdb-parse-postcode (string)
  "Check whether STRING is a legal postcode.
Do this only if `bbdb-check-postcode' is non-nil."
  (if (and bbdb-check-postcode
           (not (memq t (mapcar (lambda (regexp)
                                  ;; if it matches, (not (not index-of-match)) returns t
                                  (not (not (string-match regexp string))))
                                bbdb-legal-postcodes))))
      (error "not a valid postcode.")
    string))

(defun bbdb-divide-name (string)
  "Divide STRING into a first name and a last name.
Case is ignored.  Return name as (FIRST . LAST)."
  (let ((case-fold-search t)
        first last suffix)
    (if (string-match (concat "[-,. \t/\\]+\\("
                              (regexp-opt bbdb-lastname-suffixes)
                              ;; suffices are complemented by optional `.'.
                              "\\.?\\)\\W*\\'")
                      string)
        (setq suffix (concat " " (match-string 1 string))
              string (substring string 0 (match-beginning 0))))
    (if (string-match (concat "[- \t]*\\(\\(?:"
                              (regexp-opt bbdb-lastname-prefixes)
                              ;; multiple last names concatenated by `-'
                              "[- \t]+\\)?\\(?:\\w+[ \t]*-[ \t]*\\)*\\w+\\)\\'")
                      string)
        (progn
          (setq last (match-string 1 string))
          (unless (zerop (match-beginning 0))
            (setq first (substring string 0 (match-beginning 0)))))
      (setq last (bbdb-string-trim string))) ; strange case
    (cons first (concat last suffix))))

(defun bbdb-read-record ()
  "Prompt for and return a new BBDB record.
Does not insert it into the database or update the hashtables,
but does ensure that there will not be name collisions."
  (bbdb-records)                        ; make sure database is loaded
  (if bbdb-read-only
      (error "The Insidious Big Brother Database is read-only."))
  (let (firstname lastname)
    (bbdb-error-retry
     (progn
       (if current-prefix-arg
           (setq firstname (bbdb-read-string "First Name: ")
                 lastname (bbdb-read-string "Last Name: "))
         (let ((name (bbdb-divide-name (bbdb-read-string "Name: "))))
           (setq firstname (car name)
                 lastname (cdr name))))
       (if (string= firstname "") (setq firstname nil))
       (if (string= lastname "") (setq lastname nil))
       (if (and bbdb-no-duplicates
                (bbdb-gethash (bbdb-concat " " firstname lastname)))
           (error "%s %s is already in the database"
                  (or firstname "") (or lastname "")))))
    (let ((organizations (bbdb-split 'organization
                                     (bbdb-read-string "Organizations: ")))
          ;; mail
          (mail (bbdb-split 'mail (bbdb-read-string "E-Mail Addresses: ")))
          ;; address
          (addresses
           (let (addresses label address)
             (while (not (string= ""
                                  (setq label
                                        (bbdb-read-string
                                         "Snail Mail Address Label [RET when done]: "
                                         nil
                                         (bbdb-label-completion-list
                                          'address)))))
               (setq address (make-vector bbdb-address-length nil))
               (bbdb-record-edit-address address label t)
               (push address addresses))
             (nreverse addresses)))
          ;; phones
          (phones
           (let (phones phone-list label)
             (while (not (string= ""
                                  (setq label
                                        (bbdb-read-string
                                         "Phone Label [RET when done]: " nil
                                         (bbdb-label-completion-list
                                          'phone)))))
               (setq phone-list
                     (bbdb-error-retry
                      (bbdb-parse-phone
                       (read-string "Phone: "
                                    (and (integerp bbdb-default-area-code)
                                         (format "(%03d) "
                                                 bbdb-default-area-code))))))
               (push (apply 'vector label phone-list) phones))
             (nreverse phones)))
          ;; notes
          (notes (bbdb-read-string "Notes: ")))
      (setq notes (unless (string= notes "")
                    `((notes . ,notes))))
      (vector firstname lastname nil nil organizations phones addresses
              mail notes (make-vector bbdb-cache-length nil)))))

;;;###autoload
(defun bbdb-create (record)
  "Add a new RECORD to the bbdb database ; prompts for all relevant info
using the echo area, inserts the new record in BBDB, sorted alphabetically,
and offers to save the BBDB file.  DO NOT call this from a program.  Call
`bbdb-create-internal' instead."
  (interactive (list (bbdb-read-record)))
  (run-hook-with-args 'bbdb-create-hook record)
  (bbdb-change-record record t t)
  (bbdb-display-records (list record)))

(defsubst bbdb-check-type (place predicate)
  (unless (funcall predicate place)
    (signal 'wrong-type-argument (list predicate place))))

(defun bbdb-create-internal (name &optional degree aka organizations mail
                                  phones addresses notes)
  "Adds a record to the database; this function does a fair amount of
error-checking on the passed in values, so it is safe to call this from
other programs.

NAME is a string, the name of the person to add.  An error is signalled
if that name is already in use and `bbdb-no-duplicates' is t.
ORGANIZATIONS is a list of strings.
MAIL is a comma-separated list of mail addresses, or a list of strings.
An error is signalled if that name is already in use.
ADDRESSES is a list of address objects.  An address is a vector of the form
\[\"label\" (\"line1\" \"line2\" ... ) \"City\" \"State\" \"Postcode\" \"Country\"].
PHONES is a list of phone-number objects.  A phone-number is a vector of
the form [\"label\" areacode prefix suffix extension-or-nil]
or [\"label\" \"phone-number\"]
NOTES is an alist associating symbols with strings."
  (bbdb-check-type name 'stringp)
  ;; name
  (setq name (bbdb-divide-name name))
  (let ((firstname (car name))
        (lastname (cdr name)))
    (if (and bbdb-no-duplicates
             (bbdb-gethash (bbdb-concat " " firstname lastname)))
        (error "%s %s is already in the database"
               (or firstname "") (or lastname "")))
    ;; degree
    (if degree (bbdb-check-type degree 'listp))
    ;; aka
    (if aka (bbdb-check-type aka 'listp))
    ;; organizations
    (if organizations (bbdb-check-type organizations 'listp))
    ;; mail addresses
    (if (stringp mail)
        (setq mail (bbdb-split 'mail mail)))
    (if bbdb-no-duplicates
        (dolist (elt mail)
          (if (bbdb-gethash elt)
              (error "%s is already in the database" elt))))
    ;; phone numbers
    (dolist (phone phones)
      (unless (and (vectorp phone)
                   (or (= (length phone) 2)
                       (= (length phone) bbdb-phone-length)))
        (signal 'wrong-type-argument (list 'vectorp phone)))
      (bbdb-check-type (aref phone 0) 'stringp)
      (if (= 2 (length phone))
          (bbdb-check-type (aref phone 1) 'stringp)
        (bbdb-check-type (aref phone 1) 'integerp)
        (bbdb-check-type (aref phone 2) 'integerp)
        (bbdb-check-type (aref phone 3) 'integerp)
        (and (aref phone 4)
             (bbdb-check-type (aref phone 4) 'integerp))
        (if (zerop (aref phone 4))
            (aset phone 4 nil))))
    ;; addresses
    (dolist (address addresses)
      (unless (and (vectorp address)
                   (= (length address) bbdb-address-length))
        (signal 'wrong-type-argument (list 'vectorp address)))
      (bbdb-check-type (aref address 0) 'stringp) ;;; XXX use bbdb-addresses
      (bbdb-check-type (aref address 1) 'listp)
      (bbdb-check-type (aref address 2) 'stringp)
      (bbdb-check-type (aref address 3) 'stringp)
      (bbdb-check-type (aref address 4) 'stringp)
      (bbdb-check-type (aref address 5) 'stringp))
    ;; notes
    (dolist (note notes)
      (bbdb-check-type note 'consp)
      (bbdb-check-type (car note) 'symbolp)
      (bbdb-check-type (cdr note) 'stringp))
    ;; record
    (let ((record
           (vector firstname lastname degree aka organizations phones
                   addresses mail notes
                   (make-vector bbdb-cache-length nil))))
      (run-hook-with-args 'bbdb-create-hook record)
      (bbdb-change-record record t t)
      record)))

;;; bbdb-mode stuff

(defun bbdb-record-get-field (record field)
  (cond ((eq field 'name)     (bbdb-record-name record))
        ((eq field 'degree)   (bbdb-record-degree record))
        ((eq field 'organization)  (bbdb-record-organization record))
        ((eq field 'mail)     (bbdb-record-mail record))
        ((eq field 'aka)      (bbdb-record-aka record))
        ((eq field 'phone)    (bbdb-record-phone record))
        ((eq field 'address)  (bbdb-record-address record))
        ((eq field 'note)     (bbdb-record-notes record))
        (t (error "Unknown field type `%s'" field))))

(defun bbdb-record-set-field (record field value)
  (cond ((eq field 'name)     (error "does not work on names"))
        ((eq field 'degree)   (bbdb-record-set-degree record value))
        ((eq field 'organization)  (bbdb-record-set-organization record value))
        ((eq field 'mail)     (bbdb-record-set-mail record value))
        ((eq field 'aka)      (bbdb-record-set-aka record value))
        ((eq field 'phone)    (bbdb-record-set-phone record value))
        ((eq field 'address)  (bbdb-record-set-address record value))
        ((eq field 'note)     (bbdb-record-set-notes record value))
        (t (error "Unknown field type `%s'" field))))

;;;###autoload
(defun bbdb-insert-field (record field contents)
  "Add a new field to the current record; the field type and contents
are prompted for if not supplied.

If you are inserting a new phone-number field, the phone number style
is controlled via `bbdb-phone-style'.  A prefix C-u inverts the style,

If you are inserting a new mail address, you can have BBDB append a
default domain to any mail address that does not contain one.  Set
`bbdb-default-domain' to a string such as \"mycompany.com\" (or,
depending on your environment, (getenv \"DOMAINNAME\")), and
\"@mycompany.com\" will be appended to an address that is entered as
just a username.  A prefix arg C-u (or a `bbdb-default-domain'
value of \"\", the default) means do not alter the address."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (or (bbdb-current-record)
                      (error "Point not on a record")))
          (list (append bbdb-notes-label-list
                        '(degree organization aka phone address mail)))
          (field "")
          (completion-ignore-case t)
          (present (mapcar 'car (bbdb-record-notes record)))
          init init-f)
     (if (bbdb-record-degree record) (push 'degree present))
     (if (bbdb-record-organization record) (push 'organization present))
     (if (bbdb-record-mail record) (push 'mail present))
     (if (bbdb-record-aka record) (push 'aka present))
     (dolist (field present)
       (setq list (remq field list)))
     (setq list (mapcar 'symbol-name list))
     (while (string= field "")
       (setq field (downcase (completing-read "Insert Field: " list))))
     (if (member (intern field) present)
         (error "Field \"%s\" already exists" field))
     (setq init-f (intern-soft (concat "bbdb-init-" field))
           init   (if (and init-f (functionp init-f))
                      (funcall init-f record) "")
           field (intern field))
     (list record field (bbdb-prompt-for-new-field field init
                                                   current-prefix-arg))))

  (cond (;; degree
         (eq field 'degree)
         (if (bbdb-record-degree record)
             (error "Degree field exists already"))
         (if (stringp contents)
             (setq contents (bbdb-split 'degree contents)))
         (dolist (degree contents)
             (bbdb-puthash degree record))
         (bbdb-record-set-degree record contents))
        ;; organization
        ((eq field 'organization)
         (if (bbdb-record-organization record)
             (error "Organization field exists already"))
         (if (stringp contents)
             (setq contents (bbdb-split 'organization contents)))
         (dolist (organization contents)
             (bbdb-puthash organization record))
         (bbdb-record-set-organization record contents))
        ;; phone
        ((eq field 'phone)
         (bbdb-record-set-phone record
                                 (nconc (bbdb-record-phone record)
                                        (list contents))))
        ;; address
        ((eq field 'address)
         (bbdb-record-set-address record
                                    (nconc (bbdb-record-address record)
                                           (list contents))))
        ;; mail
        ((eq field 'mail)
         (if (bbdb-record-mail record)
             (error "Mail field exists already"))
         (if (stringp contents)
             (setq contents (bbdb-split 'mail contents)))
         ;; first detect any conflicts....
         (if bbdb-no-duplicates
             (dolist (mail contents)
               (let ((old (remq record (bbdb-gethash mail))))
                 (if old
                     (error "Mail address \"%s\" is used by \"%s\""
                            mail (mapconcat 'bbdb-record-name old ", "))))))
         ;; then store.
         (dolist (mail contents)
             (bbdb-puthash mail record))
         (bbdb-record-set-mail record contents))
        ;; AKA
        ((eq field 'aka)
         (if (bbdb-record-aka record)
             (error "Alternate names field exists already"))
         (if (stringp contents)
             (setq contents (bbdb-split 'aka contents)))
         ;; first detect any conflicts....
         (if bbdb-no-duplicates
             (dolist (aka contents)
               (let ((old (remq record (bbdb-gethash aka))))
                 (if old
                     (error "Alternate name \"%s\" is used by \"%s\""
                            aka (mapconcat 'bbdb-record-name old ", "))))))
         ;; then store.
         (dolist (aka contents)
           (bbdb-puthash aka record))
         (bbdb-record-set-aka record contents))
        ;; notes
        ((memq field bbdb-notes-label-list)
         (if (and (consp (bbdb-record-notes record))
                  (assq field (bbdb-record-notes record)))
             (error "Note field \"%s\" already exists" field))
         (bbdb-record-set-note record field contents))
        (t (error "Unknow field type `%s'" field)))
  (bbdb-change-record record)
  (let (bbdb-layout)
    (bbdb-redisplay-record record)))

;; Used only for interactive calls of `bbdb-insert-field'.
(defun bbdb-prompt-for-new-field (field &optional init flag)
  (cond (;; degree
         (eq field 'degree) (bbdb-read-string "Degree: " init))
        ;; organization
        ((eq field 'organization) (bbdb-read-string "Organization: " init))
        ;; mail
        ((eq field 'mail)
         (let ((mail (bbdb-read-string "Mail: " init)))
           (if (string-match "^mailto:" mail)
               (setq mail (substring mail (match-end 0))))
           (if (or (not bbdb-default-domain)
                   current-prefix-arg (string-match "[@%!]" mail))
               mail
             (concat mail "@" bbdb-default-domain))))
        ;; AKA
        ((eq field 'aka) (bbdb-read-string "Alternate Names: " init))
        ;; Phone
        ((eq field 'phone)
         (let ((bbdb-phone-style
                (if current-prefix-arg
                    (if (eq bbdb-phone-style 'nanp) nil 'nanp)
                  bbdb-phone-style)))
           (apply 'vector
                  (bbdb-read-string "Label: " nil
                                    (bbdb-label-completion-list 'phone))
                  (bbdb-error-retry
                   (bbdb-parse-phone
                    (read-string "Phone: "
                                 (and (integerp bbdb-default-area-code)
                                      (format "(%03d) "
                                              bbdb-default-area-code))))))))
        ;; Address
        ((eq field 'address)
         (let ((address (make-vector bbdb-address-length nil)))
           (bbdb-record-edit-address address nil flag)
           address))
        ;; Notes
        ((memq field bbdb-notes-label-list)
         (bbdb-read-string (format "%s: " field) init))
        ;; New note fields
        (t
         (if (y-or-n-p
              (format "\"%s\" is an unknown field name.  Define it? " field))
             (bbdb-set-notes-labels field)
           (error "Aborted"))
         (bbdb-read-string (format "%s: " field) init))))

;;;###autoload
(defun bbdb-edit-field (record field &optional flag)
  "Edit the contents of the BBDB field on the current line.
\(This is only meaningful in the \"*BBDB*\" buffer.)
If point is in the middle of a multi-line field (e.g., address),
then the entire field is edited, not just the current line."
  (interactive
   (save-excursion
     (bbdb-editable)
     ;; when at the end of the line take care of it
     (if (and (eolp) (not (bobp)) (not (bbdb-current-field)))
         (backward-char 1))
     (let ((record (bbdb-current-record))
           (field (bbdb-current-field)))
       (unless field (error "Point not in a field"))
       (list record field))))
  ;;
  (let* ((fname (car field))
         (value (nth 1 field))
         (type (elt value 0))
         bbdb-need-to-sort)
    ;; Some editing commands require re-sorting records
    (cond ((eq fname 'name)     (bbdb-record-edit-name record)) ; possibly
          ((eq fname 'degree)   (bbdb-record-edit-degree record)) ; nil
          ((eq fname 'organization)  (bbdb-record-edit-organization record)) ; possibly
          ((eq fname 'mail)     (bbdb-record-edit-mail record)) ; nil
          ((eq fname 'aka)      (bbdb-record-edit-aka record)) ; nil
          ((eq fname 'phone)    (bbdb-record-edit-phone
                                 (bbdb-record-phone record) value)) ; nil
          ((eq fname 'address)  (bbdb-record-edit-address value nil flag)) ; nil
          ((eq fname 'note)     (bbdb-record-edit-notes record type)) ; nil
          (t (error "Unknown field type `%s'" fname)))

    (bbdb-change-record record bbdb-need-to-sort)
    (bbdb-redisplay-record record)
    (if (and (eq 'note fname)
             (memq type '(mail-alias mail)))
        (setq bbdb-mail-aliases-need-rebuilt 'edit))))

(defun bbdb-record-edit-name (record)
  (let (fn ln new-name old-name)
    (bbdb-error-retry
     (progn
       (if current-prefix-arg
           (setq fn (bbdb-read-string "First Name: "
                                      (bbdb-record-firstname record))
                 ln (bbdb-read-string "Last Name: "
                                      (bbdb-record-lastname record)))
         (let ((name (bbdb-divide-name
                      (bbdb-read-string "Name: "
                                        (bbdb-record-name record)))))
           (setq fn (car name)
                 ln (cdr name))))
       (if (string= "" fn) (setq fn nil))
       (if (string= "" ln) (setq ln nil))
       ;; check for collisions
       (setq new-name (if (and fn ln) (concat fn " " ln)
                        (or fn ln))
             old-name (bbdb-record-name record))
       (if (and bbdb-no-duplicates
                new-name
                (not (bbdb-string= new-name old-name))
                (bbdb-gethash new-name))
           (error "%s is already in BBDB" new-name))))
    (setq bbdb-need-to-sort
          (or (not (string= fn
                            (or (bbdb-record-firstname record) "")))
              (not (string= ln
                            (or (bbdb-record-lastname record) "")))))
    ;;
    (bbdb-record-unset-name record)       ; delete old cache entry
    (bbdb-record-set-name record fn ln)))

(defun bbdb-record-edit-degree (record)
  (bbdb-record-set-degree record
                          (bbdb-split  'degree (bbdb-read-string "Degree: "
                          (bbdb-concat 'degree (bbdb-record-degree record))))))

(defun bbdb-record-edit-organization (record)
  (let ((org (bbdb-split  'organization (bbdb-read-string "Organization: "
             (bbdb-concat 'organization (bbdb-record-organization record))))))

    (setq bbdb-need-to-sort
          (not (equal (mapconcat 'downcase org "")
                      (mapconcat 'downcase (bbdb-record-organization record) ""))))

    ;; delete the old hash entry
    (dolist (organization (bbdb-record-organization record))
      (bbdb-remhash organization record))

    (bbdb-record-set-organization record org)
    ;; add a new hash entry
    (dolist (organization org)
      (bbdb-puthash organization record))))

(defun bbdb-record-edit-address (address &optional label default)
  "Edit ADDRESS.
If LABEL is nil, edit the label sub-field of the address as well.
If the country field of ADDRESS is set, use the matching rule from
`bbdb-address-format-list'.  Otherwise use the default rule according
to `bbdb-address-format-list'."
  (unless label
    (setq label (bbdb-read-string "Label: "
                                  (bbdb-address-label address)
                                  (bbdb-label-completion-list
                                   'address))))
  (let ((country (or (bbdb-address-country address) ""))
        new-addr edit)
    (unless (or default (string= "" country))
      (let ((list bbdb-address-format-list)
            identifier elt)
        (while (and (not edit) (setq elt (pop list)))
          (setq identifier (car elt))
          (if (or (and (listp identifier)
                       (member-ignore-case country identifier))
                  (and (functionp identifier)
                       (funcall identifier address)))
              (setq edit (nth 1 elt))))))
    (unless edit
      (setq edit (nth 1 (assq t bbdb-address-format-list))))
    (unless edit (error "No address editing function defined"))
    (if (functionp edit)
        (setq new-addr (funcall edit address))
      (setq new-addr (make-vector 5 ""))
      (dolist (elt (append edit nil))
        (cond ((eq elt ?s)
               (aset new-addr 0 (bbdb-edit-address-street
                                 (bbdb-address-streets address))))
              ((eq elt ?c)
               (aset new-addr 1 (bbdb-read-string
                              "City: " (bbdb-address-city address))))
              ((eq elt ?S)
               (aset new-addr 2 (bbdb-read-string
                              "State: " (bbdb-address-state address))))
              ((eq elt ?p)
               (aset new-addr 3
                     (bbdb-error-retry
                      (bbdb-parse-postcode
                       (bbdb-read-string
                        "Postcode: " (bbdb-address-postcode address))))))
              ((eq elt ?C)
               (aset new-addr 4
                     (bbdb-read-string
                      "Country: " (or (bbdb-address-country address)
                                      bbdb-default-country)))))))
    (bbdb-address-set-label address label)
    (bbdb-address-set-streets address (elt new-addr 0))
    (bbdb-address-set-city address (elt new-addr 1))
    (bbdb-address-set-state address (elt new-addr 2))
    (bbdb-address-set-postcode address (elt new-addr 3))
    (if (string= "" (bbdb-concat "" (elt new-addr 0) (elt new-addr 1)
                                 (elt new-addr 2) (elt new-addr 3)
                                 (elt new-addr 4)))
        ;; User did not enter anything. this causes a display bug.
        ;; The following is a temporary fix.  Ideally, we would simply discard
        ;; the entire address, but that requires bigger hacking.
        (bbdb-address-set-country address "Emacs")
      (bbdb-address-set-country address (elt new-addr 4)))))

(defun bbdb-edit-address-street (streets)
  "Edit list STREETS."
  (let ((n 0) street list)
    (while (not (string= "" (setq street
                                  (bbdb-read-string
                                   (format "Street, line %d: " (+ 1 n))
                                   (nth n streets)))))
      (push street list)
      (setq n (1+ n)))
    (reverse list)))

;; This function can provide some guidance for writing
;; your own address editing function
(defun bbdb-edit-address-default (address)
  "Function to use for address editing.
The sub-fields and the prompts used are:
Street, line n:  (nth n street)
City:            city
State:           state
Postcode:        postcode
Country:         country"
  (list (bbdb-edit-address-street (bbdb-address-streets address))
        (bbdb-read-string "City: " (bbdb-address-city address))
        (bbdb-read-string "State: " (bbdb-address-state address))
        (bbdb-error-retry
         (bbdb-parse-postcode
          (bbdb-read-string "Postcode: " (bbdb-address-postcode address))))
        (bbdb-read-string "Country: " (or (bbdb-address-country address)
                                          bbdb-default-country))))

(defun bbdb-record-edit-phone (phones phone)
  "For list PHONES edit PHONE number."
  ;; Phone numbers are special.  They are vectors with either
  ;; two or four elements.  We do not know whether after editing PHONE
  ;; we still have a number requiring the same format as PHONE.
  ;; So we take all numbers PHONES of the record so that we can
  ;; replace the element PHONE in PHONES.
  (setcar (memq phone phones)
          (apply 'vector
                 (bbdb-read-string "Label: "
                                   (bbdb-phone-label phone)
                                   (bbdb-label-completion-list 'phone))
                 (bbdb-error-retry
                  (bbdb-parse-phone
                   (read-string "Phone: " (bbdb-phone-string phone)))))))

(defun bbdb-record-edit-mail (record)
  (let ((newmails (bbdb-split 'mail
                    (bbdb-read-string
                     "Mail: " (bbdb-concat 'mail (bbdb-record-mail record))))))
    ;; first check for any conflicts...
    (if bbdb-no-duplicates
        (dolist (mail newmails)
          (let ((old (remq record (bbdb-gethash mail))))
            (if old (error "Mail address \"%s\" is used by \"%s\""
                           mail (mapconcat 'bbdb-record-name old ", "))))))
    ;; then update.
    (dolist (mail (bbdb-record-mail record))
      (bbdb-remhash mail record))
    (dolist (mail newmails)
      (bbdb-puthash mail record))
    (bbdb-record-set-mail record newmails)))

(defun bbdb-record-edit-aka (record)
  (let ((str (bbdb-read-string "AKA: "
                               (bbdb-concat 'aka (bbdb-record-aka record)))))
    (let ((oldaka (bbdb-record-aka record))
          (newaka (bbdb-split 'aka str)))
      ;; first check for any conflicts...
      (if bbdb-no-duplicates
          (dolist (aka newaka)
            (let ((old (remq record (bbdb-gethash aka))))
              (if old (error "Alternate name address \"%s\" is used by \"%s\""
                             aka (mapconcat 'bbdb-record-name old ", "))))))
      ;; then update.
      (dolist (aka oldaka)
        (bbdb-remhash aka record))
      (dolist (aka newaka)
        (bbdb-puthash aka record))
      (bbdb-record-set-aka record newaka))))

;;;###autoload
(defun bbdb-record-edit-notes (record &optional note redisplay)
  (let* ((note-name (if note (symbol-name note)
                      (completing-read (format "Edit notes of %s: "
                                               (bbdb-record-name record))
                                       bbdb-notes-label-list)))
         (note-sym (or note (if (equal "" note-name) 'notes (intern note-name))))
         (string (bbdb-read-string (format "%s: " note-name)
                                   (bbdb-record-note record note-sym))))
    (bbdb-record-set-note record note-sym
                          (if (string= "" string) nil string)))
  ;; Unlike the other editing commands, this one has its own redisplay code
  ;; because it is used by the mail modes.
  (if redisplay
      (with-current-buffer bbdb-buffer-name
        (bbdb-redisplay-record record))))

(defsubst bbdb-field-equal (x y)
  (if (and (consp x) (consp y))
      (and (eq (car x) (car y))
           (eq (car (cdr x)) (car (cdr y)))
           (eq (car (cdr (cdr x))) (car (cdr (cdr y)))))
    (eq x y)))

(defun bbdb-next-field (&optional count)
  (or count (setq count 1))
  (beginning-of-line)
  (let* ((record (bbdb-current-record))
         (field (bbdb-current-field))
         (next-record record)
         (next-field field)
         (signum (if (< count 0) -1 1))
         (i 0))
    (if (< count 0) (setq count (- count)))
    (if field
        (while (and next-field (< i count))
          (while (bbdb-field-equal next-field field)
            (forward-line signum)
            (setq next-record (bbdb-current-record)
                  next-field (bbdb-current-field))
            (or (eq next-record record)
                (setq next-field nil)))
          (setq i (1+ i))
          (setq field next-field)))
    next-field))

;;;###autoload
(defun bbdb-transpose-fields (&optional arg)
  "This is like the `transpose-lines' command, but it is for BBDB fields.
If point is on a field of a BBDB record, that field and the previous
field will be transposed.

With argument ARG, takes previous line and moves it past ARG fields.
With argument 0, interchanges field point is in with field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone-number
fields are listed, but you cannot use it to make an address appear before a
phone number; the order of field types is fixed.\)"
  (interactive "p")
  (bbdb-editable)
  (let ((record (bbdb-current-record))
        moving-field position-after position-before swap-p field list)
    (if (/= arg 0)
        (setq moving-field (or (bbdb-next-field -1)
                               (error "No previous field"))
              position-after (bbdb-next-field arg)
              position-before (bbdb-next-field (if (< arg 0) -1 1)))
      ;; if arg is 0, swap fields at point and mark
      (setq swap-p t)
      (setq position-after (bbdb-current-field))
      (save-excursion
        (goto-char (mark))
        (setq moving-field (bbdb-current-field))
        (or (eq record (bbdb-current-record))
            (error "Not in the same record"))))
    (if (< arg 0)
        (let ((x position-after))
          (setq position-after position-before
                position-before x)
          (forward-line 2)))
    (setq field (car moving-field))
    (or position-after position-before
        (error "Transposition would be out of the record"))
    (or (eq field (car position-after))
        (eq field (car position-before))
        (error "Cannot transpose fields of different types (%s and %s)"
               field (if (eq field (car position-after))
                        (car position-before) (car position-after))))
    (or (eq field (car position-after)) (setq position-after nil))
    (or (eq field (car position-before)) (setq position-before nil))
    (setq moving-field (nth 1 moving-field)
          position-after (nth 1 position-after)
          position-before (nth 1 position-before))
    (cond ((memq field '(name aka mail))
           (error "There is only one %s field" field))
          ((memq field '(phone address note))
           (setq list (bbdb-record-get-field record field)))
          (t (error "Unknown field %s" field)))
    (if swap-p
        (let ((rest list))
          (while rest
            (cond ((eq (car rest) moving-field) (setcar rest position-after))
                  ((eq (car rest) position-after) (setcar rest moving-field)))
            (setq rest (cdr rest))))
      (if (eq position-before (car list))
          (setq list (cons moving-field (delq moving-field list)))
        (let ((rest list))
          (while (and rest (not (eq position-after (car rest))))
            (setq rest (cdr rest)))
          (or rest (error "Could not reorder list"))
          (let ((inhibit-quit t))
            (setq list (delq moving-field list))
            (setcdr rest (cons moving-field (cdr rest)))))))
    (bbdb-record-set-field record field list)
    (bbdb-change-record record)
    (bbdb-redisplay-record record)))

;;;###autoload
(defun bbdb-delete-field-or-record (records field &optional noprompt)
  "For RECORDS delete FIELD.
If FIELD is the `name' field, delete RECORDS from datanbase.
Only then RECORDS may be more than one record.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records',
and FIELD is the field point is on.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  (interactive
   (list (bbdb-do-records) (bbdb-current-field) current-prefix-arg))
  (bbdb-editable)
  (unless field (error "Not a field"))
  (setq records (bbdb-record-list records))
  (let ((type (car field)) (record (car records)))
    ;; Multiple elements in RECORDS are only meaningful if we delete these
    ;; records completely (so that the cdr of FIELD is irrelevant).
    (if (eq type 'name)
        (bbdb-delete-records records noprompt)
      (if (cdr records)
          (error "Cannot delete same field from multiple records"))
      (when (or noprompt
                (y-or-n-p (format "delete this %s field (of %s)? "
                                  type (bbdb-record-name record))))
        (cond ((memq type '(phone address))
               (bbdb-record-set-field
                record type
                (delq (nth 1 field)
                      (bbdb-record-get-field record type))))
              ((eq type 'degree)
               (bbdb-record-set-degree record nil))
              ((memq type '(mail aka))
               (dolist (ff (bbdb-record-get-field record type))
                 (bbdb-remhash ff record))
               (bbdb-record-set-field record type nil))
              ((eq type 'organization)
               (dolist (organization (bbdb-record-organization record))
                 (bbdb-remhash organization record))
               (bbdb-record-set-organization record nil))
              ((eq type 'note)
               (bbdb-record-set-note record (car (nth 1 field)) nil))
              (t (error "Unknown field %s" type)))
        (bbdb-change-record record)
        (bbdb-redisplay-record record)))))

;;;###autoload
(defun bbdb-delete-records (records &optional noprompt)
  "Delete RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  (interactive (list (bbdb-do-records) current-prefix-arg))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    (when (or noprompt
              (y-or-n-p (format "Delete the BBDB record of %s? "
                                (or (bbdb-record-name record)
                                    (car (bbdb-record-mail record))))))
      (bbdb-debug (if (bbdb-record-deleted-p record)
                      (error "Deleting deleted record")))
      (bbdb-redisplay-record record t)
      (bbdb-record-set-deleted-p record t)
      (bbdb-delete-record-internal record)
      (setq bbdb-records (delq (assq record bbdb-records) bbdb-records))
      (setq bbdb-changed-records (delq record bbdb-changed-records)))))

;;;###autoload
(defun bbdb-display-all-records (&optional layout)
  "Show all records."
  (interactive (list (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-records) layout))

(defun bbdb-change-records-layout (records layout)
  (dolist (record records)
    (unless (eq layout (nth 1 record))
      (setcar (cdr record) layout)
      (bbdb-redisplay-record (car record)))))

;;;###autoload
(defun bbdb-toggle-records-layout (records &optional arg)
  "Toggle layout of RECORDS (elided or expanded).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
With prefix ARG 0, RECORDS are displayed elided.
With any other non-nil ARG, RECORDS are displayed expanded."
  (interactive (list (bbdb-do-records t) current-prefix-arg))
  (let* ((record (bbdb-current-record))
         (current-layout (nth 1 (assq record bbdb-records)))
         (layout-alist
          ;; Try to consider only those layouts that have the `toggle'
          ;; option set
          (or (delq nil (mapcar (lambda (l)
                                    (if (and (assq 'toggle l)
                                             (cdr (assq 'toggle l)))
                                        l))
                                  bbdb-layout-alist))
              bbdb-layout-alist))
         (layout
          (cond ((eq arg 0)
                 'one-line)
                ((null current-layout)
                 'multi-line)
                 ;; layout is not the last element of layout-alist
                 ;; and we switch to the following element of layout-alist
                ((caar (cdr (memq (assq current-layout layout-alist)
                                  layout-alist))))
                (t ; layout is the last element of layout-alist
                 ;;  and we switch to the first element of layout-alist
                 (caar layout-alist)))))
    (message "Using %S layout" layout)
    (bbdb-change-records-layout (bbdb-record-list records t) layout)))

;;;###autoload
(defun bbdb-display-records-completely (records)
  "Display RECORDS using layout `full-multi-line' (i.e., display all fields).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records t)))
  (let* ((record (bbdb-current-record))
         (current-layout (nth 1 (assq record bbdb-records)))
         (layout (if (not (eq current-layout 'full-multi-line))
                     'full-multi-line
                   'multi-line)))
    (bbdb-change-records-layout (bbdb-record-list records t) layout)))

;;;###autoload
(defun bbdb-display-records-with-layout (records layout)
  "Display RECORDS using LAYOUT.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive
   (list (bbdb-do-records t)
         (intern (completing-read "Layout: "
                                  (mapcar (lambda (i)
                                            (list (symbol-name (car i))))
                                          bbdb-layout-alist)))))
  (bbdb-change-records-layout (bbdb-record-list records t) layout))

;;;###autoload
(defun bbdb-omit-record (n)
  "Remove current record from the display without deleting it from BBDB.
With prefix N, omit the next N records.  If negative, omit backwards."
  (interactive "p")
  (while (not (= n 0))
    (if (< n 0) (bbdb-prev-record 1))
    (let ((record (bbdb-current-record t)))
      (bbdb-redisplay-record (car record) t)
      (setq bbdb-records (delete record bbdb-records)))
    (setq n (if (> n 0) (1- n) (1+ n)))))

;;; Fixing up bogus records
(defun bbdb-merge-concat (string1 string2 &optional separator)
  "Returns the concatenation of STRING1 and STRING2"
  (concat string1 (if (stringp separator) separator "\n") string2))

(defun bbdb-merge-concat-remove-duplicates (string1 string2)
  "Concatenate STRING1 and STRING2, but remove duplicate lines."
  (let ((note1 (split-string string1 "\n")))
    (dolist (line (split-string string2 "\n"))
      (unless (member line note1)
        (push line note1)))
    (bbdb-concat "\n" note1)))

(defun bbdb-merge-string-least (string1 string2)
  "Returns the string that is lessp."
  (if (string-lessp string1 string2)
      string1
    string2))

(defun bbdb-merge-string-most (string1 string2)
  "Returns the string that is not lessp."
  (if (string-lessp string1 string2)
      string2
    string1))

(defun bbdb-merge-lists (l1 l2 cmp &optional mod)
  "Merge two lists L1 and L2 modifying L1.
An element from L2 is added to L1 if CMP returns nil for all elements of L1.
If optional arg MOD is non-nil, it must be a function that is applied
to the elements of L1 and L2 prior to evaluating CMP."
  (if (null l1)
      l2
    (let ((end (last l1))
          (chk (if mod (mapcar mod l1) l1)))
      (dolist (elt l2)
        (let ((src1 chk)
              (val (if mod (funcall mod elt) elt))
              fail)
          (while src1
            (if (funcall cmp (car src1) val)
                (setq src1 nil
                      fail t)
              (setq src1 (cdr src1))))
          (unless fail
            (setcdr end (list elt))
            (setq end (cdr end)))))
      l1)))

(defun bbdb-merge-records-internal (old-record new-record)
  "Merge the contents of OLD-RECORD into NEW-RECORD.
OLD-RECORD remains unchanged.  If names differ, query which one to use.
All other fields are concatenated.  Idealy this would be better about
checking for duplicate entries in other fields, as well as possibly
querying about differing values.

This function does nothing to ensure the integrity of the rest of BBBDB.
That is somebody elses problem (something like `bbdb-merge-records')."
  (if (or (null new-record) (eq old-record new-record))
      (error "Merging record with itself"))
  (let* ((new-name (bbdb-record-name new-record))
         (old-name (bbdb-record-name old-record))
         (old-aka  (bbdb-record-aka  old-record))
         extra-name
         (name
          (cond ((or (string= "" old-name)
                     (bbdb-string= old-name new-name))
                 (cons (bbdb-record-firstname new-record)
                       (bbdb-record-lastname new-record)))
                ((string= "" new-name)
                 (cons (bbdb-record-firstname old-record)
                       (bbdb-record-lastname old-record)))
                (t (prog1
                       (if (y-or-n-p
                            (format "Use name \"%s\" instead of \"%s\"? "
                                    old-name new-name))
                           (progn
                             (setq extra-name new-name)
                             (cons (bbdb-record-firstname old-record)
                                   (bbdb-record-lastname old-record)))
                         (setq extra-name old-name)
                         (cons (bbdb-record-firstname new-record)
                               (bbdb-record-lastname new-record)))
                     (unless (and bbdb-use-alternate-names
                                  (y-or-n-p
                                   (format "Keep \"%s\" as an alternate name? "
                                           (bbdb-record-name extra-name))))
                       (setq extra-name nil)))))))

    (bbdb-record-unset-name new-record)
    (bbdb-record-set-name new-record (car name) (cdr name))

    (if extra-name (push extra-name old-aka))
    (bbdb-record-set-aka new-record
                         (bbdb-merge-lists
                          (bbdb-record-aka new-record) old-aka
                          'bbdb-string=))

    (bbdb-record-set-degree new-record
                            (bbdb-merge-lists
                             (bbdb-record-degree new-record)
                             (bbdb-record-degree old-record)
                             'bbdb-string=))

    (bbdb-record-set-organization new-record
                                  (bbdb-merge-lists
                                   (bbdb-record-organization new-record)
                                   (bbdb-record-organization old-record)
                                   'bbdb-string=))

    (bbdb-record-set-phone new-record
                            (bbdb-merge-lists
                             (bbdb-record-phone new-record)
                             (bbdb-record-phone old-record)
                             'bbdb-string=))
    (bbdb-record-set-address new-record
                               (bbdb-merge-lists
                                (bbdb-record-address new-record)
                                (bbdb-record-address old-record)
                                'bbdb-string=))

    (bbdb-record-set-mail new-record
                           (bbdb-merge-lists
                            (bbdb-record-mail new-record)
                            (bbdb-record-mail old-record)
                            'bbdb-string=))

    (let ((new-notes (bbdb-record-notes new-record))
          new-field)
      (dolist (old-field (bbdb-record-notes old-record))
        (if (setq new-field (assq (car old-field) new-notes))
            ;; merge field contents
            (setcdr new-field
                    (funcall
                     (or (cdr (assq (car old-field)
                                    bbdb-merge-notes-function-alist))
                         bbdb-merge-notes-function)
                     (cdr new-field) (cdr old-field)))
          (push old-field new-notes)))
      (bbdb-record-set-notes new-record new-notes))
    new-record))

;;;###autoload
(defun bbdb-merge-records (old-record new-record)
  "Merge the current record into some other record; that is, delete the
record under point after copying all of the data within it into some other
record.  This is useful if you realize that somehow a redundant record has
gotten into the database, and you want to merge it with another.

If both records have names and/or organizations, you are asked which to use.
Phone numbers, addresses, and mail addresses are simply concatenated.
The first record is the record under the point; the second is prompted for."
  (interactive
   (let* ((old-record (bbdb-current-record))
          (name (bbdb-record-name old-record)))
     (list old-record
           (if current-prefix-arg
               ;; take the first record with the same name
               (car (delq old-record (bbdb-search (bbdb-records) name)))
             (bbdb-completing-read-record
              (format "merge record \"%s\" into: "
                      (or (bbdb-record-name old-record)
                          (car (bbdb-record-mail old-record))
                          "???"))
              (list old-record))))))

  (if (or (null new-record) (eq old-record new-record))
      (error "Records are equal"))
  (setq new-record (bbdb-merge-records-internal old-record new-record))

  (bbdb-delete-records (list old-record) 'noprompt)
  (bbdb-change-record new-record t t)
  (let ((bbdb-layout 'multi-line))
    (if (assq new-record bbdb-records)
        (bbdb-redisplay-record new-record))
    (unless (memq new-record bbdb-changed-records)
      (push new-record bbdb-changed-records))
    (unless bbdb-records             ; nothing displayed, display something.
      (bbdb-display-records (list new-record))))
  (message "Records merged."))

;; The following sorting functions are also intended for use
;; in `bbdb-change-hook'.  Then they will be called with one arg, the record.

;;;###autoload
(defun bbdb-sort-addresses (records)
  "Sort the addresses in RECORDS according to the label.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Can be used in `bbdb-change-hook'."
  (interactive (list (bbdb-do-records)))
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-address
     record (sort (bbdb-record-address record)
                  (lambda (xx yy) (string< (aref xx 0) (aref yy 0)))))
    (bbdb-change-record record)
    (bbdb-redisplay-record record)))

;;;###autoload
(defun bbdb-sort-phones (records)
  "Sort the phones in RECORDS according to the label.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Can be used in `bbdb-change-hook'."
  (interactive (list (bbdb-do-records)))
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-phone
     record (sort (bbdb-record-phone record)
                  (lambda (xx yy) (string< (aref xx 0) (aref yy 0)))))
    (bbdb-change-record record)
    (bbdb-redisplay-record record)))

;;;###autoload
(defun bbdb-sort-notes (records)
  "Sort the notes in RECORDS according to `bbdb-notes-sort-order'.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Can be used in `bbdb-change-hook'."
  (interactive (list (bbdb-do-records)))
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-notes
     record (sort (bbdb-record-notes record)
                  (lambda (a b)
                    (< (or (cdr (assq (car a) bbdb-notes-sort-order)) 100)
                       (or (cdr (assq (car b) bbdb-notes-sort-order)) 100)))))
    (bbdb-change-record record)
    (bbdb-redisplay-record record)))

;;; Send-Mail interface

;;;###autoload
(defun bbdb-dwim-mail (record &optional mail)
  ;; Do What I Mean!
  "Return a string to use as the mail address of RECORD.
The mail address is formatted like \"Firstname Lastname <address>\"
unless both the first name and last name are constituents of the address,
as in John.Doe@SomeHost, or the address is already in the form
\"Name <foo>\" or \"foo (Name)\", in which case the address is used as-is.
If `bbdb-mail-allow-redundancy' is non-nil, the name is always included.
`bbdb-mail-allow-redundancy' is 'mail-only the name is never included.
MAIL may be a mail addresses to be used for RECORD.
If MAIL is an integer, use the MAILth mail address of RECORD.
If Mail is nil use the first mail address of RECORD."
  (unless mail
    (let ((mails (bbdb-record-mail record)))
      (setq mail (or (and (integerp mail) (nth mail mails))
                     (car mails)))))
  (unless mail (error "Record has no mail addresses"))
  (let* ((mail-name (bbdb-record-note record 'mail-name))
         (name (or mail-name (bbdb-record-name record)))
         fn ln (i 0))
    (if mail-name
        (let ((name (bbdb-divide-name mail-name)))
          (setq fn (car name)
                ln (cdr name)))
      (setq fn (bbdb-record-firstname record)
            ln (bbdb-record-lastname  record)))
    (if (or (eq 'mail-only bbdb-mail-allow-redundancy)
            (null name)
            (and (not bbdb-mail-allow-redundancy)
                 (cond ((and fn ln)
                        (or (string-match
                             (concat "\\`[^!@%]*\\b" (regexp-quote fn)
                                     "\\b[^!%@]+\\b" (regexp-quote ln) "\\b")
                             mail)
                            (string-match
                             (concat "\\`[^!@%]*\\b" (regexp-quote ln)
                                     "\\b[^!%@]+\\b" (regexp-quote fn) "\\b")
                             mail)))
                       ((or fn ln)
                        (string-match
                         (concat "\\`[^!@%]*\\b" (regexp-quote (or fn ln)) "\\b")
                         mail))))
            ;; MAIL already in "foo <bar>" or "bar (foo)" format.
            (string-match "\\`[ \t]*[^<]+[ \t]*<" mail)
            (string-match "\\`[ \t]*[^(]+[ \t]*(" mail))
        mail
      ;; If the name contains backslashes or double-quotes, backslash them.
      (setq name (replace-regexp-in-string "[\\\"]" "\\\\\\&" name))
      ;; If the name contains control chars or RFC822 specials, it needs
      ;; to be enclosed in quotes.  This quotes a few extra characters as
      ;; well (!,%, and $) just for common sense.
      ;; `define-mail-alias' uses regexp "[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]".
      (format (if (string-match "[][[:cntrl:]\177()<>@,;:.!$%[:nonascii:]]" name)
                  "\"%s\" <%s>"
                "%s <%s>")
              name mail))))

(defun bbdb-compose-mail (&rest args)
  "Start composing a mail message to send.
Use `bbdb-mail-user-agent' or (if nil) use `mail-user-agent'.
ARGS are passed to `compose-mail'."
  (let ((mail-user-agent (or bbdb-mail-user-agent mail-user-agent)))
    (apply 'compose-mail args)))

;;;###autoload
(defun bbdb-mail (records &optional subject n verbose)
  "Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (bbdb-do-records) nil
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (if verbose (message "No records"))
    (if bbdb-inside-electric-display
        (bbdb-electric-throw
         `(bbdb-mail ',records ',subject ',n ',verbose)))
    (let ((to (bbdb-mail-address records n nil verbose)))
      (unless (string= "" to)
        (bbdb-compose-mail to subject)))))

(defun bbdb-mail-address (records &optional n kill-ring-save verbose)
  "Return mail addresses of RECORDS as a string.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If KILL-RING-SAVE is non-nil (as in interactive calls), copy mail addresses
to kill ring.  If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (bbdb-do-records)
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (progn (if verbose (message "No records")) "")
    (if bbdb-inside-electric-display
        (bbdb-electric-throw
         `(bbdb-mail-address ',records ',n ',kill-ring-save ',verbose)))
    (let ((good "") bad)
      (dolist (record records)
        (let ((mails (bbdb-record-mail record)))
          (cond ((not mails)
                 (push record bad))
                ((eq n t)
                 (setq good (bbdb-concat ",\n\t"
                                         good
                                         (mapcar (lambda (mail)
                                                   (bbdb-dwim-mail record mail))
                                                 mails))))
                (t
                 (setq good (bbdb-concat ",\n\t" good
                            (bbdb-dwim-mail record (or (and (numberp n)
                                                            (nth (1- n) mails))
                                                       (car mails)))))))))
      (when (and bad verbose)
        (message "No mail addresses for %s."
                 (mapconcat 'bbdb-record-name (nreverse bad) ", "))
        (unless (string= "" good) (sit-for 2)))
      (when (and kill-ring-save (not (string= good "")))
        (kill-new good)
        (if verbose (message "%s" good)))
      good)))

;; Is there better way to yank selected mail addresses from the BBDB
;; buffer into a message buffer?  We need some kind of a link between
;; the BBDB buffer and the message buffer, where the mail addresses
;; are supposed to go. Then we could browse the BBDB buffer and copy
;; selected mail addresses from the BBDB buffer into a message buffer.

(defun bbdb-mail-yank ()
  "CC the people displayed in the *BBDB* buffer on this mail message.
The primary mail of each of the records currently listed in the
*BBDB* buffer will be appended to the CC: field of the current buffer."
  (interactive)
  (let ((addresses (with-current-buffer bbdb-buffer-name
                     (delq nil
                           (mapcar (lambda (x)
                                     (if (bbdb-record-mail (car x))
                                         (bbdb-dwim-mail (car x))))
                                   bbdb-records)))))
    (goto-char (point-min))
    (if (re-search-forward "^CC:[ \t]*" nil t)
        ;; We have a CC field. Move to the end of it, inserting a comma
        ;; if there are already addresses present.
        (unless (eolp)
          (end-of-line)
          (while (looking-at "\n[ \t]")
            (forward-char) (end-of-line))
          (insert ",\n")
          (indent-relative))
      ;; Otherwise, if there is an empty To: field, move to the end of it.
      (unless (and (re-search-forward "^To:[ \t]*" nil t)
                   (eolp))
        ;; Otherwise, insert an empty CC: field.
        (end-of-line)
        (while (looking-at "\n[ \t]")
          (forward-char) (end-of-line))
        (insert "\nCC:")
        (indent-relative)))
    ;; Now insert each of the addresses on its own line.
    (while addresses
      (insert (car addresses))
      (when (cdr addresses) (insert ",\n") (indent-relative))
      (setq addresses (cdr addresses)))))
(define-obsolete-function-alias 'bbdb-yank-addresses 'bbdb-mail-yank)

;;; completion

;;;###autoload
(defun bbdb-completion-predicate (symbol)
  "For use as the third argument to `completing-read'.
Obey `bbdb-completion-list'."
  (cond ((null bbdb-completion-list)
         nil)
        ((eq t bbdb-completion-list)
         t)
        ((not (boundp symbol))
         nil) ; deleted (unhashed) record
        (t
         (let ((sym (symbol-name symbol)))
           (catch 'done
             (dolist (record (symbol-value symbol))
               (if (and (memq 'name bbdb-completion-list)
                        (string= sym (downcase (or (bbdb-record-name record)
                                                   ""))))
                   (throw 'done t))
               (if (memq 'aka bbdb-completion-list)
                   (dolist (aka (bbdb-record-aka record))
                     (if (string= sym (downcase aka))
                         (throw 'done t))))
               (if (memq 'organization bbdb-completion-list)
                   (dolist (organization (bbdb-record-organization record))
                     (if (string= sym (downcase organization))
                         (throw 'done t))))
               (if (memq 'primary bbdb-completion-list)
                   (if (string= sym (downcase (car (bbdb-record-mail record))))
                       (throw 'done t))
                 (if (memq 'mail bbdb-completion-list)
                     (dolist (mail (bbdb-record-mail record))
                       (if (string= sym (downcase mail))
                           (throw 'done t)))))))))))

(defun bbdb-completing-read-records (prompt &optional omit-records)
  "Prompt for and return list of records from the bbdb.
Completion is done according to `bbdb-completion-list'.  If the user
just hits return, nil is returned.  Otherwise, a valid response is forced."
  (let* ((completion-ignore-case t)
         (string (completing-read prompt bbdb-hashtable
                                  'bbdb-completion-predicate t))
         symbol ret)
  (unless (string= "" string)
    (setq symbol (intern-soft string bbdb-hashtable))
    (if (and (boundp symbol) (symbol-value symbol))
        (dolist (record (symbol-value symbol) (delete-dups ret))
          (if (not (memq record omit-records))
              (push record ret)))
      (error "Selecting deleted (unhashed) record \"%s\"" symbol)))))

(defun bbdb-completing-read-record (prompt &optional omit-records)
  "Prompt for and return a single record from the bbdb;
completion is done according to `bbdb-completion-list'.  If the user
just hits return, nil is returned. Otherwise, a valid response is forced.
If OMIT-RECORDS is non-nil it should be a list of records to dis-allow
completion with."
  (let ((records (bbdb-completing-read-records prompt omit-records)))
    (cond ((eq (length records) 1)
           (car records))
          ((> (length records) 1)
           (bbdb-display-records records 'one-line)
           (let* ((count (length records))
                  (result (completing-read
                           (format "Which record (1-%s): " count)
                           (mapcar 'number-to-string (number-sequence 1 count))
                           nil t)))
             (nth (1- (string-to-number result)) records))))))

;;;###autoload
(defun bbdb-completing-read-mails (prompt &optional default)
  "Like `read-string', but allows `bbdb-complete-mail' style completion."
  (read-from-minibuffer prompt default
                        bbdb-completing-read-mails-map))

;;;###autoload
(defun bbdb-complete-mail (&optional start-pos cycle-completion-buffer)
  "In a mail buffer, complete the user name or mail before point.
Completion happens up to the preceeding newline, colon, or comma,
or the value of START-POS).
Return non-nil if there is a valid completion, else return nil.

Completion behaviour can be controlled with `bbdb-completion-list'.
If what has been typed is unique, insert an entry of the form
\"User Name <mail>\" (although see `bbdb-mail-allow-redundancy').
If it is a valid completion but not unique, a list of completions is displayed.
If the completion is done and `bbdb-complete-mail-allow-cycling' is
t then cycle through the mails for the matching record.
With prefix CYCLE-COMPLETION-BUFFER non-nil, display a list of all mails
available for cycling.

Set variable `bbdb-complete-mail' non-nil for enabling this feature
as part of the MUA insinuation."
  ;; Should this code issue error messages if, for example,
  ;; it cannot complete because a record has no mail address?
  ;; Or should it simply return nil so that possibly a different
  ;; completion approach can be used?
  (interactive (list nil current-prefix-arg))

  (let* ((end (point))
         (beg (or start-pos
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
         (orig (buffer-substring beg end))
         (typed (downcase orig))
         (pattern (bbdb-string-trim typed))
         (completion-ignore-case t)
         (completion (try-completion pattern bbdb-hashtable))
         all-completions dwim-completions one-record done)

    ;; We cannot use the return value of the function `all-completions'
    ;; to set the variable `all-completions' because this function
    ;; converts all symbols into strings
    (all-completions pattern bbdb-hashtable
                     (lambda (sym)
                       (if (bbdb-completion-predicate sym)
                           (push sym all-completions))))
    ;; Resolve the records matching pattern:
    ;; Multiple completions may match the same record
    (let ((records (delete-dups
                    (apply 'append (mapcar 'symbol-value all-completions)))))
          ;; Is there only one matching record?
      (setq one-record (and (not (cdr records))
                            (car records))))

    ;; Clean up *Completions* buffer, if it exists
    (when bbdb-complete-mail-saved-window-config
      (when (get-buffer-window "*Completions*")
        (set-window-configuration bbdb-complete-mail-saved-window-config)
        (bury-buffer "*Completions*"))
      (setq bbdb-complete-mail-saved-window-config nil))

    (cond
     ;; Match for a single record
     (one-record
      (let (mail lst elt matched)
        (unless (bbdb-record-mail one-record)
          (error "Matching record has no mail field"))
        ;; Do we have a preferential order for these tests?
        ;; (1) Try to match name, AKA, or organization
        (setq matched
              (try-completion pattern
                              (append
                               (list (or (bbdb-record-name one-record) "")
                                     (or (bbdb-cache-lf-name
                                          (bbdb-record-cache one-record)) ""))
                               (bbdb-record-aka one-record)
                               (bbdb-record-organization one-record))))
        ;; (2) Try to match mail addresses
        (unless matched
          (setq lst (bbdb-record-mail one-record))
          (while (setq elt (pop lst))
            (if (try-completion pattern (list elt))
                (setq mail elt
                      lst  nil
                      matched t))))
        ;; This error message indicates a bug!
        (unless matched
          (error "No match for %s" pattern))

        (let ((address (bbdb-dwim-mail one-record mail)))
          (if (string= address (buffer-substring-no-properties beg end))
              (unless (and bbdb-complete-mail-allow-cycling
                           (< 1 (length (bbdb-record-mail one-record))))
                (setq done 'unchanged))
            ;; now replace the text with the expansion
            (delete-region beg end)
            (insert address)
            (setq done 'unique)))))

     ;; Partial completion
     ;; Note: we cannot use the trimmed version of the pattern here
     ;; or we will recurse infinitely on e.g. common first names
     ((and (stringp completion)
           (not (string= typed completion)))
      (delete-region beg end)
      (insert completion)
      (setq done 'partial))

     ;; Partial match not allowing further partial completion
     (completion
      (let ((completion-list (if (eq t bbdb-completion-list)
                                 '(fl-name lf-name mail aka organization)
                               bbdb-completion-list))
            sname records)
        ;; Now collect all the dwim-addresses for each completion, but only
        ;; once for each record!  Add it if the mail is part of the completions
        (dolist (sym all-completions)
          (setq sname (symbol-name sym))
          (dolist (record (symbol-value sym))
            (unless (memq record records)
              (push record records)
              (let ((mails (bbdb-record-mail record))
                    accept)
                (when mails
                  (dolist (field completion-list)
                    (cond ((eq field 'fl-name)
                           (if (bbdb-string= sname (bbdb-record-name record))
                               (push (car mails) accept)))
                          ((eq field 'lf-name)
                           (if (bbdb-string= sname (bbdb-cache-lf-name (bbdb-record-cache record)))
                               (push (car mails) accept)))
                          ((eq field 'aka)
                           (if (member-ignore-case sname (bbdb-record-aka record))
                               (push (car mails) accept)))
                          ((eq field 'organization)
                           (if (member-ignore-case sname (bbdb-record-organization record))
                               (push (car mails) accept)))
                          ((eq field 'primary)
                           (if (bbdb-string= sname (car mails))
                               (push (car mails) accept)))
                          ((eq field 'mail)
                           (dolist (mail mails)
                             (if (bbdb-string= sname mail)
                                 (push mail accept))))))
                  (when accept
                    ;; If in the end dwim-completions contains only one element,
                    ;; we got here only once.
                    (setq one-record record)
                    (dolist (mail (delete-dups accept))
                      (push (bbdb-dwim-mail record mail) dwim-completions))))))))

        (cond ((not dwim-completions)
               (error "No mail address for \"%s\"" orig))
              ;; This can happen if multiple completions match the same record
              ((eq 1 (length dwim-completions))
               (delete-region beg end)
               (insert (car dwim-completions))
               (setq done 'unique))
              (t (setq done 'choose))))))

    ;; Consider cycling
    (when (and (not done) bbdb-complete-mail-allow-cycling)
      ;; find the record we are working on.
      (let* ((address (mail-extract-address-components orig))
             (record (and (listp address)
                          (car (or (bbdb-message-search (nth 0 address)
                                                        (nth 1 address))
                                   ;; If the mail address of a record contains
                                   ;; a name explicitly, we need to search for orig.
                                   (bbdb-message-search orig (nth 1 address))))))
             (mails (and record (bbdb-record-mail record))))
        (if mails
            (cond ((= 1 (length mails))
                   (setq done 'unchanged))
                  (cycle-completion-buffer ; use completion buffer
                   (setq dwim-completions
                         (mapcar (lambda (n) (bbdb-dwim-mail record n)) mails)
                         done 'choose))
                  (t ; use next mail
                   (let ((mail (or (nth 1 (or (member (nth 1 address) mails)
                                              (member orig mails)))
                                   (nth 0 mails))))
                     ;; replace with new mail address
                     (delete-region beg end)
                     (insert (bbdb-dwim-mail record mail))
                     (setq done 'cycle)))))))

    ;; Clean up
    (cond ((eq done 'unique)
           ;; If we are past `fill-column', wrap at the previous comma.
           (if (and (not (auto-fill-function))
                    (>= (current-column) fill-column))
               (save-excursion
                 (when (search-backward "," (line-beginning-position) t)
                   (forward-char 1)
                   (insert "\n   "))))

           ;; Update the *BBDB* buffer if desired.
           (if bbdb-completion-display-record
               (let ((bbdb-silent-internal t))
                 (bbdb-pop-up-buffer)
                 (bbdb-display-records-internal (list one-record) nil t)))

           ;; call the unique-completion hook
           (run-hooks 'bbdb-complete-mail-hook))

          ;; Pop up a completions window.
          ;; `completion-in-region' does not work here as `dwim-completions'
          ;; is not a collection for completion in the usual sense, but it
          ;; is really a list of replacements.
          ((eq done 'choose)
           (unless (eq (selected-window) (minibuffer-window))
             (message "Making completion list..."))
           (unless (get-buffer-window "*Completions*")
             (setq bbdb-complete-mail-saved-window-config
                   (current-window-configuration)))
           (let ((completion-base-position (list beg end)))
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list dwim-completions)))
           (unless (eq (selected-window) (minibuffer-window))
             (message "Making completion list...done"))))
    done))

;;;###autoload
(define-obsolete-function-alias 'bbdb-complete-name 'bbdb-complete-mail)

;;; interface to mail-abbrevs.el.

;;;###autoload
(defun bbdb-mail-aliases (&optional force-rebuilt noisy)
  "Define mail aliases for the records in the database.
Define a mail alias for every record that has a `mail-alias' field
which is the contents of that field.
If there are multiple comma-separated words in the `mail-alias' field,
then all of those words will be defined as aliases for that person.

If multiple records in the database have the same mail alias,
then that alias expands to a comma-separated list of the mail addresses
of all of these people.
Add this command to `mail-setup-hook'.

Mail aliases are (re)built only if `bbdb-mail-aliases-need-rebuilt' is non-nil
because the database was newly loaded or it has been edited.
Rebuilding the aliases is enforced if prefix FORCE-REBUILT is t."
  (interactive (list current-prefix-arg t))
  ;; Build `mail-aliases' if not yet done.
  ;; Note: `mail-abbrevs-setup' rebuilds the mail-aliases only if
  ;; `mail-personal-alias-file' has changed.  So it would not do anything
  ;; if we want to rebuild the mail-aliases because of changes in BBDB.
  (if (or force-rebuilt (eq t mail-aliases)) (build-mail-aliases))

  ;; We should be cleverer here and instead of rebuilding all aliases
  ;; we should just do what's necessary, i.e. remove deleted records
  ;; and add new records
  ;; Calling `bbdb-records' can change `bbdb-mail-aliases-need-rebuilt'
  (let ((records (bbdb-search (bbdb-records) nil nil nil
                              (cons bbdb-mail-alias-field ".")))
        results aliases match)
    (if (not (or force-rebuilt bbdb-mail-aliases-need-rebuilt))
        (if noisy (message "BBDB mail alias: nothing to do"))
      (setq bbdb-mail-aliases-need-rebuilt nil)

      ;; collect an alist of (alias rec1 [rec2 ...])
      (dolist (record records)
        (if (bbdb-record-mail record)
            (setq aliases (bbdb-split bbdb-mail-alias-field
                           (bbdb-record-note record bbdb-mail-alias-field)))
          (unless bbdb-silent
            (bbdb-warn "record %S has no mail address, but the aliases: %s"
                       (bbdb-record-name record)
                       (bbdb-record-note record bbdb-mail-alias-field))
            (sit-for 1))
          (setq aliases nil))

        (dolist (alias aliases)
          (if (setq match (assoc alias results))
              ;; If an alias appears more than once, we collect all records
              ;; that refer to it.
              (nconc match (list record))
            (push (list alias record) results))))

      ;; Iterate over the results and create the aliases
      (dolist (result results)
        (let* ((aliasstem (car result))
               (expansions
                (if (cddr result)
                    ;; for group aliases we just take all the primary mails
                    ;; and define only one expansion!
                    (list (mapconcat (lambda (record) (bbdb-dwim-mail record))
                                     (cdr result) mail-alias-separator-string))
                  ;; this is an alias for a single person so deal with it
                  ;; according to `bbdb-mail-alias'
                  (let* ((record (nth 1 result))
                         (mails (bbdb-record-mail record)))
                    (if (or (eq 'first bbdb-mail-alias)
                            (not (cdr mails)))
                        ;; Either we want to define only one alias for
                        ;; the first mail address or there is anyway
                        ;; only one address.  In either case, we take
                        ;; take only the first address.
                        (list (bbdb-dwim-mail record (car mails)))
                      ;; We need to deal with more than one mail address...
                      (let* ((all (mapcar (lambda (m) (bbdb-dwim-mail record m))
                                          mails))
                             (star (bbdb-concat mail-alias-separator-string all)))
                        (if (eq 'star bbdb-mail-alias)
                            (list star (car all))
                          ;; if `bbdb-mail-alias' is 'all, we create
                          ;; two aliases for the primary mail address
                          (cons star (cons (car all) all))))))))
               (count -1) ; n=-1: <alias>*;  n=0: <alias>;  n>0: <alias>n
               (len (length expansions))
               alias f-alias)

          ;; create the aliases for each expansion
          (dolist (expansion expansions)
            (cond ((or (= 1 len)
                       (= count 0))
                   (setq alias aliasstem))
                  ((= count -1) ;; all the mails of a record
                   (setq alias (concat aliasstem "*")))
                  (t ;; <alias>n for each mail of a record
                   (setq alias (format "%s%s" aliasstem count))))
            (setq count (1+ count))

            (add-to-list 'mail-aliases (cons alias expansion))

            (define-mail-abbrev alias expansion)
            (unless (setq f-alias (intern-soft (downcase alias) mail-abbrevs))
              (error "Cannot find the alias"))

            ;; `define-mail-abbrev' initializes f-alias to be
            ;; `mail-abbrev-expand-hook'. We replace this by
            ;; `bbdb-mail-abbrev-expand-hook'
            (unless (eq (symbol-function f-alias) 'mail-abbrev-expand-hook)
              (error "mail-aliases contains unexpected hook %s"
                     (symbol-function f-alias)))
            ;; `bbdb-mail-abbrev-hook' is called with mail addresses instead of
            ;; bbdb records to avoid keeping pointers to records, which would
            ;; lose if the database was reverted.
            ;; `bbdb-mail-abbrev-hook' uses `bbdb-message-search' to convert
            ;; these mail addresses to records, which is plenty fast.
            ;; FIXME: The value of arg MAILS for `bbdb-mail-abbrev-hook'
            ;; is wrong. Currently it is based on the list of records that have
            ;; referenced ALIASTEM and we simply take the first mail address
            ;; from each of these records.
            ;; Then `bbdb-message-search' will find the correct records
            ;; (assuming that each mail address appears only once in the
            ;; database).  Nonethless, arg MAILS for `bbdb-mail-abbrev-hook'
            ;; does not, in general, contain the actual mail addresses
            ;; of EXPANSION.  So what we would need is to go back from
            ;; EXPANSION to the mail addresses it contains (which is tricky
            ;; because mail addresses in the database can be shortcuts for
            ;; the addresses in EXPANSION).
            (fset f-alias `(lambda ()
                             (bbdb-mail-abbrev-expand-hook
                              ,alias
                              ',(mapcar (lambda (r) (car (bbdb-record-mail r)))
                                        (cdr result))))))))

      (if noisy (message "BBDB mail alias: rebuilding done")))))

(defun bbdb-mail-abbrev-expand-hook (alias mails)
  (run-hook-with-args 'bbdb-mail-abbrev-expand-hook alias mails)
  (mail-abbrev-expand-hook)
  (when bbdb-completion-display-record
    (let ((bbdb-silent-internal t))
      (bbdb-display-records-internal
       (apply 'append
              (mapcar (lambda (mail) (bbdb-message-search nil mail)) mails))
       nil t))))

(defun bbdb-get-mail-aliases ()
  "Return a list of mail aliases used in the BBDB."
  (let ((records (bbdb-search (bbdb-records) nil nil nil
                              (cons bbdb-mail-alias-field ".")))
        result)
    (dolist (record records result)
      (dolist (alias (bbdb-split bbdb-mail-alias-field
                                 (bbdb-record-note
                                  record bbdb-mail-alias-field)))
        (add-to-list 'result alias)))))

;;;###autoload
(defun bbdb-add-mail-alias (record &optional alias delete)
  "Add ALIAS to RECORD.
If pefix DELETE is non-nil, remove ALIAS from RECORD."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (bbdb-current-record))
          (init-f (concat "bbdb-init-" (symbol-name bbdb-mail-alias-field)))
          (init (if (and (setq init-f (intern-soft init-f))
                         (functionp init-f))
                    (funcall init-f record))))
     (list record
           (completing-read
            (format "%s mail alias: "
                    (if current-prefix-arg "Remove" "Add"))
            (bbdb-get-mail-aliases) nil nil
            init) current-prefix-arg)))
  (setq alias (bbdb-string-trim alias))
  (unless (string= "" alias)
    (let ((aliases (bbdb-record-note record bbdb-mail-alias-field)))
      (if aliases (setq aliases (bbdb-split bbdb-mail-alias-field aliases)))
      (if delete
          (setq aliases (delete alias aliases))
        ;; Add alias only if it is not there yet
        (add-to-list 'aliases alias))
      (setq aliases (bbdb-concat bbdb-mail-alias-field aliases))
      (bbdb-record-set-note record bbdb-mail-alias-field aliases)
      (bbdb-change-record record))
    (bbdb-redisplay-record record)
    ;; Rebuilt mail aliases
    (setq bbdb-mail-aliases-need-rebuilt
          (if delete
              'deleted
            (if (bbdb-record-mail record)
                'new)))))

;;; Dialing numbers from BBDB

(defun bbdb-play-sound (num &optional volume)
  "Play the specified touchtone number NUM at VOLUME.
Uses external program `bbdb-sound-player' if set, otherwise
try to use internal sound if available."
  ;; We cannot tell a priori if Emacs facility will actually work.
  (cond ((not (condition-case nil
                  (play-sound (list 'sound
                                    :file (aref bbdb-sound-files
                                                (string-to-number num))
                                    :volume (or volume bbdb-sound-volume)))
                (error nil))))
        ((and bbdb-sound-player
              (file-exists-p bbdb-sound-player))
         (call-process bbdb-sound-player nil nil nil
                       (aref bbdb-sound-files num)))
        ((error "BBDB has no means of playing sound."))))

(defun bbdb-dial-number (phone-string)
  "Dial the number specified by PHONE-STRING.
The number is dialed either by playing touchtones through the audio
device using `bbdb-sound-player', or by sending a dial sequence to
`bbdb-modem-device'. # and * are dialed as-is, and a space is treated as
a pause in the dial sequence."
  (interactive "sDial number: ")
  (if bbdb-modem-dial
      (with-temp-buffer
        (insert bbdb-modem-dial)
        (insert (mapconcat
                 (lambda (d)
                   (cond ((eq ?\s d) ",")
                         ((memq d '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?* ?#))
                          (format "%c" d))
                         (t "")))
                 phone-string "") ";\r\n")
        (write-region (point-min) (point-max) bbdb-modem-device t)
        (message "%s dialed. Pick up the phone now and hit any key ..."
                 phone-string)
        (read-event)
        (erase-buffer)
        (insert "ATH\r\n")
        (write-region (point-min) (point-max) bbdb-modem-device t))

    (mapc (lambda (d)
            (cond
             ((eq ?# d)
              (bbdb-play-sound 10))
             ((eq ?* d)
              (bbdb-play-sound 11))
             ((eq ?\s d)
              ;; if we use `sit-for', the user can interrupt!
              (sleep-for 1)) ;; configurable?
             ((memq d '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
              (bbdb-play-sound (- d ?0)))
             (t)))
          phone-string)))

;;;###autoload
(defun bbdb-dial (phone force-area-code)
  "Dial the number at point.
If the point is at the beginning of a record, dial the first
phone number.  Does not dial the extension.  Does not apply the
transformations from bbdb-dial-local-prefix-alist if a prefix arg
is given."
  (interactive (list (bbdb-current-field) current-prefix-arg))
  (if (eq (car-safe phone) 'name)
      (setq phone (car (bbdb-record-phone (bbdb-current-record)))))
  (if (eq (car-safe phone) 'phone)
      (setq phone (car (cdr phone))))
  (or (vectorp phone) (error "Not on a phone field"))

  (let* ((number (bbdb-phone-string phone)) shortnumber elt)
    (when (not force-area-code)
      (let ((alist bbdb-dial-local-prefix-alist))
        (while (setq elt (pop alist))
          (if (string-match (concat "^" (eval (car elt))) number)
              (setq shortnumber (concat (cdr elt)
                                        (substring number (match-end 0)))
                    alist nil)))))

    ;; cut off the extension
    (if (string-match "x[0-9]+$" number)
        (setq number (substring number 0 (match-beginning 0))))

    ;; This is terrifically Americanized...
    ;; Leading 0 => local number (?)
    (if (and (not shortnumber) bbdb-dial-local-prefix
             (string-match "^0" number))
        (setq number (concat bbdb-dial-local-prefix number)))

    ;; Leading + => long distance/international number
    (if (and (not shortnumber) bbdb-dial-long-distance-prefix
             (string-match "^\+" number))
        (setq number (concat bbdb-dial-long-distance-prefix " "
                             (substring number 1))))

    ;; use the short number if it's available
    (setq number (or shortnumber number))
    (unless bbdb-silent
      (message "Dialing %s" number))
    (bbdb-dial-number number)))

;;; url interface

;;;###autoload
(defun bbdb-browse-url (records &optional which)
  "Brwose URLs stored in the `url' field of RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Prefix WHICH specifies which URL in field `url' is used (starting from 0).
Default is the first URL."
  (interactive (list (bbdb-get-records "Visit (URL): ")
                     current-prefix-arg))
  (dolist (record (bbdb-record-list records))
    (let ((url (bbdb-record-note-n record 'url (or which 0))))
      (when url
        (setq url (read-string "fetch: " url))
        (unless (string= "" url)
          (browse-url url))))))

;;;###autoload
(defun bbdb-grab-url (record url)
  "Grab URL and store it in RECORD."
  (interactive (list (bbdb-completing-read-record "Add URL for: ")
                     (browse-url-url-at-point)))
  (bbdb-set-notes-labels 'url)
  (bbdb-merge-note record 'url url)
  (bbdb-change-record record)
  (bbdb-display-records (list record)))

;;; Copy to kill ring

;;;###autoload
(defun bbdb-copy-records-as-kill (records)
  "Copy displayed RECORDS to kill ring.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records t)))
  (let (drec marker)
    (dolist (record (bbdb-record-list records t))
      (push (buffer-substring (nth 2 record)
                              (or (nth 2 (car (cdr (memq record bbdb-records))))
                                  (point-max)))
            drec))
    (kill-new (replace-regexp-in-string
               "[ \t\n]*\\'" "\n"
               (mapconcat 'identity (nreverse drec) "")))))

;;; Help and documentation

;;;###autoload
(defun bbdb-info ()
  (interactive)
  (require 'info)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw '(bbdb-info))
    (info (format "(%s)Top" (or bbdb-info-file "bbdb")))))

;;;###autoload
(defun bbdb-help ()
  (interactive)
  (message (substitute-command-keys "\\<bbdb-mode-map>\
new field: \\[bbdb-insert-field]; \
edit field: \\[bbdb-edit-field]; \
delete field: \\[bbdb-delete-field-or-record]; \
mode help: \\[describe-mode]; \
info: \\[bbdb-info]")))

(provide 'bbdb-com)
