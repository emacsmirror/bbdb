;;; bbdb-mua.el --- various MUA functionality for BBDB

;; Copyright (C) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;; Copyright (C) 2010 Roland Winkler <winkler@gnu.org>

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
;;; This file provides various additional functionality for BBDB
;;; See bbdb.texinfo for documentation.

;;; This file lets you do stuff like
;;;
;;; o  automatically add some string to the notes field(s) based on the
;;;    contents of header fields of the current message
;;; o  only automatically create records when certain header fields
;;;    are matched
;;; o  do not automatically create records when certain header fields
;;;    are matched
;;;
;;; Read the docstrings; read the texinfo file.

(require 'bbdb)
(require 'bbdb-com)

(eval-when-compile
  (autoload 'gnus-fetch-field "gnus-utils")
  (autoload 'gnus-summary-select-article "gnus-sum")

  (autoload 'bbdb/vm-header "bbdb-vm")
  (autoload 'vm-follow-summary-cursor "vm-motion")
  (autoload 'vm-select-folder-buffer "vm-macro")
  (autoload 'vm-check-for-killed-summary "vm-misc")
  (autoload 'vm-error-if-folder-empty "vm-misc")
  (defvar vm-message-pointer)

  (autoload 'bbdb/rmail-header "bbdb-rmail")
  (autoload 'rmail-buffer "rmail")
  (defvar rmail-current-message)

  (autoload 'bbdb/mh-header "bbdb-mhe")
  (autoload 'mh-show "mh-show")
  (defvar mh-show-buffer)

  (autoload 'message-field-value "message")
  (autoload 'mail-decode-encoded-word-string "mail-parse"))

(defun bbdb-mua ()
  "For the current message return the MUA.
Return values include
  gnus      Newsreader Gnus
  rmail     Reading Mail in Emacs
  vm        VM
  mh        Emacs interface to the MH mail system (aka MH-E)
  message   Mail and News composition mode that goes with Gnus
  mail      Emacs Mail Mode."
  (cond ((member major-mode ;; VM
                 '(vm-mode vm-virtual-mode vm-summary-mode vm-presentation-mode))
         'vm)
        ((member major-mode ;; Gnus
                 '(gnus-summary-mode gnus-article-mode gnus-tree-mode))
         'gnus)
        ((member major-mode '(rmail-mode rmail-summary-mode)) ;; Rmail
         'rmail)
        ((member major-mode '(mhe-mode mhe-summary-mode mh-folder-mode)) ;: MH-E
         'mh)
        ((eq major-mode 'message-mode) ;; Message mode
         'message)
        ((eq major-mode 'mail-mode) ;; Mail mode
         'mail)
        (t (error "BBDB: MUA `%s' not supported" major-mode))))

;;;###autoload
(defun bbdb-message-header (header)
  "For the current message return the value of HEADER.
MIME encoded headers are decoded.  Return nil if HEADER does not exist."
  ;; RW: If HEADER was allowed to be a regexp and the content of multiple
  ;; matching headers was concatenated as in `message-field-value',
  ;; this would simplify the usage of `bbdb-accept-message-alist' and
  ;; `bbdb-ignore-message-alist'.
  (let* ((mua (bbdb-mua))
         (val (cond ((eq mua 'gnus) (gnus-fetch-field header))
                    ((eq mua 'vm) (bbdb/vm-header header))
                    ((eq mua 'rmail) (bbdb/rmail-header header))
                    ((eq mua 'mh) (bbdb/mh-header header))
                    ((member mua '(message mail)) (message-field-value header))
                    (t (error "BBDB/%s: header function undefined" mua)))))
    (if val (mail-decode-encoded-word-string val))))

;;; Update database

;;;###autoload
(defun bbdb-accept-message (&optional invert)
  "For use with MUA-specific variables `bbdb/MUA-update-records-p'.
Return the value of `bbdb-update-records-p' for messages matching
`bbdb-accept-message-alist'.  If INVERT is non-nil, accept messages
not matching `bbdb-ignore-message-alist'."
  (let ((rest (if invert bbdb-ignore-message-alist
                bbdb-accept-message-alist))
        (case-fold-search t)
        hd-val done elt)
    (if (eq rest t)
        (setq done t)
      (while (and (setq elt (pop rest)) (not done))
        (dolist (header (if (stringp (car elt)) (list (car elt)) (car elt)))
          (setq hd-val (bbdb-message-header header))
          (if (and hd-val (string-match (cdr elt) hd-val))
              (setq done t)))))
    (if invert (setq done (not done)))
    (if done bbdb-update-records-p)))

;;;###autoload
(defun bbdb-ignore-message (&optional invert)
  "For use with MUA-specific variables `bbdb/MUA-update-records-p'.
Return the value of `bbdb-update-records-p' for messages not matching
`bbdb-ignore-message-alist'.  If INVERT is non-nil, accept messages
matching `bbdb-accept-message-alist'."
  (bbdb-accept-message (not invert)))

;;;###autoload
(defun bbdb-select-message ()
  "For use with MUA-specific variables `bbdb/MUA-update-records-p'.
Return the value of `bbdb-update-records-p' for messages both matching
`bbdb-accept-message-alist' and not matching `bbdb-ignore-message-alist'."
  (and (bbdb-accept-message)
       (bbdb-ignore-message)))

(defun bbdb-get-address-components (&optional header-class ignore-address)
  "Extract mail addresses from a message.
Return list with elements (NAME EMAIL HEADER HEADER-CLASS).
HEADER-CLASS is defined in `bbdb-message-headers'.  If arg HEADER-CLASS is
nil, use all classes in `bbdb-message-headers'.
If regexp IGNORE-ADDRESS matches NAME or EMAIL of an address, this address
will be ignored. If IGNORE-ADDRESS is nil accept all addresses."
  ;; We do not use `bbdb-message-all-addresses' here because only when we
  ;; have compared the addresses with the records in BBDB do we know which
  ;; address(es) are relevant for us.
  (let ((message-headers (if header-class
                             (list (assoc header-class bbdb-message-headers))
                           bbdb-message-headers))
        (ignore-address (or ignore-address bbdb-user-mail-address-re))
        address-list address name mail mail-list content)
    (dolist (headers message-headers)
      (setq header-class (car headers)) ; sender or recipient
      (dolist (header (cdr headers))
        (when (setq content (bbdb-message-header header))
          ;; Real work is done by `mail-extract-address-components'.
          ;; Always extract all addresses because we do not know yet which
          ;; address might match IGNORE-ADDRESS.
          (dolist (address (mail-extract-address-components content t))
            (setq name (nth 0 address)
                  mail (nth 1 address))
            ;; ignore uninteresting addresses
            (unless (or (and (stringp ignore-address)
                             (or (and name (string-match ignore-address name))
                                 (string-match ignore-address mail)))
                        (member mail mail-list))
              ;; Add each address only once. (Use MAIL-LIST for book keeping.)
              ;; Thus if we care about whether an address gets associated with
              ;; one or another header, the order of elements in
              ;; `bbdb-message-headers' is relevant.  The "most important"
              ;; headers should be first in `bbdb-message-headers'.
              (push mail mail-list)
              (push (list name mail header header-class) address-list))))))
    (nreverse address-list)))

;;;###autoload
(defun bbdb-update-records (address-list &optional update-p msg-key)
  "Return the list of BBDB records matching ADDRESS-LIST.
ADDRESS-LIST is a list of mail addresses.  (It can be extracted from
a mail message using `bbdb-get-address-components'.)
UPDATE-P may take the following values:
 search       Search for existing records matching ADDRESS.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return any of the above values.
 nil          Take the MUA-specific variable `bbdb/MUA-update-records-p'
                which may take any of the above values.
                If this still gives nil, `bbdb-update-records' returns nil.
If MSG-KEY is non-nil consult cache.

Usually this function is called by the wrapper `bbdb-mua-update-records'."
  ;; UPDATE-P allows filtering of complete messages.
  ;; Filtering of individual addresses within an accepted message
  ;; is done by `bbdb-get-address-components' using `bbdb-user-mail-address-re'.
  (unless update-p
    (setq update-p (eval (intern-soft (format "bbdb/%s-update-records-p"
                                              (bbdb-mua))))))
  (if (functionp update-p)
      (setq update-p (funcall update-p)))
  (if (eq t update-p)
      (setq update-p 'create))
  (let ((bbdb-records (bbdb-records)) ;; search all records
        ;; `bbdb-update-records-p' and `bbdb-offer-to-create' are used here
        ;; as internal variables for communication with
        ;; `bbdb-prompt-for-create'.  This does not affect the value of the
        ;; global user variable `bbdb-update-records-p'.
        (bbdb-offer-to-create 'start)
        (bbdb-update-records-p update-p)
        address records)
    ;; Ignore cache if we may be creating a record, since the cache
    ;; may otherwise tell us that the user did not want a record for
    ;; this person.
    (if (and msg-key (not (member update-p '(query create))))
        (setq records (bbdb-message-get-cache msg-key)))

    (when (and (not records) update-p)
      (while (setq address (pop address-list))
        (let* ((bbdb-update-records-address address)
               (mail (nth 1 address))
               hits
               (task
                (catch 'done
                  (setq hits
                        (cond ((null mail)
                               nil) ; ignore emtpy mails, e.g. (??? nil)
                              ((eq bbdb-update-records-p 'create)
                               (list (bbdb-annotate-message address t)))
                              ((eq bbdb-update-records-p 'query)
                               (list ; Search might return a list
                                (bbdb-annotate-message
                                 address 'bbdb-prompt-for-create)))
                              ((eq bbdb-update-records-p 'search)
                               ;; Search for records having this mail address
                               ;; but do not modify an existing record.
                               (let ((mail (concat "^" (regexp-quote mail) "$")))
                                 ;; MAIL must be atomic arg.
                                 (bbdb-search bbdb-records nil nil mail)))))
                  nil)))
          (cond ((eq task 'quit)
                 (setq address-list nil))
                ((not (eq task 'next))
                 (dolist (hit (delq nil (nreverse hits)))
                   ;; people should be listed only once so we use `add-to-list'
                   (add-to-list 'records hit))))
          (if (and records (not bbdb-message-all-addresses))
              (setq address-list nil))))
      ;; update cache
      (if msg-key (bbdb-message-set-cache msg-key records)))

    (if bbdb-message-all-addresses
        records
      (if records (list (car records))))))

(defun bbdb-prompt-for-create ()
  "Interactive query used by `bbdb-update-records'.
Return t if the record should be created or `nil' otherwise.
Honor previous answers such as \"!\"."
  (let ((task bbdb-offer-to-create))
    ;; If we have remembered what the user typed previously,
    ;; `bbdb-offer-to-create' holds a character, i.e., a number.
    ;; -- Right now, we only remember "!".
    (when (not (integerp task))
      (let ((prompt (format "%s is not in BBDB; add? (y,!,n,s,q,?) "
                            (or (nth 0 bbdb-update-records-address)
                                (nth 1 bbdb-update-records-address))))
            event)
        (while (not event)
          (setq event (read-key-sequence prompt))
          (setq event (if (stringp event) (aref event 0))))
        (setq task event)
        (message ""))) ; clear the message buffer

    (cond ((eq task ?y)
           t)
          ((eq task ?!)
           (setq bbdb-offer-to-create task)
           t)
          ((or (eq task ?n)
               (eq task ?\s))
           (throw 'done 'next))
          ((or (eq task ?q)
               (eq task ?\a)) ; ?\a = C-g
           (throw 'done 'quit))
          ((eq task ?s)
           (setq bbdb-update-records-p 'search)
           (throw 'done 'next))
          (t ; any other key sequence
           (save-window-excursion
             (let* ((buffer (get-buffer-create " *BBDB Help*"))
                    (window (or (get-buffer-window buffer)
                                (split-window (get-lru-window)))))
               (with-current-buffer buffer
                 (setq buffer-read-only t)
                 (let (buffer-read-only)
                   (erase-buffer)
                   (insert
                    "Your answer controls how BBDB updates/searches for records.

Type ?  for this help.
Type y  to add the current record.
Type !  to add all remaining records.
Type n  to skip the current record. (You might also type space)
Type s  to switch from annotate to search mode.
Type q  to quit updating records.  No more search or annotation is done.")
                   (set-buffer-modified-p nil)
                   (goto-char (point-min)))
                 (set-window-buffer window buffer)
                 (fit-window-to-buffer window)))
             ;; Try again!
             (bbdb-prompt-for-create))))))


;;; message-caching, to speed up the the mail interfaces
;; `bbdb-message-cache' is a buffer-local alist for each MUA or MUA folder.
;; Its elements are (MESSAGE-KEY RECORDS). MESSAGE-KEY is specific to the MUA.
;; Note: For MESSAGE-KEY, we can always use (bbdb-message-header "Message-ID").

(defun bbdb-message-get-cache (message-key)
  "Return cached BBDB records for MESSAGE-KEY.
If not present or when the records have been modified return nil."
  (bbdb-records)
  (if (and bbdb-message-caching message-key)
      (let ((records (cdr (assq message-key bbdb-message-cache)))
            (valid t) record)
        (while (and valid (setq record (pop records)))
          (if (bbdb-record-deleted-p record)
              (setq valid nil)))
        (if valid records))))

(defun bbdb-message-set-cache (message-key records)
  "Cache the RECORDS for a message identified by MESSAGE-KEY and
return them."
  (and bbdb-message-caching records
       (add-to-list 'bbdb-message-cache (cons message-key records))
       records))

;; not used anywhere
(defun bbdb-message-rem-cache (message-key)
  "Remove an element from `bbdb-message-cache'."
  (if bbdb-message-caching
      (setq bbdb-message-cache
            (delq (assq message-key bbdb-message-cache) bbdb-message-cache))))



(defun bbdb-annotate-message (address &optional update-p)
  "Fill the record for message ADDRESS with as much info as possible.
If a record for ADDRESS does not yet exist, UPDATE-P controls whether
a new record is created for ADDRESS.  UPDATE-P may take the values:
 nil          Never create a new record.
 query        Query interactively whether to create a new record.
 create or t  Create a new record.
 a function   Create a new record if the function returns non-nil.
Return the record matching ADDRESS or nil."
  ;; ADDRESS should be compatible with `mail-extract-address-components'.
  (let* ((data (if (consp address)
                   address ; if from is a cons, it's pre-parsed (hack hack)
                 (mail-extract-address-components address)))
         (name (car data))
         (mail (nth 1 data)))
    (if (equal name mail) (setq name nil))
    (bbdb-debug
     (if (equal name "") (error "mail-extr returned \"\" as name"))
     (if (equal mail "") (error "mail-extr returned \"\" as mail")))

    (setq mail (bbdb-canonicalize-mail mail))

    ;; FIXME: We drop all records but the first!!
    (let* ((record (car (bbdb-message-search name mail)))
           (old-name (and record (bbdb-record-name record)))
           change-p created-p fname lname duplicate)
      (if (and (not record) (functionp update-p))
          (setq update-p (funcall update-p)))
      (if (eq t update-p)
          (setq update-p 'create))

      ;; This is to prevent having losers like "John <blat@foop>" match
      ;; against existing records like "Someone Else <john>".
      ;;
      ;; The solution implemented here is to never create or show records
      ;; corresponding to a person who has a real-name which is the same
      ;; as the mail of someone in the BBDB already.  This is not
      ;; a good solution.
      (when (and record name (not (bbdb-string= name old-name)))
        (let ((old-mail (bbdb-record-mail record)) om)
          (while (setq om (pop old-mail))
            (when (bbdb-string= name om)
              (setq duplicate t
                    old-mail nil)
              (message
               "Ignoring duplicate %s's name \"%s\" to avoid name clash with \"%s\""
               mail name old-name)
              (sit-for 2)))))

      ;; Create a new record if nothing else fits.
      ;; In this way, we can fill the slots of the new record with the same code
      ;; that overwrites the slots of exisiting records.
      (unless (or record bbdb-read-only
                  (not update-p)
                  (eq update-p 'search) ; for simple compatibility
                  (not (or name mail))
                  duplicate)
        ;; otherwise, the db is writable, and we may create a record.
        ;; first try to get a reasonable default name if not given
        ;; often I get things like <firstname>.<surname>@ ...
        (if (or (null name) (and (stringp name) (string= "" name)))
            (if (string-match "^[^@]+" mail)
                (setq name (bbdb-message-clean-name (match-string 0 mail)))))
        (setq record (if (or (eq update-p 'create)
                             (and (eq update-p 'query)
                                  (y-or-n-p (format "%s is not in the BBDB.  Add? "
                                                    (or name mail)))))
                         (make-vector bbdb-record-length nil))
              created-p (not (null record)))
        (if record (bbdb-record-set-cache
                    record (make-vector bbdb-cache-length nil))))

      ;; Analyze the name part of the record.
      (unless (or duplicate (null record))
        (if (and name
                 (not (bbdb-string= name old-name))
                 ;; Check if name equals the name of the record
                 (let ((fullname (bbdb-divide-name name)))
                   (setq fname (car fullname)
                         lname (cdr fullname))
                   (not (and (bbdb-string= fname (bbdb-record-firstname record))
                             (bbdb-string= lname (bbdb-record-lastname record))))))

            ;; name differs from the old name.
            (cond (bbdb-read-only nil);; skip if readonly

                  ;; ignore name mismatches?
                  ((and bbdb-accept-name-mismatch old-name)
                   (let ((sit-for-secs
                          (if (numberp bbdb-accept-name-mismatch)
                              bbdb-accept-name-mismatch 2)))
                     (unless (or bbdb-silent (zerop sit-for-secs))
                       (message "name mismatch: \"%s\" changed to \"%s\""
                                old-name name)
                       (sit-for sit-for-secs))))

                  ((or bbdb-silent
                       (not (or old-name (bbdb-record-mail record))) ; new record
                       (y-or-n-p
                        (if old-name
                            (format "Change name \"%s\" to \"%s\"? "
                                    old-name name)
                          (format "Assign name \"%s\" to address \"%s\"? "
                                  name (car (bbdb-record-mail record))))))
                   ;; Keep old name?
                   (and old-name bbdb-use-alternate-names
                        (not (member old-name (bbdb-record-aka record)))
                        (if (or bbdb-silent
                                (y-or-n-p
                                 (format "Keep name \"%s\" as an AKA? " old-name)))
                            (bbdb-record-set-aka
                             record (cons old-name (bbdb-record-aka record)))
                          (bbdb-remhash old-name record)))
                   (bbdb-debug (or fname lname
                                   (error "BBDB: should have a name by now")))
                   (bbdb-record-unset-name record)
                   (bbdb-record-set-name record fname lname)
                   (setq change-p 'sort))

                  ;; make new name an alias?
                  ((and old-name bbdb-use-alternate-names
                        (not (member name (bbdb-record-aka record)))
                        (unless bbdb-silent
                          (y-or-n-p
                           (format "Make \"%s\" an alternate for \"%s\"? "
                                   name old-name))))
                   (bbdb-record-set-aka
                    record (cons name (bbdb-record-aka record)))
                   (bbdb-puthash name record)
                   (setq change-p 'sort))))

        ;; It's kind of a kludge that the "redundancy" concept is built in.
        ;; Maybe I should just add a new hook here...  The problem is that
        ;; `bbdb-canonicalize-mail' is run before database lookup,
        ;; and thus it cannot refer to the database to determine whether a mail
        ;; is redundant.
        (if bbdb-canonicalize-redundant-mails
            (setq mail (or (bbdb-mail-redundant-p mail (bbdb-record-mail record))
                           mail)))

        ;; Analyze the mail part of the new records
        (if (and mail (not (equal mail "???")) (not bbdb-read-only))
            (if (null (bbdb-record-mail record))
                ;; Record has not yet a mail field.  Names are usually
                ;; a sure match, so do not bother prompting here.
                (progn (bbdb-record-set-mail record (list mail))
                       (bbdb-puthash mail record)
                       (or change-p (setq change-p t)))
              ;; new mail address; ask before adding.
              (unless (let ((mails (bbdb-record-mail record))
                            (new (downcase mail))
                            elt match)
                        (while (and (setq elt (pop mails)) (null match))
                          (setq match (string= new (downcase elt))))
                        match)
                (let ((bbdb-add-mails bbdb-add-mails))
                  (if (functionp bbdb-add-mails)
                      (setq bbdb-add-mails (funcall bbdb-add-mails)))
                  (if (or (eq bbdb-add-mails t) ; add it automatically
                          (and (eq bbdb-add-mails 'query)
                               (or (y-or-n-p (format "Add address \"%s\" to \"%s\"? " mail
                                                     (bbdb-concat 'mail (bbdb-record-mail record))))
                                   (and (or (eq update-p 'create)
                                            (and (eq update-p 'query)
                                                 (y-or-n-p
                                                  (format "Create a new record for %s? "
                                                          (bbdb-record-name record)))))
                                        (setq record
                                              (bbdb-create-internal name nil nil nil mail))))))
                      ;; then modify an existing record
                      (if (or (eq t bbdb-new-mails-always-primary)
                              (and bbdb-new-mails-always-primary
                                   (y-or-n-p
                                    (format "Make \"%s\" the primary address? " mail))))
                          (bbdb-record-set-mail record (cons mail (bbdb-record-mail record)))
                        (bbdb-record-set-mail record (nconc (bbdb-record-mail record)
                                                            (list mail))))
                      (bbdb-puthash mail record)
                      (or change-p (setq change-p t)))))))

        (bbdb-debug
         (if (and change-p bbdb-read-only)
             (error "How did we change anything in readonly mode?")))
        (if (and change-p (not bbdb-silent))
            (if (eq change-p 'sort)
                (message "noticed \"%s\"" (bbdb-record-name record))
              (if (bbdb-record-name record)
                  (message "noticed %s's address \"%s\""
                           (bbdb-record-name record) mail)
                (message "noticed naked address \"%s\"" mail))))

        (if created-p (run-hook-with-args 'bbdb-create-hook record))
        (if change-p (bbdb-change-record record (eq change-p 'sort) created-p))

        ;; only invoke `bbdb-notice-hook' if we actually noticed something
        (if record
            (let ((bbdb-notice-hook-pending t))
              (run-hook-with-args 'bbdb-notice-hook record)))

        record))))

(defun bbdb-mua-update-records (&optional header-class update-p)
  "Wrapper for `bbdb-update-records'.
HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P is defined in `bbdb-update-records'."
  (let ((mua (bbdb-mua)))
    ;; If below we change buffers, do we ever need a `save-current-buffer'?
    (cond ;; VM
          ((eq mua 'vm)
           (vm-select-folder-buffer)
           (vm-check-for-killed-summary)
           (vm-error-if-folder-empty)
           (let ((enable-local-variables t))  ; ...or vm bind this to nil.
             (bbdb-update-records (bbdb-get-address-components header-class)
                                  update-p (car vm-message-pointer))))
          ;; Gnus
          ((eq mua 'gnus)
           (bbdb-update-records (bbdb-get-address-components header-class)
                                update-p (bbdb-message-header "Message-ID")))
          ;; MH-E
          ((eq mua 'mh)
           (if mh-show-buffer (set-buffer mh-show-buffer))
           (bbdb-update-records (bbdb-get-address-components header-class)
                                update-p (bbdb-message-header "Message-ID")))
          ;; Rmail
          ((eq mua 'rmail)
           (if (and (boundp 'rmail-buffer) rmail-buffer)
               (set-buffer rmail-buffer)
             (error "Not in an rmail buffer"))
           (when rmail-current-message
             (bbdb-update-records (bbdb-get-address-components header-class)
                                  update-p (bbdb-message-header "Message-ID"))))
          ;; Message and Mail
          ((member mua '(message mail))
           (bbdb-update-records (bbdb-get-address-components header-class)
                                update-p)))))

(defun bbdb-mua-update-mua ()
  "Update MUA before running a BBDB command."
  (let ((mua (bbdb-mua)))
    (cond ((eq mua 'vm) (vm-follow-summary-cursor))
          ((eq mua 'gnus) (gnus-summary-select-article))
          ((and (eq mua 'rmail)
                (boundp 'rmail-buffer) rmail-buffer)
           (set-buffer rmail-buffer))
          ((eq mua 'mh) (mh-show) (set-buffer mh-show-buffer)))))

(defun bbdb-mua-update-interactive-p ()
  "Interactive spec for arg UPDATE-P of `bbdb-mua-display-records' and friends.
If these commands are called without a prefix, the value of their arg
UPDATE-P is the car of the variable `bbdb-mua-update-interactive-p'.
Called with a prefix, the value of UPDATE-P becomes the cdr of this variable."
  (let ((update-p (if current-prefix-arg
                      (cdr bbdb-mua-update-interactive-p)
                    (car bbdb-mua-update-interactive-p))))
    (if (eq update-p 'read)
        (let ((str (completing-read "Action: " '((query) (search) (create))
                                    nil t)))
          (unless (string= "" str) (intern str))) ; nil otherwise
      update-p)))

;;;###autoload
(defun bbdb-mua-display-records (&optional header-class update-p)
  "Display the BBDB record(s) for the addresses in this message.
HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'."
  (interactive (list nil (bbdb-mua-update-interactive-p)))
  (save-current-buffer ;; rmail and mhe change buffers
    (bbdb-mua-update-mua)
    (let ((records (bbdb-mua-update-records header-class update-p)))
      (if records (bbdb-display-records-internal records))
      records)))

;;;###autoload
(defun bbdb-mua-display-sender (&optional update-p)
  "Display the BBDB record(s) for the sender of this message.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'."
  (interactive (list (bbdb-mua-update-interactive-p)))
  (bbdb-mua-display-records 'sender update-p))

;;;###autoload
(defun bbdb-mua-display-recipients (&optional update-p)
  "Display the BBDB record(s) for the recipients of this message.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'."
  (interactive (list (bbdb-mua-update-interactive-p)))
  (bbdb-mua-display-records 'recipients update-p))

;; RW: This command appears to be obsolete
;;;###autoload
(defun bbdb-display-all-recipients (&optional header-class)
  "Display BBDB records for all addresses of the message in this buffer.
If the records do not exist, they are generated."
  (interactive)
  (let ((bbdb-message-all-addresses t))
    (bbdb-mua-display-records header-class 'create)))

;; Used by `bbdb-auto-notes' and `bbdb-mua-annotate-sender'
(defun bbdb-annotate-notes (record annotation &optional field replace)
  "In RECORD add an ANNOTATION to the note FIELD.
FIELD defaults to notes.
If REPLACE is non-nil, ANNOTATION replaces the content of FIELD."
  (unless (string= "" (setq annotation (bbdb-string-trim annotation)))
    (unless field (setq field 'notes))
    (bbdb-set-notes-names field)
    (bbdb-merge-note record field annotation replace)
    (bbdb-change-record record)
    (bbdb-maybe-update-display record)))

;;;###autoload
(defun bbdb-mua-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender(s) of this message.
If prefix REPLACE is non-nil, replace the existing notes entry (if any)."
  (interactive (list (read-string "Comments: ") current-prefix-arg))
  (save-current-buffer ;; rmail and mhe change buffers
    (bbdb-mua-update-mua)
    (dolist (record (bbdb-mua-update-records 'sender))
      (bbdb-annotate-notes record string 'notes replace))))

;;;###autoload
(defun bbdb-mua-annotate-recipients (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the recipient(s) of this message.
If prefix REPLACE is non-nil, replace the existing notes entry (if any)."
  (interactive (list (read-string "Comments: ") current-prefix-arg))
  (save-current-buffer ;; rmail and mhe change buffers
    (bbdb-mua-update-mua)
    (dolist (record (bbdb-mua-update-records 'recipients))
      (bbdb-annotate-notes record string 'notes replace))))

(defun bbdb-mua-edit-notes-sender (&optional field)
  "Edit notes FIELD of record corresponding to sender of this message.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read
                      "Field: " (mapcar 'symbol-name bbdb-notes-names))))))
  (unless field (setq field 'notes))
  (save-current-buffer ;; rmail and mhe change buffers
    (bbdb-mua-update-mua)
    (let ((records (bbdb-mua-update-records 'sender)))
      (bbdb-display-records records)
      (dolist (record records)
        (bbdb-record-edit-notes record field t)))))

(defun bbdb-mua-edit-notes-recipients (&optional field)
  "Edit notes FIELD of record corresponding to recipient of this message.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read
                      "Field: " (mapcar 'symbol-name bbdb-notes-names))))))
  (unless field (setq field 'notes))
  (save-current-buffer ;; rmail and mhe change buffers
    (bbdb-mua-update-mua)
    (let ((records (bbdb-mua-update-records 'recipients)))
      (bbdb-display-records records)
      (dolist (record records)
        (bbdb-record-edit-notes record field t)))))

;; Functions for noninteractive use in MUA hooks

(defun bbdb-mua-pop-up-bbdb-buffer (&optional header-class update-p)
  "Make the *BBDB* buffer be displayed along with the MUA window(s).
Displays the records corresponding to the sender respectively
recipients of the current message.
See `bbdb-message-pop-up', and the MUA-specic variables
`bbdb/MUA-update-records-p' for configuration of what is being displayed.
Intended for noninteractive use via appropriate MUA hook.
See `bbdb-mua-display-records' and friends for interactive commands."
  (if bbdb-message-pop-up
      (let* ((bbdb-silent-internal t)
             (records (bbdb-mua-update-records header-class update-p))
             (mua (bbdb-mua))
             (mode (cond ((eq mua 'vm) 'vm-mode)
                         ((eq mua 'gnus) 'gnus-article-mode)
                         ((eq mua 'rmail) 'rmail-mode)
                         ((eq mua 'mh) 'mh-folder-mode)
                         ((eq mua 'message) 'message-mode)
                         ((eq mua 'mail) 'mail-mode))))
        (if records
            (bbdb-display-records-internal
             records nil nil nil
             `(lambda (window)
                (with-current-buffer (window-buffer window)
                  (eq major-mode ',mua))))
          ;; If there are no records, empty the BBDB window.
          (bbdb-undisplay-records)))))

;; RW: This appears to be obsolete if BBDB supports message mode
;;;###autoload
(defun bbdb-force-record-create (&optional header-class)
  "Force automatic creation of BBDB record(s) for the current message.
You might add this to the reply hook of your MUA in order to automatically
get records added for those people you reply to."
  (interactive)
  (bbdb-mua-pop-up-bbdb-buffer header-class 'create))

;;;###autoload
(defun bbdb-auto-notes (record)
  "For use as a `bbdb-notice-hook'.
Automatically add some text to the notes field of RECORD based on the headers
of the current message.  See the variables `bbdb-auto-notes-alist',
`bbdb-auto-notes-ignore' and `bbdb-auto-notes-ignore-all'."
  ;; This could stand to be faster...
  ;; could optimize this to check the cache, and noop if this record is
  ;; cached for any other message, but that's probably not the right thing.
  (unless bbdb-read-only
   (let ((ignore-all bbdb-auto-notes-ignore-all)
         (case-fold-search t)
         ignore header pairs hd-val regexp string field
         replace-p rule elt re)
     ;; Do nothing if this message is from us.  Note that we have to look
     ;; at the message instead of the record, because the record will be
     ;; of the recipient of the message if it is from us.
     (unless (and (setq hd-val (bbdb-message-header "From"))
                  (string-match bbdb-user-mail-address-re hd-val))
       ;; check the ignore-all pattern
       (while (and (setq rule (pop ignore-all)) (not ignore))
         (setq header (car rule)
               regexp (cdr rule)
               hd-val (bbdb-message-header header))
         (if (and hd-val (string-match regexp hd-val))
             (setq ignore t)))

       (unless ignore          ; ignore-all matched
         (dolist (rule bbdb-auto-notes-alist)
           (setq header (car rule) ; name of header, e.g., "Subject"
                 pairs (cdr rule)  ; (REGEXP . STRING) or
                                   ; (REGEXP FIELD-NAME STRING) or
                                   ; (REGEXP FIELD-NAME STRING REPLACE-P)
                 hd-val (bbdb-message-header header)) ; e.g., Subject line
           (when hd-val
             ;; we perform the auto notes stuff only for sender of a message
             ;; or if explicitly requested
             (if (or (symbolp (caar pairs)) (listp (caar pairs)))
                 (if (or (memq (nth 3 bbdb-update-records-address) (car pairs))
                         (and (assoc (nth 3 bbdb-update-records-address) (car pairs))
                              (string= (nth 2 bbdb-update-records-address)
                                       (cdr (assoc (nth 3 bbdb-update-records-address)
                                                   (car pairs))))))
                     (setq pairs (cdr pairs))
                   (setq pairs nil))
               (unless (and (eq 'sender (nth 3 bbdb-update-records-address))
                            (string-match "From" (nth 2 bbdb-update-records-address)))
                 (setq pairs nil)))

             ;; now handle the remaining pairs
             (dolist (elt pairs)
               (setq regexp (car elt)
                     string (cdr elt))
               (if (consp string) ; not just the (REGEXP . STRING) format
                   (setq field (car string)
                         string (nth 1 string)
                         replace-p (nth 2 string)) ; perhaps nil
                 ;; else it's simple (REGEXP . STRING)
                 (setq field 'notes
                       replace-p nil))
               (setq re (cdr (assoc header bbdb-auto-notes-ignore)))
               (when (and (string-match regexp hd-val)
                          ;; make sure it is not to be ignored
                          (not (and re (string-match re hd-val))))
                 (setq string
                       ;; An integer as STRING is an index into `match-data':
                       (cond ((integerp string) ; backward compat
                              (match-string string hd-val))
                             ((stringp string)
                              (replace-match string nil nil hd-val))
                             ((functionp string)
                              ;; A function as STRING calls the function on hd-val:
                              (funcall string hd-val))
                             (t (error "Illegal value: %s" string))))
                 (bbdb-annotate-notes record string field replace-p))))))))))

;;; Massage of mail addresses

(defun bbdb-canonicalize-mail (mail)
  "Canonicalize MAIL address using `bbdb-canonicalize-mail-function'."
  (if mail
      (if (functionp bbdb-canonicalize-mail-function)
          (funcall bbdb-canonicalize-mail-function mail)
        mail)))

;;; I use `bbdb-canonicalize-mail-1' as the value of `bbdb-canonicalize-mail-function'.
;;; It is provided as an example for you to customize.

(defcustom bbdb-canonical-hosts
  ;; Example
  (mapconcat 'regexp-quote '("cs.cmu.edu" "ri.cmu.edu") "\\|")
  "Certain sites have a single mail-host; for example, all mail originating
at hosts whose names end in \".cs.cmu.edu\" can (and probably should) be
addressed to \"user@cs.cmu.edu\" instead."
  :group 'bbdb-mua
  :type '(regexp :tag "Regexp matching sites"))

;;;###autoload
(defun bbdb-canonicalize-mail-1 (address)
  (cond
   ;;
   ;; rewrite mail-drop hosts.
   ((string-match
     (concat "\\`\\([^@%!]+@\\).*\\.\\(" bbdb-canonical-hosts "\\)\\'")
     address)
    (concat (match-string 1 address) (match-string 2 address)))
   ;;
   ;; Here at Lucid, our workstation names sometimes get into our mail
   ;; addresses in the form "jwz%thalidomide@lucid.com" (instead of simply
   ;; "jwz@lucid.com").  This removes the workstation name.
   ((string-match "\\`\\([^@%!]+\\)%[^@%!.]+@\\(lucid\\.com\\)\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another way that our local mailer is misconfigured: sometimes addresses
   ;; which should look like "user@some.outside.host" end up looking like
   ;; "user%some.outside.host" or even "user%some.outside.host@lucid.com"
   ;; instead.  This rule rewrites it into the original form.
   ((string-match "\\`\\([^@%]+\\)%\\([^@%!]+\\)\\(@lucid\\.com\\)?\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user@foobar.com".
   ;; That's totally redundant, so this rewrites it as "user@foobar.com".
   ((string-match "\\`\\([^@%!]+\\)!\\([^@%!]+[@%]\\1\\)\\'" address)
    (match-string 2 address))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user".  Turn it around.
   ((string-match "\\`\\([^@%!.]+\\.[^@%!]+\\)!\\([^@%]+\\)\\'" address)
    (concat (match-string 2 address) "@" (match-string 1 address)))
   ;;
   ;; The mailer at hplb.hpl.hp.com tends to puke all over addresses which
   ;; pass through mailing lists which are maintained there: it turns normal
   ;; addresses like "user@foo.com" into "user%foo.com@hplb.hpl.hp.com".
   ;; This reverses it.  (I actually could have combined this rule with
   ;; the similar lucid.com rule above, but then the regexp would have been
   ;; more than 80 characters long...)
   ((string-match "\\`\\([^@!]+\\)%\\([^@%!]+\\)@hplb\\.hpl\\.hp\\.com\\'"
          address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another local mail-configuration botch: sometimes mail shows up
   ;; with addresses like "user@workstation", where "workstation" is a
   ;; local machine name.  That should really be "user" or "user@netscape.com".
   ;; (I'm told this one is due to a bug in SunOS 4.1.1 sendmail.)
   ((string-match "\\`\\([^@%!]+\\)[@%][^@%!.]+\\'" address)
    (match-string 1 address))
   ;;
   ;; Sometimes I see addresses like "foo%somewhere%uunet.uu.net@somewhere.else".
   ;; This is silly, because I know that I can send mail to uunet directly.
   ((string-match ".%uunet\\.uu\\.net@[^@%!]+\\'" address)
    (concat (substring address 0 (+ (match-beginning 0) 1)) "@UUNET.UU.NET"))
   ;;
   ;; Otherwise, leave it as it is.  Returning a string equal to the one
   ;; passed in tells BBDB that we are done.
   (t address)))

(defun bbdb-mail-redundant-p (mail old-mails)
  "Return non-nil if MAIL is a sub-domain of one of the OLD-MAILS.
The return value is the address which makes this one redundant.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\",
and \"foo@quux.bar.baz.com\" is redundant w.r.t. \"foo@bar.baz.com\"."
  (let (redundant-address)
    (while (and (not redundant-address) old-mails)
      ;; Calculate a host-regexp for each address in OLD-MAILS
      (let* ((old (car old-mails))
             (host-index (string-match "@" old))
             (name (and host-index (substring old 0 host-index)))
             (host (and host-index (substring old (1+ host-index))))
             ;; host-regexp is "^<name>@.*\.<host>$"
             (host-regexp (and name host
                               (concat "\\`" (regexp-quote name)
                                       "@.*\\." (regexp-quote host)
                                       "\\'"))))
        ;; If MAIL matches host-regexp, then it is redundant
        (if (and host-regexp mail
                 (string-match host-regexp mail))
            (setq redundant-address old)))
      (setq old-mails (cdr old-mails)))
    redundant-address))

;;; Here's another approach; sometimes one gets mail from foo@bar.baz.com,
;;; and then later gets mail from foo@baz.com.  At this point, one would
;;; like to delete the bar.baz.com address, since the baz.com address is
;;; obviously superior.  See also var `bbdb-canonicalize-redundant-mails'.
;;;
;;; Turn this on with
;;;   (add-hook 'bbdb-change-hook 'bbdb-delete-redundant-mails)

(defun bbdb-delete-redundant-mails (record)
  "Deletes redundant mail addresses.
For use as a value of `bbdb-change-hook'.  See `bbdb-mail-redundant-p'."
  (let ((mails (bbdb-record-mail record))
         okay redundant)
    (dolist (mail mails)
      (if (bbdb-mail-redundant-p mail mails)
          (push mail redundant)
        (push mail okay)))
    (when redundant
      (message "Deleting redundant mails %s..."
               (bbdb-concat 'mail (nreverse redundant)))
      (bbdb-record-set-mail record (nreverse okay)))))

(defun bbdb-message-clean-name (string)
  "Strip garbage from the user full name string."
  (if (string-match "[@%!]" string)  ; ain't no user name!  It's an address!
      (bbdb-string-trim string)
   (let ((case-fold-search t))
     ;; Remove leading non-alpha chars
     (if (string-match "\\`[^[:alpha:]]+" string)
         (setq string (substring string (match-end 0))))
     ;; Remove phone extensions (like "x1234" and "ext. 1234")
     ;; This does not work all the time because some of our friends in
     ;; northern europe have brackets in their names...
     (setq string (replace-regexp-in-string
                   "\\W+\\(x\\|ext\\.?\\)\\W*[-0-9]+" "" string))
     ;; Remove trailing non-alpha chars
     (if (string-match "[^[:alpha:]]+\\'" string)
         (setq string (substring string 0 (match-beginning 0))))
     ;; Replace tabs, spaces, and underscores with a single space.
     (setq string (replace-regexp-in-string "[ \t\n_]+" " " string))
     ;; Do not replace ". " with " " because that could be an initial.
     (setq string (replace-regexp-in-string "\\.\\([^ ]\\)" " \\1" string))
     ;; Remove trailing parenthesized comments
     (when (string-match "[^ \t]\\([ \t]*\\((\\| -\\| #\\)\\)" string)
       (setq string (substring string 0 (match-beginning 1))))
     string)))

(provide 'bbdb-mua)
