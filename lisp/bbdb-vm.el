;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to VM (View Mail) 5.31 or greater.  See bbdb.texinfo.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;
;; $Id$
;;

(require 'cl)
(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-snarf)
(require 'vm-autoload)
(require 'vm)

(if (not (fboundp 'vm-record-and-change-message-pointer))
    (load-library "vm-motion"))
(if (not (fboundp 'vm-su-from))
    (load-library "vm-summary"))
(or (boundp 'vm-mode-map)
    (load-library "vm-vars"))

(defcustom bbdb/vm-get-from-headers
  '(;; authors headers
    "From:" "Sender:" "Resent-From:" "Reply-To:"
    ;; recipients headers
    "Resent-To:" "Resent-CC:" "To:" "CC:")
  "*List of headers to search for senders respectively recipients.
You may add additional headers, however be warned since it will take
more time to search more headers!"
  :group 'bbdb-mua-specific-vm
  :type 'list)

(defcustom bbdb/vm-get-only-first-from-p
  t
  "*If t `bbdb/vm-update-records' will return only the first one.
Changing this variable will show its effect only after clearing the
`bbdb-message-cache' of a folder or closing and visiting it again."
  :group 'bbdb-mua-specific-vm
  :type 'boolean)

(defun bbdb/vm-get-from (msg &optional only-first-from)
  "Return real name and email address of sender respectively recipient.
If an address matches `vm-summary-uninteresting-senders' it will be ignored.
The headers to search can be configured by `bbdb/vm-get-from-headers'."
  (setq msg (vm-real-message-of msg))
  (let ((headers  bbdb/vm-get-from-headers)
    (fromlist nil)
    header adlist fn ad)
    (while headers
      (setq header (vm-get-header-contents msg (car headers)))
      (when header
    (setq adlist (bbdb-extract-address-components
          (vm-decode-mime-encoded-words-in-string header)))
    (while adlist
      (setq fn (caar adlist)
        ad (cadar adlist))

      ;; ignore uninteresting addresses
      ;; this is kinda gross!
      (if (or
           (not (stringp vm-summary-uninteresting-senders))
           (not
        (or (and fn
             (string-match vm-summary-uninteresting-senders fn))
        (and ad
             (string-match vm-summary-uninteresting-senders ad)))))
          (add-to-list 'fromlist (car adlist)))

      (if (and only-first-from fromlist)
          (setq adlist nil headers nil)
        (setq adlist (cdr adlist)))))
      (setq headers (cdr headers)))
    (nreverse fromlist)))

;; We use our own caching functions instead of the bbdb default
;; functions since we are handling a set of records and not a single
;; one.
(defun bbdb/vm-message-cache-lookup (message-key)
  (bbdb-records)
  (if bbdb-message-caching-enabled
      (let ((records (assq message-key bbdb-message-cache))
        (invalid nil))
    (mapcar (lambda (record)
          (if (bbdb-record-deleted-p record)
              (setq invalid t)))
        (cdr records))
    (if invalid nil records))))

(defun bbdb/vm-encache-message (message-key bbdb-record)
  "Don't call this multiple times with the same args, it doesn't replace."
  (and bbdb-message-caching-enabled
       (setq bbdb-message-cache  (cons (cons message-key bbdb-record)
                       bbdb-message-cache))
       (notice-buffer-with-cache (current-buffer))))

;;;###autoload
(defun bbdb/vm-update-record (&optional offer-to-create)
  (let* ((bbdb/vm-get-only-first-from-p t)
     (records (bbdb/vm-update-records offer-to-create)))
    (if records (car records) nil)))

;;;###autoload
(defun bbdb/vm-update-records (&optional offer-to-create)
  "Returns the records corresponding to the current VM message,
creating or modifying them as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation.

When hitting C-g once you will not be asked anymore for new people listed
in this message, but it will search only for existing records.  When hitting
C-g again it will stop scanning."
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((msg (car vm-message-pointer))
    (inhibit-local-variables nil)   ; vm binds this to t...
    (enable-local-variables t)  ; ...or vm bind this to nil.
    (inhibit-quit nil)      ; vm better not bind this to t!
    (bbdb/vm-offer-to-create offer-to-create)
    cache records)

    ;; ignore cache if we may be creating a record, since the cache
    ;; may otherwise tell us that the user didn't want a record for
    ;; this person.
    (if (not bbdb/vm-offer-to-create)
        (setq cache (and msg (bbdb/vm-message-cache-lookup msg))))

    (if cache
        (setq records (if bbdb/vm-get-only-first-from-p
                          (if (cadr cache);; stop it from returning '(nil)
                              (list (cadr cache))
                            nil)
                        (cdr cache)))
      (and msg
           (let ((addrs (bbdb/vm-get-from msg bbdb/vm-get-only-first-from-p))
                 (bbdb-records (bbdb-records))
         (processed-addresses 0)
         (bbdb/vm-update-records-mode
          (if offer-to-create 'annotating
            (eval bbdb/vm-update-records-mode)))
         rec)
             (mapc (lambda (bbdb/vm-address)
             (condition-case nil
             (progn
               (setq rec
                 (cond ((eq bbdb/vm-update-records-mode
                        'annotating)
                    (bbdb-annotate-message-sender
                     bbdb/vm-address t
                     (or (bbdb-invoke-hook-for-value
                          bbdb/mail-auto-create-p)
                         bbdb/vm-offer-to-create);; force create
                     'bbdb/vm-prompt-for-create))
                       ((eq bbdb/vm-update-records-mode
                        'searching)
                    ;; search for the first record having
                    ;; this net
                    (let ((net (cadr bbdb/vm-address))
                          ;; there is no case for nets
                          (bbdb-case-fold-search t)
                          record)
                      (setq record (bbdb-search
                            bbdb-records
                            nil nil net))
                      (if record (car record) nil))))
                 processed-addresses (+ processed-addresses 1))
               (when (and (not bbdb-silent-running)
                      (not (eq bbdb/vm-offer-to-create 'quit))
                      (= 0 (% processed-addresses 5)))
                 (let ((mess (format "Hit C-g to stop BBDB from %s.  %d of %d addresses processed." bbdb/vm-update-records-mode processed-addresses (length addrs))))
                   (display-message 'progress mess))
                 (sit-for 0)))
               (quit (cond ((eq bbdb/vm-update-records-mode
                    'annotating)
                    (setq bbdb/vm-update-records-mode
                      'searching))
                   ((eq bbdb/vm-update-records-mode 'searching)
                    nil)
                   ((eq bbdb/vm-update-records-mode 'next)
                    (setq bbdb/vm-update-records-mode
                      'annotating))
                   (t
                    (setq bbdb/vm-update-records-mode 'quit)))
                 nil))

             ;; people should be listed only once so we use
             ;; add-to-list
             (if rec (add-to-list 'records rec)))

           addrs)
             (setq records (nreverse records))
         (bbdb/vm-encache-message msg records))))

    ;; If there were multiple records to update, let the user know we're done.
    (and (not bbdb-silent-running)
         records
         (> (length records) 1)
         (display-message 'progress "Updating of BBDB records finished"))
    records))

(defcustom bbdb/vm-update-records-mode
  '(if (vm-new-flag msg) 'annotating 'searching)
  "Controls how `bbdb/vm-update-records' processes email addresses.
Set this to an expression which evaluates either to 'searching or
'annotating.  When set to 'annotating email addresses will be fed to
`bbdb-annotate-message-sender' in order to update existing records or create
new ones.  A value of 'searching will search just for existing records having
the right net.

The default is to annotate only new messages.

This variable is also used for inter-function communication between the
functions `bbdb/vm-update-records' and `bbdb/vm-prompt-for-create'."
  :group 'bbdb-mua-specific-vm
  :type '(choice (const :tag "annotating all messages"
            'annotating)
         (const :tag "annotating no messages"
            'searching)
         (const :tag "annotating only new messages"
            (if (vm-new-flag msg) 'annotating 'searching))
         (sexp   :tag "user defined")))

(defvar bbdb/vm-offer-to-create nil
  "Used for inter-function communication between the functions
`bbdb/vm-update-records' and `bbdb/vm-prompt-for-create'.")
(defvar bbdb/vm-address nil
  "Used for inter-function communication between the functions
`bbdb/vm-update-records' and `bbdb/vm-prompt-for-create'.")

(if (fboundp 'characterp)
    (defalias 'bbdb/vm-characterp 'characterp)
  (defalias 'bbdb/vm-characterp 'char-or-string-p))

;; This is a hack.  The function is called by bbdb-annotate-message-sender and
;; uses the above variable in order to manipulate bbdb/vm-update-records.
;; Some cases are handled with signals in order to keep the changes in
;; bbdb-annotate-message-sender as minimal as possible.
(defun bbdb/vm-prompt-for-create ()
  (let ((old-offer-to-create bbdb/vm-offer-to-create))
    (when (or (bbdb-invoke-hook-for-value bbdb/prompt-for-create-p)
          bbdb/vm-offer-to-create)
      (when (not (bbdb/vm-characterp bbdb/vm-offer-to-create))
    (message (format "%s is not in the db; add? (y,!,n,s,q)"
             (or (car bbdb/vm-address) (cadr bbdb/vm-address))))
    (setq bbdb/vm-offer-to-create (read-char)))

      (cond ((eq bbdb/vm-offer-to-create ?y)
         (setq bbdb/vm-offer-to-create old-offer-to-create)
         nil)
        ((eq bbdb/vm-offer-to-create ?!)
         nil)
        ((eq bbdb/vm-offer-to-create ?n)
         (setq bbdb/vm-update-records-mode 'next
           bbdb/vm-offer-to-create old-offer-to-create)
         (signal 'quit nil))
        ((eq bbdb/vm-offer-to-create ?q)
         (setq bbdb/vm-update-records-mode 'quit)
         (signal 'quit nil))
        ((eq bbdb/vm-offer-to-create ?s)
         (setq bbdb/vm-update-records-mode 'searching)
         (signal 'quit nil))))))

;;;###autoload
(defun bbdb/vm-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive
   (list (if bbdb-readonly-p
         (error "The Insidious Big Brother Database is read-only.")
       (read-string "Comments: "))))
  (vm-follow-summary-cursor)
  (bbdb-annotate-notes (bbdb/vm-update-record t) string 'notes replace))


(defun bbdb/vm-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (vm-follow-summary-cursor)
  (let ((record (or (bbdb/vm-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
    (bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

;;;###autoload
(defun bbdb/vm-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (vm-follow-summary-cursor)
  (let ((record (bbdb/vm-update-record t)))
    (if record
    (bbdb-display-records (list record))
      (error "unperson"))))


(defun bbdb/vm-show-all-recipients ()
  "Show all recipients of this message. Counterpart to bbdb/vm-show-sender."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (bbdb-show-all-recipients))

(defun bbdb/vm-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the VM window(s).
Displays the records corresponding to the sender respectively
recipients of the current message.
See `bbdb/vm-get-from-headers' and 'bbdb/vm-get-only-first-from-p' for
configuration of what is being displayed."
  (save-excursion
    (let ((bbdb-gag-messages t)
          (bbdb-electric-p nil)
          (records (bbdb/vm-update-records offer-to-create))
          (bbdb-elided-display (bbdb-pop-up-elided-display))
          (bbdb-buffer-name bbdb-buffer-name))

      (when (and bbdb-use-pop-up records)
        (bbdb-pop-up-bbdb-buffer
         (function (lambda (w)
                     (let ((b (current-buffer)))
                       (set-buffer (window-buffer w))
                       (prog1 (eq major-mode 'vm-mode)
                         (set-buffer b))))))

        ;; Always update the records; if there are no records, empty the
        ;; BBDB window. This should be generic, not VM-specific.
        (bbdb-display-records records))
      (when (not records)
        (bbdb-undisplay-records)
        (if (get-buffer-window bbdb-buffer-name)
            (delete-window (get-buffer-window bbdb-buffer-name)))))))


;; By Alastair Burt <burt@dfki.uni-kl.de>
;; vm 5.40 and newer support a new summary format, %U<letter>, to call
;; a user-provided function.  Use "%-17.17UB" instead of "%-17.17F" to
;; have your VM summary buffers display BBDB's idea of the sender's full
;; name instead of the name (or lack thereof) in the message itself.

(defun vm-summary-function-B (m &optional to-p)
  "Given a VM message returns the BBDB name of the sender.
Respects vm-summary-uninteresting-senders."
  (if (and vm-summary-uninteresting-senders (not to-p))
      (let ((case-fold-search nil))
    (if (string-match vm-summary-uninteresting-senders (vm-su-from m))
        (concat vm-summary-uninteresting-senders-arrow
            (vm-summary-function-B m t))
      (or (bbdb/vm-alternate-full-name  (vm-su-from m))
          (vm-su-full-name m))))
    (or (bbdb/vm-alternate-full-name (if to-p (vm-su-to m) (vm-su-from m)))
    (vm-decode-mime-encoded-words-in-string
     (if to-p (vm-su-to-names m) (vm-su-full-name m))))))

(defun bbdb/vm-alternate-full-name (address)
  (if address
      (let ((entry (bbdb-search-simple
            nil
            (if (and address bbdb-canonicalize-net-hook)
            (bbdb-canonicalize-address address)
              address))))
    (if entry
        (or (bbdb-record-getprop entry 'mail-name)
        (bbdb-record-name entry))))))


;; From: Mark Thomas <mthomas@jprc.com>
;; Subject: auto-folder-alist from bbdb

;;;###autoload
(defcustom bbdb/vm-set-auto-folder-alist-field 'vm-folder
  "*The field which `bbdb/vm-set-auto-folder-alist' searches for."
  :group 'bbdb
  :type 'symbol)

;;;###autoload
(defun bbdb/vm-set-auto-folder-alist ()
  "Create a `vm-auto-folder-alist' according to the records in the bbdb.
For each record that has a 'vm-folder' attribute, add an
\(email-regexp .  folder) element to the `vm-auto-folder-alist'.

The element gets added to the 'element-name' sublist of the
`vm-auto-folder-alist'.

The car of the element consists of all the email addresses for the
bbdb record concatenated with OR; the cdr is the value of the
vm-folder attribute.
If the fist character of vm-folders value is a quote (') it will be
parsed a lisp expression and consequently one may do his own tweaks
in order to get a nice folder name.

The only processing this defun does to the email address is to
`regexp-quote' it; if you're email circle is small enough, you could
consider using just the user part of the email address --- the part
before the @."
  (interactive)
  (let* (;; we add the email-address/vm-folder-name pair to this
     ;; sublist of the vm-auto-folder-alist variable
     (element-name "from\\|to\\|cc")
     ;; grab the folder list from the vm-auto-folder-alist
     (folder-list (assoc element-name vm-auto-folder-alist))
     ;; the raw-notes and vm-folder attributes of the current bbdb
     ;; record
     notes-field folder
     ;; a regexp matching all the email addresses from the bbdb
         ;; record
     email-regexp
     )
    ;; create the folder-list in vm-auto-folder-alist if it doesn't exist
    (unless folder-list
      (setq vm-auto-folder-alist (append vm-auto-folder-alist
                                         (list (list element-name)))
            folder-list (assoc element-name vm-auto-folder-alist)))
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-raw-notes record))
      (when (and (listp notes-field)
         (setq folder (cdr (assq bbdb/vm-set-auto-folder-alist-field
                     notes-field))))
        ;; quote all the email addresses for the record and join them
        ;; with OR
    (setq email-regexp (mapconcat '(lambda (addr)
                     (regexp-quote addr))
                      (bbdb-record-net record) "\\|"))
    (unless (or (zerop (length email-regexp))
            (assoc email-regexp folder-list))
      ;; be careful: nconc modifies the list in place
      (if (equal (elt folder 0) ?\')
          (setq folder (read folder)))
      (nconc folder-list (list (cons email-regexp folder))))))))


(defcustom bbdb/vm-snarf-all-headers
  bbdb/vm-get-from-headers
  "*List of headers to look for new email-addresses by `bbdb/vm-snarf-all'."
  :group 'bbdb-mua-specific-vm
  :type 'list)

;;;###autoload
(defun bbdb/vm-snarf-all ()
  "Snarfs all email addresses from the headers.
The headers specified in `bbdb/vm-snarf-all-headers' are searched
for new email addresses."
  (interactive)

  (vm-check-for-killed-folder)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)

  (let ((bbdb/vm-get-from-headers bbdb/vm-snarf-all-headers)
    (bbdb/vm-get-only-first-from-p nil)
    (bbdb-message-cache nil))
    (bbdb/vm-pop-up-bbdb-buffer t)))


;;; bbdb/vm-auto-add-label
;;; Howard Melman, contributed Jun 16 2000
(defcustom bbdb/vm-auto-add-label-list nil
  "*List used by `bbdb/vm-auto-add-label' to automatically label messages.
Each element in the list is either a string or a list of two strings.
If a single string then it is used as both the field value to check for
and the label to apply to the message.  If a list of two strings, the first
is the field value to search for and the second is the label to apply."
  :group 'bbdb-mua-specific-vm
  :type 'list)

(defcustom bbdb/vm-auto-add-label-field bbdb-define-all-aliases-field
  "*Fields used by `bbdb/vm-auto-add-label' to automatically label messages.
Value is either a single symbol or a list of symbols of bbdb fields that
`bbdb/vm-auto-add-label' uses to check for labels to apply to messages.
Defaults to `bbdb-define-all-aliases-field' which is typically `mail-alias'."
  :group 'bbdb-mua-specific-vm
  :type '(choice symbol list))

(defun bbdb/vm-auto-add-label (record)
  "Automatically add labels to messages based on the mail-alias field.
Add this to `bbdb-notice-hook' and if using VM each message that bbdb
notices will be checked.  If the sender has a value in the
bbdb/vm-auto-add-label-field  in their BBDB record that
matches a value in `bbdb/vm-auto-add-label-list' then a VM
label will be added to the message.

This works great when `bbdb-user-mail-names' is set.  As a result
mail that you send to people (and copy yourself on) is labeled as well.

This is how you hook it in.
;;   (add-hook 'bbdb-notice-hook 'bbdb/vm-auto-add-label)
"
  (let (field aliases sep)
    (and (eq major-mode 'vm-mode)
     (mapcar #'(lambda(x)
             (and
              (setq field (bbdb-record-getprop record x))
              (setq sep (or (get x 'field-separator) ","))
              (setq aliases (append aliases (bbdb-split field sep)))))
         (cond ((listp bbdb/vm-auto-add-label-field)
            bbdb/vm-auto-add-label-field)
               ((symbolp bbdb/vm-auto-add-label-field)
            (list bbdb/vm-auto-add-label-field))
               (t (error "Bad value for bbdb/vm-auto-add-label-field"))
               ))
     (vm-add-message-labels
      (mapconcat #'(lambda (l)
             (cond ((stringp l)
                (if (member l aliases)
                    l))
                   ((and (consp l)
                     (stringp (car l))
                     (stringp (cdr l)))
                (if (member (car l) aliases)
                    (cdr l)))
                   (t
                (error "Malformed bbdb/vm-auto-add-label-list")
                )))
             bbdb/vm-auto-add-label-list
             " ")
      1))))


;;;###autoload
(defun bbdb-insinuate-vm ()
  "Call this function to hook BBDB into VM."
  (cond ((boundp 'vm-select-message-hook) ; VM 5.36+
     (add-hook 'vm-select-message-hook 'bbdb/vm-pop-up-bbdb-buffer))
    ((boundp 'vm-show-message-hook) ; VM 5.32.L+
     (add-hook 'vm-show-message-hook 'bbdb/vm-pop-up-bbdb-buffer))
    (t
     (error "vm versions older than 5.36 no longer supported")))
  (define-key vm-mode-map ":" 'bbdb/vm-show-sender)
   ;;  (define-key vm-mode-map "'" 'bbdb/vm-show-all-recipients) ;; not yet
  (define-key vm-mode-map ";" 'bbdb/vm-edit-notes)
  (define-key vm-mode-map "/" 'bbdb)
  ;; VM used to inherit from mail-mode-map, so bbdb-insinuate-sendmail
  ;; did this.  Kyle, you loser.
  (if (boundp 'vm-mail-mode-map)
      (define-key vm-mail-mode-map "\M-\t" 'bbdb-complete-name)))

(provide 'bbdb-vm)
