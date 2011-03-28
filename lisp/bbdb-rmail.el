;;; bbdb-rmail.el --- BBDB interface to Rmail

;; Copyright (C) 1991, 1992 Jamie Zawinski <jwz@netscape.com>.
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
;;; This file contains the BBDB interface to Rmail.
;;; See bbdb.texinfo for documentation.

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'rmail)
(require 'rmailsum)
(require 'mailheader)

(defcustom bbdb/rmail-update-records-p
  (lambda () (if (bbdb/rmail-new-flag) (bbdb-select-message) 'search))
  "Controls how `bbdb/rmail-update-records' processes mail addresses.
Set this to an expression which evaluates to 'search, t. or nil.
When set to t mail addresses will be fed to
`bbdb-annotate-message' in order to update existing records or create
new ones.  A value of 'search will search just for existing records having
the right mail.  A value of nil will not do anything.

The default is to annotate only new messages."
  :group 'bbdb-mua-rmail
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" search)
                 (const :tag "annotate all messages" t)
                 (const :tag "query annotation of all messages" query)
                 (const :tag "annotate (query) only new messages"
                        (lambda () (if (bbdb/rmail-new-flag)
                                       (bbdb-select-message) 'search)))
                 (const :tag "accept messages" bbdb-accept-message)
                 (const :tag "ignore messages" bbdb-ignore-message)
                 (const :tag "select messages" bbdb-select-message)
                 (sexp  :tag "user defined")))

(defun bbdb/rmail-new-flag ()
  "Returns t if the current message in buffer BUF is new."
  (rmail-message-labels-p rmail-current-message ", ?\\(unseen\\),"))

(defun bbdb/rmail-header (header)
  "Pull HEADER out of Rmail header."
  (with-current-buffer rmail-buffer
    (if (fboundp 'rmail-get-header)  ; Emacs 23
        (rmail-get-header header)
      (save-restriction
        (with-no-warnings (rmail-narrow-to-non-pruned-header))
        (mail-header (intern-soft (downcase header))
                     (mail-header-extract))))))

;;;###autoload
(defun bbdb/rmail-update-records (&optional update-p)
  "Rmail wrapper for `bbdb-update-records'.
Return the records corresponding to the current Rmail message,
creating or modifying them as necessary.
UPDATE-P may take the same values as in `bbdb-update-records'.
If UPDATE-P is nil, use the value of `bbdb/rmail-update-records-p'."
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer)
    (error "Not in an rmail buffer"))
  (when rmail-current-message
    (let (records)
      (unless update-p
        (setq update-p
              (if (functionp bbdb/rmail-update-records-p)
                  (funcall bbdb/rmail-update-records-p)
                bbdb/rmail-update-records-p)))
      ;; ignore cache if we may be creating a record, since the cache
      ;; may otherwise tell us that the user did not want a record for
      ;; this person.
      (unless (member update-p '(t query))
        (setq records (bbdb-message-get-cache rmail-current-message)))
      (unless records
        (setq records (bbdb-update-records
                       (bbdb-get-address-components
                        'bbdb/rmail-header user-mail-address)
                       update-p))
        (bbdb-message-set-cache rmail-current-message records))
      (if bbdb-message-all-addresses
          records
        (if records (list (car records)))))))

(defun bbdb/rmail-pop-up-bbdb-buffer (&optional update-p)
  "Make the *BBDB* buffer be displayed along with the RMAIL window(s).
Displays the records corresponding to the sender respectively
recipients of the current message.
See `bbdb-message-headers' and `bbdb-message-all-addresses'
for configuration of what is being displayed.
Intended for noninteractive use via `rmail-show-message-hook'."
  (if bbdb-message-pop-up
      (let ((bbdb-silent-internal t)
            (records (bbdb/rmail-update-records update-p)))
        (if records
            (bbdb-display-records-internal
             records nil nil nil
             (lambda (window)
               (let ((buffer (current-buffer)))
                 (set-buffer (window-buffer window))
                 (prog1 (eq major-mode 'rmail-mode)
                   (set-buffer buffer)))))
          ;; If there are no records, empty the BBDB window.
          (bbdb-undisplay-records)))))

;;;###autoload
(defun bbdb/rmail-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive (progn (bbdb-editable) (list (read-string "Comments: "))))
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer))
  (dolist (record (bbdb/rmail-update-records))
    (bbdb-annotate-notes record string 'notes replace)))

(defun bbdb/rmail-edit-notes (&optional field)
  "Edit the notes FIELD of the BBDB record corresponding to the sender
of this message.
If called interactively, FIELD defaults to 'notes. With a prefix arg,
ask interactively for FIELD."
  (interactive (list (unless current-prefix-arg 'notes)))
  (let ((records (bbdb/rmail-update-records)))
    (bbdb-display-records records)
    (dolist (record records)
      (bbdb-record-edit-notes record field t))))

;; ;;;###autoload
;; (defun bbdb/rmail-show-sender ()
;;   "Display the BBDB record(s) for the sender of this message.
;;   (interactive)
;;   (if (and (boundp 'rmail-buffer) rmail-buffer)
;;       (set-buffer rmail-buffer))
;;   (let ((record (bbdb/rmail-update-record)))
;;     (if record
;;         (bbdb-display-records (list record))
;;         (error "unperson"))))

;;;###autoload
(defun bbdb-insinuate-rmail ()
  "Call this function to hook BBDB into RMAIL."
  (define-key rmail-mode-map ":" 'bbdb/rmail-show-sender)
  (define-key rmail-mode-map ";" 'bbdb/rmail-edit-notes)
  (define-key rmail-summary-mode-map ":" 'bbdb/rmail-show-sender)
  (define-key rmail-summary-mode-map ";" 'bbdb/rmail-edit-notes)

  (add-hook 'rmail-show-message-hook 'bbdb/rmail-pop-up-bbdb-buffer)

  ;; We must patch into rmail-only-expunge to clear the cache, since
  ;; expunging a message invalidates the cache (which is based on
  ;; message numbers).
  (defadvice rmail-only-expunge (before bbdb/rmail-only-expunge)
    "Invalidate BBDB cache before expunging."
    (setq bbdb-message-cache nil))

  ;; Same for undigestifying.
  (or (fboundp 'undigestify-rmail-message)
      (autoload 'undigestify-rmail-message "undigest" nil t))
  (if (eq (car-safe (symbol-function 'undigestify-rmail-message)) 'autoload)
      (load (nth 1 (symbol-function 'undigestify-rmail-message))))
  (defadvice undigestify-rmail-message (before bbdb/undigestify-rmail-message)
    "Invalidate BBDB cache before undigestifying."
    (setq bbdb-message-cache nil)))

(provide 'bbdb-rmail)
