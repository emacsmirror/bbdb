;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to RMAIL.  See bbdb.texinfo.
;;; last change  8-sep-92.

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

(require 'bbdb)
(require 'rmail)
;(require 'rmailsum)   ; not provided, dammit!
(if (not (fboundp 'rmail-make-summary-line)) (load-library "rmailsum"))

(defun bbdb/rmail-update-record (&optional offer-to-create)
  "returns the record corresponding to the current RMAIL message, creating or
modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (if bbdb-use-pop-up
      (bbdb/rmail-pop-up-bbdb-buffer offer-to-create)
    (if (and (boundp 'rmail-buffer) rmail-buffer)
	(set-buffer rmail-buffer))
    (if rmail-current-message
      (or (bbdb-message-cache-lookup rmail-current-message nil)
	(save-excursion
         (let ((from (mail-fetch-field "from"))
	       name net)
	  (if (or (null from)
		  (string-match (bbdb-user-mail-names)
				(mail-strip-quoted-names from)))
	      ;; if logged-in user sent this, use recipients.
	      (setq from (or (mail-fetch-field "to") from)))
	  (if from
	      (bbdb-encache-message rmail-current-message
		(bbdb-annotate-message-sender from t
	          (or (bbdb-invoke-hook-for-value bbdb/mail-auto-create-p)
		      offer-to-create))))))))))

(defun bbdb/rmail-annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message."
  (interactive (list (if bbdb-readonly-p
			 (error "The Insidious Big Brother Database is read-only.")
			 (read-string "Comments: "))))
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer))
  (bbdb-annotate-notes (bbdb/rmail-update-record t) string))

(defun bbdb/rmail-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (bbdb/rmail-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))


(defun bbdb/rmail-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer))
  (let ((record (bbdb/rmail-update-record t)))
    (if record
	(bbdb-display-records (list record))
	(error "unperson"))))


(defun bbdb/rmail-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the RMAIL window(s),
displaying the record corresponding to the sender of the current message."
  (bbdb-pop-up-bbdb-buffer
    (function (lambda (w)
      (let ((b (current-buffer)))
	(set-buffer (window-buffer w))
	(prog1 (eq major-mode 'rmail-mode)
	  (set-buffer b))))))
  (let ((bbdb-gag-messages t)
	(bbdb-use-pop-up nil)
	(bbdb-electric-p nil))
    (let ((record (bbdb/rmail-update-record offer-to-create))
	  (bbdb-elided-display (bbdb-pop-up-elided-display))
	  (b (current-buffer)))
      (bbdb-display-records (if record (list record) nil))
      (set-buffer b)
      record)))

(defun bbdb/rmail-expunge ()
  "Actually erase all deleted messages in the file."
  (interactive)
  (setq bbdb-message-cache nil)
  (bbdb-orig-rmail-expunge))

(defun bbdb/undigestify-rmail-message ()
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  (interactive)
  (setq bbdb-message-cache nil)
  (bbdb-orig-undigestify-rmail-message))

(defun bbdb-insinuate-rmail ()
  "Call this function to hook BBDB into RMAIL."
  (define-key rmail-mode-map ":" 'bbdb/rmail-show-sender)
  (define-key rmail-mode-map ";" 'bbdb/rmail-edit-notes)
  (define-key rmail-summary-mode-map ":" 'bbdb/rmail-show-sender)
  (define-key rmail-summary-mode-map ";" 'bbdb/rmail-edit-notes)
  
  (bbdb-add-hook 'rmail-show-message-hook 'bbdb/rmail-update-record)
  
  ;; We must patch into rmail-expunge to clear the cache, since expunging a 
  ;; message invalidates the cache (which is based on message numbers).
  ;; Same for undigestifying.
  (or (fboundp 'bbdb-orig-rmail-expunge)
      (fset 'bbdb-orig-rmail-expunge (symbol-function 'rmail-expunge)))
  (fset 'rmail-expunge 'bbdb/rmail-expunge)

  (or (fboundp 'undigestify-rmail-message)
      (autoload 'undigestify-rmail-message "undigest" nil t))
  (if (eq (car-safe (symbol-function 'undigestify-rmail-message)) 'autoload)
      (load (nth 1 (symbol-function 'undigestify-rmail-message))))
  (or (fboundp 'bbdb-orig-undigestify-rmail-message)
      (fset 'bbdb-orig-undigestify-rmail-message
	    (symbol-function 'undigestify-rmail-message)))
  (fset 'undigestify-rmail-message 'bbdb/undigestify-rmail-message)
  )

(provide 'bbdb-rmail)
