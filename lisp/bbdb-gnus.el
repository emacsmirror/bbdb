;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to GNUS version 3.12 or greater.  See bbdb.texinfo.
;;; last change 11-oct-93.

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
(require 'gnus)

(defun bbdb/gnus-update-record (&optional offer-to-create)
  "returns the record corresponding to the current GNUS message, creating 
or modifying it as necessary.  A record will be created if 
bbdb/news-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (if bbdb-use-pop-up
      (bbdb/gnus-pop-up-bbdb-buffer offer-to-create)
    (let ((from
	   (progn
	     (set-buffer "*Article*")
	     (save-restriction
	       (widen)
	       ;;(gnus-article-show-all-headers)
	       (narrow-to-region (point-min)
				 (progn (goto-char (point-min))
					(or (search-forward "\n\n" nil t)
					    (error "message unexists"))
					(- (point) 2)))
	       (mail-fetch-field "from")))))
      (if from
	  (bbdb-annotate-message-sender from t
					(or (bbdb-invoke-hook-for-value
					     bbdb/news-auto-create-p)
					    offer-to-create)
					offer-to-create)))))

(defun bbdb/gnus-annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message."
  (interactive (list (if bbdb-readonly-p
			 (error "The Insidious Big Brother Database is read-only.")
			 (read-string "Comments: "))))
  (bbdb-annotate-notes (bbdb/gnus-update-record t) string))

(defun bbdb/gnus-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (bbdb/gnus-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun bbdb/gnus-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (let ((record (bbdb/gnus-update-record t)))
    (if record
	(bbdb-display-records (list record))
	(error "unperson"))))


(defun bbdb/gnus-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the GNUS windows,
displaying the record corresponding to the sender of the current message."
  (let ((bbdb-gag-messages t)
	(bbdb-use-pop-up nil)
	(bbdb-electric-p nil))
    (let ((record (bbdb/gnus-update-record offer-to-create))
	  (bbdb-elided-display (bbdb-pop-up-elided-display))
	  (b (current-buffer)))
      ;; display the bbdb buffer iff there is a record for this article.
      (cond (record
	     (bbdb-pop-up-bbdb-buffer
	      (function (lambda (w)
			  (let ((b (current-buffer)))
			    (set-buffer (window-buffer w))
			    (prog1 (or (eq major-mode 'gnus-Article-mode)
				       (eq major-mode 'gnus-article-mode))
				   (set-buffer b))))))
	     (bbdb-display-records (list record)))
	    (t
	     (or bbdb-inside-electric-display
		 (not (get-buffer-window bbdb-buffer-name))
		 (let (w)
		   (delete-other-windows)
		   (if (assq 'article gnus-window-configuration) ; 3.15+
		       (gnus-configure-windows 'article)
		     (gnus-configure-windows 'SelectArticle))
		   (if (setq w (get-buffer-window
				(if (boundp 'gnus-summary-buffer)
				    gnus-summary-buffer
				  gnus-Subject-buffer)))
		       (select-window w))
		   ))))
      (set-buffer b)
      record)))

(defvar bbdb/gnus-lines-and-from-length 18
  "*The number of characters used to display From: info in GNUS, if you have
set gnus-optional-headers to 'bbdb/gnus-lines-and-from.")

(defvar bbdb/gnus-header-prefer-real-names nil
  "*If T, then the GNUS subject list will display real names instead of network
addresses (gnus-optional-headers is 'bbdb/gnus-lines-and-from.)")

(defvar bbdb/gnus-mark-known-posters t
  "*If T, then the GNUS subject list will contain an indication of those 
messages posted by people who have entries in the Insidious Big Brother 
Database (assuming gnus-optional-headers is 'bbdb/gnus-lines-and-from.)")

(defvar bbdb/gnus-header-show-bbdb-names t
  "*If both this variable and bbdb/gnus-header-prefer-real-names are true,
then for messages from folks who are in your database, the name displayed 
will be the primary name in the database, rather than the one in the From
line of the message.  This doesn't affect the names of people who aren't
in the database, of course.  (gnus-optional-headers must be
bbdb/gnus-lines-and-from.)")

(defvar bbdb-message-marker-field 'mark-char
  "*The field whose value will be used to mark messages by this user in GNUS.")


(defun bbdb/gnus-lines-and-from (header)
  "Useful as the value of gnus-optional-headers."
  (let* ((length bbdb/gnus-lines-and-from-length)
	 (lines (nntp-header-lines header))
	 (from (nntp-header-from header))
	 (data (and (or bbdb/gnus-mark-known-posters
			bbdb/gnus-header-show-bbdb-names)
		    (condition-case ()
			(mail-extract-address-components from)
		      (error nil))))
	 (name (car data))
	 (net (car (cdr data)))
	 (record (and data 
		      (bbdb-search-simple name 
		       (if (and net bbdb-canonicalize-net-hook)
			   (bbdb-canonicalize-address net)
			 net))))
	 string L)

    (if (and record name (member (downcase name) (bbdb-record-net record)))
	;; bogon!
	(setq record nil))

      (setq name (or (and bbdb/gnus-header-prefer-real-names
			  (or (and bbdb/gnus-header-show-bbdb-names record
				   (bbdb-record-name record))
			      name))
		     net))
      ;; GNUS can't cope with extra square-brackets appearing in the summary.
      (if (and name (string-match "[][]" name))
	  (progn (setq name (copy-sequence name))
		 (while (string-match "[][]" name)
		   (aset name (match-beginning 0) ? ))))
      (setq string (format "%s%3d:%s"
			   (if (and record bbdb/gnus-mark-known-posters)
			       (or (bbdb-record-getprop
				    record bbdb-message-marker-field)
				   "*")
			     " ")
			   lines (or name from))
	    L (length string))
      (cond ((> L length) (substring string 0 length))
	    ((< L length) (concat string (make-string (- length L) ? )))
	    (t string))))

(defun bbdb-insinuate-gnus ()
  "Call this function to hook BBDB into GNUS."
  (setq gnus-optional-headers 'bbdb/gnus-lines-and-from)
  (cond ((boundp 'gnus-Article-prepare-hook) ; 3.14 or worse
	 (bbdb-add-hook 'gnus-Article-prepare-hook 'bbdb/gnus-update-record)
	 (bbdb-add-hook 'gnus-Save-newsrc-hook 'bbdb-offer-save)
	 (define-key gnus-Subject-mode-map ":" 'bbdb/gnus-show-sender)
	 (define-key gnus-Subject-mode-map ";" 'bbdb/gnus-edit-notes))
	(t
	 (bbdb-add-hook 'gnus-article-prepare-hook 'bbdb/gnus-update-record)
	 (bbdb-add-hook 'gnus-save-newsrc-hook 'bbdb-offer-save)
	 (define-key gnus-summary-mode-map ":" 'bbdb/gnus-show-sender)
	 (define-key gnus-summary-mode-map ";" 'bbdb/gnus-edit-notes))))

(provide 'bbdb-gnus)
