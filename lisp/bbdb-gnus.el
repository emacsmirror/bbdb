;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to GNUS version 3.12 or greater.  See bbdb.texinfo.

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
;; $Log$
;; Revision 1.55  1997/11/02 07:37:44  simmonmt
;; Added REPLACE argument to bbdb/gnus-annotate-sender.  Variable to
;; catch changes to bbdb/gnus-score-default.  Commented out most of score
;; insinuation code.  Variable aliases.  Added in-bbdb format letter.
;;
;; Revision 1.54  1997/10/26 04:56:51  simmonmt
;; Integration of Brian Edmonds <edmonds@cs.ubc.ca>'s gnus-bbdb.el.  Got
;; scoring and summary buffer stuff.  Need to do splitting
;;
;; Revision 1.53  1997/10/11 23:57:24  simmonmt
;; Created bbdb-insinuate-message to set M-t binding in message-mode so I
;; don't have to load gnus first.
;;
;; Revision 1.52  1997/09/28 06:00:17  simmonmt
;; Fix to accomodate nil gnus-single-article-buffer
;;
;;

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
	     (set-buffer gnus-article-buffer)
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

(defun bbdb/gnus-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive (list (if bbdb-readonly-p
			 (error "The Insidious Big Brother Database is read-only.")
			 (read-string "Comments: "))))
  (bbdb-annotate-notes (bbdb/gnus-update-record t) string 'notes replace))

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

;;
;; Announcing BBDB entries in the summary buffer
;;

(defvar bbdb/gnus-lines-and-from-length 18
  "*The number of characters used to display From: info in GNUS, if you have
set gnus-optional-headers to 'bbdb/gnus-lines-and-from.")

(defvar bbdb/gnus-summary-prefer-real-names nil
  "*If T, then the GNUS subject list will display real names instead of network
addresses (gnus-optional-headers is 'bbdb/gnus-lines-and-from.)")
(defvaralias 'bbdb/gnus-header-prefer-real-names
  'bbdb/gnus-summary-prefer-real-names)

(defvar bbdb/gnus-summary-mark-known-posters t
  "*If T, then the GNUS subject list will contain an indication of those 
messages posted by people who have entries in the Insidious Big
Brother Database (assuming `gnus-optional-headers' is
'bbdb/gnus-lines-and-from. [for those versions of Gnus that still have
`gnus-optional-headers'])")
(defvaralias 'bbdb/gnus-mark-known-posters
  'bbdb/gnus-summary-mark-known-posters)

(defvar bbdb/gnus-summary-known-poster-mark "+"
  "This is the default character to prefix author names with if
gnus-bbdb-summary-mark-known-posters is t.  If the poster's record has
an entry in the field named by bbdb-message-marker-field, then that will
be used instead.")

(defvar bbdb/gnus-summary-show-bbdb-names t
  "*If both this variable and bbdb/gnus-summary-prefer-real-names are true,
then for messages from authors who are in your database, the name
displayed will be the primary name in the database, rather than the
one in the From line of the message.  This doesn't affect the names of
people who aren't in the database, of course.  (gnus-optional-headers
must be bbdb/gnus-lines-and-from.)")
(defvaralias 'bbdb/gnus-header-show-bbdb-names
  'bbdb/gnus-summary-show-bbdb-names)

(defvar bbdb/gnus-summary-prefer-bbdb-data t
  "If t, then for posters who are in our BBDB, replace the information
provided in the From header with data from the BBDB.")

(defvar bbdb/gnus-summary-prefer-real-names t
  "If t, then display the poster's name from the BBDB if we have one,
otherwise display his/her primary net address if we have one.  If it is
set to the symbol bbdb, then real names will be used from the BBDB if
present, otherwise the net address in the post will be used.  If
bbdb/gnus-summary-prefer-bbdb-data is nil, then this has no effect.")
(defvaralias 'bbdb/gnus-header-prefer-real-names
  'bbdb/gnus-summary-prefer-real-names)

(defvar bbdb/gnus-summary-user-format-letter "B"
  "This is the gnus-user-format-function- that will be used to insert
the information from the BBDB in the summary buffer (using
`bbdb/gnus-summary-get-author').  This format code is meant to replace
codes that insert sender names or addresses (like %A or %n). Unless
you've alread got other code using user format B, you might as well
stick with the default.  Additionally, if the value of this variable
is nil, no format function will be installed for
`bbdb/gnus-summary-get-author'.  See also
`bbdb/gnus-summary-in-bbdb-format-letter', which installs a format
code for `bbdb/gnus-summary-author-in-bbdb'")

(defvar bbdb/gnus-summary-in-bbdb-format-letter "b"
  "This is the gnus-user-format-function- that will be used to insert
`bbdb/gnus-summary-known-poster-mark' (using
`bbdb/gnus-summary-author-in-bbdb') if the poster is in the BBDB, and
\" \" if not.  If the value of this variable is nil, no format code
will be installed for `bbdb/gnus-summary-author-in-bbdb'.  See also
`bbdb/gnus-summary-user-format-letter', which installs a format code
for `bbdb/gnus-summary-get-author'.")

(defvar bbdb-message-marker-field 'mark-char
  "*The field whose value will be used to mark messages by this user in GNUS.")

(defun bbdb/gnus-lines-and-from (header)
  "Useful as the value of gnus-optional-headers in *GNUS* (not Gnus).
NOTE: This variable no longer seems to be present in Gnus.  It seems
to have been replaced by `message-default-headers', which only takes
strings.  In the future this should change."
  (let* ((length bbdb/gnus-lines-and-from-length)
	 (lines (nntp-header-lines header))
	 (from (nntp-header-from header))
	 (data (and (or bbdb/gnus-summary-mark-known-posters
			bbdb/gnus-summary-show-bbdb-names)
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

    (setq name 
	  (or (and bbdb/gnus-summary-prefer-bbdb-data
		   (or (and bbdb/gnus-summary-prefer-real-names
			    (and record (bbdb-record-name record)))
		       (and record (bbdb-record-net record)
			    (nth 0 (bbdb-record-net record)))))
	      (and bbdb/gnus-summary-prefer-real-names
		   (or (and (equal bbdb/gnus-summary-prefer-real-names 'bbdb)
			    net)
		       name))
	      net from "**UNKNOWN**"))
      ;; GNUS can't cope with extra square-brackets appearing in the summary.
      (if (and name (string-match "[][]" name))
	  (progn (setq name (copy-sequence name))
		 (while (string-match "[][]" name)
		   (aset name (match-beginning 0) ? ))))
      (setq string (format "%s%3d:%s"
			   (if (and record bbdb/gnus-summary-mark-known-posters)
			       (or (bbdb-record-getprop
				    record bbdb-message-marker-field)
				   "*")
			     " ")
			   lines (or name from))
	    L (length string))
      (cond ((> L length) (substring string 0 length))
	    ((< L length) (concat string (make-string (- length L) ? )))
	    (t string))))

(defun bbdb/gnus-summary-get-author (header)
  "Given a GNUS message header, returns the appropriate piece of
information to identify the author in a GNUS summary line, depending on
the settings of the various configuration variables.  See the
documentation for the following variables for more details:
  `bbdb/gnus-summary-mark-known-posters'
  `bbdb/gnus-summary-known-poster-mark'
  `bbdb/gnus-summary-prefer-bbdb-data'
  `bbdb/gnus-summary-prefer-real-names'
This function is meant to be used with the user function defined in
  `bbdb/gnus-summary-user-format-letter'"
  (let* ((from (mail-header-from header))
	 (data (and (or bbdb/gnus-summary-mark-known-posters
			bbdb/gnus-summary-show-bbdb-names)
		    (condition-case ()
			(mail-extract-address-components from)
		      (error nil))))
	 (name (car data))
	 (net (car (cdr data)))
	 (record (and data 
		      (bbdb-search-simple name 
		       (if (and net bbdb-canonicalize-net-hook)
			   (bbdb-canonicalize-address net)
			 net)))))
    (if (and record name (member (downcase name) (bbdb-record-net record)))
	;; bogon!
	(setq record nil))
    (setq name 
	  (or (and bbdb/gnus-summary-prefer-bbdb-data
		   (or (and bbdb/gnus-summary-prefer-real-names
			    (and record (bbdb-record-name record)))
		       (and record (bbdb-record-net record)
			    (nth 0 (bbdb-record-net record)))))
	      (and bbdb/gnus-summary-prefer-real-names
		   (or (and (equal bbdb/gnus-summary-prefer-real-names 'bbdb)
			    net)
		       name))
	      net from "**UNKNOWN**"))
    (format "%s%s"
	    (or (and record bbdb/gnus-summary-mark-known-posters
		     (or (bbdb-record-getprop
			  record bbdb-message-marker-field)
			 bbdb/gnus-summary-known-poster-mark))
		" ")
	    name)))

(defun bbdb/gnus-summary-author-in-bbdb (header)
  "Given a Gnus message header, returns
  `bbdb/gnus-summary-known-poster-mark' if the poster is in the BBDB,
  \" \" otherwise."
  (let* ((from (mail-header-from header))
	 (data (condition-case ()
		   (mail-extract-address-components from)
		 (error nil)))
	 (name (car data))
	 (net (cadr data)))
    (if (and data (bbdb-search-simple name
		   (if (and net bbdb-canonicalize-net-hook)
		       (bbdb-canonicalize-address net)
		     net)))
	bbdb/gnus-summary-known-poster-mark " ")))

;;
;; Scoring
;;

(defvar bbdb/gnus-score-field 'gnus-score
  "This variable contains the name of the BBDB field which should be
checked for a score to add to the net addresses in the same record.")

(defvar bbdb/gnus-score-default nil
  "If this is set, then every net address in the BBDB that does not have
an associated score field will be assigned this score.  A value of nil
implies a default score of zero.")

(defvar bbdb/gnus-score-default-internal nil
  "Internal variable for detecting changes to
`bbdb/gnus-score-default'.  You should not set this variable directly -
set `bbdb/gnus-score-default' instead.")

(defvar bbdb/gnus-score-alist nil
  "The text version of the scoring structure returned by
bbdb/gnus-score.  This is built automatically from the BBDB.")

(defvar bbdb/gnus-score-rebuild-alist t
  "Set to t to rebuild bbdb/gnus-score-alist on the next call to
bbdb/gnus-score.  This will be set automatically if you change a BBDB
record which contains a gnus-score field.")

(defun bbdb/gnus-score-invalidate-alist (rec)
  "This function is called through bbdb-after-change-hook, and sets
bbdb/gnus-score-rebuild-alist to t if the changed record contains a
gnus-score field."
  (if (bbdb-record-getprop rec bbdb/gnus-score-field)
      (setq bbdb/gnus-score-rebuild-alist t)))

;;;###autoload
(defun bbdb/gnus-score (group)
  "This returns a score alist for GNUS.  A score pair will be made for
every member of the net field in records which also have a gnus-score
field.  This allows the BBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile."
  (list (list
   (condition-case nil
       (read (bbdb/gnus-score-as-text group))
     (error (setq bbdb/gnus-score-rebuild-alist t)
	    (message "Problem building BBDB score table.")
	    (ding) (sit-for 2)
	    nil)))))

(defun bbdb/gnus-score-as-text (group)
  "Returns a SCORE file format string built from the BBDB."
  (when (or (when (/= (or bbdb/gnus-score-default 0)
		      (or bbdb/gnus-score-default-internal 0))
	      (setq bbdb/gnus-score-default-internal
		    bbdb/gnus-score-default)
	      t)
	    (not bbdb/gnus-score-alist)
	    bbdb/gnus-score-rebuild-alist)
    (setq bbdb/gnus-score-rebuild-alist nil)
    (setq bbdb/gnus-score-alist
	  (concat "((touched nil) (\"from\"\n"
		  (mapconcat
		   (lambda (rec)
		     (let ((score (or (bbdb-record-getprop rec
							   bbdb/gnus-score-field)
				      bbdb/gnus-score-default))
			   (net (bbdb-record-net rec)))
		       (if (not (and score net)) nil
			 (mapconcat
			  (lambda (addr)
			    (concat "(\"" addr "\" " score ")\n"))
			  net ""))))
		   (bbdb-records) "")
		  "))")))
  bbdb/gnus-score-alist)

;;
;; Insinuation
;;

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
	 (define-key gnus-summary-mode-map ";" 'bbdb/gnus-edit-notes)))

  ;; Set up user field for use in gnus-summary-line-format
  (let ((get-author-user-fun (intern
			      (concat "gnus-user-format-function-"
				      bbdb/gnus-summary-user-format-letter)))
	(in-bbdb-user-fun (intern
			   (concat "gnus-user-format-function-"
				   bbdb/gnus-summary-in-bbdb-format-letter))))
					; The big one - whole name
    (when bbdb/gnus-summary-user-format-letter
      (if (and (fboundp get-author-user-fun)
	       (not (eq (symbol-function get-author-user-fun)
			'bbdb/gnus-summary-get-author)))
	  (bbdb-warn (concat
		      "Redefine bbdb/gnus-summary-user-format-letter, "
		      "%%u" bbdb/gnus-summary-user-format-letter " in Gnus, "
		      "or set bbdb/gnus-define-format-functions to nil."))
	(fset get-author-user-fun 'bbdb/gnus-summary-get-author)))
    
    ; One tick.  One tick only, please
    (when bbdb/gnus-summary-in-bbdb-format-letter
      (if (and (fboundp in-bbdb-user-fun)
	       (not (eq (symbol-function in-bbdb-user-fun)
			'bbdb/gnus-summary-author-in-bbdb)))
	  (bbdb-warn (concat
		      "Redefine bbdb/gnus-summary-in-bbdb-format-letter, "
		      "%%u" bbdb/gnus-summary-in-bbdb-format-letter " in Gnus,"
		      " or set bbdb/gnus-define-format-functions to nil."))
	(fset in-bbdb-user-fun 'bbdb/gnus-summary-author-in-bbdb))))

  ;; Scoring
  (add-hook 'bbdb-after-change-hook 'bbdb/gnus-score-invalidate-alist)
;  (setq gnus-score-find-score-files-function
;	(if (boundp 'gnus-score-find-score-files-function)
;	    (cond ((functionp gnus-score-find-score-files-function)
;		   (list gnus-score-find-score-files-function
;			 'bbdb/gnus-score))
;		  ((listp gnus-score-find-score-files-function)
;		   (append gnus-score-find-score-files-function
;			   'bbdb/gnus-score))
;		  (t 'bbdb/gnus-score))
;	  'bbdb/gnus-score))
  )

(defun bbdb-insinuate-message ()
  "Call this function to hook BBDB into message-mode."
  (define-key message-mode-map "\M-\t" 'bbdb-complete-name))

(provide 'bbdb-gnus)
