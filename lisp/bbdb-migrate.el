;;; -*- Mode:Emacs-Lisp -*-

;;; This file contains the migration functions for the Insidious Big
;;; Brother Database (aka BBDB), copyright (c) 1991, 1992, 1993, 1994
;;; Jamie Zawinski <jwz@netscape.com>.  See the file bbdb.texinfo for
;;; documentation.
;;;
;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
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
;;;

;;
;; $Id$
;;
;; $Log$
;; Revision 1.10  2000/06/30 19:12:36  sds
;; (bbdb-migrate): re-wrote using `mapcar' instead of `append'
;; this is linear instead of quadratic and avoids much consing
;;
;; Revision 1.9  2000/05/29 22:47:50  waider
;; *** empty log message ***
;;
;; Revision 1.8  2000/04/17 08:30:47  waider
;; omitted bracket on unmigrate for v5->v4
;;
;; Revision 1.7  2000/04/16 04:00:54  waider
;; * Added 5->4 unmigration
;;
;; Revision 1.6  2000/04/15 14:57:38  waider
;; * Fixed misplaced bracket in street migration code.
;;
;; Revision 1.5  2000/04/12 23:57:16  waider
;; * Added v5 migration. NB no back-migration yet.
;;
;; Revision 1.4  2000/04/05 16:45:07  bbdb-writer
;; * Added Alex's BBDB v4 format migration (country field)
;;
;; Revision 1.3  1998/10/10 18:47:21  simmonmt
;; From slbaur: Don't pass an integer to concat.
;; Format dates with number format - not string - so we get leading
;; zeros.
;;
;; Revision 1.2  1998/04/11 07:19:19  simmonmt
;; Colin Rafferty's patch adding autoload cookies back
;;
;; Revision 1.1  1998/01/06 06:06:06  simmonmt
;; Initial revision
;;
;;

(require 'bbdb)

;;; Migrating the BBDB

;; Features that have changed in the various database revs.  Format:
;;    ((VERSION . DIFFERENCES) ... )
(defconst bbdb-migration-features
  '((3 . "* Date format for `creation-date' and `timestamp' has changed,
  from \"dd mmm yy\" (ex: 25 Sep 97) to \"yyyy-mm-dd\" (ex: 1997-09-25).")
    (4 . "* Country field added.")
    (5 . "* More flexible street address.")))

;;;###autoload
(defun bbdb-migration-query (ondisk)
  "Ask if the database is to be migrated.  ONDISK is the version
number of the database as currently stored on disk.  Returns the
version for the saved database."
  (save-excursion
    (let ((wc (current-window-configuration))
	  (buf (get-buffer-create "*BBDB Migration Info*"))
	  (newfeatures bbdb-migration-features)
	  (first t)
	  win update)
      (set-buffer buf)
      (erase-buffer)
      (goto-char (point-min))
      (insert-string (format "BBDB new data version notice:
=============================

Your BBDB data is stored in an older format (version %d).  At this point,
you have the option of either upgrading or continuing to save your data
in your current format.  Please note that if you elect the latter option,
any changes made to your data using features intended for the newer
versions will be lost.  For your convenience, a list of file format
changes introduced after version %d is shown below:\n\n" ondisk ondisk))

      (while newfeatures
	(if (> (caar newfeatures) ondisk)
	  (insert-string (concat (if first (setq first nil) "\n\n")
				 "New features in database version "
				 (format "%d" (caar newfeatures))
				 ":\n\n" (cdar newfeatures))))
	(setq newfeatures (cdr newfeatures)))
      (setq win (display-buffer buf))
      (shrink-window-if-larger-than-buffer win)
      (setq update
	    (y-or-n-p (concat "Upgrade BBDB to version "
			      (format "%d" bbdb-file-format)
			      "? ")))
      (delete-window win)
      (kill-buffer buf)
      (set-window-configuration wc)
      (if update bbdb-file-format ondisk))))

;;;###autoload
(defun bbdb-migrate (records)
  "Migrate the BBDB from the version on disk (the car of
`bbdb-file-format-migration') to the current version (in
`bbdb-file-format')."
  (cond
   ;; Version 2 -> 3
   ((= (car bbdb-file-format-migration) 2)
    (mapcar (lambda (rec)
              (bbdb-migrate-record
               rec
               '((bbdb-record-raw-notes
                  bbdb-record-set-raw-notes
                  bbdb-migrate-change-dates))))
            records))
   ;; Version 3 -> 4
   ((= (car bbdb-file-format-migration) 3)
    (mapcar (lambda (rec)
              ;; Take the address-list, ie. the 5th field, and add the empty
              ;; string to each address.  This is the new country field.
              (aset rec 5 (mapcar (lambda (addr) (vconcat addr [""]))
                                  (aref rec 5)))
              rec)
            records))
   ;; Version 4 -> 5
   ((= (car bbdb-file-format-migration) 4)
    (mapcar (lambda (rec)
              ;; Do address changes
              (aset rec 5 (mapcar
                           (lambda (addr)
                             (vector (aref addr 0) ; tag
                                     (delete "" ; nuke empties
                                             (list (aref addr 1) ; street1
                                                   (aref addr 2) ; street2
                                                   (aref addr 3)));street3
                                     (aref addr 4) ; city
                                     (aref addr 5) ; state
                                     (aref addr 6) ; zip
                                     (aref addr 7))) ; country
                           (aref rec 5)))
              rec)
            records))

   ;; Unknown Version
   (t (error (format "BBDB Cannot migrate from unknown version %d"
					 (car bbdb-file-format-migration))))))

;;;###autoload
(defun bbdb-unmigrate-record (record)
  "Reverse-migrate a single record from the current version (in
`bbdb-file-format') to the version to be saved (the cdr of
`bbdb-file-format-migration')."
  (cond
   ;; Version 5 -> 4
   ((= (cdr bbdb-file-format-migration) 4)
    ;; Take all the old addresses, ie. the 5th field, and for each
    ;; address, render the third element (a list of streets) as three
    ;; vector elements (v4-style address). If there's more than 3
    ;; lines, everything remaining gets crammed into the third, using
    ;; commas to separate the bits. If there's less, fill out with nil.
    (let ((old-addr-list (aref record 5))
	  (new-addr-list))
      (while old-addr-list
	(let* ((old-addr (car old-addr-list))
	       (new-addr)
	       (streets (aref old-addr 1)))
	  (setq new-addr (vector (aref old-addr 0) ;; tag
				 (nth 0 streets)
				 (nth 1 streets)
				 (if (> (length streets) 3)
				     (let ((more-streets) (i 4))
				       (while (<= i (length streets))
					 (add-to-list 'more-streets (nth i streets)))
				       (mapconcat 'concat more-streets ", "))
				   (nth 3 streets))
				 (aref old-addr 2) ;; city
				 (aref old-addr 3) ;; state
				 (aref old-addr 4) ;; zip
				 (aref old-addr 5))) ;; country
	  (setq old-addr-list (cdr old-addr-list))
	  (setq new-addr-list (append new-addr-list (list new-addr)))))
      (aset record 5 new-addr-list)))

   ;; Version 4 -> 3
   ((= (cdr bbdb-file-format-migration) 3)
    ;; Take all the old addresses, ie. the 5th field, and for each
    ;; address, copy all but the last string to the new address.  This
    ;; was the country of version 4.  Some version 4 zip codes will be
    ;; illegal version 3 (as used in 2.00.06) zip codes.  This problem
    ;; has not been solved.
    (let ((old-addr-list (aref record 5))
	  (new-addr-list))
      (while old-addr-list
	(let* ((old-addr (car old-addr-list))
	       (len (1- (length old-addr)))
	       (new-addr (make-vector len nil))
	       (i 0))
	  (setq old-addr-list (cdr old-addr-list))
	  (while (< i len)
	    (aset new-addr i (aref old-addr i))
	    (setq i (1+ i)))
	  (setq new-addr-list (append new-addr-list (list new-addr)))))
      (aset record 5 new-addr-list)))
   ;; Version 3 -> 2
   ((= (cdr bbdb-file-format-migration) 2)
    (bbdb-migrate-record record '((bbdb-record-raw-notes
								   bbdb-record-set-raw-notes
								   bbdb-unmigrate-change-dates))))
   (t (error (format "BBDB Cannot unmigrate to unknown version %d"
					 (car bbdb-file-format-migration))))))

(defun bbdb-migrate-record (rec changes)
  "Perform changes on a single database record (passed in REC).
CHANGES is a function containing entries of the form

        (GET SET FUNCTION)

where GET is the function to be used to retrieve the field to be
modified, and SET is the function to be used to set the field to be
modified.  FUNCTION will be applied to the result of GET, and its
results will be saved with SET."

;  (message "bbdb-migrate-record %s %s" (prin1-to-string rec)
;	   (prin1-to-string changes))
  (let (a b)
    (while changes
      ;; Whee!!!
      (setq a (eval (list (nth 0 (car changes)) rec)))
      (setq b (eval (list (nth 2 (car changes)) (quote a))))
      (eval (list (nth 1 (car changes)) rec (quote b)))
      (setq changes (cdr changes))))
  rec)

(defun bbdb-migrate-change-dates (rec)
  "Change date formats in timestamp and creation-date fields from
\"dd mmm yy\" to \"yyyy-mm-dd\".  Assumes the notes are passed in as an
argument."

  (cond ((listp rec)
	 (let ((recptr rec))
	   (while recptr
	     (if (memq (caar recptr) '(creation-date timestamp))
		 (setcar recptr (bbdb-migrate-change-dates-change-field
			     (car recptr))))
	     (setq recptr (cdr recptr))))))
  rec)

(defun bbdb-migrate-change-dates-change-field (field)
  "Migrate the date field (the cdr of FIELD) from \"dd mmm yy\" to
\"yyyy-mm-dd\"."

  (let ((date (cdr field))
	parsed)
    ;; Verify and extract - this is fairly hideous
    (and (equal (setq parsed (timezone-parse-date (concat date " 00:00:00")))
		["0" "0" "0" "0" nil])
	 (equal (setq parsed (timezone-parse-date date))
		["0" "0" "0" "0" nil])
	 (cond ((string-match
		 "^\\([0-9]\\{4\\}\\)[-/]\\([ 0-9]?[0-9]\\)[-/]\\([ 0-9]?[0-9]\\)" date)
		(setq parsed (vector (string-to-int (match-string 1 date))
				     (string-to-int (match-string 2 date))
				     (string-to-int (match-string 3 date))))
		;; This should be fairly loud for GNU Emacs users
		(bbdb-warn "BBDB is treating %s field value %s as %s %d %d"
			   (car field) (cdr field)
			   (upcase-initials
			    (downcase (car (rassoc (aref parsed 1)
						   timezone-months-assoc))))
			   (aref parsed 2) (aref parsed 0)))
	       ((string-match
		 "^\\([ 0-9]?[0-9]\\)[-/]\\([ 0-9]?[0-9]\\)[-/]\\([0-9]\\{4\\}\\)" date)
		(setq parsed (vector (string-to-int (match-string 3 date))
				     (string-to-int (match-string 1 date))
				     (string-to-int (match-string 2 date))))
		;; This should be fairly loud for GNU Emacs users
		(bbdb-warn "BBDB is treating %s field value %s as %s %d %d"
			   (car field) (cdr field)
			   (upcase-initials
			    (downcase (car (rassoc (aref parsed 1)
						   timezone-months-assoc))))
			   (aref parsed 2) (aref parsed 0)))
	       (t ["0" "0" "0" "0" nil])))

    ;; I like numbers
    (and (stringp (aref parsed 0))
	 (aset parsed 0 (string-to-int (aref parsed 0))))
    (and (stringp (aref parsed 1))
	 (aset parsed 1 (string-to-int (aref parsed 1))))
    (and (stringp (aref parsed 2))
	 (aset parsed 2 (string-to-int (aref parsed 2))))

    ;; Sanity check
    (cond ((and (< 0 (aref parsed 0))
		(< 0 (aref parsed 1)) (>= 12 (aref parsed 1))
		(< 0 (aref parsed 2))
		(>= (timezone-last-day-of-month (aref parsed 1)
						(aref parsed 0))
		    (aref parsed 2)))
	   (setcdr field (format "%04d-%02d-%02d" (aref parsed 0)
				 (aref parsed 1) (aref parsed 2)))
	   field)
	  (t
	   (error "BBDB cannot parse %s header value %S for upgrade"
		  field date)))))

(defun bbdb-unmigrate-change-dates (rec)
  "Change date formats is timestamp and creation-date fields from
\"yyyy-mm-dd\" to \"dd mmm yy\".  Assumes the notes list is passed in
as an argument."

  (cond ((listp rec)
	 (let ((recptr rec))
	   (while recptr
	     (if (memq (caar recptr) '(creation-date timestamp))
		 (setcar recptr (bbdb-unmigrate-change-dates-change-field
			     (car recptr))))
	     (setq recptr (cdr recptr)))))
	(t rec)))

(defun bbdb-unmigrate-change-dates-change-field (field)
  "Unmigrate the date field (the cdr of FIELD) from \"yyyy-mm-dd\" to
\"yyyy-mm-dd\"."
  (cons (car field) (bbdb-time-convert (cdr field) "%e %b %y")))

;;;###autoload
(defun bbdb-migrate-rewrite-all (message-p &optional records)
  "Rewrite each and every record in the bbdb file; this is necessary if we
are updating an old file format.  MESSAGE-P says whether to sound off
for each record converted.  If RECORDS is non-nil, its value will be
used as the list of records to update."
  ;; RECORDS is used by the migration mechanism.  Since the migration
  ;; mechanism is called from within bbdb-records, if we called
  ;; bbdb-change-record, we'd recurse and die.  We're therefore left
  ;; with the slightly more palatable (but still not pretty) calling
  ;; of bbdb-overwrite-record-internal.
  (or records (setq records (bbdb-records)))
  (let ((i 0))
    (while records
      (bbdb-overwrite-record-internal (car records) nil)
      (if message-p (message "Updating %d: %s %s" (setq i (1+ i))
			     (bbdb-record-firstname (car records))
			     (bbdb-record-lastname  (car records))))
      (setq records (cdr records)))))
(defalias 'bbdb-dry-heaves 'bbdb-migrate-rewrite-all)

;;;###autoload
(defun bbdb-migrate-update-file-version (old new)
  "Change the `file-version' string from the OLD version to the NEW
version."
  (goto-char (point-min))
  (if (re-search-forward (format "^;;; file-version: %d$" old) nil t)
      (replace-match (format ";;; file-version: %d" new))
    (error (format "Can't find file-version string in %s buffer for v%d migration"
		   bbdb-file new))))

(provide 'bbdb-migrate)
