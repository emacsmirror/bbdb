;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; WWW-related functions for the BBDB.  See bbdb.texinfo.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 or (at your
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
;; Revision 1.8  2001/01/17 19:55:07  fenk
;; (bbdb-www-grab-homepage):
;; 	Fix to read just one record not a list of records
;;
;; Revision 1.7  2000/05/02 18:19:17  sds
;; * lisp/bbdb.el, lisp/bbdb-com.el: define `unless' and `when' if
;; necessary, do not quote `lambda' in code, do quote (`') functions
;; and variables in doc strings.
;; * lisp/bbdb.el (bbdb-get-field): new helper function.
;; * lisp/bbdb-com.el (bbdb-notes-sort-order): new variable
;; (bbdb-sort-notes, bbdb-sort-phones, bbdb-sort-addresses): new
;; functions, suitable for `bbdb-change-hook'.
;; (bbdb-get-record): new helper function.
;; * lisp/bbdb-w3.el (bbdb-www): do not browse to multiple URLs
;; simultaneously, allow multiple URLs for the same record instead.
;; (bbdb-www-grab-homepage): add the URL if there is such a fields
;; already.
;;
;; Revision 1.6  1998/04/11 07:06:30  simmonmt
;; Colin Rafferty's patch adding autoload cookies back
;;
;; Revision 1.5  1998/01/06 06:18:22  simmonmt
;; Removed autoloads and added provide for bbdb-w3
;;
;; Revision 1.4  1997/10/26 05:03:49  simmonmt
;; Use browse-url-browser-function rather than a funcall
;;
;; Revision 1.3  1997/10/12 00:18:50  simmonmt
;; Added bbdb-insinuate-w3 to set keyboard map correctly.  Merged
;; bbdb-www-netscape into bbdb-www using browse-url-browser-function to
;; differentiate.
;;
;; Revision 1.2  1997/10/11 20:21:32  simmonmt
;; Modifications mailed in by David Carlton <carlton@math.mit.edu>.  They
;; look to be mostly adaptations for netscape
;;
;; Revision 1.1  1997/10/11 19:05:54  simmonmt
;; Initial revision
;;
;;

(require 'browse-url)

;;;###autoload
(defun bbdb-www (rec &optional which)
  "Visit URLs stored in the `www' field of the current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-www] \
means to try all records currently visible.
Non-interactively, do all records if arg is nonnil."
  (interactive (list (bbdb-get-record "Visit (WWW): ")
                     (or current-prefix-arg 0)))
  (browse-url (read-string "fetch: " (bbdb-get-field rec 'www which))))

;;;###autoload
(defun bbdb-www-grab-homepage (record)
  "Grab the current URL and store it in the bbdb database"
  (interactive (list (bbdb-completing-read-one-record
                      "Add WWW homepage for: ")))
  ;; if there is no database record for this person, create one
  (unless record
    (setq record (bbdb-read-new-record))
    (bbdb-invoke-hook 'bbdb-create-hook record))
  (if (bbdb-record-getprop record 'www)
      (bbdb-record-putprop
       record 'www
       (concat (bbdb-record-getprop record 'www) "," (url-view-url t)))
    (bbdb-record-putprop record 'www (url-view-url t)))
  (bbdb-change-record record t)
  (bbdb-display-records (list record)))

;;;###autoload
(defun bbdb-insinuate-w3 ()
  "Call this function to hook BBDB into W3."
  (add-hook 'w3-mode-hook
	    (lambda () (define-key w3-mode-map ":" 'bbdb-www-grab-homepage))))

(provide 'bbdb-w3)
