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

;;;###autoload
(defun bbdb-www (all)
  "Visit URL's stored in `www' fields of the current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-www] \
means to try all records currently visible.
Non-interactively, do all records if arg is nonnil."
  (interactive (list (bbdb-do-all-records-p)))
  (let ((urls (mapcar '(lambda (r) (bbdb-record-getprop r 'www))
		      (if all 
			  (mapcar 'car bbdb-records)
			(list (bbdb-current-record)))))
	(got-one nil))
    (while urls
      (if (car urls)
	  (funcall browse-url-browser-function (setq got-one (car urls))))
      (setq urls (cdr urls)))
    (if (not got-one)
	(error "No WWW field!"))))

;;;###autoload
(defun bbdb-www-grab-homepage (record)
  "Grab the current URL and store it in the bbdb database"
  (interactive (list (bbdb-completing-read-record "Add WWW homepage for: ")))
  ;; if there is no database record for this person, create one
  (cond ((null record)
	 (setq record (bbdb-read-new-record))
	 (bbdb-invoke-hook 'bbdb-create-hook record))
	;; .. ok, it already exists ..
	(t nil))
  (bbdb-record-putprop record 'www (url-view-url t))
  (bbdb-change-record record t)
  (bbdb-display-records (list record)))

;;;###autoload
(defun bbdb-insinuate-w3 ()
  "Call this function to hook BBDB into W3."
  (add-hook 'w3-mode-hook
	    '(lambda () (define-key w3-mode-map ":" 'bbdb-www-grab-homepage))))
