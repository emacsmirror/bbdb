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
;; Revision 1.1  1997/10/11 19:05:54  simmonmt
;; Initial revision
;;
;;

(add-hook 'w3-mode-hooks
	  '(lambda () (define-key w3-mode-map ":" 'bbdb-grab-www-homepage)))
(add-hook 'bbdb-load-hook 
	   '(lambda () (define-key bbdb-mode-map "W" 'bbdb-www)))

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
	  (w3-fetch (setq got-one (car urls))))
      (setq urls (cdr urls)))
    (if (not got-one)
	(error "No WWW field!"))))

(defun bbdb-grab-www-homepage (record)
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


;;;; From Fran Litterio <franl@centerline.com>
;(defun fetch-url (url)
;  "Causes a running Mosaic 2.x process to fetch the document at the URL (which
;is the contents of the region for interactive invocations)."
;  (interactive (list (buffer-substring (region-beginning) (region-end))))
;  (let ((pid ""))
;    (string-match "[ \t\n]*\\(.*[^ \t\n]\\)[ \t\n]*$" url)
;    (setq url (substring url (match-beginning 1) (match-end 1)))
;    (save-excursion
;      (set-buffer (get-buffer-create " *Mosaic Control*"))
;      (if (string= (buffer-name) " *Mosaic Control*")	; Paranoia!
;	  (erase-buffer))
;      (insert-file-contents "~/.mosaicpid")
;      (setq pid (buffer-substring (point-min) (1- (point-max))))
;      (erase-buffer)
;      (insert (concat "newwin\n" url "\n"))
;      (write-region (point-min) (point-max) (concat "/tmp/Mosaic." pid)
;		    nil nil))
;    (signal-process (string-to-number pid)
;		    (cond
;		     ((eq system-type 'hpux)
;		      16)	; SIGUSR1 under HP-UX
;		     ((eq system-type 'berkeley-unix)
;		      30)))	; SIGUSR1 under SunOS 4.x
;    (message (format "Told Mosaic to fetch \"%s\"" url))))

;;; From Fran Litterio <franl@centerline.com>
;;; adapted for netscape by Josef Schneeberger <jws@forwiss.uni-erlangen.de>
(defun fetch-url (url)
  "Causes a running netscape process to fetch the document at the URL (which
is the contents of the region for interactive invocations)."
  (interactive (list (buffer-substring (region-beginning) (region-end))))
  (let ()
    (string-match "[ \t\n]*\\(.*[^ \t\n]\\)[ \t\n]*$" url)
    (setq url (substring url (match-beginning 1) (match-end 1)))
    (save-excursion
      (shell-command (concat "/usr/local/bin/NETSCAPE/netscape-1.1b3/netscape "
			     "-remote "
			     "'openURL("
			     url
			     ")'"))))
    (message (format "Told netscape to fetch \"%s\"" url)))
