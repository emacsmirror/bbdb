;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1995 Jamie Zawinski <jwz@netscape.com>.
;;; Invoking BBDB from another process, via `gnudoit'.
;;; last change 25-apr-96.

;;; This requires the `gnuserv' and `itimer' packages.
;;;
;;; To use:
;;;
;;; First, do `(gnuserv-start)' to initialize the emacs server process.
;;; If you don't know what this does, see the doc for gnuserv.el.
;;;
;;; Then, an external process may invoke `gnudoit' in the following way:
;;;
;;;     gnudoit '(bbdb-server "...all message headers..")'
;;;
;;; The bbdb-srv.perl program is a good choice for this; it takes a header
;;; block on stdin, and converts them to a lisp string, taking care to
;;; "sanitize" them so that hostile data can't take over the executing shell.
;;;
;;; The string should be a validly-formatted-and-quoted lisp string, and
;;; should contain multiple lines, which are the headers of the message for
;;; which a record should be displayed.  It should contain at least a "From:"
;;; header, or nothing will be displayed, but it should contain as many headers
;;; as your various BBDB hooks might want access to.
;;;
;;; Records will not be displayed until no record has been requested for 
;;; `bbdb/srv-display-delay' seconds (default 2.)  This is to prevent rapid
;;; display of records from queueing up and swamping the emacs server process.


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

(require 'bbdb)
(require 'gnuserv)
(require 'itimer)

(defvar bbdb/srv-auto-create-p nil
  "*Like bbdb/news-auto-create-p and bbdb/mail-auto-create-p,
but for the case where the record is being displayed by some external
process via the `gnudoit' mechanism.

If this is t, then records will automatically be created; if this is a
function name or lambda, then it is called with no arguments to decide 
whether an entry should be automatically created.  You can use this to,
for example, create or not create messages which have a particular subject.")

(defvar bbdb/srv-display-delay 2
  "*How long we must be idle before displaying a record.")

(defvar bbdb/srv-pending-headers nil)
(defvar bbdb/srv-pending-map
  (and (fboundp 'set-extent-property)
       (condition-case nil
	   (let ((m (make-sparse-keymap)))
	     (set-keymap-name m 'bbdb/srv-pending-map)
	     (define-key m 'button1 'bbdb/srv-pending-add)
	     m)
	 (error nil))))

(defun bbdb/srv-handle-headers (headers &optional create-p)
  "Display (or create) the BBDB entry corresponding to the message headers.
HEADERS should be a string containing an RFC822 header block; at least a
\"From:\" header should be provided, but others will be made available to
the various hooks (like `bbdb-notice-hook' and `bbdb/news-auto-create-p')."
  (let ((buf "*bbdb-tmp*")
	(record nil)
	(bbdb-force-dialog-boxes t) ; affects bbdb-y-or-n-p
	from)
    (save-excursion
      (set-buffer (or (get-buffer buf)
		      (progn
			(setq buf (get-buffer-create buf))
			(set-buffer buf)
			(buffer-disable-undo buf))))
      (erase-buffer)
      (insert headers "\n\n")
      (setq from (mail-fetch-field "from"))
      (if (or (null from)
	      (string-match (bbdb-user-mail-names)
			    (mail-strip-quoted-names from)))
	  ;; if logged-in user sent this, use recipients.
	  (setq from (or (mail-fetch-field "to") from)))
      (if from
	  (setq record
		(bbdb-annotate-message-sender from t
					      (or create-p
						  (bbdb-invoke-hook-for-value
						   bbdb/srv-auto-create-p))
					      nil))))
    (let ((w (get-buffer-window bbdb-buffer-name)))
      (if w
	  nil
	(bbdb-pop-up-bbdb-buffer)
	(setq w (get-buffer-window bbdb-buffer-name))
	(if (fboundp 'set-window-buffer-dedicated)
	    (set-window-buffer-dedicated w bbdb-buffer-name))))
    (cond (record
	   (let ((bbdb-gag-messages t)
		 (bbdb-use-pop-up nil)
		 (bbdb-electric-p nil)
		 (bbdb-elided-display (bbdb-pop-up-elided-display))
		 (b (current-buffer)))
	     (bbdb-display-records (list record))
	     (set-buffer b)))
	  ((and (not create-p) bbdb/srv-pending-map)
	   (setq bbdb/srv-pending-headers headers)
	   (save-excursion
	     (set-buffer bbdb-buffer-name)
	     (let ((buffer-read-only nil))
	       (erase-buffer)
	       (insert "\t\t\t")
	       (let ((p (point))
		     e)
		 (insert from)
		 (setq e (make-extent p (point)))
		 (set-extent-face e 'bold)
		 (set-extent-property e 'highlight t)
		 (set-extent-property e 'keymap bbdb/srv-pending-map)
		 )
	       (insert "\n\n\t\t\tClick to add to BBDB.")
	       ))))))

(defun bbdb/srv-pending-add ()
  (interactive "@")
  (or bbdb/srv-pending-headers (error "lost headers?"))
  (bbdb/srv-handle-headers bbdb/srv-pending-headers t))


(defvar bbdb/srv-itimer-arg nil)
(defun bbdb/srv-itimer ()
  "Used as a timer function by bbdb/srv-handle-headers-with-delay.
This invokes bbdb/srv-handle-headers with bbdb/srv-itimer-arg.
We do it this way instead of by using a lambda to start-itimer so that
we cons less."
  (defvar current-itimer)
  (if current-itimer (delete-itimer current-itimer))
  (if bbdb/srv-itimer-arg
      (bbdb/srv-handle-headers
       (prog1 bbdb/srv-itimer-arg
	 (setq bbdb/srv-itimer-arg nil)))))

(defun bbdb/srv-handle-headers-with-delay (headers)
  "Just like bbdb/srv-handle-headers, but only updates every few seconds.
This is so that trying to display many records in succession won't queue them
up, but will end up only displaying a record when no displays have been 
requested for a couple of seconds."
  (let* ((name "bbdb-srv")
	 (itimer (get-itimer name)))
    (setq bbdb/srv-itimer-arg headers)
    (if itimer
	;; It hasn't gone off yet; just change what it's argument will be.
	nil
      ;; else, start the timer going again.
      (start-itimer name 'bbdb/srv-itimer bbdb/srv-display-delay nil))
    nil))

(fset 'bbdb-srv 'bbdb/srv-handle-headers-with-delay)

(provide 'bbdb-srv)
