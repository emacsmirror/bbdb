;;; bbdb-whois.el -- Big Brother gets a little help from Big Brother
;;; This file is part of the Insidious Big Brother Database (aka BBDB).
;;;
;;; Copyright (C) 1992, 1993 Roland McGrath
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@gnu.ai.mit.edu) or
;;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to roland@gnu.ai.mit.edu.

(require 'bbdb-com)

(defmacro bbdb-add-to-field (record field text)
  (let ((get (intern (concat "bbdb-record-" (symbol-name field))))
	(set (intern (concat "bbdb-record-set-" (symbol-name field)))))
    (` (let ((old ((, get) (, record)))
	     (text (, text)))
	 (or (member text old)
	     ((, set) (, record) (nconc old (list text))))))))

(defcustom bbdb-whois-server (or (and (boundp 'whois-server) whois-server)
			      "rs.internic.net")
  "*Server for \\[bbdb-whois] lookups."
  :group 'bbdb-utilities
  :type 'string)

;;;###autoload
(defun bbdb-whois (the-record &optional server)
  (interactive (list (if (string= bbdb-buffer-name (buffer-name))
			 (bbdb-current-record)
		       (let (r (p "BBDB Whois: "))
			 (while (not r)
			   (setq r (bbdb-completing-read-record p))
			   (if (not r) (ding))
			   (setq p "Not in the BBDB!  Whois: "))
			 r))
		     (and current-prefix-arg
			  (read-string "Query whois server: "
				       bbdb-whois-server))))
  (or server
      (setq server bbdb-whois-server))
  (if (or (bbdb-record-lastname the-record) (bbdb-record-firstname the-record))
      ;; XXX we seem to get called with a vector of nils.
      (save-excursion
	(set-buffer (generate-new-buffer " *bbdb-whois*"))
	(set (make-local-variable 'bbdb-whois-record) the-record)
	(set (make-local-variable 'bbdb-whois-name)
	     (if (bbdb-record-getprop the-record 'nic)
		 (concat "!" (bbdb-record-getprop the-record 'nic))
	       (concat (bbdb-record-lastname the-record) ", "
		       (bbdb-record-firstname the-record))))
	(let ((proc (open-network-stream "whois" (current-buffer) server 43)))
	  (set-process-sentinel proc 'bbdb-whois-sentinel)
	  (process-send-string proc (concat bbdb-whois-name "\r\n"))))))

(defun bbdb-whois-sentinel (proc status)
  (save-excursion
    (let (rec)
      (set-buffer (process-buffer proc))
      (setq rec bbdb-whois-record)
      (goto-char 1)
      (if (search-forward "To single out one record" nil t)
	  (message "%s is ambiguous to whois; try a different name"
		   bbdb-whois-name)
	(replace-string "\r\n" "\n")
	(goto-char 1)
	(if (re-search-forward
	     (concat (if (string-match "^!" bbdb-whois-name)
			 (concat "(\\("
				 (regexp-quote (substring bbdb-whois-name 1))
				 "\\))")
		       (concat (regexp-quote bbdb-whois-name)
			       ".*(\\([A-Z0-9]+\\))"))
		     "\\s *\\(\\S +@\\S +\\)?$")
	     nil t)
	    (let ((net (if (match-beginning 2)
			   (downcase (buffer-substring (match-beginning 2)
						       (match-end 2)))))
		  (nic (buffer-substring (match-beginning 1) (match-end 1)))
		  (lines nil))
	      (if net
		  (bbdb-add-to-field rec net net))
	      (bbdb-record-putprop rec 'nic nic)

	      ;; Snarf company.
	      (forward-line 1)
	      (back-to-indentation)
	      (let ((company (buffer-substring (point) (progn (end-of-line)
							      (point))))
		    (old (bbdb-record-company rec)))
		(cond ((not old)
		       (bbdb-record-set-company rec company))
		      ((string= old company)
		       nil)
		      (t
		       (bbdb-record-putprop rec 'nic-organization company))))

	      ;; Read the address info into LINES.
	      (while (progn (forward-line 1)
			    (not (looking-at "^$")))
		(back-to-indentation)
		(setq lines (cons (buffer-substring (point)
						    (progn (end-of-line)
							   (point)))
				  lines)))

	      ;; Snarf phone number.
	      (if (car lines)
		  (progn
		    (if (not (bbdb-find-phone (car lines)
					      (bbdb-record-phones rec)))
			(let ((phone-number (vector "phone" (car lines))))
			  (bbdb-add-to-field rec phones phone-number)))
		    (setq lines (cdr lines))))

	      ;; Snarf address.
	      (if (car lines)
		  (let ((addr (make-vector bbdb-address-length nil))
			(city "")
			(state "")
			zip)
		    (if (string-match
			 "\\([^,]+\\),\\s *\\(\\S +\\)\\s *\\([0-9-]+\\)"
			 (car lines))
			(setq city (substring (car lines)
					      (match-beginning 1)
					      (match-end 1))
			      state (substring (car lines)
					       (match-beginning 2)
					       (match-end 2))
			      zip (string-to-int (substring (car lines)
							    (match-beginning 3)
							    (match-end 3)))
			      lines (cdr lines)))
		    (bbdb-address-set-location addr "address") ;???
		    (bbdb-address-set-city addr city)
		    (bbdb-address-set-state addr state)
		    (bbdb-address-set-zip addr zip)
; FIXME whois in general is busted.
; 		    (setq lines (nreverse lines))
; 		    (bbdb-address-set-street1 addr (or (car lines) ""))
; 		    (setq lines (cdr lines))
; 		    (bbdb-address-set-street2 addr (or (car lines) ""))
; 		    (setq lines (cdr lines))
; 		    (bbdb-address-set-street3 addr (or (car lines) ""))
; 		    (setq lines (cdr lines))
		    (bbdb-add-to-field rec addresses addr)))

	      ;; Snarf any random notes.
	      (setq lines nil)
	      (while (progn
		       (forward-line 1)
		       (back-to-indentation)
		       (not (looking-at
			     "$\\|Record last updated on")))
		(if (looking-at "Alternate mailbox: \\(\\S +\\)$")
		    (bbdb-add-to-field rec net
				       (buffer-substring (match-beginning 1)
							 (match-end 1)))
		  (setq lines (cons (buffer-substring (point)
						      (progn (end-of-line)
							     (point)))
				    lines))))
	      (if lines
		  (bbdb-record-putprop rec 'nic-notes
				       (mapconcat 'identity
						  (nreverse lines)
						  "\n")))
	    
	      ;; Snarf the last-update date.
	      (if (re-search-forward "Record last updated on \\(\\S *\\)\\."
				     nil t)
		  (bbdb-record-putprop rec 'nic-updated
				       (buffer-substring (match-beginning 1)
							 (match-end 1))))

	      (save-excursion
		(set-buffer bbdb-buffer-name)
		(bbdb-redisplay-one-record rec)))
	  (message "No whois information for %s" bbdb-whois-name)))
      (delete-process proc)
      (kill-buffer (current-buffer)))))
	      
(defun bbdb-find-phone (string record)
  "Return the vector entry if STRING is a phone number listed in RECORD."
  (let ((phone nil)
	(done nil))
    (while (and record (not done))
      (setq phone (car record))
      (if (string= string (bbdb-phone-string phone))
	  (setq done phone))
      (setq record (cdr record)))
    done))

(provide 'bbdb-whois)
