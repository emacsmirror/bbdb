;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.
;;; Mouse sensitivity and menus for XEmacs.
;;; last change 20-may-96.

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

;;; This code is kind of kludgey, mostly because it needs to parse the contents
;;; of the *BBDB* buffer, since BBDB doesn't save the buffer-positions of the
;;; various fields when it fills in that buffer (doing that would be slow and
;;; cons a lot, so it doesn't seem to be worth it.)

(or (string-match "XEmacs\\|Lucid" emacs-version)
    (error "This file only works in XEmacs."))

(require 'bbdb)
(require 'bbdb-com)

(define-key bbdb-mode-map 'button3 'bbdb-menu)

(or (find-face 'bbdb-name)
    (face-differs-from-default-p (make-face 'bbdb-name))
    (set-face-underline-p 'bbdb-name t))

(or (find-face 'bbdb-company)
    (face-differs-from-default-p (make-face 'bbdb-company))
    (make-face-italic 'bbdb-company))

(or (find-face 'bbdb-field-value)
    (make-face 'bbdb-field-value))

(or (find-face 'bbdb-field-name)
    (face-differs-from-default-p (make-face 'bbdb-field-name))
    (copy-face 'bold 'bbdb-field-name))

;;;###autoload
(defun bbdb-fontify-buffer ()
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (if (and (fboundp 'set-specifier)
	     (featurep 'scrollbar))
	(set-specifier scrollbar-height (cons (current-buffer) 0)))
    ;; first delete existing extents
    (map-extents (function (lambda (x y)
			     (if (eq (extent-property x 'data) 'bbdb)
				 (delete-extent x))))
		 (current-buffer) (point-min) (point-max) nil)
    (let ((rest bbdb-records)
	  record face start end elided-p p e)
      (while rest
	(setq record (car (car rest))
	      elided-p (eq (nth 1 (car rest)) t)
	      face (and (not elided-p) (bbdb-record-getprop record 'face))
	      start (marker-position (nth 2 (car rest)))
	      end (1- (or (nth 2 (car (cdr rest))) (point-max))))
	(set-extent-property (setq e (make-extent start end)) 'highlight t)
	(set-extent-property e 'data 'bbdb)
	(setq p (+ start (length (bbdb-record-name record))))
	(if (bbdb-record-company record)
	    (setq p (+ p 3 (length (bbdb-record-company record)))))
	(goto-char start)
	(if (search-forward " - " p t)
	    (progn
	      (setq e (make-extent (point) p))
	      (set-extent-property e 'data 'bbdb)
	      (set-extent-face e 'bbdb-company)
	      (set-extent-property e 'highlight t)
	      (forward-char -3))
	  (goto-char p))
	(setq e (make-extent start (point)))
	(set-extent-property e 'data 'bbdb)
	(set-extent-face e 'bbdb-name)
	(set-extent-property e 'highlight t)
	(if face (bbdb-hack-x-face face e))
	(forward-line 1)
	(while (< (point) end)
	  (skip-chars-forward " \t")
	  (setq p (point))
	  (and (looking-at "[^:\n]+:")
	       (progn
		 (setq e (make-extent p (match-end 0)))
		 (set-extent-face e 'bbdb-field-name)
		 (set-extent-property e 'data 'bbdb)))
	  (while (progn (forward-line 1)
			(looking-at "^\\(\t\t \\|                 \\)")))
	  (setq e (make-extent p (1- (point))))
	  (set-extent-property e 'data 'bbdb)
	  (set-extent-face e 'bbdb-field-value)
	  (set-extent-property e 'highlight t))
	(setq rest (cdr rest))))))


(defun bbdb-hack-x-face (face extent)
  ;; requires lemacs 19.10 version of highlight-headers.el
  (cond
   ((and (boundp 'highlight-headers-hack-x-face-p)
	 highlight-headers-hack-x-face-p)
    (setq face (bbdb-split face "\n"))
    (while face
      (cond
       ((fboundp 'highlight-headers-x-face)			; the 19.10 way
	(highlight-headers-x-face (car face) extent)
	(let ((b (extent-property extent 'begin-glyph)))
	  (cond (b ; I'd like this to be an end-glyph instead
		 (set-extent-property extent 'begin-glyph nil)
		 (set-extent-property extent 'end-glyph b)))))
       (t							; the 19.13 way
	(save-excursion
	  (set-buffer (get-buffer-create " *tmp*"))
	  (buffer-disable-undo (current-buffer))
	  (erase-buffer)
	  (insert (car face))
	  (set-extent-begin-glyph extent nil)
	  (set-extent-end-glyph extent
				(highlight-headers-x-face-to-pixmap
				 (point-min) (point-max)))
	  (erase-buffer))))
      (setq face (cdr face))
      (cond (face ; there are more, so clone the extent
	     (setq extent (make-extent
			   (extent-start-position extent)
			   (extent-end-position extent)))
	     (set-extent-property extent 'data 'bbdb)))))))


(defvar global-bbdb-menu-commands
  '(["Save BBDB" bbdb-save-db t]
    ["Elide All Records" bbdb-elide-record t]
    ["Finger All Records" (bbdb-finger (mapcar 'car bbdb-records)) t]
    ["BBDB Manual" bbdb-info t]
    ["BBDB Quit" bbdb-bury-buffer t]
    ))

(defun build-bbdb-finger-menu (record)
  (let ((addrs (bbdb-record-finger-host record)))
    (if (cdr addrs)
	(cons "Finger..."
	      (nconc
	       (mapcar '(lambda (addr)
			  (vector addr (list 'bbdb-finger record addr)
				  t))
		       addrs)
	       (list "----"
		     (vector "Finger all addresses"
			     (list 'bbdb-finger record ''(4)) t))))
      (vector (concat "Finger " (car addrs))
	      (list 'bbdb-finger record (car addrs)) t))))

(defun build-bbdb-sendmail-menu (record)
  (let ((addrs (bbdb-record-net record)))
    (if (cdr addrs)
	(cons "Send Mail..."
	      (mapcar '(lambda (addr)
			 (vector addr (list 'bbdb-send-mail-internal
					    (bbdb-dwim-net-address record addr))
				 t))
		      addrs))
      (vector (concat "Send mail to " (car addrs))
	      (list 'bbdb-send-mail-internal
		    (bbdb-dwim-net-address record (car addrs)))
	      t))))


(defun build-bbdb-field-menu (record field)
  (let ((type (car field)))
    (nconc
     (list
      (concat "Commands for "
	      (cond ((eq type 'property)
		     (concat "\""
			     (symbol-name (if (consp (car (cdr field)))
					      (car (car (cdr field)))
					    (car (cdr field))))
			     "\" field:"))
		    ((eq type 'name) "Name field:")
		    ((eq type 'company) "Company field:")
		    ((eq type 'net) "Network Addresses field:")
		    ((eq type 'aka) "Alternate Names field:")
		    (t
		     (concat "\"" (aref (nth 1 field) 0) "\" "
			     (capitalize (symbol-name type)) " field:"))))
      "-----"
      ["Edit Field" bbdb-edit-current-field t]
      )
     (if (memq type '(name company))
	 nil
       (list ["Delete Field" bbdb-delete-current-field-or-record t]))
     (cond ((eq type 'phone)
	    (list (vector (concat "Dial " (bbdb-phone-string (car (cdr field))))
			  (list 'bbdb-dial (list 'quote field) nil) t)))
	   )
     )))


(defun build-bbdb-insert-field-menu (record)
  (cons "Insert New Field..."
	(mapcar
	 '(lambda (field)
 	    (let ((type (if (string= (car field) "AKA")
			    'aka
			  (intern (car field)))))
	      (vector (car field)
		      (list 'bbdb-insert-new-field (list 'quote type)
			    (list 'bbdb-prompt-for-new-field-value
				  (list 'quote type)))
		      (not
		       (or (and (eq type 'net) (bbdb-record-net record))
			   (and (eq type 'aka) (bbdb-record-aka record))
			   (and (eq type 'notes) (bbdb-record-notes record))
			   (and (consp (bbdb-record-raw-notes record))
				(assq type (bbdb-record-raw-notes record))))))))
	 (append '(("phone") ("address") ("net") ("AKA") ("notes"))
		 (bbdb-propnames)))))


(defun build-bbdb-menu (record field)
  (append
   '("bbdb-menu" "Global BBDB Commands" "-----")
   global-bbdb-menu-commands
   (if record
       (list
	"-----"
	(concat "Commands for record \""
		(bbdb-record-name record) "\":")
	"-----"
	(vector "Delete Record"
		(list 'bbdb-delete-current-record record) t)
	(if (nth 1 (assq record bbdb-records))
	    ["Unelide Record" bbdb-elide-record t]
	  ["Elide Record" bbdb-elide-record t])
	["Omit Record" bbdb-omit-record t]
	["Refile (Merge) Record" bbdb-refile-record t]
	))
   (if record
       (list (build-bbdb-finger-menu record)))
   (if (bbdb-record-net record)
       (list (build-bbdb-sendmail-menu record)))
   (if record
       (list (build-bbdb-insert-field-menu record)))
   (if field
       (cons "-----" (build-bbdb-field-menu record field)))
   ))


;;;###autoload
(defun bbdb-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (require 'bbdb-com)
  (beginning-of-line)
  (popup-menu
   (save-window-excursion
     (save-excursion
       (mouse-set-point e)
       (let ((extent (or (extent-at (point) (current-buffer) 'highlight)
			 (error "")))
	     record field face)
	 (or (eq (extent-property extent 'data) 'bbdb)
	     (error "not a bbdb extent"))
	 (highlight-extent extent t)	; shouldn't be necessary...
	 (goto-char (extent-start-position extent))
	 (beginning-of-line)
	 (setq record (bbdb-current-record)
	       face (extent-face extent)
	       field (cond ((memq face
				  '(bbdb-name bbdb-field-value bbdb-field-name))
			    (bbdb-current-field))
			   ((eq face 'bbdb-company)
			    (cons 'company (cdr (bbdb-current-field))))
			   (t nil)))
	 (build-bbdb-menu record field))))))

(add-hook 'bbdb-list-hook 'bbdb-fontify-buffer)

;; Utility functions that mask others to provide XEmacs-specific functionality
;;;###autoload
(defun bbdb-xemacs-display-completion-list (list &optional callback data)
  "Wrapper for `display-completion-list'.
Allows callbacks on XEmacs `display-completion-list' is called with
`:activate-callback CALLBACK' if CALLBACK is non-nil.
`:user-data DATA' is also used if DATA is non-nil.
Neither are used if CALLBACK is nil."
  (cond ((and callback data)
	 (display-completion-list list
				  :activate-callback callback
				  :user-data data))
	(callback
	 (display-completion-list list
				  :activate-callback callback))
	(t
	 (display-completion-list list))))

(provide 'bbdb-xemacs)
