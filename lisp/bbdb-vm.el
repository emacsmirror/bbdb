;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to VM (View Mail) 5.31 or greater.  See bbdb.texinfo.

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
;; Revision 1.59  2000/08/10 19:11:31  fenk
;; T(bbdb/vm-get-from): uses now
;; bbdb-extract-address-components to extract all recipients and uses
;; vm-summary-uninteresting-senders for ignoring senders, which is
;; more consistently with respect VM.  One can set the variable
;; `bbdb/vm-get-from-headers' and `bbdb/vm-get-first-from-p' in order
;; to control what headers are processed and what is display is what
;; order.
;; (bbdb/vm-message-cache-lookup):
;; (bbdb/vm-encache-message): We use our own caching functions
;; instead of the bbdb default functions since we are handling a set
;; of records and not a single one.
;; (bbdb/vm-update-record): is now just a call to
;; (bbdb/vm-update-records): which performs the actual work of
;; finding and updating records.
;; (bbdb/vm-set-auto-folder-alist): Is a function from Mark Thomas
;; <mthomas@jprc.com> which sets `vm-auto-folder-alist' according to
;; the field `bbdb/vm-set-auto-folder-alist-field'.
;;
;; Files:	lisp/bbdb-vm.el
;;
;; Revision 1.58  2000/08/01 10:51:41  waider
;; Added Howard Melman's VM labeling code. It's switched off by default;
;; use (add-hook 'bbdb-notice-hook 'bbdb/vm-auto-add-label) to enable it.
;;
;; Revision 1.57  2000/06/14 14:51:34  waider
;; * Trying another way to get the from field, since the Presentation
;;   buffer hack seems to be somewhat unusable.
;;
;; Revision 1.56  2000/03/31 09:58:50  bbdb-writer
;; (bbdb/vm-get-from): If there's a presentation buffer, get the address
;; from there, since it will be MIME-decoded.
;;
;; Revision 1.55  1998/04/11 07:06:47  simmonmt
;; Colin Rafferty's patch adding autoload cookies back
;;
;; Revision 1.54  1998/02/23 07:14:01  simmonmt
;; Use add-hook, not bbdb-add-hook
;;
;; Revision 1.53  1997/11/02 07:40:18  simmonmt
;; bbdb/vm-annotate-sender now takes REPLACE argument
;;
;;

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-snarf)
(require 'vm-autoload)
(require 'vm)

(if (not (fboundp 'vm-record-and-change-message-pointer))
    (load-library "vm-motion"))
(if (not (fboundp 'vm-su-from))
    (load-library "vm-summary"))
(or (boundp 'vm-mode-map)
    (load-library "vm-vars"))

(defgroup bbdb-mua-specific-vm nil
  "VM-specific BBDB customizations"
  :group 'bbdb-mua-specific)
(put 'bbdb-mua-specific-vm 'custom-loads '("bbdb-vm"))

(defcustom bbdb/vm-get-from-headers
  '("Resent-From:" "From:" "Resent-To:" "Resent-CC:" "To:" "CC:")
  "*List of headers to search for senders respectively recipients.
You may add additional headers, however be warned since it will take
more time to search more headers!"
  :group 'bbdb-mua-specific-vm
  :type 'list)

(defcustom bbdb/vm-get-first-from-p
  nil
  "*If t `bbdb/vm-update-records' will return only the first one."
  :group 'bbdb-mua-specific-vm
  :type 'boolean)

(defun bbdb/vm-get-from (msg &optional firstfrom)
  "Return real name and email address of sender respectively recipient.
If an address matches `vm-summary-uninteresting-senders' it will be ignored.
The headers to search can be configured by `bbdb/vm-get-from-headers'."
  (setq msg (vm-real-message-of msg))
  (let ((headers  bbdb/vm-get-from-headers)
	(fromlist nil)
	header)
    (while (and headers (or (and firstfrom (not fromlist)) (not firstfrom)))
      (setq header (vm-get-header-contents msg (car headers)))
      (if header
	  (let ((adlist (bbdb-extract-address-components
			 (vm-decode-mime-encoded-words-in-string header)))
		fn ad)
	    ;; ignore uninteresting addresses
	    (while adlist
	      (setq fn (caar adlist) ad (cadar adlist))
	      (if (or (and fn (string-match vm-summary-uninteresting-senders
					    fn))
		      (string-match vm-summary-uninteresting-senders ad))
		  nil
		(add-to-list 'fromlist (car adlist)))
	      (setq adlist (cdr adlist)))))
      (setq headers (cdr headers)))
    (nreverse fromlist)))

;; We use our own caching functions instead of the bbdb default
;; functions since we are handling a set of records and not a single
;; one.
(defun bbdb/vm-message-cache-lookup (message-key)
  (bbdb-records)
  (if bbdb-message-caching-enabled
      (let ((records (assq message-key bbdb-message-cache))
	    (invalid nil))
	(mapcar (lambda (record)
		  (if (bbdb-record-deleted-p record)
		      (setq invalid t)))
		(cdr records))
	(if invalid nil records))))

(defun bbdb/vm-encache-message (message-key bbdb-record)
  "Don't call this multiple times with the same args, it doesn't replace."
  (and bbdb-message-caching-enabled
       (setq bbdb-message-cache  (cons (cons message-key bbdb-record)
				       bbdb-message-cache))
       (notice-buffer-with-cache (current-buffer))))

;;;###autoload
(defun bbdb/vm-update-record (&optional offer-to-create)
  (let ((bbdb/vm-get-first-from-p t))
    (bbdb/vm-update-records offer-to-create)))

;;;###autoload
(defun bbdb/vm-update-records (&optional offer-to-create)
  "Returns the record corresponding to the current VM message, 
creating or modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation.
When hitting C-g you will not be asked anymore for new people listed
in this message."
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  
  (let ((msg (car vm-message-pointer))
	(inhibit-local-variables nil)	; vm binds this to t...
	(enable-local-variables t)	; ...or vm bind this to nil.
	(inhibit-quit nil)		; vm better not bind this to t!
	cache records)

    (setq cache (and msg (bbdb/vm-message-cache-lookup msg)))

    (if cache
	(setq records (if bbdb/vm-get-first-from-p
			  (list (cadr cache))
			(cdr cache)))
      (and msg
	   (let ((addrs (bbdb/vm-get-from msg bbdb/vm-get-first-from-p))
		 (bbdb-records (bbdb-records))
		 rec (create-p t))
	     (mapc (lambda (address)
		     (condition-case nil
			 (setq rec
			       (if create-p
				   (bbdb-annotate-message-sender
				    address t
				    (bbdb-invoke-hook-for-value
				     bbdb/mail-auto-create-p)
				    t)
				 (let ((name (car address))
				       (net (cadr address)))
				   (if name 
				       (setq name (bbdb-search bbdb-records
							       name nil net)))
				   (if name (car name) nil))))
		       (quit (setq create-p nil)))
		     ;; people should be listed only once so we use
		     ;; add-to-list  
		     (if rec (add-to-list 'records rec)))
		   addrs)
	     (setq records (nreverse records))
	     (bbdb/vm-encache-message msg records))))
    records))

;;;###autoload
(defun bbdb/vm-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive
   (list (if bbdb-readonly-p
	     (error "The Insidious Big Brother Database is read-only.")
	   (read-string "Comments: "))))
  (vm-follow-summary-cursor)
  (bbdb-annotate-notes (car (bbdb/vm-update-record t)) string 'notes replace))


(defun bbdb/vm-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (vm-follow-summary-cursor)
  (let ((records (or (bbdb/vm-update-record t) (error ""))))
    (bbdb-display-records records)
    (if arg
	(bbdb-record-edit-property (car records) nil t)
      (bbdb-record-edit-notes (car records) t))))

;;;###autoload
(defun bbdb/vm-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (vm-follow-summary-cursor)
  (let ((records (bbdb/vm-update-record t)))
    (if records
	(bbdb-display-records records)
      (error "unperson"))))


(defun bbdb/vm-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the VM window(s).
Displays the records corresponding to the sender respectively
recipients of the current message.
See `bbdb/vm-get-from-headers' and 'bbdb/vm-get-first-from-p' for
configuration of what is being displayed." 
  (if bbdb-use-pop-up
      (bbdb-pop-up-bbdb-buffer
       (function (lambda (w)
		   (let ((b (current-buffer)))
		     (set-buffer (window-buffer w))
		     (prog1 (eq major-mode 'vm-mode)
		       (set-buffer b)))))))
  
  (save-excursion
    (let ((bbdb-gag-messages t)
	  (bbdb-electric-p nil)
	  (records (bbdb/vm-update-records offer-to-create))
	  (bbdb-elided-display (bbdb-pop-up-elided-display)))
      (if records (bbdb-display-records records))
      records)))


;; By Alastair Burt <burt@dfki.uni-kl.de>
;; vm 5.40 and newer support a new summary format, %U<letter>, to call
;; a user-provided function.  Use "%-17.17UB" instead of "%-17.17F" to
;; have your VM summary buffers display BBDB's idea of the sender's full
;; name instead of the name (or lack thereof) in the message itself.

(defun vm-summary-function-B (m &optional to-p)
  "Given a VM message returns the BBDB name of the sender.
Respects vm-summary-uninteresting-senders."
  (if (and vm-summary-uninteresting-senders (not to-p))
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow
		    (vm-summary-function-B m t))
	  (or (bbdb/vm-alternate-full-name  (vm-su-from m))
	      (vm-su-full-name m))))
    (or (bbdb/vm-alternate-full-name (if to-p (vm-su-to m) (vm-su-from m)))
	(if to-p (vm-su-to-names m) (vm-su-full-name m)))))

(defun bbdb/vm-alternate-full-name (address)
  (if address 
      (let ((entry (bbdb-search-simple
		    nil
		    (if (and address bbdb-canonicalize-net-hook)
			(bbdb-canonicalize-address address)
		      address))))
	(if entry
	    (or (bbdb-record-getprop entry 'mail-name)
		(bbdb-record-name entry))))))


;; From: Mark Thomas <mthomas@jprc.com>
;; Subject: auto-folder-alist from bbdb

;;;###autoload
(defcustom bbdb/vm-set-auto-folder-alist-field 'vm-folder
  "*The field which `bbdb/vm-set-auto-folder-alist' searches for."
  :group 'bbdb
  :type 'symbol)

;;;###autoload
(defun bbdb/vm-set-auto-folder-alist ()
  "Create a `vm-auto-folder-alist' according to the records in the bbdb.
For each record that has a 'vm-folder' attribute, add an
\(email-regexp .  folder) element to the `vm-auto-folder-alist'.

The element gets added to the 'element-name' sublist of the
`vm-auto-folder-alist'.

The car of the element consists of all the email addresses for the
bbdb record concatenated with OR; the cdr is the value of the
vm-folder attribute.
If the fist character of vm-folders value is a quote (') it will be
parsed a lisp expression and consequently one may do his own tweaks
in order to get a nice folder name.

The only processing this defun does to the email address is to
`regexp-quote' it; if you're email circle is small enough, you could
consider using just the user part of the email address --- the part
before the @."
  (interactive)
  (let* (;; we add the email-address/vm-folder-name pair to this
	 ;; sublist of the vm-auto-folder-alist variable
	 (element-name "from\\|to\\|cc")
	 ;; grab the folder list from the vm-auto-folder-alist
	 (folder-list (assoc element-name vm-auto-folder-alist))
	 ;; the raw-notes and vm-folder attributes of the current bbdb
	 ;; record
	 notes-field folder
	 ;; a regexp matching all the email addresses from the bbdb
         ;; record
	 email-regexp
	 )
    ;; create the folder-list in vm-auto-folder-alist if it doesn't exist
    (unless folder-list
      (setq vm-auto-folder-alist (append vm-auto-folder-alist
                                         (list (list element-name)))
            folder-list (assoc element-name vm-auto-folder-alist)))
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-raw-notes record))
      (when (and (listp notes-field)
		 (setq folder (cdr (assq bbdb/vm-set-auto-folder-alist-field
					 notes-field))))
        ;; quote all the email addresses for the record and join them
        ;; with OR
	(setq email-regexp (mapconcat '(lambda (addr)
					 (regexp-quote addr))
				      (bbdb-record-net record) "\\|"))
	(unless (or (zerop (length email-regexp))
		    (assoc email-regexp folder-list))
	  ;; be careful: nconc modifies the list in place
	  (if (equal (elt folder 0) ?\')
	      (setq folder (read folder)))
	  (nconc folder-list (list (cons email-regexp folder))))))))


;;;###autoload
(defcustom bbdb/vm-snarf-all-headers
  '("From" "To" "CC" "Sender")
  "*List of headers to look for new email-addresses by `bbdb/vm-snarf-all'."
  :group 'bbdb-mua-specific-vm
  :type 'list)

;;;###autoload
(defun bbdb/vm-snarf-all (&optional offer-to-create)
  "Snarfs all email addresses from the headers.
The headers specified in `bbdb/vm-get-email-addresses-headers'.  are
searched.
Optional argument MSG is a VM message pointer."
  (interactive)

  (vm-check-for-killed-folder)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)

  (bbdb/vm-pop-up-bbdb-buffer t)

  (let ((bbdb/vm-get-from-headers bbdb/vm-snarf-all-headers)
	(bbdb/vm-get-first-from-p nil)
	(bbdb-message-cache nil)
	records)
    (setq records (bbdb/vm-update-records offer-to-create))
    (bbdb-display-records records)))


;;; bbdb/vm-auto-add-label
;;; Howard Melman, contributed Jun 16 2000
(defcustom bbdb/vm-auto-add-label-list nil
  "*List used by `bbdb/vm-auto-add-label' to automatically label messages.
Each element in the list is either a string or a list of two strings.
If a single string then it is used as both the field value to check for
and the label to apply to the message.  If a list of two strings, the first
is the field value to search for and the second is the label to apply."
  :group 'bbdb-mua-specific-vm
  :type 'list)

(defcustom bbdb/vm-auto-add-label-field bbdb-define-all-aliases-field
  "*Fields used by `bbdb/vm-auto-add-label' to automatically label messages.
Value is either a single symbol or a list of symbols of bbdb fields that
`bbdb/vm-auto-add-label' uses to check for labels to apply to messages.
Defaults to `bbdb-define-all-aliases-field' which is typically `mail-alias'."
  :group 'bbdb-mua-specific-vm
  :type '(choice symbol list))

(defun bbdb/vm-auto-add-label (record)
  "Automatically add labels to messages based on the mail-alias field.
Add this to `bbdb-notice-hook' and if using VM each message that bbdb
notices will be checked.  If the sender has a value in the
bbdb/vm-auto-add-label-field  in their BBDB record that
matches a value in `bbdb/vm-auto-add-label-list' then a VM
label will be added to the message.

This works great when `bbdb-user-mail-names' is set.  As a result
mail that you send to people (and copy yourself on) is labeled as well."
  (let (field aliases sep)
    (and (eq major-mode 'vm-mode)
	 (mapcar #'(lambda(x)
		     (and
		      (setq field (bbdb-record-getprop record x))
		      (setq sep (or (get x 'field-separator) ","))
		      (setq aliases (append aliases (bbdb-split field sep)))))
		 (cond ((listp bbdb/vm-auto-add-label-field)
			bbdb/vm-auto-add-label-field)
		       ((symbolp bbdb/vm-auto-add-label-field)
			(list bbdb/vm-auto-add-label-field))
		       (t (error "Bad value for bbdb/vm-auto-add-label-field"))
		       ))
	 (vm-add-message-labels
	  (mapconcat #'(lambda (l)
			 (cond ((stringp l)
				(if (member l aliases)
				    l))
			       ((and (consp l)
				     (stringp (car l))
				     (stringp (cdr l)))
				(if (member (car l) aliases)
				    (cdr l)))
			       (t
				(error "Malformed bbdb/vm-auto-add-label-list")
				)))
		     bbdb/vm-auto-add-label-list
		     " ")
	  1))))

;; this is how you hook it in.
;;(add-hook 'bbdb-notice-hook 'bbdb/vm-auto-add-label)


;;;###autoload
(defun bbdb-insinuate-vm ()
  "Call this function to hook BBDB into VM."
  (cond ((boundp 'vm-select-message-hook) ; VM 5.36+
	 (add-hook 'vm-select-message-hook 'bbdb/vm-pop-up-bbdb-buffer))
	((boundp 'vm-show-message-hook)	; VM 5.32.L+
	 (add-hook 'vm-show-message-hook 'bbdb/vm-pop-up-bbdb-buffer))
	(t
	 (error "vm versions older than 5.36 no longer supported")))
  (define-key vm-mode-map ":" 'bbdb/vm-show-sender)
  (define-key vm-mode-map ";" 'bbdb/vm-edit-notes)
  (define-key vm-mode-map "/" 'bbdb)
  ;; VM used to inherit from mail-mode-map, so bbdb-insinuate-sendmail
  ;; did this.  Kyle, you loser.
  (if (boundp 'vm-mail-mode-map)
      (define-key vm-mail-mode-map "\M-\t" 'bbdb-complete-name)))

(provide 'bbdb-vm)
