;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; Most of the user-level interactive commands for BBDB.  See bbdb.texinfo.
;;; last change 22-mar-96.

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

(defmacro bbdb-grovel-elide-arg (arg)
  (list 'if arg
	(list 'not (list 'eq arg 0))
	'bbdb-elided-display))


(defmacro bbdb-search (records &optional name company net notes phone)
  ;; this macro only emits code for those things being searched for;
  ;; literal nils at compile-time cause no code to be emitted.
  (let (clauses)
    ;; I didn't protect these vars from multiple evaluation because that
    ;; actually generates *less efficient code* in elisp, because the extra
    ;; bindings can't easily be optimized away without lexical scope.  fmh.
    (or (stringp name) (symbolp name) (error "name must be atomic"))
    (or (stringp company) (symbolp company) (error "company must be atomic"))
    (or (stringp net) (symbolp net) (error "net must be atomic"))
    (or (stringp notes) (symbolp notes) (error "notes must be atomic"))
    (or (stringp phone) (symbolp phone) (error "phone must be atomic"))
    (if phone
	(setq clauses
	      (cons
	       (` (let ((rest-of-phones (bbdb-record-phones record))
			(done nil))
		    (if rest-of-phones
			(while (and rest-of-phones (not done))
			  (setq done (string-match (, phone)
						   ;; way way wasteful...
						   (bbdb-phone-string
						    (car rest-of-phones)))
				rest-of-phones (cdr rest-of-phones)))
		      ;; so that "^$" can be used to find entries that
		      ;; have no phones
		      (setq done (string-match (, phone) "")))
		    done))
	       clauses)))
    (if notes
	(setq clauses
	      (cons
	       (` (if (stringp (, notes))
		      (string-match (, notes)
				    (or (bbdb-record-notes record) ""))
		    (if (eq (car (, notes)) '*)
			(let ((fields all-fields) done tmp)
			  (if (bbdb-record-raw-notes record)
			      (while (and (not done) fields)
				(setq tmp (bbdb-record-getprop
					   record (car fields))
				      done (and tmp (string-match
						     (cdr (, notes))
						     tmp))
				      fields (cdr fields)))
			    ;; so that "^$" can be used to find entries that
			    ;; have no notes
			    (setq done (string-match (cdr (, notes)) "")))
			  done)
		      (string-match (cdr (, notes))
				    (or (bbdb-record-getprop
					 record (car (, notes))) "")))))
	       clauses)))
    (if name
	(setq clauses
	      (append
	       (` ((string-match (, name) (or (bbdb-record-name record) ""))
		   (let ((rest-of-aka (bbdb-record-aka record))
			 (done nil))
		     (while (and rest-of-aka (not done))
		       (setq done (string-match (, name) (car rest-of-aka))
			     rest-of-aka (cdr rest-of-aka)))
		     done)))
	       clauses)))
    (if net
	(setq clauses
	      (cons
	       (` (let ((rest-of-nets (bbdb-record-net record))
			(done nil))
		    (if rest-of-nets
			(while (and rest-of-nets (not done))
			  (setq done (string-match (, net) (car rest-of-nets))
				rest-of-nets (cdr rest-of-nets)))
		      ;; so that "^$" can be used to find entries that
		      ;; have no net addresses.
		      (setq done (string-match (, net) "")))
		    done))
	       clauses)))
    (if company
	(setq clauses
	      (cons
	       (` (string-match (, company)
				(or (bbdb-record-company record) "")))
	       clauses)))

    (` (let ((matches '())
	     (,@ (if notes
		     '((all-fields (cons 'notes
					 (mapcar
					  (function (lambda (x)
						      (intern (car x))))
					  (bbdb-propnames)))))
		   nil))
	     (case-fold-search bbdb-case-fold-search)
	     (records (, records))
	     record)
	 (while records
	   (setq record (car records))
	   (if (or (,@ clauses))
	       (setq matches (cons record matches)))
	   (setq records (cdr records)))
	 (nreverse matches)))))


(defun bbdb (string elidep)
  "Display all entries in the BBDB matching the regexp STRING 
in either the name(s), company, network address, or notes."
  (interactive "sRegular Expression: \nP")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	(notes (cons '* string)))
    (bbdb-display-records
     (bbdb-search (bbdb-records) string string string notes nil))))

(defun bbdb-name (string elidep)
  "Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names\)."
  (interactive "sRegular Expression: \nP")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-records) string))))

(defun bbdb-company (string elidep)
  "Display all entries in BBDB matching STRING in the company field."
  (interactive "sRegular Expression: \nP")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-records) nil string))))

(defun bbdb-net (string elidep)
  "Display all entries in BBDB matching regexp STRING in the network address."
  (interactive "sRegular Expression: \nP")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-records) nil nil string))))

(defun bbdb-notes (which string elidep)
  "Display all entries in BBDB matching STRING in the named notes field."
  (interactive
    (list (completing-read "Notes field to search (RET for all): "
			   (append '(("notes")) (bbdb-propnames))
			   nil t)
	  (if (featurep 'gmhist)
	      (read-with-history-in 'bbdb-notes-field "Regular expression: ")
	      (read-string "Regular Expression: "))
	  current-prefix-arg))
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	(notes (if (string= which "")
		   (cons '* string)
		 (cons (intern which) string))))
    (bbdb-display-records (bbdb-search (bbdb-records) nil nil nil notes))))

(defun bbdb-phones (string elidep)
  "Display all entries in BBDB matching the regexp STRING in the phones field."
  (interactive "sRegular Expression: \nP")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records
     (bbdb-search (bbdb-records) nil nil nil nil string))))

(defun bbdb-changed (elidep)
  "Display all entries in the bbdb database which have been changed since
the database was last last saved."
  (interactive "P")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records
      (bbdb-with-db-buffer
	bbdb-changed-records))))

(defun bbdb-display (record)
  "Prompts for and displays a single record (this is faster than searching.)"
  (interactive (list (bbdb-completing-read-record "Display record of: ")))
  (bbdb-display-records (list record)))

;;; fancy redisplay

(defun bbdb-redisplay-records ()
  "Regrinds the contents of the *BBDB* buffer, without scrolling.
If possible, you should call bbdb-redisplay-one-record instead."
  (let ((p (point))
	(m (mark)))
    (goto-char (window-start))
    (let ((p2 (point)))
      (bbdb-display-records-1 bbdb-records)
      (goto-char p2)
      (if m (set-mark m)))
    (recenter 0)
    (goto-char p)
    (save-excursion
      (run-hooks 'bbdb-list-hook))
    ))

(defun bbdb-redisplay-one-record (record &optional record-cons next-record-cons
					 delete-p)
  "Regrind one record.  The *BBDB* buffer must be current when this is called."
  (bbdb-debug (if (not (eq (not (not delete-p))
			   (not (not (bbdb-record-deleted-p record)))))
		  (error "splorch.")))
  (if (null record-cons) (setq record-cons (assq record bbdb-records)))
  (if (null next-record-cons)
      (setq next-record-cons (car (cdr (memq record-cons bbdb-records)))))
  (beginning-of-line)
  (let ((marker (nth 2 record-cons))
	(next-marker (nth 2 next-record-cons))
	(buffer-read-only nil)
	(p (point)))
    (bbdb-debug
      (if (null record-cons) (error "doubleplus ungood: record unexists!"))
      (if (null marker) (error "doubleplus ungood: marker unexists!")))
    (goto-char marker)
    (if delete-p nil
	(bbdb-format-record (car record-cons) (car (cdr record-cons))))
    (delete-region (point) (or next-marker (point-max)))
    (goto-char p)
    (save-excursion
      (run-hooks 'bbdb-list-hook))
    ))

;;; Parsing phone numbers

(defconst bbdb-phone-area-regexp "(?[ \t]*\\+?1?[ \t]*[-\(]?[ \t]*[-\(]?[ \t]*\\([0-9][012][0-9]\\)[ \t]*)?[- \t]*")
(defconst bbdb-phone-main-regexp "\\([1-9][0-9][0-9]\\)[ \t]*-?[ \t]*\\([0-9][0-9][0-9][0-9]\\)[ \t]*")
(defconst bbdb-phone-ext-regexp  "x?[ \t]*\\([0-9]+\\)[ \t]*")

(defconst bbdb-phone-regexp-1 (concat "^[ \t]*" bbdb-phone-area-regexp bbdb-phone-main-regexp bbdb-phone-ext-regexp "$"))
(defconst bbdb-phone-regexp-2 (concat "^[ \t]*" bbdb-phone-area-regexp bbdb-phone-main-regexp "$"))
(defconst bbdb-phone-regexp-3 (concat "^[ \t]*" bbdb-phone-main-regexp bbdb-phone-ext-regexp "$"))
(defconst bbdb-phone-regexp-4 (concat "^[ \t]*" bbdb-phone-main-regexp "$"))
(defconst bbdb-phone-regexp-5 (concat "^[ \t]*" bbdb-phone-ext-regexp "$"))

(defun bbdb-parse-phone-number (string &optional number-type)
  "Parse a phone number from STRING and return a list of integers the form
\(area-code exchange number) or (area-code exchange number extension).
This is both lenient and strict in what it will parse - whitespace may 
appear (or not) between any of the groups of digits, parentheses around the
area code are optional, as is a dash between the exchange and number, and
a '1' preceeding the area code; but there must be three digits in the area 
code and exchange, and four in the number (if they are present).  An error 
will be signalled if unparsable.  All of these are unambigously parsable:

  ( 415 ) 555 - 1212 x123   ->   (415 555 1212 123)
  (415)555-1212 123         ->   (415 555 1212 123)
  (1-415) 555-1212 123      ->   (415 555 1212 123)
  1 (415)-555-1212 123      ->   (415 555 1212 123)
  555-1212 123              ->   (0 555 1212 123)
  555 1212                  ->   (0 555 1212)
  415 555 1212              ->   (415 555 1212)
  1 415 555 1212            ->   (415 555 1212)
  5551212                   ->   (0 555 1212)
  4155551212                ->   (415 555 1212)
  4155551212123             ->   (415 555 1212 123)
  5551212x123               ->   (0 555 1212 123)
  1234                      ->   (0 0 0 1234)

Note that \"4151212123\" is ambiguous; it could be interpreted either as
\"(415) 121-2123\" or as \"415-1212 x123\".  (However, all area codes have
either 0, 1, or 2 as their second digit, and no exchange begins with 0, 
so this function can sometimes use that to disambiguate.)

\(And uh, oh yeah, this does little if bbdb-north-american-phone-numbers-p
is nil...\)"

  (cond ((if number-type
	     (eq number-type 'euro)
	   (not bbdb-north-american-phone-numbers-p))
	 (list (bbdb-string-trim string)))
	((string-match bbdb-phone-regexp-1 string)
	 ;; (415) 555-1212 x123
	 (list (bbdb-subint string 1) (bbdb-subint string 2)
	       (bbdb-subint string 3) (bbdb-subint string 4)))
	((string-match bbdb-phone-regexp-2 string)
	 ;; (415) 555-1212
	 (list (bbdb-subint string 1) (bbdb-subint string 2)
	       (bbdb-subint string 3)))
	((string-match bbdb-phone-regexp-3 string)
	 ;; 555-1212 x123
	 (list 0 (bbdb-subint string 1) (bbdb-subint string 2)
	       (bbdb-subint string 3)))
	((string-match bbdb-phone-regexp-4 string)
	 ;; 555-1212
	 (list 0 (bbdb-subint string 1) (bbdb-subint string 2)))
	((string-match bbdb-phone-regexp-5 string)
	 ;; x123
	 (list 0 0 0 (bbdb-subint string 1)))
	(t (error "phone number unparsable."))))

;;; Parsing other things

(defun bbdb-parse-zip-string (string)
  (cond ((string-match "^[ \t\n]*$" string) 0)
	((string-match "^[ \t\n]*[0-9][0-9][0-9][0-9][0-9][ \t\n]*$" string)
	 (string-to-int string))
	((string-match "^[ \t\n]*\\([0-9][0-9][0-9][0-9][0-9]\\)[ \t\n]*-?[ \t\n]*\\([0-9][0-9][0-9][0-9]\\)[ \t\n]*$" string)
	 (list (bbdb-subint string 1) (bbdb-subint string 2)))
	;; Match zip codes for Canada, UK, etc. (result is ("LL47" "U4B")).
	((string-match
	  "^[ \t\n]*\\([A-Za-z0-9]+\\)[ \t\n]+\\([A-Za-z0-9]+\\)[ \t\n]*$"
	  string)
	 (list (substring string (match-beginning 1) (match-end 1))
	       (substring string (match-beginning 2) (match-end 2))))
	((string-match "-[^-]-" string)
	 (error "too many dashes in zip code."))
	((string-match "[^-0-9 \t\n]" string)
	 (error "illegal characters in zip code."))
	((string-match "[0-9][0-9][0-9][0-9][0-9][0-9]" string)
	 (error "too many digits in zip code."))
	((< (length string) 5)
	 (error "not enough digits in zip code."))
	(t (error "not a valid 5-digit or 5+4 digit zip code."))))


(defun bbdb-read-new-record ()
  "Prompt for and return a completely new bbdb-record.  Doesn't insert it in to
the database or update the hashtables, but does insure that there will not be
name collisions."
  (bbdb-records) ; make sure database is loaded
  (if bbdb-readonly-p (error "The Insidious Big Brother Database is read-only."))
  (let (firstname lastname)
    (bbdb-error-retry
      (progn
	(if current-prefix-arg
	    (setq firstname (bbdb-read-string "First Name: ")
		  lastname (bbdb-read-string "Last Name: "))
	  (let ((names (bbdb-divide-name (bbdb-read-string "Name: "))))
	    (setq firstname (car names)
		  lastname (nth 1 names))))
	(if (string= firstname "") (setq firstname nil))
	(if (string= lastname "") (setq lastname nil))
	(if (bbdb-gethash (downcase (if (and firstname lastname) (concat firstname " " lastname)
					(or firstname lastname ""))))
	    (error "%s %s is already in the database" (or firstname "") (or lastname "")))))
    (let ((company (bbdb-read-string "Company: "))
	  (net (bbdb-split (bbdb-read-string "Network Address: ") ","))
	  (addrs (let (L L-tail str addr)
		   (while (not (string= ""
				 (setq str (bbdb-read-string "Address Description [RET when no more addrs]: "))))
		     (setq addr (make-vector bbdb-address-length nil))
		     (bbdb-record-edit-address addr str)
		     (if L
			 (progn (setcdr L-tail (cons addr nil))
				(setq L-tail (cdr L-tail)))
			 (setq L (cons addr nil)
			       L-tail L)))
		   L))
	  (phones (let (L L-tail str)
		    (while (not (string= ""
					 (setq str
					       (bbdb-read-string "Phone Location [RET when no more phones]: "))))
		      (let* ((phonelist
			      (bbdb-error-retry
				(bbdb-parse-phone-number
				  (read-string "Phone: "
					       (and bbdb-default-area-code (format "(%03d) " bbdb-default-area-code))))))
			     (phone (apply 'vector str
					   (if (= 3 (length phonelist))
					       (nconc phonelist '(0))
					       phonelist))))
			(if L
			    (progn (setcdr L-tail (cons phone nil))
				   (setq L-tail (cdr L-tail)))
			    (setq L (cons phone nil)
				  L-tail L))))
		    L))
	  (notes (bbdb-read-string "Additional Comments: ")))
      (if (string= company "") (setq company nil))
      (if (string= notes "") (setq notes nil))
      (let ((record
	     (vector firstname lastname nil company phones addrs net notes
		     (make-vector bbdb-cache-length nil))))
	record))))

(defun bbdb-create (record)
  "Add a new entry to the bbdb database; prompts for all relevant info
using the echo area, inserts the new record in the db, sorted alphabetically,
and offers to save the db file.  DO NOT call this from a program.  Call
bbdb-create-internal instead."
  (interactive (list (bbdb-read-new-record)))
  (bbdb-invoke-hook 'bbdb-create-hook record)
  (bbdb-change-record record t)
  (bbdb-display-records (list record)))


(defmacro bbdb-check-type (place predicate)
  (list 'while (list 'not (list predicate place))
	(nconc (cond ((eq (car-safe place) 'aref)
		      (list 'aset (nth 1 place) (nth 2 place)))
		     ((eq (car-safe place) 'car)
		      (list 'setcar (nth 1 place)))
		     ((eq (car-safe place) 'cdr)
		      (list 'setcdr (nth 1 place)))
		     (t (list 'setq place)))
	       (list 
		(list 'signal ''wrong-type-argument
		      (list 'list (list 'quote predicate) place))))))


(defun bbdb-create-internal (name company net addrs phones notes)
  "Adds a record to the database; this function does a fair amount of
error-checking on the passed in values, so it's safe to call this from
other programs.

NAME is a string, the name of the person to add.  An error is signalled
 if that name is already in use.
COMPANY is a string or nil.
NET is a comma-separated list of email addresses, or a list of strings.
 An error is signalled if that name is already in use.
ADDRS is a list of address objects.  An address is a vector of the form
   [\"location\" \"line1\" \"line2\" \"line3\" \"City\" \"State\" zip]
 where `zip' is nil, an integer, or a cons of two integers.
PHONES is a list of phone-number objects.  A phone-number is a vector of
 the form
   [\"location\" areacode prefix suffix extension-or-nil]
 or
   [\"location\" \"phone-number\"]
NOTES is a string, or an alist associating symbols with strings."
  (let (firstname lastname aka)
    (while (progn
	     (setq name (and name (bbdb-divide-name name)))
	     (setq firstname (car name) lastname (nth 1 name))
	     (bbdb-gethash (downcase (if (and firstname lastname)
					 (concat firstname " " lastname)
				       (or firstname lastname "")))))
      (setq name (signal 'error
			 (list (format "%s %s is already in the database"
				       (or firstname "") (or lastname ""))))))
    (and company (bbdb-check-type company stringp))
    (if (stringp net)
	(setq net (bbdb-split net ",")))
    (let ((rest net))
      (while rest
	(while (bbdb-gethash (downcase (car rest)))
	  (setcar rest
		  (signal 'error (list (format
					"%s is already in the database"
					(car rest))))))
	(setq rest (cdr rest))))
    (setq addrs
	  (mapcar
	    (function (lambda (addr)
	      (while (or (not (vectorp addr))
			 (/= (length addr) bbdb-address-length))
		(setq addr (signal 'wrong-type-argument (list 'vectorp addr))))
	      (bbdb-check-type (aref addr 0) stringp)
	      (bbdb-check-type (aref addr 1) stringp)
	      (bbdb-check-type (aref addr 2) stringp)
	      (bbdb-check-type (aref addr 3) stringp)
	      (bbdb-check-type (aref addr 4) stringp)
	      (bbdb-check-type (aref addr 5) stringp)
	      (while (and (aref addr 6)
			  (not (integerp (aref addr 6)))
			  (not (and (consp (aref addr 6))
				    (integerp (car (aref addr 6)))
				    (integerp (car (cdr (aref addr 6))))
				    (null (cdr (cdr (aref addr 6)))))))
		(aset addr 6 (signal 'wrong-type-argument
				     (list 'zipcodep (aref addr 6)))))
	      addr))
	    addrs))
    (setq phones
	  (mapcar
	   (function (lambda (phone)
	     (while (or (not (vectorp phone))
			(and (/= (length phone) 2)
			     (/= (length phone) bbdb-phone-length)))
	       (setq phone
		     (signal 'wrong-type-argument (list 'vectorp phone))))
	     (bbdb-check-type (aref phone 0) stringp)
	     (if (= 2 (length phone))
		 (bbdb-check-type (aref phone 1) stringp)
	       (bbdb-check-type (aref phone 1) integerp)
	       (bbdb-check-type (aref phone 2) integerp)
	       (bbdb-check-type (aref phone 3) integerp)
	       (and (aref phone 4) (bbdb-check-type (aref phone 4) integerp))
	       (if (eq 0 (aref phone 4)) (aset phone 4 nil)))
	     phone))
	   phones))
    (or (stringp notes)
	(setq notes
	      (mapcar (function (lambda (note)
		        (bbdb-check-type note consp)
			(bbdb-check-type (car note) symbolp)
			(if (consp (cdr note))
			    (setq note (cons (car note) (car (cdr note)))))
			(bbdb-check-type (cdr note) stringp)
		        note))
		      notes)))
    (let ((record
	   (vector firstname lastname aka company phones addrs net notes
		   (make-vector bbdb-cache-length nil))))
      (bbdb-invoke-hook 'bbdb-create-hook record)
      (bbdb-change-record record t)
      record)))


;;; bbdb-mode stuff

(defun bbdb-current-record (&optional planning-on-modifying)
  "Returns the record which the point is point at.  In linear time, man..."
  (if (and planning-on-modifying bbdb-readonly-p)
      (error "The Insidious Big Brother Database is read-only."))
  (if (not (equal bbdb-buffer-name (buffer-name (current-buffer))))
      (error "this command only works while in the \"%s\" buffer."
	     bbdb-buffer-name))
  (let ((p (point))
	(rest bbdb-records)
	(rec nil))
    (while (and (cdr rest) (not rec))
      (if (> (nth 2 (car (cdr rest))) p)
	  (setq rec (car (car rest))))
      (setq rest (cdr rest)))
    (or rec (car (car rest)))))


;; yow, are we object oriented yet?
(defun bbdb-record-get-field-internal (record field)
  (cond ((eq field 'name)	(bbdb-record-name record))
	((eq field 'net)	(bbdb-record-net record))
	((eq field 'aka)	(bbdb-record-aka record))
	((eq field 'phone)	(bbdb-record-phones record))
	((eq field 'address)	(bbdb-record-addresses record))
	((eq field 'property)	(bbdb-record-raw-notes record))
	(t (error "doubleplus ungood: unknown field type %s" field))))

(defun bbdb-record-store-field-internal (record field value)
  (cond ((eq field 'name)	(error "doesn't work on names"))
	((eq field 'net)	(bbdb-record-set-net record value))
	((eq field 'aka)	(bbdb-record-set-aka record value))
	((eq field 'phone)	(bbdb-record-set-phones record value))
	((eq field 'address)	(bbdb-record-set-addresses record value))
	((eq field 'property)	(bbdb-record-set-raw-notes record value))
	(t (error "doubleplus ungood: unknown field type %s" field))))

(defun bbdb-record-edit-field-internal (record field &optional which)
  (cond ((eq field 'name)	(bbdb-record-edit-name record))
	((eq field 'net)	(bbdb-record-edit-net record))
	((eq field 'aka)	(bbdb-record-edit-aka record))
	((eq field 'phone)	(bbdb-record-edit-phone which))
	((eq field 'address)	(bbdb-record-edit-address which))
	((eq field 'property)	(bbdb-record-edit-property record (car which)))
	(t (error "doubleplus ungood: unknown field type %s" field))))

	
(defun bbdb-current-field (&optional planning-on-modifying)
  (save-excursion
    ;; get to beginning of this record
    (beginning-of-line)
    (let ((p (point)))
      (while (not (or (eobp) (looking-at "^[^ \t\n]")))
	(forward-line -1))
      (let* ((record (or (bbdb-current-record planning-on-modifying)
			 (error "unperson")))
	     (bbdb-elided-display (nth 1 (assq record bbdb-records)))
	     (count 0)
	     (tmp (nconc
		   (list (list 'name record))
		   (and (bbdb-field-shown-p 'phone)
		     (mapcar (function (lambda (phone) (list 'phone phone)))
			     (bbdb-record-phones record)))
		   (and (bbdb-field-shown-p 'address)
		     (apply 'nconc
		       (mapcar (function (lambda (addr)
				(let ((L (list 'address addr)))
				  (nconc
				   (if (string= "" (bbdb-address-street1 addr))
				       nil (list L))
				   (if (string= "" (bbdb-address-street2 addr))
				       nil (list L))
				   (if (string= "" (bbdb-address-street3 addr))
				       nil (list L))
				   (list L)))))
			       (bbdb-record-addresses record))))
		   (if (and (bbdb-record-net record)
			    (bbdb-field-shown-p 'net))
		       (list (list 'net record)))
		   (if (and (bbdb-record-aka record)
			    (bbdb-field-shown-p 'aka))
		       (list (list 'aka record)))
		   (let ((notes (bbdb-record-raw-notes record)))
		     (if (stringp notes)
			 (setq notes (list (cons 'notes notes))))
		     (apply
		       'nconc
		       (mapcar
			(function (lambda (note)
			  (if (bbdb-field-shown-p (car note))
			      (let* ((L (list 'property note))
				     (LL (list L))
				     (i 0))
				(while (string-match "\n" (cdr note) i)
				  (setq i (match-end 0)
					LL (cons L LL)))
				LL))))
			notes)))
		   )))
	(while (< (point) p)
	  (setq count (1+ count))
	  (forward-line 1))
	(nth count tmp)))))


(defun bbdb-apply-next-command-to-all-records ()
  "Typing \\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] \
in the *BBDB* buffer makes the next command operate on all
of the records currently displayed.  \(Note that this only works for
certain commands.\)"
  (interactive)
  (message (substitute-command-keys
	    "\\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] - "))
  (setq prefix-arg current-prefix-arg
	last-command this-command)
  nil)

(defmacro bbdb-do-all-records-p ()
  "Whether the last command was bbdb-apply-next-command-to-all-records."
  '(eq last-command 'bbdb-apply-next-command-to-all-records))


(defun bbdb-insert-new-field (name contents)
  "Add a new field to the current record; the field type and contents
are prompted for if not supplied.

If you are inserting a new phone-number field, you can control whether
it is a north american or european phone number by providing a prefix
argument.  A prefix arg of ^U means it's to be a euronumber, and any
other prefix arg means it's to be a a structured north american number.
Otherwise, which style is used is controlled by the variable
bbdb-north-american-phone-numbers-p."
  (interactive (let ((name "")
		     (completion-ignore-case t))
		 (while (string= name "")
		   (setq name
			 (downcase
			   (completing-read "Insert Field: "
			     (append '(("phone") ("address") ("net")
				       ("AKA") ("notes"))
				     (bbdb-propnames))
			     nil
			     nil ; used to be t
			     nil))))
		 (setq name (intern name))
		 (list name (bbdb-prompt-for-new-field-value name))))
  (if (null contents)
      (setq contents (bbdb-prompt-for-new-field-value name)))
  (let ((record (bbdb-current-record t)))
    (if (null record) (error "current record unexists!"))
    (cond ((eq name 'phone)
	   (bbdb-record-set-phones record
	     (nconc (bbdb-record-phones record) (list contents))))
	  ((eq name 'address)
	   (bbdb-record-set-addresses record
	     (nconc (bbdb-record-addresses record) (list contents))))
	  ((eq name 'net)
	   (if (bbdb-record-net record)
	       (error "There already are net addresses!"))
	   (if (stringp contents)
	       (setq contents (bbdb-split contents ",")))
	   ;; first detect any conflicts....
	   (let ((nets contents))
	     (while nets
	       (let ((old (bbdb-gethash (downcase (car nets)))))
		 (if (and old (not (eq old record)))
		     (error "net address \"%s\" is used by \"%s\""
			    (car nets)
			    (or (bbdb-record-name old) (car (bbdb-record-net old))))))
	       (setq nets (cdr nets))))
	   ;; then store.
	   (let ((nets contents))
	     (while nets
	       (bbdb-puthash (downcase (car nets)) record)
	       (setq nets (cdr nets))))
	   (bbdb-record-set-net record contents)
	   )
	  ((eq name 'aka)
	   (if (bbdb-record-aka record)
	       (error "there already are alternate names!"))
	   (if (stringp contents)
	       (setq contents (bbdb-split contents ";")))
	   ;; first detect any conflicts....
	   (let ((aka contents))
	     (while aka
	       (let ((old (bbdb-gethash (downcase (car aka)))))
		 (if (and old (not (eq old record)))
		     (error "alternate name \"%s\" is used by \"%s\""
			    (car aka)
			    (or (bbdb-record-name old)
				(car (bbdb-record-net old))))))
	       (setq aka (cdr aka))))
	   ;; then store.
	   (let ((aka contents))
	     (while aka
	       (bbdb-puthash (downcase (car aka)) record)
	       (setq aka (cdr aka))))
	   (bbdb-record-set-aka record contents)
	   )
	  ((eq name 'notes)
	   (if (bbdb-record-notes record) (error "there already are notes!"))
	   (bbdb-record-set-notes record contents))
	  ((assoc (symbol-name name) (bbdb-propnames))
	   (if (and (consp (bbdb-record-raw-notes record))
		    (assq name (bbdb-record-raw-notes record)))
	       (error "there is already a \"%s\" note!" name))
	   (bbdb-record-putprop record name contents))
	  (t (error "doubleplus ungood: unknow how to set slot %s" name)))
    (bbdb-change-record record nil)
;    (bbdb-offer-save)
    (let ((bbdb-elided-display nil))
      (bbdb-redisplay-one-record record))))

(defun bbdb-prompt-for-new-field-value (name)
  (cond ((eq name 'net) (bbdb-read-string "Net: "))
	((eq name 'aka) (bbdb-read-string "Alternate Names: "))
	((eq name 'phone)
	 (let ((p (make-vector
		    (if (if current-prefix-arg
			    (numberp current-prefix-arg)
			    bbdb-north-american-phone-numbers-p)
			bbdb-phone-length
			2)
		    0)))
	   (aset p 0 nil)
	   (aset p 1
		 (if (= bbdb-phone-length (length p))
		     (or bbdb-default-area-code 0)
		     nil))
	   (bbdb-record-edit-phone p)
	   p))
	((eq name 'address)
	 (let ((a (make-vector bbdb-address-length nil)))
	   (bbdb-record-edit-address a)
	   a))
	((eq name 'notes) (bbdb-read-string "Notes: "))
	((assoc (symbol-name name) (bbdb-propnames))
	 (bbdb-read-string (format "%s: " name)))
	(t
	 (if (bbdb-y-or-n-p (format "\"%s\" is an unknown field name.  Define it? " name))
	     (bbdb-set-propnames
	       (append (bbdb-propnames) (list (list (symbol-name name)))))
	     (error "unknown field \"%s\"" name))
	 (bbdb-read-string (format "%s: " name)))))


(defun bbdb-edit-current-field ()
  "Edit the contents of the Insidious Big Brother Database field displayed on 
the current line (this is only meaningful in the \"*BBDB*\" buffer.)   If the 
cursor is in the middle of a multi-line field, such as an address or comments 
section, then the entire field is edited, not just the current line."
  (interactive)
  (let* ((record (bbdb-current-record t))
	 (field (bbdb-current-field t))
	 need-to-sort)
    (or field (error "on an unfield"))
    (setq need-to-sort
	  (bbdb-record-edit-field-internal record (car field) (nth 1 field)))
    (bbdb-change-record record need-to-sort)
    (bbdb-redisplay-one-record record)
;    (bbdb-offer-save)
    ))

(defun bbdb-record-edit-name (bbdb-record)
  (let (fn ln co need-to-sort new-name old-name)
    (bbdb-error-retry
      (progn
	(if current-prefix-arg
	    (setq fn (bbdb-read-string "First Name: "
				       (bbdb-record-firstname bbdb-record))
		  ln (bbdb-read-string "Last Name: "
				       (bbdb-record-lastname bbdb-record)))
	  (let ((names (bbdb-divide-name
			(bbdb-read-string "Name: "
			  (bbdb-record-name bbdb-record)))))
	    (setq fn (car names)
		  ln (nth 1 names))))
	(setq need-to-sort (or (not (string= fn
					     (or (bbdb-record-firstname bbdb-record) "")))
			       (not (string= ln
					     (or (bbdb-record-lastname bbdb-record) "")))))
	(if (string= "" fn) (setq fn nil))
	(if (string= "" ln) (setq ln nil))
	;; check for collisions
	(setq new-name (if (and fn ln) (concat fn " " ln)
			   (or fn ln))
	      old-name (bbdb-record-name bbdb-record))
	(if (and new-name
		 (not (and old-name (string= (downcase new-name)
					     (downcase old-name))))
		 (bbdb-gethash (downcase new-name)))
	    (error "%s is already in the database!" new-name))))
    (setq co (bbdb-read-string "Company: "
			       (bbdb-record-company bbdb-record)))
    (if (string= "" co) (setq co nil))
    (setq need-to-sort
	  (or need-to-sort
	      (not (equal (if co (downcase co) "")
			  (downcase (or (bbdb-record-company bbdb-record)
					""))))))
    ;;
    ;; delete the old hash entry
    (and (bbdb-record-name bbdb-record)
	 (bbdb-remhash (downcase (bbdb-record-name bbdb-record))))
    (bbdb-record-set-namecache bbdb-record nil)
    (bbdb-record-set-firstname bbdb-record fn)
    (bbdb-record-set-lastname bbdb-record ln)
    (bbdb-record-set-company bbdb-record co)
    ;; add a new hash entry
    (and (or fn ln)
	 (bbdb-puthash (downcase (bbdb-record-name bbdb-record))
		       bbdb-record))
    need-to-sort))

(defun bbdb-record-edit-address (addr &optional location)
  (let* ((loc (or location (bbdb-read-string "Location: " (bbdb-address-location addr))))
	 (st1 (bbdb-read-string "Street, line 1: " (bbdb-address-street1 addr)))
	 (st2 (if (string= st1 "") ""
		  (bbdb-read-string "Street, line 2: " (bbdb-address-street2 addr))))
	 (st3 (if (string= st2 "") ""
		  (bbdb-read-string "Street, line 3: " (bbdb-address-street3 addr))))
	 (cty (bbdb-read-string "City: " (bbdb-address-city addr)))
	 (ste (bbdb-read-string "State: " (bbdb-address-state addr)))
	 (zip (bbdb-error-retry
		(bbdb-parse-zip-string
		  (bbdb-read-string "Zip Code: " (bbdb-address-zip-string addr))))))
    (bbdb-address-set-location addr loc)
    (bbdb-address-set-street1 addr st1)
    (bbdb-address-set-street2 addr st2)
    (bbdb-address-set-street3 addr st3)
    (bbdb-address-set-city addr cty)
    (bbdb-address-set-state addr ste)
    (bbdb-address-set-zip addr zip)
    nil))

(defun bbdb-record-edit-phone (phone-number)
  (let ((newl (bbdb-read-string "Location: "
				 (bbdb-phone-location phone-number)))
	(newp (let ((bbdb-north-american-phone-numbers-p
		     (= (length phone-number) bbdb-phone-length)))
		(bbdb-error-retry
		  (bbdb-parse-phone-number
		    (read-string "Phone: " (bbdb-phone-string phone-number))
		    )))))
    (bbdb-phone-set-location phone-number newl)
    (bbdb-phone-set-area phone-number (nth 0 newp)) ; euronumbers too.
    (if (= (length phone-number) 2)
	nil
      (bbdb-phone-set-exchange phone-number (nth 1 newp))
      (bbdb-phone-set-suffix phone-number (nth 2 newp))
      (bbdb-phone-set-extension phone-number (or (nth 3 newp) 0))))
  nil)

(defun bbdb-record-edit-net (bbdb-record)
  (let ((str (bbdb-read-string "Net: "
	       (mapconcat (function identity)
			  (bbdb-record-net bbdb-record)
			  ", "))))
    (let ((oldnets (bbdb-record-net bbdb-record))
	  (newnets (bbdb-split str ",")))
      ;; first check for any conflicts...
      (let ((rest newnets))
	(while rest
	  (let ((old (bbdb-gethash (downcase (car rest)))))
	    (if (and old (not (eq old bbdb-record)))
		(error "net address \"%s\" is used by \"%s\""
		       (car rest) (bbdb-record-name old))))
	  (setq rest (cdr rest))))
      ;; then update.
      (let ((rest oldnets))
	(while rest
	  (bbdb-remhash (downcase (car rest)))
	  (setq rest (cdr rest))))
      (let ((nets newnets))
	(while nets
	  (bbdb-puthash (downcase (car nets)) bbdb-record)
	  (setq nets (cdr nets))))
      (bbdb-record-set-net bbdb-record newnets)
      ))
  nil)

(defun bbdb-record-edit-aka (bbdb-record)
  (let ((str (bbdb-read-string "AKA: "
	       (mapconcat (function identity)
			  (bbdb-record-aka bbdb-record)
			  "; "))))
    (let ((oldaka (bbdb-record-aka bbdb-record))
	  (newaka (bbdb-split str ";")))
      ;; first check for any conflicts...
      (let ((rest newaka))
	(while rest
	  (let ((old (bbdb-gethash (downcase (car rest)))))
	    (if (and old (not (eq old bbdb-record)))
		(error "alternate name address \"%s\" is used by \"%s\""
		       (car rest) (bbdb-record-name old))))
	  (setq rest (cdr rest))))
      ;; then update.
      (let ((rest oldaka))
	(while rest
	  (bbdb-remhash (downcase (car rest)))
	  (setq rest (cdr rest))))
      (let ((aka newaka))
	(while aka
	  (bbdb-puthash (downcase (car aka)) bbdb-record)
	  (setq aka (cdr aka))))
      (bbdb-record-set-aka bbdb-record newaka)
      ))
  nil)

(defun bbdb-record-edit-notes (bbdb-record &optional regrind)
  (interactive (list (bbdb-current-record t) t))
  (let ((notes (bbdb-read-string "Notes: " (bbdb-record-notes bbdb-record))))
    (bbdb-record-set-notes bbdb-record (if (string= "" notes) nil notes)))
  (if regrind
      (save-excursion
	(set-buffer bbdb-buffer-name)
	(bbdb-redisplay-one-record bbdb-record)))
  nil)

(defun bbdb-record-edit-property (bbdb-record &optional prop regrind)
  (interactive (list (bbdb-current-record t) nil t))
  (let* ((propnames (bbdb-propnames))
	 (propname (if prop (symbol-name prop)
		     (completing-read
		       (format "Edit property of %s: "
			       (bbdb-record-name bbdb-record))
		       (cons '("notes") propnames))))
	 (propsym (or prop (if (equal "" propname) 'notes (intern propname))))
	 (string (bbdb-read-string (format "%s: " propname)
				   (bbdb-record-getprop bbdb-record propsym))))
    (bbdb-record-putprop bbdb-record propsym
			 (if (string= "" string) nil string)))
  (if regrind
      (save-excursion
	(set-buffer bbdb-buffer-name)
	(bbdb-redisplay-one-record bbdb-record)))
  nil)


(defsubst bbdb-field-equal (x y)
  (if (and (consp x) (consp y))
      (and (eq (car x) (car y))
	   (eq (car (cdr x)) (car (cdr y)))
	   (eq (car (cdr (cdr x))) (car (cdr (cdr y)))))
    (eq x y)))

(defun bbdb-next-field (&optional count planning-on-modifying)
  (or count (setq count 1))
  (beginning-of-line)
  (let* ((record (bbdb-current-record planning-on-modifying))
	 (field (bbdb-current-field planning-on-modifying))
	 (next-record record)
	 (next-field field)
	 (signum (if (< count 0) -1 1))
	 (i 0))
    (if (< count 0) (setq count (- count)))
    (if field
	(while (and next-field (< i count))
	  (while (bbdb-field-equal next-field field)
	    (forward-line signum)
	    (setq next-record (bbdb-current-record planning-on-modifying)
		  next-field (bbdb-current-field planning-on-modifying))
	    (or (eq next-record record)
		(setq next-field nil)))
	  (setq i (1+ i))
	  (setq field next-field)))
    next-field))

(defun bbdb-transpose-fields (&optional arg)
  "This is like the `transpose-lines' command, but it is for BBDB fields.
If the cursor is on a field of a BBDB record, that field and the previous
field will be transposed.

With argument ARG, takes previous line and moves it past ARG fields.
With argument 0, interchanges field point is in with field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone-number
fields are listed, but you can't use it to make an address appear before a
phone number; the order of field types is fixed.\)"
  (interactive "p")
  (let ((record (bbdb-current-record t))
	moving-field position-after position-before
	swap-p type list)
    (if (/= arg 0)
	(setq moving-field (or (bbdb-next-field -1 t)
			       (error "no previous field"))
	      position-after (bbdb-next-field arg t)
	      position-before (bbdb-next-field (if (< arg 0) -1 1) t))
      ;; if arg is 0, swap fields at point and mark
      (setq swap-p t)
      (setq position-after (bbdb-current-field))
      (save-excursion
	(goto-char (mark))
	(setq moving-field (bbdb-current-field))
	(or (eq record (bbdb-current-record)) (error "not in the same record"))
	))
    (if (< arg 0)
	(let ((x position-after))
	  (setq position-after position-before
		position-before x)
	  (forward-line 2)))
    (setq type (car moving-field))
    (or position-after position-before
	(error "that would be out of the record!"))
    (or (eq type (car position-after))
	(eq type (car position-before))
	(error "can't transpose fields of different types (%s and %s)"
	       type (if (eq type (car position-after))
			(car position-before) (car position-after))))
    (or (eq type (car position-after)) (setq position-after nil))
    (or (eq type (car position-before)) (setq position-before nil))
    (setq moving-field (nth 1 moving-field)
	  position-after (nth 1 position-after)
	  position-before (nth 1 position-before))
    (cond ((memq type '(name aka net))
	   (error "there is only one %s field, so you can't transpose it"
		  type))
	  ((memq type '(phone address property))
	   (setq list (bbdb-record-get-field-internal record type)))
	  (t (error "doubleplus ungood: unknown field %s" type)))
    (if swap-p
	(let ((rest list))
	  (while rest
	    (cond ((eq (car rest) moving-field) (setcar rest position-after))
		  ((eq (car rest) position-after) (setcar rest moving-field)))
	    (setq rest (cdr rest))))
      (if (eq position-before (car list))
	  (setq list (cons moving-field (delq moving-field list)))
	(let ((rest list))
	  (while (and rest (not (eq position-after (car rest))))
	    (setq rest (cdr rest)))
	  (or rest (error "doubleplus ungood: couldn't reorder list"))
	  (let ((inhibit-quit t))
	    (setq list (delq moving-field list))
	    (setcdr rest (cons moving-field (cdr rest)))))))
    (bbdb-record-store-field-internal record type list)
    (bbdb-change-record record nil)
    (bbdb-redisplay-one-record record)))


(defun bbdb-delete-current-field-or-record ()
  "Delete the line which the cursor is on; actually, delete the field which
that line represents from the database.  If the cursor is on the first line
of a database entry (the name/company line) then the entire entry will be
deleted."
  (interactive)
  (let* ((record (bbdb-current-record t))
	 (field (bbdb-current-field t))
	 (type (car field))
	 (uname (bbdb-record-name record))
	 (name (cond ((null field) (error "on an unfield"))
		     ((eq type 'property) (symbol-name (car (nth 1 field))))
		     (t (symbol-name type)))))
    (if (eq type 'name)
	(bbdb-delete-current-record record)
	(if (not (bbdb-y-or-n-p (format "delete this %s field (of %s)? "
					name uname)))
	    nil
	  (cond ((memq type '(phone address))
		 (bbdb-record-store-field-internal record type
		 (delq (nth 1 field)
		       (bbdb-record-get-field-internal record type))))
		((memq type '(net aka))
		 (let ((rest (bbdb-record-get-field-internal record type)))
		   (while rest
		     (bbdb-remhash (downcase (car rest)))
		     (setq rest (cdr rest))))
		 (bbdb-record-store-field-internal record type nil))
		((eq type 'property)
		 (bbdb-record-putprop record (car (nth 1 field)) nil))
		(t (error "doubleplus ungood: unknown field type")))
	  (bbdb-change-record record nil)
	  (bbdb-redisplay-one-record record)))))

(defun bbdb-delete-current-record (r &optional noprompt)
  "Delete the entire bbdb database entry which the cursor is within."
  (interactive (list (bbdb-current-record t)))
  (if (or noprompt
	  (bbdb-y-or-n-p (format "delete the entire db entry of %s? "
				 (or (bbdb-record-name r)
				     (bbdb-record-company r)
				     (car (bbdb-record-net r))))))
      (let* ((record-cons (assq r bbdb-records))
	     (next-record-cons (car (cdr (memq record-cons bbdb-records)))))
	(bbdb-debug (if (bbdb-record-deleted-p r)
			(error "deleting deleted record")))
	(bbdb-record-set-deleted-p r t)
	(bbdb-delete-record-internal r)
	(if (eq record-cons (car bbdb-records))
	    (setq bbdb-records (cdr bbdb-records))
	    (let ((rest bbdb-records))
	      (while (cdr rest)
		(if (eq record-cons (car (cdr rest)))
		    (progn
		      (setcdr rest (cdr (cdr rest)))
		      (setq rest nil)))
		(setq rest (cdr rest)))))
	(bbdb-redisplay-one-record r record-cons next-record-cons t)
	(bbdb-with-db-buffer
	  (setq bbdb-changed-records (delq r bbdb-changed-records)))
	;;(bbdb-offer-save)
	)))

(defun bbdb-elide-record (arg)
  "Toggle whether the current record is displayed expanded or elided
\(multi-line or one-line display.\)  With a numeric argument of 0, the 
current record will unconditionally be made elided; with any other argument,
the current record will unconditionally be shown expanded.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-elide-record]\" is \
used instead of simply \"\\[bbdb-elide-record]\", then the state of all \
records will
be changed instead of just the one at point.  In this case, an argument 
of 0 means that all records will unconditionally be made elided; any other
numeric argument means that all of the records will unconditionally be shown
expanded; and no numeric argument means that the records are made to be in
the opposite state of the record under point."
  (interactive "P")
  (if (bbdb-do-all-records-p)
      (bbdb-elide-all-records-internal arg)
    (bbdb-elide-record-internal arg)))


(defun bbdb-elide-record-internal (arg)
  (let* ((record (bbdb-current-record))
	 (cons (assq record bbdb-records))
	 (current-state (nth 1 cons))
	 (desired-state
	  (cond ((null arg) (not current-state))
		((eq arg 0) nil)
		(t t))))
    (if (eq current-state desired-state)
	nil
      (setcar (cdr cons) desired-state)
      (bbdb-redisplay-one-record record))))

(defun bbdb-elide-all-records-internal (arg)
  (let* ((record (bbdb-current-record))
	 (cons (assq record bbdb-records))
	 (current-state (nth 1 cons))
	 (desired-state
	  (cond ((null arg) (not current-state))
		((eq arg 0) nil)
		(t t)))
	 (records bbdb-records)
	 (any-change-p nil))
    (while records
      (if (eq desired-state (nth 1 (car records)))
	  nil
	(setq any-change-p t)
	(setcar (cdr (car records)) desired-state))
      (setq records (cdr records)))
    (if (not any-change-p)
	nil
      (bbdb-redisplay-records)
      (set-buffer bbdb-buffer-name)
      (goto-char (nth 2 (assq record bbdb-records)))
      (recenter '(4)))))

(defun bbdb-omit-record (n)
  "Remove the current record from the display without deleting it from the
database.  With a prefix argument, omit the next N records.  If negative, 
omit backwards."
  (interactive "p")
  (while (not (= n 0))
    (if (< n 0) (bbdb-prev-record 1))
    (let* ((record (or (bbdb-current-record) (error "no records")))
	   (rest bbdb-records)
	   cons next prev-tail)
      (while rest
	(if (eq (car (car rest)) record)
	    (setq cons (car rest)
		  next (car (cdr rest))
		  rest nil)
	  (setq prev-tail rest
		rest (cdr rest))))
      (or record (error "can't find current record"))
      (let ((buffer-read-only nil))
	(delete-region (nth 2 cons) (if next (nth 2 next) (point-max))))
      (if prev-tail
	  (setcdr prev-tail (cdr (cdr prev-tail)))
	(setq bbdb-records (cdr bbdb-records)))
      (setq n (if (> n 0) (1- n) (1+ n)))))
  (bbdb-frob-mode-line (length bbdb-records)))

;;; Fixing up bogus entries

(defun bbdb-refile-record (old-record new-record)
  "Merge the current record into some other record; that is, delete the
record under point after copying all of the data within it into some other
record.  this is useful if you realize that somehow a redundant record has
gotten into the database, and you want to merge it with another.

If both records have names and/or companies, you are asked which to use.
Phone numbers, addresses, and network addresses are simply concatenated.
The first record is the record under the point; the second is prompted for.
Completion behaviour is as dictated by the variable `bbdb-completion-type'."
  (interactive
    (let ((r (bbdb-current-record)))
      (list r
        (bbdb-completing-read-record
	  (format "merge record \"%s\" into: "
		  (or (bbdb-record-name r) (car (bbdb-record-net r))
		      "???"))))))
  (if (or (null new-record) (eq old-record new-record))
      (error "those are the same"))
  (let*(extra-name
	(name
	 (cond ((and (/= 0 (length (bbdb-record-name old-record)))
		     (/= 0 (length (bbdb-record-name new-record))))
		(prog1
		    (if (bbdb-y-or-n-p
			 (format "Use name \"%s\" instead of \"%s\"? "
				 (bbdb-record-name old-record)
				 (bbdb-record-name new-record)))
			(progn
			  (setq extra-name new-record)
			  (cons (bbdb-record-firstname old-record)
				(bbdb-record-lastname old-record)))
			(setq extra-name old-record)
			(cons (bbdb-record-firstname new-record)
			      (bbdb-record-lastname new-record)))
		  (or (and bbdb-use-alternate-names
			   (bbdb-y-or-n-p
			     (format "Keep \"%s\" as an alternate name? "
				     (bbdb-record-name extra-name))))
		      (setq extra-name nil))
		  ))
	       ((= 0 (length (bbdb-record-name old-record)))
		(cons (bbdb-record-firstname new-record)
		      (bbdb-record-lastname new-record)))
	       (t (cons (bbdb-record-firstname old-record)
			(bbdb-record-lastname old-record)))))
	(comp
	 (cond ((and (/= 0 (length (bbdb-record-company old-record)))
		     (/= 0 (length (bbdb-record-company new-record))))
		(if (bbdb-y-or-n-p (format
				    "Use company \"%s\" instead of \"%s\"? "
				      (bbdb-record-company old-record)
				      (bbdb-record-company new-record)))
		    (bbdb-record-company old-record)
		    (bbdb-record-company new-record)))
	       ((= 0 (length (bbdb-record-company old-record)))
		(bbdb-record-company new-record))
	       (t (bbdb-record-company old-record))))
	(old-nets (bbdb-record-net old-record))
	(old-aka (bbdb-record-aka old-record))
	)
    (if extra-name
	(setq old-aka (cons (bbdb-record-name extra-name) old-aka)))
    (bbdb-record-set-phones new-record
      (nconc (bbdb-record-phones new-record)
	     (bbdb-record-phones old-record)))
    (bbdb-record-set-addresses new-record
      (nconc (bbdb-record-addresses new-record)
	     (bbdb-record-addresses old-record)))
    (bbdb-record-set-company new-record comp)
    (let ((n1 (bbdb-record-raw-notes new-record))
	  (n2 (bbdb-record-raw-notes old-record))
	  tmp)
      (or (equal n1 n2)
	  (progn
	    (or (listp n1) (setq n1 (list (cons 'notes n1))))
	    (or (listp n2) (setq n2 (list (cons 'notes n2))))
	    (while n2
	      (if (setq tmp (assq (car (car n2)) n1))
		  (setcdr tmp (concat (cdr tmp) "\n" (cdr (car n2))))
		(setq n1 (nconc n1 (list (car n2)))))
	      (setq n2 (cdr n2)))
	    (bbdb-record-set-raw-notes new-record n1))))
    (bbdb-delete-current-record old-record 'noprompt)
    (bbdb-record-set-net new-record
      (nconc (bbdb-record-net new-record) old-nets))
    (bbdb-record-set-firstname new-record (car name))
    (bbdb-record-set-lastname new-record (cdr name))
    (bbdb-record-set-namecache new-record nil)
    (bbdb-record-set-aka new-record
      (nconc (bbdb-record-aka new-record) old-aka))
    (bbdb-change-record new-record t) ; don't always need-to-sort...
    (let ((bbdb-elided-display nil))
      (if (assq new-record bbdb-records)
	  (bbdb-redisplay-one-record new-record))
      (bbdb-with-db-buffer
	(if (not (memq new-record bbdb-changed-records))
	    (setq bbdb-changed-records
		  (cons new-record bbdb-changed-records))))
      (if (null bbdb-records)  ; nothing displayed, display something.
	  (bbdb-display-records (list new-record)))))
  (message "records merged."))


;;; Send-Mail interface

(defun bbdb-dwim-net-address (record &optional net)
  "Returns a string to use as the email address of the given record.  The
given address is the address the mail is destined to; this is formatted like
\"Firstname Lastname <addr>\" unless both the first name and last name are
constituents of the address, as in John.Doe@SomeHost, or the address is
already in the form \"Name <foo>\" or \"foo (Name)\", in which case the
address is used as-is."
  (or net (setq net (car (bbdb-record-net record))))
  (or net (error "record unhas network addresses"))
  (let* ((override (bbdb-record-getprop record 'mail-name))
	 (name (or override (bbdb-record-name record)))
	 fn ln (i 0))
    (if override
	(let ((both (bbdb-divide-name override)))
	  (setq fn (car both)
		ln (car (cdr both)))
	  (if (equal fn "") (setq fn nil))
	  (if (equal ln "") (setq ln nil)))
      (setq fn (bbdb-record-firstname record)
	    ln (bbdb-record-lastname record)))
    ;; if the name contains backslashes or double-quotes, backslash them.
    (if name
	(while (setq i (string-match "[\\\"]" name i))
	  (setq name (concat (substring name 0 i) "\\" (substring name i))
		i (+ i 2))))
    (cond ((or (null name)
	       (cond ((and fn ln)
		      (or (string-match
			   (concat "\\`[^!@%]*\\b" (regexp-quote fn)
				   "\\b[^!%@]+\\b" (regexp-quote ln) "\\b")
			   net)
			  (string-match
			   (concat "\\`[^!@%]*\\b" (regexp-quote ln)
				   "\\b[^!%@]+\\b" (regexp-quote fn) "\\b")
			   net)))
		     ((or fn ln)
		      (string-match
		       (concat "\\`[^!@%]*\\b" (regexp-quote (or fn ln)) "\\b")
		       net)))
	       ;; already in "foo <bar>" or "bar <foo>" format.
	       (string-match "\\`[ \t]*[^<]+[ \t]*<" net)
	       (string-match "\\`[ \t]*[^(]+[ \t]*(" net))
	   net)
	  ;; if the name contains control chars or RFC822 specials, it needs
	  ;; to be enclosed in quotes.  Double-quotes and backslashes have
	  ;; already been escaped.  This quotes a few extra characters as
	  ;; well (!,%, and $) just for common sense.
	  ((string-match "[][\000-\037\177()<>@,;:.!$%]" name)
	   (format "\"%s\" <%s>" name net))
	  (t
	   (format "%s <%s>" name net)))))


(defun bbdb-send-mail-internal (&optional to subj records)
  (let ((type (or bbdb-send-mail-style
		  (cond ((featurep 'mh-e) 'mh)
			((featurep 'vm) 'vm)
			(t 'mail)))))
    (cond
     ((eq type 'mh)
      (or (fboundp 'mh-send) (autoload 'mh-send "mh-e"))
      (mh-send to "" (or subj "")))
     ((eq type 'vm)
      (cond ((not (fboundp 'vm-mail-internal))
	     (load-library "vm") ; 5.32 or later
	     (or (fboundp 'vm-mail-internal)
		 (load-library "vm-reply")))) ; 5.31 or earlier
      (vm-mail-internal
        (and records (format "mail to %s%s" (bbdb-record-name (car records))
			     (if (cdr records) ", ..." "")))
	to subj))
     ((or (eq type 'mail) (eq type 'rmail))
      (mail nil to subj))
     (t
      (error "bbdb-send-mail-style must be vm, mh, or rmail")))))
		   

(defun bbdb-send-mail (bbdb-record &optional subject)
  "Compose a mail message to the person indicated by the current bbdb record.
The first (most-recently-added) address is used if there are more than one.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-send-mail]\" is \
used instead of simply \"\\[bbdb-send-mail]\", then mail will be sent to \
all of the
folks listed in the *BBDB* buffer instead of just the person at point."
  (interactive (list (if (bbdb-do-all-records-p)
			 (mapcar 'car bbdb-records)
		       (bbdb-current-record))))
  (if (consp bbdb-record)
      (bbdb-send-mail-many bbdb-record subject)
    (bbdb-send-mail-1 bbdb-record subject)))


(defun bbdb-send-mail-1 (bbdb-record &optional subject)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw-to-execute
	(list 'bbdb-send-mail bbdb-record subject)))
  ;; else...

  (cond ((null bbdb-record) (error "record unexists"))
	((null (bbdb-record-net bbdb-record))
	 (error "Current record unhas a network addresses."))
	(t (bbdb-send-mail-internal (bbdb-dwim-net-address bbdb-record)
				    subject (list bbdb-record))
	   (if (re-search-backward "^Subject: $" nil t) (end-of-line)))))


(defun bbdb-send-mail-many (records &optional subject)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw-to-execute
	(list 'bbdb-send-mail (list 'quote records) subject)))
  ;; else...

  (let ((good '()) (bad '())
	(orec records))
    (while records
      (if (bbdb-record-net (car records))
	  (setq good (cons (car records) good))
	  (setq bad (cons (car records) bad)))
      (setq records (cdr records)))
    (bbdb-send-mail-internal
      (mapconcat (function (lambda (x) (bbdb-dwim-net-address x)))
		 (nreverse good) ",\n    ")
      subject orec)
    (if (not bad) nil
      (goto-char (point-max))
      (let ((p (point))
	    (fill-prefix "    ")
	    (fill-column 70))
	(insert "*** Warning: No net addresses for "
		(mapconcat (function (lambda (x) (bbdb-record-name x)))
			   (nreverse bad) ", ") ".")
	(fill-region-as-paragraph p (point))
	(goto-char p))))
  (if (re-search-backward "^Subject: $" nil t) (end-of-line)))


(defun bbdb-yank-addresses ()
  "CC the people displayed in the *BBDB* buffer on this message.
The primary net-address of each of the records currently listed in the
*BBDB* buffer (whether it is visible or not) will be appended to the 
CC: field of the current buffer (assuming the current buffer is a mail
composition buffer.)"
  (interactive)
  (let ((addrs (save-excursion
		 (set-buffer bbdb-buffer-name)
		 (delq nil
		       (mapcar (function (lambda (x)
					   (if (bbdb-record-net (car x))
					       (bbdb-dwim-net-address (car x))
					     nil)))
			       bbdb-records)))))
    (goto-char (point-min))
    ;; If there's a CC field, move to the end of it, inserting a comma if 
    ;;  there are already addresses present.
    ;; Otherwise, if there's an empty To: field, move to the end of it.
    ;; Otherwise, insert an empty CC: field.
    (if (re-search-forward "^CC:[ \t]*" nil t)
	(if (eolp)
	    nil
	  (end-of-line)
	  (while (looking-at "\n[ \t]")
	    (forward-char) (end-of-line))
	  (insert ",\n")
	  (indent-relative))
      (re-search-forward "^To:[ \t]*")
      (if (eolp)
	  nil
	(end-of-line)
	(while (looking-at "\n[ \t]")
	  (forward-char) (end-of-line))
	(insert "\nCC:")
	(indent-relative)))
    ;; Now insert each of the addresses on its own line.
    (while addrs
      (insert (car addrs))
      (if (cdr addrs) (progn (insert ",\n") (indent-relative)))
      (setq addrs (cdr addrs)))))

(defun bbdb-show-all-recipients ()
  "*Display BBDB records for all recipients of the message in this buffer."
  (interactive)
  (let ((marker (bbdb-header-start))
	addrs)
    (message "Searching...")
    (save-excursion
      (set-buffer (marker-buffer marker))
      (goto-char marker)
      (setq addrs
	    (append
	     (save-excursion
	       (bbdb-split (or (bbdb-extract-field-value "from") "") ","))
	     (save-excursion
	       (bbdb-split (or (bbdb-extract-field-value "to") "") ","))
	     (save-excursion
	       (bbdb-split (or (bbdb-extract-field-value "cc") "") ","))
	     (save-excursion
	       (bbdb-split (or (bbdb-extract-field-value "bcc") "") ",")))))
    (let ((rest addrs)
	  (records '())
	  record)
      (while rest
	(setq record (bbdb-annotate-message-sender (car rest) t t t))
	(if record (setq records (cons record records)))
	(setq rest (cdr rest)))
      (message "Sorting...")
      (setq records (sort records '(lambda (x y) (bbdb-record-lessp x y))))
      (bbdb-display-records records))))


;;; completion

(defun bbdb-completion-predicate (symbol)
  "For use as the third argument to completing-read, to obey the
semantics of bbdb-completion-type."
  (let (name r n)
    (and (boundp symbol)
	 (setq name (symbol-name symbol)
	       r (symbol-value symbol))
	 (or (null bbdb-completion-type)
	     (and (memq bbdb-completion-type
			'(name primary-or-name name-or-primary))
		  (setq n (or (bbdb-record-name r)
			      (bbdb-record-company r)))
		  (string= name (downcase n)))
	     ;; #### do something about AKA or mail-name or mail-alias here?
	     (and (setq n (bbdb-record-net r))
		  (or (and (memq bbdb-completion-type
				 '(primary primary-or-name name-or-primary))
			   (string= name (downcase (car n))))
		      (and (eq bbdb-completion-type 'net)
			   (let ((done nil))
			     (while (and n (not done))
			       (if (string= name (downcase (car n)))
				   (setq done t))
			       (setq n (cdr n)))
			     done))))))))

(defun bbdb-completing-read-record (prompt)
  "Prompt for and return a record from the bbdb; completion is done according
to bbdb-completion-type.  If the user just hits return, nil is returned.
Otherwise, a valid response is forced."
  (let* ((ht (bbdb-hashtable))
	 (string (completing-read prompt ht 'bbdb-completion-predicate t))
	 (symbol (and (not (= 0 (length string)))
		      (intern-soft string ht))))
    (if symbol
	(if (and (boundp symbol) (symbol-value symbol))
	    (symbol-value symbol)
	    (error "selecting deleted (unhashed) record \"%s\"!" symbol))
	nil)))


(defvar bbdb-read-addresses-with-completion-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " 'self-insert-command)
    (define-key map "\t" 'bbdb-complete-name)
    (define-key map "\M-\t" 'bbdb-complete-name)
    map))

(defun bbdb-read-addresses-with-completion (prompt &optional default)
  "Like read-string, but allows bbdb-complete-name style completion."
    (read-from-minibuffer prompt default
			  bbdb-read-addresses-with-completion-map))


(defvar bbdb-complete-name-saved-window-config nil)

(defun bbdb-complete-name-cleanup ()
  (if bbdb-complete-name-saved-window-config
      (progn
	(if (get-buffer-window "*Completions*")
	    (set-window-configuration
	      bbdb-complete-name-saved-window-config))
	(setq bbdb-complete-name-saved-window-config nil))))

(defun bbdb-complete-name (&optional start-pos)
  "Complete the user full-name or net-address before point (up to the 
preceeding newline, colon, or comma).  If what has been typed is unique,
insert an entry of the form \"User Name <net-addr>\".  If it is a valid
completion but not unique, a list of completions is displayed.  

Completion behaviour can be controlled with 'bbdb-completion-type'."
  (interactive)
  (let* ((end (point))
	 (beg (or start-pos
		  (save-excursion
		    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		    (goto-char (match-end 0))
		    (point))))
	 (pattern (downcase (buffer-substring beg end)))
	 (ht (bbdb-hashtable))
	 ;; If we have two completions which expand to the same record, only
	 ;; treat one as a completion.  For example, if the user asked for
	 ;; completion on "foo" and there was a record of "Foo Bar <foo@baz>",
	 ;; pretend the first completion ("Foo Bar") is valid and the second
	 ;; ("foo@baz") is not, since they're actually the *same* completion
	 ;; even though they're textually different.
	 (yeah-yeah-this-one nil)
	 (only-one-p t)
	 (all-the-completions nil)
	 (pred (function (lambda (sym)
		 (and (bbdb-completion-predicate sym)
		      (let* ((rec (symbol-value sym))
			     (net (bbdb-record-net rec)))
			(if (and yeah-yeah-this-one
				 (not (eq rec yeah-yeah-this-one)))
			    (setq only-one-p nil))
			(setq all-the-completions
			      (cons sym all-the-completions))
			(if (eq rec yeah-yeah-this-one)
			    nil
			  (and net (setq yeah-yeah-this-one rec))
			  net))))))
	 (completion (try-completion pattern ht pred)))
    ;; If there were multiple completions for this record, the one that was
    ;; picked is random (hash order.)  So canonicalize that to be the one
    ;; closest to the front of the list.
    (if (and (stringp completion)
	     yeah-yeah-this-one
	     only-one-p)
	(let ((addrs (bbdb-record-net yeah-yeah-this-one))
	      (rest all-the-completions))
	  (while rest
	    (if (member (symbol-name (car rest)) addrs)
		(setq completion (symbol-name (car rest))
		      rest nil))
	    (setq rest (cdr rest)))))
    (setq yeah-yeah-this-one nil
	  all-the-completions nil)
    (cond ((eq completion t)
	   (let* ((sym (intern-soft pattern ht))
		  (val (symbol-value sym)))
	     (delete-region beg end)
	     (insert (bbdb-dwim-net-address val
		       (if (string= (symbol-name sym)
				    (downcase (or (bbdb-record-name val) "")))
			   nil
			 ;; get the case right
			 (let ((nets (bbdb-record-net val))
			       (want (symbol-name sym))
			       (the-one nil))
			   (while (and nets (not the-one))
			     (if (string= want (downcase (car nets)))
				 (setq the-one (car nets))
				 (setq nets (cdr nets))))
			   the-one))))
	     ;;
	     ;; if we're past fill-column, wrap at the previous comma.
	     (if (and
		  (if (boundp 'auto-fill-function) ; the emacs19 name.
		      auto-fill-function
		    auto-fill-hook)
		  (>= (current-column) fill-column))
		 (let ((p (point))
		       bol)
		   (save-excursion
		     (beginning-of-line)
		     (setq bol (point))
		     (goto-char p)
		     (if (search-backward "," bol t)
			 (progn
			   (forward-char 1)
			   (insert "\n   "))))))
	     ;;
	     ;; Update the *BBDB* buffer if desired.
	     (if bbdb-completion-display-record
		 (let ((bbdb-gag-messages t))
		   (bbdb-display-records-1 (list val) t)))
	     (bbdb-complete-name-cleanup)
	     ))
	  ((null completion)
	   (bbdb-complete-name-cleanup)
	   (message "completion for \"%s\" unfound." pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion)
	   (setq end (point))
	   (let ((last ""))
	     (while (and (stringp completion)
			 (not (string= completion last))
			 (setq last completion
			       pattern (downcase (buffer-substring beg end))
			       completion (try-completion pattern ht pred)))
	       (if (stringp completion)
		   (progn (delete-region beg end)
			  (insert completion))))
	     (bbdb-complete-name beg)
	     ))
	  (t
	   (or (eq (selected-window) (minibuffer-window))
	       (message "Making completion list..."))
	   (let* ((list (all-completions pattern ht pred))
;;		  (recs (delq nil (mapcar (function (lambda (x)
;;					    (symbol-value (intern-soft x ht))))
;;					  list)))
		  )
	     (if (and (not (eq bbdb-completion-type 'net))
		      (= 2 (length list))
		      (eq (symbol-value (intern (car list) ht))
			  (symbol-value (intern (nth 1 list) ht)))
		      (not (string= completion (car list))))
		 (progn
		   (delete-region beg end)
		   (insert (car list))
		   (message " ")
		   (bbdb-complete-name beg))
	       (if (not (get-buffer-window "*Completions*"))
		   (setq bbdb-complete-name-saved-window-config
			 (current-window-configuration)))
	       (with-output-to-temp-buffer "*Completions*"
		 (display-completion-list list))
	       (or (eq (selected-window) (minibuffer-window))
		   (message "Making completion list...done"))))))))

(defun bbdb-yank ()
  "Insert the current contents of the *BBDB* buffer at point."
  (interactive)
  (insert (let ((b (current-buffer)))
	   (set-buffer bbdb-buffer-name)
	   (prog1 (buffer-string) (set-buffer b)))))


;;; interface to mail-abbrevs.el.

(defvar bbdb-define-all-aliases-field 'mail-alias
  "*The field which bbdb-define-all-aliases searches for.")

(defun bbdb-define-all-aliases ()
  "Define mail aliases for some of the records in the database.
Every record which has a `mail-alias' field will have a mail alias
defined for it which is the contents of that field.  If there are 
multiple comma-separated words in the `mail-alias' field, then all
of those words will be defined as aliases for that person.

If multiple entries in the database have the same mail alias, then 
that alias expands to a comma-separated list of the network addresses
of all of those people."
  (let* ((target (cons bbdb-define-all-aliases-field "."))
	 (records (bbdb-search (bbdb-records) nil nil nil target))
	 result record aliases match)
    (while records
      (setq record (car records))
      (setq aliases (bbdb-split 
		     (bbdb-record-getprop record bbdb-define-all-aliases-field)
		     ","))
      (while aliases
	(if (setq match (assoc (car aliases) result))
	    (nconc match (cons record nil))
	  (setq result (cons (list (car aliases) record) result)))
	(setq aliases (cdr aliases)))
      (setq records (cdr records)))
    (while result
      (let ((alias (downcase (car (car result))))
	    (expansion (mapconcat 'bbdb-dwim-net-address (cdr (car result))
				  (if (boundp 'mail-alias-separator-string)
				      mail-alias-separator-string
				    ", "))))
	(if (fboundp 'define-mail-abbrev)
	    (define-mail-abbrev alias expansion)
	  (define-mail-alias alias expansion))
	(setq alias (or (intern-soft alias
			 (if (boundp 'mail-abbrevs) mail-abbrevs mail-aliases))
 			(error "couldn't find the alias we just defined!")))
	(or (eq (symbol-function alias) 'mail-abbrev-expand-hook)
	    (error "mail-aliases contains unexpected hook %s"
		   (symbol-function alias)))
	;; The abbrev-hook is called with network addresses instead of bbdb
	;; records to avoid keeping pointers to records, which would lose if
	;; the database was reverted.  It uses -search-simple to convert
	;; these to records, which is plenty fast.
	(fset alias (list 'lambda '()
			  (list 'bbdb-mail-abbrev-expand-hook
				(list 'quote
				      (mapcar
				       (function
					(lambda (x) (car (bbdb-record-net x))))
				       (cdr (car result))))))))
      (setq result (cdr result)))))

(defun bbdb-mail-abbrev-expand-hook (records)
  (mail-abbrev-expand-hook)
  (if bbdb-completion-display-record
      (let ((bbdb-gag-messages t))
	(bbdb-display-records-1
	 (mapcar (function (lambda (x) (bbdb-search-simple nil x))) records)
	 t))))

;;; Sound

(defvar bbdb-dial-local-prefix nil
  "*If this is non-nil, it should be a string of digits which your phone
system requires before making local calls (for example, if your phone system
requires you to dial 9 before making outside calls.)")

(defvar bbdb-dial-long-distance-prefix nil
  "*If this is non-nil, it should be a string of digits which your phone
system requires before making a long distance call (one not in your local
area code).  For example, in some areas you must dial 1 before an area code.")


(defvar bbdb-sound-player "/usr/demo/SOUND/play")
(defvar bbdb-sound-files
  '["/usr/demo/SOUND/sounds/touchtone.0.au"
    "/usr/demo/SOUND/sounds/touchtone.1.au"
    "/usr/demo/SOUND/sounds/touchtone.2.au"
    "/usr/demo/SOUND/sounds/touchtone.3.au"
    "/usr/demo/SOUND/sounds/touchtone.4.au"
    "/usr/demo/SOUND/sounds/touchtone.5.au"
    "/usr/demo/SOUND/sounds/touchtone.6.au"
    "/usr/demo/SOUND/sounds/touchtone.7.au"
    "/usr/demo/SOUND/sounds/touchtone.8.au"
    "/usr/demo/SOUND/sounds/touchtone.9.au"])

(defun bbdb-dial (phone force-area-code)
  "On a Sun SparcStation, play the appropriate tones on the builtin 
speaker to dial the phone number corresponding to the current line.
If the point is at the beginning of a record, dial the first phone
number.  Does not dial the extension.  Does not dial the area code if
it is the same as `bbdb-default-area-code' unless a prefix arg is given."
  (interactive (list (bbdb-current-field)
		     current-prefix-arg))
  (if (eq (car-safe phone) 'name)
      (setq phone (car (bbdb-record-phones (car (cdr phone))))))
  (if (eq (car-safe phone) 'phone)
      (setq phone (car (cdr phone))))
  (or (vectorp phone) (error "not on a phone field"))
  (or window-system (error "You're not under window system."))
  (or (file-exists-p bbdb-sound-player)
      (error "no sound player program"))
  (let* ((str (bbdb-phone-string phone))
	 L (i 0))
    (or force-area-code
	(if (string-match (format "^(%03d)" bbdb-default-area-code) str)
	    (setq str (substring str (match-end 0)))))
    (if (string-match "x[0-9]+$" str)
	(setq str (substring str 0 (match-beginning 0))))
    (if bbdb-dial-local-prefix
	(let ((d (append bbdb-dial-local-prefix nil)))
	  (or (string-match "\\`[0-9]*\\'" bbdb-dial-local-prefix)
	      (error "bbdb-dial-local-prefix contains non-digits"))
	  (while d
	    (call-process bbdb-sound-player nil nil nil
			  (aref bbdb-sound-files (- (car d) ?0)))
	    (sleep-for 1)
	    (setq d (cdr d)))))
    (if (and bbdb-dial-long-distance-prefix
	     (string-match "^([0-9][0-9][0-9])" str))
	(let ((d (append bbdb-dial-long-distance-prefix nil)))
	  (or (string-match "\\`[0-9]*\\'" bbdb-dial-long-distance-prefix)
	      (error "bbdb-dial-long-distance-prefix contains non-digits"))
	  (while d
	    (call-process bbdb-sound-player nil nil nil
			  (aref bbdb-sound-files (- (car d) ?0)))
	    (sleep-for 1)
	    (setq d (cdr d)))))
    (setq L (length str))
    (while (< i L)
      (if (and (<= ?0 (aref str i))
	       (>= ?9 (aref str i)))
	  (call-process bbdb-sound-player nil nil nil
			(aref bbdb-sound-files (- (aref str i) ?0)))
	  (sit-for 0))
      (setq i (1+ i)))))


;;; Finger, based on code by Sam Cramer <cramer@sun.com>.
;;; Note that process-death bugs in 18.57 may make this eat up all the cpu...

(defvar bbdb-finger-buffer-name "*finger*")

(defun bbdb-finger-internal (address)
  (message "Fingering %s..." address)
  (condition-case condition
      (let* ((@ (string-match "@" address))
	     (stream (open-network-stream
		      "finger" bbdb-finger-buffer-name
		      (if @ (substring address (1+ @)) "localhost")
		      "finger")))
	(set-process-sentinel stream 'bbdb-finger-process-sentinel)
	(princ (concat "finger " address "\n"))
	(process-send-string stream
	  (concat ;;"/W " ; cs.stanford.edu doesn't like this...
		  (if @ (substring address 0 @) address) "\n"))
	(process-send-eof stream))
    (error
     (princ (format "error fingering %s: %s\n" address
		    (if (stringp condition) condition
		      (concat "\n" (nth 1 condition)
			      (if (cdr (cdr condition)) ": ")
			      (mapconcat '(lambda (x)
					    (if (stringp x) x
					      (prin1-to-string x)))
					 (cdr (cdr condition)) ", ")))))
     (bbdb-finger-process-sentinel nil nil) ; hackaroonie
     )))

(defvar bbdb-remaining-addrs-to-finger)
(defun bbdb-finger-process-sentinel (process s)
  (save-excursion
    (set-buffer bbdb-finger-buffer-name)
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (delete-char -1))
    (if (and (boundp 'bbdb-remaining-addrs-to-finger)
	     bbdb-remaining-addrs-to-finger)
	(let ((addr (car bbdb-remaining-addrs-to-finger)))
	  (setq bbdb-remaining-addrs-to-finger
		(cdr bbdb-remaining-addrs-to-finger))
	  (goto-char (point-max))
	  (let ((standard-output (current-buffer)))
	    (princ "\n\n\^L\n")
	    (bbdb-finger-internal addr)))
      (goto-char (point-max))
      (message "Finger done."))))


(defun bbdb-finger (record &optional which-address)
  "Finger the network address of a BBDB record. 
If this command is executed from the *BBDB* buffer, finger the network
address of the record at point; otherwise, it prompts for a user.
With a numeric prefix argument, finger the Nth network address of the 
current record\; with a prefix argument of ^U, finger all of them.
The *finger* buffer is filled asynchronously, meaning that you don't
have to wait around for it to finish\; but fingering another user before
the first finger has finished could have unpredictable results.
\\<bbdb-mode-map>
If this command is executed from the *BBDB* buffer, it may be prefixed
with \"\\[bbdb-apply-next-command-to-all-records]\" \(as in \
\"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-finger]\" instead of \
simply \"\\[bbdb-finger]\"\), meaning to finger all of 
the users currently listed in the *BBDB* buffer instead of just the one
at point.  The numeric prefix argument has the same interpretation."
  (interactive (list (if (string= bbdb-buffer-name (buffer-name))
			 (if (bbdb-do-all-records-p)
			     (mapcar 'car bbdb-records)
			   (bbdb-current-record))
		       (let (r (p "BBDB Finger: "))
			 (while (not r)
			   (setq r (bbdb-completing-read-record p))
			   (if (not r) (ding))
			   (setq p "Not in the BBDB!  Finger: "))
			 r))
		     current-prefix-arg))
  (if (not (consp record)) (setq record (list record)))
  (let ((addrs nil))
    (while record
      (cond ((null which-address)
	     (setq addrs
		   (nconc addrs
			  (list (car (bbdb-record-net (car record)))))))
	    ((stringp which-address)
	     (setq addrs (nconc addrs (list which-address))))
	    ((numberp which-address)
	     (setq addrs
		   (nconc addrs
			  (list (nth which-address
				     (bbdb-record-net (car record)))))))
	    (t
	     (setq addrs
		   (nconc addrs
			  (copy-sequence (bbdb-record-net (car record)))))))
      (setq record (cdr record)))
    (save-excursion
      (with-output-to-temp-buffer bbdb-finger-buffer-name
	(set-buffer bbdb-finger-buffer-name)
	(make-local-variable 'bbdb-remaining-addrs-to-finger)
	(setq bbdb-remaining-addrs-to-finger (cdr addrs))
	(bbdb-finger-internal (car addrs))))))


;;; Help and documentation

(defvar bbdb-info-file nil
  "*Set this to the location of the bbdb info file, if it's not in the
standard place.")

(defvar Info-directory) ; v18
(defun bbdb-info ()
  (interactive)
  (require 'info)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw-to-execute '(bbdb-info))
    (let ((file bbdb-info-file)
	  (Info-directory (and (boundp 'Info-directory) Info-directory)))
      (if file
	  (setq file (expand-file-name file Info-directory))
	(setq file (expand-file-name "bbdb" Info-directory))
	(or (file-exists-p file)
	    (setq file (concat file ".info"))))
      (or (file-exists-p file) (error "Info file %s doesn't exist" file))
      (let ((Info-directory (file-name-directory file)))
	(Info-goto-node (format "(%s)Top" file))))))

(defun bbdb-help ()
  (interactive)
  (message (substitute-command-keys "\\<bbdb-mode-map>\
new field: \\[bbdb-insert-new-field]  \
edit field: \\[bbdb-edit-current-field]  \
delete field: \\[advertized-bbdb-delete-current-field-or-record]  \
mode help: \\[describe-mode]  \
info: \\[bbdb-info]")))


(or (fboundp 'member)	;; v18 lossage
    (defun member (item list)
      (while (and list (not (equal item (car list)))) (setq list (cdr list)))
      list))


;;; If Sebastian Kremer's minibuffer history package is around, use it.
(if (and (fboundp 'gmhist-make-magic)
	 (string-lessp emacs-version "19")) ; v19 has history built in
    (mapcar 'gmhist-make-magic
	    '(bbdb bbdb-name bbdb-company bbdb-net bbdb-changed)))

(provide 'bbdb-com)
