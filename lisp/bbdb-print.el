;;; bbdb-print.el -- for printing BBDB databases using TeX.

;;; Authors: Boris Goldowsky <boris@cs.rochester.edu>
;;;          Dirk Grunwald <grunwald@cs.colorado.edu>
;;;          Luigi Semenzato <luigi@paris.cs.berkeley.edu>
;;; Copyright (C) 1993 Boris Goldowsky
;;; Version: 3.0; 21Mar94

;;; This file is part of the bbdb-print extensions to the Insidious
;;; Big Brother Database, which is for use with GNU Emacs. 
;;;
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

;;; Commentary:
;;;
;;; In the *BBDB* buffer, type P to convert the listing to TeX
;;; format. It will prompt you for a filename.  Then run TeX on that
;;; file and print it out.
;;;
;;; Bbdb-print understands one new bbdb field: tex-name.  If it
;;; exists, this will be used for the printed listing instead of the
;;; name field of that record.  This is designed for entering names
;;; with lots of accents that would mess up mailers, or when for any
;;; reason you want the printed version of the name to be different
;;; from the version that appears on outgoing mail and in the *BBDB*
;;; buffer.  You may want to add tex-name to bbdb-elided-display so
;;; you only see it in the printout.  tex-name is exempted from the
;;; usual special-character quoting done by bbdb-print; it is used
;;; verbatim.
;;;
;;; Not all fields or records need be printed.  To not print a certain
;;; field, add it to `bbdb-print-elide' (which see).  If after eliding
;;; fields a record contains no interesting information, it will not
;;; be printed at all; the variable `bbdb-print-require' determines
;;; what is meant by "interesting" information.  You can also restrict
;;; printing to just the records currently in the *BBDB* buffer by
;;; using *P instead of P.
;;;       
;;; There are various options for the way the formatting is done; most
;;; are controlled by the variable bbdb-print-alist. See its
;;; documentation for the allowed options.

;;; Installation:
;;;
;;; Put this file somewhere on your load-path.  Put bbdb-print.tex and
;;; multicol.tex somewhere on your TEXINPUTS path, or put absolute
;;; pathnames into the variable bbdb-print-format-files (which see). Put 
;;; (add-hook 'bbdb-load-hook (function (lambda () (require 'bbdb-print))))
;;; into your .emacs, or autoload it.
;;;
;;; This program was adapted for BBDB by Boris Goldowsky
;;; <boris@cs.rochester.edu> and Dirk Grunwald 
;;; <grunwald@cs.colorado.edu> using a TeX format designed by Luigi
;;; Semenzato <luigi@paris.cs.berkeley.edu>. 
;;; We are also grateful to numerous people on the info-bbdb
;;; mailing list for suggestions and bug reports. 

;;; Code:

(require 'bbdb)
(require 'bbdb-com)

(define-key bbdb-mode-map "P" 'bbdb-print)

;;; Variables:

(defvar bbdb-print-file-name "~/bbdb.tex"
  "*Default file name for printouts of BBDB database.")

(defvar bbdb-print-format-files '("bbdb-print" "multicol")
  "*Names of TeX files for formatting BBDB databases.
If these filenames are not absolute, the files must be located
somewhere that TeX will find them.")

(defvar bbdb-print-elide '(tex-name aka mail-alias nic nic-updated)
  "*List of fields NOT to print in address list.
See also bbdb-print-require.")

(defvar bbdb-print-require '(or address phone)
  "*What fields are required for printing a record.
This is evaluated for each record, and the record will be printed only
if it returns non-nil.  The symbols name, company, net, phone,
address, and notes will be set to appropriate values when this is
evaluated; they will be non-nil if the field does not exist or is elided.

The value of this variable can be any lisp expression, but typically
it will be used for a boolean combination of the field variables, as
in the following simple examples:

  Print only people whose phone numbers are known:
    (setq bbdb-print-require 'phone)
  Print people whose names AND companies are known:
    (setq bbdb-print-require '(and name company))
  Print people whose names, and either addresses OR phone numbers are known:
    (setq bbdb-print-require '(and name (or address phone))).")

(defvar bbdb-print-alist '((columns . 3) 
			   (separator . 2) 
			   (phone-on-first-line . "^[ \t]*$")
			   (ps-fonts . nil)
			   (font-size . 6)
			   (quad-hsize . "3.15in")
			   (quad-vsize . "4.5in"))
  "Formatting options for bbdb-print.
This is an alist of the form ((option1 . value1) (option2 . value2) ...)
The possible options and legal values are:
 - columns: 1, 2, 3, 4 or 'quad (4 little 2-column pages per sheet)
     or 'grid (12 credit-card-sized pages per sheet)
 - separator: 0-7, the style of heading for each letter.
     0=none, 1=line, 2=boxed letters, 3=large boxed letters, 4=large letters,
     5=letters with lines, 6=letters with suits, 7=boxed letters with suits.
 - phone-on-first-line: t means to put first phone number on the same
     line with the name, nil means just the name.  A string means to
     use the first phone number whose `location' matches that string,
     which should be a valid regular expression.
 - ps-fonts: nonnil means to use them, nil to use standard TeX fonts.
 - font-size: in points, any integer (assuming fonts in that size exist!).
 - quad-hsize, quad-vsize: for the quad format, horizontal and
     vertical size of the little pages.  These must be strings which
     are valid TeX dimensions, eg \"10cm\".")

(defvar bbdb-print-prolog 
  (concat "%%%% ====== Phone/Address list in -*-TeX-*- Format =====\n"
	  "%%%%        produced by bbdb-print, version 3.0\n\n")
  "*TeX statements to include at the beginning of the bbdb-print file.")

(defvar bbdb-print-epilog "\\endaddresses\n\\bye\n"
  "*TeX statements to include at the end of the bbdb-print file.")

;;; Functions:

(defsubst bbdb-print-if-not-blank (string &rest more)
  "If STRING is not null, then return it concatenated
with rest of arguments.  If it is null, then all arguments are 
ignored and the null string is returned."
  (if (or (null string) (equal "" string))
      ""
    (apply 'concat string more)))

;;;###autoload
(defun bbdb-print (visible-records to-file)
  "Make a TeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is \
used instead of simply \"\\[bbdb-print]\", then includes only the 
people currently in the *BBDB* buffer.  There are various variables
for customizing the content & format of the printout, see the file
bbdb-print.el for more information."
  (interactive (list (bbdb-do-all-records-p)
		     (read-file-name "Print To File: " bbdb-print-file-name)))
  (setq bbdb-print-file-name (expand-file-name to-file))
  (let ((current-letter t)
	(records (if (not visible-records)
		     (bbdb-records)
		   (set-buffer bbdb-buffer-name)
		   (mapcar 'car bbdb-records)))
	(psstring (if (cdr (assoc 'bbdb-print-ps-fonts bbdb-print-alist)) 
		      "ps" ""))
	(columns (cdr (assoc 'columns bbdb-print-alist))))
    (find-file bbdb-print-file-name)
    (widen) (erase-buffer)
    (insert bbdb-print-prolog)
    (let ((infiles bbdb-print-format-files))
      (while infiles
	(insert (format "\\input %s\n" (car infiles)))
	(setq infiles (cdr infiles))))
    (insert (format "\n\\set%ssize{%d}\n" 
		    psstring (cdr (assoc 'font-size bbdb-print-alist)))
	    (format "\\setseparator{%d}\n" 
		    (cdr (assoc 'separator bbdb-print-alist)))
	    (cond ((eq 'quad columns)
		   (format "\\quadformat{%s}{%s}" 
			   (cdr (assoc 'quad-hsize bbdb-print-alist))
			   (cdr (assoc 'quad-vsize bbdb-print-alist))))
		  ((eq 'grid columns) "\\grid")
		  ((= 4 columns) "\\fourcol")
		  ((= 3 columns) "\\threecol")
		  ((= 2 columns) "\\twocol")
		  ((= 1 columns) "\\onecol"))
	    "\n\n")
    (while records
      (setq current-letter 
	    (bbdb-print-format-record (car records) current-letter))
      (setq records (cdr records)))
    (insert bbdb-print-epilog)
    (goto-char (point-min))))

(defun bbdb-print-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in TeX format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: tex formatting deleted record")))
  
      (let* ((bbdb-elided-display bbdb-print-elide)
	     (first-letter 
	      (substring (concat (bbdb-record-sortkey record) "?") 0 1))
	     (name    (and (bbdb-field-shown-p 'name)
			   (or (bbdb-record-getprop record 'tex-name)
			       (bbdb-print-tex-quote
				(bbdb-record-name record)))))
	     (company (and (bbdb-field-shown-p 'company)
			   (bbdb-record-company record)))
	     (net     (and (bbdb-field-shown-p 'net)
			   (bbdb-record-net record)))
	     (phone   (and (bbdb-field-shown-p 'phone)
			   (bbdb-record-phones record)))
	     (address (and (bbdb-field-shown-p 'address)
			   (bbdb-record-addresses record)))
	     (notes   (bbdb-record-raw-notes record))
	     (begin   (point)))

	(if (not (eval bbdb-print-require))
	    nil				; lacks required fields

	  ;; Section header, if neccessary.

	  (if (and current-letter
		   (not (string-equal first-letter current-letter)))
	      (insert (format "\\separator{%s}\n\n" (bbdb-print-tex-quote
						     (upcase first-letter)))))

	  (insert "\\beginrecord\n")

	  ;; if there is no name, use company instead
	  (if (and (not name) company)
	      (setq name (bbdb-print-tex-quote company)
		    company nil))

	  (let ((pofl (cdr (assoc 'phone-on-first-line bbdb-print-alist)))
		(rightside "")
		p)
	    (cond ((null phone))
		  ((eq t pofl)
		   (setq rightside (bbdb-phone-string (car phone))
			 phone (cdr phone)))
		  ((stringp pofl)
		   (let ((p (bbdb-print-front-if
			     (function (lambda (ph)
					 (string-match pofl (aref ph 0))))
			     phone)))
		     (if p
			 (setq rightside (bbdb-phone-string (car p))
			       phone (cdr p))))))
	    (insert (format "\\firstline{%s}{%s}\n" 
		       name
		       (bbdb-print-tex-quote rightside))))

	  (if company
	      (insert (format "\\comp{%s}\n" (bbdb-print-tex-quote company))))

	  ;; Phone numbers

	  (while phone
	    (let ((place (aref (car phone) 0))
		  (number (bbdb-phone-string (car phone))))
	      (insert (format "\\phone{%s%s}\n" 
			      (bbdb-print-tex-quote 
			       (bbdb-print-if-not-blank place ": "))
			      (bbdb-print-tex-quote number)))
	      (setq phone (cdr phone))))

	  ;; Email address -- just list their first address.
	  ;;  Make all dots legal line-breaks.

	  (if net
	      (let ((net-addr (bbdb-print-tex-quote (car net)))
		    (start 0))
		(while (string-match "\\." net-addr start)
		  (setq net-addr
			(concat (substring net-addr 0 (match-beginning 0))
				".\\-"
				(substring net-addr (match-end 0))))
		  (setq start (+ 2 (match-end 0))))
		(insert (format "\\email{%s}\n" net-addr))))

	  ;; Addresses

	  (while address
	    (let ((addr (car address)))
	      (insert
	       (format 
		"\\address{%s}\n"
		(bbdb-print-tex-quote 
		 (concat 
		  (bbdb-print-if-not-blank (bbdb-address-street1 addr) "\\\\\n")
		  (bbdb-print-if-not-blank (bbdb-address-street2 addr) "\\\\\n")
		  (bbdb-print-if-not-blank (bbdb-address-street3 addr) "\\\\\n")
		  (bbdb-address-city addr)
		  (if (and (not (equal "" (bbdb-address-city addr)))
			   (not (equal "" (bbdb-address-state addr))))
		      ", ")
		  (bbdb-print-if-not-blank (bbdb-address-state addr) " ")
		  (bbdb-address-zip-string addr)
		  "\\\\")))))
	    (setq address (cdr address)))

	  ;; Notes

	  (if (stringp notes)
	      (setq notes (list (cons 'notes notes))))
	  (while notes
	    (let ((thisnote (car notes)))
	      (if (bbdb-field-shown-p (car thisnote))
		  (progn
		    (if (eq 'notes (car thisnote))
			(insert (format "\\notes{%s}\n" (bbdb-print-tex-quote 
							 (cdr thisnote))))
		      (insert (format "\\note{%s}{%s}\n" 
				      (bbdb-print-tex-quote (symbol-name
							     (car thisnote)))
				      (bbdb-print-tex-quote (cdr thisnote))))))))
	    (setq notes (cdr notes)))

	  ;; Mark end of the record.

	  (insert "\\endrecord\n\n")
	  (setq current-letter first-letter)))

      current-letter)

(defun bbdb-print-front-if (func list)
  "Move first elt of LIST satisfying FUNC to front.
The car of the returned list is the first element that returned nonnil;
The cdr is the rest of the list.  
But if the FUNC returns nil for every elements of the LIST, returns nil."
  (cond ((null list) nil)
	((funcall func (car list))
	 list)
	((let ((rest (bbdb-print-front-if func (cdr list))))
	   (if rest
	       (cons (car rest)
		     (cons (car list) (cdr rest))))))))

(defun bbdb-print-tex-quote (string)
  "Quote any unquoted TeX special characters that appear in STRING.
In other words, # alone will be replaced by \\#, but \\^ will be left for 
TeX to process as an accent."
  (if string
      (save-excursion
	(set-buffer (get-buffer-create " bbdb-print-tex-quote"))
	(erase-buffer)
	(insert string)
	(goto-char (point-min))
	(while (not (eobp))
	  (cond ((looking-at "[<>=]+") 
		 (replace-match "$\\&$"))
		((and (looking-at "[#$%&~_]")
		      (not (eq ?\\ (char-after (1- (point))))))
		 (insert "\\")
		 (forward-char 1))
		((and (looking-at "[{}]")
		      (not (eq ?\\ (char-after (1- (point))))))
		 (insert "$\\")
		 (forward-char 1)
		 (insert "$"))
		(t (forward-char 1))))
	(buffer-string))))

(provide 'bbdb-print)

;;; bbdb-print ends here.
