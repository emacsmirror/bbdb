;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the core of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.
;;; See the file bbdb.texinfo for documentation.
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
;;;  ------------------------------------------------------------------------
;;; |  There is a mailing list for discussion of BBDB: info-bbdb@xemacs.org  |
;;; |  To join, send mail to info-bbdb-request@xemacs.org (don't forget the  |
;;; |  -request part or you'll look silly in front of lots of people who     |
;;; |  have the ability to remember it indefinitely...)                      |
;;; |                                                                        |
;;; |  There is also a second mailing list, to which only bug fixes and      |
;;; |  new version announcements are sent; to be added to it, send mail to   |
;;; |  bbdb-announce-request@xemacs.org.  This is a very low volume list,    |
;;; |  and if you're using BBDB, you really should be on it.                 |
;;; |                                                                        |
;;; |  When joining these lists or reporting bugs, please mention which      |
;;; |  version you have.                                                     |
;;;  ------------------------------------------------------------------------

(defconst bbdb-version "1.51; 21-may-96.")
(defconst bbdb-file-format 2)

;; This nonsense is to get the definition of defsubst loaded in when this file
;; is loaded,without necessarily forcing the compiler to be loaded if we're 
;; running in an emacs with bytecomp-runtime.el predumped.  We are using 
;; `require' as a way to get compile-time evaluation of this form so that this
;; works in the old compiler as well as the new one.
;;
(require (progn
	   (provide 't) ; eeeewwww, gross!
	   (condition-case ()
	       (if (fboundp 'defsubst)
		   't
		 ;; If byte-optimize can be loaded, use that.
		 (require 'byte-optimize)
		 'byte-optimize)
	     ;; otherwise, use the boneheaded version of defsubst.
	     (error 'defsubst))))

(defvar bbdb-file "~/.bbdb"
  "*The name of the Insidious Big Brother Database file.")

(defvar bbdb-default-area-code 415
  "*The default area code to use when prompting for a new phone number.
This must be a number, not a string.")

(defvar bbdb-north-american-phone-numbers-p t
  "*Set this to nil if you want to enter phone numbers that aren't the same
syntax as those in North America (that is, [[1] nnn] nnn nnnn ['x' n*]).
If this is true, then some error checking is done so that you can't enter
incorrect phone numbers, and all phone numbers are pretty-printed the same
way.  European phone numbers don't have as strict a syntax, however, so
this is a harder problem for them (on which I am punting).

You can have both styles of phone number in your database by providing a
prefix argument to the bbdb-insert-new-field command.")

(defvar bbdb-electric-p t
  "*Whether bbdb mode should be `electric' like electric-buffer-list.")

(defvar bbdb-case-fold-search (default-value 'case-fold-search)
  "*This is the value of case-fold-search used by Meta-X bbdb and related
commands.  This variable lets the case-sensitivity of ^S and of the bbdb
commands be different.")

(defvar bbdb/mail-auto-create-p t
  "*If this is t, then VM, MH, and RMAIL will automatically create new bbdb
records for people you receive mail from.  If this is a function name
or lambda, then it is called with no arguments to decide whether an
entry should be automatically created.  You can use this to, for example,
not create records for messages which have reached you through a 
particular mailing list, or to only create records automatically if
the mail has a particular subject.")

(defvar bbdb/news-auto-create-p nil
  "*If this is t, then GNUS will automatically create new bbdb 
records for people you receive mail from.  If this is a function name
or lambda, then it is called with no arguments to decide whether an
entry should be automatically created.  You can use this to, for example,
create or not create messages which have a particular subject.  If you
want to autocreate messages based on the current newsgroup, it's 
probably a better idea to set this variable to t or nil from your 
gnus-Select-group-hook instead.")

(defvar bbdb-quiet-about-name-mismatches nil
  "*If this is true, then BBDB will not prompt you when it notices a
name change, that is, when the \"real name\" in a message doesn't correspond
to a record already in the database with the same network address.  As in,
\"John Smith <jqs@frob.com>\" versus \"John Q. Smith <jqs@frob.com>\".  
Normally you will be asked if you want to change it.")

(defvar bbdb-use-alternate-names t
  "*If this is true, then when bbdb notices a name change, it will ask you
if you want both names to map to the same record.")

(defvar bbdb-readonly-p nil
  "*If this is true, then nothing will attempt to change the bbdb database
implicitly, and you will be prevented from doing it explicitly.  If you have 
more than one emacs running at the same time, you might want to arrange for 
this to be set to t in all but one of them.")

(defvar bbdb-auto-revert-p nil
  "*If this variable is true and the BBDB file is noticed to have changed on 
disk, it will be automatically reverted without prompting you first.  Otherwise
you will be asked. (But if the file has changed and you hae made changes in 
memory as well, you will always be asked.)")

(defvar bbdb-notice-auto-save-file nil
  "*If this is true, then the BBDB will notice when its auto-save file is
newer than the file is was read from, and will offer to revert.")

(defvar bbdb-use-pop-up t
  "If true, display a continuously-updating bbdb window while in VM, MH,
RMAIL, or GNUS.  If 'horiz, stack the window horizontally if there is room.")

(defvar bbdb-pop-up-target-lines 5
  "*Desired number of lines in a VM/MH/RMAIL/GNUS pop-up bbdb window.")

(defvar bbdb-completion-type nil
  "*Controls the behaviour of 'bbdb-complete-name'.  If nil, completion is 
done across the set of all full-names and user-ids in the bbdb-database;
if the symbol 'name, completion is done on names only; if the symbol 'net, 
completion is done on network addresses only; if it is 'primary, then 
completion is done only across the set of primary network addresses (the
first address in the list of addresses for a given user).  If it is 
'primary-or-name, completion is done across primaries and real names.")

(defvar bbdb-completion-display-record t
  "*Whether bbdb-complete-name (\\<mail-mode-map>\\[bbdb-complete-name] \
in mail-mode) will update the *BBDB* buffer
to display the record whose email address has just been inserted.")

(defvar bbdb-user-mail-names nil
  "*A regular expression identifying the addresses that belong to you.
If a message from an address matching this is seen, the BBDB record for
the To: line will be shown instead of the one for the From: line.  If
this is nil, it will default to the value of (user-login-name).")

(defvar bbdb-always-add-addresses nil
  "*If this is true, then when the Insidious Big Brother Database notices
a new email address for a person, it will automatically add it to the list
of addresses.  If it is nil, you will be asked whether to add it.  If it is
the symbol 'never (really, if it is any non-t, non-nil value) then new 
network addresses will never be automatically added.

See also the variable `bbdb-new-nets-always-primary' for control of whether
the addresses go at the front of the list or the back.")

(defvar bbdb-new-nets-always-primary nil
  "*If this is true, then when the Insidious Big Brother Database adds a new 
address to a record, it will always add it to the front of the list of 
addresses, making it the primary address.  If this is nil, you will be asked.
If it is the symbol 'never (really, if it is any non-t, non-nil value) then
new network addresses will always be added at the end of the list.")

(defvar bbdb-send-mail-style nil
  "*Specifies which package should be used to send mail.
Should be 'vm, 'mh, 'mail (or nil, meaning guess.)")

(defvar bbdb-offer-save t
  "*If t, then certain actions will cause the BBDB to ask you whether
you wish to save the database.  If nil, then the offer to save will never
be made.  If not t and not nil, then any time it would ask you, it will
just save it without asking.")

(defvar bbdb-message-caching-enabled t
  "*Whether caching of the message->bbdb-record association should be used
for the interfaces which support it (VM, MH, and RMAIL).  This can speed
things up a lot.  One implication of this variable being true is that the
bbdb-notice-hook will not be called each time a message is selected, but
only the first time.  Likewise, if selecting a message would generate a
question (whether to add an address, change the name, etc) you will only
be asked that question the very first time the message is selected.")

(defvar bbdb-mode-hook nil
  "*Hook or hooks invoked when the *BBDB* buffer is created.")

(defvar bbdb-list-hook nil
  "*Hook or hooks invoked after the bbdb-list-buffer is filled in.  Invoked
with no arguments.")

(defvar bbdb-create-hook nil
  "*Hook or hooks invoked each time a new bbdb-record is created.  Invoked
with one argument, the new record.  This is called *before* the record is 
added to the database.  Note that bbdb-change-hook will be called as well.")

(defvar bbdb-change-hook nil
  "*Hook or hooks invoked each time a bbdb-record is altered.  Invoked with
one argument, the record.  This is called *before* the bbdb-database buffer
is modified.  Note that if a new bbdb record is created, both this hook and
bbdb-create-hook will be called.")

(defvar bbdb-after-change-hook nil
  "*Hook or hooks invoked each time a bbdb-record is altered.  Invoked with
one argument, the record.  This is called *after* the bbdb-database buffer
is modified, so if you want to modify the record each time it is changed,
you should use the `bbdb-change-hook' instead.  Note that if a new bbdb 
record is created, both this hook and bbdb-create-hook will be called.")

(defvar bbdb-canonicalize-net-hook nil
  "*If this is non-nil, it should be a function of one arg: a network address
string.  Whenever the Insidious Big Brother Database \"notices\" a message,
the corresponding network address will be passed to this function first, as
a kind of \"filter\" to do whatever transformations upon it you like before
it is compared against or added to the database.  For example: it is the case
that CS.CMU.EDU is a valid return address for all mail originating at a 
machine in the .CS.CMU.EDU domain.  So, if you wanted all such addresses to
be canonically hashed as user@CS.CMU.EDU, instead of as user@host.CS.CMU.EDU,
you might set this variable to a function like this:

 (setq bbdb-canonicalize-net-hook
       '(lambda (addr)
          (cond ((string-match \"\\\\`\\\\([^@]+@\\\\).*\\\\.\\\\(CS\\\\.CMU\\\\.EDU\\\\)\\\\'\"
                               addr)
                 (concat (substring addr (match-beginning 1) (match-end 1))
                         (substring addr (match-beginning 2) (match-end 2))))
                (t addr))))

You could also use this function to rewrite UUCP-style addresses into domain-
style addresses, or any number of things.

This function will be called repeatedly until it returns a value EQ to the
value passed in.  So multiple rewrite rules might apply to a single address.")

(defvar bbdb-canonicalize-redundant-nets-p t
  "*If this is non-nil, redundant network addresses will be ignored.
If a record has an address of the form foo@baz.com, setting this to t
will cause subsequently-noticed addresses like foo@bar.baz.com to be
ignored (since we already have a more general form of that address.)
This is similar in function to one of the possible uses of the variable
`bbdb-canonicalize-net-hook' but is somewhat more automatic.  (This 
can't quite be implemented in terms of the canonicalize-net-hook because
it needs access to the database to determine whether an address is 
redundant, and the canonicalize-net-hook is purely a textual manipulation
which is performed before any database access.)")

(defvar bbdb-notice-hook nil
  "*Hook or hooks invoked each time a bbdb-record is \"noticed\", that is,
each time it is displayed by the news or mail interfaces.  Invoked with
one argument, the new record.  The record need not have been modified for 
this to be called - use bbdb-change-hook for that.  You can use this to, 
for example, add something to the notes field based on the subject of the 
current message.  It is up to your hook to determine whether it is running
in GNUS, VM, MH, or RMAIL, and to act appropriately.

Also note that bbdb-change-hook will NOT be called as a result of any
modifications you may make to the record inside this hook.

Beware that if the variable `bbdb-message-caching-enabled' is true (a good
idea) then when you are using VM, MH, or RMAIL, this hook will be called only 
the first time that message is selected.  (The GNUS interface does not use
caching.)  When debugging the value of this hook, it is a good idea to set 
caching-enabled to nil.")

(defvar bbdb-after-read-db-hook nil
  "*Hook or hooks invoked (with no arguments) just after the Insidious Big 
Brother Database is read in.  Note that this can be called more than once if 
the BBDB is reverted.")

(defvar bbdb-load-hook nil
  "*Hook or hooks invoked (with no arguments) when the Insidious Big Brother 
Database code is first loaded.")

(defvar bbdb-mode-map nil
  "Keymap for Insidious Big Brother Database listings.")


;;; These are the buffer-local variables we use.
;;; They are mentioned here so that the compiler doesn't warn about them 
;;; when byte-compile-warn-about-free-variables is on.

(defvar bbdb-records nil)
(defvar bbdb-changed-records nil)
(defvar bbdb-end-marker nil)
(defvar bbdb-hashtable nil)
(defvar bbdb-propnames nil)
(defvar bbdb-message-cache nil)
(defvar bbdb-showing-changed-ones nil)
(defvar bbdb-modified-p nil)
(defvar bbdb-elided-display nil)

(defvar bbdb-debug t)
(defmacro bbdb-debug (&rest body)
  ;; ## comment out the next line to turn off debugging.
  ;; ## You really shouldn't do this!  But it will speed things up.
  (list 'and 'bbdb-debug (list 'let '((debug-on-error t)) (cons 'progn body)))
  )


;;; internal kludge to force queries to always happen with the mouse rather
;;; than basing the decision on the last-input-event; bind this, don't set it.
(defvar bbdb-force-dialog-boxes nil)

(defun bbdb-y-or-n-p (prompt)
  (prog1
      (cond ((and bbdb-force-dialog-boxes
		  (fboundp 'yes-or-no-p-dialog-box))
	     (if (and (fboundp 'raise-screen)
		      (not (screen-visible-p (selected-screen))))
		 (raise-screen (selected-screen)))
	     (yes-or-no-p-dialog-box prompt))
	    (t
	     (y-or-n-p prompt)))
    (message " ")))

(defun bbdb-yes-or-no-p (prompt)
  (prog1
      (funcall (if (and bbdb-force-dialog-boxes
			(fboundp 'yes-or-no-p-dialog-box))
		   'yes-or-no-p-dialog-box
		 'yes-or-no-p)
	       prompt)
    (message " ")))

(defun bbdb-invoke-hook (hook arg)
  "Like invoke-hooks, but invokes the given hook with one argument."
  (if (and (boundp hook) (setq hook (symbol-value hook)))
      (if (and (consp hook) (not (eq (car hook) 'lambda)))
	  (while hook
	    (funcall (car hook) arg)
	    (setq hook (cdr hook)))
	  (funcall hook arg))))

(defun bbdb-invoke-hook-for-value (hook &rest args)
  "If HOOK is nil, return nil.  If it is t, return t.  Otherwise,
return the value of funcalling it with the rest of the arguments."
  (cond ((eq hook nil) nil)
	((eq hook t) t)
	(t (apply hook args))))

(defmacro bbdb-defstruct (conc-name &rest slots)
  (setq conc-name (symbol-name conc-name))
  (let ((body '())
	(i 0)
	(L (length slots)))
    (while slots
      (setq body
	(nconc body
	  (let ((name1 (intern (concat conc-name (symbol-name (car slots)))))
		(name2 (intern (concat conc-name "set-" (symbol-name (car slots))))))
	    (list
	      (list 'defmacro name1 '(vector)
		    (list 'list ''aref 'vector i))
	      (list 'defmacro name2 '(vector value)
		    (list 'list ''aset 'vector i 'value))
	      ;(list 'put (list 'quote name1) ''edebug-form-hook ''(form))
	      ;(list 'put (list 'quote name2) ''edebug-form-hook ''(form form))
	      ))))
      (setq slots (cdr slots) i (1+ i)))
    (setq body (nconc body (list (list 'defconst
				       (intern (concat conc-name "length"))
				       L))))
    (cons 'progn body)))

;;; When reading this code, beware that "cache" refers to two things.
;;; It refers to the cache slot of bbdb-record structures, which is
;;; used for computed properties of the records; and it also refers
;;; to a message-id --> bbdb-record association list which speeds up
;;; the RMAIL, VM, and MH interfaces.

(bbdb-defstruct bbdb-record-
  firstname lastname aka company
  phones addresses net raw-notes
  cache
  )
(bbdb-defstruct bbdb-phone-
  location area exchange suffix extension
  )
(bbdb-defstruct bbdb-address-
  location street1 street2 street3 city state zip
  )
(bbdb-defstruct bbdb-cache-
  namecache sortkey marker deleted-p
  )

(defsubst bbdb-record-name-1 (record)
  (bbdb-cache-set-namecache (bbdb-record-cache record)
    (let ((fname (bbdb-record-firstname record))
	  (lname (bbdb-record-lastname record)))
      (if (> (length fname) 0)
	  (if (> (length lname) 0)
	      (concat fname " " lname)
	    fname)
	lname))))

(defun bbdb-record-name (record)
  (or (bbdb-cache-namecache (bbdb-record-cache record))
      (bbdb-record-name-1 record)))

(defun bbdb-record-sortkey (record)
  (or (bbdb-cache-sortkey (bbdb-record-cache record))
      (bbdb-cache-set-sortkey (bbdb-record-cache record)
        (downcase
          (concat (bbdb-record-lastname record)
		  (bbdb-record-firstname record)
		  (bbdb-record-company record))))))

(defmacro bbdb-record-marker (record)
  (list 'bbdb-cache-marker (list 'bbdb-record-cache record)))

(defmacro bbdb-record-deleted-p (record)
  (list 'bbdb-cache-deleted-p (list 'bbdb-record-cache record)))

(defmacro bbdb-record-set-deleted-p (record val)
  (list 'bbdb-cache-set-deleted-p (list 'bbdb-record-cache record) val))

(defmacro bbdb-record-set-namecache (record newval)
  (list 'bbdb-cache-set-namecache (list 'bbdb-record-cache record) newval))

(defmacro bbdb-record-set-sortkey (record newval)
  (list 'bbdb-cache-set-sortkey (list 'bbdb-record-cache record) newval))

(defmacro bbdb-record-set-marker (record newval)
  (list 'bbdb-cache-set-marker (list 'bbdb-record-cache record) newval))


;; The "notes" and "properties" accessors don't need to be fast.

(defun bbdb-record-notes (record)
  (if (consp (bbdb-record-raw-notes record))
      (cdr (assq 'notes (bbdb-record-raw-notes record)))
      (bbdb-record-raw-notes record)))

;; this works on the 'company field as well.
(defun bbdb-record-getprop (record property)
  (if (memq property '(name address addresses phone phones net aka AKA))
      (error "bbdb: cannot access the %s field this way" property))
  (if (eq property 'company)
      (bbdb-record-company record)
    (if (consp (bbdb-record-raw-notes record))
	(cdr (assq property (bbdb-record-raw-notes record)))
      (if (and (eq property 'notes)
	       (stringp (bbdb-record-raw-notes record)))
	  (bbdb-record-raw-notes record)
	nil))))

;; this works on the 'company field as well.
(defun bbdb-record-putprop (record property newval)
  (if (memq property '(name address addresses phone phones net aka AKA))
      (error "bbdb: cannot annotate the %s field this way" property))
  (if (eq property 'company)
      (bbdb-record-set-company record
	(bbdb-record-set-company record newval))
    (if (and (eq property 'notes)
	     (not (consp (bbdb-record-raw-notes record))))
	(bbdb-record-set-raw-notes record newval)
      (or (listp (bbdb-record-raw-notes record))
	  (bbdb-record-set-raw-notes record
	    (list (cons 'notes (bbdb-record-raw-notes record)))))
      (let ((old (assq property (bbdb-record-raw-notes record))))
	(if old
	    (if newval
		(setcdr old newval)
	      (bbdb-record-set-raw-notes record
		(delq old (bbdb-record-raw-notes record))))
	  (and newval
	       (bbdb-record-set-raw-notes record
		 (append (bbdb-record-raw-notes record)
			 (list (cons property newval))))))))
    ;; save some file space: if we ever end up with ((notes . "...")),
    ;; replace it with the string.
    (if (and (consp (bbdb-record-raw-notes record))
	     (null (cdr (bbdb-record-raw-notes record)))
	     (eq 'notes (car (car (bbdb-record-raw-notes record)))))
	(bbdb-record-set-raw-notes record
	  (cdr (car (bbdb-record-raw-notes record)))))
    )
  ;; If we're changing the company, then we need to sort, since the company
  ;; is the sortkey for nameless records.  This should almost never matter...
  (bbdb-change-record record (eq property 'company))
  newval)

(defun bbdb-record-set-notes (record newval)
  (if (consp (bbdb-record-raw-notes record))
      (bbdb-record-putprop record 'notes newval)
    (bbdb-record-set-raw-notes record newval)
    (bbdb-change-record record nil)))

(defun bbdb-phone-string (phone)
  (if (= 2 (length phone)) ; euronumbers....
      (aref phone 1)
    ;; numbers should come in two forms:
    ;; ["where" 415 555 1212 99] or ["where" "the number"]
    (if (stringp (aref phone 1))
	(error "doubleplus ungood: euronumbers unwork"))
    (concat (if (/= 0 (bbdb-phone-area phone))
		(format "(%03d) " (bbdb-phone-area phone))
		"")
	    (if (/= 0 (bbdb-phone-exchange phone))
		(format "%03d-%04d"
			(bbdb-phone-exchange phone) (bbdb-phone-suffix phone))
		"")
	    (if (and (bbdb-phone-extension phone)
		     (/= 0 (bbdb-phone-extension phone)))
		(format " x%d" (bbdb-phone-extension phone))
		""))))

(defun bbdb-address-zip-string (addr)
  (if (consp (bbdb-address-zip addr))
      (if (stringp (car (bbdb-address-zip addr)))
	  (concat (car (bbdb-address-zip addr))
		  " "
		  (car (cdr (bbdb-address-zip addr))))
	(format "%05d-%04d" (car (bbdb-address-zip addr))
		(car (cdr (bbdb-address-zip addr)))))
    (if (or (eq 0 (bbdb-address-zip addr))
	    (null (bbdb-address-zip addr)))
	""
      (format "%05d" (bbdb-address-zip addr)))))

(defmacro bbdb-record-lessp (record1 record2)
  (list 'string< (list 'bbdb-record-sortkey record1)
	         (list 'bbdb-record-sortkey record2)))

(defmacro bbdb-subint (string match-number)
  (list 'string-to-int
	(list 'substring string
	      (list 'match-beginning match-number)
	      (list 'match-end match-number))))

(defmacro bbdb-error-retry (form)
  (list 'catch ''--bbdb-error-retry--
	(list 'while ''t
	      (list 'condition-case '--c--
		    (list 'throw ''--bbdb-error-retry-- form)
		    '(error
		      (ding)
		      (let ((cursor-in-echo-area t))
			(if (fboundp 'display-error) ; lemacs 19.8+
			    (display-error --c-- nil)
			  (message "Error: %s" (nth 1 --c--)))
			(sit-for 2)))))))

;;; I no longer remember why I felt this was necessary, but I think it 
;;; might have been because of the bug in the save-excursion of 18.55-57
(defmacro bbdb-save-buffer-excursion (&rest body)
  (list 'save-excursion
    (list 'let '((--bbdb-obuf-- (current-buffer)))
      (list 'unwind-protect (cons 'progn body)
	'(set-buffer --bbdb-obuf--)))))

(defvar bbdb-buffer nil)
(defmacro bbdb-buffer ()
  '(if (and bbdb-buffer (buffer-name bbdb-buffer))
       bbdb-buffer
     (setq bbdb-buffer (find-file-noselect bbdb-file 'nowarn))))

(defmacro bbdb-with-db-buffer (&rest body)
  (cons 'bbdb-save-buffer-excursion
	(cons '(set-buffer (bbdb-buffer))
	      (if (and (boundp 'bbdb-debug) bbdb-debug)
		  ;; if we're debugging, and the .bbdb buffer is visible in
		  ;; a window, temporarilly switch to that window so that
		  ;; when we come out, that window has been scrolled to the
		  ;; record we've just modified.  (make w-point = b-point)
		  (list
		    (list 'let '((w (and bbdb-debug
					 (get-buffer-window
					  (buffer-name
					   (get-buffer bbdb-file))))))
			  (list 'save-excursion
			    (cons 'save-window-excursion
				  (cons '(and w (select-window w))
					body)))))
		  body))))


(defsubst bbdb-string-trim (string)
  "lose leading and trailing whitespace"
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (substring string 0 (match-beginning 0))
    string))


(defun bbdb-read-string (prompt &optional default)
  "Reads a string, trimming trailing whitespace.  If DEFAULT is multiple
lines, then the minibuffer is enlarged to fit it while editing."
  (let ((n 0)
	(start 0)
	(L (length default)))
    (while (< start L)
      (setq start (1+ (or (string-match "\n" default start) L))
	    n (1+ n)))
    (save-excursion
     (save-window-excursion
      (if (and (boundp 'epoch::version) epoch::version)
	  nil  ; this breaks epoch...
	(let ((w (selected-window)))
	  (select-window (minibuffer-window))
	  (enlarge-window (max 0 (- n (window-height))))
	  (sit-for 0) ; avoid redisplay glitch
	  (select-window w)))
      (bbdb-string-trim
	(read-string prompt default))))))


(defsubst bbdb-field-shown-p (field)
  (or (null bbdb-elided-display)
      (eq field 'name)
      (not (or (eq bbdb-elided-display t)
	       (memq field bbdb-elided-display)))))


(defun bbdb-format-record (record &optional brief)
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: formatting deleted record")))
  (let ((name (bbdb-record-name record))
	(comp (bbdb-record-company record)))
    (cond ((and name comp) (insert name " - " comp))
	  ((or name comp) (insert (or name comp)))
	  (t (insert "???")))
    (cond ((eq brief t)
	   (let ((p (point)))
	     (beginning-of-line)
	     (if (<= (- p (point)) 47)
		 (goto-char p)
	       (goto-char (+ (point) 44))
	       (setq p (point))
	       (end-of-line)
	       (delete-region p (point))
	       (insert "...")))
	   (let ((phone (car (bbdb-record-phones record)))
		 (net (car (bbdb-record-net record)))
		 (notes (bbdb-record-raw-notes record)))
	     (if (or phone net notes)
		 (progn (indent-to 48)
			(insert (if notes ". " "  "))))
	     (cond (phone (insert (bbdb-phone-string phone))
			  (indent-to 70)
			  (insert " ("); don't ask, it compiles better
			  (insert (bbdb-phone-location phone))
			  (insert ")"))
		   (net   (insert net))))
	   (insert "\n"))
	  (t
	   (insert "\n")
	   (let* ((bbdb-elided-display brief) ;pfeh.
		  (aka (bbdb-record-aka record))
		  (phones (and (bbdb-field-shown-p 'phone)
			       (bbdb-record-phones record)))
		  (addrs (and (bbdb-field-shown-p 'address)
			      (bbdb-record-addresses record)))
		  phone)
	   (while phones
	     (setq phone (car phones))
	     (insert (format " %14s: " (bbdb-phone-location phone)))
	     (insert (bbdb-phone-string phone) "\n")
	     (setq phones (cdr phones)))
	   (let (addr c s)
	     (while addrs
	       (setq addr (car addrs))
	       (insert (format " %14s: " (bbdb-address-location addr)))
	       (if (= 0 (length (setq s (bbdb-address-street1 addr)))) nil
		 (indent-to 17) (insert s "\n"))
	       (if (= 0 (length (setq s (bbdb-address-street2 addr)))) nil
		 (indent-to 17) (insert s "\n"))
	       (if (= 0 (length (setq s (bbdb-address-street3 addr)))) nil
		 (indent-to 17) (insert s "\n"))
	       (indent-to 17)
	       (insert (setq c (bbdb-address-city addr)))
	       (setq s (bbdb-address-state addr))
	       (if (and (> (length c) 0) (> (length s) 0)) (insert ", "))
	       (insert s "  ")
	       (insert (bbdb-address-zip-string addr) "\n")
	       (setq addrs (cdr addrs))))
	   (if (and (bbdb-record-net record)
		    (bbdb-field-shown-p 'net))
	       (insert (format " %14s: %s\n" "net"
			       (mapconcat (function identity)
					  (bbdb-record-net record)
					  ", "))))
	   (if (and aka
		    (bbdb-field-shown-p 'aka))
	       (insert (format " %14s: %s\n" "AKA"
			       (mapconcat (function identity)
					  aka ", "))))
	   (let ((notes (bbdb-record-raw-notes record)))
	     (if (stringp notes)
		 (setq notes (list (cons 'notes notes))))
	     (while notes
	       (if (bbdb-field-shown-p (car (car notes)))
		   (progn
		     (insert (format " %14s: " (car (car notes))))
		     (let ((p (point)))
		       (insert (cdr (car notes)))
		       (save-excursion
			 (save-restriction
			   (narrow-to-region p (1- (point)))
			   (goto-char (1+ p))
			   (while (search-forward "\n" nil t)
			     (insert (make-string 17 ?\ )))))
		       (insert "\n"))))
	       (setq notes (cdr notes)))))
	   (insert "\n")))))

(defconst bbdb-gag-messages nil
  "Bind this to t to quiet things down - do not set it!")

(defconst bbdb-buffer-name "*BBDB*")

(defvar bbdb-elided-display nil
  "*Set this to t if to make the bbdb-display commands default to displaying
one line per record instead of a full listing.  Set this to a list of some
of the symbols '(address phone net notes) to select those fields to be left 
out of the listing (you can't leave out the name field).

This is the default state for Meta-x bbdb and friends.  You can have a
different default for when the BBDB buffer is automatically updated by the
mail and news interfaces by setting the variable `bbdb-pop-up-elided-display'.
If that variable is unbound, this variable will be consulted instead.")

(defvar bbdb-pop-up-elided-display) ; default unbound.
(put 'bbdb-pop-up-elided-display
     'variable-documentation
     "*Set this to t if to make the pop-up BBDB buffer default to displaying
one line per record instead of a full listing.  Set this to a list of some
of the symbols '(address phone net notes) to select those fields to be left
out of the listing (you can't leave out the name field).

The default state for Meta-x bbdb and friends is controlled by the variable
`bbdb-elided-display'; this variable (`bbdb-pop-up-elided-display') is the
default for when the BBDB buffer is automatically updated by the mail and
news interfaces.  If bbdb-pop-up-elided-display is unbound, then 
bbdb-elided-display will be consulted instead by mail and news.")


(defmacro bbdb-pop-up-elided-display ()
  '(if (boundp 'bbdb-pop-up-elided-display)
       bbdb-pop-up-elided-display
       bbdb-elided-display))

(defun bbdb-frob-mode-line (n)
  (setq mode-line-buffer-identification
	(if (> n 0)
	    (list 24 "BBDB: "
		  (list 10
		    (format "%d/%d" n (length (bbdb-records))))
		  '(bbdb-showing-changed-ones " !!" "   "))
	  '("- Insidious Big Brother Database " mode-line-modified "-"))
	mode-line-modified
	'(bbdb-readonly-p "--%%%%-" (bbdb-modified-p "--**-" "-----"))))

(defun bbdb-display-records-1 (records &optional append)
  (if (or (null records)
	  (consp (car records)))
      nil
    (setq records (mapcar (function (lambda (x)
			    (list x bbdb-elided-display (make-marker))))
			  records)))
  (let ((b (current-buffer))
	(first (car (car records))))
    (with-output-to-temp-buffer bbdb-buffer-name
      (set-buffer bbdb-buffer-name)
      ;; If we're appending these records to the ones already displayed,
      ;; then first remove any duplicates, and then sort them.
      (if append
	  (let ((rest records))
	    (while rest
	      (if (assq (car (car rest)) bbdb-records)
		  (setq records (delq (car rest) records)))
	      (setq rest (cdr rest)))
	    (setq records (append bbdb-records records))
	    (setq records
		  (sort records
			(function (lambda (x y)
				    (bbdb-record-lessp (car x) (car y))))))))
      (make-local-variable 'mode-line-buffer-identification)
      (make-local-variable 'mode-line-modified)
      (set (make-local-variable 'bbdb-showing-changed-ones) nil)
      (let ((done nil)
	    (rest records)
	    (changed (bbdb-changed-records)))
	(while (and rest (not done))
	  (setq done (memq (car (car rest)) changed)
		rest (cdr rest)))
	(setq bbdb-showing-changed-ones done))
      (bbdb-frob-mode-line (length records))
      (if (not bbdb-gag-messages) (message "Formatting..."))
      (bbdb-mode)
      ;; this in in the *BBDB* buffer, remember, not the .bbdb buffer.
      (set (make-local-variable 'bbdb-records) nil)
      (setq bbdb-records records)
      (let ((buffer-read-only nil)
	    prs)
	(bbdb-debug (setq prs (bbdb-records)))
	(setq truncate-lines t)
	(while records
	  (bbdb-debug (if (not (memq (car (car records)) prs)) (error "record doubleplus unpresent!")))
	  (set-marker (nth 2 (car records)) (point))
	  (bbdb-format-record (nth 0 (car records))
			      (nth 1 (car records)))
	  (setq records (cdr records))))
      (if (not bbdb-gag-messages) (message "Formatting...done.")))
    (set-buffer bbdb-buffer-name)
    (if append
	(let ((cons (assq first bbdb-records))
	      (window (get-buffer-window (current-buffer))))
	  (if window (set-window-start window (nth 2 cons)))
	  ;; this doesn't really belong here, but it's convenient...
	  (save-excursion (run-hooks 'bbdb-list-hook))))
    (bbdbq)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (set-buffer b)))

(defun bbdb-undisplay-records ()
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (setq bbdb-showing-changed-ones nil
	  mode-line-modified nil
	  bbdb-records nil
	  buffer-read-only t)
    (set-buffer-modified-p nil)))

;;; Electric display stuff

(defconst bbdb-inside-electric-display nil)
;; hack hack: a couple of specials that the electric stuff uses for state.
(defvar bbdb-electric-execute-me)
(defvar bbdb-electric-completed-normally)

(defun electric-bbdb-display-records (records)
  (require 'electric)
  (let ((bbdb-electric-execute-me nil))   ; Hack alert!  throw-to-execute sets this!
   (let ((bbdb-inside-electric-display t)
	 buffer
	 bbdb-electric-completed-normally ; Hack alert!  throw-to-execute sets this!
	 )
    (save-excursion
     (save-window-excursion
      (save-window-excursion (bbdb-display-records-1 records))
      (setq buffer (window-buffer (Electric-pop-up-window bbdb-buffer-name)))
      (set-buffer buffer)
      (if (not bbdb-gag-messages)
	  (message "<<< Press Space to bury the Insidious Big Brother Database list >>>"))
      (catch 'Done
	(while t
	  (catch 'Blow-off-the-error
	    (setq bbdb-electric-completed-normally nil)
	    (unwind-protect
		 (progn
		   (catch 'electric-bbdb-list-select
		     (Electric-command-loop 'electric-bbdb-list-select
					    "-> " t))
		   (setq bbdb-electric-completed-normally t))
	      ;; protected
	      (if bbdb-electric-completed-normally
		  (throw 'Done t)
		(ding)
		(message "BBDB-Quit")
		(throw 'Blow-off-the-error t)
		)))))
      (bury-buffer buffer))))
   (message " ")
   (if bbdb-electric-execute-me
       (eval bbdb-electric-execute-me)))
  nil)

(defun bbdb-electric-throw-to-execute (form-to-execute)
  "Exit the electric-command-loop, and evaluate the given form once we're out."
  ;; Hack alert!  These variables are bound only within the scope of
  ;; bbdb-electric-display-records!
  (if (not (boundp 'bbdb-electric-execute-me))
      (error "plusungood: electrical short"))
  (setq bbdb-electric-execute-me form-to-execute
	bbdb-electric-completed-normally t)
  (throw 'electric-bbdb-list-select t))


(defun bbdb-done-command () (interactive)
  (throw 'electric-bbdb-list-select t))

(defun bbdb-bury-buffer ()
  (interactive)
  (if bbdb-inside-electric-display
      (bbdb-done-command)
    (bury-buffer)))

(defun bbdb-display-records (records)
  (let ((bbdb-window (get-buffer-window bbdb-buffer-name)))
    (if (and bbdb-electric-p
	     ;; never be electric if the buffer is already on screen.
	     (not bbdb-window))
	(progn
	  (define-key bbdb-mode-map " " 'bbdb-done-command)
	  (electric-bbdb-display-records records))
      (bbdb-display-records-1 records)
      (save-excursion (run-hooks 'bbdb-list-hook))
      ;; don't smash keybinding if they invoked the bbdb-display
      ;; from inside an electric loop.
      (if bbdb-inside-electric-display
	  nil
	(define-key bbdb-mode-map " " 'undefined))
      (if (and (not bbdb-gag-messages)
	       (not bbdb-window))
	  (message
	    (substitute-command-keys
	      (if (one-window-p t)
		  (if pop-up-windows
		      "Type \\[delete-other-windows] to unshow the bbdb-list window."
		      "Type \\[switch-to-buffer] RET to unshow the bbdb-list window.")
		"Type \\[switch-to-buffer-other-window] RET to restore old contents of the bbdb-list window.")))))))

(defun bbdbq ()
  (if (not (zerop (logand (random) 31))) nil
    (let ((v '["\104\157\156\47\164\40\163\165\163\160\145\143\164\40\171\157\
\165\162\40\156\145\151\147\150\142\157\162\72\40\162\145\160\157\162\164\40\
\150\151\155\41" "\146\156\157\162\144" "\103\157\156\163\165\155\145\40\55\55\
\40\102\145\40\123\151\154\145\156\164\40\55\55\40\104\151\145" "\114\157\166\
\145\40\102\151\147\40\102\162\157\164\150\145\162" "\114\145\145\40\110\141\
\162\166\145\171\40\117\163\167\141\154\144\40\141\143\164\145\144\40\141\154\
\157\156\145"]))
      (message (aref v (% (logand 255 (random)) (length v))))
      (message " "))))


(defmacro bbdb-hashtable ()
  '(bbdb-with-db-buffer (bbdb-records nil t) bbdb-hashtable))

(defun bbdb-changed-records ()
  (bbdb-with-db-buffer (bbdb-records nil t) bbdb-changed-records))

(defmacro bbdb-gethash (name &optional ht)
  (list 'symbol-value
	(list 'intern-soft name
	      (or ht '(bbdb-hashtable)))))

(defmacro bbdb-puthash (name record &optional ht)
  (list 'set (list 'intern name
		   (or ht '(bbdb-hashtable)))
	record))

(defmacro bbdb-remhash (name &optional ht)
  (list 'let (list (list 's (list 'intern-soft name
				  (or ht '(bbdb-hashtable)))))
	'(and s (set s nil))))


(defsubst bbdb-search-simple (name net)
  "name is a string; net is a string or list of strings."
  (if (eq 0 (length name)) (setq name nil))
  (if (eq 0 (length net)) (setq net nil))
  (bbdb-records t) ; make sure db is parsed; don't check disk (faster)
  (or (and name (bbdb-gethash (downcase name)))
      (and net
	   (if (stringp net)
	       (bbdb-gethash (downcase net))
	     (let ((answer nil))
	       (while (and net (null answer))
		 (setq answer (bbdb-gethash (downcase (car net)))
		       net (cdr net)))
	       answer)))))


(defun bbdb-net-convert (record)
  "Given a record whose net field is a comma-separated string, convert it to
a list of strings (the new way of doing things.)  Returns the new list."
  (bbdb-record-set-net record (bbdb-split (bbdb-record-net record) ",")))

(defun bbdb-split (string separators)
  (let (result
	(not-separators (concat "^" separators)))
    (save-excursion
      (set-buffer (get-buffer-create " *split*"))
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (while (progn
	       (skip-chars-forward separators)
	       (skip-chars-forward " \t\n\r")
	       (not (eobp)))
	(let ((begin (point))
	      p)
	  (skip-chars-forward not-separators)
	  (setq p (point))
	  (skip-chars-backward " \t\n\r")
	  (setq result (cons (buffer-substring begin (point)) result))
	  (goto-char p)))
      (erase-buffer))
    (nreverse result)))


(defsubst bbdb-hash-record (record)
  "Insert the record in the appropriate hashtables.  This must be called 
while the .bbdb buffer is selected."
  (let ((name (bbdb-record-name-1 record))  ; faster version
	(company (bbdb-record-company record))
	(aka (bbdb-record-aka record))
	(net (bbdb-record-net record)))
    (if (not (= 0 (length name))) ; could be nil or ""
	(bbdb-puthash (downcase name) record bbdb-hashtable))
    ;; #### we don't do hash collision detection on company names, so this
    ;;      is a potentially dangerous thing to do I guess.  But it's useful.
    ;;      This makes completion possible on company fields of records that
    ;;      have a company but no name.
    (if (and (= 0 (length name))
	     (not (= 0 (length company))))
	(bbdb-puthash (downcase company) record bbdb-hashtable))
    (while aka
      (bbdb-puthash (downcase (car aka)) record bbdb-hashtable)
      (setq aka (cdr aka)))
    (while net
      (bbdb-puthash (downcase (car net)) record bbdb-hashtable)
      (setq net (cdr net)))))

(defvar inside-bbdb-records nil)

(defun bbdb-records (&optional dont-check-disk already-in-db-buffer)
  "Return a list of all bbdb records; read in and parse the db if necessary.
This also notices if the disk file has changed out from under us, unless
optional arg DONT-CHECK-DISK is non-nil (which is faster, but hazardous.)"
  (if inside-bbdb-records
      (let ((debug-on-error t))
	(error "catastrophic: bbdb-records recursed")))
  (let ((inside-bbdb-records t)
	shut-up)
    (bbdb-save-buffer-excursion
      ;; get the buffer, don't worry if it's out of synch with disk yet.
      (let ((buf (if already-in-db-buffer
		     --bbdb-obuf--  ; hackorama; let's bum some cycles...
		     (set-buffer (bbdb-buffer)))))
	;; make sure the BBDB in memory is not out of synch with disk.
	(cond (dont-check-disk nil)
	      ((verify-visited-file-modtime buf) nil)
	      ((and bbdb-auto-revert-p
		    (not (buffer-modified-p buf)))
	       (message "BBDB has changed on disk, reverting...")
	       (setq shut-up t)
	       (revert-buffer t t))
	      ;; hassle the user
	      ((bbdb-yes-or-no-p (if (buffer-modified-p buf)
				"BBDB has changed on disk; flush your changes and revert? "
				"BBDB has changed on disk; revert? "))
	       (or (file-exists-p bbdb-file)
		   (error "bbdb: file %s no longer exists!!" bbdb-file))
	       (revert-buffer t t)
	       )
	      ;; this is the case where the .bbdb file has changed; the buffer
	      ;; has changed as well; and the user has answered "no" to the
	      ;; "flush your changes and revert" question.  The only other
	      ;; alternative is to save the file right now.  If they answer
	      ;; no to the following question, they will be asked the
	      ;; preceeding question again and again some large (but finite)
	      ;; number of times.  `bbdb-records' is called a lot, you see...
	      ((buffer-modified-p buf)
	       ;; this prompts
	       (bbdb-save-db t t))
	      ;; otherwise, the buffer and file are inconsistent, but we let
	      ;; them stay that way.
	      )
	(if (assq 'bbdb-records (buffer-local-variables))
	    nil
	  (set (make-local-variable 'bbdb-records) nil)
	  (set (make-local-variable 'bbdb-changed-records) nil)
	  (set (make-local-variable 'bbdb-end-marker) nil)
	  (set (make-local-variable 'bbdb-hashtable) nil)
	  (set (make-local-variable 'bbdb-propnames) nil)
	  (set (make-local-variable 'revert-buffer-function)
	       'bbdb-revert-buffer)
	  (make-local-variable 'write-file-hooks)
	  (setq write-file-hooks
		(append write-file-hooks '(bbdb-write-file-hook-fn))
		bbdb-hashtable (make-vector 1021 0)))
	(setq bbdb-modified-p (buffer-modified-p)
	      buffer-read-only bbdb-readonly-p)
	(or bbdb-records
	    (cond ((= (point-min) (point-max)) ; special-case empty db
		   ;; this doesn't need to be insert-before-markers because
		   ;; there are no db-markers in this buffer.
		   (insert (format ";;; file-version: %d\n" bbdb-file-format))
		   (bbdb-flush-all-caches)
		   (setq bbdb-end-marker (point-marker))
		   ;;(run-hooks 'bbdb-after-read-db-hook) ; run this?
		   nil)
		  (t
		   (or shut-up (message "Parsing BBDB..."))
		   (bbdb-flush-all-caches)
		   (cond ((and bbdb-notice-auto-save-file
			       (file-newer-than-file-p (make-auto-save-file-name)
						       buffer-file-name))
			  (if (bbdb-yes-or-no-p "BBDB auto-save file is newer; recover it? ")
			      (progn
				(recover-file buffer-file-name)
				(bury-buffer (current-buffer)) ; recover-file selects it
				(auto-save-mode 1) ; turn autosave back on
				(delete-file (make-auto-save-file-name))
				(message "Auto-save mode is ON in BBDB buffer.  Suggest you save it soon.")
				(sleep-for 2))
			      ;; delete auto-save anyway, so we don't keep asking.
			    (condition-case nil
				(delete-file (make-auto-save-file-name))
			      (file-error nil)))
			  ;; tail-recurse and try again
			  (let ((inside-bbdb-records nil))
			    (bbdb-records)))
			 (t
			  ;; normal case
			  (fillarray bbdb-hashtable 0)
			  (parse-bbdb-internal))))))))))

(defun bbdb-revert-buffer (arg noconfirm)
  ;; The .bbdb file's revert-buffer-function.
  ;; Don't even think of calling this.
  (kill-all-local-variables)		; clear db and caches.
  (if (get-buffer bbdb-buffer-name)	; now contains invalid records; nukem.
      (bbdb-undisplay-records))
  (let ((revert-buffer-function nil))	; don't loop.
    (revert-buffer arg noconfirm)))

(defun parse-bbdb-internal ()
  (bbdb-debug (message "Parsing BBDB... (reading...)"))
  (widen)
  (goto-char (point-min))
  ;; go to the point at which the first record begins
  (cond ((eq (following-char) ?\[) nil)
	((search-forward "\n[" nil 0) (forward-char -1))
	(t nil)) ;; no records
  ;; look backwards for user-defined field names (for completion purposes.)
  (save-excursion
    (if (re-search-backward "^;+[ \t]*user-fields:[ \t]*\(" nil t)
	(progn
	  (goto-char (1- (match-end 0)))
	  (setq bbdb-propnames
		(mapcar (function (lambda (x) (list (symbol-name x))))
			(read (point-marker)))))))
  ;; look backwards for file version, and convert if necessary.
  ;; (at least, I'll write this code if I ever change the file format again...)
  (let (v)
    (save-excursion
       (if (re-search-backward
	    "^;+[ \t]*file-version:[ \t]*\\([0-9]+\\)[ \t]*$" nil t)
	   (setq v (car (read-from-string
			 (buffer-substring
			  (match-beginning 1) (match-end 1)))))))
     (if (null v) ; version 2, but no file-version: line. Bootstrap it.
	 (let ((modp (buffer-modified-p)))
	   ;; This should never happen (not any more, anyway...)
	   (bbdb-debug (error "bbdb corrupted: no file-version line"))
	   (setq v 2)
	   (save-excursion
	     (if (re-search-backward "^;" nil t)
		 (forward-line 1)
	       (goto-char 1))
	     ;; remember, this goes before the begin-marker of the first
	     ;; record in the database!
	     (insert-before-markers ";;; file-version: 2\n"))
	   (set-buffer-modified-p modp)))
     (cond ((< v bbdb-file-format)
	    ;; add calls to appropriate conversion routines here.
	    (error "bbdb: how can the file format version possibly be less than 2?"))
	   ((> v bbdb-file-format)
	    (error "BBDB version %s doesn't understand file format version %s."
		   bbdb-version bbdb-file-format))))
  
  (bbdb-debug
   (or (eobp) (looking-at "[\[]")
       (error "no following bracket: bbdb corrupted"))
   (if (save-excursion
	 (save-restriction
	   (widen)
	   (save-excursion (search-backward "\n[" nil t))))
       (error "bbdb corrupted: records before point")))

  (parse-bbdb-frobnicate (parse-bbdb-read)))

(defun parse-bbdb-read ()
  ;; narrow the buffer to skip over the rubbish before the first record.
  (narrow-to-region (point) (point-max))
  (let ((records nil))
    ;; insert parens so we can read the db in one fell swoop (down in C).
    (let ((buffer-read-only nil)
	  (modp (buffer-modified-p))
	  ;; Make sure those parens get cleaned up.
	  ;; This code had better stay simple!
	  (inhibit-quit t))
      (goto-char (point-min)) (insert "(\n")
      (goto-char (point-max)) (insert "\n)")
      (goto-char (point-min))
      (setq records (read (current-buffer)))
      (goto-char (point-min)) (delete-char 2)
      (goto-char (point-max)) (delete-char -2)
      (set-buffer-modified-p modp))
    records))

(defun parse-bbdb-frobnicate (records)
  ;; now we have to come up with a marker for each record.  Rather than
  ;; calling read for each record, we read them at once (already done) and
  ;; assume that the markers are at each newline.  If this isn't the case,
  ;; things can go *very* wrong.
  (goto-char (point-min))
  (while (looking-at "[ \t\n\f]*;")
    (forward-line 1))
  (widen)
  (bbdb-debug (message "Parsing BBDB... (frobnicating...)"))
  (let ((rest records)
	record)
    (while rest
      (setq record (car rest))
      ;; yow, are we stack-driven yet??  Damn byte-compiler...
      ;; Make a cache.  Put it in the record.  Put a marker in the cache.
      (bbdb-cache-set-marker
       (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
       (point-marker))
      (bbdb-debug
       (let ((name (bbdb-record-name record))
	     tmp)
	 (if (and name
		  (setq tmp (bbdb-gethash (setq name (downcase name))
					  bbdb-hashtable)))
	     (signal 'error (list "duplicate bbdb entries" record tmp)))))
      (bbdb-hash-record record)
      (forward-line 1)
      (setq rest (cdr rest))
      (bbdb-debug
       (if (and rest (not (looking-at "[\[]")))
	   (error "bbdb corrupted: junk between records at %s" (point))))
      ))
  ;; all done.
  (setq bbdb-records records)
  (setq bbdb-end-marker (point-marker))
  (run-hooks 'bbdb-after-read-db-hook)
  (bbdb-debug (message "Parsing BBDB... (frobnicating...done)"))
  records)


(defmacro bbdb-user-mail-names ()
  "Returns a regexp matching the address of the logged-in user"
  '(or bbdb-user-mail-names
    (setq bbdb-user-mail-names
     (concat "\\b" (regexp-quote (user-login-name)) "\\b"))))

(defun bbdb-write-file-hook-fn ()
  "Added to write-file-hooks locally to the bbdb-file buffer."
  ;; this is premature as the file isn't actually written yet; but it's just
  ;; for the benefit of the mode-line of the *BBDB* buffer, and there isn't
  ;; an after-write-file-hook, so it'll do.
  (setq bbdb-modified-p nil
	bbdb-changed-records nil)
  (let ((b (get-buffer bbdb-buffer-name)))
    (if b
	(bbdb-save-buffer-excursion
	  (set-buffer b)
	  (setq bbdb-showing-changed-ones nil)
	  (set-buffer-modified-p nil)))))


(defun bbdb-delete-record-internal (record)
  (if (null (bbdb-record-marker record)) (error "bbdb: marker unpresent"))
  (bbdb-with-db-buffer
    (if (memq record bbdb-changed-records) nil
	(setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((tail (memq record bbdb-records)))
      (if (null tail) (error "bbdb: unfound %s" record))
      (setq bbdb-records (delq record bbdb-records))
      (delete-region (bbdb-record-marker record)
		     (if (cdr tail)
			 (bbdb-record-marker (car (cdr tail)))
			 bbdb-end-marker))
      (if (bbdb-record-name record)
	  (let ((name (downcase (bbdb-record-name record))))
	    (bbdb-remhash name bbdb-hashtable)))
      (let ((nets (bbdb-record-net record)))
	(while nets
	  (bbdb-remhash (downcase (car nets)) bbdb-hashtable)
	  (setq nets (cdr nets))))
      (let ((aka (bbdb-record-aka record)))
	(while aka
	  (bbdb-remhash (downcase (car aka)) bbdb-hashtable)
	  (setq aka (cdr aka))))
      (bbdb-record-set-sortkey record nil)
      (setq bbdb-modified-p t))))

(defun bbdb-insert-sorted (record records)
  "Inserts the RECORD into the list of RECORDS, in order (assuming the list is
already sorted.)  Returns the new head."
  (bbdb-debug (if (memq record records) (error "doubleplus ununique: - %s" record)))
  (let* ((rest (cons nil records))
	 (top rest))
    (while (and (cdr rest)
		(bbdb-record-lessp (nth 1 rest) record))
      (setq rest (cdr rest)))
    (setcdr rest (cons record (cdr rest)))
    (cdr top)))

(defun bbdb-insert-record-internal (record)
  (if (null (bbdb-record-marker record))
      (bbdb-record-set-marker record (make-marker)))
  (bbdb-with-db-buffer
    (if (memq record bbdb-changed-records) nil
	(setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((print-escape-newlines t))
      (bbdb-record-set-sortkey record nil) ; just in case...
      (setq bbdb-records
	    (bbdb-insert-sorted record bbdb-records))
      (let ((next (car (cdr (memq record bbdb-records)))))
	(goto-char (if next
		       (bbdb-record-marker next)
		       bbdb-end-marker))
	;; before printing the record, remove the cache \(we don't want that
	;; written to the file.\)  Ater writing, put the cache back and update
	;; the cache's marker.
	(let ((cache (bbdb-record-cache record))
	      (point (point)))
	  (bbdb-debug
	   (if (= point (point-min))
	       (error "doubleplus ungood: inserting at point-min (%s)" point))
	   (if (and (/= point bbdb-end-marker)
		    (not (looking-at "[\[]")))
	       (error "doubleplus ungood: not inserting before a record (%s)"
		      point))
	   )
	  (bbdb-record-set-cache record nil)
	  (insert-before-markers (prin1-to-string record) "\n")
	  (set-marker (bbdb-cache-marker cache) point)
	  (bbdb-record-set-cache record cache)
;;	  (if (bbdb-record-name record)
;;	      (bbdb-puthash (downcase (bbdb-record-name record)) record bbdb-hashtable))
;;	  (let ((nets (bbdb-record-net record)))
;;	    (while nets
;;	      (bbdb-puthash (downcase (car nets)) record bbdb-hashtable)
;;	      (setq nets (cdr nets))))
	  ;; This is marginally slower because it rebuilds the namecache,
	  ;; but it makes jbw's life easier. :-\)
	  (bbdb-hash-record record)
	  )
	record))
    (setq bbdb-modified-p t)))

(defun bbdb-overwrite-record-internal (record)
  (bbdb-with-db-buffer
    (if (memq record bbdb-changed-records) nil
	(setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((print-escape-newlines t)
	  (tail bbdb-records))
      (while (and tail (not (eq record (car tail))))
	(setq tail (cdr tail)))
      (if (null tail) (error "bbdb: unfound %s" record))
      (let ((cache (bbdb-record-cache record)))

	(bbdb-debug
	 (if (<= (bbdb-cache-marker cache) (point-min))
	     (error "doubleplus ungood: cache marker is %s"
		    (bbdb-cache-marker cache)))
	 (goto-char (bbdb-cache-marker cache))
	 (if (and (/= (point) bbdb-end-marker)
		  (not (looking-at "[\[]")))
	     (error "doubleplus ungood: not inserting before a record (%s)"
		    (point)))
	 )

	(goto-char (bbdb-cache-marker cache))
	(bbdb-record-set-cache record nil)

	(insert (prin1-to-string record) "\n")
	(delete-region (point)
		       (if (cdr tail)
			   (bbdb-record-marker (car (cdr tail)))
			 bbdb-end-marker))
	(bbdb-record-set-cache record cache)

	(bbdb-debug
	 (if (<= (if (cdr tail)
		     (bbdb-record-marker (car (cdr tail)))
		   bbdb-end-marker)
		 (bbdb-record-marker record))
	     (error "doubleplus ungood: overwrite unworks")))

	(setq bbdb-modified-p t)
	record))))

(defvar inside-bbdb-change-record nil "hands off")

(defun bbdb-change-record (record need-to-sort)
  "Update the database after a change to the given record.  Second arg 
NEED-TO-SORT is whether the name has changed.  You still need to worry 
about updating the name hash-table."
  (if inside-bbdb-change-record
      record
    (let ((inside-bbdb-change-record t))
      (bbdb-invoke-hook 'bbdb-change-hook record)
      (bbdb-debug (if (bbdb-record-deleted-p record)
		      (error "bbdb: changing deleted record")))
      (if (memq record (bbdb-records))	; this checks file synchronization too.
	  (if (not need-to-sort)
	      (progn
		(bbdb-overwrite-record-internal record)
		(bbdb-debug
		 (if (not (memq record (bbdb-records)))
		     (error "doubleplus ungood: overwrite unworks"))))
	    (bbdb-delete-record-internal record)
	    (bbdb-debug
	     (if (memq record (bbdb-records))
		 (error "doubleplus ungood: delete unworks")))
	    (bbdb-insert-record-internal record)
	    (bbdb-debug
	     (if (not (memq record (bbdb-records))) (error "insert unworks")))
	    )
	;; else it's deleted -- add it.
	(bbdb-insert-record-internal record)
	(bbdb-debug (if (not (memq record (bbdb-records)))
			(error "doubleplus ungood: insert unworks")))
	)
      (setq bbdb-modified-p t)
      (bbdb-invoke-hook 'bbdb-after-change-hook record)
      record)))

(defmacro bbdb-propnames ()
  '(bbdb-with-db-buffer bbdb-propnames))

(defun bbdb-set-propnames (newval)
  (bbdb-with-db-buffer
    (setq bbdb-propnames newval)
    (widen)
    (goto-char (point-min))
    (and (not (eq (following-char) ?\[))
	 (search-forward "\n[" nil 0))
    (if (re-search-backward "^[ \t]*;+[ \t]*user-fields:[ \t]*\(" nil t)
	(progn
	  (goto-char (1- (match-end 0)))
	  (delete-region (point) (progn (end-of-line) (point))))
      (and (re-search-backward "^[ \t]*;.*\n" nil t)
	   (goto-char (match-end 0)))
      ;; remember, this goes before the begin-marker of the first
      ;; record in the database!
      (insert-before-markers ";;; user-fields: \n")
      (forward-char -1))
    (prin1 (mapcar (function (lambda (x) (intern (car x))))
		   bbdb-propnames)
	   (current-buffer))
    bbdb-propnames))

(defun bbdb-modified-p ()
  (setq bbdb-modified-p (buffer-modified-p (bbdb-buffer))))


;;; BBDB mode

(defun bbdb-mode ()
  "Major mode for viewing and editing the Insidious Big Brother Database.
Letters no longer insert themselves.  Numbers are prefix arguments.
You can move around using the usual cursor motion commands.
\\<bbdb-mode-map>
\\[bbdb-edit-current-field]\t edit the field on the current line.
\\[bbdb-record-edit-notes]\t shortcut for editing the 'notes' field.
\\[advertized-bbdb-delete-current-field-or-record]\t delete the field on the \
current line.  If the current line is the\n\t first line of a record, then \
this deletes the entire record from\n\t the database.
\\[bbdb-insert-new-field]\t inserts a new field into the current record, as \
opposed to editing\n\t an existing one.  Note that this will let you add \
new fields of your\n\t own as well.
\\[bbdb-next-record], \\[bbdb-prev-record]\t move to the next and previous \
displayed record, respectively.
\\[bbdb-elide-record]\t toggles whether the current record is displayed in a \
one-line\n\t listing, or a full multi-line listing.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-elide-record]\t does that \
for all records.
\\[bbdb-send-mail]\t lets you compose mail to the person represented by the \
current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-send-mail]\t sends a mail \
message to everyone listed in the BBDB \
buffer.
\\[bbdb-save-db]\t saves the BBDB file to disk.
\\[bbdb-refile-record]\t merges the contents of this record with some other, \
and then deletes\n\t this one.  See this command's documentation.
\\[bbdb-finger]\t fingers the network address of the current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-finger]\t fingers the \
network address of all displayed records.
\\[bbdb-omit-record]\t removes the current record from the display without \
deleting it\n\t from the database.  This is often a useful thing to do before \
using\n\t one of the `*' commands.
\\[bbdb-info]\t enters the Info node (online documentation) for BBDB.
\\[bbdb-help]\t displays a one-line command-summary in the echo-area.

In send-mail mode, \\<mail-mode-map>\\[bbdb-complete-name] does completion \
across the set of names and network \naddresses in the database.

Variables of note:
	bbdb-file
	bbdb-electric-p
	bbdb-use-pop-up
	bbdb-pop-up-target-lines
	bbdb-readonly-p
	bbdb-notice-auto-save-file
	bbdb/mail-auto-create-p
	bbdb/news-auto-create-p
	bbdb-quiet-about-name-mismatches
	bbdb-completion-type
	bbdb-default-area-code
	bbdb-north-american-phone-numbers-p

There are numerous hooks.  M-x apropos ^bbdb.*hook RET

The keybindings, more precisely:
\\{bbdb-mode-map}"
  (setq major-mode 'bbdb-mode)
  (setq mode-name "BBDB")
  (use-local-map bbdb-mode-map)
  (run-hooks 'bbdb-mode-hook))

;;; these should be in bbdb-com.el but they're so simple, why load it all.

(defun bbdb-next-record (p)
  "Move the cursor to the first line of the next bbdb-record."
  (interactive "p")
  (if (< p 0)
      (bbdb-prev-record (- p))
    (forward-char)
    (while (> p 0)
      (or (re-search-forward "^[^ \t\n]" nil t)
	  (progn (beginning-of-line)
		 (error "no next record")))
      (setq p (1- p)))
    (beginning-of-line)))

(defun bbdb-prev-record (p)
  "Move the cursor to the first line of the previous bbdb-record."
  (interactive "p")
  (if (< p 0)
      (bbdb-next-record (- p))
    (while (> p 0)
      (or (re-search-backward "^[^ \t\n]" nil t)
	  (error "no previous record"))
      (setq p (1- p)))))


(defun bbdb-maybe-update-display (bbdb-record)
  (save-excursion
    (save-window-excursion
      (let ((w (get-buffer-window bbdb-buffer-name))
	    (b (current-buffer)))
	(if w
	    (unwind-protect
		(progn (set-buffer bbdb-buffer-name)
		       (save-restriction
			 (if (assq bbdb-record bbdb-records)
			     (bbdb-redisplay-records))))
	      (set-buffer b)))))))

(defun bbdb-annotate-notes (bbdb-record annotation &optional fieldname)
  (or bbdb-record (error "unperson"))
  (setq annotation (bbdb-string-trim annotation))
  (if (memq fieldname '(name address addresses phone phones net aka AKA))
      (error "bbdb: cannot annotate the %s field this way" fieldname))
  (or fieldname (setq fieldname 'notes))
  (or (memq fieldname '(notes company))
      (assoc (symbol-name fieldname) (bbdb-propnames))
      (bbdb-set-propnames (append (bbdb-propnames)
				  (list (list (symbol-name fieldname))))))
  (if (string= "" annotation)
      nil
    (let ((notes (bbdb-string-trim
		   (or (bbdb-record-getprop bbdb-record fieldname) ""))))
      (bbdb-record-putprop bbdb-record fieldname
			   (if (string= notes "")
			       annotation
			       (concat notes
				       (if (eq fieldname 'company) "; "
					 (or (get fieldname 'field-separator)
					     "\n"))
				       annotation)))
      (bbdb-maybe-update-display bbdb-record))))


(defun bbdb-offer-save ()
  "Offer to save the Insidious Big Brother Database if it is modified."
  (if bbdb-offer-save
      (bbdb-save-db (eq bbdb-offer-save t))))

(defvar bbdb-save-db-timeout nil
  "*If non-nil, then when bbdb-save-db is asking you whether to save the db,
it will time out to `yes' after this many seconds.  This only works if the
function y-or-n-p-with-timeout is defined (meaning XEmacs.)")

(defun bbdb-save-db (&optional prompt-first mention-if-not-saved)
  "save the db if it is modified."
  (interactive (list nil t))
  (bbdb-with-db-buffer
    (if (and (buffer-modified-p)
	     (or (null prompt-first)
		 (if bbdb-readonly-p
		     (bbdb-y-or-n-p "Save the BBDB, even though it's supposedly read-only? ")
		   (if (and bbdb-save-db-timeout
			    (fboundp 'y-or-n-p-with-timeout))
		       (y-or-n-p-with-timeout
			bbdb-save-db-timeout "Save the BBDB now? " t)
		     (bbdb-y-or-n-p "Save the BBDB now? ")))))
	(save-buffer)
      (if mention-if-not-saved (message "BBDB unsaved")))))

;(defun bbdb-dry-heaves (sort-p message-p)
;  "Rewrite each and every record in the bbdb file; this is necessary if we 
;are updating an old file format.  SORT-P says whether we need to resort the
;entire db (you should never need this).  MESSAGE-P says whether to sound
;off for each record converted."
;  (let ((records (bbdb-records))
;	(i 0))
;    (while records
;      (bbdb-change-record (car records) sort-p)
;      (setq records (cdr records))
;      (if message-p (message "%d" (setq i (1+ i)))))))


(defun bbdb-add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (if (not (boundp hook)) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(setq old (list old)))
    (if (member function old)
	nil
      (set hook (if append
		    (append old (list function)) ; don't nconc
		  (cons function old))))))

;;; mail and news interface

(defun bbdb-clean-username (string)
  "Strips garbage from the user full name string."
  ;; This function is called a lot, and should be fast.  But I'm loathe to
  ;; remove any of the functionality in it.
  (if (string-match "[@%!]" string)  ; ain't no user name!  It's an address!
      (bbdb-string-trim string)
   (let ((case-fold-search t))
     ;; Take off leading and trailing non-alpha chars \(quotes, parens,
     ;; digits, etc) and things which look like phone extensions \(like
     ;; "x1234" and "ext. 1234". \)
     ;; This doesn't work all the time because some of our friends in
     ;; northern europe have brackets in their names...
     (if (string-match "\\`[^a-z]+" string)
	 (setq string (substring string (match-end 0))))
     (while (string-match
	     "\\(\\W+\\([Xx]\\|[Ee]xt\\.?\\)\\W*[-0-9]+\\|[^a-z]+\\)\\'"
	     string)
       (setq string (substring string 0 (match-beginning 0))))
     ;; replace tabs, multiple spaces, dots, and underscores with a single space.
     ;; but don't replace ". " with " " because that could be an initial.
     (while (string-match "\\(\t\\|  +\\|\\(\\.\\)[^ \t_]\\|_+\\)" string)
       (setq string (concat (substring string 0
				       (or (match-beginning 2)
					   (match-beginning 1)))
			    " "
			    (substring string (or (match-end 2)
						  (match-end 1))))))
     ;; If the string contains trailing parenthesized comments, nuke 'em.
     (if (string-match "[^ \t]\\([ \t]*\\((\\| -\\| #\\)\\)" string)
	 (progn
	   (setq string (substring string 0 (match-beginning 1)))
	   ;; lose rubbish this may have exposed.
	   (while
	       (string-match
		"\\(\\W+\\([Xx]\\|[Ee]xt\\.?\\)\\W*[-0-9]+\\|[^a-z]+\\)\\'"
		string)
	       (setq string (substring string 0 (match-beginning 0))))
	   ))
     string)))

;;; message-caching, to speed up the the mail interfaces

(defvar bbdb-buffers-with-message-caches '()
  "A list of all the buffers which have stuff on their bbdb-message-cache 
local variable.  When we re-parse the .bbdb file, we need to flush all of
these caches.")

(defun notice-buffer-with-cache (buffer)
  (or (memq buffer bbdb-buffers-with-message-caches)
      (progn
	;; First remove any deleted buffers which may have accumulated.
	;; This happens only when a buffer is added to the list, so it
	;; ought not happen that frequently (each time you read mail, say.)
	(let ((rest bbdb-buffers-with-message-caches))
	  (while rest
	    (if (null (buffer-name (car rest)))
		(setq bbdb-buffers-with-message-caches
		      (delq (car rest) bbdb-buffers-with-message-caches)))
	    (setq rest (cdr rest))))
	;; now add this buffer.
	(setq bbdb-buffers-with-message-caches
	      (cons buffer bbdb-buffers-with-message-caches)))))

(make-variable-buffer-local 'bbdb-message-cache)

(defmacro bbdb-message-cache-lookup (message-key
				     &optional message-sequence-buffer)
  (list 'progn '(bbdb-records)  ; yuck, this is to make auto-revert happen
				; in a convenient place.
  (list 'and 'bbdb-message-caching-enabled
	(let ((bod
	       (list 'let (list (list '--cons--
				      (list 'assq message-key 'bbdb-message-cache)))
		     '(if (and --cons-- (bbdb-record-deleted-p (cdr --cons--)))
		       (progn
			 (setq bbdb-message-cache (delq --cons-- bbdb-message-cache))
			 nil)
		       (cdr --cons--)))))
	  (if message-sequence-buffer
	      (list 'save-excursion
		    (list 'set-buffer message-sequence-buffer)
		    bod)
	      bod))))
  )

(defmacro bbdb-encache-message (message-key bbdb-record &optional message-sequence-buffer)
  "Don't call this multiple times with the same args, it doesn't replace."
  (let ((bod (list 'let (list (list '--rec-- bbdb-record))
		   (list 'if 'bbdb-message-caching-enabled
			 (list 'and '--rec--
			  (list 'progn
			   '(notice-buffer-with-cache (current-buffer))
			   (list 'cdr
			    (list 'car
			     (list 'setq 'bbdb-message-cache
			      (list 'cons (list 'cons message-key '--rec--)
				    'bbdb-message-cache))))))
			 '--rec--))))
    (if message-sequence-buffer
	(cons 'save-excursion
	      (list (list 'set-buffer message-sequence-buffer)
		    bod))
	bod)))

(defun bbdb-flush-all-caches ()
  (bbdb-debug
    (and bbdb-buffers-with-message-caches
	 (message "Flushing BBDB caches")))
  (save-excursion
    (while bbdb-buffers-with-message-caches
      (if (buffer-name (car bbdb-buffers-with-message-caches))
	  (progn
	    (set-buffer (car bbdb-buffers-with-message-caches))
	    (setq bbdb-message-cache nil)))
      (setq bbdb-buffers-with-message-caches
	    (cdr bbdb-buffers-with-message-caches)))))


(defconst bbdb-name-gubbish
  (concat "[-,. \t/\\]+\\("
	  "[JjSs]r\\.?"
	  "\\|V?\\(I\\.?\\)+V?"
	  "\\)\\W*\\'"))

(defun bbdb-divide-name (string)
  "divide the string into a first name and a last name, cleverly."
  ;; ## This shouldn't be here.
  (if (string-match "\\W+\\([Xx]\\|[Ee]xt\\.?\\)\\W*[-0-9]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  (let* ((case-fold-search nil)
	 (str string)
	 (gubbish (string-match bbdb-name-gubbish string)))
    (if gubbish
	(setq gubbish (substring str gubbish)
	      str (substring string 0 (match-beginning 0))))
    (if (string-match " +\\(\\([^ ]+ *- *\\)?[^ ]+\\)\\'" str)
	(list (substring str 0 (match-beginning 0))
	      (concat
	       (substring str (match-beginning 1))
	       (or gubbish "")))
      (list string ""))))

(defun bbdb-check-alternate-name (possible-name record)
  (let (aka)
    (if (setq aka (bbdb-record-aka record))
	(let ((down-name (downcase possible-name))
	      match)
	  (while aka
	    (if (equal down-name (downcase (car aka)))
		(setq match (car aka)
		      aka nil)
		(setq aka (cdr aka))))
	  match))))


(defun bbdb-canonicalize-address (net)
  ;; call the bbdb-canonicalize-net-hook repeatedly until it returns a
  ;; value eq to the value passed in.  This implies that it can't
  ;; destructively modify the string.
  (while (not (eq net (setq net (funcall bbdb-canonicalize-net-hook net)))))
  net)


;; Mostly written by Rod Whitby.
(defun bbdb-net-redundant-p (net old-nets)
  "Returns non-nil if NET represents a sub-domain of one of the OLD-NETS.
The returned value is the address which makes this one redundant.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\", 
and \"foo@quux.bar.baz.com\" is redundant w.r.t. \"foo@bar.baz.com\"."
  (let ((redundant-addr nil))
    (while (and (not redundant-addr) old-nets)
      ;; Calculate a host-regexp for each address in OLD-NETS
      (let* ((old (car old-nets))
	     (host-index (string-match "@" old))
	     (name (and host-index (substring old 0 host-index)))
	     (host (and host-index (substring old (1+ host-index))))
	     ;; host-regexp is "^<name>@.*\.<host>$"
	     (host-regexp (and name host
			       (concat "\\`" (regexp-quote name)
				       "@.*\\." (regexp-quote host)
				       "\\'"))))
	;; If NET matches host-regexp, then it is redundant
	(if (and host-regexp net
		 (string-match host-regexp net))
	    (setq redundant-addr old)))
      (setq old-nets (cdr old-nets)))
    redundant-addr))


(defun bbdb-annotate-message-sender (from &optional loudly create-p
				     prompt-to-create-p)
  "Fills the record corresponding to the sender with as much info as possible.
A record may be created by this; a record or nil is returned.
If bbdb-readonly-p is true, then a record will never be created.
If CREATE-P is true, then a record may be created, otherwise it won't.
If PROMPT-TO-CREATE-P is true, then the user will be asked for confirmation
before the record is created, otherwise it is created without confirmation 
\(assuming that CREATE-P is true\).  "
  (let* ((data (if (consp from)
		   from ; if from is a cons, it's pre-parsed (hack hack)
		 (mail-extract-address-components from)))
	 (name (car data))
	 (net (car (cdr data))))
    (if (equal name net) (setq name nil))
    (bbdb-debug
     (if (equal name "") (error "mail-extr returned \"\" as name"))
     (if (equal net "") (error "mail-extr returned \"\" as net")))

  (if (and net bbdb-canonicalize-net-hook)
      (setq net (bbdb-canonicalize-address net)))

  (let ((change-p nil)
	(record (bbdb-search-simple name net))
	(created-p nil)
	(fname name)
	(lname nil)
	old-name
	bogon-mode)
    (and record (setq old-name (bbdb-record-name record)))

    ;; This is to prevent having losers like "John <blat@foop>" match
    ;; against existing records like "Someone Else <john>".
    ;;
    ;; The solution implemented here is to never create or show records
    ;; corresponding to a person who has a real-name which is the same
    ;; as the network-address of someone in the db already.  This is not
    ;; a good solution.
    (let (down-name old-net)
      (if (and record name
	       (not (equal (setq down-name (downcase name))
			   (and old-name (downcase old-name)))))
	  (progn
	    (setq old-net (bbdb-record-net record))
	    (while old-net
	      (if (equal down-name (downcase (car old-net)))
		  (progn
		    (setq bogon-mode t
			  old-net nil)
		    (message
	 "Ignoring bogon %s's name \"%s\" to avoid name-clash with \"%s\""
	             net name old-name)
		    (sit-for 2))
		(setq old-net (cdr old-net)))))))
    
    (if (or record
	    bbdb-readonly-p
	    (not create-p)
	    (not (or name net))
	    bogon-mode)
	;; no further action required
	nil
      ;; otherwise, the db is writable, and we may create a record.
      (setq record (if (or (null prompt-to-create-p)
			   (bbdb-y-or-n-p (format "%s is not in the db; rectify? " name)))
		       (make-vector bbdb-record-length nil))
	    created-p (not (null record)))
      (if record
	  (bbdb-record-set-cache record (make-vector bbdb-cache-length nil)))
      (if created-p (bbdb-invoke-hook 'bbdb-create-hook record)))
    (if (or bogon-mode (null record))
	nil
      (bbdb-debug (if (bbdb-record-deleted-p record)
		      (error "nasty nasty deleted record nasty.")))
      (if (and name
	       (not (equal (and name (downcase name))
			   (and old-name (downcase old-name))))
	       (or (null bbdb-use-alternate-names)
		   (not (bbdb-check-alternate-name name record)))
	       (let ((fullname (bbdb-divide-name name))
		     tmp)
		 (setq fname (car fullname)
		       lname (nth 1 fullname))
		 (not (and (equal (downcase fname)
				(and (setq tmp (bbdb-record-firstname record))
				     (downcase tmp)))
			   (equal (downcase lname)
				  (and (setq tmp (bbdb-record-lastname record))
				       (downcase tmp)))))))
	  ;; have a message-name, not the same as old name.
	  (cond (bbdb-readonly-p nil)
		;;(created-p nil)
		((and bbdb-quiet-about-name-mismatches old-name)
		 (message "name mismatch: \"%s\" changed to \"%s\""
			  (bbdb-record-name record) name)
		 (sit-for 1))
		((or created-p
		     (if (null old-name)
			 (bbdb-y-or-n-p (format "Assign name \"%s\" to address \"%s\"? "
						name (car (bbdb-record-net record))))
		       (bbdb-y-or-n-p (format "Change name \"%s\" to \"%s\"? "
					      old-name name))))
		 (setq change-p 'sort)
		 (and old-name bbdb-use-alternate-names
		     (if (bbdb-y-or-n-p (format "Keep name \"%s\" as an AKA? " old-name))
			 (bbdb-record-set-aka record
			   (cons old-name (bbdb-record-aka record)))
		       (bbdb-remhash (downcase old-name))))
		 (bbdb-record-set-namecache record nil)
		 (bbdb-record-set-firstname record fname)
		 (bbdb-record-set-lastname record lname)
		 (bbdb-debug (or fname lname
				 (error "bbdb: should have a name by now")))
		 (bbdb-puthash (downcase (bbdb-record-name record))
			       record)
		 )
		((and old-name
		      bbdb-use-alternate-names
		      (bbdb-y-or-n-p
			(format "Make \"%s\" an alternate for \"%s\"? "
				name old-name)))
		 (setq change-p 'sort)
		 (bbdb-record-set-aka
		   record
		   (cons name (bbdb-record-aka record)))
		 (bbdb-puthash (downcase name) record)
		 )))

      ;; It's kind of a kludge that the "redundancy" concept is built in.
      ;; Maybe I should just add a new hook here...  The problem is that the
      ;; canonicalize-net-hook is run before database lookup, and thus can't
      ;; refer to the database to determine whether a net is redundant.
      (if bbdb-canonicalize-redundant-nets-p
	  (setq net (or (bbdb-net-redundant-p net (bbdb-record-net record))
			net)))

      (if (and net (not bbdb-readonly-p))
	  (if (null (bbdb-record-net record))
	      ;; names are always a sure match, so don't bother prompting here.
	      (progn (bbdb-record-set-net record (list net))
		     (bbdb-puthash (downcase net) record) ; important!
		     (or change-p (setq change-p t)))
	    ;; new address; ask before adding.
	    (if (let ((rest-net (bbdb-record-net record))
		      (new (downcase net))
		      (match nil))
		  (while (and rest-net (null match))
		    (setq match (string= new (downcase (car rest-net)))
			  rest-net (cdr rest-net)))
		  match)
		nil
	      (if (cond
		   ((eq bbdb-always-add-addresses t)
		    t)
		   (bbdb-always-add-addresses ; non-t and non-nil = never
		    nil)
		   (t
		    (and
		     (not (equal net "???"))
		     (let ((the-first-bit
			    (format "add address \"%s\" to \"" net))
			   ;; this groveling is to prevent the "(y or n)" from
			   ;; falling off the right edge of the screen.
			   (the-next-bit (mapconcat 'identity
						    (bbdb-record-net record)
						    ", "))
			   (w (window-width (minibuffer-window))))
		       (if (> (+ (length the-first-bit)
				 (length the-next-bit) 15) w)
			   (setq the-next-bit
				 (concat
				  (substring the-next-bit
				    0 (max 0 (- w (length the-first-bit) 20)))
				  "...")))
		       (bbdb-y-or-n-p (concat the-first-bit the-next-bit
					      "\"? "))))))
		  (let ((front-p (cond ((null bbdb-new-nets-always-primary)
					(bbdb-y-or-n-p
					 (format
					  "Make \"%s\" the primary address? "
					  net)))
				       ((eq bbdb-new-nets-always-primary t)
					t)
				       (t nil))))
		    (bbdb-record-set-net record
		      (if front-p
			  (cons net (bbdb-record-net record))
			(nconc (bbdb-record-net record) (list net))))
		    (bbdb-puthash (downcase net) record)  ; important!
		    (or change-p (setq change-p t)))))))
      (bbdb-debug
	(if (and change-p bbdb-readonly-p)
	    (error
	      "doubleplus ungood: how did we change anything in readonly mode?")))
      (if (and loudly change-p)
	  (if (eq change-p 'sort)
	      (message "noticed \"%s\"" (bbdb-record-name record))
	      (if (bbdb-record-name record)
		  (message "noticed %s's address \"%s\"" (bbdb-record-name record) net)
		  (message "noticed naked address \"%s\"" net))))
      (if change-p
	  (bbdb-change-record record (eq change-p 'sort)))
      (bbdb-invoke-hook 'bbdb-notice-hook record)
      record))))


;;; window configuration hackery

(defun bbdb-pop-up-bbdb-buffer (&optional horiz-predicate)
  "Find the largest window on the screen, and split it, displaying the
*BBDB* buffer in the bottom 'bbdb-pop-up-target-lines' lines (unless
the *BBDB* buffer is already visible, in which case do nothing.)

If 'bbdb-use-pop-up' is the symbol 'horiz, and the first window 
matching HORIZ-PREDICATE is sufficiently wide (> 100 columns) then
the window will be split vertically rather than horizontally."
  (let ((b (current-buffer)))
   (if (get-buffer-window bbdb-buffer-name)
       nil
     (if (and (eq bbdb-use-pop-up 'horiz)
	      horiz-predicate
	      (bbdb-pop-up-bbdb-buffer-horizontally horiz-predicate))
	 nil
      (let* ((first-window (selected-window))
	     (tallest-window first-window)
	     (window first-window))
	;; find the tallest window...
	(while (not (eq (setq window (previous-window window)) first-window))
	  (if (> (window-height window) (window-height tallest-window))
	      (setq tallest-window window)))
	;; select it and split it...
	(select-window tallest-window)
	(let ((size (min
		      (- (window-height tallest-window)
			 window-min-height 1)
		      (- (window-height tallest-window)
			 (max window-min-height
			      (1+ bbdb-pop-up-target-lines))))))
	  (split-window tallest-window
			(if (> size 0) size window-min-height)))
	(if (memq major-mode
		  '(gnus-Group-mode gnus-Subject-mode gnus-Article-mode))
	    (goto-char (point-min))) ; make gnus happy...
	;; goto the bottom of the two...
	(select-window (next-window))
	;; make it display *BBDB*...
	(let ((pop-up-windows nil))
	  (switch-to-buffer (get-buffer-create bbdb-buffer-name)))
	;; select the original window we were in...
	(select-window first-window)))
    ;; and make sure the current buffer is correct as well.
    (set-buffer b)
    nil)))

(defun bbdb-pop-up-bbdb-buffer-horizontally (predicate)
  (if (<= (screen-width) 112)
      nil
    (let* ((first-window (selected-window))
	   (got-it nil)
	   (window first-window))
      (while (and (not (setq got-it (funcall predicate window)))
		  (not (eq first-window (setq window (next-window window)))))
	)
      (if (or (null got-it)
	      (<= (window-width window) 112))
	  nil
	(let ((b (current-buffer)))
	  (select-window window)
	  (split-window-horizontally 80)
	  (select-window (next-window window))
	  (let ((pop-up-windows nil))
	    (switch-to-buffer (get-buffer-create bbdb-buffer-name)))
	  (select-window first-window)
	  (set-buffer b)
	  t)))))

(defun bbdb-version () (interactive) (message "BBDB version %s" bbdb-version))

;;; resorting, which really shouldn't be necesary...

(defun bbdb-record-lessp-fn (record1 record2) ; for use as a funarg
  (bbdb-record-lessp record1 record2))

(defun bbdb-resort-database ()
  ;; only as a last resort, ha ha
  (let* ((records (copy-sequence (bbdb-records))))
    (bbdb-with-db-buffer
     (setq bbdb-records (sort bbdb-records 'bbdb-record-lessp-fn))
     (if (equal records bbdb-records)
	 nil
       (message "DANGER!  BBDB was mis-sorted; it's being fixed...")
       (goto-char (point-min))
       (cond ((eq (following-char) ?\[) nil)
	     ((search-forward "\n[" nil 0) (forward-char -1)))
       (delete-region (point) bbdb-end-marker)
       (let ((print-escape-newlines t)
	     (standard-output (current-buffer))
	     (inhibit-quit t) ; really, don't fuck with this
	     record cache)
	 (setq records bbdb-records)
	 (while records
	   (setq record (car records)
		 cache (bbdb-record-cache record))
	   (bbdb-record-set-cache record nil)
	   (prin1 (car records))
	   (bbdb-record-set-cache record cache)
	   (insert ?\n)
	   (setq records (cdr records))))
       (kill-all-local-variables)
       (error "the BBDB was mis-sorted: it has been repaired.")))))

;;; Keybindings

(fset 'advertized-bbdb-delete-current-field-or-record
      'bbdb-delete-current-field-or-record)

(if bbdb-mode-map
    nil
  (setq bbdb-mode-map (make-keymap))
  (suppress-keymap bbdb-mode-map)
  (define-key bbdb-mode-map "*"      'bbdb-apply-next-command-to-all-records)
  (define-key bbdb-mode-map "e"      'bbdb-edit-current-field)
  (define-key bbdb-mode-map "n"      'bbdb-next-record)
  (define-key bbdb-mode-map "p"      'bbdb-prev-record)
  (define-key bbdb-mode-map "d"      'advertized-bbdb-delete-current-field-or-record)
  (define-key bbdb-mode-map "\^K"    'bbdb-delete-current-field-or-record)
  (define-key bbdb-mode-map "\^O"    'bbdb-insert-new-field)
  (define-key bbdb-mode-map "s"      'bbdb-save-db)
  (define-key bbdb-mode-map "\^X\^S" 'bbdb-save-db)
  (define-key bbdb-mode-map "r"      'bbdb-refile-record)
  (define-key bbdb-mode-map "t"      'bbdb-elide-record)
  (define-key bbdb-mode-map "o"      'bbdb-omit-record)
  (define-key bbdb-mode-map ";"      'bbdb-record-edit-notes)
  (define-key bbdb-mode-map "m"      'bbdb-send-mail)
  (define-key bbdb-mode-map "\M-d"   'bbdb-dial)
  (define-key bbdb-mode-map "f"      'bbdb-finger)
  (define-key bbdb-mode-map "i"	     'bbdb-info)
  (define-key bbdb-mode-map "?"	     'bbdb-help)
  (define-key bbdb-mode-map "q"	     'bbdb-bury-buffer)
  (define-key bbdb-mode-map "\^X\^T" 'bbdb-transpose-fields)
  )

(defvar bbdbid "Insidious Big Brother Database autoload")

;; tie it all together...
;;
(autoload 'bbdb		"bbdb-com" bbdbid t)
(autoload 'bbdb-name	"bbdb-com" bbdbid t)
(autoload 'bbdb-company	"bbdb-com" bbdbid t)
(autoload 'bbdb-net	"bbdb-com" bbdbid t)
(autoload 'bbdb-notes	"bbdb-com" bbdbid t)
(autoload 'bbdb-changed	"bbdb-com" bbdbid t)
(autoload 'bbdb-create	"bbdb-com" bbdbid t)
(autoload 'bbdb-dial	"bbdb-com" bbdbid t)
(autoload 'bbdb-finger	"bbdb-com" bbdbid t)
(autoload 'bbdb-info	"bbdb-com" bbdbid t)
(autoload 'bbdb-help	"bbdb-com" bbdbid t)

(autoload 'bbdb-insinuate-vm    "bbdb-vm"    "Hook BBDB into VM")
(autoload 'bbdb-insinuate-rmail "bbdb-rmail" "Hook BBDB into RMAIL")
(autoload 'bbdb-insinuate-mh    "bbdb-mhe"   "Hook BBDB into MH-E")
(autoload 'bbdb-insinuate-gnus  "bbdb-gnus"  "Hook BBDB into GNUS")

(autoload 'bbdb-apply-next-command-to-all-records "bbdb-com" bbdbid t)

(autoload 'bbdb-insert-new-field		"bbdb-com" bbdbid t)
(autoload 'bbdb-edit-current-field		"bbdb-com" bbdbid t)
(autoload 'bbdb-transpose-fields		"bbdb-com" bbdbid t)
(autoload 'bbdb-record-edit-notes		"bbdb-com" bbdbid t)
(autoload 'bbdb-delete-current-field-or-record	"bbdb-com" bbdbid t)
(autoload 'bbdb-delete-current-record		"bbdb-com" bbdbid t)
(autoload 'bbdb-refile-record			"bbdb-com" bbdbid t)
(autoload 'bbdb-elide-record			"bbdb-com" bbdbid t)
(autoload 'bbdb-omit-record			"bbdb-com" bbdbid t)
(autoload 'bbdb-send-mail			"bbdb-com" bbdbid t)
(autoload 'bbdb-complete-name			"bbdb-com" bbdbid t)
(autoload 'bbdb-yank				"bbdb-com" bbdbid t)
(autoload 'bbdb-completion-predicate            "bbdb-com" bbdbid)
(autoload 'bbdb-dwim-net-address                "bbdb-com" bbdbid)
(autoload 'bbdb-redisplay-records		"bbdb-com" bbdbid)
(autoload 'bbdb-define-all-aliases		"bbdb-com" bbdbid)
(autoload 'bbdb-read-addresses-with-completion	"bbdb-com" bbdbid)
(autoload 'bbdb-record-edit-property		"bbdb-com" bbdbid t)

(autoload 'bbdb/vm-show-sender        "bbdb-vm"    bbdbid t)
(autoload 'bbdb/vm-annotate-sender    "bbdb-vm"    bbdbid t)
(autoload 'bbdb/vm-update-record      "bbdb-vm"    bbdbid t)
(autoload 'bbdb/rmail-show-sender     "bbdb-rmail" bbdbid t)
(autoload 'bbdb/rmail-annotate-sender "bbdb-rmail" bbdbid t)
(autoload 'bbdb/rmail-update-record   "bbdb-rmail" bbdbid t)
(autoload 'bbdb/mh-show-sender        "bbdb-mhe"   bbdbid t)
(autoload 'bbdb/mh-annotate-sender    "bbdb-mhe"   bbdbid t)
(autoload 'bbdb/mh-update-record      "bbdb-mhe"   bbdbid t)
(autoload 'bbdb/gnus-show-sender      "bbdb-gnus"  bbdbid t)
(autoload 'bbdb/gnus-annotate-sender  "bbdb-gnus"  bbdbid t)
(autoload 'bbdb/gnus-update-record    "bbdb-gnus"  bbdbid t)
(autoload 'bbdb/gnus-lines-and-from   "bbdb-gnus"  bbdbid nil)

(autoload 'bbdb-extract-field-value          "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-timestamp-hook               "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-ignore-most-messages-hook    "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-ignore-some-messages-hook    "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-auto-notes-hook              "bbdb-hooks" bbdbid nil)
(autoload 'sample-bbdb-canonicalize-net-hook "bbdb-hooks" bbdbid nil)
(autoload 'bbdb-creation-date-hook	     "bbdb-hooks" bbdbid nil)

(autoload 'bbdb-fontify-buffer	"bbdb-xemacs" bbdbid nil)
(autoload 'bbdb-menu		"bbdb-xemacs" bbdbid t)

(makunbound 'bbdbid)

;;; RMAIL, MHE, and VM interfaces might need these.
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'mail-fetch-field "mail-utils")

;;; All of the interfaces need this.
(autoload 'mail-extract-address-components "mail-extr")


(defun bbdb-insinuate-sendmail ()
  "Call this function to hook BBDB into sendmail (that is, M-x mail)."
  (define-key mail-mode-map "\M-\t" 'bbdb-complete-name)
  )


(provide 'bbdb)  ; provide before loading things which might require

(defmacro safe-require (thing)
  (list 'condition-case nil (list 'require thing) '(file-error nil)))

(if (featurep 'rmail) (safe-require 'bbdb-rmail))
(if (featurep 'gnus)  (safe-require 'bbdb-gnus))
(if (featurep 'vm)    (safe-require 'bbdb-vm))
(if (featurep 'mh-e)  (safe-require 'bbdb-mhe))

;;; elucidate
(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       (bbdb-add-hook 'bbdb-list-hook 'bbdb-fontify-buffer)
       (define-key bbdb-mode-map 'button3 'bbdb-menu)
       ))

(run-hooks 'bbdb-load-hook)
