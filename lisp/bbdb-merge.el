;;; BBDB merge/sync framework
;;; GNU Public License to go here. This file is under GPL, thanks guys.
;;; Copyright (c) 2000 Waider

;;; just starting off on this, may be of use to others
;;; to do:
;;; smarter phone, notes and address merging.

;;;###autoload
(defun bbdb-merge-record( firstname lastname aka nets addrs phones company
									notes date override)
  "Generic merge function.

Merges FIRSTNAME LASTNAME AKA NETS ADDRS PHONES COMPANY NOTES into
your bbdb, using DATE to check who's more up-to-date and OVERRIDE to
decide who gets precedence if two dates match. DATE can be extracted
from a notes if it's an alist with an element marked timestamp. Set
OVERRIDE to 'new to allow the new record to stomp on existing data,
'old to preserve existing data or nil to merge both together . If it
can't find a record to merge with, it will create a new record.

Returns the Grand Unified Record."

  (let ((name (bbdb-string-trim (concat firstname " " lastname)))
		olddate newrecord)

	;; data dinking. this allows some of the parameters to be
	;; strings/vectors OR lists, saving on the world's supply of
	;; brackets.
	;; for doubleplus bonus points, I should parse the
	;; wish-you-were-vectors from strings, too.
	(and (stringp aka)
		 (setq aka (list aka)))

	(and (stringp nets)
		 (setq nets (list nets)))

	(and (vectorp addrs)
		 (setq addrs (list addrs)))

	(and (vectorp phones)
		 (setq phones (list phones)))

	;; See if we have a record that looks right
	(if (setq newrecord (bbdb-search-simple name nets))
		(progn
		  ;; If date is unset, see if there's a datestamp in the notes field
		  (if (null date)
			  (if (listp notes)
				  (setq date (assq 'timestamp notes))))

		  ;; if date is still unset, set it to the existing record's date.
		  (setq olddate (bbdb-record-getprop newrecord 'timestamp)
				date (or date olddate))

		  ;; if the old record is actually newer, invert the sense of override
		  (if (string-lessp olddate date)
			  (setq override (cond ((eq 'old) 'new)
								   ((eq 'new) 'old)
								   (t nil))))

		  (bbdb-record-set-firstname newrecord
		   (if (null override)
			   (bbdb-merge-strings (bbdb-record-firstname newrecord) firstname " ")
			 (if (eq 'new override) firstname
			   (bbdb-record-firstname newrecord))))

		  (bbdb-record-set-lastname newrecord
		   (if (null override)
			   (bbdb-merge-strings (bbdb-record-lastname newrecord) lastname " ")
			 (if (eq 'new override) lastname
			   (bbdb-record-lastname newrecord))))

		  (bbdb-record-set-company newrecord
		   (if (null override)
			   (bbdb-merge-strings (bbdb-record-company newrecord)
								   company " ")
			 (if (eq 'new override) company
			   (bbdb-record-company newrecord))))

		  (bbdb-record-set-aka 
		   newrecord
		   (if (null override)
			   (bbdb-merge-lists (bbdb-record-aka newrecord) aka)
			 (if (eq 'new override) aka
			   (bbdb-record-aka newrecord))))

		  (bbdb-record-set-net
		   newrecord
		   (if (null override)
			   (bbdb-merge-lists (bbdb-record-net newrecord) nets)
			 (if (eq 'new override) nets
			   (bbdb-record-net newrecord))))

		  (bbdb-record-set-phones
		   newrecord
		   (if (null override)
			   (bbdb-merge-lists (bbdb-record-phones newrecord) phones)
			 (if (eq 'new override) phones
			   (bbdb-record-phones newrecord))))

		  (bbdb-record-set-addresses
		   newrecord
		   (if (null override)
			   (bbdb-merge-lists (bbdb-record-addresses newrecord) addrs)
			 (if (eq 'new override) addrs
			   (bbdb-record-addresses newrecord))))

		  (bbdb-record-set-raw-notes
		   newrecord
		   (if (null override)
			   (bbdb-merge-lists (bbdb-record-raw-notes newrecord) notes)
			 (if (eq 'new override) notes
			   (bbdb-record-raw-notes newrecord)))))
							   
		  
	  ;; we couldn't find a record, so create one
	  (setq newrecord (bbdb-create-internal name company nets addrs phones
											notes))
	  ;; bite me, bbdb-create-internal
	  (bbdb-record-set-firstname firstname)
	  (bbdb-record-set-lastname lastname))

	;; your record, sir.
	newrecord))

;; fixme this could be a macro, I guess.
(defun bbdb-merge-strings( s1 s2 &optional sep )
  "Merge two strings together uniquely. If s1 doesn't contain s2, return s1+sep+s2."
  (if (string-match s2 s1)
	  s1
	(concat s1 (or sep "") s2))) 

;; fixme this too could be a macro
(defun bbdb-merge-lists( l1 l2 )
  "Merge two lists without duplication"
  (append l1
   (delete nil 
		   (mapcar (function (lambda( e ) 
							   (if (member e l1) 
								   nil
								 e))) l2))))

(provide 'bbdb-merge)