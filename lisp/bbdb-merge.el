;;; BBDB merge/sync framework
;;; GNU Public License to go here. This file is under GPL, thanks guys.
;;; Copyright (c) 2000 Waider

(require 'bbdb)
(require 'bbdb-com)
;;; just starting off on this, may be of use to others
;;; to do:
;;; smarter phone, notes and address merging.

;;;###autoload
(defun bbdb-merge-record( firstname lastname aka nets addrs phones company
                                    notes date override &optional merge-record )
  "Generic merge function.

Merges FIRSTNAME LASTNAME AKA NETS ADDRS PHONES COMPANY NOTES into
your bbdb, using DATE to check who's more up-to-date and OVERRIDE to
decide who gets precedence if two dates match. DATE can be extracted
from a notes if it's an alist with an element marked timestamp. Set
OVERRIDE to 'new to allow the new record to stomp on existing data,
'old to preserve existing data or nil to merge both together . If it
can't find a record to merge with, it will create a new record. If
MERGE-RECORD is set, it's a record discovered by other means that should
be merged with.

Returns the Grand Unified Record."

  (let ((name (bbdb-string-trim (concat firstname " " lastname)))
        olddate)

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

    (if (stringp notes)
        (setq notes (list (cons 'notes notes))))

    ;; See if we have a record that looks right, using an intertwingle
    ;; search. Could probably parameterize
    ;; that. bbdb-merge-search-function or some such.
    (if (null merge-record)
        (setq merge-record (bbdb-search-simple name nets)))

    (if merge-record
        (progn
          ;; If date is unset, see if there's a datestamp in the notes field
          (if (null date)
              (if (listp notes)
                  (setq date (cdr (assq 'timestamp notes)))))

          ;; if date is still unset, set it to the existing record's date.
          (setq olddate (bbdb-record-getprop merge-record 'timestamp)
                date (or date olddate))

          ;; if the old record is actually newer, invert the sense of override
          (if (string-lessp olddate date)
              (setq override (cond ((eq 'old override) 'new)
                                   ((eq 'new override) 'old)
                                   (t nil))))

          (bbdb-record-set-firstname merge-record
           (if (null override)
               (bbdb-merge-strings (bbdb-record-firstname merge-record) firstname " ")
             (if (eq 'new override) firstname
               (bbdb-record-firstname merge-record))))

          (bbdb-record-set-lastname merge-record
           (if (null override)
               (bbdb-merge-strings (bbdb-record-lastname merge-record) lastname " ")
             (if (eq 'new override) lastname
               (bbdb-record-lastname merge-record))))

          (bbdb-record-set-company merge-record
           (if (null override)
               (bbdb-merge-strings (bbdb-record-company merge-record)
                                   company " ")
             (if (eq 'new override) company
               (bbdb-record-company merge-record))))

          (bbdb-record-set-aka
           merge-record
           (if (null override)
               (bbdb-merge-lists (bbdb-record-aka merge-record) aka)
             (if (eq 'new override) aka
               (bbdb-record-aka merge-record))))

          (bbdb-record-set-net
           merge-record
           (if (null override)
               (bbdb-merge-lists (bbdb-record-net merge-record) nets)
             (if (eq 'new override) nets
               (bbdb-record-net merge-record))))

          (bbdb-record-set-phones
           merge-record
           (if (null override)
               (bbdb-merge-lists (bbdb-record-phones merge-record) phones)
             (if (eq 'new override) phones
               (bbdb-record-phones merge-record))))

          (bbdb-record-set-addresses
           merge-record
           (if (null override)
               (bbdb-merge-lists (bbdb-record-addresses merge-record) addrs)
             (if (eq 'new override) addrs
               (bbdb-record-addresses merge-record))))

          ;; lifted from bbdb-com.el
          (let ((n1 (bbdb-record-raw-notes merge-record))
                (n2 notes)
                tmp)
            (or (equal n1 n2)
                (progn
                  (or (listp n1) (setq n1 (list (cons 'notes n1))))
                  (or (listp n2) (setq n2 (list (cons 'notes n2))))
                  (while n2
                    (if (setq tmp (assq (car (car n2)) n1))
                        (setcdr tmp
                                (funcall (or (cdr (assq (car (car n2))
                                                        bbdb-refile-notes-generate-alist))
                                             bbdb-refile-notes-default-merge-function)
                                         (cdr tmp) (cdr (car n2))))
                      (setq n1 (nconc n1 (list (car n2)))))
                    (setq n2 (cdr n2)))
                  (bbdb-record-set-raw-notes merge-record n1)))))

      ;; we couldn't find a record, so create one
      (setq merge-record (bbdb-create-internal name company nets addrs phones
                                               notes))
      ;; bite me, bbdb-create-internal
      (bbdb-record-set-firstname merge-record firstname)
      (bbdb-record-set-lastname merge-record lastname))

    ;; more general bitingness
    (if (equal (bbdb-record-firstname merge-record) "")
        (bbdb-record-set-firstname merge-record nil))
    (if (equal (bbdb-record-lastname merge-record) "")
        (bbdb-record-set-lastname merge-record nil))

    ;; fix up the in-memory copy. which doesn't appear to actually work.
    ;;(bbdb-change-record merge-record t)
    ;;(bbdb-with-db-buffer
    ;;(if (not (memq merge-record bbdb-changed-records))
    ;;(setq bbdb-changed-records
    ;;(cons merge-record bbdb-changed-records))))

    ;; your record, sir.
    merge-record))

;; fixme this could be a macro, I guess.
(defun bbdb-merge-strings( s1 s2 &optional sep )
  "Merge two strings together uniquely. If s1 doesn't contain s2, return s1+sep+s2."
  (cond ((or (null s1)
             (string-equal s1 ""))
         s2)
        ((or (null s2)
             (string-equal s2 ""))
         s1)
        (t
         (if (string-match s2 s1)
             s1
           (concat s1 (or sep "") s2)))))

;; fixme this too could be a macro
(defun bbdb-merge-lists( l1 l2 )
  "Merge two lists without duplication."
  (append l1
   (delete nil
           (mapcar (function (lambda( e )
                               (if (member e l1)
                                   nil
                                 e))) l2))))

;;;###autoload
(defun bbdb-merge-file( &optional bbdb-new override match-fun)
  "Merge a bbdb file into the in-core bbdb."
  (interactive "fMerge bbdb file: ")
  (message "Merging %s" bbdb-new)
  ;; argh urgle private environment
  (let* ((bbdb-live-file bbdb-file) (bbdb-file bbdb-new)
         (bbdb-live-buffer-name bbdb-buffer-name)
         (bbdb-buffer-name "*BBDB-merge*")
         (new-records (bbdb-records))
         (bbdb-buffer nil) ;; hack hack
         (bbdb-file bbdb-live-file)
         (bbdb-buffer-name bbdb-live-buffer-name)
         (live-records (bbdb-records)))

    ;; merge everything
    (mapcar (function (lambda(r)
                        (bbdb-merge-record (bbdb-record-firstname r)
                                           (bbdb-record-lastname r)
                                           (bbdb-record-aka r)
                                           (bbdb-record-net r)
                                           (bbdb-record-addresses r)
                                           (bbdb-record-phones r)
                                           (bbdb-record-company r)
                                           (bbdb-record-raw-notes r)
                                           override
                                           (if match-fun
                                               (funcall match-fun r)
                                             nil)))) new-records))

  ;; flush bbdb's internals. urgh. This forces bbdb-records to reread
  ;; the database on the next call.
  (bbdb-with-db-buffer
   (setq bbdb-records nil))
  (setq bbdb-buffer nil))

(provide 'bbdb-merge)
