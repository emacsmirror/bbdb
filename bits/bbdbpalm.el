;;; bbdbpalm.el -- BBDBpalm exporter of BBDB database to Palm(R) address book

;; Copyright (C) 1999,2002,2006 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <neil@neilvandyke.org>
;; Version:  0.3
;; X-URL:    http://www.neilvandyke.org/bbdbpalm/

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

;;; Commentary:

;; ABOUT:
;;
;;     BBDBpalm exports your BBDB address database to your Palm/USR/3Com
;;     Pilot/PalmPilot/Palm-Connected-Organizer (hereinafter simply "Palm").
;;     BBDB is a sort of electronic address book written by Jamie Zawinsky
;;     <jwz@jwz.org> that integrates nicely with Emacs-based E-mail and Usenet
;;     clients.  BBDBpalm lets you bring that address book with you if you
;;     don't have the luxury of 24/7 network access to a remote Emacs session
;;     from your Palm.
;;
;;     The Web page is `http://www.neilvandyke.org/bbdbpalm/'.
;;
;;     This package is no longer being maintained.

;; REQUIREMENTS:
;;
;;     BBDBpalm was developed with BBDB 1.51, which is available at
;;     `http://www.jwz.org/bbdb/' if you don't already have it.  It has also
;;     been reported to work with BBDB 2.00.02.
;;
;;     BBDBpalm uses the `pilot-addresses' program, which is part of the
;;     Pilot-Link package.  Pilot-Link is freely available on the 'net, and is
;;     included with some GNU/Linux distributions.  The master sources are
;;     available at `ftp://ryeham.ee.ryerson.ca/pub/PalmOS/', although you may
;;     wish to instead find a pre-compiled distribution for your operating
;;     system.  BBDBpalm was tested with version 0.9.0.
;;
;;     BBDBpalm was developed under GNU Emacs 20.3 running atop the GNU/Linux
;;     platform.  It will probably work with Emacs 19 and has been reported to
;;     work under XEmacs 21.2b17.  It will probably work under Emacs on other
;;     Unix variants.  Please let me know if you encounter any problems with
;;     other Emacsen or Unixen.

;; INSTALLATION:
;;
;;     If you're Emacs-savvy enough to be using BBDB, then you probably don't
;;     need much install instructions.  The only three non-obvious things you
;;     need to do are:
;;
;;       1. On your Palm, add an Address List category called "BBDB", in all
;;          caps.  (Reason: Unfortunately, `pilot-addresses' does not presently
;;          create categories on your Palm, or overwrite duplicate entries.
;;          So, every time we upload to the Palm, we blast everything in the
;;          "BBDB" category and put all the BBDB-exported records into that
;;          category.)  If you change entries on the Palm side of things,
;;          change their category so that you remember to manually propagate
;;          the change to BBDB.
;;
;;       2. If you don't want to export everything in your BBDB to the Palm,
;;          then do both of:
;;
;;            a. Put the following your `.emacs' file or wherever you put your
;;               Emacs customizations:
;;
;;                   (setq bbdbpalm-export-all-p nil)
;;
;;            b. Add a field called `palm' to each BBDB record that you wish to
;;               export (by pressing `C-o' in the `*BBDB*' buffer with the
;;               point on the desired record).  Give each `palm' field a value
;;               of `yes' for now.  Note that a later version of BBDBpalm might
;;               put something else in that field.
;;
;;       3. If you wish to have titles for people be exported to the Palm, add
;;          a field called `title' to BBDB and use it.

;; ALTERNATIVES:
;;
;;     Tom Fawcett <fawcett@basit.com> wrote `bbdb-pilot', which is available
;;     at `ftp://ftp.croftj.net/usr/fawcett/bbdb-pilot.el'.  The version I
;;     found, dated 1998, didn't seem to do what I wanted (I'm so picky) but
;;     you may prefer it.
;;
;;     Neale Picket <zephyr@roguetrader.com> hacked up a small convertor in
;;     Feb-1998.  See `http://acm.rpi.edu/~albert/pilot/Feb98/0039.html'.

;; THINGS TO DO:
;;
;;     * Add support for custom fields.
;;
;;     * Add special support for `Web' custom field.
;;
;;     * Maybe someday do bidirectional sync-ing.  For now, users should just
;;       keep all the BBDB-exported records in the `BBDB' category on their
;;       Palm, and manually change those records on the BBDB end rather than on
;;       the Palm end.
;;
;;     * Maybe I should make it fix certain family names that BBDB has
;;       mis-parsed, such as, well... I dunno... how about... "Van Dyke"?
;;       Better yet, I should move to BBDB 2.x and make sure it's fixed there.
;;
;;     * Make it be smarter about which address it picks if there are multiple
;;       ones.
;;
;;     * Add BBDB extension so that it prompts you for `palm' field.
;;
;;     * Make it reformat phone numbers.
;;
;;     * Add completion-percentage indicator for file-exporting and uploading.
;;
;;     * Check for error from `pilot-addresses' and maybe do something with it.
;;
;;     * ``I think it would be nice if the order of phone numbers as they
;;       appear in the BBDB was preserved in the Pilot. Same thing for multiple
;;       email addresses; they are now listed in reverse order.''  [Mark Moll
;;       <mmoll@cs.cmu.edu>, 24-Jun-1999]
;;
;;     * ``Maybe it's a good idea to put in a message "Please press the HotSync
;;       button" at the appropiate time.''  [Mark Moll <mmoll@cs.cmu.edu>,
;;       24-Jun-1999]

;;; Change Log:

;; [Version 0.3, 2006-11-12] Made `bbdbpalm-format-record' work with newer BBDB
;; versions by use of `bbdb-address-streets'.  Thanks to Christoph Conrad for
;; the patch.  Note that I do not have access to a Palm and cannot test this
;; myself.
;;
;; [Version 0.2, 2002-10-15] I no longer have access to a Palm, so I am not
;; maintaining this package.  This release is a snapshot of my last modified
;; version, which fixes a typo in jwz's name (writes one observant BBDBpalm
;; user: ``jwz's spelling of his last name differs a bit from yours.  If
;; "Zawinksy" an in-joke, it's probably funny.''), and updates my email
;; address.
;;
;; [Version 0.1, 1999-06-23] Initial release.

;;; Code:

(defconst bbdbpalm-name    "BBDBpalm")
(defconst bbdbpalm-version "0.3")

;; Package Dependencies:

(require 'bbdb)
(require 'cl)

;; Options:

(defvar bbdbpalm-category
  "BBDB"
  "*Name of the category under which the exported address records are to be
filed.  Note that all addresses in this category will be removed from the Palm
when the new addresses are uploaded by `pilot-addresses'.")

(defvar bbdbpalm-export-all-p
  t
  "*If non-nil, export all records, rather than exporting only those records
that that have a `palm' field.")

(defvar bbdbpalm-export-file
  (expand-file-name ".bbdbpalm-export" "~")
  "*Filename of the file into which BBDB-Palm puts the exported address data
for `pilot-addresses'.")

(defvar bbdbpalm-leave-work-field-p
  t
  "*If non-nil, never put anything in the first contact field (which defaults
to the Work phone number) except Work.")

(defvar bbdbpalm-pilot-addresses-program
  "pilot-addresses"
  "*Command to invoke the `pilot-addresses' program.  The program should either
be in the executable search path, or this variable should be set to a
fully-qualified pathname to the program file.")

;; Constants:

(defconst bbdbpalm-contactcode-strings
  '((email  . "E-Mail")
    (fax    . "Fax")
    (home   . "Home")
    (mail   . "Main")
    (mobile . "Mobile")
    (other  . "Other")
    (pager  . "Pager")
    (work   . "Work")))

(defconst bbdbpalm-octal-700 448)

;; Macros:

(defmacro bbdbpalm-assq-del (key alist)
  (assert (symbolp alist))
  (let ((cell     (gensym))
        (eval-key (gensym))
        (head     (gensym))
        (lasthead (gensym)))
    `(let ((,eval-key ,key)
           (,cell     nil)
           (,head     ,alist)
           (,lasthead nil))
       (while (and ,head (not ,cell))
         (setq ,cell (car ,head))
         (if (eq (car ,cell) ,eval-key)
             (if ,lasthead
                 (setcdr ,lasthead (cdr ,head))
               (setq ,alist (cdr ,head)))
           (setq ,cell nil)
           (setq ,lasthead ,head)
           (setq ,head (cdr ,head))))
       ,cell)))

(defmacro bbdbpalm-log-activity (what &rest body)
  ;; Note: This function was adapted from `jomtool-log-activity' in Neil's
  ;;       Jomtool package.
  (let ((eval-what (gensym)))
    `(let ((,eval-what ,what))
       (bbdbpalm-log (concat ,eval-what "..."))
       (prog1 (progn ,@body)
         (bbdbpalm-log (concat ,eval-what "...done"))))))

;; Functions:

(defun bbdbpalm ()
  (interactive)
  (bbdbpalm-log-activity
   "Exporting BBDB data to the Palm"
   (bbdbpalm-export-to-file bbdbpalm-export-file)
   (bbdbpalm-upload-export-file bbdbpalm-export-file)))

(defun bbdbpalm-contactcode-string (contactcode)
  (cdr (assq contactcode bbdbpalm-contactcode-strings)))

(defun bbdbpalm-export-to-file (export-file)
  (let (buf
        old-default-file-modes
        record)
    (bbdbpalm-log-activity
     (format "Exporting to file \"%s\"" export-file)
     (unwind-protect
         (progn
           ;; Make sure created files are only readable by user.
           (setq old-default-file-modes (default-file-modes))
           (set-default-file-modes bbdbpalm-octal-700)
           ;; Find the file and empty it.
           (setq buf (find-file-noselect export-file))
           (set-buffer buf)
           (goto-char (point-min))
           (delete-region (point-min) (point-max))
           ;; Write the records.
           (mapc (function
                  (lambda (record)
                    (let ((notes (bbdb-record-raw-notes record)))
                      (if (or bbdbpalm-export-all-p
                              (and (listp notes) (assq 'palm notes)))
                          (insert (bbdbpalm-format-record record))))))
                 (bbdb-records))
           ;; Save the file and get rid of the buffer.
           (save-buffer buf)
           (kill-buffer buf))
       ;; unwind-protect cleanup: Restore default-file-modes.
       (set-default-file-modes old-default-file-modes)))))

(defun bbdbpalm-format-contact-field (contact-field)
  (if contact-field
      (list (bbdbpalm-contactcode-string (car contact-field))
            (cdr contact-field))
    nil))
  
(defun bbdbpalm-format-field (field)
  (cond ((null field)    "\"\"")
        ((stringp field) (bbdbpalm-format-field-string field))
        ((listp field)   (mapconcat 'bbdbpalm-format-field-string
                                    field
                                    ";"))
        (t               (error "Can't handle type of this field: "
                                field))))

(defun bbdbpalm-format-field-list (list)
  (concat (mapconcat 'bbdbpalm-format-field
                     list
                     ",")
          "\n"))

(defun bbdbpalm-format-field-string (field)
  (if field
      ;; Note: This is a grossly slow way to do it.
      (concat "\""
              (mapconcat (function
                          (lambda (c)
                            (cond ((= c 34) "\"\"")
                                  ((and (> c 31) (< c 128)) (char-to-string c))
                                  ((= c 9) "\\t")
                                  ((= c 10) "\\n")
                                  (t ""))))
                         field
                         "")
              "\"")
    "\"\""))

(defun bbdbpalm-format-record (record)
  (let ((city           nil)
        (contact-fields nil)
        (country        nil)
        (custom-1       nil)
        (custom-2       nil)
        (custom-3       nil)
        (custom-4       nil)
        (group          nil)
        (show-field     nil)
        (state          nil)
        (street         nil)
        (title          nil)
        (zip            nil))

    ;; Prepare the contact fields.
    (let ((contact-cands '()))

      ;; Add phone numbers to contact-cands.
      (mapcar
       (function
        (lambda (phone)
          (let ((code (bbdbpalm-location-to-contactcode
                       (bbdb-phone-location phone))))
            (if code
                (setq contact-cands
                      (nconc contact-cands
                             (list (cons code
                                         (bbdb-phone-string phone)))))))))
       (bbdb-record-phones record))

      ;; Add E-mail addresses to contact-cands (note that we want these
      ;; added after the phone numbers, so that phone numbers get higher
      ;; priority when we're filling up extra contact fields).
      (mapcar (function (lambda (net)
                          (setq contact-cands
                                (nconc contact-cands
                                       (list (cons 'email net))))))
              (bbdb-record-net record))

      ;; Set the contact fields, giving preference to one of each and to the
      ;; Palm default ordering.  Fill the remaining empty contact fields with
      ;; other contact info.
      (setq contact-fields (list (bbdbpalm-assq-del 'work  contact-cands)
                                 (bbdbpalm-assq-del 'home  contact-cands)
                                 (bbdbpalm-assq-del 'fax   contact-cands)
                                 (bbdbpalm-assq-del 'other contact-cands)
                                 (bbdbpalm-assq-del 'email contact-cands)))
      (let ((probe contact-fields))
        (if bbdbpalm-leave-work-field-p
            (setq probe (cdr probe)))
        (while (and probe contact-cands)
          (if (not (car probe))
              (progn
                (setcar probe (car contact-cands))
                (setq contact-cands (cdr contact-cands))))
          (setq probe (cdr probe))))
      
      ;; Set show-field.
      (setq show-field (if (and (not (assq 'work contact-fields))
                                (assq 'home contact-fields))
                           "Home"
                         "Work")))
    
    ;; Prepare address fields.
    (let ((addr  nil)
          (addrs (bbdb-record-addresses record)))
      (setq addr (car addrs))
      (if addr
          (setq street  (mapconcat
                         'identity
                         (delq nil
                               (mapcar (function
                                        (lambda (s)
                                          (if (= (length s) 0) nil s)))
                                       (bbdb-address-streets addr)
                                       ;; Note: Old code.  Replaced by above
                                       ;; line for newer BBDB.
                                       ;;
                                       ;; (list (bbdb-address-street1 addr)
                                       ;;       (bbdb-address-street2 addr)
                                       ;;       (bbdb-address-street3 addr))
                                       ))
                         ", ")
                city    (bbdb-address-city addr)
                state   (bbdb-address-state addr)
                zip     (bbdb-address-zip-string addr)
                country nil)))

    ;; Get information from the raw notes fields.
    (mapcar (function (lambda (field)
                        (if (consp field)
                            (case (car field)
                              ('title (setq title (cdr field)))
                              ('group (setq group (cdr field)))))))
            (bbdb-record-raw-notes record))

    ;; Return the formatted record.
    (bbdbpalm-format-field-list
     (list
      ;;   1. <FileCategory> SEMI <DisplayField> SEMI <LastName>
      ;;      "Unfiled";"Fax";"ALastName",
      (list bbdbpalm-category show-field (bbdb-record-lastname record))
      ;;   2. <FirstName>
      ;;      "AFirstName",
      (bbdb-record-firstname record)
      ;;   3. <Title>
      ;;      "ATitle",
      title
      ;;   3. <Company>
      ;;      "ACompany",
      (bbdb-record-company record)
      ;; 4-8. ( <ContactAttr> SEMI <ContactValue> ) | ( <emptystring> )
      ;;      "","","Fax";"zFax","Other";"zOther, with ""quotes""",
      ;;      "E-mail";"zEmail",
      (bbdbpalm-format-contact-field (nth 0 contact-fields))
      (bbdbpalm-format-contact-field (nth 1 contact-fields))
      (bbdbpalm-format-contact-field (nth 2 contact-fields))
      (bbdbpalm-format-contact-field (nth 3 contact-fields))
      (bbdbpalm-format-contact-field (nth 4 contact-fields))
      ;;   9. <Street>
      ;;      "zAddress",
      street
      ;;  10. <City>
      ;;      "zCity",
      city
      ;;  11. "zState",
      state
      ;;  12. "zZip",
      zip
      ;;  13. "zCountry",
      country
      ;;  14. "zGroup",
      custom-1
      ;;  15. "zWeb",
      custom-2
      ;;  16. "zIrc",
      custom-3
      ;;  17. "zC4",
      custom-4
      ;;  18. <Notes>
      ;;      "",
      (bbdb-record-notes record)
      ;;  19. <Unknown2>
      ;;      "0"
      "0"))))

(defun bbdbpalm-location-to-contactcode (loc)
  (let ((s (assoc (downcase loc) 
                  '(("cell"    . mobile)
                    ("fax"     . fax)
                    ("home"    . home)
                    ("main"    . main)
                    ("mobile"  . mobile)
                    ("office"  . work)
                    ("pad"     . home)
                    ("work"    . work)))))
    (if s (cdr s) 'other)))

(defun bbdbpalm-log (format &rest args)
  (apply 'message (concat bbdbpalm-name ": " format) args))

(defun bbdbpalm-upload-export-file (export-file)
  (bbdbpalm-log-activity
   (format "Uploading file \"%s\" to Palm" export-file)
   (save-excursion
     (save-window-excursion
       (let ((buf (get-buffer-create "*BBDBpalm*")))
         (set-buffer buf)
         (setq buffer-read-only nil)
         (delete-region (point-min) (point-max))
         (insert bbdbpalm-name
                 " will now run \""
                 bbdbpalm-pilot-addresses-program
                 "\" to upload the data to your Palm.\n\n")
         (or (getenv "PILOTRATE")
             (insert "You may be able to speed up uploads by setting your"
                     " \"PILOTRATE\" environment\n"
                     "variable. See your Pilot-Link documentation for"
                     " details.\n\n"))
         (pop-to-buffer buf)
         (call-process bbdbpalm-pilot-addresses-program
                       nil buf t
                       "-d" bbdbpalm-category
                       "-r" bbdbpalm-export-file)
         (setq buffer-read-only t))))))

(provide 'bbdbpalm)

;;; bbdbpalm.el ends here
