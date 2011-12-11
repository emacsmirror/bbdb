;;; bbdb-spell.el --- export bbdb record to spell dictionary

;;; Commentary:

;; This is version 1.1

;; This code will export your bbdb entries to your spell personnal
;; dictionary. To run the export run the following command
;;
;; M-x bbdb-spell-export
;;

;;; THANKS:
;;
;; Roland Winkler for constructive criticism

;;; BUGS:
;;
;; Save your personal directory before running this program. I had my
;; dictionary truncated while debugging. It shouldn't happen but
;; better be safe than sorry...

;;; INSTALLATION:

;; Of course you need to have bbdb installed.
;; Put this file in your path an have the following in your .emacs:
;;
;; (require 'bbdb-spell)
;;
;; Alternatively you can use autoload.

;;; Code:

(require 'ispell)
(require 'bbdb-com)

(defcustom bbdb-spell-dictionary '("american" "french")
  "List of spell personal dictionaries."
  :group 'bbdb-spell
  :type '(list string))

(defcustom bbdb-spell-field '(name organization aka address)
  "List of field that will be added to the dictionary.
Other possible values are firstname lastname affix mail phone and
note."
  :group 'bbdb-spell
  :type '(list))

(defcustom bbdb-spell-min-word-length 3
  "Minimal word length to be inserted.
Anything smaller than 3 is not a good idea."
  :group 'bbdb-spell
  :type 'number)

(defcustom bbdb-spell-filter "[0-9]"
  "Word matching this regexp will not be inserted"
  :group 'bbdb-spell
  :type 'regexp)

;; global
(defvar bbdb-spell-word-list nil)

;;;###autoload
(defun bbdb-spell-export ()
  "Go through all bbdb record and insert fields in spell personal
dictionary."
  (interactive)
  (setq bbdb-spell-word-list nil)
  (save-window-excursion
    (dolist (record (bbdb-record-list (bbdb-search (bbdb-records) "" nil nil nil nil nil)))
      (dolist (field bbdb-spell-field)
        (bbdb-spell-export-field (bbdb-record-get-field record field)))))
  (mapc (lambda (lang)
          (bbdb-spell-add-word lang))
        bbdb-spell-dictionary)
  (message "Export to personal dictionary done."))

(defun bbdb-spell-export-field (field)
  "Parse a bbdb field and extract words."
  (if (stringp field)
      (bbdb-spell-append-word field)
    (let ((el (car field)))
      (cond ((null el))
            ;; turn vector into a list
            ((vectorp el)
             (bbdb-spell-export-field (append el nil)))
            ;; handle nested list
            ((listp el)
             (bbdb-spell-export-field el)
             (bbdb-spell-export-field (cdr field)))
            ((stringp el)
             (bbdb-spell-append-word el)
             (bbdb-spell-export-field (cdr field)))
            (t
             (bbdb-spell-export-field (cdr field)))))))

(defun bbdb-spell-append-word (word)
  "Add words in global `bbdb-spell-word-list'."
  (mapc
   (lambda (split)
     (when (and (>= (length split) bbdb-spell-min-word-length)
                (not (string-match bbdb-spell-filter split)))
       (setq bbdb-spell-word-list
             (cons split bbdb-spell-word-list))))
       (split-string word)))

(defun bbdb-spell-add-word (language)
  "Add words to the personal dictionary.
Known words will just be ignored. List of words are in the global
`bbdb-spell-word-list'."
  (ispell-change-dictionary language)
  ;; Initialize variables and dicts alists
  (ispell-set-spellchecker-params)
  ;; use the correct dictionary
  (ispell-accept-buffer-local-defs)
  (ispell-init-process)
  ;; put in verbose mode
  (ispell-send-string "%\n")
  (dolist (word bbdb-spell-word-list)
    (ispell-send-string (concat "^" word "\n"))
    (while (progn
             (ispell-accept-output)
             (not (string= "" (car ispell-filter)))))
    ;; remove extra \n
    (setq ispell-filter (cdr ispell-filter))
    (when (and ispell-filter
               (listp ispell-filter)
               (not (eq (ispell-parse-output (car ispell-filter)) t)))
      ;; ok the word doesn't exist, add it
      (ispell-send-string (concat "*" word "\n"))))
  ;; save dictionary
  (ispell-send-string "#\n")
  ;; wait for process to flush, is there a better way?
  (ispell-accept-output 1)
  (ispell-kill-ispell))

(provide 'bbdb-spell)

;; Copyright (C) 2011 Ivan Kanis
;; Author: Ivan Kanis
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;; vi:et:sw=4:ts=4:
;; Local Variables:
;; compile-command: "make"
;; End:
