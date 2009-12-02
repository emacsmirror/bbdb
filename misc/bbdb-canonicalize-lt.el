;;; As per email to bbdb-info list from Len Trigg <len@netvalue.net>
;;; http://sourceforge.net/mailarchive/message.php?msg_name=hbr60bfmvt.wl%25len@netvalue.net.nz

;;; Useful name canonicalizer; consider inclusion in main package.

(defun bbdb-canonicalize-name-hook-lt (name)
  "Function used to canonicalize the full names of bbdb entries."
  ;; (message (format "canonicalize name %s" name))
  (cond
   ;; strip extra quotes (Some MS mailer likes "'full name'")
   ((string-match "\\`[`'\"]\\(.*\\)[`'\"]\\'" name)
    (bbdb-match-substring name 1))
   ;; replace multiple whitespace with single
   ((string-match "[ \f\t\n\r\v]\\{2,\\}" name)
    (replace-match " " nil t name))
   ;; remove anything in round brackets, e.g.: "Firstname Surname (E-mail)"
   ((string-match "[ ]+(.*)" name)
    (replace-match "" nil t name))
   ;; strip leading whitespace (this is a bug in std11 libs?)
   ((string-match "\\`[ \t]+\\(.*\\)" name)
    (bbdb-match-substring name 1))
   ;; strip trailing whitespace
   ((string-match "\\(.*\\)[ ]+\\'" name)
    (bbdb-match-substring name 1))
   ;; strip Dr pronoun
   ((string-match "\\`Dr\\.? \\(.*\\)" name)
    (bbdb-match-substring name 1))
   ;; person and person -> person & person
   ((string-match "\\`\\(\\w+\\) and \\(\\w.+\\)\\'" name)
    (concat (bbdb-match-substring name 1) " & " (bbdb-match-substring name 2)))
   ;; Surname, Firstname -> Firstname Surname
   ((string-match "\\`\\(\\w.+\\), \\(\\w.+\\)\\'" name)
    (concat (bbdb-match-substring name 2) " " (bbdb-match-substring name 1)))
   ;; Sometimes get an email address in the name part. Map the username to a name: <Name@domain> -> Name
   ((string-match "\\`<\\(.*\\)@.*\\'" name)
    (bbdb-match-substring name 1))
   ;; replace name without any whitespace with empty; I don't want bbdb names containing only a single name
   ((string-match "\\`\\(\\w+\\)\\'" name)
    ;;(message (format "Eliding name %s" name))
    "")
   (t name)))
