;;; gnus-bbdb.el - gnus functions which utilize bbdb data 
;;; $Id$ 
;;; We require rfc822.el here as it handles comma-separated lists of 
;;; addresses, which is needed for gnus-bbdb-split-method. If someone 
;;; knows of a better way to handle this, let me know. 
(require 'bbdb-gnus)
(require 'rfc822)
(require 'message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Finding canonical author data in the BBDB 
;;; 
;;; This feature is largely lifted from the original bbdb-gnus.el by 
;;; JWZ, both in spirit and in code. Variable names have been changed 
;;; to protect the innocent. Any resemblance to real code executing or 
;;; swapped is probably because you've already loaded this. 
;;; 
;;; To use this feature, you need to put this file somewhere on your 
;;; load-path, and the following line into your .gnus file: 
;;; 
;;; (require 'gnus-bbdb) (gnus-bbdb-insinuate-summary-buffer) 
;;;

;;; If you want to change gnus-summary-line-format, do this before 
;;; calling the insinuation function, as it modifies this variable. You 
;;; should also check out the variables defined below for a bit more 
;;; customization you can do. 
(defvar gnus-bbdb-summary-mark-known-posters t "If t, then prefix the names of authors who appear in the BBDB with an identifying mark.")
(defvar gnus-bbdb-summary-known-poster-mark "+" "This is the default character to prefix author names with if gnus-bbdb-summary-mark-known-posters is t. If the poster's record has an entry in the field named by bbdb-message-marker-field, then that will be used instead.") 
(defvar gnus-bbdb-summary-prefer-bbdb-data t "If t, then for
posters who are in our BBDB, replace the information provided in the From
header with data from the BBDB.") 
(defvar gnus-bbdb-summary-prefer-real-names t "If t, then display the poster's name
from the BBDB if we have one, otherwise display his/her primary net address
if we have one. If it is set to the symbol bbdb, then real names will be
used from the BBDB if present, otherwise the net address in the post will be
used. If gnus-bbdb-summary-prefer-bbdb-data is nil, then this has no
effect.")
(defvar gnus-bbdb-summary-user-format-letter "B" "This is the
gnus-user-format-function- that will be used to insert the information from
the BBDB in the summary buffer. Unless you've alread got other code using
user format B, you might as well stick with the default.") 
(defun gnus-bbdb-insinuate-summary-buffer nil "Patch some gnus internals to allow BBDB information to infect the summary buffer. You should call this *after*
you make any personal changes to gnus-summary-line-format, as it will tweak
that a little." 
  (let (gslf i (fold case-fold-search)) 
	(if case-fold-search
		(setq case-fold-search nil))
	(while (setq gslf gnus-summary-line-format i
				 (string-match "\\(%[-0-9,]*\\)[An]" gslf))
	  (setq gnus-summary-line-format
			(concat (substring gslf 0 i)
					(substring gslf (match-beginning 1)
							   (match-end 1)) "u" 
							   gnus-bbdb-summary-user-format-letter 
							   (substring gslf (match-end 0)))))
	(or (equal fold case-fold-search) (setq case-fold-search fold)))
  (eval (read (concat "(defun gnus-user-format-function-"
					  gnus-bbdb-summary-user-format-letter " (header) (gnus-bbdb-summary-get-author header))")))) 

;;; This code appears to have come originally from bbdb-gnus.el, then 
;;; was hacked by Sudish Joseph ,then hacked 
;;; further by Brian Edmonds . 
(defun gnus-bbdb-summary-get-author (header) 
  "Given a GNUS message header, returns
the appropriate piece of information to identify the author in a GNUS
summary line, depending on the settings of the various configuration
variables. See the documentation for the following variables for more
details: gnus-bbdb-summary-mark-known-posters
gnus-bbdb-summary-known-poster-mark gnus-bbdb-summary-prefer-bbdb-data
gnus-bbdb-summary-prefer-real-names" 
  (let* ((from (mail-header-from header))
		 (data (and (or gnus-bbdb-summary-mark-known-posters
						gnus-bbdb-summary-show-bbdb-names) 
					(condition-case ()
						(mail-extract-address-components from) 
					  (error nil)))) 
		 (name (car data)) 
		 (net (car (cdr data)))
		 (record (and data 
					  (bbdb-search-simple name 
										  (if (and net bbdb-canonicalize-net-hook) 
											  (bbdb-canonicalize-address net) net)))))
	(if (and record name 
			 (member (downcase name) (bbdb-record-net record))) ;; bogon! 
		(setq record nil)) 
	(setq name (or (and gnus-bbdb-summary-prefer-bbdb-data 
						(or (and gnus-bbdb-summary-prefer-real-names 
								 (and record (bbdb-record-name record)))
							(and record (bbdb-record-net record) 
								 (nth 0 (bbdb-record-net record)))))
				   (and gnus-bbdb-summary-prefer-real-names 
						(or (and (equal gnus-bbdb-summary-prefer-real-names 'bbdb)
								 net) name))
				   net from "Some Smeghead"))
	(format "%s%s" 
			(or (and record gnus-bbdb-summary-mark-known-posters 
					 (or (bbdb-record-getprop record
											  bbdb-message-marker-field)
						 gnus-bbdb-summary-known-poster-mark)) " ")
			name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Scoring with gnus-score REQUIRES (ding) 0.50 or higher 
;;; 
;;; This is not currently supported by GNUS, but hopefully will be soon. 
;;; Once I know how it will be integrated, there will be instructions 
;;; here on how to use it.

;;; 
;;; The following variables may be changed in your .gnus file to 
;;; customize the scoring behaviour. See their documentation strings 
;;; for more information. 
;;; - gnus-bbdb-score-field 
;;; - gnus-bbdb-score-default
(defvar gnus-bbdb-score-field 'gnus-score "This variable contains the name
of the BBDB field which should be checked for a score to add to the net
addresses in the same record.") (defvar gnus-bbdb-score-default nil "If this
is set, then every net address in the BBDB that does not have an associated
score field will be assigned this score. A value of nil implies a default
score of zero.") (defvar gnus-bbdb-score-alist nil "The text version of the
scoring structure returned by gnus-bbdb-score. This is built automatically
from the BBDB.") (defvar gnus-bbdb-score-rebuild-alist t "Set to t to
rebuild gnus-bbdb-score-alist on the next call to gnus-bbdb-score. This will
be set automatically if you change a BBDB record which contains a gnus-score
field.") (add-hook 'bbdb-after-change-hook
'gnus-bbdb-score-invalidate-alist) (defun gnus-bbdb-score-invalidate-alist
(rec) "This function is called through bbdb-after-change-hook, and sets
gnus-bbdb-score-rebuild-alist to t if the changed record contains a
gnus-score field." (if (bbdb-record-getprop rec gnus-bbdb-score-field) (setq
gnus-bbdb-score-rebuild-alist t))) (defun gnus-bbdb-score (group) "This
returns a score alist for GNUS. A score pair will be made for every member
of the net field in records which also have a gnus-score field. This allows
the BBDB to serve as a supplemental global score file, with the advantage
that it can keep up with multiple and changing addresses better than the
traditionally static global scorefile." (list (condition-case nil (read
(gnus-bbdb-score-as-text group)) (error (setq gnus-bbdb-score-rebuild-alist
t) (message "Problem building BBDB score table.") (ding) (sit-for 2) nil))))
(defun gnus-bbdb-score-as-text (group) "Returns a SCORE file format string
built from the BBDB." (if (and gnus-bbdb-score-alist (not
gnus-bbdb-score-rebuild-alist)) nil (setq gnus-bbdb-score-rebuild-alist nil)
(setq gnus-bbdb-score-alist (concat "((touched nil) (\"from\"\n" (mapconcat
(lambda (rec) (let ((score (or (bbdb-record-getprop rec
gnus-bbdb-score-field) gnus-bbdb-score-default)) (net (bbdb-record-net
rec))) (if (not (and score net)) nil (mapconcat (lambda (addr) (concat "(\""
addr "\" " score ")\n")) net "")))) (bbdb-records) "") "))")))
gnus-bbdb-score-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;;
Filing with gnus-folder REQUIRES (ding) 0.50 OR HIGHER 
;;; 
;;; To use this
feature, you need to put this file somewhere in your 
;;; load-path and add
the following lines of code to your .gnus file: 
;;; 
;;; (require 'gnus-bbdb)

;;; (setq nnmail-split-methods 'gnus-bbdb-split-method) 
;;; 
;;; You should
also examine the variables defvar'd below and customize 
;;; them to your
taste. They're listed roughly in descending likelihood 
;;; of your wanting
to change them. Once that is done, you need to add 
;;; filing information to
your BBDB. There are two fields of interest: 
;;; 
;;; 1. gnus-private. This
field contains the name of the group in which 
;;; mail to you from any of
the addresses associated with this record 
;;; will be filed. Also, any
self-copies of mail you send any of the 
;;; same addresses will be filed
here. 
;;; 2. gnus-public. This field is used to keep mail from mailing lists

;;; out of the private mailboxes. It should be added to a record for 
;;; the
list submission address, and is formatted as follows: 
;;; "group regexp" ;;;
where group is where mail from the list should be filed, and 
;;; regexp is a
regular expression which is checked against the 
;;; envelope sender (from
the From_ header) to verify that this is 
;;; the copy which came from the
list. For example, the entry for 
;;; the ding mailing list might be: ;;;
"mail.emacs.ding ding-request@ifi.uio.no" 
;;; Yes, the second part *is* a
regexp, so those dots may match 
;;; something other than dots. Sue me. ;;;

;;; Note that you can also specify a gnus-private field for mailing list ;;;
addresses, in which case self-copies of mail you send to the list 
;;; will
be filed there. Also, the field names can be changed below if 
;;; the
defaults aren't hip enough for you. Lastly, if you specify a ;;;
gnus-private field for your *own* BBDB record, then all self-copies 
;;; of
mail you send will be filed to that group. 
;;; 
;;; This documentation should
probably be expanded and moved to a 
;;; separate file, but it's late, and
*I* know what I'm trying to 
;;; say. :) (defvar
gnus-bbdb-split-default-group "mail.misc" "If the BBDB doesn't indicate any
group to spool a message to, it will be spooled to this group. If
gnus-bbdb-split-crosspost-default is not nil, and if the BBDB did not
indicate a specific group for one or more addresses, messages will be
crossposted to this group in addition to any group(s) which the BBDB
indicated.") (defvar gnus-bbdb-split-nomatch-function nil "This function
will be called after searching the BBDB if no place to file the message
could be found. It should return a group name (or list of group names) --
nnmail-split-fancy as provided with Gnus is an excellent choice.") (defvar
gnus-bbdb-split-myaddr-regexp (concat "^" (user-login-name) "$\\|^"
(user-login-name) "@\\([-a-z0-9]+\\.\\)*" (or gnus-local-domain
(message-make-domain) (system-name) "smeghead\\.baka\\.org") "$") "This
regular expression should match your address as found in the From header of
your mail. You should make sure gnus-local-domain or gnus-use-generic-from
are set before loading this module, if they differ from (system-name). If
you send mail/news from multiple addresses, then you'll likely have to set
this yourself anyways.") (defvar gnus-bbdb-split-crosspost-default nil "If
this variable is not nil, then if the BBDB could not identify a group for
every mail address, messages will be filed in gnus-bbdb-split-default-group
in addition to any group(s) which the BBDB identified.") (defvar
gnus-bbdb-split-private-field 'gnus-private "This variable is used to
determine the field to reference to find the associated group when saving
private mail for a network address known to the BBDB. The value of the field
should be the name of a mail group.") (defvar gnus-bbdb-split-public-field
'gnus-public "This variable is used to determine the field to reference to
find the associated group when saving non-private mail (received from a
mailing list) for a network address known to the BBDB. The value of the
field should be the name of a mail group, followed by a space, and a regular
expression to match on the envelope sender to verify that this mail came
from the list in question.") ;; The split function works by assigning one of
four spooling priorities ;; to each group that is associated with an address
in the message. The ;; priorities are assigned as follows: ;; ;; 0. This
priority is assigned when crosspost-default is nil to To/Cc ;; addresses
which have no private group defined in the BBDB. If the ;; user's own
address has no private group defined, then it will ;; always be given this
priority. ;; 1. This priority is assigned to To/Cc addresses which have a
private ;; group defined in the BBDB. If crosspost-default is not nil, then
;; To/Cc addresses which have no private group will also be assigned ;; this
priority. This is also assigned to the user's own address in ;; the From
position if a private group is defined for it. ;; 2. This priority is
assigned to From addresses which have a private ;; group defined in the
BBDB, except for the user's own address as ;; described under priorities 0
and 1. ;; 3. This priority is assigned to To/Cc addresses which have a
public ;; group defined in the BBDB, and whose associated regular expression
;; matches the envelope sender (found in the header From_). ;; ;; The split
function evaluates the spool priority for each address in ;; the headers of
the message, and returns as a list all the groups ;; associated with the
addresses which share the highest calculated ;; priority. (defun
gnus-bbdb-split-method nil "This function expects to be called in a buffer
which contains a mail message to be spooled, and the buffer should be
narrowed to the message headers. It returns a list of groups to which the
message should be spooled, using the addresses in the headers and
information from the BBDB." (let ((prq (list (cons 0 nil) (cons 1 nil) (cons
2 nil) (cons 3 nil)))) ;; the From: header is special (let* ((hdr (or
(mail-fetch-field "from") (user-login-name))) (rv (gnus-bbdb-split-to-group
hdr t))) (setcdr (nth (cdr rv) prq) (cons (car rv) nil))) ;; do the rest of
the headers (let ((hdr (or (concat (mail-fetch-field "to" nil t) ", "
(mail-fetch-field "cc" nil t) ", " (mail-fetch-field "apparently-to" nil t))
""))) (setq hdr (rfc822-addresses hdr)) (while hdr (let* ((rv
(gnus-bbdb-split-to-group (car hdr))) (pr (nth (cdr rv) prq))) (or (member
(car rv) pr) (setcdr pr (cons (car rv) (cdr pr))))) (setq hdr (cdr hdr))))
;; find the highest non-empty queue (setq prq (reverse prq)) (while (and prq
(not (cdr (car prq)))) (setq prq (cdr prq))) ;; and return... (if (not (or
(not (cdr (car prq))) (and (equal (cdr (car prq)) (list
gnus-bbdb-split-default-group)) (symbolp gnus-bbdb-split-nomatch-function)
(fboundp gnus-bbdb-split-nomatch-function)))) (cdr (car prq)) (goto-char
(point-min)) (funcall gnus-bbdb-split-nomatch-function)))) (defun
gnus-bbdb-split-to-group (addr &optional source) "This function is called
from gnus-bbdb-split-method in order to determine the group and spooling
priority for a single address." (condition-case tmp (progn (setq tmp
(mail-extract-address-components addr)) (let* ((nam (car tmp)) (net (if (not
bbdb-canonicalize-net-hook) (car (cdr tmp)) (bbdb-canonicalize-address (car
(cdr tmp))))) (rec (bbdb-search-simple nam net)) pub prv rgx) (if (not rec)
nil (setq prv (bbdb-record-getprop rec gnus-bbdb-split-private-field) pub
(bbdb-record-getprop rec gnus-bbdb-split-public-field)) (if (and pub (not
source) (string-match "^\\([^ ]+\\) \\(.*\\)$" pub)) (setq rgx (substring
pub (match-beginning 2) (match-end 2)) pub (substring pub (match-beginning
1) (match-end 1))) (setq pub nil))) (cond ((and rgx pub (goto-char
(point-min)) (re-search-forward "^From \\([^ \n]+\\)[ \n]" nil t)
(string-match rgx (buffer-substring (match-beginning 1) (match-end 1))))
(cons pub 3)) (prv (cons prv (- 1 (if source -1 0) (if (string-match
gnus-bbdb-split-myaddr-regexp net) 1 0)))) (t (cons
gnus-bbdb-split-default-group (if (string-match
gnus-bbdb-split-myaddr-regexp net) 0 (if source 2 (if
gnus-bbdb-split-crosspost-default 1 0)))))))) (error (cons
gnus-bbdb-split-default-group 0)))) (provide 'gnus-bbdb) 
;;; EOF
