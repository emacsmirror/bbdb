;; Testing
;;
;; This is a rough start on a test harness for BBDB. It attempts to
;; get around the interactive nature of BBDB in order to allow batch
;; testing. Eventually, there will be a 'make test' target, which
;; verifies that the changes you've just made haven't killed some
;; other part of BBDB.
;;
;; Authors: Waider & Robert Fenk
;;
;; This stuff doesn't get included in the tarball.

(require 'bbdb)
(require 'bbdb-snarf) ;; should be autoloaded, I'm sure.

(defun bbdb-test/initialize()
  ;; Set up a test BBDB
  (setq bbdb-file (expand-file-name "~/.bbdb-test"))
  (find-file bbdb-file)
  (erase-buffer)
  (save-buffer)
  (kill-buffer))

(defun bbdb-test/log-result (format &rest rest)
  (save-excursion
    (set-buffer (get-buffer-create "*Test Results*"))
    (goto-char (point-max))
    (insert (apply 'format format rest))))

;;;###autoload
(defun bbdb-test/run-all ()
  (interactive)
  (bbdb-initialize)
  (let ((bbdb-read-only-p t) ;; Don't trash BBDB!
        (test-vars (apropos-internal "^bbdb-test/.*$"
                                      (lambda (s) (and (symbolp s)
                                                       (boundp s)
                                                       (not (fboundp s))))))
        (test-res  (get-buffer-create "*Test Results*")))
    (pop-to-buffer test-res)

    (save-excursion
      (set-buffer test-res)
      (erase-buffer)
      (insert "Testing started at ")
      (insert (current-time-string))
      (insert "\n\n"))

    (while test-vars
      (bbdb-test/run-one-test (car test-vars))
      (setq test-vars (cdr test-vars)))
    (set-buffer test-res)))

(defun bbdb-test/run-one-test (test-var)
  (interactive "SEnter a variable to test: ")
  (or (string-match "^bbdb-test/" (symbol-name test-var))
      (setq test-var (intern (concat "bbdb-test/" (symbol-name test-var)))))
  (let ((frob-var (intern (substring (symbol-name test-var) 10)))
        (frob-vals (symbol-value test-var))
        test-func)

    ;; Peel the test function off the top of the variable, and
    ;; adjust the variable upward.
    (setq test-func (car frob-vals)
          frob-vals (cdr frob-vals))

    (bbdb-test/log-result "Testing %s\n  using\t%s:\n\n"
                          frob-var test-func)

    (while frob-vals
      (let* ((current-test-data (car frob-vals))
             (frob-val (car current-test-data))
             (frob-par (cdr current-test-data)))
        (bbdb-test/log-result "  %s:\n" frob-val)

        ;; ick. Hope you weren't using this.
        (set frob-var frob-val)

        (while frob-par
          (bbdb-test/log-result "    ")
          (bbdb-test/log-result
           (funcall test-func (nth 0 (car frob-par))
                    (nth 1 (car frob-par))))
          (bbdb-test/log-result "\n")
          (setq frob-par (cdr frob-par)))

        (bbdb-test/log-result "\n")
        (with-current-buffer (get-buffer "*Test Results*")
          (goto-char (point-max))))

      ;; next set of values
      (setq frob-vals (cdr frob-vals)))

    (bbdb-test/log-result "Completed testing of %s.\n%s\n"
                          frob-var (make-string 79 ?-))))


;; Coverage guestimation (VARIABLES ONLY; turns up a few false ones)
(defun bbdb-test/guestimate-coverage()
  (interactive)
  (let* ((vars (apropos-internal "^bbdb-.*$"
                                 (lambda (s)
                                   (and (symbolp s)
                                        (not (fboundp s))
                                        (not (string-match
                                              "^bbdb-test"
                                              (symbol-name s)))))))
         (tests (apropos-internal "^bbdb-test/.*$"
                                  (lambda (s) (and (symbolp s)
                                                   (not (fboundp s))))))
         (cov (* 100 (/ (float (length tests)) (float (length vars))))))
    (message "coverage: %d of %d variables (%0.2f%%)" (length tests)
             (length vars) cov)
    cov))


;;; These are test-harness functions for BBDB functionality
;;;
;;; Test BBDB's completion
(defun bbdb-test/bbdb-completion-type-test (input output)
  (let ((bbdb-complete-name-allow-cycling t)       ;; some test cases
        (bbdb-dwim-net-address-allow-redundancy t) ;; need this
        bbdb-completion-display-record) ;; stop BBDB buffer from
                                        ;; popping up

    ;; Make sure there isn't a completions buffer lying around
    (if (get-buffer "*Completions*")
        (kill-buffer "*Completions*"))

    ;; Try completing
    (with-current-buffer (get-buffer-create "*BBDB_TEST*")
      (erase-buffer)
      (insert input)

      ;; Try completion. Disable beeping so that we don't get noise
      ;; while testing uncompletables.
      (flet ((beep nil ())
             (ding nil ()))
        (bbdb-complete-name))

      ;; Hack to get around interactivity
      (if (get-buffer "*Completions*")
          (progn
            (save-excursion
              (set-buffer (get-buffer "*Completions*"))
              (goto-char (point-min))
              (forward-line 4) ;; pick the first completion provided
              (beginning-of-line)
              (copy-region-as-kill (point)
                                   (progn (end-of-line)
                                          (point))))
            (kill-buffer "*Completions*")
            (beginning-of-line)
            (delete-region (point)
                           (progn (end-of-line)
                                  (point)))
            (yank)))

      (beginning-of-line)
      (copy-region-as-kill (point) (progn (end-of-line) (point)))

      ;; Check the output
      (let ((result (current-kill 0)))
        (set-text-properties 0 (length result) nil result)
        (if (equal result output)
            (format "PASSED %s => %s" input output)
          (format "FAILED %s => %s (%s)" input output result))))))

;;; Test BBDB parsing of email addresses
(defun bbdb-test/bbdb-extract-address-components (input output)
  "Test suite for BBDB developers internal use."
  (let (parsed)
    (setq parsed (funcall bbdb-extract-address-components-func input t))
    (if (and parsed (equal output (car parsed)))
        (format "PASSED %S => \n\t\t%S" input (car parsed))
      (format "FAILED got `%S' expected `%S'" (car parsed) output))))

;; Test username-cleaning-function
;; This function doesn't depend on any variables.
;; XXX fix the test harness to not require a variable to frob!
(defun bbdb-test/bbdb-clean-username(input output)
  (let ((result (bbdb-clean-username input)))
    (if (and result (equal output result))
        (format "PASSED %S => \n\t\t%S" input result)
      (format "FAILED got `%S' expected `%S'" result output))))

;; Test string trimming. Should possibly be combined with the above!


;; These are setup variables for the testing
(defvar bbdb-clean-username-dummy nil)
(defvar bbdb-test/bbdb-clean-username-dummy)
(setq bbdb-test/bbdb-clean-username-dummy
      '(bbdb-test/bbdb-clean-username
        (nil ("Ronan Waide" "Ronan Waide")
             ("Forrester Research, Inc." "Forrester Research, Inc")
             ("Ronan Waide ext 5781" "Ronan Waide")
             ("Ronan Waide (Just This Guy)" "Ronan Waide")
             )))

(defvar bbdb-test/bbdb-extract-address-components-func
  '(bbdb-test/bbdb-extract-address-components
    (bbdb-rfc822-addresses
     ("Robert Fenk <fenk@users.sourceforge.net>"
      ("Robert Fenk" "fenk@users.sourceforge.net"))
     ("\"Robert Fenk, Jr\" <fenk@users.sourceforge.net>"
      ("Robert Fenk, Jr." "fenk@users.sourceforge.net"))
     ("\"Fenk, Robert\" <fenk@users.sourceforge.net>"
      ("Robert Fenk" "fenk@users.sourceforge.net"))
     ("fenk@users.sourceforge.net (Robert Fenk)"
      ("Robert Fenk" "fenk@users.sourceforge.net"))
     ("fenk@users.sourceforge.net (Robert Fenk, Jr)"
      ("Robert Fenk, Jr." "fenk@users.sourceforge.net"))
     ("Robert.Fenk@users.sourceforge.net"
      ("Robert Fenk" "Robert.Fenk@users.sourceforge.net")))
    (bbdb-extract-address-components
     ("Robert Fenk <fenk@users.sourceforge.net>"
      ("Robert Fenk" "fenk@users.sourceforge.net"))
     ("\"Robert Fenk, Jr\" <fenk@users.sourceforge.net>"
      ("Robert Fenk, Jr." "fenk@users.sourceforge.net"))
     ("\"Fenk, Robert\" <fenk@users.sourceforge.net>"
      ("Robert Fenk" "fenk@users.sourceforge.net"))
     ("fenk@users.sourceforge.net (Robert Fenk)"
      ("Robert Fenk" "fenk@users.sourceforge.net"))
     ("fenk@users.sourceforge.net (Robert Fenk, Jr)"
      ("Robert Fenk, Jr." "fenk@users.sourceforge.net"))
     ("Robert.Fenk@users.sourceforge.net"
      ("Robert Fenk" "Robert.Fenk@users.sourceforge.net")))))

;; Things to test bbdb-completion-type with
(defvar bbdb-test/bbdb-completion-type
  '(bbdb-test/bbdb-completion-type-test ;; test function
    ;; variable setting, (input, output)
    (nil ("waider" "Ronan Waide <waider@waider.ie>")
         ;; test cycling
         ("Ronan Waide <waider@waider.ie>"
          "Ronan Waide <0872867770@e-merge.ie>")
         ("ronan waide" "Ronan Waide <waider@waider.ie>")
         ("ronan.waide" "Ronan Waide <ronan.waide@euroconex.com>")
         ("Robert.Fenk@g" "Robert Fenk <Robert.Fenk@gmx.de>")
         ("jwz" "Jamie Zawinski <jwz@jwz.org>"))
    (name ("waider" "waider") ;; no completion
          ("ronan waide" "Ronan Waide <waider@waider.ie>"))
    (net ("waider" "Ronan Waide <waider@waider.ie>")
         ("jwz" "Jamie Zawinski <jwz@jwz.org>"))
    (primary ("waider" "Ronan Waide <waider@waider.ie>"))
    (primary-or-name ("waider" "Ronan Waide <waider@waider.ie>"))
    (name-or-primary ("waider" "Ronan Waide <waider@waider.ie>"))))

;; This is a list of **158** symbols defined in BBDB at present. Some
;; are obviously not actually variables and can be ignored. Mark the
;; variables done as they have test cases applied to them. Coverage
;; function above should probably use this to determine what can be
;; safely ignored. Additionally, I should load all the files before
;; generating a list like this and thinking it's definitive :)
;;
;; DISREGARD      bbdb-test/bbdb-
;; INTERNAL ONLY  bbdb-test/bbdb-address
;; DISREGARD      bbdb-test/bbdb-address-
;;                bbdb-test/bbdb-address-editing-function
;;                bbdb-test/bbdb-address-formatting-alist
;; INTERNAL ONLY  bbdb-test/bbdb-address-length
;;                bbdb-test/bbdb-address-print-formatting-alist
;;                bbdb-test/bbdb-addresses-label-list
;;                bbdb-test/bbdb-after-change-hook
;;                bbdb-test/bbdb-after-read-db-hook
;;                bbdb-test/bbdb-alist-with-header
;;                bbdb-test/bbdb-always-add-addresses
;;                bbdb-test/bbdb-auto-revert-p
;;                bbdb-test/bbdb-autoloads
;;                bbdb-test/bbdb-buffer-name
;;                bbdb-test/bbdb-buffers-with-message-caches
;; DISREGARD      bbdb-test/bbdb-cache-
;;                bbdb-test/bbdb-cache-length
;;                bbdb-test/bbdb-canonicalize-net-hook
;;                bbdb-test/bbdb-canonicalize-redundant-nets-p
;;                bbdb-test/bbdb-case-fold-search
;;                bbdb-test/bbdb-change-hook
;;                bbdb-test/bbdb-check-zip-codes-p
;;                bbdb-test/bbdb-com
;;                bbdb-test/bbdb-complete-name-allow-cycling
;;                bbdb-test/bbdb-complete-name-full-completion
;;                bbdb-test/bbdb-complete-name-hooks
;;                bbdb-test/bbdb-complete-name-saved-window-config
;;                bbdb-test/bbdb-completion-display-record
;; DONE           bbdb-test/bbdb-completion-type
;;                bbdb-test/bbdb-continental-zip-regexp
;;                bbdb-test/bbdb-create-hook
;; INTERNAL       bbdb-test/bbdb-cycling-exit
;;                bbdb-test/bbdb-database
;;                bbdb-test/bbdb-default-area-code
;;                bbdb-test/bbdb-default-country
;;                bbdb-test/bbdb-default-domain
;;                bbdb-test/bbdb-default-label-list
;;                bbdb-test/bbdb-define-all-aliases-field
;;                bbdb-test/bbdb-dial-local-prefix
;;                bbdb-test/bbdb-dial-local-prefix-alist
;;                bbdb-test/bbdb-dial-long-distance-prefix
;;                bbdb-test/bbdb-display-buffer
;;                bbdb-test/bbdb-display-layout
;;                bbdb-test/bbdb-display-layout-alist
;;                bbdb-test/bbdb-dwim-net-address-allow-redundancy
;;                bbdb-test/bbdb-electric-completed-normally
;;                bbdb-test/bbdb-electric-execute-me
;;                bbdb-test/bbdb-electric-p
;;                bbdb-test/bbdb-elided-display
;;                bbdb-test/bbdb-end-marker
;;                bbdb-test/bbdb-expand-mail-aliases
;;                bbdb-test/bbdb-extract-address-component-handler
;;                bbdb-test/bbdb-extract-address-component-ignore-regexp
;;                bbdb-test/bbdb-extract-address-component-regexps
;; DONE           bbdb-test/bbdb-extract-address-components-func
;;                bbdb-test/bbdb-field
;;                bbdb-test/bbdb-file
;;                bbdb-test/bbdb-file-format
;;                bbdb-test/bbdb-file-format-migration
;;                bbdb-test/bbdb-file-remote
;;                bbdb-test/bbdb-file-remote-save-always
;;                bbdb-test/bbdb-finger-buffer-name
;;                bbdb-test/bbdb-finger-host-field
;;                bbdb-test/bbdb-force-dialog-boxes
;;                bbdb-test/bbdb-gag-messages
;;                bbdb-test/bbdb-get-addresses-headers
;;                bbdb-test/bbdb-get-only-first-address-p
;;                bbdb-test/bbdb-gui
;;                bbdb-test/bbdb-hashtable-size
;;                bbdb-test/bbdb-hooks
;;                bbdb-test/bbdb-info-file
;;                bbdb-test/bbdb-init-forms
;;                bbdb-test/bbdb-initialize-hook
;;                bbdb-test/bbdb-inside-electric-display
;;                bbdb-test/bbdb-insinuate-sc
;;                bbdb-test/bbdb-legal-zip-codes
;;                bbdb-test/bbdb-list-hook
;;                bbdb-test/bbdb-load-hook
;;                bbdb-test/bbdb-message-cache
;;                bbdb-test/bbdb-message-caching-enabled
;;                bbdb-test/bbdb-mode-hook
;;                bbdb-test/bbdb-mode-map
;;                bbdb-test/bbdb-modem-device
;;                bbdb-test/bbdb-modem-dial
;;                bbdb-test/bbdb-modified-p
;;                bbdb-test/bbdb-mua-specific
;;                bbdb-test/bbdb-mua-specific-gnus
;;                bbdb-test/bbdb-mua-specific-gnus-scoring
;;                bbdb-test/bbdb-mua-specific-gnus-splitting
;;                bbdb-test/bbdb-mua-specific-vm
;;                bbdb-test/bbdb-name-gubbish
;;                bbdb-test/bbdb-new-nets-always-primary
;;                bbdb-test/bbdb-no-duplicates-p
;;                bbdb-test/bbdb-north-american-phone-numbers-p
;;                bbdb-test/bbdb-notes-default-separator
;;                bbdb-test/bbdb-notes-field
;;                bbdb-test/bbdb-notes-sort-order
;;                bbdb-test/bbdb-notice-auto-save-file
;;                bbdb-test/bbdb-notice-hook
;;                bbdb-test/bbdb-noticing-records
;;                bbdb-test/bbdb-offer-to-create
;;                bbdb-test/bbdb-phone-
;;                bbdb-test/bbdb-phone-area-regexp
;;                bbdb-test/bbdb-phone-dialing
;;                bbdb-test/bbdb-phone-ext-regexp
;;                bbdb-test/bbdb-phone-length
;;                bbdb-test/bbdb-phone-main-regexp
;;                bbdb-test/bbdb-phone-regexp-1
;;                bbdb-test/bbdb-phone-regexp-2
;;                bbdb-test/bbdb-phone-regexp-3
;;                bbdb-test/bbdb-phone-regexp-4
;;                bbdb-test/bbdb-phone-regexp-5
;;                bbdb-test/bbdb-phones-label-list
;;                bbdb-test/bbdb-pop-up-display-layout
;;                bbdb-test/bbdb-pop-up-elided-display
;;                bbdb-test/bbdb-pop-up-target-lines
;;                bbdb-test/bbdb-quiet-about-name-mismatches
;;                bbdb-test/bbdb-read-addresses-with-completion-map
;;                bbdb-test/bbdb-read-only-p
;;                bbdb-test/bbdb-readonly-p
;;                bbdb-test/bbdb-record
;;                bbdb-test/bbdb-record-
;;                bbdb-test/bbdb-record-creation
;;                bbdb-test/bbdb-record-display
;;                bbdb-test/bbdb-record-length
;;                bbdb-test/bbdb-record-use
;;                bbdb-test/bbdb-refile-notes-generate-alist
;;                bbdb-test/bbdb-remaining-addrs-to-finger
;;                bbdb-test/bbdb-save
;;                bbdb-test/bbdb-save-db-timeout
;;                bbdb-test/bbdb-saving
;;                bbdb-test/bbdb-send-mail-style
;;                bbdb-test/bbdb-showing-changed-ones
;;                bbdb-test/bbdb-silent-running
;;                bbdb-test/bbdb-snarf-phone-regexp
;;                bbdb-test/bbdb-snarf-web-prop
;;                bbdb-test/bbdb-snarf-zip-regexp
;;                bbdb-test/bbdb-sound-files
;;                bbdb-test/bbdb-sound-player
;;                bbdb-test/bbdb-sound-volume
;;                bbdb-test/bbdb-sounds-directory
;;                bbdb-test/bbdb-suppress-changed-records-recording
;;                bbdb-test/bbdb-time-display-format
;;                bbdb-test/bbdb-update-address-class
;;                bbdb-test/bbdb-update-address-header
;;                bbdb-test/bbdb-update-records-mode
;;                bbdb-test/bbdb-use-alternate-names
;;                bbdb-test/bbdb-use-pop-up
;;                bbdb-test/bbdb-utilities
;;                bbdb-test/bbdb-utilities-finger
;;                bbdb-test/bbdb-utilities-ftp
;;                bbdb-test/bbdb-utilities-print
;;                bbdb-test/bbdb-utilities-server
;;                bbdb-test/bbdb-utilities-supercite
;;                bbdb-test/bbdb-version-date
;;                bbdb-test/bbdb-window
;;                bbdb-test/bbdb-write-file-hooks

;; not strictly necessary!
(provide 'bbdb-test)