(setq load-path (append (list "../lisp" ".") load-path))
(setq make-backup-files nil)
(if (file-exists-p "bbdb-test-results") (delete-file
                                           "bbdb-test-results"))
(require 'bbdb-test)
(bbdb-test/initialize)
(bbdb-test/run-all-tests 'batch)
