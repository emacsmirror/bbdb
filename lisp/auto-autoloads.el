;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'bbdb-autoloads))
    (progn

;;;### (autoloads (bbdb-initialize) "bbdb" "lisp/bbdb.el")

(autoload 'bbdb-initialize "bbdb" "\
*Initialize the BBDB.  One or more of the following symbols can be
passed as arguments to initiate the appropriate insinuations.

 Initialization of mail/news readers:

   Gnus       Initialize BBDB support for the Gnus version 3.14 or
              older.
   gnus       Initialize BBDB support for the Gnus mail/news reader
              version 3.15 or newer.  If you pass the `gnus' symbol,
              you should probably also pass the `message' symbol.
   mh-e       Initialize BBDB support for the MH-E mail reader.
   rmail      Initialize BBDB support for the RMAIL mail reader.
   sendmail   Initialize BBDB support for sendmail (M-x mail).
   vm         Initialize BBDB support for the VM mail reader.
              NOTE: For the VM insinuation to work properly, you must
              either call `bbdb-initialize' with the `vm' symbol from
              within your VM initialization file (\"~/.vm\") or you
              must call `bbdb-insinuate-vm' manually from within your
              VM initialization file.

 Initialization of miscellaneous package:

   message    Initialize BBDB support for Message mode.
   reportmail Initialize BBDB support for the Reportmail mail
              notification.
   sc         Initialize BBDB support for the Supercite message
              citation package.
   w3         Initialize BBDB support for Web browsers." nil nil)

;;;***


(provide 'bbdb-autoloads)
))
