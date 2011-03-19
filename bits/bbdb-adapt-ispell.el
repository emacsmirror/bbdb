;;; bbdbadapt-ispell.el --- Use the BBDB to insert a  gcc field

;; Copyright (C) 2009 Uwe Brauer

;; Author: Uwe Brauer oub@mat.ucm.es
;; Maintainer: Uwe Brauer oub@mat.ucm.es
;; Created: 17 Mar 2009
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to oub@mat.ucm.es) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

;; LCD Archive Entry:
;; bbdbadpt-gcc|Uwe Brauer|oub@mat.ucm.es
;; |Use the BBDB to insert a  gcc field
;; |Date|Revision|~/packages/bbdbadpt-gcc.el

;;; Commentary:
;; I wanted to have the ispell dict selected 
;; based on a relevant bbdb entry which I call ispell-dict.
;; This is what the code does. 

;; The starting point was some code provided to me by Robert
;; Fenk. However the problem with that code was that it took the gcc
;; field from some entry of the BBDB buffer and if there was more than
;; on entry often the wrong string was inserted. So I had to use code
;; which extracted the correct BBDB entry from the TO field .  I
;; succeeded by using to a large extend code from sigadapt.el written
;; by by C.Queinnec (University of Paris 6 & INRIA)
;; 

;;; Change log:
;; Revision 1.1  2009/03/17 17:27:31  oub
;; Initial revision
;;
;; Revision 1.2  2009/03/17 16:50:20  oub
;; modify the central function
;;
;; Revision 1.1  2009/03/17 16:32:26  oub
;; Initial revision
;;

;;; Code:




(defun bbdbispelladpt-search-record (to)
  "Search a BBDB record associated to TO or return NIL."
  (let* ((data (mail-extract-address-components to))
         (name (car data))
         (net  (car (cdr data))))
    (if (equal name net) (setq name nil))
    (if (and net bbdb-canonicalize-net-hook)
        (setq net (bbdb-canonicalize-address net)))
    (bbdb-search-simple name net)))

(defun bbdbispelladpt-try-bbdbispell-new ()
  "Try to adapt non-interactively the current bbdbispell. 
This function looks silently in the current message to find how to
choose the bbdbispell. It does nothinng if not enough information is
present. This function is useful in a hook."
  (save-excursion
    (condition-case nil
        (progn
          (goto-char (point-min))
          (let ((record (bbdbispelladpt-search-record 
                         (bbdb-extract-field-value "To"))))
			(if record
				(let ((signame (bbdbispelladpt-retrieve-bbdbispell record)))
				  (when (and (stringp signame) (string= signame "castellano8"))
					(ispell-change-dictionary    "castellano8"       nil))
			  (when (and (stringp signame) (string= signame "english"))
				(ispell-change-dictionary "american" nil	))
			  (when (and (stringp signame) (string= signame "deutsch"))
				(ispell-change-dictionary "deutsch8" nil))
			  (when (and (stringp signame) (string= signame "french"))
				(ispell-change-dictionary "francais" nil)))))))))




(defun bbdbispelladpt-retrieve-bbdbispell (&optional record) 
  "Retrieve the bbdbispell (a symbol) associated to a mailee.
The search is done through a BBDB record. "
  (if (not record)
      (save-excursion
        (goto-char (point-min))
        (let* ((to  (bbdb-extract-field-value "To"))
               (rec (bbdbispelladpt-search-record to)) )
          (if rec (bbdbispelladpt-do-retrieve-bbdbispell rec)
            (progn (message "No bound record")
                   nil))))
    (bbdbispelladpt-do-retrieve-bbdbispell record) ) )

(defun bbdbispelladpt-do-retrieve-bbdbispell (record)
  (let ((signame
	 (bbdb-record-getprop record 'ispell-dict)))
    (if (stringp signame)
        (setq signame signame))
    signame))


(provide 'bbdbadapt-ispell)

;;; BBDBADPT-ISPELL.EL ends here
