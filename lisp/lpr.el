;; Print Emacs buffer on line printer.
;; Copyright (C) 1985 Richard M. Stallman.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; GNU Emacs so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.


;(defconst lpr-switches nil
;  "*List of strings to pass as extra switch args to lpr when it is invoked.")

(defun lpr-buffer ()
  "Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (dot-min) (dot-max) lpr-switches))

(defun print-buffer ()
  "Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (dot-min) (dot-max) (cons "-p" lpr-switches)))

(defun lpr-region (start end)
  "Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end lpr-switches))

(defun print-region (start end)
  "Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end (cons "-p" lpr-switches)))

(defun print-region-1 (start end switches)
  (save-excursion
   (message "Spooling...")
   (if (/= tab-width 8)
       (let ((oldbuf (current-buffer)))
	(set-buffer (get-buffer-create " *spool temp*"))
	(widen) (erase-buffer)
	(insert-buffer-substring oldbuf)
	(call-process-region start end "/usr/ucb/expand"
			     t t nil
			     (format1 "-%d" tab-width))))
   (apply 'call-process-region
	  (nconc (list start end "/usr/ucb/lpr"
		       nil nil nil
		       "-J" (concat "Emacs buffer " (buffer-name))
		       "-T" (concat "Emacs buffer " (buffer-name)))
		 switches))
   (message "Spooling...done")))
