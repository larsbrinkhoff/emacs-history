;; Read in and display parts of Unix manual.
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


(defun manual-entry (topic)
  "Display the Unix manual entry for TOPIC."
  (interactive "sManual entry (topic): ")
  (with-output-to-temp-buffer "*Manual Entry*"
    (buffer-flush-undo standard-output)
    (save-excursion
     (set-buffer standard-output)
     (message "Looking for formatted entry for %s..." topic)
     (let ((dirlist '("/usr/man/cat1"
		      "/usr/man/cat2" "/usr/man/cat3" "/usr/man/cat4"
		      "/usr/man/cat5" "/usr/man/cat6" "/usr/man/cat7"
		      "/usr/man/cat8" "/usr/man/catl"))
	   (name (concat topic ".")))
       (while dirlist
	 (let* ((dir (car dirlist))
		(name1 (concat dir "/" name (substring dir -1)))
		completions)
	   (if (file-exists-p name1)
	       (insert-file-contents name1)
	     (condition-case ()
		 (progn
		   (setq completions (file-name-all-completions name dir))
		   (while completions
		     (insert-file-contents (concat dir "/" (car completions)))
		     (setq completions (cdr completions))))
	       (file-error nil)))
	   (goto-char (dot-max)))
	 (setq dirlist (cdr dirlist))))

     (if (= (buffer-size) 0)
	 (progn
	  (message "No formatted entry, invoking man %s..." topic)
	  (call-process "/usr/ucb/man" nil t nil
			topic)
	  (if (< (buffer-size) 80)
	      (progn
	       (goto-char (dot-min))
	       (end-of-line)
	       (error (buffer-substring 1 (dot)))))))

     (message "Cleaning manual entry for %s..." topic)

     (goto-char (dot-min))
     (while (re-search-forward "_\b" nil t)
       (replace-match ""))

     (goto-char (dot-min))
     (while (re-search-forward "^[A-Z][A-Z]*([0-9]*).*)$" nil t)
       (replace-match ""))

     (goto-char (dot-min))
     (while (re-search-forward "^Printed [0-9].*[0-9]$" nil t)
       (replace-match ""))

     (goto-char (dot-min))
     (while (re-search-forward "\n\n\n\n*" nil t)
       (replace-match "\n\n"))

     (goto-char (dot-min))
     (skip-chars-forward "\n")
     (delete-region (dot-min) (dot))
     (set-buffer-modified-p nil)
     (message ""))))

