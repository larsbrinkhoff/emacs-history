;; Replace commands for Emacs.
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


(fset 'delete-non-matching-lines 'keep-lines)
(defun keep-lines (regexp)
  "Delete lines not containing matches for REGEXP.
Applies to lines after dot."
  (interactive "sKeep lines (containing match for regexp): ")
  (save-excursion
    (while (not (eobp))
      (let ((end (scan-buffer (dot) 1 ?\n)))
	(if (re-search-forward regexp end t)
	    (goto-char end)
	  (delete-region (dot)
			 (if (re-search-forward regexp nil t)
			     (progn (beginning-of-line) (dot))
			   (dot-max))))))))

(fset 'delete-matching-lines 'flush-lines)
(defun flush-lines (regexp)
  "Delete lines containing matches for REGEXP.
Applies to lines after dot."
  (interactive "sFlush lines (containing match for regexp): ")
  (save-excursion
    (while (and (not (eobp))
		(re-search-forward regexp nil t))
      (beginning-of-line)
      (delete-region (dot)
		     (progn (forward-line 1) (dot))))))

(fset 'count-matches 'how-many)
(defun how-many (regexp)
  "Print number of matches for REGEXP following dot."
  (interactive "sHow many (matches for regexp): ")
  (let ((count 1) odot)
    (save-excursion
     (while (re-search-forward regexp nil t)
       (setq count (1+ count)))
     (message "%d occurrences" count))))

(fset 'list-matching-lines 'occur)
(defun occur (regexp &optional nlines)
  "Show all lines containing of REGEXP following dot.
Display each line with NLINES lines before and after.
NLINES defaults to 0.  Interactively it is the prefix arg."
  (interactive "sOccur (show lines matching regexp): \nP")
  (setq nlines (if nlines (prefix-numeric-value nlines) 0))
  (let ((first t))
   (with-output-to-temp-buffer "*Occur*"
    (save-excursion
     (while (re-search-forward regexp nil t)
      (let ((buffer (current-buffer))
	    (start
	     (save-excursion
	      (beginning-of-line)
	      (forward-line (- nlines))
	      (dot)))
	    (end
	     (save-excursion
	      (forward-line (1+ nlines))
	      (dot))))
	(save-excursion
	 (set-buffer standard-output)
	 (or first
	     (insert "--------\n"))
	 (setq first nil)
	 (insert-buffer-substring buffer start end))
	(forward-line 1)))))))

(defconst query-replace-help
  "Type Space to replace one match, Delete to skip to next,
ESC to exit, Period to replace one match and exit,
Comma to replace but not move dot immediately,
C-R to enter recursive edit, C-W to delete match and recursive edit,
! to replace all remaining matches with no more questions,
^ to move dot back to previous match."
  "Help message while in query-replace")

(defun perform-replace (from-string to-string
		        query-flag regexp-flag delimited-flag)
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(literal (not regexp-flag))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(keep-going t)
	(help-form
	 '(concat "Query replacing "
		  (if regexp-flag "regexp " "")
		  from-string " with " to-string ".\n\n"
		  query-replace-help)))
    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (push-mark)
    (while (and keep-going
		(not (eobp))
		(progn
		 (set-mark (dot))
		 (funcall search-function search-string nil t)))
      (undo-boundary)
      (if (not query-flag)
	  (replace-match to-string nocasify literal)
	(let (done replaced)
	  (while (not done)
	    (message "Query replacing %s with %s: " from-string to-string)
	    (setq char (read-char))
	    (if (not (memq char '(?\e ?\ ?\, ?\. ?! ?\177 ?\^r ?\^w ?^)))
		(progn (setq keep-going nil)
		       (setq unread-command-char char)
		       (setq done t)))
	    (if (= char ?\e)
		(progn (setq keep-going nil)
		       (setq done t)))
	    (if (= char ?^)
		(progn (goto-char (mark))
		       (setq replaced t)))
	    (if (= char ?\ )
		(progn (or replaced
			   (replace-match to-string nocasify literal))
		       (setq done t)))
	    (if (= char ?\.)
		(progn (or replaced
			   (replace-match to-string nocasify literal))
		       (setq keep-going nil)
		       (setq done t)))
	    (if (and (not replaced) (= char ?\,))
		(progn (replace-match to-string nocasify literal)
		       (setq replaced t)))
	    (if (= char ?!)
		(progn (or replaced
			   (replace-match to-string nocasify literal))
		       (setq done t query-flag nil)))
	    (if (= char ?\177)
		(setq done t))
	    (if (= char ?\^r)
		(store-match-data
		 (prog1 (match-data)
			(save-excursion (recursive-edit)))))
	    (if (= char ?\^w)
		(progn (delete-region (match-beginning 0) (match-end 0))
		       (store-match-data
			(prog1 (match-data)
			       (save-excursion (recursive-edit))))
		       (setq done t)))))))
    (pop-mark)
    (message "Done")
    keep-going))
