;; Spelling correction interface for Emacs.
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


(defun spell-buffer ()
  "Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped."
  (interactive)
  (spell-region (dot-min) (dot-max) "buffer"))

(defun spell-word ()
  "Check spelling of word at or after dot.
If it is not correct, ask user for the correct spelling
and query-replace the entire buffer to substitute it."
  (interactive)
  (let (beg end)
    (save-excursion
     (forward-word 1)
     (setq end (dot))
     (forward-word -1)
     (setq beg (dot)))
    (spell-region beg end (buffer-substring beg end))))

(defun spell-region (start end &optional description)
  "Like spell-buffer but applies only to region.
From program, applies from START to END."
  (interactive "r")
  (let ((buf (get-buffer-create " *temp*")))
    (save-excursion
     (set-buffer buf)
     (widen)
     (erase-buffer))
    (message "Checking spelling of %s..." (or description "region"))
    (if (= ?\n (char-after (1- end)))
	(call-process-region start end "/usr/bin/spell"
			     nil buf)
      (let ((oldbuf (current-buffer)))
	(save-excursion
	 (set-buffer buf)
	 (insert-buffer-substring oldbuf start end)
	 (insert ?\n)
	 (call-process-region (dot-min) (dot-max) "/usr/bin/spell"
			      t buf))))
    (message "Checking spelling of %s...%s"
	     (or description "region")
	     (if (save-excursion
		  (set-buffer buf)
		  (> (buffer-size) 0))
		 "not correct"
	       "correct"))
    (let (word newword
	  (case-fold-search t)
	  (case-replace t))
      (while (save-excursion
	      (set-buffer buf)
	      (> (buffer-size) 0))
	(save-excursion
	 (set-buffer buf)
	 (goto-char (dot-min))
	 (setq word (buffer-substring (dot)
				      (progn (end-of-line) (dot))))
	 (forward-char 1)
	 (delete-region (dot-min) (dot))
	 (setq newword (read-input (concat "Replacement for " word ": ")
				   word))
	 (flush-lines (concat "^" (regexp-quote word) "$")))
	(if (not (equal word newword))
	    (progn
	     (goto-char (dot-min))
	     (query-replace-regexp (concat "\\b" (regexp-quote word) "\\b")
				   newword)))))))


(defun spell-string (string)
  "Check spelling of string supplied as argument."
  (interactive "sSpell string: ")
  (let ((buf (get-buffer-create " *temp*")))
    (save-excursion
     (set-buffer buf)
     (widen)
     (erase-buffer)
     (insert string "\n")
     (call-process-region (dot-min) (dot-max) "/usr/bin/spell"
			  t t)
     (if (= 0 (buffer-size))
	 (message "%s is correct" string)
       (goto-char (dot-min))
       (while (search-forward "\n" nil t)
	 (replace-match " "))
       (message "%sincorrect" (buffer-substring 1 (dot-max)))))))
