;; Page motion commands for emacs.
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


(defun forward-page (&optional count)
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp  page-delimiter."
  (interactive "p")
  (or count (setq count 1))
  (while (and (> count 0) (not (eobp)))
    (if (re-search-forward page-delimiter nil t)
	nil
      (goto-char (dot-max)))
    (setq count (1- count)))
  (while (and (< count 0) (not (bobp)))
    (forward-char -1)
    (if (re-search-backward page-delimiter nil t)
	(goto-char (match-end 0))
      (goto-char (dot-min)))
    (setq count (1+ count))))

(defun backward-page (&optional count)
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any line whose beginning matches the regexp  page-delimiter."
  (interactive "p")
  (or count (setq count 1))
  (forward-page (- count)))

(defun mark-page (arg)
  "Put mark at end of page, dot at beginning."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (if (> arg 0)
      (forward-page arg)
    (if (< arg 0)
        (forward-page (1- arg))))
  (forward-page)
  (set-mark (dot))
  (forward-page -1))

(defun narrow-to-page (arg)
  "Make text outside current page invisible."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
   (if (> arg 0)
       (forward-page arg)
     (if (< arg 0)
	 (forward-page (1- arg))))
   (forward-page)
   (beginning-of-line)
   (narrow-to-region (dot)
		     (progn
		      (forward-page -1)
		      (dot)))))

(defun count-lines-page ()
  "Report number of lines on current page, and how many are before or after dot."
  (interactive)
  (save-excursion
    (let ((odot (dot)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (setq end (dot))
      (backward-page)
      (setq beg (dot))
      (setq total (count-lines beg end)
	    before (count-lines beg odot)
	    after (count-lines odot end))
      (message "Page has %d lines (%d + %d)" total before after))))

(defun what-page ()
  "Print page and line number of dot."
  (interactive)
  (let ((count 1) odot)
    (save-restriction
      (widen)
      (save-excursion
	(beginning-of-line)
	(setq odot (dot))
	(goto-char 1)
	(while (re-search-forward page-delimiter odot t)
	  (setq count (1+ count)))
	(message "Page %d, line %d" count (1+ (count-lines (dot) odot)))))))
