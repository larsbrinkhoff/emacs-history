;; Lisp editing commands for Emacs
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


(defun forward-sexp (arg)
  "Move forward across one balanced expression.
With argument, do this that many times."
  (interactive "p")
  (goto-char (or (scan-sexps (dot) arg) (buffer-end arg)))
  (if (< arg 0) (backward-prefix-chars)))

(defun backward-sexp (arg)
  "Move backward across one balanced expression.
With argument, do this that many times."
  (interactive "p")
  (forward-sexp (- arg)))

(defun mark-sexp (arg)
  "Set mark ARG sexps from dot."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-sexp arg)
      (dot))))

(defun forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (goto-char (or (scan-lists (dot) arg 0) (buffer-end arg))))

(defun backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-list (- arg)))

(defun down-list (arg)
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (dot) inc -1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun backward-up-list (arg)
  "Move backward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  (interactive "p")
  (up-list (- arg)))

(defun up-list (arg) 
  "Move forward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (dot) inc 1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun kill-sexp (arg)
  "Kill the syntactic expression following the cursor.
With argument, kill that many expressions after (or before) the cursor."
  (interactive "p")
  (let ((odot (dot)))
    (forward-sexp arg)
    (kill-region odot (dot))))

(defun backward-kill-sexp (arg)
  "Kill the syntactic expression preceding the cursor.
With argument, kill that many expressions before (or after) the cursor."
  (interactive "p")
  (kill-sexp (- arg)))

(defun eval-last-sexp (arg)
  "Evaluate sexp before point; print value in minibuffer.
With argument, print output into current buffer."
  (interactive "P")
  (eval-region
   (let ((stab (syntax-table)))
     (unwind-protect
      (save-excursion
       (set-syntax-table lisp-mode-syntax-table)
       (forward-sexp -1)
       (dot))
      (set-syntax-table stab)))
   (dot)
   (if arg (current-buffer) t)))

(defun eval-defun (arg)
  "Evaluate defun that dot is in or before.
Print value in minibuffer.
With argument, insert value in current buffer after the defun."
  (interactive "P")
  (save-excursion
   (end-of-defun)
   (let ((end (dot)))
     (beginning-of-defun)
     (eval-region (dot) end
		  (if arg (current-buffer) t)))))

(defun beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (and (re-search-backward "^\\s(" nil 'move (or arg 1))
       (progn (beginning-of-line) t)))

(defun buffer-end (arg)
  (if (> arg 0) (dot-max) (dot-min)))

(defun end-of-defun (&optional arg)
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (dot) (dot-max)))
      (let ((pos (dot)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (beginning-of-defun 1)))
		    nil
		  (or (bobp) (forward-char -1))
		  (beginning-of-defun -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[;\n]")
		    (forward-line 1))
		(<= (dot) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (dot)))
	(beginning-of-defun 1)
	(forward-sexp 1)
	(goto-char (scan-buffer (dot) 1 ?\n))
	(if (>= (dot) pos)
	    (if (beginning-of-defun 2)
		(progn
		 (forward-list 1)
		 (skip-chars-forward " \t")
		 (if (looking-at "[;\n]")
		     (forward-line 1)))
	      (goto-char (dot-min)))))
      (setq arg (1+ arg)))))

(defun mark-defun ()
  "Put mark at end of defun, dot at beginning."
  (interactive)
  (push-mark (dot))
  (end-of-defun)
  (push-mark (dot))
  (beginning-of-defun)
  (re-search-backward "^\n" (- (dot) 1) t))

(defun insert-parentheses (arg)
  "Put parentheses around next ARG sexps.  Leave dot after open-paren.
No argument is equivalent to zero: just insert () and leave dot between."
  (interactive "P")
  (insert ?\()
  (save-excursion
    (if arg
	(forward-sexp (prefix-numeric-value arg)))
    (insert ?\))))

(defun move-past-close-and-reindent ()
  "Move past next ), delete indentation before it, then indent after it."
  (interactive)
  (up-list 1)
  (forward-char -1)
  (delete-indentation)
  (forward-char 1)
  (newline-and-indent))
