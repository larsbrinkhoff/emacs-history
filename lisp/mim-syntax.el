;; Syntax checker for Mim (MDL).
;; Copyright (C) 1985 Richard M. Stallman and K. Shane Hartman.
;; Bugs to shane@mit-xx (or bug-gnu-emacs@mit-prep)

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

(require 'mim-mode)

(defun slow-syntax-check-mim ()
  "Check Mim syntax slowly.
Points out the context of the error, if the syntax is incorrect."
  (interactive)
  (message "checking syntax...")
  (let ((stop (dot-max)) dot-stack current last-bracket whoops last-dot)
    (save-excursion
      (goto-char (dot-min))
      (while (and (not whoops)
		  (re-search-forward "\\s(\\|\\s)\\|\"\\|[\\]" stop t))
	(setq current (preceding-char))
	(cond ((= current ?\")
	       (condition-case nil
		   (progn (re-search-forward "[^\\]\"")
			  (setq current nil))
		 (error (setq whoops (dot)))))
	      ((= current ?\\)
	       (condition-case nil (forward-char 1) (error nil)))
	      ((= (char-syntax current) ?\))
	       (if (or (not last-bracket)
		       (not (= (logand (lsh (aref (syntax-table) last-bracket) -8)
				       ?\177)
			       current)))
		   (setq whoops (dot))
		 (setq last-dot (car dot-stack))
		 (setq last-bracket (if last-dot (char-after (1- last-dot))))
		 (setq dot-stack (cdr dot-stack))))
	      (t
	       (if last-dot (setq dot-stack (cons last-dot dot-stack)))
	       (setq last-dot (dot))
	       (setq last-bracket current)))))
    (cond ((not (or whoops last-dot))
	   (message "Syntax correct"))
	  (whoops
	   (goto-char whoops)
	   (cond ((equal current ?\")
		  (error "Unterminated string"))
		 ((not last-dot)
		  (error "Extraneous %s" (char-to-string current)))
		 (t
		  (error "Mismatched %s with %s"
			   (save-excursion
			     (setq whoops (1- (dot)))
			     (goto-char (1- last-dot))
			     (buffer-substring (dot)
					       (min (progn (end-of-line) (dot))
						    whoops)))
			   (char-to-string current)))))
	  (t
	   (goto-char last-dot)
	   (error "Unmatched %s" (char-to-string last-bracket))))))
      
(defun fast-syntax-check-mim ()
  "Checks Mim syntax quickly.
Answers correct or incorrect, cannot point out the error context."
  (interactive)
  (save-excursion
    (goto-char (dot-min))
    (let (state)
      (while (and (not (eobp))
		  (equal (car (setq state (parse-partial-sexp (dot) (dot-max) 0)))
			 0)))
      (if (equal (car state) 0)
	  (message "Syntax correct")
	(error "Syntax incorrect")))))


	
