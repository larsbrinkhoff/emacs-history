;; C code editing commands for Emacs
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


(defvar c-mode-syntax-table nil
  "Syntax table in use in C-mode buffers.")
(defvar c-mode-abbrev-table nil
  "Abbrev table in use in C-mode buffers.")
(defvar c-mode-map nil
  "Keymap used in C mode.")

(defconst c-indent-level 2
  "*Indentation of C statements with respect to containing block.")
(defconst c-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")
(defconst c-argdecl-indent 5
  "*Indentation level of declarations of C function arguments.")
(defconst c-label-offset -2
  "*Offset of C label lines and case statements relative to usual indentation.")
(defconst c-continued-statement-indent 2
  "*Extra indent for lines not starting new statements.")

(defconst c-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in C code.")

(defun c-mode ()
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Meta-Control-Q indents all lines within following bracket-grouping.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.

Meta-Control-H puts mark at end of C function and dot before it.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
 c-auto-newline
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-brace-offset
    Extra indentation for line if it starts with a brace.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default.

Turning on C mode calls the value of the variable c-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not c-mode-map)
      (progn
       (setq c-mode-map (make-sparse-keymap))
       (define-key c-mode-map "{" 'electric-c-brace)
       (define-key c-mode-map "}" 'electric-c-brace)
       (define-key c-mode-map ";" 'electric-c-semi)
       (define-key c-mode-map ":" 'electric-c-terminator)
       (define-key c-mode-map "\e\^h" 'mark-c-function)
       (define-key c-mode-map "\e\^q" 'indent-c-exp)
       (define-key c-mode-map "\177" 'backward-delete-char-untabify)
       (define-key c-mode-map "\t" 'c-indent-line)))
  (use-local-map c-mode-map)
  (setq major-mode 'c-mode)
  (setq mode-name "C")
  (define-abbrev-table 'c-mode-abbrev-table ())
  (setq local-abbrev-table c-mode-abbrev-table)
  (if (not c-mode-syntax-table)
      (let ((i 0))
	(setq c-mode-syntax-table (make-syntax-table))
	(set-syntax-table c-mode-syntax-table)
	(modify-syntax-entry ?{ "(}")
	(modify-syntax-entry ?} "){")
	(modify-syntax-entry ?[ "(]")
	(modify-syntax-entry ?] ")[")
	(modify-syntax-entry ?\\ "\\")
	(modify-syntax-entry ?/ "  14")
	(modify-syntax-entry ?* "  23")
	(modify-syntax-entry ?+ " ")
	(modify-syntax-entry ?- " ")
	(modify-syntax-entry ?= " ")
	(modify-syntax-entry ?% " ")
	(modify-syntax-entry ?< " ")
	(modify-syntax-entry ?> " ")
	(modify-syntax-entry ?\' "\""))
    (set-syntax-table c-mode-syntax-table))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (and (boundp 'c-mode-hook)
       c-mode-hook
       (funcall c-mode-hook)))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in C code
;; based on its context.
(defun c-comment-indent ()
  (save-excursion
   (if (and (bolp) (not (eolp)))
       0				;Existing comment at bol stays there.
     (skip-chars-backward " \t")
     (if (bolp)				;Line empty except for comment =>
	 (calculate-c-indent 0)		; indent it like code.
       (max (1+ (current-column))	;Else indent at comment column
	    comment-column)))))		; except leave at least one space.

(defun electric-c-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		  (skip-chars-backward " \t")
		  (bolp))
		 (progn
		  (if c-auto-newline (newline))
		  c-auto-newline)))
	(progn
	 (insert last-command-char)
	 (c-indent-line nil)
	 (if c-auto-newline
	     (progn
	      (setq insertpos (1- (dot)))
	      (newline)
	      (c-indent-line nil)))
	 (save-excursion
	  (if insertpos (goto-char (1+ insertpos)))
	  (delete-char -1))))
    (if insertpos
	(save-excursion
	 (goto-char insertpos)
	 (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-c-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (electric-c-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-c-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (dot)))
    (if (and (not arg) (eolp)
	     (not (save-excursion (beginning-of-line)
				  (skip-chars-forward " \t")
				  (or (= (following-char) ?#)
				      (let ((pps (parse-partial-sexp (dot) end)))
					(or (nth 3 pps) (nth 4 pps) (nth 5 pps)))))))
	(progn
	 (insert last-command-char)
	 (c-indent-line nil)
	 (and c-auto-newline
	      (not (c-inside-parens-p))
	      (progn
	       (setq insertpos (1- (dot)))
	       (newline)
	       (c-indent-line nil)))
	 (save-excursion
	  (if insertpos (goto-char (1+ insertpos)))
	  (delete-char -1))))
    (if insertpos
	(save-excursion
	 (goto-char insertpos)
	 (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun c-inside-parens-p ()
  (condition-case ()
      (save-excursion
       (save-restriction
	(narrow-to-region (dot)
			  (progn (beginning-of-defun) (dot)))
	(goto-char (dot-max))
	(= (char-after (or (scan-lists (dot) -1 1) (dot-min))) ?\()))
    (error nil)))

(defun c-indent-line (&optional whole-exp)
  "Indent current line as C code.
Argument means shift any additional lines of grouping
rigidly with thls line."
  (interactive "P")
  (let ((indent (calculate-c-indent))
	beg end shift-amt
	(pos (- (dot-max) (dot))))
    (beginning-of-line)
    (setq beg (dot))
    (if (or (null indent)
	    (eq indent t)
	    (looking-at "#"))
	(if (eq indent t)
	    ;; Line starts inside a comment.
	    (setq indent
		  (let (end star-start)
		    (save-excursion
		     (beginning-of-line)
		     (skip-chars-forward " \t")
		     (setq star-start (= (following-char) ?\*))
		     (skip-chars-backward " \t\n")
		     (setq end (dot))
		     (beginning-of-line)
		     (skip-chars-forward " \t")
		     (and (re-search-forward "/\\*[ \t]*" end t)
			  star-start
			  (goto-char (1+ (match-beginning 0))))
		     (current-column))))
	  (setq indent (current-indentation)))
      (skip-chars-forward " \t")
      (if (listp indent) (setq indent (car indent)))
      (if (or (looking-at "case[ \t]")
	      (and (looking-at "[A-Za-z]")
		   (save-excursion
		    (forward-sexp 1)
		    (looking-at ":"))))
	  (setq indent (max 1 (+ indent c-label-offset))))
      (if (= (following-char) ?})
	  (setq indent (- indent c-indent-level)))
      (if (= (following-char) ?{)
	  (setq indent (+ indent c-brace-offset))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (dot-max) pos) (dot))
	    (goto-char (- (dot-max) pos)))
      (delete-region beg (dot))
      (indent-to indent)
      ;; If initial dot was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (dot-max) pos) (dot))
	  (goto-char (- (dot-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp
	   (save-excursion
	    (goto-char beg)
	    (forward-sexp 1)
	    (setq end (dot))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (dot))
	    (> end beg))
	   (indent-code-rigidly beg end shift-amt "#")))))

(defun calculate-c-indent (&optional parse-start)
  "Return appropriate indentation for current line as C code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (dot))
	  state
	  parse-start
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (dot) indent-point)
	(setq parse-start (dot))
	(setq state (parse-partial-sexp (dot) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (if (or (nth 3 state) (nth 4 state))
	  ;; return nil or t if should not change this line
	  (nth 4 state)
	(if (null containing-sexp)
	    ;; Line is at top level.  May be data or function definition,
	    ;; or may be function argument declaration.
	    ;; Indent like the previous top level line
	    ;; unless that ends in a closeparen without semicolon,
	    ;; in which case this line is the first argument decl.
	    (progn
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?{)
		 0   ;; Unless it starts a function body
	       (c-backward-to-noncomment (or parse-start (dot-min)))
	       (if (= (preceding-char) ?\))
		   c-argdecl-indent
		 (current-indentation))))
	  (if (/= (char-after containing-sexp) ?{)
	      ;; line is expression, not statement:
	      ;; indent to just after the surrounding open.
	      (progn (goto-char (1+ containing-sexp))
		     (current-column))
	    ;; Statement.  Find previous non-comment character.
	    (goto-char indent-point)
	    (c-backward-to-noncomment containing-sexp)
	    (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?\{)))
		;; This line is continuation of preceding line's statement;
		;; indent 2 more than the previous line of the statement.
		(progn
		  (c-backward-to-start-of-if containing-sexp)
		  (+ c-continued-statement-indent (current-column)))
	      ;; This line starts a new statement.
	      ;; Position following last unclosed open.
	      (goto-char containing-sexp)
	      ;; Is line first statement after an open-brace?
	      (or
	       ;; If no, find that first statement and indent like it.
	       (save-excursion
		(forward-char 1)
		(while (progn (skip-chars-forward " \t\n")
			      (looking-at "#\\|/\\*\\|case[ \t\n]\\|[a-zA-Z0-9_$]*:"))
		  ;; Skip over comments and labels following openbrace.
		  (if (= (following-char) ?\#)
		      (forward-line 1)
		    (if (looking-at "/\\*")
			(search-forward "*/" nil 'move)
		      (search-forward ":"))))
		;; The first following code counts
		;; if it is before the line we want to indent.
		(and (< (dot) indent-point)
		     (current-column)))
	       ;; If no previous statement,
	       ;; indent it relative to line brace is on.
	       (+ c-indent-level
		  (current-indentation))))))))))

(defun c-backward-to-noncomment (lim)
  (let (odot stop)
    (while (not stop)
      (skip-chars-backward " \t\n" lim)
      (setq odot (dot))
      (if (and (>= (dot) (+ 2 lim))
	       (save-excursion
		(forward-char -2)
		(looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(beginning-of-line)
	(skip-chars-forward " \t")
	(if (looking-at "#")
	    (setq stop (<= (dot) lim))
	  (setq stop t)
	  (goto-char odot))))))   

(defun c-backward-to-start-of-if (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (dot) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun mark-c-function ()
  "Put mark at end of C function, dot at beginning."
  (interactive)
  (push-mark (dot))
  (end-of-defun)
  (push-mark (dot))
  (beginning-of-defun)
  (backward-paragraph))

(defun indent-c-exp ()
  "Indent each line of the C grouping following dot."
  (interactive)
  (let ((indent-stack (list nil))
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	(odot (dot))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (dot) (progn (end-of-line) (dot))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (car (nthcdr 4 ostate))
	      (c-indent-line))
	  (if (car (nthcdr 4 state))
	      (progn
	       (if (not (car (nthcdr 4 ostate)))
		   (indent-for-comment))
	       (forward-line 1))
	    (if (car (nthcdr 3 state))
		(forward-line 1)
	      (setq innerloop-done t))))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Statement is on an existing nesting level.
		;; All we have to check now is whether it is really a new statement.
		;; Find last non-comment character before this line
		(save-excursion
		  (c-backward-to-noncomment odot)
		  (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?:)))
		      ;; Preceding line did not end in comma or semi;
		      ;; indent this line 2 more than previous.
		      (progn
			(c-backward-to-start-of-if odot)
			(setq this-indent (+ 2 (current-indentation))))
		    ;; Preceding line ended in comma or semi;
		    ;; use the standard indent for this level.
		    (setq this-indent (car indent-stack))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-c-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (or (looking-at "case[ \t]")
		    (and (looking-at "[A-Za-z]")
			 (save-excursion
			  (forward-sexp 1)
			  (looking-at ":"))))
		(setq this-indent (max 1 (+ this-indent c-label-offset))))
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent c-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent c-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		 (delete-region (dot) (progn (beginning-of-line) (dot)))
		 (indent-to this-indent)))))))))
