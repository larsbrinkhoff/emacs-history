;; Lisp mode, and its idiosyncratic commands.
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

(defvar lisp-mode-syntax-table nil "")
(defvar lisp-mode-abbrev-table nil "")

(if (not lisp-mode-syntax-table)
    (let ((i 0))
      (setq lisp-mode-syntax-table (make-syntax-table))
      (set-syntax-table lisp-mode-syntax-table)
      (while (< i ?0)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    ")
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f ">   ")
      (modify-syntax-entry ?\; "<   ")
      (modify-syntax-entry ?` "'   ")
      (modify-syntax-entry ?' "'   ")
      (modify-syntax-entry ?, "'   ")
      (modify-syntax-entry ?. "'   ")
      (modify-syntax-entry ?# "'   ")
      (modify-syntax-entry ?\" "\"    ")
      (modify-syntax-entry ?\\ "\\   ")
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")))

(define-abbrev-table 'lisp-mode-abbrev-table ())

(defun lisp-mode-variables ()
  (set-syntax-table lisp-mode-syntax-table)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'lisp-comment-indent))

(defun lisp-mode-commands (map)
  (define-key map "\e\C-q" 'indent-sexp)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\t" 'lisp-indent-line))

(defvar emacs-lisp-mode-map nil "")

(defun emacs-lisp-mode ()
  "Major mode for editing Lisp code to run in Emacs.
Commands:
Delete converts tabs to spaces as it moves back.
\\[lisp-indent-line] indents for Lisp; with argument, shifts rest
 of expression rigidly with the current line.
\\[indent-sexp] does Tab on each line starting within following expression.
\\[eval-defun] evaluates the largest Lisp expression around or after dot.
Blank lines separate paragraphs.  Semicolons start comments.

Entry to this mode calls the value of emacs-lisp-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not emacs-lisp-mode-map)
      (progn (setq emacs-lisp-mode-map (make-sparse-keymap))
	     (define-key emacs-lisp-mode-map "\e\C-x" 'eval-defun)
	     (lisp-mode-commands emacs-lisp-mode-map)))
  (use-local-map emacs-lisp-mode-map)
  (setq major-mode 'emacs-lisp-mode)
  (setq mode-name "Emacs-Lisp")
  (lisp-mode-variables)
  (and (boundp 'emacs-lisp-mode-hook)
       emacs-lisp-mode-hook
       (funcall emacs-lisp-mode-hook)))

(defvar lisp-mode-map nil)

(defun lisp-mode ()
  "Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
\\[lisp-indent-line] indents for Lisp; with argument, shifts rest
 of expression rigidly with the current line.
\\[indent-sexp] does Tab on each line starting within following expression.
\\[lisp-send-defun] sends the top-level expression around or after dot
 to the subprocess named `lisp'.
Blank lines separate paragraphs.  Semicolons start comments.

Entry to this mode calls the value of lisp-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not lisp-mode-map)
      (progn (setq lisp-mode-map (make-sparse-keymap))
	     (define-key lisp-mode-map "\e\C-x" 'lisp-send-defun)
	     (lisp-mode-commands lisp-mode-map)))
  (use-local-map lisp-mode-map)
  (setq major-mode 'lisp-mode)
  (setq mode-name "Lisp")
  (lisp-mode-variables)
  (and (boundp 'lisp-mode-hook)
       lisp-mode-hook
       (funcall lisp-mode-hook)))

;; This will do unless shell.el is loaded.
(defun lisp-send-defun nil
  "Send the current defun to the Lisp process made by M-x run-lisp."
  (interactive)
  (error "Process lisp does not exist"))

; scheme mode now exists
;(fset 'scheme-mode 'lisp-mode)

(defvar lisp-interaction-mode-map nil)

(defun lisp-interaction-mode ()
  "Major mode for typing and evaluating Lisp forms.
Like Lisp mode except that \\[eval-print-last-sexp] evals the Lisp expression
before dot, and prints its value into the buffer, advancing dot.

Commands:
\\[eval-print-last-sexp] as above.
Delete converts tabs to spaces as it moves back.
\\[lisp-indent-line] indents for Lisp; with argument, shifts rest
 of expression rigidly with the current line.
\\[indent-sexp] does Tab on each line starting within following expression.
\\[eval-defun] evaluates the largest Lisp expression around or after dot.
Paragraphs are separated only by blank lines.  Semicolons start comments.

Entry to this mode calls the value of lisp-interaction-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not lisp-interaction-mode-map)
      (progn
	(setq lisp-interaction-mode-map (make-sparse-keymap))
	(lisp-mode-commands lisp-interaction-mode-map)
	(define-key lisp-interaction-mode-map "\e\C-x" 'eval-defun)
	(define-key lisp-interaction-mode-map "\n" 'eval-print-last-sexp)))
  (use-local-map lisp-interaction-mode-map)
  (setq major-mode 'lisp-interaction-mode)
  (setq mode-name "Lisp Interaction")
  (lisp-mode-variables)
  (and (boundp 'lisp-interaction-mode-hook)
       lisp-interaction-mode-hook
       (funcall lisp-interaction-mode-hook)))

(defun eval-print-last-sexp (arg)
  "Evaluate sexp before point; print value into current buffer."
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
   (current-buffer)))

(defun lisp-comment-indent ()
  (if (looking-at ";;;")
      (current-column)
    (if (looking-at ";;")
	(let ((tem (calculate-lisp-indent)))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (1+ (current-column)) comment-column))))

(defconst lisp-indent-offset nil "")
(defconst lisp-indent-hook 'lisp-indent-hook "")

(defun lisp-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt beg end
	(pos (- (dot-max) (dot))))
    (beginning-of-line)
    (setq beg (dot))
    (skip-chars-forward " \t")
    (if (looking-at "[ \t]*;;;")
	;; Don't alter indentation of a ;;; comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (dot))
	(indent-to indent))
      ;; If initial dot was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (dot-max) pos) (dot))
	  (goto-char (- (dot-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (dot))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (dot))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))

(defun calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (dot)) state paren-depth desired-indent (retry t)
	  last-sexp containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (dot) indent-point)
	(setq state (parse-partial-sexp (dot) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
	(setq retry nil)
	(setq last-sexp (nth 2 state))
	(setq containing-sexp (car (cdr state)))
	;; Position following last unclosed open.
	(goto-char (1+ containing-sexp))
	;; Is there a complete sexp since then?
	(if (and last-sexp (> last-sexp (dot)))
	    ;; Yes, but is there a containing sexp after that?
	    (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
	      (if (setq retry (car (cdr peek))) (setq state peek))))
	(if (not retry)
	    ;; Innermost containing sexp found
	    (progn
	      (goto-char (1+ containing-sexp))
	      (if (not last-sexp)
		  ;; indent-point immediately follows open paren.
		  ;; Don't call hook.
		  (setq desired-indent (current-column))
		;; Move to first sexp after containing open paren
		(parse-partial-sexp (dot) last-sexp 0 t)
		(cond
		 ((looking-at "\\s(")
		  ;; Looking at a list.  Don't call hook.
		  (if (not (> (save-excursion (forward-line 1) (dot)) last-sexp))
		      (progn (goto-char last-sexp)
			     (beginning-of-line)
			     (parse-partial-sexp (dot) last-sexp 0 t)))
		  ;; Indent under the list or under the first sexp on the
		  ;; same line as last-sexp.  Note that first thing on that
		  ;; line has to be complete sexp since we are inside the
		  ;; innermost containing sexp.
		  (backward-prefix-chars)
		  (setq desired-indent (current-column)))
		 ((> (save-excursion (forward-line 1) (dot)) last-sexp)
		  ;; Last sexp is on same line as containing sexp.
		  ;; It's almost certainly a function call.
		  (parse-partial-sexp (dot) last-sexp 0 t)
		  (if (/= (dot) last-sexp)
		      ;; Indent beneath first argument or, if only one sexp
		      ;; on line, indent beneath that.
		      (progn (forward-sexp 1)
			     (parse-partial-sexp (dot) last-sexp 0 t)))
		  (backward-prefix-chars))
		 (t
		  ;; Indent beneath first sexp on same line as last-sexp.
		  ;; Again, it's almost certainly a function call.
		  (goto-char last-sexp)
		  (beginning-of-line)
		  (parse-partial-sexp (dot) last-sexp 0 t)
		  (backward-prefix-chars)))))))
      ;; Dot is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond ((car (nthcdr 3 state))
	     ;; Inside a string, don't change indentation.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (setq desired-indent (current-column)))
	    ((and (integerp lisp-indent-offset) containing-sexp)
	     ;; Indent by constant offset
	     (goto-char containing-sexp)
	     (setq desired-indent (+ lisp-indent-offset (current-column))))
	    ((not (or desired-indent
		      (and (fboundp 'lisp-indent-hook)
			   (not retry)
			   (setq desired-indent
				 (funcall lisp-indent-hook indent-point state)))))
	     ;; Use default indentation if not computed yet
	     (setq desired-indent (current-column))))
      desired-indent)))

(defun lisp-indent-hook (indent-point state)
  (let ((normal-indent (current-column)))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (dot) (car (cdr state)))
	  (let ((function (buffer-substring (progn (forward-char -1) (dot))
					    (progn (forward-sexp 1) (dot))))
		method)
	    (setq method (get (intern-soft function) 'lisp-indent-hook))
	    (if (or (eq method 'defun)
		    (and (null method)
			 (> (length function) 3)
			 (string-equal (substring function 0 3) "def")))
		(lisp-indent-defform state indent-point)
	      (if (integerp method)
		  (lisp-indent-specform method state indent-point)
		(if method
		    (funcall method state indent-point)))))))))

(defconst lisp-body-indent 2 "")

(defun lisp-indent-specform (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
	body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (dot) indent-point 1 t)
    (while (and (< (dot) indent-point)
		(condition-case nil
		    (progn
		      (setq count (1- count))
		      (forward-sexp 1)
		      (parse-partial-sexp (dot) indent-point 1 t))
		  (error nil))))
    ;; Dot is sitting on first character of last (or count) sexp.
    (if (> count 0)
	;; A distinguished form.  If it is the first or second form use double
	;; lisp-body-indent, else normal indent.  With lisp-body-indent bound
	;; to 2 (the default), this just happens to work the same with if as
	;; the older code, but it makes unwind-protect, condition-case,
	;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
	;; less hacked, behavior can be obtained by replacing below with
	;; (list normal-indent containing-form-start).
	(if (<= (- i count) 1)
	    (list (+ containing-form-column (* 2 lisp-body-indent))
		  containing-form-start)
	  (list normal-indent containing-form-start))
      ;; A non-distinguished form. Use body-indent if there are no distinguished
      ;; forms and this is the first undistinguished form, or if this is the
      ;; first undistinguished form and the preceding distinguished form has
      ;; indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
	      (and (= count 0) (<= body-indent normal-indent)))
	  body-indent
	normal-indent))))

(defun lisp-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (dot) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ lisp-body-indent (current-column)))))

;; (put 'progn 'lisp-indent-hook 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'lambda 'lisp-indent-hook 'defun)
(put 'progn 'lisp-indent-hook 0)
(put 'prog1 'lisp-indent-hook 1)
(put 'save-excursion 'lisp-indent-hook 0)
(put 'save-window-excursion 'lisp-indent-hook 0)
(put 'save-restriction 'lisp-indent-hook 0)
(put 'let 'lisp-indent-hook 1)
(put 'let* 'lisp-indent-hook 1)
(put 'while 'lisp-indent-hook 1)
(put 'if 'lisp-indent-hook 2)
(put 'catch 'lisp-indent-hook 1)
(put 'condition-case 'lisp-indent-hook 2)
(put 'unwind-protect 'lisp-indent-hook 1)
(put 'with-output-to-temp-buffer 'lisp-indent-hook 1)

(defun indent-sexp ()
  "Indent each line of the list starting just after dot."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) bol
	outer-loop-done inner-loop-done state this-indent)
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
	(setq last-depth next-depth
	      innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (dot) (progn (end-of-line) (dot))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  (if (car (nthcdr 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (setq outer-loop-done (<= next-depth 0))
	    nil
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  (forward-line 1)
	  (setq bol (dot))
	  (skip-chars-forward " \t")
	  (if (or (eobp) (looking-at "[;\n]"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-lisp-indent
			  (if (car indent-stack) (- (car indent-stack))))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (dot))
		       (indent-to this-indent)))))))))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings,
assuming that the start of the region is not inside them.
Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (dot-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (dot)
					  (progn
					    (forward-line 1) (dot))
					  nil nil state)))
      (while (< (dot) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (dot) (progn (skip-chars-forward " \t") (dot)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (dot)
					(progn
					  (forward-line 1) (dot))
					nil nil state))))))

