;; Scheme mode, and its idiosyncratic commands.
;; Copyright (C) 1985 Bill Rozas & Richard M. Stallman

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

;; Initially a query replace of Lisp mode, except for the indentation 
;; of special forms.  Probably the code should be merged at some point 
;; so that there is sharing between both libraries.

(defvar scheme-mode-syntax-table nil "")
(defvar scheme-mode-abbrev-table nil "")

(if (not scheme-mode-syntax-table)
    (let ((i 0))
      (setq scheme-mode-syntax-table (make-syntax-table))
      (set-syntax-table scheme-mode-syntax-table)
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

(define-abbrev-table 'scheme-mode-abbrev-table ())

(defvar scheme-mode-map nil "")
(defvar scheme-interaction-mode-map nil "")

(defun scheme-mode ()
  "Major mode for editing Scheme code.
Commands:
Delete converts tabs to spaces as it moves back.
\\[scheme-indent-line] indents for Scheme; with argument, shifts rest
 of expression rigidly with the current line.
\\[scheme-send-buffer] zaps whole buffer and resumes Scheme.
\\[scheme-zap-define] zaps the current definition.
\\[scheme-indent-sexp] scheme-indent on each line starting within following expression.
\\[find-scheme-definition] finds a scheme definition.
\\[scheme-send-define] zaps the current definition and resumes Scheme.
\\[resume-scheme] resumes Scheme.
Blank lines separate paragraphs.  Semicolons start comments.

Entry to this mode calls the value of scheme-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not scheme-mode-map)
      (progn (setq scheme-mode-map (make-sparse-keymap))
	     (define-key scheme-mode-map "\eo" 'scheme-send-buffer)
	     (define-key scheme-mode-map "\ez" 'scheme-zap-define)
	     (define-key scheme-mode-map "\e\C-q" 'scheme-indent-sexp)
	     (define-key scheme-mode-map "\e\C-s" 'find-scheme-definition)
	     (define-key scheme-mode-map "\e\C-y" 'scheme-send-define)
	     (define-key scheme-mode-map "\e\C-z" 'resume-scheme)
	     (define-key scheme-mode-map "\177"
	       'backward-delete-char-untabify)
	     (define-key scheme-mode-map "\t" 'scheme-indent-line)))
  (use-local-map scheme-mode-map)
  (set-syntax-table scheme-mode-syntax-table)
  (setq major-mode 'scheme-mode)
  (setq mode-name "Scheme")
  (setq local-abbrev-table scheme-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'scheme-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'scheme-comment-indent)
  (and (boundp 'scheme-mode-hook)
       scheme-mode-hook
       (funcall scheme-mode-hook)))

(defun scheme-comment-indent (&optional pos)
  (save-excursion
    (if pos (goto-char pos))
    (if (looking-at ";;;")
	(current-column)
      (if (looking-at ";;")
	  (let ((tem (calculate-scheme-indent)))
	    (if (listp tem) (car tem) tem))
	comment-column))))

(defconst scheme-indent-offset nil "")
(defconst scheme-indent-hook 'scheme-indent-hook "")

(defun scheme-indent-line (&optional whole-exp)
  "Indent current line as scheme code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-scheme-indent)) shift-amt beg end
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

(defun calculate-scheme-indent (&optional parse-start)
  "Return appropriate indentation for current line as scheme code.
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
      ;; Call indentation hook except when overriden by scheme-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond ((car (nthcdr 3 state))
	     ;; Inside a string, don't change indentation.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (setq desired-indent (current-column)))
	    ((and (integerp scheme-indent-offset) containing-sexp)
	     ;; Indent by constant offset
	     (goto-char containing-sexp)
	     (setq desired-indent (+ scheme-indent-offset (current-column))))
	    ((not (or desired-indent
		      (and (boundp 'scheme-indent-hook)
			   (not retry)
			   (setq desired-indent
				 (funcall scheme-indent-hook
					  indent-point state)))))
	     ;; Use default indentation if not computed yet
	     (setq desired-indent (current-column))))
      desired-indent)))

(defun scheme-indent-hook (indent-point state)
  (let ((normal-indent (current-column)))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (dot) (car (cdr state)))
	  (let ((function (buffer-substring (progn (forward-char -1) (dot))
					    (progn (forward-sexp 1) (dot))))
		method)
	    (setq method (get (intern-soft function) 'scheme-indent-hook))
	    (if (or (memq method '(define DEFINE))
		    (and (null method)
			 (> (length function) 3)
			 (or (string-equal (substring function 0 3) "def")
			     (string-equal (substring function 0 3) "DEF"))))
		(scheme-indent-defform state indent-point)
	      (if (integerp method)
		  (scheme-indent-specform method state indent-point)
		(if method
		    (funcall method state indent-point)))))))))

(defconst scheme-body-indent 2 "")

(defun scheme-indent-specform (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
	body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  scheme-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ scheme-body-indent containing-form-column))
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
	;; A distinguished form. If it is the first or second form
	;; use double scheme-body-indent, else normal indent. With
	;; scheme-body-indent bound to 2 (the default), this just
	;; happens to work the same with if as the older code, but it
	;; makes unwind-protect, condition-case,
	;; with-output-to-temp-buffer, et. al. much more tasteful.
	;; The older, less hacked, behavior can be obtained by
	;; replacing below with (list normal-indent containing-form-start).
	(if (<= (- i count) 1)
	    (list (+ containing-form-column (* 2 scheme-body-indent))
		  containing-form-start)
	  (list normal-indent containing-form-start))
      ;; A non-distinguished form. Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished
      ;; form, or if this is the first undistinguished form and
      ;; the preceding distinguished form has indentation at
      ;; least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
	      (and (= count 0) (<= body-indent normal-indent)))
	  body-indent
	normal-indent))))

(defun scheme-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (dot) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ scheme-body-indent (current-column)))))

;;; Let is different in Scheme

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

;; Assumes that protected by a save-excursion

(defun next-sexp-as-string ()
  (forward-sexp 1)
  (let ((the-end (dot)))
    (backward-sexp 1)
    (buffer-substring (dot) the-end)))

(defun scheme-let-indent (state indent-point)
  (if (would-be-symbol (next-sexp-as-string))
      (scheme-indent-specform 2 state indent-point)
      (scheme-indent-specform 1 state indent-point)))

;; (put 'sequence 'scheme-indent-hook 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).
     
(put 'fluid-let 'scheme-indent-hook 1)
(put 'if 'scheme-indent-hook 3)
(put 'lambda 'scheme-indent-hook 1)
(put 'let 'scheme-indent-hook 'scheme-let-indent)
(put 'named-lambda 'scheme-indent-hook 1)
(put 'sequence 'scheme-indent-hook 0)

(put 'COND 'scheme-indent-hook (get 'cond 'scheme-indent-hook))
(put 'FLUID-LET 'scheme-indent-hook 1)
(put 'IF 'scheme-indent-hook 3)
(put 'LAMBDA 'scheme-indent-hook 1)
(put 'LET 'scheme-indent-hook 'scheme-let-indent)
(put 'NAMED-LAMBDA 'scheme-indent-hook 1)
(put 'SEQUENCE 'scheme-indent-hook 0)

(defun scheme-indent-sexp ()
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
	      (let ((val (calculate-scheme-indent
			  (if (car indent-stack) (- (car indent-stack))))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (dot))
		       (indent-to this-indent)))))))))

;;; Schedit commands

(defconst scheme-zap-name "fromedit.zap"
  "Name of transfer file between Scheme and Emacs")

(defconst scheme-invocation-string "%scheme"
  "*String to give to the Cshell to proceed a sibling Scheme")

(defun goto-parallel-scheme-fork ()
  (suspend-emacs scheme-invocation-string))

;; This currently assumes that Emacs runs as an inferior to Scheme

(fset 'goto-scheme 'suspend-emacs)

;; if not, do (fset 'goto-scheme 'goto-parallel-scheme-fork)

(defun resume-scheme ()
  "Suspend Emacs and resume Scheme"
  (interactive)
  (let ((zap-buffer (get-buffer scheme-zap-name))
	(this-buffer (current-buffer)))
    (if zap-buffer
	(save-excursion
	  (unwind-protect
	      (progn (set-buffer zap-buffer)
		     (or buffer-file-name
			 (setq buffer-file-name scheme-zap-name))
		     (save-buffer)
		     (erase-buffer)
		     (setq buffer-modified-p nil))
	    (set-buffer this-buffer)))))
  (goto-scheme))

(defun scheme-do-zap-region (start end buffer &optional separate)
  "Internal routine which zaps a region of text for Scheme."
  (let ((the-text (buffer-substring start end)))
    (save-excursion
      (unwind-protect
	  (progn (set-buffer (get-buffer-create scheme-zap-name))
		 (insert-string the-text)
		 (if separate (newline 2)))
	(set-buffer buffer)))))

(defun scheme-zap-region (start end)
  "Zap region between point and mark into Scheme."
  (interactive "r")
  (scheme-do-zap-region start end (current-buffer)))

(defun scheme-zap-expression (arg)
  "Zap sexp before point into Scheme."
  (interactive "P")
  (scheme-do-zap-region
   (let ((stab (syntax-table)))
     (unwind-protect
	 (save-excursion
	   (set-syntax-table lisp-mode-syntax-table)
	   (forward-sexp -1)
	   (dot))
       (set-syntax-table stab)))
   (dot)
   (current-buffer)
   t))

(defun scheme-zap-define (arg)
  "Zap current definition into Scheme."
  (interactive "P")
  (let ((stab (syntax-table)))
    (unwind-protect
	(save-excursion
	  (set-syntax-table scheme-mode-syntax-table)
	  (if (not (= (dot) (dot-max))) (forward-char 1))
	  (beginning-of-defun 1)
	  (let ((start (dot)))
	    (forward-sexp 1)
	    (scheme-do-zap-region start
				  (dot)
				  (current-buffer)
				  t)))
      (set-syntax-table stab))))

(defun scheme-send-buffer (arg)
  "Zap whole buffer and resume Scheme"
  (interactive "P")
  (scheme-do-zap-region (dot-min)
			(dot-max)
			(current-buffer))
  (resume-scheme))

(defun scheme-send-define (arg)
  "Zap current definition and resume Scheme"
  (interactive "P")
  (scheme-zap-define arg)
  (resume-scheme))

(defun defining_p ()
  (save-excursion
    (let* ((here (dot))
	   (name (buffer-substring (progn (backward-sexp 1) (dot)) here)))
      (beginning-of-defun 1)
      (if (char-equal (char-after (dot)) ?\()
	  (progn (forward-char 1)
		 (let ((sub (substring (next-sexp-as-string) 0 3)))
		   (if (or (string-equal sub "def") (string-equal sub "DEF"))
		       (progn (forward-sexp 1)
			      (forward-word 1)
			      (backward-word 1)
			      (string-equal name
					    (next-sexp-as-string))))))))))

(defun find-scheme-definition (name)
  "Find the definition of its argument in the current buffer"
  (interactive "sFind Scheme definition of: ")
  (beginning-of-buffer)
  (let ((stop nil))
    (while (not stop)
      (search-forward name)
      (setq stop (defining_p)))))
	  
