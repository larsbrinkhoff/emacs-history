;; Debuggers and related commands for Emacs
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


(setq debugger 'debug)

(defun debug (&rest args)
  "Enter debugger.  Returns if user says \"continue\".
Arguments are mainly for use when this is called
 from the internals of the evaluator.
You may call with no args, or you may
 pass nil as the first arg and any other args you like.
 In that case, the list of args after the first will 
 be printed into the backtrace buffer."
  (message "Entering debugger...")
  (let (value)
    (save-excursion
     (save-window-excursion
      (pop-to-buffer " *Backtrace*")
      (erase-buffer)
      (let ((standard-output (current-buffer)))
	(backtrace))
      (goto-char (dot-min))
      (delete-region (dot)
		     (progn
		      (forward-sexp 8)
		      (forward-line 1)
		      (dot)))
      (debugger-mode)
      (if (memq (car args) '(lambda debug))
	  (progn
	   (insert "Entering:\n")
	   (if (eq (car args) 'debug)
	       (progn
		(backtrace-debug 4 t)
		(delete-char 1)
		(insert ?*)
		(beginning-of-line))))
	(if (eq (car args) 'exit)
	    (progn (insert "Return value: ")
		   (setq value (nth 1 args))
		   (prin1 value (current-buffer))
		   (insert ?\n)
		   (delete-char 1)
		   (insert ? )
		   (beginning-of-line))
	  (if (eq (car args) 'error)
	      (progn
	       (insert "Signalling: ")
	       (prin1 (nth 1 args) (current-buffer))
	       (insert ?\n))
	    (if (eq (car args) t)
		(insert "Beginning evaluation of function call form:\n")
	      (if (eq (car args) nil)
		  (prin1 (cdr args) (current-buffer))
		(prin1 args (current-buffer)))
	      (insert ?\n)))))
      (message "")
      (let ((inhibit-trace t)
	    (debug-on-error nil)
	    (request-debug-on-call))
	(message "")
	(recursive-edit)
	(setq debug-on-next-call request-debug-on-call))))
    value))

(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq request-debug-on-call t)
  (message "Proceding, will debug on next eval or call.")
  (exit-recursive-edit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  (message "Continuing.")
  (exit-recursive-edit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (setq value val)
  (princ "Returning " t)
  (prin1 value)
  (exit-recursive-edit))

(defun debugger-frame-number ()
  "Return number of frames in backtrace before the one dot points at."
  (save-excursion
   (beginning-of-line)
   (let ((odot (dot))
	 (count 0))
     (goto-char (dot-min))
     (if (or (equal (buffer-substring (dot) (+ (dot) 6))
		    "Signal")
	     (equal (buffer-substring (dot) (+ (dot) 6))
		    "Return"))
	 (progn
	  (search-forward ":")
	  (forward-sexp 1)))
     (forward-line 1)
     (while (progn
	     (forward-char 2)
	     (if (= (following-char) ?\()
		 (forward-sexp 1)
	       (forward-sexp 2))
	     (forward-line 1)
	     (<= (dot) odot))
       (setq count (1+ count)))
     count)))

;; Chosen empirically to account for all the frames
;; that will exist when debugger-frame is called
;; within the first one that appears in the backtrace buffer.
;; Assumes debugger-frame is called from a key;
;; will be wrong if it is called with Meta-x.
(defconst debugger-frame-offset 8 "")

(defun debugger-frame ()
  "Request entry to debugger when this frame exits.
Applies to the frame whose line dot is on in the backtrace."
  (interactive)
  (beginning-of-line)
  (let ((level (debugger-frame-number)))
    (backtrace-debug (+ level debugger-frame-offset) t))
  (if (= (following-char) ? )
      (progn
       (delete-char 1)
       (insert ?*)))
  (beginning-of-line))

(defun debugger-frame-clear ()
  "Do not enter to debugger when this frame exits.
Applies to the frame whose line dot is on in the backtrace."
  (interactive)
  (beginning-of-line)
  (let ((level (debugger-frame-number)))
    (backtrace-debug (+ level debugger-frame-offset) nil))
  (if (= (following-char) ?*)
      (progn
       (delete-char 1)
       (insert ? )))
  (beginning-of-line))

;; These two function names are equivalent except that
;; debugger-eval-expression is not normally disabled.
(fset 'debugger-eval-expression 'eval-expression)

(defvar debugger-mode-map nil "")

(if debugger-mode-map
    nil
  (let ((loop ? ))
    (setq debugger-mode-map (make-keymap))
    (if (fboundp 'suppress-keymap)
	(suppress-keymap debugger-mode-map)
      ;; Turn off self-insertion of letters.
      (setq loop ? )
      (while (< loop 127)
	(aset debugger-mode-map loop 'undefined)
	(setq loop (1+ loop)))
      ;; Make plain numbers do numeric args.
      (setq loop ?0)
      (while (<= loop ?9)
	(aset debugger-mode-map loop 'digit-argument)
	      (setq loop (+ loop 1))
	(setq loop (1+ loop))))
    (aset debugger-mode-map ?b 'debugger-frame)
    (aset debugger-mode-map ?c 'debugger-continue)
    (aset debugger-mode-map ?r 'debugger-return-value)
    (aset debugger-mode-map ?u 'debugger-frame-clear)
    (aset debugger-mode-map ?d 'debugger-step-through)
    (aset debugger-mode-map ?h 'describe-mode)
    (aset debugger-mode-map ?q 'top-level)
    (aset debugger-mode-map ?e 'debugger-eval-expression)
    (aset debugger-mode-map ?  'next-line)))

(defun debugger-mode ()
  "Mode for backtrace buffers, selected in debugger.
b -- reenter debugger on exit from frame dot is pointing at.
c -- continue execution.
d -- step through subexpressions of this expression.
u -- don't enter debugger on exit from frame dot is pointing at.
e -- read expression in minibuffer, eval, print in minibuffer.
q -- return to top level.
r -- return value from debugger.  When in debugger due to frame
    being exited, the value specified here will be used as the
    value of that frame.

Note lines starting with * are frames that will
 enter debugger when exited."
  (kill-all-local-variables)    
  (setq major-mode 'debugger-mode)
  (setq mode-name "Debugger")
  (setq truncate-lines t)
  (use-local-map debugger-mode-map))

(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If the user continues, FUNCTION's execution proceeds.
Works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined."
  (interactive "aDebug on entry (to function): ")
  (let ((defn (symbol-function function)))
    (if (eq (car defn) 'macro)
	(fset function (cons 'macro (debug-on-entry-1 function (cdr defn) t)))
      (fset function (debug-on-entry-1 function defn t))))
  function)

(defun cancel-debug-on-entry (function)
  "Undoes effect of debug-on-entry on FUNCTION."
  (interactive "aCancel debug on entry (to function): ")
  (let ((defn (symbol-function function)))
    (if (eq (car defn) 'macro)
	(fset function (cons 'macro (debug-on-entry-1 function (cdr defn)) nil))
      (fset function (debug-on-entry-1 function defn nil))))
  function)

(defun debug-on-entry-1 (function defn flag)
  (or (eq (car defn) 'lambda)
      (error "%s not user-defined Lisp function." function))
  (let (tail prec)
    (if (stringp (car (nthcdr 2 defn)))
	(setq tail (nthcdr 3 defn)
	      prec (list (car defn) (car (cdr defn)) (car (cdr (cdr defn)))))
      (setq tail (nthcdr 2 defn)
	    prec (list (car defn) (car (cdr defn)))))
    (if (eq flag (equal (car tail) '(debug 'debug)))
	nil
      (if flag
	  (nconc prec (cons '(debug 'debug) tail))
	(nconc prec (cdr tail))))))
