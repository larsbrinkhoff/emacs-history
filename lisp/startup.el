;; Process Emacs shell arguments
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


; These are processed only at the beginning of the argument list.
; -q			load no profile
; -u user		load user's profile

; These are processed in the order encountered.
; -e function		execute function
; -l file		load file
; file			visit file
; -kill			kill (exit) emacs

(setq top-level '(normal-top-level))

(defconst inhibit-command-line nil
  "*Non-nil inhibits usual processing of command line args from shell.
Exception: -q and -u are processed normally anyway.")

(defvar command-line-processed nil "t once command line has been processed")

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup messages")

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    (command-line)))

(defun command-line ()
  (let ((args (cdr command-line-args))
	(user (getenv "USER"))
	done)
    (while (and (not done) args)
      (let ((argi (car args)))
	(if (string-equal argi "-q")
	    (setq user nil
		  args (cdr args))
	  (if (string-equal argi "-u")
	      (setq args (cdr args)
		    user (car args)
		    args (cdr args))
	    (setq done t)))))
    ;; Load library for our terminal type.
    (load (concat "term-" (getenv "TERM")) t t)
    ;; Load user's init file, or load default one.
    (if user
	(or (load (concat "~" user "/.emacs") t t)
	    (load "default-profile" t t)))
    (setq mode-line-format default-mode-line-format)
    (setq case-fold-search default-case-fold-search)
    (setq fill-column default-fill-column)
    (and (eq major-mode 'lisp-interaction-mode)
	 (boundp 'lisp-interaction-mode-hook)
	 lisp-interaction-mode-hook
	 (funcall lisp-interaction-mode-hook))
    ;; init file sets inhibit-command-line to prevent normal processing.
    (if (not inhibit-command-line)
	(command-line-1 args))))

(defun command-line-1 (args)
  (if (null args)
      (cond ((and (not inhibit-startup-message) (not noninteractive)
		  (not (input-pending-p)))
	     (insert (emacs-version))
	     (goto-char (dot-min))
	     (search-forward " " nil nil 3)
	     (delete-region (1- (dot)) (dot-max))
	     ;; If keys have their default meanings,
	     ;; use precomputed string to save lots of time.
	     (if (and (eq (key-binding "\C-h\C-c") 'describe-copying)
		      (eq (key-binding "\C-h\C-d") 'describe-distribution)
		      (eq (key-binding "\C-h") 'help-command)
		      (eq (key-binding "\C-xu") 'advertised-undo))
		 (insert ", Copyright (C) 1985 by Richard M. Stallman.
Type C-h for help; C-x u to undo changes.
  (Note that `C-' stands for `Control-'.)
Emacs is freeware: you may copy and redistribute it under certain conditions.
Type C-h C-c to see those conditions.
Type C-h C-d for information on getting copies from me.\n")
	       (insert (substitute-command-keys
			", Copyright (C) 1985 by Richard M. Stallman.
Type \\[help-command] for help; \\[advertised-undo] to undo changes.
  (Note that `C-' stands for `Control-'.)
Emacs is freeware: you may copy and redistribute it under certain conditions.
Type \\[describe-copying] to see those conditions.
Type \\[describe-distribution] for information on getting copies from me.\n")))
	     (set-buffer-modified-p nil)
	     (sit-for 10)
	     (erase-buffer)
	     (set-buffer-modified-p nil)))
    (let ((dir default-directory)
	  (line 0))
      (while args
	(let ((argi (car args)))
	  (setq args (cdr args))
	  (cond ((string-equal argi "-e")
		 (setq tem (funcall (intern (car args)))
		       args (cdr args)))
		((string-equal argi "-l")
		 (setq tem (load (expand-file-name (car args) dir))
		       args (cdr args)))
		((string-equal argi "-kill")
		 (kill-emacs t))
		((string-match "^\\a+[0-9]+$" argi)
		 (setq line (string-to-int argi)))
		(t
		 (find-file (expand-file-name argi dir))
		 (goto-line line)
		 (setq line 0))))))))
