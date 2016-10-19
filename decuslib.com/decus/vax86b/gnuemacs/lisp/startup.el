;;;	$Source: /u1/third_party/gnuemacs.v17/lisp/RCS/startup.el,v $
;;;	$Author: rlk $
;;;	$Locker:  $
;;;	$Header: startup.el,v 1.14 86/02/17 12:25:48 rlk Exp $

;; Process Emacs shell arguments
;; Copyright (C) 1985 Richard M. Stallman.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


; These are processed only at the beginning of the argument list.
; -batch		execute noninteractively (messages go to stdout,
;			 variable noninteractive set to t)
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -t file		Specify to use file rather than stdin/stdout
;			 as the terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -q			load no init file
; -u user		load user's init file

; These are processed in the order encountered.
; -f function		execute function
; -l file		load file
; file			visit file
; -kill			kill (exit) emacs

(setq top-level '(normal-top-level))

(defconst inhibit-command-line nil
  "*Non-nil inhibits usual processing of command line args from shell.
Exception: -batch, -q and -u are processed normally anyway.")

(defvar losing-keyboard nil "T if the terminal emacs is running on uses
losing xon-xoff flow control.")

(defvar command-line-processed nil "t once command line has been processed")

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup messages")

(defconst command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `args'.")

(defvar term-setup-hook nil)

(defvar startup-major-mode 'fundamental-mode
  "*Normal major mode that Emacs starts up in")

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    (unwind-protect
	(command-line)
      (and term-setup-hook
	   (funcall term-setup-hook)))))

(defun command-line ()
  (let ((args (cdr command-line-args))
	(user (if noninteractive
		  nil
		(or (getenv "USER")
		    (getenv "LOGNAME")))) ;USG bletcherousness.
	(termtype (if noninteractive nil (getenv "TERM")))
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
    ;; Load user's init file, or load default one.
    (condition-case error
	(if user
	    (or (load (user-init-file) t t)
		(load "default_profile" t t)))
      (error (message "Error in init file")))
    (setq mode-line-format default-mode-line-format
	  case-fold-search default-case-fold-search
	  fill-column default-fill-column
	  abbrev-mode default-abbrev-mode
	  ctl-arrow default-ctl-arrow
	  left-margin default-left-margin
	  tab-width default-tab-width
	  truncate-lines default-truncate-lines)
    ;; Load library for our terminal type.
    ;; User init file can set term-file-prefix to nil to prevent this.
    (and term-file-prefix (not noninteractive)
	 (load (concat term-file-prefix
		       (substring termtype 0 (string-match "-" termtype)))
	        t t))
    (condition-case nil
	(funcall startup-major-mode)
      (error nil))
    (and (eq major-mode 'lisp-interaction-mode)
	 (run-hooks 'lisp-interaction-mode-hook))
    ;; init file sets inhibit-command-line to prevent normal processing.
    (if (not inhibit-command-line)
	(command-line-1 args))))

(defun command-line-1 (command-line-args)
  (if (null command-line-args)
      (cond ((and (not inhibit-startup-message) (not noninteractive)
		  (not (input-pending-p)))
	     ;; If there are no switches to procss, we might as well
	     ;; run this hook now, and there may be some need to do it
	     ;; before doing any output.
	     (and term-setup-hook
		  (funcall term-setup-hook))
	     ;; Don't let the hook be run twice.
	     (setq term-setup-hook nil)
	     (insert (emacs-version))
	     (insert "
 Copyright (C) 1985 Richard Stallman/Free Software Foundation\n")
	     (insert "\n VMS Version by Mukesh Prasad\n\n")
	     (if losing-keyboard
		 (progn (insert"
You cannot type C-s or C-q on this terminal.

Use:    C-~  for  C-s
        C-\  for  C-q    instead.

For example, C-x C-s  is now  C-x  C-~
If you do type C-s, the terminal will freeze.  Type C-q to unfreeze it.
Type C-h C-t for more information.")
			(define-key help-map "\^t" 'help-for-losing-terminal)))
	     ;; If keys have their default meanings,
	     ;; use precomputed string to save lots of time.
	     (if (and (eq (key-binding "\C-h") 'help-command)
		      (eq (key-binding "\C-xu") 'advertised-undo)
		      (eq (key-binding "\C-h\C-c") 'describe-copying)
		      (eq (key-binding "\C-h\C-d") 'describe-distribution)
		      (eq (key-binding "\C-h\C-w") 'describe-no-warranty)
		      (eq (key-binding "\C-ht") 'help-with-tutorial))
		 (insert 
 "Type C-h for help; C-x u to undo changes.  (`C-' means use CTRL key.)

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type C-h C-w for full details.
You may give out copies of Emacs; type C-h C-c to see the conditions.
Type C-h C-d for information on getting the latest version.
Type C-h t for a tutorial on using Emacs.")
	       (insert (substitute-command-keys
 "Type \\[help-command] for help; \\[advertised-undo] to undo changes.  (`C-' means use CTRL key.)

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
You may give out copies of Emacs; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version.
Type \\[help-with-tutorial] for a tutorial on using Emacs.")))
	     (set-buffer-modified-p nil)
	     (sit-for 30)
	     (erase-buffer)
	     (set-buffer-modified-p nil)))
    (let ((dir default-directory)
	  (line 0))
      (while command-line-args
	(let ((argi (car command-line-args))
	      tem)
	  (setq command-line-args (cdr command-line-args))
	  (cond ((setq tem (assoc argi command-switch-alist))
		 (funcall (cdr tem) argi))
		((or (string-equal argi "-f")  ;what the manual claims
		     (string-equal argi "-e")) ; what the source used to say
		 (setq tem (intern (car command-line-args)))
		 (setq command-line-args (cdr command-line-args))
		 (funcall tem))
		((string-equal argi "-l")
		 (load (car command-line-args) nil t)
		 (setq command-line-args (cdr command-line-args)))
		((string-equal argi "-kill")
		 (kill-emacs t))
		((string-match "^\\+[0-9]+\\'" argi)
		 (setq line (string-to-int argi)))
		(t
		 (find-file (expand-file-name argi dir))
		 (goto-line line)
		 (setq line 0))))))
    (and losing-keyboard (not inhibit-startup-message)
	 (save-window-excursion
	   (with-output-to-temp-buffer "*Help*"
	     (switch-to-buffer "*Help*")
	     (insert "
You cannot type C-s or C-q on this terminal.

Use:    C-~  for  C-s
          C-\  for  C-q    instead.

For example, C-x C-s  is now  C-x  C-~
If you do type C-s, the terminal will freeze.  Type C-q to unfreeze it.
Type C-h C-t for more information.")
	     (sit-for 30))))))

(defun user-init-file()
    (if (file-exists-p "emacsini")
	"emacsini"
	"sys$login:emacsini.el"))
