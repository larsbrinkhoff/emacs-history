;; Run-time support for mocklisp code.
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

(provide 'mlsupport)

(defun ml-defun (&quote &rest args)
  (while args
    (fset (car (car args)) (cons 'mocklisp (cdr (car args))))
    (setq args (cdr args))))

(defun declare-buffer-specific (&quote &rest vars)
  (while vars
    (make-variable-buffer-local (car vars))))

(defun setq-default (&quote &rest args)
  (set-default (car args) (eval (car (cdr args)))))

(defun ml-set-default (varname value)
  (set-default (intern varname) value))

; Lossage: must make various things default missing args to the prefix arg
; Alternatively, must make provide-prefix-argument do something hairy.

(defun >> (val count) (lsh val (- count)))
(defun novalue () nil)

(defun provide-prefix-arg (arg form)
  (funcall (car form) arg))

(defun define-keymap (name)
  (fset (intern name) (make-keymap)))

(defun ml-use-local-map (name)
  (use-local-map (intern (concat name "-map"))))

(defun ml-use-global-map (name)
  (use-global-map (intern (concat name "-map"))))

(defun local-bind-to-key (name key)
  (or (current-local-map)
      (use-local-map (make-keymap)))
  (define-key (current-local-map)
    (if (integerp key)
	(if (>= key 128)
	    (concat (char-to-string meta-prefix-char)
		    (char-to-string (- key 128)))
	  (char-to-string key))
      key)
    (intern name)))

(defun bind-to-key (name key)
  (define-key global-map (if (integerp key) (char-to-string key) key)
    (intern name)))

(defun ml-autoload (name file)
  (autoload (intern name) file))

(defun ml-define-string-macro (name defn)
  (fset (intern name) defn))

(defun push-back-character (char)
  (setq unread-command-char char))

(defun to-col (column)
  (indent-to column 0))

(defun is-bound (&quote &rest syms)
  (while (and syms (boundp (car syms)))
    (setq syms (cdr syms)))
  (not syms))

(defun declare-global (&quote &rest syms)
  (while syms
    (set (car syms) nil)
    (put (car syms) 'variable-documentation
		    "Defined by mocklisp; documentation not given")
    (setq syms (cdr syms))))

(defun error-occurred (&quote &rest body)
  (condition-case ()
      (progn
        (while body
	  (eval (car body)) (setq body (cdr body)))
	nil)
    (error t)))

(defun return-prefix-argument (value)
  (setq prefix-arg value))

(defun ml-prefix-argument ()
  (if (null current-prefix-arg) 1
    (if (listp current-prefix-arg) (car current-prefix-arg)
      (if (eq current-prefix-arg '-) -1
	current-prefix-arg))))

(defun ml-print (varname)
  (interactive "vPrint variable: ")
  (if (boundp varname)
    (message "%s => %s" (symbol-name varname) (symbol-value varname))
    (message "%s has no value" (symbol-name varname))))

(defun ml-set (str val) (set (intern str) val))

(defun ml-message (&rest args) (message "%s" (apply 'concat args)))

(defun kill-to-end-of-line ()
  (ml-prefix-argument-loop
    (if (eolp) (kill-region (dot) (1+ (dot))))))

(defun set-auto-fill-hook (arg)
  (setq auto-fill-hook (intern arg)))

(defun auto-execute (function pattern)
  (if (/= (aref pattern 0) ?*)
      (error "Only patterns starting with * supported in auto-execute"))
  (setq auto-mode-alist
	(cons (cons (substring pattern 1) function)
	      auto-mode-alist)))

(defun move-to-comment-column ()
  (indent-to comment-column))

(defun erase-region ()
  (delete-region (dot) (mark)))

(defun delete-region-to-buffer (bufname)
  (copy-to-buffer bufname (dot) (mark))
  (delete-region (dot) (mark)))

(defun copy-region-to-buffer (bufname)
  (copy-to-buffer bufname (dot) (mark)))

(defun append-region-to-buffer (bufname)
  (append-to-buffer bufname (dot) (mark)))

(defun prepend-region-to-buffer (bufname)
  (prepend-to-buffer bufname (dot) (mark)))

(defun delete-next-character ()
  (delete-char (ml-prefix-argument)))

(defun delete-next-word ()
  (delete-region (dot) (progn (forward-word (ml-prefix-argument)) (dot))))

(defun delete-previous-word ()
  (delete-region (dot) (progn (backward-word (ml-prefix-argument)) (dot))))

(defun delete-previous-character ()
  (delete-backward-char (ml-prefix-argument)))

(defun forward-character ()
  (forward-char (ml-prefix-argument)))

(defun backward-character ()
  (backward-char (ml-prefix-argument)))

(defun ml-newline ()
  (newline (ml-prefix-argument)))

(defun ml-next-line ()
  (next-line (ml-prefix-argument)))

(defun ml-previous-line ()
  (previous-line (ml-prefix-argument)))

(defun delete-to-kill-buffer ()
  (kill-region (dot) (mark)))

(defun narrow-region ()
  (narrow-to-region (dot) (mark)))

(defun ml-newline-and-indent ()
  (let ((column (current-indentation)))
    (newline (ml-prefix-argument))
    (indent-to column)))

(defun newline-and-backup ()
  (open-line (ml-prefix-argument)))

(defun quote-char ()
  (quoted-insert (ml-prefix-argument)))

(defun ml-current-column ()
  (1+ (current-column)))

(defun ml-current-indent ()
  (1+ (current-indent)))

(defun use-abbrev-table (name)
  (let ((symbol (intern (concat name "-abbrev-table"))))
    (or (boundp symbol)
	(define-abbrev-table symbol nil))
    (symbol-value symbol)))

(defun define-hooked-local-abbrev (name exp hook)
  (define-local-abbrev name exp (intern hook)))

(defun define-hooked-global-abbrev (name exp hook)
  (define-global-abbrev name exp (intern hook)))

(defun case-word-lower ()
  (ml-casify-word 'downcase-region))

(defun case-word-upper ()
  (ml-casify-word 'upcase-region))

(defun case-word-capitalize ()
  (ml-casify-word 'capitalize-region))

(defun ml-casify-word (fun)
  (save-excursion
   (forward-char 1)
   (forward-word -1)
   (funcall fun (dot)
	    (progn (forward-word (ml-prefix-argument))
		   (dot)))))

(defun case-region-lower ()
  (downcase-region (dot) (mark)))

(defun case-region-upper ()
  (upcase-region (dot) (mark)))

(defun case-region-capitalize ()
  (capitalize-region (dot) (mark)))

(defun argc ()
  (setq inhibit-command-line t)
  (length command-line-args))

(defun argv (i)
  (setq inhibit-command-line t)
  (nth i command-line-args))

(defun invisible-argc ()
  (length command-line-args))

(defun invisible-argv (i)
  (nth i command-line-args))

(defun exit-emacs ()
  (interactive)
  (condition-case ()
		  (exit-recursive-edit)
    (error (kill-emacs))))

;; Lisp function buffer-size returns total including invisible;
;; mocklisp wants just visible.
(defun ml-buffer-size ()
  (- (dot-max) (dot-min)))

(defun previous-command ()
  last-command)

(defun beginning-of-window ()
  (goto-char (window-start)))

(defun end-of-window ()
  (goto-char (window-start))
  (vertical-motion (- (window-height) 2)))

(defun ml-search-forward (string)
  (search-forward string nil nil (ml-prefix-argument)))

(defun ml-re-search-forward (string)
  (re-search-forward string nil nil (ml-prefix-argument)))

(defun ml-search-backward (string)
  (search-backward string nil nil (ml-prefix-argument)))

(defun ml-re-search-backward (string)
  (re-search-backward string nil nil (ml-prefix-argument)))

(defvar use-users-shell 1
  "Mocklisp compatibility variable; 1 means use shell from SHELL env var.
0 means use /bin/sh.")

(defvar use-csh-option-f 1
  "Mocklisp compatibility variable; 1 means pass -f when calling csh.")

(defun filter-region (command)
  (let ((shell (if (/= use-users-shell 0) shell-file-name "/bin/sh"))
	(csh (equal (file-name-nondirectory shell) "csh")))
    (call-process-region (dot) (mark) shell t t nil
			 (if (and csh use-csh-option-f) "-cf" "-c")
			 (concat "exec " command))))

(defun execute-monitor-command (command)
  (let ((shell (if (/= use-users-shell 0) shell-file-name "/bin/sh"))
	(csh (equal (file-name-nondirectory shell) "csh")))
    (call-process shell nil t t
		  (if (and csh use-csh-option-f) "-cf" "-c")
		  (concat "exec " command))))

(defun use-syntax-table (name)
  (set-syntax-table (symbol-value (intern (concat name "-syntax-table")))))

(defun line-to-top-of-window ()
  (recenter-window (1- (ml-prefix-argument))))

(defun ml-previous-page (&optional arg)
  (let ((count (or arg (ml-prefix-argument))))
    (while (> count 0)
      (scroll-down nil)
      (setq count (1- count)))
    (while (< count 0)
      (scroll-up nil)
      (setq count (1+ count)))))

(defun ml-next-page ()
  (previous-page (- (ml-prefix-argument))))

(defun page-next-window (&optional arg)
  (let ((count (or arg (ml-prefix-argument))))
    (while (> count 0)
      (scroll-other-window nil)
      (setq count (1- count)))
    (while (< count 0)
      (scroll-other-window '-)
      (setq count (1+ count)))))

(defun ml-next-window ()
  (select-window (next-window)))

(defun ml-previous-window ()
  (select-window (previous-window)))

(defun scroll-one-line-up ()
  (scroll-up (ml-prefix-argument)))

(defun scroll-one-line-down ()
  (scroll-down (ml-prefix-argument)))

(defun split-current-window ()
  (split-window (selected-window)))

(defun last-key-struck () last-command-char)

(defun execute-mlisp-line (string)
  (eval (read string)))

(defun ml-modify-syntax-entry (string)
  (let ((i 5)
	(len (length string))
	(datastring (substring string 0 2)))
    (if (= (aref string 0) ?\-)
	(aset datastring 0 ?\ ))
    (if (= (aref string 2) ?\{)
	(if (= (aref string 4) ?\ )
	    (aset datastring 0 ?\<)
	  (error "Two-char comment delimiter: use modify-syntax-entry directly")))
    (if (= (aref string 3) ?\})
	(if (= (aref string 4) ?\ )
	    (aset datastring 0 ?\>)
	  (error "Two-char comment delimiter: use modify-syntax-entry directly")))
    (while (< i len)
      (modify-syntax-entry
       (aref string i)
       datastring)
      (setq i (1+ i))
      (if (and (< i len)
	       (= (aref string i) ?\-))
	  (let ((c (aref string (1- i)))
		(lim (aref string (1+ i))))
	    (while (<= c lim)
	      (modify-syntax-entry c datastring)
	      (setq c (1+ c)))
	    (setq i (+ 2 i)))))))