;; Run subshell under Emacs
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


(defvar last-input-start nil
  "In a shell-mode buffer, marker for start of last unit of input.")
(defvar last-input-end nil
  "In a shell-mode buffer, marker for start of last unit of input.")

(defvar shell-mode-map nil "")

(defvar shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

(defun shell-mode ()
  "Major mode for interacting with an inferior shell.
Shell name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the shell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from shell.
C-x C-v puts top of last batch of output at top of window.

Entry to this mode calls the value of shell-mode-hook with no args,
if that value is non-nil.

cd, pushd and popd commands given to the shell are watched
by Emacs to keep this buffer's default directory
the same as the shell's working directory.

You can send text to the shell (or its subjobs) from other buffers
using the commands send-region, send-string and shell-send-defun."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (setq mode-line-format 
	"--%1*%1*-Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
  (use-local-map shell-mode-map)
  (make-local-variable 'shell-directory-stack)
  (setq shell-directory-stack nil)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (and (boundp 'shell-mode-hook)
       shell-mode-hook
       (funcall shell-mode-hook)))

(if shell-mode-map
    nil
  (setq shell-mode-map (make-keymap))
  (define-key shell-mode-map "\C-m" 'shell-send-input)
  (define-key shell-mode-map "\C-d" 'delete-char-or-send-eof)
  (define-key shell-mode-map "\C-u" 'kill-shell-input)
  (define-key shell-mode-map "\C-w" 'backward-kill-word)
  (define-key shell-mode-map "\C-c" 'interrupt-shell-subjob)
  (define-key shell-mode-map "\C-z" 'stop-shell-subjob)
  (define-key shell-mode-map "\C-\\" 'quit-shell-subjob)
  (define-key shell-mode-map "\C-x\C-k" 'kill-output-from-shell)
  (define-key shell-mode-map "\C-x\C-v" 'show-output-from-shell)
  (define-key shell-mode-map "\e=" 'copy-last-shell-input))

(defun shell ()
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in shell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See shell-mode.
See also variable shell-prompt-pattern.

Note that many people's .cshrc files unconditionally clear the prompt.
If yours does, you will probably want to change it."
  (interactive)
  (let* ((prog (or explicit-shell-file-name (getenv "ESHELL") (getenv "SHELL")))
	 (name (file-name-nondirectory prog)))
    (make-shell "shell" prog
		(if (file-exists-p (concat "~/.emacs_" name))
		    (concat "~/.emacs_" name)))))

(defun make-shell (name program &optional startfile)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc
	(setq status (process-status proc)))
    (switch-to-buffer buffer)
;    (setq size (buffer-size))
    (if (memq status '(run stop))
	nil
      (if proc (delete-process proc))
      (setq proc (start-process name buffer program))
      (cond (startfile
;This is guaranteed to wait long enough
;but has bad results if the shell does not prompt at all
;	     (while (= size (buffer-size))
;	       (sleep-for 1))
;I hope 1 second is enough!
	     (sleep-for 1)
	     (goto-char (dot-max))
	     (insert-file-contents startfile)
	     (setq startfile (buffer-substring (dot) (dot-max)))
	     (delete-region (dot) (dot-max))
	     (send-string proc startfile)))
      (setq name (process-name proc)))
    (goto-char (dot-max))
    (set-marker (process-mark proc) (dot))
    (shell-mode)))

;In loaddefs.el now.
;(defconst shell-prompt-pattern
;  "^[^ ]*>"
;  "*Regexp used by Newline command to match subshell prompts.
;Anything from beginning of line up to the end of what this pattern matches
;is deemed to be prompt, and is not reexecuted.")

(defun shell-send-input ()
  "Send input to subshell.
At end of buffer, sends all text after last output
 as input to the subshell, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of shell-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (end-of-line)
  (if (eobp)
      (let ((mark (process-mark (get-buffer-process (current-buffer)))))
       (newline)
       (if (/= (dot) mark)
	   (progn
	    (move-marker last-input-start mark)
	    (move-marker last-input-end (dot)))))
    (beginning-of-line)
    (re-search-forward shell-prompt-pattern nil t)
    (let ((copy (buffer-substring (dot)
				  (progn (forward-line 1) (dot)))))
      (goto-char (dot-max))
      (move-marker last-input-start (dot))
      (insert copy)
      (move-marker last-input-end (dot))))
  (save-excursion
   (goto-char last-input-start)
   (if (looking-at "popd\n")
       (if shell-directory-stack
	   (progn
	    (cd (car shell-directory-stack))
	    (setq shell-directory-stack (cdr shell-directory-stack)))))
   (if (looking-at "pushd\n")
       (if shell-directory-stack
	   (let ((old default-directory))
	    (cd (car shell-directory-stack))
	    (setq shell-directory-stack
		  (cons old (cdr shell-directory-stack))))))
   (if (looking-at "pushd ")
       (progn
	 (skip-chars-forward "^ ")
	 (skip-chars-forward " \t")
	 (if (file-directory-p (buffer-substring (dot) (1- last-input-end)))
	     (progn
	       (setq shell-directory-stack
		     (cons default-directory shell-directory-stack))
	       (cd (buffer-substring (dot) (1- last-input-end)))))))
   (if (looking-at "cd\n")
       (cd (getenv "HOME")))
   (if (looking-at "cd ")
       (progn
	(forward-char 3)
	(skip-chars-forward " \t")
	(if (file-directory-p (buffer-substring (dot)
						(1- last-input-end)))
	    (cd (buffer-substring (dot) (1- last-input-end)))))))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region process last-input-start last-input-end)
    (set-marker (process-mark process) (dot))))

(defun delete-char-or-send-eof (arg killp)
  "At end of buffer, send eof to subshell.  Otherwise delete character."
  (interactive "p\nP")
  (if (and (eobp) (not killp))
      (process-send-eof)
    (delete-char arg killp)))

(defun kill-output-from-shell ()
  "Kill all output from shell since last input."
  (interactive)
  (goto-char (dot-max))
  (kill-region last-input-end (dot))
  (insert "> output flushed ***\n"))

(defun show-output-from-shell ()
  "Display start of this batch of shell output at top of window.
Also put cursor there."
  (interactive)
  (set-window-start (selected-window) last-input-end)
  (goto-char last-input-end))

(defun copy-last-shell-input ()
  "Copy previous shell input, sans newline, and insert before dot."
  (interactive)
  (insert (buffer-substring last-input-end last-input-start))
  (delete-char -1))

(defun interrupt-shell-subjob ()
  "Interrupt this shell's current subjob."
  (interactive)
  (interrupt-process nil t))

(defun kill-shell-subjob ()
  "Send kill signal to this shell's current subjob."
  (interactive)
  (kill-process nil t))

(defun quit-shell-subjob ()
  "Send quit signal to this shell's current subjob."
  (interactive)
  (quit-process nil t))

(defun stop-shell-subjob ()
  "Stop this shell's current subjob."
  (interactive)
  (stop-process nil t))

(defun kill-shell-input ()
  "Kill all text since last stuff output by the shell or its subjobs."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (dot)))

(defvar inferior-lisp-mode-map nil)

(defun inferior-lisp-mode ()
  "Major mode for interacting with an inferior Lisp process.

Commands:
Delete converts tabs to spaces as it moves back.
Tab indents for Lisp; with argument, shifts rest
 of expression rigidly with the current line.
Meta-Control-Q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the shell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from shell.
C-x C-v puts top of last batch of output at top of window.

Entry to this mode calls the value of lisp-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
lisp-mode-hook is called after shell-mode-hook.

You can send text to the inferior Lisp from other buffers
using the commands \\[send-region], \\[send-string] and \\[lisp-send-defun]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (setq mode-line-format 
	"--%1*%1*-Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
  (or inferior-lisp-mode-map
      (progn
	(setq inferior-lisp-mode-map (copy-sequence shell-mode-map))
	(lisp-mode-commands inferior-lisp-mode-map)
	(define-key inferior-lisp-mode-map "\e\C-x" 'lisp-send-defun)))
  (lisp-mode-variables)
  (use-local-map inferior-lisp-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (and (boundp 'shell-mode-hook)
       shell-mode-hook
       (funcall shell-mode-hook))
  (and (boundp 'lisp-mode-hook)
       lisp-mode-hook
       (funcall lisp-mode-hook)))

(defun run-lisp ()
  "Run an inferior Lisp process, input and output via buffer *lisp*."
  (interactive)
  (make-shell "lisp" "lisp")
  (inferior-lisp-mode))

(defun lisp-send-defun nil
  "Send the current defun to the Lisp process made by M-x run-lisp."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (dot)))
     (beginning-of-defun)
     (send-region "lisp" (dot) end)
     (send-string "lisp" "\n"))))

(defun lisp-send-defun-and-go nil
  "Send the current defun to the inferior Lisp, and switch to *lisp* buffer."
  (interactive)
  (shell-send-defun)
  (switch-to-buffer "*lisp*"))
