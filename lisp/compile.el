;; Run compiler as inferior of Emacs, and parse its error messages.
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


(defvar compilation-process nil
  "Process created by compile command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a list of length two.
Its car is a marker pointing to an error message.
Its cadr is a marker pointing to the text of the line the message is about,
  or nil if that is not interesting.
The value may be t instead of a list;
this means that the buffer of error messages should be reparsed
the next time the list of errors is wanted.")

(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages parsed.")

(defvar compilation-error-format nil
  "t means parse output from grep -n, nil means parse usual error messages")

(defun compile (command)
  "Compile the program including the current buffer.  Default: run make.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it."
  (interactive (list (read-input "Compile command: " compile-command)))
  (setq compile-command command)
  (compile1 compile-command nil))

(defun grep (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  (interactive "sRun grep (with args): ")
  (compile1 (concat "grep -n " command " /dev/null")
	    t))

(defun compile1 (command format)
  (save-some-buffers)
  (if compilation-process
      (if (or (not (eq (process-status compilation-process) 'run))
	      (yes-or-no-p "A compilation process is running; kill it? "))
	  (condition-case ()
			  (delete-process compilation-process)
	    (error nil))
	(error "Cannot have two compilation processes")))
  (setq compilation-process nil)
  (compilation-forget-errors)
  (setq compilation-error-list t)
  (setq compilation-error-format format)
  (setq compilation-process
	(start-process "compilation" "*compilation*"
		       shell-file-name
		       "-c" (concat "exec " command)))
  (with-output-to-temp-buffer "*compilation*"
    (princ "cd ")
    (princ default-directory)
    (terpri)
    (princ command)
    (terpri))
  (set-process-sentinel compilation-process 'compilation-sentinel)
  (let* ((thisdir default-directory)
	 (outbuf (process-buffer compilation-process))
	 (outwin (get-buffer-window outbuf)))
    (if (eq outbuf (current-buffer))
	(goto-char (dot-max)))
    (save-excursion
     (set-buffer outbuf)
     (buffer-flush-undo outbuf)
     (set-window-start outwin 1)
     (or (eq outwin (selected-window))
	 (set-window-dot outwin 1))
     (setq default-directory thisdir)
     (fundamental-mode)
     (setq mode-name (if format "grep" "Compilation"))
     ;; Make log buffer's mode line show process state
     (setq mode-line-format
	   "--%1*%1*-Emacs: %17b   %   %[(%m: %s)%]----%3p--%-"))))

;; Called when compilation process changes state.
(defun compilation-sentinel (proc msg)
  (if (memq (process-status proc) '(signal exit))
      (let* ((obuf (current-buffer))
	     (omax (dot-max))
	     (odot (dot)))
	(unwind-protect
	    (progn
	      (set-buffer (process-buffer proc))
	      (goto-char (dot-max))
	      (insert ?\n mode-name " " msg)
	      (setq mode-line-format
		    (concat
		     "--%1*%1*-Emacs: %17b   %M   %[(%m: "
		     (symbol-name (process-status proc))
		     ")%]----%3p--%-"))
	      (delete-process proc)
	      (setq compilation-process nil)
	      ;; Force mode line redisplay soon
	      (set-buffer-modified-p (buffer-modified-p)))
	 (if (< odot omax)
	     (goto-char odot))
	 (set-buffer obuf)))))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (if compilation-process
      (kill-process compilation-process)))

(defun kill-grep ()
  "Kill the process made by the \\[grep] command."
  (interactive)
  (if compilation-process
      (kill-process compilation-process)))

(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  (interactive "P")
  (if (or (eq compilation-error-list t)
	  argp)
      (progn (compilation-forget-errors)
	     (setq compilation-parsing-end 1)))
  (if compilation-error-list
      nil
    (save-excursion
     (set-buffer "*compilation*")
     (set-buffer-modified-p nil)
     (compilation-parse-errors)))
  (let ((next-error (car compilation-error-list)))
    (if (null next-error)
	(error (concat (if compilation-error-format
			   "No more grep hits" "No more errors")
		       (if (and compilation-process
				(eq (process-status compilation-process)
				    'run))
			   " yet" ""))))
    (setq compilation-error-list (cdr compilation-error-list))
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      (set-marker (car (cdr next-error)) nil))
    (let ((w (display-buffer (marker-buffer (car next-error)))))
      (set-window-dot w (car next-error))
      (set-window-start w (car next-error)))
    (set-marker (car next-error) nil)))

;; Set compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection,
;; but it is better to do it right away.
(defun compilation-forget-errors ()
  (if (eq compilation-error-list t)
      (setq compilation-error-list nil))
  (while compilation-error-list
    (let ((next-error (car compilation-error-list)))
      (set-marker (car next-error) nil)
      (if (car (cdr next-error))
	  (set-marker (car (cdr next-error)) nil)))
    (setq compilation-error-list (cdr compilation-error-list))))

(defun compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in compilation-error-list.
The function next-error, assigned to \\[next-error], takes the next error off the list
and visits its location."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
	last-filename last-linenum
	(regexp "\\((\\|: *\\|, line \\)[0-9]+"))
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward regexp nil t)
      (let (linenum filename
	    error-marker text-marker
	    name-end
	    (prevchar (char-after (match-beginning 0))))
	;; If it's a lap message, use the last file(linenum) on the line.
	;; Normally we use the first on the line.
	(if (= prevchar ?\()
	    (progn
	      (end-of-line)
	      (re-search-backward regexp)))
	(setq name-end (match-beginning 0))
	(save-restriction
	 (narrow-to-region (dot-min) (match-end 0))
	 (goto-char (dot-max))
	 (skip-chars-backward "[0-9]")
	 (setq linenum (read (current-buffer))))
	(goto-char name-end)
	(if (= (preceding-char) ?\")
	    (backward-sexp 1)
	  (skip-chars-backward "^ \n\t("))
	(if (eq (following-char) ?\")
	    (save-restriction
	     (narrow-to-region (dot) name-end)
	     (setq filename (read (current-buffer))))
	  (setq filename (buffer-substring (dot) name-end)))
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (beginning-of-line 1)
	  (setq error-marker (dot-marker))
	  (setq last-linenum linenum)
	  (if (not (equal filename last-filename))
	      (setq text-buffer (find-file-noselect (setq last-filename filename))))
	  (save-excursion
	   (set-buffer text-buffer)
	   (goto-char 1)
	   (forward-line (1- linenum))
	   (setq text-marker (dot-marker)))
	  (setq compilation-error-list
		(cons (list error-marker text-marker)
		      compilation-error-list)))
	(forward-line 1)))
    (setq compilation-parsing-end (dot-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))

(define-key ctl-x-map "`" 'next-error)
