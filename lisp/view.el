;; View: Tour files, buffers without editing.
;; Copyright (C) 1985 Richard M. Stallman and K. Shane Hartman

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

(provide 'view)

(or (fboundp 'Helper-help)
    (autoload 'Helper-help
	      "helper"
	      "Provide help for current mode."
	      t))

(or (fboundp 'Helper-describe-bindings)
     (autoload 'Helper-describe-bindings
               "helper"
               "Describe local key bindings of current mode."
               t))

(defconst view-mode-map (make-keymap))
(save-excursion
  (fillarray view-mode-map 'view-undefined)
  (define-key view-mode-map "\C-c" 'exit-recursive-edit)
  (define-key view-mode-map "\C-z" 'suspend-emacs)
  (define-key view-mode-map "q" 'exit-recursive-edit)
  (define-key view-mode-map "-" 'negative-argument)
  (define-key view-mode-map "0" 'digit-argument)
  (define-key view-mode-map "1" 'digit-argument)
  (define-key view-mode-map "2" 'digit-argument)
  (define-key view-mode-map "3" 'digit-argument)
  (define-key view-mode-map "4" 'digit-argument)
  (define-key view-mode-map "5" 'digit-argument)
  (define-key view-mode-map "6" 'digit-argument)
  (define-key view-mode-map "7" 'digit-argument)
  (define-key view-mode-map "8" 'digit-argument)
  (define-key view-mode-map "9" 'digit-argument)
  (define-key view-mode-map "\C-u" 'universal-argument)
  (define-key view-mode-map "\e" (make-keymap))
  (fillarray (lookup-key view-mode-map "\e") 'view-undefined)
  (define-key view-mode-map "\e\e" 'eval-expression)
  (define-key view-mode-map "\e<" 'beginning-of-buffer)
  (define-key view-mode-map "\e>" 'end-of-buffer)
  (define-key view-mode-map "<" 'beginning-of-buffer)
  (define-key view-mode-map ">" 'end-of-buffer)
  (define-key view-mode-map "\ev" 'view-scroll-lines-backward)
  (define-key view-mode-map "\e\C-v" 'scroll-other-window)
  (define-key view-mode-map "\C-v" 'view-scroll-lines-forward)
  (define-key view-mode-map "\e\C-b" 'backward-sexp)
  (define-key view-mode-map "\e\C-f" 'forward-sexp)
  (define-key view-mode-map "\e\C-a" 'beginning-of-defun)
  (define-key view-mode-map "\e\C-e" 'end-of-defun)
  (define-key view-mode-map " " 'view-scroll-lines-forward)
  (define-key view-mode-map "\177" 'view-scroll-lines-backward)
  (define-key view-mode-map "\n" 'view-scroll-one-more-line)
  (define-key view-mode-map "\r" 'view-scroll-one-more-line)
  (define-key view-mode-map "\C-l" 'recenter)
  (define-key view-mode-map "z" 'view-scroll-lines-forward-set-scroll-size)
  (define-key view-mode-map "g" 'view-goto-line)
  (define-key view-mode-map "=" 'what-line) 
  (define-key view-mode-map "." 'set-mark-command)
  (define-key view-mode-map "\C-@" 'set-mark-command)
  (define-key view-mode-map "'" 'view-back-to-mark)
  (define-key view-mode-map "@" 'view-back-to-mark)  
  (define-key view-mode-map "x" 'exchange-dot-and-mark)
  (define-key view-mode-map "h" 'view-give-help)
  (define-key view-mode-map "?" 'Helper-describe-bindings)
  (define-key view-mode-map "\C-h" 'Helper-help)
  (define-key view-mode-map "\C-n" 'next-line)
  (define-key view-mode-map "\C-p" 'previous-line)
  (define-key view-mode-map "\C-s" 'isearch-forward)
  (define-key view-mode-map "\C-r" 'isearch-backward)
  (define-key view-mode-map "s" 'isearch-forward)
  (define-key view-mode-map "r" 'isearch-backward)
  (define-key view-mode-map "/" 'view-search-regexp-forward)
  (define-key view-mode-map "\\" 'view-search-regexp-backward)
  (define-key view-mode-map "\e\C-s" 'view-search-regexp-forward)
  (define-key view-mode-map "\e\C-r" 'view-search-regexp-backward)  
  (define-key view-mode-map "n" 'view-search-last-regexp-forward)
  (define-key view-mode-map "p" 'view-search-last-regexp-backward))

(defun view-file (file-name)
  "View FILE in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

Calls the value of  view-hook  if that is non-nil."
  (interactive "fView file: ")
  (view-mode (prog1 (current-buffer) (find-file file-name))))

(defun view-buffer (buffer-name)
  "View BUFFER in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

Calls the value of  view-hook  if that is non-nil."
  (interactive "bView buffer: ")
  (view-mode (prog1 (current-buffer) (switch-to-buffer buffer-name))))

(defun view-mode (&optional view-return-to-buffer)
  "Major mode for viewing text but not editing it.
Letters do not insert themselves.  Instead these commands are provided.
Most commands take prefix arguments.  Commands dealing with lines
default to \"scroll size\" lines (initially size of window).
Search commands default to a repeat count of one.
M-< or <	move to beginning of buffer.
M-> or >	move to end of buffer.
C-v or Space	scroll forward lines.
M-v or DEL	scroll backward lines.
CR or LF	scroll forward one line (backward with prefix argument).
z		like Space except set number of lines for further
		   scrolling commands to scroll by.
C-u and Digits	provide prefix arguments.  `-' denotes negative argument.
=		prints the current line number.
g		goes to line given by prefix argument.
/ or M-C-s	searches forward for regular expression
\\ or M-C-r	searches backward for regular expression.
n		searches forward for last regular expression.
p		searches backward for last regular expression.
C-@ or .	set the mark.
x		exchanges dot and mark.
C-s or s	do forward incremental search.
C-r or r	do reverse incremental search.
@ or '		return to mark and pops mark ring.
		  Mark ring is pushed at start of every
		  successful search and when jump to line to occurs.
		  The mark is set on jump to buffer start or end.
? or h		provide help message (list of commands).
C-h		provides help (list of commands or description of a command).
C-n		moves down lines vertically.
C-p		moves upward lines vertically.
C-l		recenters the screen.
q or C-c	exit view-mode and return to previous buffer.

Entry to this mode calls the value of  view-hook  if non-nil."
  (interactive)
  (let* ((view-buffer-window (selected-window))
	 (view-scroll-size (1- (window-height view-buffer-window))))
    (unwind-protect
	(let ((buffer-read-only t)
	      (mode-line-format (if (buffer-file-name)
				    "--Viewing %f  %M  ----%p--%-"
				  "--Viewing %b  %M  ----%p--%-"))
	      (mode-name "View"))
	  (beginning-of-line)
	  (catch 'view-mode-exit (view-mode-command-loop)))
      (if view-return-to-buffer
	  (switch-to-buffer view-return-to-buffer)))))

(defun view-undefined ()
  (interactive)
  (ding)
  (message
   (substitute-command-keys
    "Type \\[Helper-help] for help, \\[Helper-describe-bindings] for commands, \\[exit-recursive-edit] to quit.")))

(defun view-set-window (end lines &optional newend)
  (move-to-window-line end)
  (forward-line lines)
  (cond ((= (dot) (dot-max)) (recenter -1))
	((= (dot) (dot-min)) (recenter 0))
	(t (recenter (or newend end))))
  (move-to-window-line -1)
  (beginning-of-line)
  (if (pos-visible-in-window-p (dot-max))
      (message (substitute-command-keys
		"End.  Type \\[exit-recursive-edit] to quit viewing."))))

(defun view-window-size () (1- (window-height view-buffer-window)))

(defun view-scroll-size () (min (view-window-size) view-scroll-size))

(defvar view-hook nil
  "If non-nil, its value is called when viewing buffer or file.")

(defun view-mode-command-loop ()
  (push-mark)
  (let ((old-local-map (current-local-map))
	(mark-ring)
;	(view-last-command)
;	(view-last-command-entry)
;	(view-last-command-argument)
	(view-last-regexp)
	(Helper-mode-name "View")
	(Helper-major-mode 'view-mode)
	(Helper-return-blurb
	 (format "continue viewing %s"
		 (if (buffer-file-name)
		     (file-name-nondirectory (buffer-file-name))
		   (buffer-name))))
	(goal-column 0))
    (unwind-protect
	(progn
	  (use-local-map view-mode-map)
	  (and (boundp 'view-hook) view-hook (funcall view-hook))
	  (message
	   (substitute-command-keys
	    "Type \\[Helper-help] for help, \\[Helper-describe-bindings] for commands, \\[exit-recursive-edit] to quit."))
	  (recursive-edit))
      (use-local-map old-local-map)))
  (pop-mark))

;(defun view-last-command (&optional who what)
;  (setq view-last-command-entry this-command)
;  (setq view-last-command who)
;  (setq view-last-command-argument what))

;(defun view-repeat-last-command ()
;  "Repeat last command issued in View mode."
;  (interactive)
;  (if (and view-last-command
;	   (eq view-last-command-entry last-command))
;      (funcall view-last-command view-last-command-argument))
;  (setq this-command view-last-command-entry))

(defun view-goto-line (&optional line)
  "Move to LINE in View mode.
Display is centered at LINE.  Sets mark at starting position and pushes
mark ring."
  (interactive "p")
  (push-mark)
  (goto-line (or line 1))
  (recenter (/ (view-window-size) 2)))

(defun view-scroll-lines-forward (&optional lines)
  "Scroll forward in View mode.
No arg means whole window full, or number of lines set by \\[view-scroll-lines-forward-set-scroll-size].
Arg is number of lines to scroll."
  (interactive "P")
  (setq lines
	(if lines (prefix-numeric-value lines)
	  (view-scroll-size)))
; (view-last-command 'view-scroll-lines-forward lines)
  (view-set-window (if (< lines 0) 0 -1) lines))

(defun view-scroll-lines-forward-set-scroll-size (&optional lines)
  "Scroll forward LINES lines in View mode, setting the \"scroll size\".
This is the number of lines which \\[view-scroll-lines-forward] and \\[view-scroll-lines-backward] scroll by default.
The absolute value of LINES is used, so this command can be used to scroll
backwards (but \"scroll size\" is always positive).  If LINES is greater than
window height or omitted, then window height is assumed.  If LINES is less
than window height then scrolling context is provided from previous screen."
  (interactive "P")
  (if (not lines)
      (setq view-scroll-size (view-window-size))
    (setq lines (prefix-numeric-value lines))
    (setq view-scroll-size
	  (min (if (> lines 0) lines (- lines)) (view-window-size))))
  (view-scroll-lines-forward lines))

(defun view-scroll-one-more-line (&optional arg)
  "Scroll one more line up in View mode.
With ARG scroll one line down."
  (interactive "P")
  (view-scroll-lines-forward (if (not arg) 1 -1)))

(defun view-scroll-lines-backward (&optional lines)
  "Scroll backward in View mode.
No arg means whole window full, or number of lines set by \\[view-scroll-lines-forward-set-scroll-size].
Arg is number of lines to scroll."
  (interactive "P")
  (view-scroll-lines-forward (if lines
				 (- (prefix-numeric-value lines))
			       (- (view-scroll-size)))))
  
(defun view-search-regexp-forward (times regexp)
  "Search forward for NTH occurrence of REGEXP in View mode.
Displays line found at center of window.  REGEXP is remembered for
searching with \\[view-search-last-regexp-forward] and \\[view-search-last-regexp-backward].  Sets mark at starting position and pushes mark ring."
  (interactive "p\nsSearch forward (regexp): ")
  (if (> (length regexp) 0)
      (progn
       ;(view-last-command 'view-search-last-regexp-forward times)
	(view-search times regexp))))

(defun view-search-regexp-backward (times regexp)
  "Search backward from window start for NTH instance of REGEXP in View mode.
Displays line found at center of window.  REGEXP is remembered for
searching with \\[view-search-last-regexp-forward] and \\[view-search-last-regexp-backward].  Sets mark at starting position and pushes mark ring."
  (interactive "p\nsSearch backward (regexp): ")
  (view-search-regexp-forward (- times) regexp))

(defun view-search-last-regexp-forward (times)
  "Search forward from window end for NTH instance of last regexp in View mode.
Displays line found at center of window.  Sets mark at starting position
and pushes mark ring."
  (interactive "p")
  (view-search-regexp-forward times view-last-regexp))

(defun view-search-last-regexp-backward (times)
  "Search backward from window start for NTH instance of last regexp in View mode.
Displays line found at center of window.  Sets mark at starting position and
pushes mark ring."
  (interactive "p")
  (view-search-regexp-backward times view-last-regexp))

(defun view-back-to-mark (&optional ignore)
  "Return to last mark set in View mode, else beginning of file.
Displays line at center of window.  Pops mark ring so successive
invocations return to earlier marks."
  (interactive)
  (goto-char (or (mark) (dot-min)))
  (pop-mark)
  (recenter (/ (view-window-size) 2)))
	     
(defun view-search (times regexp)
  (setq view-last-regexp regexp)
  (let (where)
    (save-excursion
      (move-to-window-line (if (< times 0) 0 -1))
      (if (re-search-forward regexp nil t times)
	  (setq where (dot))))
    (if where
	(progn
	  (push-mark)
	  (goto-char where)
	  (beginning-of-line)
	  (recenter (/ (view-window-size) 2)))
      (message "Can't find occurrence %d of %s" times regexp)
      (sit-for 4))))

