; buggestions to mly@mit-mc

;; who says one can't have typeout windows in gnu emacs?
;; like ^r select buffer from its emacs lunar or tmacs libraries.

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

(require 'electric)

;; this depends on the format of list-buffers (from src/buffer.c) and
;; on stuff in lisp/buff-menu.el

(defun electric-buffer-list (arg)
  "Vaguely like ITS lunar select buffer;
combining typeoutoid buffer listing with menuoid buffer selection.
This pops up a buffer describing the set of emacs buffers.
If the very next character typed is a space then the buffer list is killed.

Otherwise, one may use \\[next-line] and \\[previous-line] to move around in the buffer list window
and select a buffer by typing Space when the cursor is on the
appropriate line of the buffer-list window.
Other commands are much like those of buffer-menu-mode.
Type C-h after calling \\[electric-buffer-list] for more information.

Calls value of  electric-buffer-menu-mode-hook  on entry if non-nil.
Calls value of  after-electric-buffer-menu  on exit (select) if non-nil."
 
  (interactive "P")
  (let ((Helper-return-blurb "return to buffer editing")
	(Helper-major-mode 'electric-buffer-menu-mode)
	(Helper-mode-name "Electric Buffer Menu")
	(goal-column)
	(select)
	(buffer))
    (save-window-excursion
      (save-window-excursion (list-buffers arg))
      (setq buffer (window-buffer (Electric-pop-up-window "*Buffer List*")))
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (electric-buffer-menu-mode)
	    (setq select
		  (catch 'electric-buffer-menu-select
		    (let ((first (progn (goto-char (dot-min))
					(forward-line 2)
					(dot)))
			  (last (progn (goto-char (dot-max))
				       (forward-line -1)
				       (dot))))
		      (goto-char first)
		      (Electric-command-loop 'electric-buffer-menu-select
					     nil
					     t
					     'Electric-buffer-menu-looper
					     (cons first last))))))
	(set-buffer buffer)
	(Buffer-menu-mode)
	(or select (bury-buffer buffer))
	(message "")))
    (if select
	(progn (set-buffer buffer)
	       (set-mark select)	; Buffer-menu-execute changes things.
	       (Buffer-menu-execute)
	       (goto-char (mark))
	       (Buffer-menu-select)
	       (and (boundp 'after-electric-buffer-menu)
		    after-electric-buffer-menu
		    (funcall after-electric-buffer-menu))))))

(defun Electric-buffer-menu-looper (state conditions)
  (cond ((and conditions
	      (not (memq (car conditions) '(buffer-read-only end-of-buffer))))
	 (signal (car conditions) (cdr conditions)))
	((< (dot) (car state))
	 (Electric-buffer-menu-beginning))
	((> (dot) (cdr state))
	 (Electric-buffer-menu-end))))

(defvar Electric-buffer-menu-mode-map nil)

(defun electric-buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
C-c or C-g -- exit buffer menu, returning to previous window and buffer
  configuration.  If the very first character typed is a space, it
  also has this effect.
Space -- select buffer of line dot is on.
  Also show buffers marked with m in other windows,
  kills buffers marked with k, and saves those marked with s.
m -- mark buffer to be displayed.
~ -- clear modified-flag on that buffer.
s -- mark that buffer to be saved.
d or k or C-d or C-k -- mark that buffer to be killed.
u -- remove all kinds of marks from current line.
v -- view buffer, returning when done.
Delete -- back up a line and remove marks.

Entry to this mode via command \\[electric-buffer-list] calls the value of
electric-buffer-menu-mode-hook if it is non-nil.  The value of
after-electric-buffer-menu is called on exit (when a selection is made)
if it is non-nil."
  (kill-all-local-variables)
  (use-local-map Electric-buffer-menu-mode-map)
  (setq mode-line-format "-- Electric Buffer List   %M   %[%]----%3p-%-")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'electric-buffer-menu-mode)
  (setq mode-name "Electric Buffer Menu")
  (and (boundp 'electric-buffer-menu-mode-hook)
       electric-buffer-menu-mode-hook
       (funcall electric-buffer-menu-mode-hook)))

;; generally the same as Buffer-menu-mode-map (except we don't indirect)
(save-excursion
  (setq Electric-buffer-menu-mode-map (make-keymap))
  (fillarray Electric-buffer-menu-mode-map 'Electric-buffer-menu-undefined)
  (define-key Electric-buffer-menu-mode-map "\e" (make-keymap))
  (fillarray (lookup-key Electric-buffer-menu-mode-map "\e")
	     'Electric-buffer-menu-undefined)
  (define-key Electric-buffer-menu-mode-map "\C-z"
    'suspend-emacs)
  (define-key Electric-buffer-menu-mode-map "v"
    'Electric-buffer-menu-mode-view-buffer)
  (define-key Electric-buffer-menu-mode-map "\C-h"
    'Helper-help)
  (define-key Electric-buffer-menu-mode-map "?"
    'Helper-describe-bindings)
  (define-key Electric-buffer-menu-mode-map "\C-c"
    'Electric-buffer-menu-quit)
  (define-key Electric-buffer-menu-mode-map "\C-g"
    nil)  
  (define-key Electric-buffer-menu-mode-map "\e\C-g"
    'undefined)				
  (define-key Electric-buffer-menu-mode-map "\C-]"
    'Electric-buffer-menu-quit)
  (define-key Electric-buffer-menu-mode-map "q"
    'Electric-buffer-menu-quit)
  (define-key Electric-buffer-menu-mode-map " "
    'Electric-buffer-menu-select)  
  (define-key Electric-buffer-menu-mode-map "\C-l"
    'recenter)
  (define-key Electric-buffer-menu-mode-map "s"
    'Buffer-menu-save)
  (define-key Electric-buffer-menu-mode-map "d"
    'Buffer-menu-kill)
  (define-key Electric-buffer-menu-mode-map "k"
    'Buffer-menu-kill)
  (define-key Electric-buffer-menu-mode-map "\C-d"
    'Buffer-menu-kill)
  (define-key Electric-buffer-menu-mode-map "\C-k"
    'Buffer-menu-kill)
  (define-key Electric-buffer-menu-mode-map "\177"
    'Buffer-menu-backup-unmark)
  (define-key Electric-buffer-menu-mode-map "~"
    'Buffer-menu-not-modified)
  (define-key Electric-buffer-menu-mode-map "u"
    'Buffer-menu-unmark)
  (define-key Electric-buffer-menu-mode-map "m"
    'Buffer-menu-mark)
  (define-key Electric-buffer-menu-mode-map "\C-u"
    'universal-argument)
  (define-key Electric-buffer-menu-mode-map "\C-p"
    'previous-line)
  (define-key Electric-buffer-menu-mode-map "\C-n"
    'next-line)
  (define-key Electric-buffer-menu-mode-map "\C-v"
    'scroll-up)
  (define-key Electric-buffer-menu-mode-map "\ev"
    'scroll-down)
  (define-key Electric-buffer-menu-mode-map "\e\C-v"
    'scroll-other-window)
  (define-key Electric-buffer-menu-mode-map "\e>"
    'Electric-buffer-menu-end)
  (define-key Electric-buffer-menu-mode-map "\e<"
    'Electric-buffer-menu-beginning))

(defun Electric-buffer-menu-exit ()
  (interactive)
  (setq unread-command-char last-input-char)
  ;; for robustness
  (condition-case ()
      (throw 'electric-buffer-menu-select nil)
    (error (Buffer-menu-mode)
	   (other-buffer))))

(defun Electric-buffer-menu-select ()
  "Leave Electric Buffer Menu, selecting buffers and executing changes.
Saves buffers marked `S'.  Kills buffers marked `K'.
Selects buffer at dot and displays buffers marked `>' in other
windows."
  (interactive)
  (throw 'electric-buffer-menu-select (dot)))

(defun Electric-buffer-menu-quit ()
  "Leave Electric Buffer Menu, restoring previous window configuration.
Does not execute select, save, or kill commands."
  (interactive)
  (throw 'electric-buffer-menu-select nil))

(defun Electric-buffer-menu-undefined ()
  (interactive)
  (ding)
  (message (if (and (eq (key-binding "\C-c") 'Electric-buffer-menu-quit)
		    (eq (key-binding " ") 'Electric-buffer-menu-select)
		    (eq (key-binding "\C-h") 'Helper-help)
		    (eq (key-binding "?") 'Helper-describe-bindings))
	       "Type C-c to exit, Space to select, C-h for help, ? for commands"
	     (substitute-command-keys "\
Type \\[Electric-buffer-menu-quit] to exit, \
\\[Electric-buffer-menu-select] to select, \
\\[Helper-help] for help, \\[Helper-describe-bindings] for commands.")))
  (sit-for 4))

(defun Electric-buffer-menu-beginning ()
  "Move to the first buffer description in Electric Buffer Menu."
  (interactive)
  (goto-char (dot-min))
  (forward-line 2))

(defun Electric-buffer-menu-end ()
  "Move to the last buffer description in Electric Buffer Menu."
  (interactive)
  (goto-char (dot-max))
  (forward-line -1)
  (if (not (pos-visible-in-window-p (dot-max)))
      (recenter (1- (/ (window-height (selected-window)) 2)))))

(defun Electric-buffer-menu-mode-view-buffer ()
  "View buffer on current line in Electric Buffer Menu.
Returns to Electric Buffer Menu when done."
  (interactive)
  (let ((bufnam (Buffer-menu-buffer nil)))
    (if bufnam
	(view-buffer bufnam)
      (ding)
      (message "Buffer does not exist!")
      (sit-for 4))))




