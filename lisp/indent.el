;; Indentation commands for Emacs
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


;Now in autoloads.el
;(defvar indent-line-function
;  'indent-to-left-margin
;  "Function to indent current line.")

(defun indent-according-to-mode ()
  "Indent line in proper way for current major mode."
  (interactive)
  (funcall indent-line-function))

(defun indent-for-tab-command ()
  "Indent line in proper way for current major mode."
  (interactive)
  (if (eq indent-line-function 'indent-to-left-margin)
      (insert-tab)
    (funcall indent-line-function)))

(defun insert-tab ()
  (if abbrev-mode
      (expand-abbrev))
  (insert ?\t))

(defun indent-rigidly (start end arg)
  "Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (setq end (dot-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (dot) end)
      (let ((indent (current-indentation)))
	(delete-region (dot) (progn (skip-chars-forward " \t") (dot)))
	(or (eolp)
	    (indent-to (max 0 (+ indent arg)) 0)))
      (forward-line 1))
    (move-marker end nil)))

;; This is the default indent-line-function,
;; used in Fundamental Mode, Text Mode, etc.
(defun indent-to-left-margin ()
  (or (= (current-indentation) left-margin)
      (let (epos)
	(save-excursion
	 (beginning-of-line)
	 (delete-region (dot)
			(progn (skip-chars-forward " \t")
			       (dot)))
	 (indent-to left-margin)
	 (setq epos (dot)))
	(if (< (dot) epos)
	    (goto-char epos)))))

(defvar indent-region-function nil
  "Function which is short cut to indent each line in region with Tab.
nil means really call Tab on each line.")

(defun indent-region (start end arg)
  "Indent each nonblank line in the region.
With no argument, indent each line with Tab.
With argument COLUMN, indent each line to that column.
Called from a program, takes three args: START, END and COLUMN."
  (interactive "r\nP")
  (if (null arg)
      (if indent-region-function
	  (funcall indent-region-function start end)
	(save-excursion
	 (goto-char end)
	 (setq end (dot-marker))
	 (goto-char start)
	 (or (bolp) (forward-line 1))
	 (while (< (dot) end)
	   (funcall indent-line-function)
	   (forward-line 1))
	 (move-marker end nil)))
    (setq arg (prefix-numeric-value arg))
  (save-excursion
   (goto-char end)
   (setq end (dot-marker))
   (goto-char start)
   (or (bolp) (forward-line 1))
   (while (< (dot) end)
     (delete-region (dot) (progn (skip-chars-forward " \t") (dot)))
     (or (eolp)
	 (indent-to arg 0))
     (forward-line 1))
   (move-marker end nil))))

(defun indent-relative ()
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
If the previous nonblank line has no indent points beyond
the column dot starts at,  tab-to-tab-stop  is done instead."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
	  (progn
	   (move-to-column start-column)
	   (skip-chars-forward " \t")
	   (setq indent (current-column)))))
    (if indent
	(let ((odot (dot-marker)))
	  (delete-region (dot) (progn (skip-chars-backward " \t") (dot)))
	  (indent-to indent 0)
	  (if (> odot (dot))
	      (goto-char odot))
	  (move-marker odot nil))
      (tab-to-tab-stop))))

(defvar tab-stop-list
  '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
  "List of tab stop positions used by tab-to-tab-stops.")

(defvar edit-tab-stops-map nil
  "Keymap used in edit-tab-stops.")

(defvar edit-tab-stops-buffer nil
  "Buffer whose tab stops are being edited--in case
the variable tab-stop-list is local in that buffer.")

(defun edit-tab-stops ()
  "Edit the tab stops used by tab-to-tab-stop.
Creates a buffer *Tab Stops* containing text describing the tab stops.
A colon indicates a column where there is a tab stop.
You can add or remove colons and then do C-X C-S to make changes take effect."
  (interactive)
  (setq edit-tab-stops-buffer (current-buffer))
  (switch-to-buffer (get-buffer-create "*Tab Stops*"))
  (if edit-tab-stops-map
      (use-local-map edit-tab-stops-map)
    (setq edit-tab-stops-map (make-keymap))
    (use-local-map edit-tab-stops-map)
    (define-key edit-tab-stops-map "\^x\^s" 'edit-tab-stops-note-changes))
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'overwrite-mode)
  (setq overwrite-mode t)
  (setq truncate-lines t)
  (erase-buffer)
  (let ((tabs tab-stop-list))
    (while tabs
      (indent-to (car tabs) 0)
      (insert ?:)
      (setq tabs (cdr tabs))))
  (let ((count 0))
    (insert ?\n)
    (while (< count 8)
      (insert (+ count ?0))
    (insert "         ")
      (setq count (1+ count)))
    (insert ?\n)
    (while (> count 0)
      (insert "0123456789")
      (setq count (1- count))))
  (insert "\nTo install changes, type C-X C-S"))

(defun edit-tab-stops-note-changes ()
  "Put edited tab stops into effect."
  (interactive)
    (let (tabs)
      (save-excursion
	(goto-char 1)
	(end-of-line)
	(while (search-backward ":" nil t)
	  (setq tabs (cons (current-column) tabs))))
      (bury-buffer (prog1 (current-buffer)
			  (switch-to-buffer edit-tab-stops-buffer)))
      (setq tab-stop-list tabs))
  (message "Tab stops installed"))

(defun tab-to-tab-stop ()
  "Insert spaces or tabs to next defined tab-stop column.
The variable tab-stop-list is a list of columns at which there are tab stops.
Use \\[edit-tab-stops] to edit them interactively."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
	(indent-to (car tabs))
      (insert ? ))))

(define-key global-map "\t" 'indent-for-tab-command)
(define-key esc-map "\034" 'indent-region)
(define-key ctl-x-map "\t" 'indent-rigidly)
(define-key esc-map "i" 'tab-to-tab-stop)
