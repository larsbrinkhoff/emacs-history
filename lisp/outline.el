;; Outline mode commands for Emacs
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


(defvar outline-mode-map nil "")

(if outline-mode-map
    nil
  (setq outline-mode-map (make-keymap))
  (define-key outline-mode-map "\e}" 'next-visible-heading)
  (define-key outline-mode-map "\e{" 'previous-visible-heading)
  (define-key outline-mode-map "\es" 'show-children)
  (define-key outline-mode-map "\eS" 'show-subtree)
  (define-key outline-mode-map "\eH" 'hide-subtree))

(defun outline-mode ()
  "Set major mode for editing outlines with selective display.
Headings should be lines starting with one or more asterisks.
Major headings have one asterisk, subheadings two, etc.
Lines not starting with asterisks are ordinary text.

You can make the text under a heading, or the subheadings
under a heading, temporarily invisible, or visible again.
Invisible lines are attached to the end of the previous line
so they go with it if you kill it and yank it back.

Commands:
Meta-}   next-visible-heading      move by visible headings
Meta-{   previous-visible-heading  move by visible headings

Meta-X hide-text	make all text invisible (not headings).
Meta-X show-all		make everything in buffer visible.

The remaining commands are used when dot is on a heading line.
They apply to some of the text or subheadings of that heading.
Meta-H   hide-subtree	make text and subheadings invisible.
Meta-S   show-subtree	make text and subheadings visible.
Meta-s   show-children	make direct subheadings visible.
		 No effect on text, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
hide-entry	make immediately following text invisible.
show-entry	make it visible.
hide-leaves	make text under heading and under its subheadings invisible.
		 The subheadings remain visible.
show-branches	make all subheadings at all levels visible."
  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (and (boundp 'outline-mode-hook)
       outline-mode-hook
       (funcall outline-mode-hook)))

(defun outline-level ()
  (save-excursion
   (- (- (dot) (progn (skip-chars-forward "^ \t") (dot))))))

(defun next-heading-preface ()
  (if (re-search-forward "[\n\^M]\\*"
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (memq (preceding-char) '(?\n ?\^M))
      (forward-char -1)))

(defun next-heading ()
  "Move to the next heading line (a line starting with *'s)."
  (interactive)
  (if (re-search-forward "[\n\^M]\\*"
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun next-visible-heading (arg)
  "Move to the next visible heading line (a line starting with *'s).
With argument, repeats or can move backward if negative."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (forward-line 1))
  (re-search-forward "^\\*" nil nil arg)
  (beginning-of-line))

(defun previous-visible-heading (arg)
  "Move to the previous heading line (a line starting with *'s).
With argument, repeats or can move forward if negative."
  (interactive "p")
  (if (> arg 0)
      (beginning-of-line)
    (forward-line 1))
  (re-search-backward "^\\*" nil nil arg)
  (beginning-of-line))

(defun flag-lines-in-region (from to flag)
  (let ((modp (buffer-modified-p)))
    (unwind-protect
     (subst-char-in-region from to
			   (if (= flag ?\n) ?\^M ?\n)
			   flag)
     (set-buffer-modified-p modp))))

(defun hide-entry ()
  "Hide the text directly following this heading."
  (interactive)
  (save-excursion
   (flag-lines-in-region (dot) (progn (next-heading-preface) (dot)) ?\^M)))

(defun show-entry ()
  "Show the text directly following this heading."
  (interactive)
  (save-excursion
   (flag-lines-in-region (dot) (progn (next-heading-preface) (dot)) ?\n)))

(defun hide-text ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-text (dot-min) (dot-max)))

(defun hide-region-text (start end)
  "Hide all text lines in the region, but not headings."
  (save-excursion
   (save-restriction
    (narrow-to-region start end)
    (goto-char (dot-min))
    (while (not (eobp))
     (flag-lines-in-region (dot) (progn (next-heading-preface) (dot)) ?\^M)
     (forward-char
      (if (looking-at "[\n\^M][\n\^M]")
	  2 1))))))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (flag-lines-in-region (dot-min) (dot-max) ?\n))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (flag-subtree ?\^M))

(defun hide-leaves ()
  "Hide all text after this heading at deeper levels."
  (interactive)
  (hide-region-text (dot) (progn (end-of-subtree) (dot))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (flag-subtree ?\n))

(defun flag-subtree (flag)
  (save-excursion
   (flag-lines-in-region (dot)
			 (progn (end-of-subtree) (dot))
			 flag)))

(defun end-of-subtree ()
  (beginning-of-line)
  (let ((odot (dot))
	(first t)
	(level (outline-level)))
    (while (and (not (eobp))
		(or first (> (outline-level) level)))
      (setq first nil)
      (next-heading))
    (forward-char -1)
    (if (memq (preceding-char) '(?\n ?\^M))
	(forward-char -1))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their text."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading."
  (interactive "p")
  (or level (setq level 1))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (outline-level)))
    (narrow-to-region (dot)
		      (progn (end-of-subtree) (1+ (dot))))
    (goto-char (dot-min))
    (while (and (not (eobp))
		(progn
		 (next-heading)
		 (not (eobp))))
      (if (<= (outline-level) level)
	  (save-excursion
	   (let ((end (1+ (dot))))
	     (forward-char -1)
	     (if (memq (preceding-char) '(?\n ?\^M))
		 (forward-char -1))
	     (flag-lines-in-region (dot) end ?\n))))))))

