;; Paragraph and sentence parsing.
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


(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.  With arg, do it arg times.
A line which  paragraph-start  matches either separates paragraphs
\(if  paragraph-separate  matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (let* ((fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (regexp-quote fill-prefix)))
	 (paragraph-separate
	  (if fill-prefix-regexp
	      (concat paragraph-separate "\\|^"
		      fill-prefix-regexp "[ \t]*$")
	    paragraph-separate)))
    (while (< arg 0)
      (if (and (not (looking-at paragraph-separate))
	       (re-search-backward "^\n" (max (1- (dot)) (dot-min)) t))
	  nil
	(forward-char -1) (beginning-of-line)
	(while (looking-at paragraph-separate)
	  (forward-line -1))
	(end-of-line)
	;; Saerch back for line that starts or separates paragraphs.
	(if (if (and fill-prefix (not (equal fill-prefix "")))
		;; There is a fill prefix; it overrides paragraph-start.
		(progn
		 (while (progn (beginning-of-line)
			       (and (not (bobp))
				    (not (looking-at paragraph-separate))
				    (looking-at fill-prefix-regexp)))
		   (forward-line -1))
		 (not (bobp)))
	      (re-search-backward paragraph-start nil t))
	    ;; Found one.
	    (progn
	      (while (looking-at paragraph-separate)
		(forward-line 1))
	      (if (eq (char-after (- (dot) 2)) ?\n)
		  (forward-line -1)))
	  ;; No starter or separator line => use buffer beg.
	  (goto-char (dot-min))))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (beginning-of-line)
      (while (prog1 (and (not (eobp))
			 (looking-at paragraph-separate))
		    (forward-line 1)))
      (if (and fill-prefix (not (equal fill-prefix "")))
	  ;; There is a fill prefix; it overrides paragraph-start.
	  (while (and (not (eobp))
		      (not (looking-at paragraph-separate))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(if (re-search-forward paragraph-start nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (dot-max))))
      (setq arg (1- arg)))))

(defun backward-paragraph (&optional arg)
  "Move backward to start of paragraph.  With arg, do it arg times.
A paragraph start is the beginning of a line which is a first-line-of-paragraph
or which is ordinary text and follows a paragraph-separating line; except:
if the first real line of a paragraph is preceded by a blank line,
the paragraph starts at that blank line.
See forward-paragraph for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-paragraph (- arg)))

(defun mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end."
  (interactive)
  (forward-paragraph 1)
  (set-mark (dot))
  (backward-paragraph 1))

(defun kill-paragraph (arg)
  "Kill to end of paragraph."
  (interactive "p")
  (kill-region (dot) (progn (forward-paragraph arg) (dot))))

(defun backward-kill-paragraph (arg)
  "Kill back to start of paragraph."
  (interactive "p")
  (kill-region (dot) (progn (backward-paragraph arg) (dot))))

(defun transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "p")
  (transpose-subr 'forward-paragraph arg))

(defun start-of-paragraph-text ()
  (let ((odot (dot)) ndot)
    (forward-paragraph -1)
    (setq ndot (dot))
    (skip-chars-forward " \t\n")
    (if (>= (dot) odot)
	(progn
	  (goto-char ndot)
	  (if (> ndot (dot-min))
	      (start-of-paragraph-text))))))

(defun end-of-paragraph-text ()
  (let ((odot (dot)))
    (forward-paragraph 1)
    (if (eq (preceding-char) ?\n) (forward-char -1))
    (if (<= (dot) odot)
	(progn
	  (forward-char 1)
	  (if (< (dot) (dot-max))
	      (end-of-paragraph-text))))))

(defun forward-sentence (&optional arg)
  "Move forward to next sentence-end.  With argument, repeat.
With negative argument, move backward repeatedly to sentence-beginning.
Sentence ends are identified by the value of sentence-end
treated as a regular expression.  Also, every paragraph boundary
terminates sentences as well."
  (interactive "p")
  (or arg (setq arg 1))
  (while (< arg 0)
    (let ((par-beg (save-excursion (start-of-paragraph-text) (dot))))
      (if (re-search-backward (concat sentence-end "[^ \t\n]") par-beg t)
	  (goto-char (1- (match-end 0)))
	(goto-char par-beg)))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (let ((par-end (save-excursion (end-of-paragraph-text) (dot))))
      (if (re-search-forward sentence-end par-end t)
	  (skip-chars-backward " \t\n")
	(goto-char par-end)))
    (setq arg (1- arg))))

(defun backward-sentence (&optional arg)
  "Move backward to start of sentence.  With arg, do it arg times.
See forward-sentence for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sentence (- arg)))

(defun kill-sentence (&optional arg)
  "Kill from dot to end of sentence.
With arg, repeat, or backward if negative arg."
  (interactive "p")
  (let ((beg (dot)))
    (forward-sentence arg)
    (kill-region beg (dot))))

(defun backward-kill-sentence (&optional arg)
  "Kill back from dot to start of sentence.
With arg, repeat, or forward if negative arg."
  (interactive "p")
  (let ((beg (dot)))
    (backward-sentence arg)
    (kill-region beg (dot))))

(defun mark-end-of-sentence (arg)
  "Put mark at end of sentence.  Arg works as in forward-sentence."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-sentence arg)
      (dot))))

(defun transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "p")
  (transpose-subr 'forward-sentence arg))
