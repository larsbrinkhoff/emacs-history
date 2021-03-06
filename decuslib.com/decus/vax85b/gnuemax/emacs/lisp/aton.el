;; Find occurrences of regexp and pick one from a menu. 
;; Copyright (C) 1985 Richard M. Stallman. 
;;  based loosely on mocklisp original by Jeff Shrager and Duane Williams: CMU. 
 
;; This file is part of GNU Emacs. 
 
;; GNU Emacs is distributed in the hope that it will be useful, 
;; but without any warranty.  No author or distributor 
;; accepts responsibility to anyone for the consequences of using it 
;; or for whether it serves any particular purpose or works at all, 
;; unless he says so in writing. 
 
;; Everyone is granted permission to copy, modify and redistribute 
;; GNU Emacs, but only under the conditions described in the 
;; document "GNU Emacs copying permission notice".   An exact copy 
;; of the document is supposed to have been given to you along with 
;; GNU Emacs so that you can know how you may redistribute it all. 
;; It should be in a file named COPYING.  Among other things, the 
;; copyright notice and this notice must be preserved on all copies. 
 
 
(defun occur-menu (string) 
  "Show menu of lines containing match for REGEXP. 
Enters recursive edit on text showing an entry for each matching line. 
User can move to an entry and then exit with C-Z to 
move to the line in the original buffer described by the selected entry. 
Abort with C-] to avoid moving in the original buffer. 
 
If REGEXP is empty then THE EXACT SAME menu is presented again, 
with cursor initially at the next successive entry. 
This is useful for stepping through located lines rapidly in order." 
  (interactive "sOccur menu (regexp): ") 
  (let (ln track-eol 
	   (accumbuf (get-buffer-create " *Occur menu*")) 
	  (databuf (current-buffer)) 
	  (prev (dot-min)) 
	  (rebuild-summary (not (string-equal string "")))) 
    (if rebuild-summary 
	(progn 
	 (save-excursion			; setup temp buffer 
	  (set-buffer accumbuf) 
	  (erase-buffer)) 
	 (save-excursion 
	  (beginning-of-buffer) 
	  (setq ln 1)			; init accumulator 
	  (while (re-search-forward string nil t) 
	    (beginning-of-line) 
	    (setq ln (+ ln (count-lines prev (dot)))) 
	    (setq beg (dot)) 
	    (setq prev (dot)) 
	    (end-of-line)	; to pick up entire line 
	    (setq end (dot)) 
	    (save-excursion 
	     (set-buffer accumbuf) 
	     (insert (int-to-string ln) ". ") 
	     (insert-buffer-substring databuf beg end) 
	     (newline)) 
	    (forward-line 1)) 
	  (set-buffer accumbuf) 
	  (set-buffer-modified-p nil)))) 
    (goto-line 
     (save-window-excursion 
      (switch-to-buffer accumbuf) 
      (delete-other-windows) 
      (forward-line 
       (if rebuild-summary -1 1)) 
      (if (= (dot-max) 1) 
	  (error "No occurrences found.")) 
      (message "Use C-Z to expand to the selected line.") 
      (recursive-edit) 
      (beginning-of-line) 
      (read accumbuf))))) 
