;; Tags facility for Emacs.
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


(defvar tag-table-files nil
  "List of file names covered by current tag table.
nil means it has not been computed yet; do (tag-table-files) to compute it.")

(defvar last-tag nil
  "Tag found by the last find-tag.")

(defun visit-tag-table (file)
  "Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with the `etags' program."
  (interactive "fVisit tag table: ")
  (setq tags-file-name (expand-file-name file)))

(defun visit-tag-table-buffer ()
  "Select the buffer containing the current tag table.
This is a file whose name is in the variable tags-file-name."
  (or tags-file-name
      (call-interactively 'visit-tag-table))
  (set-buffer (or (get-file-buffer tags-file-name)
		  (progn
		    (setq tag-table-files nil)
		    (find-file-noselect tags-file-name))))
  (or (= (char-after 1) ?\^L)
      (error "File %s not a valid tag table" tags-file-name))
  (or (verify-visited-file-modtime (get-file-buffer tags-file-name))
      (cond ((yes-or-no-p "Tags file has changed, read new contents? ")
	     (revert-buffer t)
	     (setq tag-table-files nil)))))

(defun file-of-tag ()
  "Return the file name of the file whose tags dot is within.
Assuems the tag table is the current buffer.
File name returned is relative to tag table file's directory."
  (let ((odot (dot))
	prev size)
    (save-excursion
     (goto-char (dot-min))
     (while (< (dot) odot)
       (forward-line 1)
       (end-of-line)
       (skip-chars-backward "^,\n")
       (setq prev (dot))
       (setq size (read (current-buffer)))
       (goto-char prev)
       (forward-line 1)
       (forward-char size))
     (goto-char (1- prev))
     (buffer-substring (dot)
		       (progn (beginning-of-line) (dot))))))

(defun tag-table-files ()
  "Return a list of files in the current tag table.
File names returned are absolute."
  (save-excursion
   (visit-tag-table-buffer)
   (or tag-table-files
       (let (files)
	(goto-char (dot-min))
	(while (not (eobp))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward "^,\n")
	  (setq prev (dot))
	  (setq size (read (current-buffer)))
	  (goto-char prev)
	  (setq files (cons (expand-file-name
			     (buffer-substring (1- (dot))
					       (save-excursion
						(beginning-of-line)
						(dot)))
			     (file-name-directory tags-file-name))
			    files))
	  (forward-line 1)
	  (forward-char size))
	(setq tag-table-files (nreverse files))))))

(defun find-tag (tagname &optional next other-window)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts dot at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before dot is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive "sFind tag: \nP")
  (if (equal tagname "")
      (setq tagname (save-excursion
		     (buffer-substring
		      (progn (backward-sexp 1) (dot))
		      (progn (forward-sexp 1) (dot))))))
  (let (buffer file linebeg startpos)
    (save-excursion
     (visit-tag-table-buffer)
     (if (not next)
	 (goto-char (dot-min))
       (setq tagname last-tag))
     (setq last-tag tagname)
     (while (progn
	     (search-forward tagname)
	     (not (looking-at "[^\n\177]*\177"))))
     (search-forward "\177")
     (setq file (expand-file-name (file-of-tag)
				  (file-name-directory tags-file-name)))
     (setq linebeg
	   (buffer-substring (1- (dot))
			     (save-excursion (beginning-of-line) (dot))))
     (search-forward ",")
     (setq startpos (read (current-buffer))))
    (if other-window
	(find-file-other-window file)
      (find-file file))
    (widen)
    (let ((offset 1000)
	  found
	  (pat (concat "^" (regexp-quote linebeg))))
      (or startpos (setq startpos (dot-min)))
      (while (and (not found)
		  (progn
		   (goto-char (- startpos offset))
		   (not (bobp))))
	(setq found
	      (re-search-forward pat (+ startpos offset) t))
	(setq offset (* 3 offset)))
      (or found
	  (re-search-forward pat)))
    (beginning-of-line)))

(defun find-tag-other-window (tagname &optional next)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts dot at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before dot is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive "sFind tag other window: \nP")
  (find-tag tagname next t))

(defvar next-file-list nil
  "List of files for next-file to process.")

(defun next-file (&optional initialize)
  "Select next file among files in current tag table.
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the tag table."
  (interactive "P")
  (if initialize
      (setq next-file-list (tag-table-files)))
  (or next-file-list
      (error "No more files."))
  (find-file (car next-file-list))
  (setq next-file-list (cdr next-file-list)))

(defvar tags-loop-form nil
  "Form for tags-loop to eval to process one file.
If it returns nil, it is through with one file; move on to next.")

(defun tags-loop-continue (&optional first-time)
  "Continue last tags-search or tags-query-replace command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  (interactive)
  (if first-time
      (progn (next-file t)
	     (goto-char (dot-min))))
  (while (not (eval tags-loop-form))
    (next-file)
    (goto-char (dot-min))))

(defun tags-search (regexp)
  "Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."

  (interactive "sTags search (regexp): ")
  (setq tags-loop-form
	(list 're-search-forward regexp nil t))
  (tags-loop-continue t))

(defun tags-query-replace (from to)
  "Query-replace FROM with TO through all files listed in tag table.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags query replace (regexp): \nsTags query replace %s by: ")
  (setq tags-loop-form
	(list 'and (list 'save-excursion
			 (list 're-search-forward from nil t))
	      (list 'not (list 'query-replace-regexp from to))))
  (tags-loop-continue t))

(defun list-tags (string)
  "Display list of tags in file FILE.
FILE should not contain a directory spec
unless it has one in the tag table."
  (interactive "sList tags (in file): ")
  (with-output-to-temp-buffer "*Help*"
    (princ "Tags in file ")
    (princ string)
    (terpri)
    (save-excursion
     (visit-tag-table-buffer)
     (goto-char 1)
     (search-forward (concat "\f\n" string ","))
     (forward-line 1)
     (while (not (looking-at "\f"))
       (princ (buffer-substring (dot)
				(progn (skip-chars-forward "^\177")
				       (dot))))
       (terpri)
       (forward-line 1)))))

(defun tags-apropos (string)
  "Display list of all tags in tag table REGEXP matches."
  (interactive "sTag apropos (regexp): ")
  (with-output-to-temp-buffer "*Help*"
    (princ "Tags matching regexp ")
    (prin1 string)
    (terpri)
    (save-excursion
     (visit-tag-table-buffer)
     (goto-char 1)
     (while (re-search-forward string nil t)
       (beginning-of-line)
       (princ (buffer-substring (dot)
				(progn (skip-chars-forward "^\177")
				       (dot))))
       (terpri)
       (forward-line 1)))))
