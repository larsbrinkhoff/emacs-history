;;; Missing: P command, sorting, setting file modes.
;;; Dired buffer containing multiple directories gets totally confused
;;; Implement insertion of subdirectories in situ --- tree dired

;; DIRED commands for Emacs
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

;Will be in loaddefs.el for versions 16.2 and up.
(defvar dired-listing-switches "-al"
  "Switches passed to ls for dired. MUST contain the 'l' option.
CANNOT contain the 'F' option.")

;Flush for versions 16.2 and up (in loaddefs.el).
(or (fboundp 'view-file)
    (autoload 'view-file "view"
      "\
View FILE in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing."
      t))

(defun dired-readin (dirname buffer)
  (save-excursion
   (set-buffer buffer)
   (let ((buffer-read-only nil))
     (widen)
     (erase-buffer)
     (setq dirname (expand-file-name dirname))
     (if (file-directory-p dirname)
	 (call-process "ls" nil buffer nil
		       dired-listing-switches dirname)
       (let ((default-directory (file-name-directory dirname)))
	 (call-process shell-file-name nil buffer nil
		       "-c" (concat "ls " dired-listing-switches " "
				    (file-name-nondirectory dirname)))))
     (goto-char (dot-min))
     (while (not (eobp))
       (insert "  ")
       (forward-line 1))
     (goto-char (dot-min)))))

(defun dired-find-buffer (dirname)
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (eq major-mode 'dired-mode)
		 (equal dired-directory dirname))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    (or found
	(progn (if (string-match "/$" dirname)
		   (setq dirname (substring dirname 0 -1)))
	       (create-file-buffer (file-name-nondirectory dirname))))))

(defun dired (dirname)
  "\"Edit\" directory DIRNAME.  Delete some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can mark files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  (interactive (list (read-file-name "Dired (directory): "
				     nil default-directory nil)))
  (switch-to-buffer (dired-noselect dirname)))

(defun dired-other-window (dirname)
  "\"Edit\" directory DIRNAME.  Like M-x dired but selects in another window."
  (interactive (list (read-file-name "Dired in other window (directory): "
				     nil default-directory nil)))
  (pop-to-buffer (dired-noselect dirname)))

(defun dired-noselect (dirname)
  "Like M-x dired but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  (if (string-match "./$" dirname)
      (setq dirname (substring dirname 0 -1)))
  (setq dirname (expand-file-name dirname))
  (and (not (string-match "/$" dirname))
       (file-directory-p dirname)
       (setq dirname (concat dirname "/")))
  (let ((buffer (dired-find-buffer dirname)))
    (save-excursion
     (set-buffer buffer)
       (dired-readin dirname buffer)
       (dired-mode dirname))
    buffer))

(defun dired-revert (&optional arg)
  (let ((odot (dot))
	(ofile (dired-get-filename t t))
	(buffer-read-only nil))
    (erase-buffer)
    (dired-readin dired-directory (current-buffer))
    (or (and ofile (re-search-forward (concat " " (regexp-quote ofile) "$") nil t))
	(goto-char odot))
    (beginning-of-line)))

(defvar dired-mode-map nil "Local keymap for dired-mode buffers.")

(defun dired-mode (dirname)
  "Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.
Instead, type d to mark a file for Deletion.
Type u to Unmark a file (remove its D mark).
  Type Rubout to back up one line and unmark.
Type x to eXecute the deletions requested.
Type f to Find the current line's file
  (or Dired it, if it is a directory).
Type o to find file or dired directory in Other window.
Type # to mark temporary files (names beginning with #) for Deletion.
Type ~ to mark backup files (names ending with ~) for Deletion.
Type r to rename a file.
Type c to copy a file.
Type v to view a file in View mode, returning to Dired when done.
Space can be used to move down and up by lines."
  (kill-all-local-variables)    
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'dired-revert)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (make-local-variable 'dired-directory)
  (setq dired-directory dirname)
  (setq default-directory 
	(if (file-directory-p dirname)
	    dirname (file-name-directory dirname)))
  (setq case-fold-search nil)
  (setq mode-line-format
	(concat "--Directory " dirname
		"      %M   %[(%m)%]----%p--%-"))
  (if (not dired-mode-map)
      (progn
	(setq dired-mode-map (make-keymap))
	(suppress-keymap dired-mode-map)
	(aset dired-mode-map ?r 'dired-rename-file)
	(aset dired-mode-map ?\^D 'dired-mark-file-deleted)
	(aset dired-mode-map ?d 'dired-mark-file-deleted)
	(aset dired-mode-map ?v 'dired-view-file)
	(aset dired-mode-map ?f 'dired-find-file)
	(aset dired-mode-map ?o 'dired-find-file-other-window)
	(aset dired-mode-map ?u 'dired-unmark)
	(aset dired-mode-map ?x 'dired-do-deletions)
	(aset dired-mode-map ?\177 'dired-backup-unmark)
	(aset dired-mode-map ?? 'dired-summary)
	(aset dired-mode-map ?c 'dired-copy-file)
	(aset dired-mode-map ?# 'dired-mark-auto-save-files)
	(aset dired-mode-map ?~ 'dired-mark-backup-files)
	(aset dired-mode-map ?h 'describe-mode)
	(aset dired-mode-map ?  'next-line)))
  (setq buffer-read-only t)
  (use-local-map dired-mode-map))

(defun dired-repeat-over-lines (arg function)
  (beginning-of-line)
  (while (and (> arg 0) (not (eobp)))
    (setq arg (1- arg))
    (funcall function)
    (forward-line 1))
  (while (and (< arg 0) (not (bobp)))
    (setq arg (1+ arg))
    (forward-line -1)
    (funcall function)))

(defun dired-mark-file-deleted (arg)
  "In dired, mark the current line's file for deletion.
With arg, repeat over several lines."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let ((buffer-read-only nil))
	 (if (looking-at "  d")
	     nil
	   (delete-char 1)
	   (insert "D"))))))

(defun dired-summary ()
  (interactive)
  (message
   "d-elete, u-ndelete, x-ecute, f-ind, o-ther window, r-ename, c-opy, v-iew"))

(defun dired-unmark (arg)
  "In dired, mark the current line's file for deletion."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert " ")
	 (forward-char -1)))))

(defun dired-backup-unmark (arg)
  "In dired, move up a line and remove deletion mark there."
  (interactive "p")
  (dired-unmark (- arg)))

(defun dired-find-file ()
  "In dired, visit the file named on this line."
  (interactive)
  (beginning-of-line)
  (if (looking-at "  d")
      (dired (dired-get-filename))
    (find-file (dired-get-filename))))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "  d"))
      (view-file (dired-get-filename))))
	    
(defun dired-find-file-other-window ()
  "In dired, visit this file in another window."
  (interactive)
  (beginning-of-line)
  (if (looking-at "  d")
      (dired-other-window (dired-get-filename))
    (find-file-other-window (dired-get-filename))))

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
A non-nil 1st argument means do not include it.  A non-nil 2nd argument
says return nil if no filename on this line, otherwise an error occurs."
  (let (eol)
    (save-excursion
      (end-of-line)
      (setq eol (dot))
      (beginning-of-line)
      (if (re-search-forward
	   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	   eol
	   no-error-if-not-filep)
	  (progn (skip-chars-forward " ")
		 (skip-chars-forward "^ " eol)
		 (skip-chars-forward " " eol)
		 (let ((beg (dot)))
		   (skip-chars-forward "^ \n")
		   (if localp
		       (buffer-substring beg (dot))
		     ;; >> uses default-directory, could lose on cd, multiple.
		     (concat default-directory (buffer-substring beg (dot))))))))))

(defun dired-move-to-filename ()
  "In dired, move to first char of filename on this line.
Returns position (dot) or nil if no filename on this line."
  (let ((eol (progn (end-of-line) (dot))))
    (beginning-of-line)
    (if (re-search-forward
	 "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	 eol t)
	(progn
	  (skip-chars-forward " ")
	  (skip-chars-forward "^ " eol)
	  (skip-chars-forward " " eol)
	  (dot)))))

(defun dired-mark-auto-save-files ()
  "Mark for deletion files whose names suggest they are auto save files."
  (interactive)
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (dot-min))
     (while (not (eobp))
       (and (not (looking-at "  d"))
	    (not (eolp))
	    (if (fboundp 'auto-save-file-name-p)
		(let ((fn (dired-get-filename t t)))
		  (if fn (auto-save-file-name-p fn)))
	      (if (dired-move-to-filename)
		  (looking-at "#")))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert "D")))
       (forward-line 1)))))

(defun dired-mark-backup-files ()
  "Mark all backup files (names ending with ~) for deletion."
  (interactive)
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (dot-min))
     (while (not (eobp))
       (and (not (looking-at "  d"))
	    (not (eolp))
	    (progn (end-of-line) (forward-char -1) (looking-at "~"))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert "D")))
       (forward-line 1)))))

(defun dired-mark-backup-and-auto-save-files ()
  "Mark all backup and temporary files for deletion.
Backup files have names ending in ~.  Auto save file names usually
start with #."
  (interactive)
  (dired-mark-backup-files)
  (dired-mark-auto-save-files))

(defun dired-rename-file (to-file)
  "Rename this file to TO-FILE."
  (interactive "sRename to: ")
  (rename-file (dired-get-filename) (setq to-file (expand-file-name to-file)))
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-region (dot) (progn (forward-line 1) (dot)))
    (setq to-file (expand-file-name to-file))
    (dired-add-entry (file-name-directory to-file)
		     (file-name-nondirectory to-file))))

(defun dired-copy-file (to-file)
  "Copy this file to TO-FILE."
  (interactive "sCopy to: ")
  (copy-file (dired-get-filename) to-file)
  (setq to-file (expand-file-name to-file))
  (dired-add-entry (file-name-directory to-file)
		   (file-name-nondirectory to-file)))

(defun dired-add-entry (directory filename)
  ;; If tree dired is implemented, this function will have to do
  ;; something smarter with the directory.  Currently, just check
  ;; default directory, if same, add the new entry at dot.  With tree
  ;; dired, should call 'dired-current-directory' or similar.  Note
  ;; that this adds the entry 'out of order' if files sorted by time,
  ;; etc.
  (if (string-equal directory default-directory)
      (let ((buffer-read-only nil))
	(call-process "ls" nil t nil
		      dired-listing-switches (concat directory filename))
	(forward-line -1)
	(insert "  ")
	(dired-move-to-filename)
	(let* ((beg (dot))
	       (end (progn (end-of-line) (dot))))
	  (setq filename (buffer-substring beg end))
	  (delete-region beg end)
	  (insert (file-name-nondirectory filename)))
	(beginning-of-line))))

(defun dired-do-deletions ()
  "In dired, delete the files marked for deletion."
  (interactive)
  (let (delete-list answer)
    (save-excursion
     (goto-char 1)
     (while (re-search-forward "^D" nil t)
       (setq delete-list
	     (cons (cons (dired-get-filename t) (1- (dot)))
		   delete-list))))
    (if (null delete-list)
	(message "(No deletions requested)")
      (save-window-excursion
       (switch-to-buffer " *Deletions*")
       (erase-buffer)
       (setq fill-column 70)
       (let ((l (reverse delete-list)))
	 ;; Files should be in forward order for this loop.
	 (while l
	   (if (> (current-column) 59)
	       (insert ?\n)
	     (or (bobp)
		 (indent-to (* (/ (+ (current-column) 19) 20) 20) 1)))
	   (insert (car (car l)))
	   (setq l (cdr l))))
       (goto-char (dot-min))
       (setq answer (yes-or-no-p "Delete these files? ")))
      (if answer
	  (let ((l delete-list))
	    ;; Files better be in reverse order for this loop!
	    ;; That way as changes are made in the buffer
	    ;; they do not shift the lines still to be changed.
	    (while l
	      (goto-char (cdr (car l)))
	      (let ((buffer-read-only nil))
		(condition-case ()
		    (progn (delete-file (concat default-directory
						(car (car l))))
			   (delete-region (dot)
					  (progn (forward-line 1) (dot))))
		  (error (delete-char 1) (insert " "))))
	      (setq l (cdr l))))))))


  
