;; Handling of disabled commands ("novice mode") for Emacs.
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


;; This function is called (autoloaded)
;; to handle any disabled command.
;; The command is found in this-command
;; and the keys are returned by (this-command-keys).

(defun disabled-command-hook (&rest ignore)
  (let (char)
    (save-window-excursion
     (with-output-to-temp-buffer "*Help*"
       (princ "You have typed ")
       (princ (key-description (this-command-keys)))
       (princ ", invoking disabled command ")
       (princ this-command)
       (princ ":\n")
       (princ (or (documentation this-command) "<< not documented >>"))
       (princ "\n\n")
       (princ "You can now type
Space to try the command just this once,
      but leave it disabled,
Y to try it and enable it (no questions if you use it again),
N to do nothing (command remains disabled)."))
     (message "Type y, n or Space: ")
     (while (not (memq (setq char (downcase (read-char)))
		       '(?  ?y ?n)))
       (ding)
       (message "Please type y, n or Space: ")))
    (if (= char ?y)
	(if (y-or-n-p "Enable command for future editing sessions also? ")
	    (enable-command this-command)
	  (put this-command 'disabled nil)))
    (if (/= char ?n)
	(call-interactively this-command))))

(defun enable-command (command)
  "Allow COMMAND to be executed without special confirmation from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
  (interactive "CEnable command: ")
  (put command 'disabled nil)
  (save-excursion
   (set-buffer (find-file-noselect (substitute-in-file-name "~/.emacs")))
   (goto-char (dot-min))
   (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
       (delete-region
	(progn (beginning-of-line) (dot))
	(progn (forward-line 1) (dot)))
     ;; Must have been disabled by default.
     (goto-char (dot-max))
     (insert "\n(put '" (symbol-name command) " 'disabled nil)\n"))
   (setq foo (buffer-modified-p))
   (save-buffer)))

(defun disable-command (command)
  "Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
  (interactive "CDisable command: ")
  (put command 'disabled t)
  (save-excursion
   (set-buffer (find-file-noselect (substitute-in-file-name "~/.emacs")))
   (goto-char (dot-min))
   (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
       (delete-region
	(progn (beginning-of-line) (dot))
	(progn (forward-line 1) (dot))))
   (goto-char (dot-max))
   (insert "(put '" (symbol-name command) " 'disabled t)\n")
   (save-buffer)))

