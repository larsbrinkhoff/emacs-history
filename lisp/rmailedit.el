;; "RMAIL edit mode"  Edit the current message.
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

(require 'rmail)

(defun rmail-edit-mode ()
  "Major mode for editing the contents of an RMAIL message.
The editing commands are the same as in Text mode,
except for \\[rmail-cease-edit] to return to RMAIL, preserving changes
you made, and \\[rmail-abort-edit] to return to RMAIL, aborting changes
you made.  Calls the value of  text-mode-hook  with no arguments
if it is non-nil."
  (use-local-map rmail-edit-map)
  (setq major-mode 'rmail-edit-mode)
  (setq mode-name "RMAIL Edit")
  (setq mode-line-format default-mode-line-format)
  (and (boundp 'text-mode-hook)
       text-mode-hook
       (funcall 'text-mode-hook)))

(defun rmail-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (rmail-edit-mode)
  (make-local-variable 'rmail-old-text)
  (setq rmail-old-text (buffer-substring (dot-min) (dot-max)))
  (setq buffer-read-only nil)
  (set-buffer-modified-p (buffer-modified-p))
  ;; Make mode line update.
  (message "Editing: Type C-c to return to Rmail, C-] to abort"))

(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  (set-buffer-modified-p (buffer-modified-p))
  (rmail-mode-1)
  (setq buffer-read-only t))

(defun rmail-abort-edit ()
  "Abort edit of current message; restore original contents."
  (interactive)
  (delete-region (dot-min) (dot-max))
  (insert rmail-old-text)
  (rmail-cease-edit))

