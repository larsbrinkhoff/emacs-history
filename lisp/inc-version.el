;; Load this file to increment the recorded Emacs version number.
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


(insert-file-contents "../lisp/version.el")

(re-search-forward "emacs-version \"[0-9.]*")
(save-restriction
 (narrow-to-region (dot)
		   (progn (skip-chars-backward "0-9") (dot)))
 (goto-char (dot-min))
 (let ((version (read (current-buffer))))
   (delete-region (dot-min) (dot-max))
   (prin1 (1+ version) (current-buffer))))

(write-region (dot-min) (dot-max) "../lisp/version.el" nil 'nomsg)
(erase-buffer)
(set-buffer-modified-p nil)

(kill-emacs)
