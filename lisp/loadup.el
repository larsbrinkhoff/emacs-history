;Load up standardly loaded Lisp files for Emacs.
;; This is loaded into a bare Emacs to make a dumpable one.
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


(load "simple")
(garbage-collect)
(load "files")
(garbage-collect)
(load "indent")
(load "window")
(garbage-collect)
(load "loaddefs.el")
(garbage-collect)
(load "startup")
(load "lisp")
(garbage-collect)
(load "page")
(load "register")
(garbage-collect)
(load "paragraphs")
(load "lisp-mode")
(garbage-collect)
(load "text-mode")
(load "fill")
(garbage-collect)
(load "c-mode")
(garbage-collect)
(load "isearch")
(garbage-collect)
(load "replace")
(garbage-collect)
(load "abbrev")
(garbage-collect)
(load "buff-menu")

(load "version.el")

(message "Finding pointers to doc strings...")
(copy-file (expand-file-name "../etc/DOCSTR")
	   (concat (expand-file-name "../etc/DOCSTR.") emacs-version)
	   t)
(Snarf-documentation (concat "DOCSTR." emacs-version))
(message "Finding pointers to doc strings...done")

(lisp-interaction-mode)
(load "site-init" t)
(garbage-collect)

(if (or (equal (nth 3 command-line-args) "dump")
	(equal (nth 4 command-line-args) "dump"))
    (progn
      (message "Dumping under names xemacs and emacs-%s" emacs-version)
      (condition-case ()
	  (delete-file "xemacs")
	(file-error nil))
      (dump-emacs "xemacs" "temacs")
      (add-name-to-file "xemacs" (concat "emacs-" emacs-version) t)
      (kill-emacs)))
