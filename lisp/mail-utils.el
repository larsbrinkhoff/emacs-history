;; Utility functions used both by rmail and rnews

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

(provide 'mail-utils)
		     
(defun mail-string-delete (string start end)
  "Returns a string containing all of STRING except the part
from START to END (inclusive)."
  (if (null end) (substring string 0 start)
    (concat (substring string 0 start)
	    (substring string (1+ end) nil))))

(defun mail-strip-quoted-names (address)
  "Delete comments and quoted strings in an address list ADDRESS.
Also replaces FOO <BAR> with just BAR.
Return a modified address list."
  (let (pos)
    (while (setq pos (string-match "\".*\"" address))
      (setq address
	    (mail-string-delete address
				pos
				(string-match "\"" address (1+ pos)))))
    (while (setq pos (string-match "(.*)" address))
      (setq address
	    (mail-string-delete address
				pos
				(string-match ")" address (1+ pos)))))
    (while (setq pos (string-match "\\(,\\|^\\)[^,\n]*<" address))
      (setq address
	    (mail-string-delete address
				pos
				(string-match "<" address (1+ pos))))
      (setq pos (string-match ">" address pos))
      (setq address
	    (mail-string-delete address
				pos
				pos)))
    address))
  
; rmail-dont-reply-to-names is defined in loaddefs
(defun rmail-dont-reply-to (userids)
  "Returns string of mail addresses USERIDS sans any recipients
that are elements of  rmail-dont-reply-to-names."
  (let (pos)
    (if (not rmail-dont-reply-to-names)
	(setq rmail-dont-reply-to-names (getenv "USER")))
    (if rmail-dont-reply-to-names
	(progn
	  (while (setq pos
		       (string-match (concat "\\(^\\|,\\) *\\("
					     rmail-dont-reply-to-names
					     "\\)")
				     (downcase userids)))
	    (if (> pos 0)
		(setq pos (1+ pos)))
	    (setq userids
		  (mail-string-delete userids
				      pos
				      (string-match "[ ,]" userids pos))))))
    ;; get rid of any trailing commas
    (if (setq pos (string-match ", *$" userids))
	(setq userids (substring userids 0 pos)))
    ;; remove leading spaces. they bother me.
    (mail-string-delete userids 0 (1- (string-match "[^ ]*" userids)))))

(defun mail-fetch-field (field-name)
  "Return the value of the header field FIELD.
The buffer is expected to be narrowed to just the headers of the message."
  (save-excursion
    (goto-char (dot-min))
    (let ((case-fold-search t))
      (goto-char (dot-min))
      (if (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
	  (let ((odot (dot)))
	    (while (progn (forward-line 1)
			  (looking-at "[ \t]")))
	    (buffer-substring odot (1- (dot))))))))
