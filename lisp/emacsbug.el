;; Copyright (C) 1985 Richard M. Stallman and K. Shane Hartman.

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


;; >> This should be an address which is accessible to your machine,
;; >> otherwise you can't use this file.  It will only work on the
;; >> internet with this address.

(defvar bug-gnu-emacs "bug-gnu-emacs@mit-prep.arpa"
  "Address of site maintaining mailing list for Gnu emacs bugs.")

(defun report-emacs-bug (topic)
  "Report a bug in Gnu emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (mail nil bug-gnu-emacs topic)
  (goto-char (dot-max))
  (newline)
  (insert "In " (emacs-version))
  (newline 2)
  (message (substitute-command-keys "Type \\[mail-send] to send bug report.")))

