;; Record version number of Emacs.
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


;; The following line is modified automatically
;; by loading inc-version.el, each time a new Emacs is dumped.
(defconst emacs-version "16.56.0"
  "Version numbers of this version of Emacs.")

(defconst emacs-build-time (current-time-string)
  "Time at which Emacs was dumped out.")

(defun emacs-version ()
  "\
Return string describing the version of Emacs that is running."
  (interactive)
  (if (interactive-p)
      (message "%s" (emacs-version))
    (format "GNU Emacs %s of %s %s"
	    emacs-version
	    (substring emacs-build-time 0
		       (string-match " *[0-9]*:" emacs-build-time))
	    (substring emacs-build-time (string-match "[0-9]*$" emacs-build-time)))))
