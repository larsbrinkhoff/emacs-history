;;; version.el --- record version number of Emacs.

;;; Copyright (C) 1985, 1992 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

;; The following line is modified automatically
;; by loading inc-version.el, each time a new Emacs is dumped.
(defconst emacs-version "19.22.0" "\
Version numbers of this version of Emacs.")

(defconst emacs-build-time (current-time-string) "\
Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defun emacs-version  (&optional here) "\
Return string describing the version of Emacs that is running.
If optional argument HERE is non-nil, insert string at point."
  (interactive "P")
  (let ((version-string 
         (format "GNU Emacs %s of %s %s on %s (%s)"
                 emacs-version
                 (substring emacs-build-time 0
                            (string-match " *[0-9]*:" emacs-build-time))
                 (substring emacs-build-time 
                            (string-match "[0-9]*$" emacs-build-time))
                 emacs-build-system system-type)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" (emacs-version))
        version-string))))

;;; We hope that this alias is easier for people to find.
(fset 'version 'emacs-version)

;;Local variables:
;;version-control: never
;;End:

;;; version.el ends here
