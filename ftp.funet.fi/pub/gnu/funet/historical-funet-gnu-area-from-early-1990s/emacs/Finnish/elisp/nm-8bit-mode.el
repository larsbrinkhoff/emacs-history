;;; Copyright (C) 1991, 1992 Niko Makila.
;;;
;;; Author: Niko Makila (makila@csc.fi)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; $Id: nm-8bit-mode.el,v 1.3 1992/07/30 10:11:16 makila Exp $

;; Add minor mode to minor mode alist.
(defvar nm-8bit-mode nil "A flag for the eight bit mode.")
(or (assq 'nm-8bit-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(nm-8bit-mode " 8bit") minor-mode-alist)))

;; A few naming conventions.
(fset 'nm-8bit-aumlaut "344")
(fset 'nm-8bit-Aumlaut "304")
(fset 'nm-8bit-oumlaut "366")
(fset 'nm-8bit-Oumlaut "326")
(fset 'nm-8bit-aring "345")
(fset 'nm-8bit-Aring "305")
(fset 'nm-8bit-leftcurly "{")
(fset 'nm-8bit-rightcurly "}")
(fset 'nm-8bit-leftbracket "[")
(fset 'nm-8bit-rightbracket "]")
(fset 'nm-8bit-bar "|")
(fset 'nm-8bit-backslash "\\")

;; Keymap should be buffer-local.
(defvar nm-8bit-mode-map nil "")
(make-variable-buffer-local 'nm-8bit-mode)

;; The mode itself.
(defun nm-8bit-mode (arg)
  "Toggle 8bit mode.
With arg, turn 8bit mode on iff arg is positive.
8bit mode is used to read and write eight bit characters.  You must have a
terminal capable of doing that, of course (X, vt200 etc.)"
  (interactive "P")
  (prog1 (setq nm-8bit-mode
	      (if (null arg) (not nm-8bit-mode)
		(> (prefix-numeric-value arg) 0))))
  (cond (nm-8bit-mode
	 (setq nm-8bit-std-ctl-arrow ctl-arrow
	       ctl-arrow '8bit
	       nm-8bit-std-map (copy-keymap (current-local-map))
	       nm-8bit-mode-map (copy-keymap (current-local-map)))
	 (define-key nm-8bit-mode-map "}" 'nm-8bit-aring)
	 (define-key nm-8bit-mode-map "]" 'nm-8bit-Aring)
	 (define-key nm-8bit-mode-map "{" 'nm-8bit-aumlaut)
	 (define-key nm-8bit-mode-map "[" 'nm-8bit-Aumlaut)
	 (define-key nm-8bit-mode-map "|" 'nm-8bit-oumlaut)
	 (define-key nm-8bit-mode-map "\\" 'nm-8bit-Oumlaut)
	 (define-key nm-8bit-mode-map "\C-c7" 'nm-8bit-leftcurly)
	 (define-key nm-8bit-mode-map "\C-c8" 'nm-8bit-leftbracket)
	 (define-key nm-8bit-mode-map "\C-c9" 'nm-8bit-rightbracket)
	 (define-key nm-8bit-mode-map "\C-c0" 'nm-8bit-rightcurly)
	 (define-key nm-8bit-mode-map "\C-c+" 'nm-8bit-backslash)
	 (define-key nm-8bit-mode-map "\C-c<" 'nm-8bit-bar)
	 (use-local-map nm-8bit-mode-map))
	(t
	 (setq ctl-arrow nm-8bit-std-ctl-arrow)
	 (use-local-map nm-8bit-std-map))))
  
