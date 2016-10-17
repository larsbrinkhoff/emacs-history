;;;
;;; Copyright (C) 1990 Mark Leisher.
;;;
;;; Author: Mark Leisher (mleisher@nmsu.edu)
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
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to mleisher@nmsu.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;;
;;; The latin1 code depends on some for of 8 bit patches to GNU Emacs that
;;; use the ctl-arrow variable.  This should be relatively easy to change.
;;;
;;; Support for easier input of the characters whose values are above 160.
;;;
;;;
;;; Loading latin1-mode.el
;;;   The simplest way to load the latin1-mode is:
;;;   (load "latin1-mode")
;;;
;;;   Make sure gemacs knows where to find the latin1-mode.el file.
;;; 
;;;   DO NOT FORGET to change the user varibles to tell the mode where to load
;;;   the data files.
;;;

(provide 'latin1-mode)

(defvar latin1-name-file "latin1-names.el"                    ;; User option
  "*Name of file containing the cons pairs of ISO-Latin1 character names and
their ascii codes")

(defvar latin1-sequence-file "latin1-seq.el"                  ;; User option
  "*Name of file containing the cons pairs of ISO-Latin1 character access
sequences and their ascii codes")

(defvar latin1-load-path "/usr/share/src/local/gnu/emacs/lisp"   ;; User option
  "*Location of the ISO-Latin1 name and sequence files.")

(defvar latin1-keymap nil
  "Keymap for ISO-Latin1 character selection sequences.")

(defconst latin1-prefix "\C-|"
  "Prefix for accessing ISO-Latin1 character values 160-255.")

(defvar original-key nil
  "Stores the original definition of the latin1 prefix in the 
(current-local-map).")

;;Add minor mode to minor-mode-alist
(or (assq 'latin1-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(latin1-mode " ISOL1")
                                 minor-mode-alist)))

;;build ISO-Latin1 keymap, load key files and set display variable
(if (null latin1-keymap)
    (progn
      (setq ctl-arrow 'latin1)
      (load (concat latin1-load-path "/" latin1-sequence-file) nil t t)
      (load (concat latin1-load-path "/" latin1-name-file) nil t t)
      (setq latin1-keymap (make-keymap))
      (mapcar '(lambda (x)
                 (define-key latin1-keymap (car x) 'insert-latin1-seq-code))
              latin1-sequence-list)
      (define-key latin1-keymap latin1-prefix 'get-latin1-char-by-name)
      (define-key minibuffer-local-map latin1-prefix latin1-keymap)
      (define-key minibuffer-local-ns-map latin1-prefix latin1-keymap)))

;; checks and modifies the syntax table to look at super-ascii characters
;; as word constituents as opposed to whitespace characters
(defun check-syntax-table ()
  (interactive)
  (let ((count 160))
    (if (= (char-syntax count) 32)
        (progn
          (while (<= count 255)
            (modify-syntax-entry count "w")
            (setq count (1+ count)))))))

(defun get-latin1-char-by-name ()
  "Gets ISO-Latin1 character by its full ISO name (see latin1-names.el)."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((s (completing-read "ISO-Latin1 character name: " latin1-name-list)))
    (if (not (assoc s latin1-name-list))
        (error "Invalid ISO-Latin1 character name: \"%s\"." s)
      (insert-char (cdr (assoc s latin1-name-list)) 1))))

(defun insert-latin1-seq-code (arg)
  "Inserts character code associated with a sequence typed after the
latin1-prefix (see latin1-seq.el)."
  (interactive "*p")
  (insert-char (cdr (assoc (substring (this-command-keys) 1) 
                           latin1-sequence-list)) arg))

(defun latin1-mode ()
  "ISO-Latin1 mode is accessed by <Ctrl>-| followed by another <Ctrl>-|
to enter the character by its ISO name, or by a character sequence.
The character sequences are described in the latin1-mode.doc file."
  (interactive)
  (if (not (boundp 'latin1-mode))
      (progn
        (make-variable-buffer-local 'latin1-mode)
        (set-default 'latin1-mode nil)))
  (setq latin1-mode (not latin1-mode))
  (cond (latin1-mode
         (check-syntax-table)
         (setq ctl-arrow 'latin1)
         (setq original-key (lookup-key (current-local-map) latin1-prefix))
         (define-key (current-local-map) latin1-prefix latin1-keymap)
         (message "ISO-Latin1 input mode is on."))
        (t
         (setq ctl-arrow t)
         (define-key (current-local-map) latin1-prefix original-key)
         (message "ISO-Latin1 input mode off."))))
