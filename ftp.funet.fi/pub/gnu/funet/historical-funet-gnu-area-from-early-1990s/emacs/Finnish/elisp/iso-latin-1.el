;;; iso-latin-1.el
;;; An electric mode to type in 8 bits ISO latin 1 characters
;;; Copyright (C) 1990 Matthieu Herrb

;;; modified by Rick Sladkey <jrs@world.std.com> to handle a few
;;; more characters

;; This file is not part of GNU emacs but distribued under the same conditions.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; WARNING 
;;; 
;;; You will need the \"8 bits\" patch to emacs (from K. Cline)
;;; and a screen font with 8 bit iso latin 1 charcater set
;;; (Sunview under SunOS 4.1, XView, vt220, kermit 3.01,...) to see
;;; the characters. Otherwise, you will see the \octal-digits
;;; representation. 

(provide 'iso-latin-1)

;;; this structure defines all translations that are made.
(defconst iso-chars-alist
  '(("\"" . ((?a . ?\344)(?e . ?\353)(?i . ?\357)(?o . ?\366)(?u . ?\374)
	     (?y . ?\377)
	     (?A . ?\304)(?E . ?\313)(?I . ?\317)(?O . ?\326)(?U . ?\334)
	     (?Y . ?\337)))
    ("^" . ((?a . ?\342)(?e . ?\352)(?i . ?\356)(?o . ?\364)(?u . ?\373)
	    (?A . ?\302)(?E . ?\312)(?I . ?\316)(?O . ?\324)(?U . ?\333)))
    ("`" . ((?a . ?\340)(?e . ?\350)(?i . ?\354)(?o . ?\362)(?u . ?\371)
	    (?A . ?\300)(?E . ?\310)(?I . ?\314)(?O . ?\322)(?U . ?\331)))
    ("'" . ((?a . ?\341)(?e . ?\351)(?i . ?\355)(?o . ?\363)(?u . ?\372)
	    (?y . ?\375)
	    (?A . ?\301)(?E . ?\311)(?I . ?\315)(?O . ?\323)(?U . ?\332)
	    (?Y . ?\335)))
    ("," . ((?c . ?\347)(?C . ?\307)))
    ("~" . ((?n . ?\361)(?N . ?\321)
	    (?a . ?\343)(?A . ?\303)
	    (?o . ?\365)(?O . ?\325)
	    (?! . ?\241)(?? . ?\277)))
    ("<" . ((?< . ?\253)))
    (">" . ((?> . ?\273)))
    ("@" . ((?a . ?\345)(?A . ?\305)))
    ("e" . ((?a . ?\346)))
    ("E" . ((?A . ?\306)))
    
    ))

;;; structure to hold old definitions of characters
(defvar iso-defs-alist nil)


;;; two utility functions
(defun cadr (l) (car (cdr l)))

(defun iso-replace-char (char count)
  (delete-backward-char 1)
  (insert-char char count))

;;; this function is bound to every modifier key defined in iso-chars-alist
(defun electric-latin-1-modifier (arg)
  "*transform preceeding character to ISO LATIN 1 if possible"
  (interactive "p")
    (let* ((typed-char (substring (this-command-keys) -1 nil))
	   (char-alist (assoc typed-char iso-chars-alist))
	   (old-def  (cadr (assoc typed-char iso-defs-alist))))
      (if char-alist
	  (let ((match (assoc (preceding-char) (cdr char-alist))))
	    (if match
		(iso-replace-char (cdr match) arg)
	      (call-interactively old-def nil)))
	(error "char %s not in iso-char-alist" (this-command-keys)))))


;;; initialize all bindings
(defun set-iso-defs ()
  ;; turn on 8 bit display
  (setq ctl-arrow 1)
  (if (eq (key-binding "'") 'electric-latin-1-modifier)
      (message "iso-latin-1 mode already active")
    ;; define new local keymap if necessary
    (setq iso-latin-1-keymap (current-local-map))
    (if (not iso-latin-1-keymap)
	(setq iso-latin-1-keymap (make-sparse-keymap)))
    ;; save old defs and define new ones
    (let ((l iso-chars-alist)(c nil))
      (while l
	(setq c (car (car l)))
	(setq iso-defs-alist (cons (list c (key-binding c)) iso-defs-alist))
	(define-key iso-latin-1-keymap c 'electric-latin-1-modifier)
	(setq l (cdr l))
	))
    ;; use new local keymap
    (use-local-map iso-latin-1-keymap)
    ;; sign on 
    (message "Iso-latin-1 turned on")))

;;; reset old bindings
(defun unset-iso-defs ()
  (if (not iso-defs-alist)
      (message "not in iso-latin-1 mode")
    ;; restore old settings
    (setq ctl-arrow nil)
    (while iso-defs-alist
      (let ((c (car iso-defs-alist)))
	(define-key (current-local-map) (car c) (cadr c)))
      (setq iso-defs-alist (cdr iso-defs-alist))
      )
    (message "Iso-latin-1 turned off")))

;;; set or switch mode
(defun iso-latin-1-mode (arg)
  "Minor mode to input ISO 8859 Latin-1 8-bit chars.
Toggle iso-latin-1-mode or turn it on if optional ARG is positive.

Iso-latin-1 mode redefines several characters, to be \"electric\". If they
form with the preceeding character a 8 bit combination defined in
iso-chars-alist, then they are replaced by the
corresponding 8 bit code. 

for example:
e' is used to type in a e-acute
c, is used to type in a \"c cedilla\".

You will need the \"8 bit\" patch to emacs and a screen font with 8
bit iso latin 1 charcater set (Sunview under SunOS 4.1, XView, vt220,
kermit 3.01,...) to see the characters."

  (interactive "P")
  (if (null arg)
      ;; toggle mode
      (if (setq iso-latin-1-mode (not iso-latin-1-mode))
	  (set-iso-defs)
	(unset-iso-defs))
    
    ;; set mode according to arg
    (if (setq iso-latin-1-mode (> (prefix-numeric-value arg) 0))
	(set-iso-defs)
      (unset-iso-defs)))
  (set-buffer-modified-p (buffer-modified-p)))

;;; Create mode control variable
(if (boundp 'iso-latin-1-mode)
    nil
  (setq minor-mode-alist (cons '(iso-latin-1-mode " latin-1")
			       minor-mode-alist))
  (make-variable-buffer-local 'iso-latin-1-mode)
  (make-variable-buffer-local 'iso-latin-1-keymap)
  (set-default 'iso-latin-1-mode nil))
