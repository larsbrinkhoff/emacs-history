;; Mail sending commands for Emacs.
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


;(defconst mail-self-blind nil
;  "Non-nil means insert BCC to self in messages to be sent.
;This is done when the message is initialized,
;so you can remove or alter the BCC field to override the default.")

;(defconst mail-interactive nil
;  "Non-nil means when sending a message wait for and display errors.
;nil means let mailer mail back a message to report errors.")

;(defconst mail-yank-ignored-headers
;   "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:"
;   "Delete these headers from old message when it's inserted in a reply.")
;>> here for emacs 16.9
(defvar send-mail-function 'sendmail-send-it
  "Function to call to send the current buffer as mail.
The headers are be delimited by \"--text follows this line--\"")

(defvar mail-abbrevs-loaded nil "")
(defvar mail-mode-map nil "")

(defun mail (&optional noerase to subject in-reply-to cc replybuffer)
  "Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer seleted; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields.  Type C-c? for a list of them.

If mail-self-blind is non-nil, a bcc to yourself is inserted
when the message is initialized.

If mail-setup-hook is bound, its value is called with no arguments
after the message is initialized.  It can add more default fields.

When calling from a program, the second through fifth arguments
 TO, SUBJECT, CC and IN-REPLY-TO specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c y."
  (interactive "P")
  (switch-to-buffer "*mail*")
  (setq default-directory (expand-file-name "~/"))
  (auto-save-mode auto-save-default)
  (mail-mode)
  (and (not noerase)
       (or (not (buffer-modified-p))
	   (y-or-n-p "Unsent message being composed; erase it? "))
       (progn (erase-buffer)
	      (mail-setup to subject in-reply-to cc replybuffer)
	      t)))

(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer)
  "Like \"mail\" command, but display mail buffer in another window."
  (interactive "P")
  (pop-to-buffer "*mail*")
  (setq default-directory (expand-file-name "~/"))
  (auto-save-mode auto-save-default)
  (mail-mode)
  (and (not noerase)
       (or (not (buffer-modified-p))
	   (y-or-n-p "Unsent message being composed; erase it? "))
       (progn (erase-buffer)
	      (mail-setup to subject in-reply-to cc replybuffer)
	      t)))

(defun mail-setup (to subject in-reply-to cc replybuffer)
  (setq mail-reply-buffer replybuffer)
  (goto-char (dot-min))
  (insert "To: ")
  (save-excursion
    (if to
	(save-restriction
	  (narrow-to-region (dot) (dot))
	  (insert to)
	  (goto-char (dot-min))
	  (if (re-search-forward "<\\([^>\n]*\\)>" nil t)
	      (let ((string (buffer-substring (match-beginning 1)
					      (match-end 1))))
		(delete-region (dot-min) (dot-max))
		(insert string)))))
    (goto-char (dot-max))
    (newline)
    (if cc
	(insert "Cc: " cc "\n"))
    (if in-reply-to
	(insert "In-reply-to: " in-reply-to "\n"))
    (insert "Subject: " (or subject "") "\n")
    (if mail-self-blind
	(insert "Bcc: " (user-login-name) "\n"))
    (insert "--text follows this line--\n"))
  (if to (goto-char (dot-max)))
  (or to subject in-reply-to
      (set-buffer-modified-p nil))
  (and (boundp 'mail-setup-hook)
       mail-setup-hook
       (funcall mail-setup-hook)))

(defun mail-mode ()
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
C-c C-s mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c t  mail-to  (move to To: field)	C-c s  mail-subject (move to Subj:)
C-c b  mail-bcc (move to Bcc: field)    C-c c  mail-cc  (move to Cc: field)
C-c w  mail-signature (insert ~/.signature at end).
C-c y  mail-yank-original (insert current message, in rmail)."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table text-mode-syntax-table)
  (or mail-mode-map
      (mail-set-local-keys))
  (use-local-map mail-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'mail-mode)
  (setq mode-name "Mail")
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^--text follows this line--$\\|^[ \t]*[-_][-_][-_]+$\\|"
				paragraph-start))
  (setq paragraph-separate (concat "^--text follows this line--$\\|^[ \t]*[-_][-_][-_]+$\\|"
				   paragraph-separate))
  (and (boundp 'mail-mode-hook)
       mail-mode-hook
       (funcall mail-mode-hook)))

(defun mail-set-local-keys ()
  (setq mail-mode-map (make-keymap))
  (use-local-map mail-mode-map)
  (define-key mail-mode-map "\^c?" 'describe-mode)
  (define-key mail-mode-map "\^ct" 'mail-to)
  (define-key mail-mode-map "\^cb" 'mail-bcc)
  (define-key mail-mode-map "\^cc" 'mail-cc)
  (define-key mail-mode-map "\^cw" 'mail-signature)		; who
  (define-key mail-mode-map "\^cs" 'mail-subject)
  (define-key mail-mode-map "\^Cy" 'mail-yank-original)
  (define-key mail-mode-map "\^c\^c" 'mail-send-and-exit)
  (define-key mail-mode-map "\^c\^s" 'mail-send))

(defun mail-send-and-exit ()
  "Send message like mail-send, then, if no errors, exit from mail buffer."
  (interactive)
  (mail-send)
  (if (fboundp 'bury-buffer)
      (bury-buffer (current-buffer)))
  (if (eq (next-window (selected-window)) (selected-window))
      (switch-to-buffer (other-buffer (current-buffer)))
    (delete-window)))

(defun mail-send ()
  "Send the message in the current buffer.
If  mail-interactive  is non-nil, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  (interactive)
  (funcall send-mail-function))

(defun sendmail-send-it ()
  (let ((errbuf (if mail-interactive
		    (get-buffer-create "*Sendmail Errors*")
		  0))
	delimline subjline subjstr
	(case-fold-search nil))
    (message "Sending...")
    (save-excursion
      ;; Flush excess whitespace at end
      (goto-char (dot-max))
;>> some of us don't want our messages fucked with
;     (skip-chars-backward " \n\t")
;     (delete-region (dot) (dot-max))
      ;; require one newline.
      (or (= (char-after (1- (dot-max))) ?\n)
	  (insert "\n"))
      ;; Change header-delimiter to be what sendmail expects.
      (goto-char (dot-min))
      (search-forward "\n--text follows this line--\n")
      (replace-match "\n\n")
      (backward-char 1)
      (setq delimline (dot-marker))
      (goto-char (dot-min))
      (while (and (re-search-forward "\n\n\n*" delimline t)
		  (< (dot) delimline))
	(replace-match "\n"))
      ;; don't send out a blank subject line
      (goto-char (dot-min))
      (setq case-fold-search t)
      (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
	  (replace-match ""))
      (unwind-protect
	   (apply 'call-process-region
		  (append (list (dot-min) (dot-max) "/usr/lib/sendmail"
				nil errbuf nil
				"-oi" "-t")
			  ;; Don't say "from root" if running under su.
			  (and (equal (user-real-login-name) "root")
			       (list "-f" (user-login-name)))
			  ;; These mean "report errors by mail"
			  ;; and "deliver in background".
			  (if (null mail-interactive) '("-oem" "-odb"))))
	;; Change header-delimiter back
	(goto-char delimline)
	(set-marker delimline nil nil)
	(insert "--text follows this line--")))
    (if (or (not mail-interactive)
	    (save-excursion
	      (set-buffer errbuf)
	      (= (buffer-size) 0)))
	(progn (message "Sending...done")
	       (set-buffer-modified-p nil))
      (let (msg)
	(save-excursion
	  (set-buffer errbuf)
	  (goto-char (dot-min))
	  (while (re-search-forward "\n\n* *" nil t)
	    (replace-match "; "))
	  (setq msg (buffer-substring (dot-min) (dot-max))))
	(goto-char (dot-min))
	(mail-position-on-field "to")
	(display-buffer errbuf)
	(error (format "Sending...failed to %s" msg))))))

(defun mail-to ()
  "Move dot to end of To-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "to"))

(defun mail-subject ()
  "Move dot to end of Subject-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "subject"))

(defun mail-cc ()
  "Move dot to end of CC-field.  Create a CC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "cc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nCc: "))))

(defun mail-bcc ()
  "Move dot to end of BCC-field.  Create a BCC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "bcc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nBcc: "))))

(defun mail-position-on-field (field &optional soft beg)
  (let (end (case-fold-search t))
    (goto-char (dot-min))
    (setq end (if (search-forward "\n\n" nil t) (dot) (dot-max)))
    (goto-char (dot-min))
    (if (re-search-forward (concat "^" (regexp-quote field) ":") end t)
	(progn (or beg (mail-end-of-field))
	       t)
      (and (not soft)
	   (progn (goto-char end)
		  (skip-chars-backward "\n")
		  (insert "\n" field ": "))))))

(defun mail-end-of-field ()
  (while (progn (end-of-line)
		(looking-at "\n[ \t]"))
    (forward-char 1)))

(defun mail-signature ()
  "Sign letter with contents of ~/.signature file."
  (interactive)
  (save-excursion
    (goto-char (dot-max))
    (insert-file-contents (expand-file-name "~/.signature"))))

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts dot before the text and mark after.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (dot)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (mail-yank-clear-headers start (mark))
	  (indent-rigidly start (mark)
			  (if arg (prefix-numeric-value arg) 3)))
	(exchange-dot-and-mark)
	(if (not (eolp)) (insert ?\n)))))

(defun mail-yank-clear-headers (start end)
  (save-excursion
    (goto-char start)
    (if (search-forward "\n\n" end t)
	(save-restriction
	  (narrow-to-region start (dot))
	  (goto-char start)
	  (while (let ((case-fold-search t))
		   (re-search-forward mail-yank-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (dot)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (dot))))))))
