;; "RMAIL Summary"  The guts of rmail summary mode.
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

(require 'rmail)

;; These used to be in rmail, but only summary mode uses them.

(defun rmail-make-labels-search-string (labels)
  (if (not (consp labels))
      (concat "," (rmail-quote-label-name labels) ",")
    (let ((search (concat "," (rmail-quote-label-name (car labels)) ",")))
      (while (setq labels (cdr labels))
	(setq search
	      (concat search "\\|," (rmail-quote-label-name (car labels)) ",")))
      search)))

(defun rmail-message-labels-p (labels &optional n what)
  (rmail-maybe-set-message-counters)
  (if (not n) (setq n rmail-current-message))
  (let ((search (if (stringp labels)
		    labels
		  (rmail-make-labels-search-string labels)))
	(case-fold-search t)
	(bound)
	(start))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (rmail-msgbeg n))
	(forward-line 1)
	(if (looking-at "[01],")
	    (progn
	      (setq start (1+ (dot)))
	      (setq bound (progn (end-of-line) (dot)))
	      (search-backward ",," start 'move)
	      (cond ((not what) (setq bound (1+ (dot))))
		    ((eq what t) (setq start (1+ (dot)))))
	      (goto-char start)
	      (re-search-forward search bound t)))))))

;; These are autoloaded from RMAIL.

(defun rmail-summary-by-label (label)
  "Display a summary of messages with LABEL, one line per message."
  (interactive (list (rmail-read-label "Summarize label")))
  (rmail-summary label))

(defun rmail-summary-by-labels (&rest labels)
  "Display a summary of messages with LABELS, one line per message.
Enter labels separately.  Terminate input with empty string."
  (interactive (read-minibuffer "Summarize labels (list): " "("))
  (rmail-summary (mapcar '(lambda (s)
			    (rmail-make-label (symbol-name s) t))
			 labels)))

(defun rmail-summary (&optional type)
  "Display a summary of all messages, one line per message."
  (interactive)
  (let ((current rmail-current-message)
	(last-type rmail-last-summary-type))
    ;; If there is a summary, but the type requested is not the same
    ;; type as the last summary we made, we have to make a new
    ;; summary.  The check for summary type fails for multiple labels,
    ;; but not much we can do other than sort lists befor comparing.
    (if (and rmail-summary-valid (eq type rmail-last-summary-type))
	(pop-to-buffer rmail-summary-buffer)
      (setq rmail-last-summary-type type
	    last-type type
	    rmail-summary-valid nil)
      (rmail-new-summary type))
    ;; If it's a full summary, position at current message, otherwise
    ;; position at first message in the topical summary.
    (goto-char (dot-min))
    (if (not type)
	(progn (forward-line current)
	       (recenter -1))
      (forward-line 1))
    (rmail-summary-goto-msg)))

(defun rmail-new-summary (&optional labels)
  (message "Computing summary lines...")
  (rmail-forget-summary)
  (or rmail-summary-buffer
      (setq rmail-summary-buffer
	    (let* ((name (concat (buffer-name) "-summary"))
		   (sbuf (get-buffer name)))
	      (if (and sbuf
		       (save-excursion (set-buffer sbuf)
				       (eq major-mode 'rmail-summary-mode)))
		  sbuf
		(create-file-buffer name)))))
  (let ((total rmail-total-messages)
	(filter (and labels (rmail-make-labels-search-string labels)))
	(current rmail-current-message)
	(rbuf (current-buffer))
	(sbuf rmail-summary-buffer))
    (unwind-protect
	(save-restriction
	  (save-excursion
	    (let ((bufnam (buffer-name)))
	      (set-buffer sbuf)
	      (setq buffer-read-only t)
	      (let ((buffer-read-only nil))
		(erase-buffer)
		(insert "Summary of " bufnam " messages")
		(if (not labels)
		    nil
		  (insert " with label")
		  (if (cdr-safe labels) (insert "s ") (insert " "))
		  (princ labels (current-buffer)))
		(newline))))
	    (set-buffer rbuf)
	    (widen)
	    (setq rmail-current-message 0)
	    (while (< rmail-current-message rmail-total-messages)
	      (setq rmail-current-message (1+ rmail-current-message))
	      (goto-char (rmail-msgbeg rmail-current-message))
	      (if (or (not filter) (rmail-message-labels-p filter))
		  (progn
		    (forward-line 2)
		    (or (looking-at "Summary-line: ")
			(rmail-make-summary-line rmail-current-message))
		    (search-forward "Summary-line:"
				    (rmail-msgend  rmail-current-message))
		    (let ((blurb (buffer-substring (dot)
						   (progn (end-of-line)
							  (dot))))
			  (current rmail-current-message))
		      (set-buffer sbuf)
		      (let ((buffer-read-only nil))
			(insert (int-to-string current) ". " blurb ?\n))
		      (set-buffer rbuf))))))
      (setq rmail-current-message current)
      (setq rmail-summary-valid t)
      (pop-to-buffer sbuf)
      (rmail-summary-mode)
      (setq rmail-buffer rbuf)
      (message "Computing summary lines...done"))))

(defun rmail-make-summary-line (n)
  (let ((beg (dot))
	(buffer-read-only nil)
	(deleted (rmail-message-deleted-p n)))
    ;; Delete existing summary line record.
    (if (looking-at "Summary-line: ")
	(delete-region (dot) (progn (forward-line 1) (dot))))
    (goto-char (rmail-msgend n))
    (search-backward "\n*** EOOH ***\n" beg t)
    (save-restriction
      (narrow-to-region beg (dot))
      (setq line (funcall rmail-make-summary-line-function)))
    (goto-char beg)
    (insert "Summary-line: "
	    (if deleted "D " "  ")
	    line "\n")
    (goto-char beg)))

(defun rmail-make-stupid-summary-line ()
  (concat (or (mail-fetch-field "date") "")
	  "  "
	  (or (mail-fetch-field "from") "anonymous")
	  "  "
	  (or (mail-fetch-field "subject") "")))

;; *** Rmail Summary Mode ***

(if rmail-summary-mode-map
    nil
  (setq rmail-summary-mode-map (make-keymap))
  (suppress-keymap rmail-summary-mode-map)
  (define-key rmail-summary-mode-map "j" 'rmail-summary-goto-msg)
  (define-key rmail-summary-mode-map "n" 'rmail-summary-next-msg)
  (define-key rmail-summary-mode-map "p" 'rmail-summary-previous-msg)
  (define-key rmail-summary-mode-map "q" 'rmail-summary-quit)
  (define-key rmail-summary-mode-map "d" 'rmail-summary-delete-forward)
  (define-key rmail-summary-mode-map " " 'rmail-summary-scroll-msg-up)
  (define-key rmail-summary-mode-map "\177" 'rmail-summary-scroll-msg-down))

(defun rmail-summary-quit ()
  "Quit RMAIL from RMAIL Summary Mode."
  (interactive)
  (pop-to-buffer rmail-buffer)
  (let ((sbuf rmail-summary-buffer))
    (rmail-quit)
    (delete-windows-on sbuf)
    (bury-buffer sbuf)))

(defun rmail-summary-scroll-msg-up (arg)
  "Scroll the current message up.
With ARG, scroll up that many lines."
  (interactive "P")
  (pop-to-buffer rmail-buffer)
  (condition-case nil
      (scroll-up (and arg (prefix-numeric-value arg)))
    (end-of-buffer nil))
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-scroll-msg-down (arg)
  "Scroll the current message down.
With ARG, scroll down that many lines."
  (interactive "P")
  (pop-to-buffer rmail-buffer)
  (condition-case nil
      (scroll-down (and arg (prefix-numeric-value arg)))
    (end-of-buffer nil))
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-next-msg ()
  "move to next summary line and display corresponding message."
  (interactive)
  (forward-line 1)
  (rmail-summary-goto-msg))

(defun rmail-summary-previous-msg ()
  "Move to previous summary line and display corresponding message."
  (interactive)
  (forward-line -1)
  (rmail-summary-goto-msg))

(defun rmail-summary-delete-forward ()
  "Delete message on current summary line, moving forward."
  (interactive)
  (rmail-summary-goto-msg)
  (pop-to-buffer rmail-buffer)
  (rmail-delete-forward)
  (pop-to-buffer rmail-summary-buffer)
  (let ((buffer-read-only nil))
    (skip-chars-forward "0-9.")
    (delete-char 1)
    (insert "D"))
  (forward-line 1))

(defun rmail-summary-goto-msg ()
  "Display message on current summary line."
  (interactive)
  (let (number (buf rmail-buffer))
    (if (not (looking-at "[0-9]"))
	(error "No message on this line.")
      (save-excursion
	(beginning-of-line)
	(setq number (read (current-buffer))))
      (pop-to-buffer buf)
      (if (not rmail-summary-valid)
	  (error "Current summary is invalid.  Make a new one in this window.")
	(rmail-show-message number)
	(pop-to-buffer rmail-summary-buffer)))))

(defun rmail-summary-mode ()
  "RMAIL Summary Mode.
A subset of the RMAIL mode commands are supported in this mode. 
As commands are issued in the summary buffer the corresponding
mail message is displayed in the RMAIL buffer.

n       Move to next message.
p       Move to previous message.
j       Jump to the message at the cursor location.
d       Delete the message at the cursor location and move to next message.
q	Exit Rmail Summary Mode and Rmail.
Space   Scroll the current message forward.
Delete  Scroll the current message backward."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'rmail-buffer)
  (setq major-mode 'rmail-summary-mode)
  (setq mode-name "RMAIL Summary")
  (use-local-map rmail-summary-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table))
