;;;  mh-e.el	(Version: 3.4k for GNU Emacs Version 18 and MH.5 and MH.6)

;;;  Copyright (C) 1985, 1986, 1987 Free Software Foundation
;;;     Author:  James Larus, larus@berkeley.arpa, ucbvax!larus
;;;	Please send suggestions and corrections to the above address.
;;;
;;;  This file contains mh-e, a GNU Emacs front end to the MH mail system.


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


;;;  Original version for Gosling emacs by Brian Reid, Stanford, 1982.
;;;  Modified by James Larus, BBN, July 1984 and UCB, 1984 & 1985.
;;;  Rewritten for GNU Emacs, James Larus 1985.


;;;  NB MH must have been compiled with the MHE compiler flag or several
;;;  features necessary to this program will be missing.



;;; Constants:

;;; Set for local environment:
;;;* These are now in paths.el.
;;;(defvar mh-progs "/usr/new/mh/"     "Directory containing MH commands")
;;;(defvar mh-lib   "/usr/new/lib/mh/" "Directory of MH library")


;;; Mode hooks:

(defvar mh-folder-mode-hook nil
  "*Invoked in mh-folder-mode on a new folder.")
(defvar mh-letter-mode-hook nil
  "*Invoked in mh-letter-mode on a new letter.")
(defvar mh-compose-letter-hook nil
  "*Invoked in mh-compose-and-send-mail on an outgoing letter.  It is passed
three arguments: TO recipients, SUBJECT, and CC recipients.")


;;; Personal preferences:

(defvar mh-clean-message-header nil
  "*Non-nil means remove invisible header lines or only show visible header
lines in messages.")
(defvar mh-visible-headers nil
  "*If non-nil, it contains a regexp specifying the headers that are shown in
a message.")
(defvar mh-use-mhl nil
  "*Non-nil means use mhl to format messages.")
(defvar mh-lpr-command-format "lpr -p -J '%s'"
  "*Format for Unix command line to print a message. The format should be
a unix command line, with the string \"%s\" where the folder and message
number should appear.")
(defvar mh-summary-height 4
  "*Number of lines in summary window.")
(defvar mh-ins-buf-prefix ">> "
  "*String to put before each non-blank line of the the current message
as it is inserted in an outgoing letter.")


;;; Real constants:

(defvar mh-cmd-note 4		       "Offset to insert notation")
(defvar mh-invisible-headers
  "^Received: \\|^Message-Id: \\|^Remailed-\\|^Via: \\|^Mail-from: \\|^Return-Path: \\|^In-Reply-To: \\|^Resent-"
  "Regexp specifying headers that are not to be shown.")
(defvar mh-rejected-letter-start "^   ----- Unsent message follows -----$"
  "Regexp specifying the beginning of the wrapper around a letter returned
by the mail system.")
(defvar mh-good-msg-regexp  "^....[^D^]"
  "Regexp specifiying the scan lines that are 'good' messages")
(defvar mh-redist-full-contents t
  "Non-nil if MH `dist' command wants the whole letter to redistribute (i.e.,
MH.6).  Nil otherwise.")


;;; Global variables:

(defvar mh-user-path  ""
  "User's mail folder.")
(defvar mh-last-destination nil
  "Destination of last "move" command.")
(defvar mh-folder-mode-map (make-sparse-keymap)
  "Keymap for MH folders.")
(defvar mh-letter-mode-map (make-sparse-keymap)
  "Keymap for composing mail.")
(defvar mh-pick-mode-map (make-sparse-keymap)
  "Keymap for searching folder.")
(defvar mh-folder-list nil
  "List of folder names for completion.")


;;; Macros and generic functions:

(defmacro push (v l)
  (list 'setq l (list 'cons v l)))

(defmacro caar (l)
  (list 'car (list 'car l)))

(defmacro cadr (l)
  (list 'car (list 'cdr l)))

(defmacro cdar (l)
  (list 'cdr (list 'car l)))

(defmacro cddr (l)
  (list 'cdr (list 'cdr l)))

(defmacro when (pred &rest body)
  (list 'cond (cons pred body)))

(defun mapc (func list)
  (while list
    (funcall func (car list))
    (setq list (cdr list))))



;;; Entry points:

(defun mh-rmail (&optional arg)
  "Inc(orporate) new mail (no arg) or scan a MH mail box (arg given).
This front end uses the MH mail system, which uses different conventions
from the usual mail system."
  (interactive "P")
  (mh-find-path)
  (if arg
      (call-interactively 'mh-visit-folder)
      (mh-inc-folder)))


(defun mh-smail ()
  "Send mail using the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))


(defun mh-smail-other-window ()
  "Send mail in other window using the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))



;;; User executable mh-e commands:


(defun mh-answer (prefix-provided msg)
  "Answer a MESSAGE (default: displayed message).
If (optional) prefix argument provided, then include the message in the reply."
  (interactive (list current-prefix-arg (mh-get-msg-num t)))
  (let ((minibuffer-help-form
	 "from => Sender\n  to => Sender and primary recipients\n  cc => Sender and all recipients"))
    (let ((reply-to
	   (completing-read "Reply to whom: " '(("from") ("to") ("cc")) nil t))
	  (msg-filename (mh-msg-filename msg))
	  (folder mh-current-folder)
	  (show-buffer mh-show-buffer))
    (message "Composing a reply...")
    (cond ((or (equal reply-to "from") (equal reply-to ""))
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" "-nodraftfolder" mh-current-folder
			 msg "-nocc" "all")
		   (if prefix-provided (list "-filter" "mhl.reply")))))
	  ((equal reply-to "to")
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" "-nodraftfolder" mh-current-folder
			 msg "-cc" "to")
		   (if prefix-provided (list "-filter" "mhl.reply")))))
	  ((equal reply-to "cc")
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" "-nodraftfolder" mh-current-folder
			 msg "-cc" "all")
		   (if prefix-provided (list "-filter" "mhl.reply"))))))

    (mh-read-draft "reply" (format "%sreply" mh-user-path))
    (delete-file (format "%sreply" mh-user-path))
    (delete-other-windows)
    (set-buffer-modified-p nil)

    (let ((to (mh-get-field "To:"))
	  (subject (mh-get-field "Subject:"))
	  (cc (mh-get-field "Cc:")))
      (goto-char (point-min))
      (mh-goto-header-end 1)
      (if (not prefix-provided)
	(mh-display-msg msg msg-filename show-buffer))
      (mh-add-msg-to-seq msg "answered" t)
      (message "Composing a reply...done")
      (mh-compose-and-send-mail "" folder msg to subject cc "-" "Replied:")))))


(defun mh-burst-digest ()
  "Burst apart the current message, which should be a digest.  Message is
replaced by its table of contents and the letters from the digest are inserted
into the folder after that message."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (if (or mh-delete-list mh-move-list)
	(if (yes-or-no-p "Process outstanding commands (or lose them)? ")
	    (mh-process-commands mh-current-folder)
	    (mh-undo-folder)))
    (message "Bursting digest...")
    (mh-exec-cmd "burst" mh-current-folder digest "-inplace")
    (mh-scan-folder mh-current-folder (format "%d-last" mh-first-msg-num))
    (message "Bursting digest...done")))


(defun mh-copy-msg (prefix-provided msg-or-seq dest)
  "Copy specified MESSAGE(s) (default: displayed message) to another
FOLDER without deleting them.
If (optional) prefix argument supplied, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Copy" mh-narrowed-to-seq)
			 (mh-get-msg-num t))
		     (mh-prompt-for-folder "Copy to" "" t)))
  (mh-exec-cmd "refile" msg-or-seq "-link" "-src" mh-current-folder dest)
  (if prefix-provided
      (mh-notate-seq msg-or-seq ?C mh-cmd-note)
      (mh-notate msg-or-seq ?C mh-cmd-note)))


(defun mh-delete-msg (prefix-provided msg-or-seq)
  "Mark the specified MESSAGE(s) (default: displayed message) for later
deletion.
If (optional) prefix argument supplied, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Delete" mh-narrowed-to-seq)
			 (mh-get-msg-num t))))
  (if prefix-provided
      (mh-map-to-seq-msgs 'mh-delete-a-msg msg-or-seq)
      (mh-delete-a-msg msg-or-seq))
  (mh-next-msg))


(defun mh-delete-msg-from-seq (prefix-provided msg-or-seq &optional from-seq)
  "Delete MESSAGE (default: displayed message) from SEQUENCE.
If (optional) prefix argument supplied, then prompt for sequence to use
instead of message."
  (interactive (let ((argp current-prefix-arg))
		 (list argp
		       (if argp
			   (mh-read-seq "Delete" mh-narrowed-to-seq)
			   (mh-get-msg-num t))
		       (if (not argp)
			   (mh-read-seq "Delete from" mh-narrowed-to-seq)))))
  (if prefix-provided
      (mh-remove-seq msg-or-seq)
      (mh-remove-msg-from-seq msg-or-seq from-seq))
  (mh-next-msg))


(defun mh-execute-commands ()
  "Process outstanding delete and move requests."
  (interactive)
  (if mh-narrowed-to-seq (mh-widen))
  (save-excursion
    (mh-process-commands mh-current-folder))
  (delete-other-windows)
  (setq mh-summarize t)
  (setq mode-name "Mh-Summary")
  (mh-make-folder-mode-line))


(defun mh-extract-rejected-mail (msg)
  "Extract a letter returned by the mail system (default: displayed message)
and make it resendable."
  (interactive (list (mh-get-msg-num t)))
  (let ((from-folder mh-current-folder))
    (mh-read-draft "extraction" (format "%s%d" mh-folder-filename msg))
    (goto-char (point-min))
    (re-search-forward mh-rejected-letter-start)
    (forward-char 1)
    (kill-region (point-min) (point))
    (mh-clean-msg-header (point-min)
			 "^Date:\\|^Received:\\|^Message-Id:\\|^From:"
			 nil)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail "" from-folder msg (mh-get-field "To")
			      (mh-get-field "From") (mh-get-field "cc"))))


(defun mh-forward (prefix-provided msg-or-seq to cc)
  "Forward MESSAGE(s) (default: displayed message).
If (optional) prefix argument supplied, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Forward" mh-narrowed-to-seq)
			 (mh-get-msg-num t))
		     (read-string "To: ")
		     (read-string "Cc: ")))
  (let ((msg-filename (mh-msg-filename msg-or-seq))
	(folder mh-current-folder))
    (cond ((or (not (file-exists-p (format "%sdraft" mh-user-path)))
	       (y-or-n-p "The file 'draft' exists.  Discard it? "))
	   (mh-exec-cmd "forw" "-build" "-nodraftfolder" mh-current-folder
			msg-or-seq)
	   (mh-read-draft "" nil)
	   (mh-insert-fields "To:" to "Cc:" cc)
	   (set-buffer-modified-p nil))
	  (t
	   (mh-read-draft "" nil)))
    (goto-char (point-min))
    (re-search-forward "^------- Forwarded Message")
    (previous-line 1)
    (narrow-to-region (point) (point-max))
    (let* ((subject (save-excursion (mh-get-field "From:")))
	   (trim (string-match "<" subject))
	   (forw-subject (save-excursion (mh-get-field "Subject:"))))
      (if trim
	  (setq subject (substring subject 0 (1- trim))))
      (widen)
      (save-excursion
	(mh-insert-fields "Subject:" (format "[%s: %s]" subject forw-subject)))
      (delete-other-windows)
      (if prefix-provided
	  (mh-add-msg-list-to-seq (mh-seq-to-msgs msg-or-seq) "forwarded" t)
	  (mh-add-msg-to-seq msg-or-seq "forwarded" t))
      (mh-compose-and-send-mail "" folder msg-or-seq
				to subject cc "F" "Forwarded:"))))


(defun mh-goto-msg (number &optional no-error-if-no-message)
  "Position the cursor at message NUMBER.
Non-nil second argument means do not signal an error if message does not exist.
Return non-nil if cursor is at message."
  (interactive "nMessage number? ")
  (let ((cur-msg (mh-get-msg-num nil))
	(starting-place (point))
	(msg-pattern (mh-msg-search-pat number)))
    (cond ((cond ((and cur-msg (= cur-msg number)) t)
		 ((and cur-msg
		       (< cur-msg number)
		       (re-search-forward msg-pattern nil t)) t)
		 ((and cur-msg
		       (> cur-msg number)
		       (re-search-backward msg-pattern nil t)) t)
		 (t			; Do thorough search of buffer
		  (goto-char (point-min))
		  (re-search-forward msg-pattern nil t)))
	    (beginning-of-line)
	    (mh-maybe-show number)
	    t)
	  (t
	   (goto-char starting-place)
	   (if (not no-error-if-no-message)
	       (error "No message %d " number))
	   nil))))


(defun mh-inc-folder (&optional maildrop-name)
  "Inc(orporate) new mail into inbox.
Optional prefix argument specifies an alternate maildrop from the default.
If this is given, mail is incorporated into the current folder, rather
than +inbox."
  (interactive (list (if current-prefix-arg
			 (expand-file-name
			  (read-file-name "inc mail from file: "
					  mh-user-path)))))
  (when (not maildrop-name)
    (switch-to-buffer "+inbox")
    (if (or (not (boundp 'mh-current-folder)) (null mh-current-folder))
	(mh-make-folder "+inbox")))
  (mh-get-new-mail maildrop-name))


(defun mh-kill-folder ()
  "Remove the current folder."
  (interactive)
  (if (yes-or-no-p (format "Remove folder %s? " mh-current-folder))
      (let ((folder mh-current-folder))
	(mh-exec-cmd-demon "rmf" folder)
	(mh-remove-folder-from-folder-list folder)
	(message "Folder removed")
	(kill-buffer folder))
      (message "Folder not removed")))


(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (with-output-to-temp-buffer " *mh-temp*"
    (save-excursion
      (switch-to-buffer " *mh-temp*")
      (delete-region (point-min) (point-max))
      (message "listing folders...")
      (mh-exec-cmd-output "folders" t)
      (goto-char (point-min))
      (message "listing folders...done"))))


(defun mh-msg-is-in-seq (msg)
  "Display the sequences that contain MESSAGE (default: displayed message)."
  (interactive (list (mh-get-msg-num t)))
  (let ((l mh-seq-list)
	(seqs ""))
      (while l
	(if (memq msg (cdar l))
	    (setq seqs (format "%s %s" (symbol-name (caar l)) seqs)))
	(setq l (cdr l)))
      (message "Message %d is in sequences: %s" msg seqs)))


(defun mh-move-msg (prefix-provided msg-or-seq dest)
  "Move MESSAGE(s) (default: displayed message) to another FOLDER.
If (optional) prefix argument provided, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Move" mh-narrowed-to-seq)
			 (mh-get-msg-num t))
		     (intern (mh-prompt-for-folder "Destination" "" t))))
  (setq mh-last-destination (cons 'move dest))
  (if prefix-provided
      (mh-map-to-seq-msgs 'mh-move-a-msg msg-or-seq dest)
      (mh-move-a-msg msg-or-seq dest))
  (mh-next-msg))


(defun mh-move-or-write-again (msg)
  "Re-execution the last move or write command on the given MESSAGE (default:
displayed message).
Use the same folder or file as the previous move or write command."
  (interactive (list (mh-get-msg-num t)))
  (if (null mh-last-destination)
      (error "No previous move"))
  (cond ((eq (car mh-last-destination) 'move)
	 (mh-move-a-msg msg (cdr mh-last-destination))
	 (message "Destination folder: %s" (cdr mh-last-destination)))
	(t
	 (mh-write-msg-to-file msg (cdr mh-last-destination))
	 (message "Destination: %s" (cdr mh-last-destination))))
  (mh-next-msg))


(defun mh-narrow-to-seq (seq)
  "Restrict display of this folder to just messages in a sequence.
Reads which sequence.  Use \\[mh-widen] to undo this command."
  (interactive (list (mh-read-seq "Narrow to")))
  (let ((eob (point-max))
	(buffer-read-only nil))
    (cond ((mh-seq-to-msgs seq)
	   (mh-copy-seq-to-point seq eob)
	   (narrow-to-region eob (point-max))
	   (mh-make-folder-mode-line (symbol-name seq))
	   (recenter)
	   (setq mh-narrowed-to-seq seq))
	  (t
	   (error "No messages in sequence `%s'" (symbol-name seq))))))


(defun mh-next-line (&optional arg)
  "Move to next undeleted message in window."
  (interactive "p")
  (forward-line (if arg arg 1))
  (setq mh-next-direction 'forward)
  (cond ((re-search-forward mh-good-msg-regexp nil 0 arg)
	 (beginning-of-line)
	 (mh-maybe-show (mh-get-msg-num t)))
	(t
	 (forward-line -1)
	 (delete-other-windows)
	 (message "No more messages"))))


(defun mh-renumber-folder ()
  "Renumber messages in folder to be 1..N."
  (interactive)
  (message "packing buffer...")
  (mh-pack-folder)
  (mh-unmark-all-headers nil)
  (mh-goto-cur-msg)
  (message "packing buffer...done"))


(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window mh-show-buffer)
    ;; Go to top of screen (incase user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      ;; Search for blank line and then for From:
      (when (not (and (search-forward "\n\n" nil t)
		      (search-forward "From:" nil t)))
	(other-window -1)
	(error "No more messages.")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (next-line 2)
    (recenter 0)
    (other-window -1)))

  
(defun mh-page-digest-backwards ()
  "Back up displayed message to previous digested message."
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window mh-show-buffer)
    ;; Go to top of screen (incase user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      (beginning-of-line)
      (when (not (and (search-backward "\n\n" nil t)
		      (search-backward "From:" nil t)))
	(other-window -1)
	(error "No more messages.")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (next-line 2)
    (recenter 0)
    (other-window -1)))


(defun mh-page-msg (&optional arg)
  (interactive "P")
  (scroll-other-window arg))


(defun mh-previous-line (&optional arg)
  "Move to previous undeleted message in window."
  (interactive "p")
  (setq mh-next-direction 'backward)
  (cond ((re-search-backward mh-good-msg-regexp nil 0 arg)
	 (mh-maybe-show (mh-get-msg-num t)))
	(t
	 (delete-other-windows)
	 (message "Beginning of messages"))))


(defun mh-previous-page ()
  "Page the displayed message backwards."
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window mh-show-buffer)
    (unwind-protect
	(scroll-down nil)
      (other-window -1))))


(defun mh-print-msg (prefix-provided msg-or-seq)
  "Print MESSAGE(s) (default: displayed message) on a line printer.
If (optional) prefix argument is supplied, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (reverse (mh-seq-to-msgs
				   (mh-read-seq "Print" mh-narrowed-to-seq)))
			 (list (mh-get-msg-num t)))))
  (message "printing message...")
  (call-process shell-file-name nil nil nil "-c"
		(if prefix-provided
		    (format "(scan -clear %s ; %smhl -nobell -clear %s) | %s"
			    (mapconcat (function (lambda (msg) msg))
				       msg-or-seq " ")
			    mh-lib
			    (mh-msg-filenames msg-or-seq mh-folder-filename)
			    (format mh-lpr-command-format
				    (if prefix-provided
					"Mail"
					(format "%s/%d" mh-current-folder
						(car msg-or-seq)))))
		    (format "%smhl -nobell -clear %s | %s"
			    mh-lib
			    (mh-msg-filenames msg-or-seq mh-folder-filename)
			    (format mh-lpr-command-format
				    (if prefix-provided
					"Mail"
					(format "%s/%d" mh-current-folder
						(car msg-or-seq)))))))
  (if prefix-provided
      (mh-notate-seq msg-or-seq ?P mh-cmd-note)
      (mh-notate (car msg-or-seq) ?P mh-cmd-note))
  (mh-add-msg-list-to-seq msg-or-seq 'printed t)
  (message "printing message...done"))


(defun mh-put-msg-in-seq (prefix-provided from to)
  "Add MESSAGE(s) (default: displayed message) to SEQUENCE.
If (optional) prefix argument supplied, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-seq-to-msgs
			  (mh-read-seq "Add messages from" mh-narrowed-to-seq))
			 (mh-get-msg-num t))
		     (mh-read-seq "Add to" mh-narrowed-to-seq)))
  (if prefix-provided
      (mh-add-msg-list-to-seq from to)
      (mh-add-msg-to-seq from to))
  (mh-next-msg))


(defun mh-rescan-folder (range)
  "Rescan a folder after optionally processing the outstanding commands.
If (optional) prefix argument supplied, prompt for the range of messages to
display.  Otherwise show the entire folder."
  (interactive (list (if current-prefix-arg
			  (read-string "Range [all]? ")
			  "all")))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder range))


(defun mh-redistribute (to cc msg)
  "Redistribute a letter."
  (interactive (list (read-string "Redist-To: ")
		     (read-string "Redist-Cc: ")
		     (mh-get-msg-num t)))
  (let ((msg-filename (mh-msg-filename msg))
	(folder mh-current-folder))
    (save-window-excursion
      (mh-read-draft "redistribution"
		     (if mh-redist-full-contents msg-filename nil))
      (mh-goto-header-end 0)
      (insert "Resent-To: " to "\n")
      (if (not (equal cc ""))
	  (insert "Resent-cc: " cc "\n"))
      (mh-clean-msg-header (point-min)
			   "^Message-Id:\\|^Received:\\|Return-Path:"
			   nil)
      (save-buffer)
      (message "Redistributing...")
      (call-process "/bin/sh" nil 0 nil "-c"
       (format "mhdist=1 mhaltmsg=%s %s/send -push %s/draft"
	       msg-filename mh-progs mh-user-path))
      (mh-annotate-msg msg folder "R"
		       "-component" "Resent:"
		       "-text" (format "\"%s %s\"" to cc))
      (message "Redistributing...done"))))


(defun mh-write-msg-to-file (msg file)
  "Append MESSAGE to the end of a FILE."
  (interactive (list (mh-get-msg-num t)
		     (expand-file-name
		      (read-file-name "Save message in file: "))))
  (setq mh-last-destination (cons 'write file))
  (let ((file-name (mh-msg-filename msg)))
    (save-excursion
      (set-buffer (get-buffer-create " *mh-temp*"))
      (delete-region (point-min) (point-max))
      (insert-file-contents file-name)
      (append-to-file (point-min) (point-max) file))))


(defun mh-search-folder ()
  "Search the current folder for messages matching a pattern."
  (interactive)
  (let ((folder mh-current-folder))
    (switch-to-buffer-other-window "pick-pattern")
    (if (or (zerop (buffer-size))
	    (not (y-or-n-p "Reuse pattern? ")))
	(mh-make-pick-template)
	(message ""))
    (setq mh-searching-folder folder)))


(defun mh-send (to cc subject)
  "Compose and send a letter."
  (interactive "sTo: \nsCc: \nsSubject: ")
  (delete-other-windows)
  (mh-send-sub to cc subject))


(defun mh-send-other-window (to cc subject)
  "Compose and send a letter in another window.."
  (interactive "sTo: \nsCc: \nsSubject: ")
  (let ((pop-up-windows t))
    (mh-send-sub to cc subject)))


(defun mh-send-sub (to cc subject)
  "Do the real work of composing and sending a letter.
Expects the TO, CC, and SUBJECT fields as arguments."
  (let ((folder (if (boundp 'mh-current-folder) mh-current-folder)))
    (message "Composing a message...")
    (when (mh-read-draft "message"
			 (if (file-exists-p
			      (format "%scomponents" mh-user-path))
			     (format "%scomponents" mh-user-path)
			     (if (file-exists-p (format "%scomponents" mh-lib))
				 (format "%scomponents" mh-lib)
				 (error "Can't find components"))))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (message "Composing a message...done"))
    (mh-compose-and-send-mail "" folder nil to subject cc)))


(defun mh-show (msg)
  "Show MESSAGE (default: displayed message)."
  (interactive (list (mh-get-msg-num t)))
  (setq mh-summarize nil)
  (setq mode-name "Mh-Show")
  (let ((folder mh-current-folder))
    (mh-display-msg msg (mh-msg-filename msg) mh-show-buffer)

    ;; These contortions are to force the summary line to be the top window.
    (switch-to-buffer-other-window folder)
    (delete-other-windows)
    (switch-to-buffer-other-window mh-show-buffer)
    (switch-to-buffer-other-window folder)
    (shrink-window (- (window-height) mh-summary-height))
    (recenter 1)
    (push msg mh-seen-list)))


(defun mh-sort-folder ()
  "Sort the messages in the current folder by date."
  (interactive "")
  (mh-process-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (message "sorting folder...")
  (mh-exec-cmd "sortm" mh-current-folder)
  (message "sorting folder...done")
  (mh-scan-folder mh-current-folder "all"))


(defun mh-toggle-summarize ()
  "Turn the summary mode of displaying messages on or off."
  (interactive)
  (setq mh-summarize (not mh-summarize))
  (cond (mh-summarize
	 (delete-other-windows)
	 (setq mode-name "Mh-Summarize")
	 (recenter (/ (window-height) 2)))
	(t
	 (setq mode-name "Mh-Show")
	 (mh-show (mh-get-msg-num t)))))


(defun mh-undo (prefix-provided msg-or-seq)
  "Undo the deletion or move of the specified MESSAGE(s)
(default: displayed message).
If (optional) prefix argument is supplied, then prompt for sequence to use
instead of message."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Undo" mh-narrowed-to-seq)
			 (mh-get-msg-num t))))
  (cond ((looking-at "^....D")
	 (cond (prefix-provided
		(mapc (function (lambda (msg)
				  (setq mh-delete-list
					(delq msg mh-delete-list))
				  (mh-remove-msg-from-seq msg 'deleted)))
		      (mh-seq-to-msgs msg-or-seq))
		(mh-notate-seq msg-or-seq ?  mh-cmd-note))
	       (t
		(setq mh-delete-list (delq msg-or-seq mh-delete-list))
		(mh-remove-msg-from-seq msg-or-seq 'deleted)
		(mh-notate msg-or-seq ?  mh-cmd-note))))

	((looking-at "^....\\^")
	 (cond (prefix-provided
		(mapc (function (lambda (msg)
				  (mapc (function
					 (lambda (dest)
					   (mh-remove-msg-from-seq msg dest)))
					mh-move-list)))
		      (mh-seq-to-msgs msg-or-seq))
		(mh-notate-seq  msg-or-seq ?  mh-cmd-note))
	       (t
		(mapc (function (lambda (dest)
				  (mh-remove-msg-from-seq msg-or-seq dest)))
		      mh-move-list)
		(mh-notate msg-or-seq ?  mh-cmd-note))))

	(t nil)))


(defun mh-undo-folder ()
  "Undo all commands in current folder."
  (interactive "")
  (cond ((yes-or-no-p "Undo all commands in folder? ")
	 (setq mh-delete-list nil
	       mh-move-list nil
	       mh-seq-list nil
	       mh-next-direction 'forward)
	 (mh-unmark-all-headers t))
	(t
	 (message "Commands not undone.")
	 (sit-for 2))))


(defun mh-visit-folder (folder range)
  "Visit FOLDER and display RANGE of messages."
  (interactive (list (mh-prompt-for-folder "Visit" "+inbox" t)
		     (read-string "Range [all]? ")))
    (mh-scan-folder folder (if (equal range "") "all" range))
    (delete-other-windows))


(defun mh-widen ()
  "Remove restrictions from the current folder, thereby showing all messages."
  (interactive "")
  (let ((buffer-read-only nil))
    (delete-region (point-min) (point-max))
    (widen)
    (mh-make-folder-mode-line))
  (setq mh-narrowed-to-seq nil))



;;; Support routines.

(defun mh-delete-a-msg (msg)
  "Delete the MESSAGE."
  (save-excursion
    (mh-goto-msg msg nil)
    (if (looking-at "....\\^")
	(error "Message %d already moved.  Undo move before deleting." msg))
    (push msg mh-delete-list)
    (mh-add-msg-to-seq msg 'deleted t)
    (mh-notate msg ?D mh-cmd-note)))


(defun mh-move-a-msg (msg destination)
  "Move the MESSAGE to the FOLDER."
  (save-excursion
    (mh-goto-msg msg nil)
    (cond ((looking-at "....D")
	   (error "Message %d is already deleted.  Undo delete before moving."
		  msg))
	  (t
	   (if (not (memq destination mh-move-list))
	       (push destination mh-move-list))
	   (mh-add-msg-to-seq msg destination t)
	   (mh-notate msg ?^ mh-cmd-note)))))


(defun mh-display-msg (msg-num msg-filename show-buffer)
  "Display the message NUMBER and PATHNAME in BUFFER."
  (if (not (file-exists-p msg-filename))
      (error "Message %d does not exist." msg-num))
  (switch-to-buffer show-buffer)
  (when (not (equal msg-filename buffer-file-name))
    ;; Buffer does not yet contain message.
    (clear-visited-file-modtime)
    (unlock-buffer)
    (delete-region (point-min) (point-max))
    (if mh-use-mhl
	(mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear" msg-filename)
	(insert-file-contents msg-filename t))
    (goto-char (point-min))
    (cond (mh-clean-message-header
	   (mh-clean-msg-header (point-min)
				mh-invisible-headers
				mh-visible-headers)
	   (goto-char (point-min)))
	  (t
	   (let ((case-fold-search t))
	     (re-search-forward "^To:\\|^From:\\|^Subject:\\|^Date:" nil t)
	     (beginning-of-line)
	     (recenter 0))))
    (set-buffer-modified-p nil)
    (setq buffer-file-name msg-filename)
    (set-mark nil)
    (setq mode-line-buffer-identification
	  (list "mh-e: {%b}  "
		(format "%s" folder) "/" (format "%d" msg-num)))))


(defun mh-clean-msg-header (start invisible-headers visible-headers)
  "Flush extraneous lines in a message header, from the given POINT to the
end of the message header.  If VISIBLE-HEADERS is non-nil, it contains a
regular expression specifying the lines to display, otherwise INVISIBLE-HEADERS
contains a regular expression specifying lines to delete from the header."
  (save-restriction
    (goto-char start)
    (if (search-forward "\n\n" nil t)
	(backward-char 2))
    (narrow-to-region start (point))
    (goto-char (point-min))
    (if visible-headers
	(while (< (point) (point-max))
	  (beginning-of-line)
	  (cond ((looking-at visible-headers)
		 (forward-line 1))
		(t
		 (kill-line 1)
		 (while (looking-at "^[ \t]+")
		   (beginning-of-line)
		   (kill-line 1)))))
	(while (re-search-forward invisible-headers nil t)
	  (beginning-of-line)
	  (kill-line 1)
	  (while (looking-at "^[ \t]+")
	    (beginning-of-line)
	    (kill-line 1))))
    (unlock-buffer)))


(defun mh-read-draft (use initial-contents)
  "Read draft file into draft buffer.  USE is a message used for prompting
about the intended use of the message.  INITIAL-CONTENTS is filename that
is read into an empty buffer, or NIL if buffer should not be modified.
Returns T if the buffer is set to the contents of this file, NIL otherwise."
  (pop-to-buffer "draft")		; Create if necessary
  (if (buffer-modified-p)
      (if (y-or-n-p "Draft is modified; kill anyways? ")
	  (set-buffer-modified-p nil)
	(error "Draft is not killed.")))
  (setq buffer-file-name (format "%sdraft" mh-user-path))
  (clear-visited-file-modtime)
  (unlock-buffer)
  (if (file-exists-p (format "%sdraft" mh-user-path))
      (insert-file-contents (format "%sdraft" mh-user-path)))
  (prog1
      (cond ((and initial-contents
		  (or (zerop (buffer-size))
		      (not (y-or-n-p
			    (format "The file 'draft' exists.  Use for %s? "
				    use)))))
	     (delete-region (point-min) (point-max))
	     (insert-file-contents initial-contents)
	     t)
	    (t nil))
    (auto-save-mode 1)))


(defun mh-next-msg ()
  "Move backward or forward to the next message in the buffer."
  (if (eq mh-next-direction 'forward)
      (mh-next-line 1)
      (mh-previous-line 1)))


(defun mh-maybe-show (msg)
  "If the scan listing is not summarized, show the message pointed to
by the cursor."
  (if (not mh-summarize) (mh-show msg)))



;;; The folder data abstraction.

(defun mh-make-folder (name)
  "Create and initialize a new mail folder called NAME and make it the
current folder."
  (switch-to-buffer name)
  (buffer-flush-undo (current-buffer))
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (make-local-vars
   'mh-current-folder name		;Name of folder
   'mh-show-buffer (format "show-%s" name) ; Buffer that displays messages
   'mh-folder-filename			; e.g. /usr/foobar/Mail/inbox/
     (format "%s%s/" mh-user-path (substring name 1))
   'mh-summarize t			; Show scan list only?
   'mh-next-seq-num 0			; Index of free sequence id
   'mh-delete-list nil			; List of msgs nums to delete
   'mh-move-list nil			; List of folder names in mh-seq-list
   'mh-seq-list nil			; Alist of (seq . msgs) nums
   'mh-seen-list nil			; List of displayed messages
   'mh-next-direction 'forward		; Direction to move to next message
   'mh-narrowed-to-seq nil		; Sequence display is narrowed to
   'mh-first-msg-num nil		; Number of first msg in buffer
   'mh-last-msg-num nil)		; Number of last msg in buffer
  (mh-folder-mode)
  (setq buffer-read-only t)
  (setq mode-name "Mh-Summarize")
  (save-excursion			; Make show buffer
    (switch-to-buffer mh-show-buffer)
    (buffer-flush-undo (current-buffer))))



(defun make-local-vars (&rest pairs)
  "Take VARIABLE-VALUE pairs and makes local variables initialized to the
value."
  (while pairs
    (make-local-variable (car pairs))
    (set (car pairs) (cadr pairs))
    (setq pairs (cddr pairs))))


(defun mh-folder-mode ()
  "Major mode for \"editing\" an MH folder scan listing.
Messages can be marked for refiling and deletion.  However, both actions
are defered until you request execution with \\[mh-execute-commands].
\\{mh-folder-mode-map}
  A prefix argument (\\[universal-argument]) to delete, move, list, or undo applies the action to a message sequence.

Variables controlling mh-e operation are (defaults in parentheses):

 mh-clean-message-header (nil)
    Non-nil means remove header lines matching the regular expression
    specified in mh-invisible-headers from messages.

 mh-use-mhl (nil)
    Non-nil means use mhl to format displayed messages.

 mh-lpr-command-format (\"lpr -p -J '%s'\")
    Format for command used to print a message on a system printer.

 mh-summary-height (4)
    Number of lines in the summary window.

 mh-ins-buf-prefix (\">> \")
    String to insert before each non-blank line of a message as it is
    inserted in a letter being composed."

  (use-local-map mh-folder-mode-map)
  (setq major-mode 'mh-folder-mode)
  (setq mode-name "mh-folder")
  (if (and (boundp 'mh-folder-mode-hook) mh-folder-mode-hook)
      (funcall mh-folder-mode-hook)))


(defun mh-scan-folder (folder range)
  "Scan the FOLDER over the RANGE.  Return in the folder's buffer."
  (cond ((null (get-buffer folder))
	 (mh-make-folder folder))
	(t
	 (if (or mh-delete-list mh-move-list)
	     (if (y-or-n-p "Process outstanding commands (or lose them)? ")
		 (mh-process-commands folder)
		 (mh-undo-folder)))
	 (switch-to-buffer-other-window folder)))
  (mh-regenerate-headers range)
  (when (= (count-lines (point-min) (point-max)) 0)
    (if (equal range "all")
	(message  "Folder %s is empty" folder)
	(message  "No messages in %s, range %s" folder range))
    (sit-for 5))
  (mh-unmark-all-headers nil)
  (mh-goto-cur-msg))


(defun mh-regenerate-headers (range)
  "Replace buffer with scan of its contents over range RANGE."
  (let ((buffer-read-only nil))
    (message (format "scanning %s..." (buffer-name)))
    (delete-other-windows)
    (delete-region (point-min) (point-max))
    (mh-exec-cmd-output "scan" nil "-noclear" "-noheader" (buffer-name) range)
    (goto-char (point-min))
    (cond ((looking-at "scan: no messages in")
	   (keep-lines "^[ ]*[0-9]"))	; Flush random scan lines
	  ((looking-at "scan: "))	; Keep error messages
	  (t
	   (keep-lines "^[ ]*[0-9]")))	; Flush random scan lines
    (mh-make-folder-mode-line)
    (message (format "scanning %s...done" (buffer-name)))))


(defun mh-get-new-mail (maildrop-name)
  "Read new mail from a maildrop into the current buffer.
Return t if there was new mail, nil otherwise.  Return in the current buffer."
  (let ((buffer-read-only nil)
	(point-before-inc (point)))
    (message (if maildrop-name
		 (format "inc %s -file %s..." (buffer-name) maildrop-name)
		 (format "inc %s..." (buffer-name))))
    (mh-unmark-all-headers nil)
    (setq mh-next-direction 'forward)
    (flush-lines "^inc:\\|^scan:")	; Kill old error messages
    (goto-char (point-max))
    (let ((start-of-inc (point)))
      (if maildrop-name
	  (mh-exec-cmd-output "inc" nil (buffer-name) "-file"
			      (expand-file-name maildrop-name)
			      "-truncate")
	  (mh-exec-cmd-output "inc" nil))
      (message
       (if maildrop-name
	   (format "inc %s -file %s...done" (buffer-name) maildrop-name)
	   (format "inc %s...done" (buffer-name))))
      (goto-char start-of-inc)
      (cond ((looking-at "inc: no mail")
	     (keep-lines "^[ ]*[0-9]")	; Flush random scan lines
	     (mh-make-folder-mode-line)
	     (goto-char point-before-inc)
	     (message "No new mail%s%s." (if maildrop-name " in " "")
		      (if maildrop-name maildrop-name ""))
	     nil)
	    ((looking-at "inc:")	; Error messages
	     (mh-make-folder-mode-line)
	     (goto-char point-before-inc)
	     (message "inc error")
	     nil)
	    (t
	     (keep-lines "^[ ]*[0-9]")
	     (mh-make-folder-mode-line)
	     (mh-goto-cur-msg)
	     t)))))


(defun mh-make-folder-mode-line (&optional annotation)
  "Set the fields of the mode line for a folder buffer.
The optional ANNOTATION string is displayed after the folder's name."
  (save-excursion
    (goto-char (point-min))
    (setq mh-first-msg-num (mh-get-msg-num nil))
    (let* ((lines (count-lines (point-min) (point-max)))
	   (case-fold-search nil))
      (goto-char (point-max))
      (previous-line 1)
      (setq mh-last-msg-num (mh-get-msg-num nil))
      (setq mode-line-buffer-identification
	    (list (format "mh-e: {%%b%s}  [%d message%s]"
			  (if annotation (format "/%s" annotation) "")
			  lines
			  (if (= lines 0)
			      "s"
			    (if (> lines 1)
				(format "s (%d - %d)" mh-first-msg-num
					mh-last-msg-num)
			      (format " (%d)" mh-first-msg-num)))))))))


(defun mh-unmark-all-headers (remove-all-flags)
  "Remove all + flags from the headers, and if called
with a non-nil argument, remove all D and ^ flags too."
  (save-excursion
    (let ((buffer-read-only nil)
	  (case-fold-search nil))
      (goto-char (point-min))
      (while (if remove-all-flags
		 (re-search-forward "^....\\D\\|^....\\^\\|^....\\+\\|.....%"
				    nil t)
		 (re-search-forward "^....\\+" nil t))
	(delete-backward-char 1)
	(insert " ")))))


(defun mh-goto-cur-msg ()
  "Position the cursor at the current message."
  (let ((curmsg (mh-get-cur-msg mh-current-folder)))
    (cond ((or (zerop curmsg) (not (mh-goto-msg curmsg t)))
	   (goto-char (point-max))
	   (forward-line -1)
	   (message "No current message"))
	  (t
	   (mh-notate curmsg ?+ 4)
	   (recenter 0)
	   (mh-maybe-show curmsg)))))


(defun mh-pack-folder ()
  "Close and pack the current folder."
  (let ((buffer-read-only nil))
    (message "closing folder...")
    (mh-process-commands mh-current-folder)
    (message "packing folder...")
    (save-excursion
      (mh-exec-cmd-quiet " *mh-temp*" "folder" mh-current-folder "-pack"))
    (mh-regenerate-headers "all")
    (message "packing done")))


(defun mh-process-commands (buffer)
  "Process outstanding commands for the buffer BUFFER."
  (message "Processing deletes and moves...")
  (set-buffer buffer)
  (let ((buffer-read-only nil))
    ;; Sequences must be first
    (mh-process-seq-commands mh-seq-list)
    ;; Update the unseen sequence
    (if mh-seen-list
	(let ((unseen-seq (mh-get-profile-field "Unseen-Sequence:")))
	  (if (null unseen-seq)		; For MH.5
	      (setq unseen-seq "unseen"))
	  (apply 'mh-exec-cmd-quiet
		 (nconc (list 0 "mark" mh-current-folder)
			mh-seen-list
			(list "-sequence" unseen-seq "-delete")))))

    ;; Then refile messages
    (mapc (function
	   (lambda (dest)
	     (let ((msgs (mh-seq-to-msgs dest)))
	       (when msgs
		 (mh-delete-scan-msgs msgs)
		 (apply 'mh-exec-cmd
			(nconc (cons "refile" msgs)
			       (list "-src" buffer (symbol-name dest))))))))
	  mh-move-list)

    ;; Now delete messages
    (when mh-delete-list
      (apply 'mh-exec-cmd
	     (nconc (list "rmm" (format "%s" buffer)) mh-delete-list))
      (mh-delete-scan-msgs mh-delete-list))

    ;; Mark as cur message.
    (cond ((mh-get-msg-num nil)
	   (mh-exec-cmd "mark" mh-current-folder (mh-get-msg-num nil)
			"-seq" "cur" "-add" "-zero"))
	  ((> (buffer-size) 0)		; Some messages left in folder.
	   (mh-exec-cmd "mark" mh-current-folder
			"-seq" "cur" "-delete" "all")))

    (save-excursion
      (set-buffer mh-show-buffer)
      (setq buffer-file-name nil))	; Invalidate buffer file cache

    (setq mh-delete-list nil
	  mh-move-list nil
	  mh-seq-list nil
	  mh-seen-list nil))
  (message "Processing deletes and moves...done"))


(defun mh-delete-scan-msgs (msgs)
  "Delete the scan listing lines for each of the msgs in the LIST."
  (save-excursion
    (goto-char (point-min))
    (flush-lines (mapconcat 'mh-msg-search-pat msgs "\\|"))))



;;; A mode for composing and sending a message.

(defun mh-letter-mode ()
    "Mode for composing letters in mh-e.
\\{mh-letter-mode-map}"
  (text-mode)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-separate))
  (use-local-map mh-letter-mode-map)
  (setq major-mode 'mh-letter-mode)
  (setq mode-name "mh-letter")
  (if (and (boundp 'mh-letter-mode-hook) mh-letter-mode-hook)
      (funcall mh-letter-mode-hook)))


(defun mh-to-field ()
  "Move point to the end of the header field indicated by the previous
keystroke.Create the field if it does not exist.  Set the mark to the
point before moving."
  (interactive "")
  (expand-abbrev)
  (let ((target (cdr (assoc last-input-char
			    '((?t . "To:") (?s . "Subject:") (?c . "Cc:")
			      (?b . "Bcc:") (?f . "Fcc:"))))))
    (cond ((mh-position-on-field target t)
	   (if (not (looking-at "[ \t]"))(insert-string " ")))
	  (t
	   (goto-char (dot-min))
	   (re-search-forward "^To:")
	   (forward-line 1)
	   (insert-string (format "%s \n" target))
	   (backward-char 1)))))


(defun mh-insert-signature ()
  "Insert the file ~/.signature at the current point"
  (interactive "")
  (insert-file-contents "~/.signature"))


(defun mh-check-whom ()
  "List recipients of the current letter."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (set-buffer-modified-p t)		; Force writing of contents
    (save-buffer)
    (message "Checking recipients...")
    (switch-to-buffer-other-window "*Mail Recipients*")
    (bury-buffer (current-buffer))
    (delete-region (point-min) (point-max))
    (mh-exec-cmd-output "whom" t file-name)
    (other-window -1)
    (message "Checking recipients...done")))



;;; Routines to make a search pattern and search for a message.

(defun mh-make-pick-template ()
  "Initialize the current buffer with a template for a pick pattern."
  (delete-region (point-min) (point-max))
  (kill-all-local-variables)
  (make-local-variable 'mh-searching-folder)
  (insert "From: \n"
	  "To: \n"
	  "Cc: \n"
	  "Date: \n"
	  "Subject: \n"
	  "---------\n")
  (mh-letter-mode)
  (use-local-map mh-pick-mode-map)
  (setq mode-line-buffer-identification (list "mh-e: {%b}  Pick Pattern"))
  (goto-char (point-min))
  (end-of-line))


(defun mh-do-pick-search ()
  "Find messages in current folder matching qualifications in current buffer.
Put messages found in a sequence named `search'."
  (interactive)
  (let* ((pattern-buffer (buffer-name))
	 (searching-buffer mh-searching-folder)
	 (range)
	 (pattern nil))
    (save-excursion
      (set-buffer searching-buffer)
      (setq range (format "%d-%d" mh-first-msg-num mh-last-msg-num)))
    (message "Searching...")
    (goto-char (point-min))
    (while (setq pattern (mh-next-pick-field pattern-buffer))
      (setq msgs (mh-seq-from-command searching-buffer
				      'search
				      (nconc (cons "pick" pattern)
					     (list searching-buffer
						   range
						   "-sequence" "search"
						   "-list"))))
      (setq range "search"))
    (message "Searching...done")
    (switch-to-buffer searching-buffer)
    (delete-other-windows)
    (mh-notate-seq 'search ?% (+ mh-cmd-note 1))))


(defun mh-next-pick-field (buffer)
  "Return the next piece of a pick argument that can be extracted from the
BUFFER.  Returns nil if no pieces remain."
  (set-buffer buffer)
  (let ((case-fold-search t))
    (cond ((eobp)
	   nil)
	  ((re-search-forward "^\\([a-z].*\\):[ \t]*\\([a-z0-9].*\\)$" nil t)
	   (let* ((component
		   (format "-%s"
			   (downcase (buffer-substring (match-beginning 1)
						       (match-end 1)))))
		  (pat (buffer-substring (match-beginning 2) (match-end 2))))
	       (forward-line 1)
	       (list component pat)))
	  ((re-search-forward "^-*$" nil t)
	   (forward-char 1)
	   (let ((body (buffer-substring (point) (point-max))))
	     (if (and (> (length body) 0) (not (equal body "\n")))
		 (list "-search" body)
		 nil)))
	  (t
	   nil))))



;;; Routines compose and send a letter.

(defun mh-compose-and-send-mail (send-args sent-from-folder sent-from-msg
					   to subject cc
					   &optional annotate-char
					   annotate-field search-prefix)
  "Edit and compose a draft message and send or save it.
SENT-FROM-FOLDER is buffer containing scan listing of current folder, or
nil if none exists.
SENT-FROM-MSG is the message number or sequence name or nil.
SEND-ARGS is an optional argument passed to the send command.
The TO, SUBJECT, and CC fields are passed to the mh-compose-letter-hook.
If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of the
message.  In that case, the ANNOTATE-FIELD is used to build a string
for mh-annotate-msg."
  (pop-to-buffer "draft")
  (mh-letter-mode)
  (make-local-vars
   'mh-send-args send-args
   'mh-sent-from-folder sent-from-folder
   'mh-sent-from-msg sent-from-msg
   'mh-annotate-field annotate-field
   'mh-annotate-char annotate-char
   'mh-annotate-search-prefix (if search-prefix search-prefix ""))
  (setq mode-line-buffer-identification (list "mh-e: {%b}  Mail/draft"))
  (if (and (boundp 'mh-compose-letter-hook) mh-compose-letter-hook)
      (funcall mh-compose-letter-hook to subject cc)))


(defun mh-send-letter (&optional arg)
  "Send the draft letter in the current buffer.
If (optional) prefix argument supplied, monitor delivery."
  (interactive "P")
  (set-buffer-modified-p t)		; Make sure buffer is written
  (save-buffer)
  (message "Sending...")
  (let ((buffer-name (buffer-name))
	(file-name (buffer-file-name)))
    (cond (arg
	   (pop-to-buffer "MH mail delivery")
	   (kill-region (point-min) (point-max))
	   (if mh-send-args
	       (mh-exec-cmd-output "send" t "-watch" "-unique"
				   "-nodraftfolder" mh-send-args file-name)
	       (mh-exec-cmd-output "send" t "-watch" "-unique"
				   "-nodraftfolder" file-name)))

	  (mh-send-args
	   (mh-exec-cmd-demon "send" "-unique" "-nodraftfolder" "-noverbose"
			      mh-send-args file-name))
	  (t
	   (mh-exec-cmd-demon "send" "-unique" "-nodraftfolder" "-noverbose"
			      file-name)))

    (if mh-annotate-char
	(mh-annotate-msg mh-sent-from-msg
			 mh-sent-from-folder
			 mh-annotate-char
			 "-component" mh-annotate-field
			 "-text" (format "\"%s %s\""
					 (mh-get-field
					  (format "%s%s"
						  mh-annotate-search-prefix
						  "To:"))
					 (mh-get-field
					  (format "%s%s"
						  mh-annotate-search-prefix
						  "Cc:")))))

    (if (or (not arg)
	    (y-or-n-p "Kill draft buffer? "))
	(let ((sent-from-folder mh-sent-from-folder)
	      (sent-from-msg mh-sent-from-msg))
	  (kill-buffer buffer-name)
	  (when sent-from-folder
	    (delete-other-windows)
	    (switch-to-buffer sent-from-folder)
	    (if sent-from-msg
		(mh-maybe-show sent-from-msg)))))

    (message "Sending...done")))



(defun mh-insert-letter (prefix-provided folder msg)
  "Insert a message from any folder into the current letter.
Removes the message's headers using mh-invisible-headers.
Prefixes each non-blank line with mh-ins-buf-prefix (default \">> \").
If (optional) prefix argument supplied, do not indent and do not delete
headers.
Leaves the point before the letter and the mark after it."
  (interactive
   (list current-prefix-arg
	 (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
	 (read-input (format "Message number%s: "
			     (if mh-sent-from-msg
				 (format " [%d]" mh-sent-from-msg)
				 "")))))
  (let ((start (point)))
    (if (equal msg "") (setq msg (format "%d" mh-sent-from-msg)))
    (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
			    (format "%s%s/%s"
				    mh-user-path (substring folder 1) msg))
    (when (not prefix-provided)
      (mh-clean-msg-header start mh-invisible-headers mh-visible-headers)
      (narrow-to-region start (mark))
      (mh-insert-prefix-string mh-ins-buf-prefix)
      (widen))
    (exchange-point-and-mark)))


(defun mh-insert-cur-msg ()
  "Insert the currently displayed message into the current draft buffer.
Prefixes each non-blank line with the string mh-ins-buf-prefix.
If there is a region set in the  message's buffer, only the region will
be inserted.  Otherwise, the region from (point) to the end of the draft
buffer will be grabbed."
  (interactive)
  (if (and (boundp 'mh-sent-from-folder) mh-sent-from-folder mh-sent-from-msg)
      (let ((to-point (point))
	    (to-buffer (current-buffer)))
	(set-buffer mh-sent-from-folder)
	(set-buffer mh-show-buffer)	; Find displayed message
	(let ((mh-ins-str (if (mark)
			      (buffer-substring (point) (mark))
			    (buffer-substring (point) (point-max)))))
	  (set-buffer to-buffer)
	  (narrow-to-region to-point to-point)
	  (insert-string mh-ins-str)
	  (mh-insert-prefix-string mh-ins-buf-prefix)
	  (widen)))
      (error "There is no current message.")))


(defun mh-insert-prefix-string (ins-string)
  "Preface each line in the current buffer with STRING."
  (goto-char (point-min))
  (while (not (eobp))
    (insert-string ins-string)
    (forward-line 1))
  (goto-char (point-min)))


(defun mh-fully-kill-draft ()
  "Kill the draft message file and the draft message buffer.
Use \\[kill-buffer] if you don't want to delete the draft message file."
  (interactive "")
  (if (y-or-n-p "Kill draft message? ")
      (let ((sent-from-folder mh-sent-from-folder)
	    (sent-from-msg mh-sent-from-msg))
	(if (file-exists-p (buffer-file-name))
	    (delete-file (buffer-file-name)))
	(set-buffer-modified-p nil)
	(kill-buffer (buffer-name))
	(when sent-from-folder
	  (delete-other-windows)
	  (switch-to-buffer sent-from-folder)
	  (if sent-from-msg
	      (mh-maybe-show sent-from-msg))))
      (error "Message not killed")))



;;; Commands to manipulate sequences.  Sequences are stored in an alist
;;; of the form:
;;;	((seq-name msgs ...) (seq-name msgs ...) ...)


(defmacro mh-seq-name (pair)
  (list 'car pair))

(defmacro mh-seq-msgs (pair)
  (list 'cdr pair))


(defun mh-seq-to-msgs (seq)
  "Return the messages in sequence SEQ."
  (mh-seq-msgs (assoc seq mh-seq-list)))


(defun mh-msg-to-seq (msg)
  "Given a MESSAGE number, return the first sequence in which it occurs."
  (let ((l mh-seq-list))
    (while (and l (not (memq msg (cdar l))))
      (setq l (cdr l)))
    (caar l)))


(defun mh-read-seq (prompt &optional default)
  "Read and return a sequence name from the minibuffer, prompting with
the string PROMPT and supplying the optional DEFAULT.
% defaults to the sequences containing the current message.
Makes sure that the sequence is known to MH commands."
  (let ((input (completing-read
		(format "%s %s %s" prompt "sequence:"
			(if default (format "[%s] " (symbol-name default)) ""))
		(mh-seq-names mh-seq-list))))
    (let ((seq (cond ((equal input "%") (mh-msg-to-seq (mh-get-msg-num t)))
		     ((equal input "") default)
		     (t (intern input)))))
      (mh-process-seq seq (mh-seq-to-msgs seq))
      seq)))


(defun mh-seq-names (seq-list)
  "Return an alist of the names of the SEQUENCES."
  (mapcar (function (lambda (entry) (cons (symbol-name (car entry)) nil)))
	  seq-list))


(defun mh-seq-from-command (folder seq command)
  "In FOLDER, make a sequence named SEQ by executing COMMAND."
  (let ((msgs ())
	(case-fold-search t))
    (save-excursion
      (save-window-excursion
	(apply 'mh-exec-cmd-quiet (cons " *mh-temp*" command))
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9]+\\)" nil t)
	  (let ((num (string-to-int (buffer-substring (match-beginning 1)
						      (match-end 1)))))
	    (if (not (zerop num))
		(push num msgs)))))
      (set-buffer folder)
      (setq msgs (nreverse msgs))	; Put in ascending order
      (push (cons seq msgs) mh-seq-list)
      msgs)))


(defun mh-remove-seq (seq)
  "Delete the sequence SEQ."
  (let ((entry (assoc seq mh-seq-list)))
    (mh-notate-seq seq ?  (+ mh-cmd-note 1))
    (setq mh-seq-list (delq entry mh-seq-list))))


(defun mh-remove-msg-from-seq (msg seq &optional do-not-mark)
  "Remove MESSAGE from the sequence SEQ.  If optional FLAG is
non-nil, do not mark the message as being part of a sequence."
  (let ((seq (assoc seq mh-seq-list)))
    (if seq
	(setcdr seq (delq msg (mh-seq-msgs seq)))))
  (if (not do-not-mark) (mh-notate msg ? (+ mh-cmd-note 1))))


(defun mh-add-msg-to-seq (msg seq &optional do-not-mark)
  "Add MESSAGE to the SEQUENCE.  If optional FLAG is non-nil,
do not mark the message as being part of a sequence."
  (let ((seq-list (assoc seq mh-seq-list)))
    (if (not do-not-mark) (mh-notate msg ?% (+ mh-cmd-note 1)))
    (if (null seq-list)
	(push (cons seq (list msg)) mh-seq-list)
	(setcdr seq-list (cons msg (cdr seq-list))))))


(defun mh-add-msg-list-to-seq (msgs seq &optional do-not-mark)
  "Add the messages in LIST to the SEQUENCE.  If optional FLAG is non-nil,
do not mark the messages as being part of a sequence."
  (mapc (function (lambda (msg) (mh-add-msg-to-seq msg seq do-not-mark)))
	msgs))


(defun mh-rename-seq (seq new-name)
  "Rename a SEQUENCE to have a new NAME."
  (interactive "SOld sequence name: \nSNew name: ")
  (let ((old-seq (assoc seq mh-seq-list)))
    (if old-seq
	(rplaca old-seq new-name)
	(error "Sequence %s does not exists" (symbol-name seq)))))


(defun mh-notate-seq (seq notation offset)
  "Mark all messages in the sequence SEQ with the NOTATION at character
OFFSET."
  (mh-map-to-seq-msgs 'mh-notate seq notation offset))


(defun mh-map-to-seq-msgs (func seq &rest args)
  "Invoke the function FUNC at each message in the sequence SEQ, passing
the remaining ARGS as arguments."
  (let ((msgs (mh-seq-to-msgs seq)))
    (while msgs
      (mh-goto-msg (car msgs))
      (apply func (cons (car msgs) args))
      (setq msgs (cdr msgs)))))


(defun mh-map-over-seqs (func seq-list)
  "Apply the function FUNC to each element in the sequence LIST,
passing the sequence name and a list of messages as arguments."
  (while seq-list
    (funcall func (caar seq-list) (cdar seq-list))
    (setq seq-list (cdr seq-list))))


(defun mh-process-seq-commands (seq-list)
  "Process outstanding sequence commands for the sequences in SEQ-LIST."
  (mh-map-over-seqs 'mh-process-seq seq-list))


(defun mh-process-seq (seq msgs)
  "Mark sequence SEQ to contain MSGS."
  ;; Do not mark pseudo-sequences or empty sequences.
  (if (not (equal (substring (symbol-name seq) 0 1) "+"))
      (if msgs
	  (apply 'mh-exec-cmd
		 (nconc (list "mark" mh-current-folder
			      "-zero" "-seq" (format "%s" seq) "-add")
			msgs)))))


(defun mh-copy-seq-to-point (seq location)
  "Copy the messages in SEQUENCE to after the LOCATION in the current buffer."
  (mh-map-to-seq-msgs 'mh-copy-line-to-point seq location))


(defun mh-copy-line-to-point (msg location)
  "Copy the current line to the LOCATION in the current buffer."
  (beginning-of-line)
  (let ((beginning-of-line (point)))
    (forward-line 1)
    (copy-region-as-kill beginning-of-line (point))
    (goto-char location)
    (yank)
    (goto-char beginning-of-line)))



;;; Issue commands to mh.

(defun mh-exec-cmd (command &rest args)
  "Execute MH command COMMAND with ARGS.  Any output is shown to the user."
  (save-window-excursion
    (switch-to-buffer-other-window " *mh-temp*")
    (delete-region (point-min) (point-max))
    (apply 'call-process
	   (nconc (list (format "%s%s" mh-progs command) nil t nil)
		  (mh-list-to-string args)))
    (if (> (buffer-size) 0)
	(sit-for 5))))


(defun mh-exec-cmd-quiet (buffer command &rest args)
  "In BUFFER, execute MH command COMMAND with ARGS.  Return in buffer, if
one exists."
  (when (stringp buffer)
    (switch-to-buffer buffer)
    (delete-region (point-min) (point-max)))
  (apply 'call-process
	 (nconc (list (format "%s%s" mh-progs command) nil buffer nil)
		(mh-list-to-string args))))


(defun mh-exec-cmd-output (command display &rest args)
  "Execute MH command COMMAND with DISPLAY flag and ARGS putting the output
into buffer after point.  Set mark after inserted text."
  (push-mark (point) t)
  (apply 'call-process
	 (nconc (list (format "%s%s" mh-progs command) nil t display)
		(mh-list-to-string args)))
  (exchange-point-and-mark))


(defun mh-exec-cmd-demon (command &rest args)
  "Execute MH command COMMAND with ARGS.  Any output from command is displayed
in an asynchronous pop-up window."
  (let ((process (apply 'start-process
			(nconc (list "mh-output" nil
				     (expand-file-name command mh-progs))
			       (mh-list-to-string args)))))
    (set-process-filter process 'mh-process-demon)))


(defun mh-process-demon (process output)
  "Process demon that puts output into a temporary buffer."
  (pop-to-buffer " *mh-temp*")
  (insert output)
  (sit-for 2))


(defun mh-exec-lib-cmd-output (command &rest args)
  "Execute MH library command COMMAND with ARGS.  Put the output into
buffer after point.  Set mark after inserted text."
  (push-mark (point) t)
  (apply 'call-process
	 (nconc (list (format "%s%s" mh-lib command) nil t nil)
		(mh-list-to-string args)))
  (exchange-point-and-mark))


(defun mh-list-to-string (l)
  "Flattens the list L and makes every element of the new list into a string."
  (let ((new-list nil))
    (while l
      (cond ((null (car l)))
	    ((symbolp (car l)) (push (format "%s" (car l)) new-list))
	    ((numberp (car l)) (push (format "%d" (car l)) new-list))
	    ((equal (car l) ""))
	    ((stringp (car l)) (push (car l) new-list))
	    ((listp (car l))
	     (setq new-list (nconc (nreverse (mh-list-to-string (car l)))
				   new-list)))
	    (t (error "Bad argument %s" (car l))))
      (setq l (cdr l)))
    (nreverse new-list)))



;;; Commands to annotate a message.

(defun mh-annotate-msg (msg buffer note &rest args)
  "Mark the MESSAGE in BUFFER listing with the character NOTE and annotate
the saved message with ARGS."
  (apply 'mh-exec-cmd (cons "anno" (cons buffer (cons msg args))))
  (save-excursion
    (set-buffer buffer)
    (if (symbolp msg)
	(mh-notate-seq msg note (+ mh-cmd-note 1))
	(mh-notate msg note (+ mh-cmd-note 1)))))


(defun mh-notate (msg notation offset)
  "Marks MESSAGE with the character NOTATION at position OFFSET."
  (save-excursion
    (if (mh-goto-msg msg t)
	(let ((buffer-read-only nil))
	  (beginning-of-line)
	  (goto-char (+ (point) offset))
	  (delete-char 1)
	  (insert notation)))))



;;; User prompting commands.

(defun mh-prompt-for-folder (prompt default can-create)
  "Prompt for a folder name with PROMPT.  Returns the folder's name.
DEFAULT is used if the folder exists and the user types return.
If the CAN-CREATE flag is t, then a non-existant folder is made."
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 name)
    (if (null mh-folder-list)
	(setq mh-folder-list (mh-make-folder-list)))
    (while (and (setq name (completing-read prompt mh-folder-list
					    nil nil "+"))
		(equal name "")
		(equal default "")))
    (cond ((or (equal name "") (equal name "+"))
	   (setq name default))
	  ((not (equal (substring name 0 1) "+"))
	   (setq name (format "+%s" name))))
    (let ((new-file-p
	   (not
	    (file-exists-p (format "%s%s" mh-user-path (substring name 1))))))
      (cond ((and new-file-p
		  (y-or-n-p
		   (format "Folder %s does not exist. Create it? " name)))
	     (message "Creating %s" name)
	     (call-process "mkdir" nil nil nil
			   (format "%s%s" mh-user-path (substring name 1)))
	     (message "Creating %s...done" name)
	     (push (list name) mh-folder-list)
	     (push (list (substring name 1 nil)) mh-folder-list))
	    (new-file-p
	     (error ""))
	    (t
	     (when (null (assoc name mh-folder-list))
	       (push (list name) mh-folder-list)
	       (push (list (substring name 1 nil)) mh-folder-list)))))
    name))


(defun mh-make-folder-list ()
  "Return a list of the user's folders.
Result is in a form suitable for completing read."
  (interactive)
  (save-window-excursion
    (mh-exec-cmd-quiet " *mh-temp*" "folders" "-fast" "-norecurse")
    (goto-char (point-min))
    (let ((list nil))
      (while (not (eobp))
	(let ((start (point)))
	  (search-forward "\n" nil t)
	  (let ((folder (buffer-substring start (- (point) 1))))
	    (push (list (format "+%s" folder)) list))))
      list)))


(defun mh-remove-folder-from-folder-list (folder)
  "Remove FOLDER from the list of folders."
  (setq mh-folder-list
	(delq (assoc (substring folder 1 nil) mh-folder-list)
	      mh-folder-list)))



;;; Misc. functions.

(defun mh-get-msg-num (error-if-no-message)
  "Return the message number of the displayed message.  If the argument
ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is not
pointing to a message."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^[ ]*\\([0-9]+\\)")
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-search-pat (n)
  "Return a search pattern for message N in the scan listing."
  (cond ((< n 10) (format "^[^0-9][^0-9][^0-9]%d" n))
	((< n 100) (format "^[^0-9][^0-9]%d" n))
	((< n 1000) (format "^[^0-9]%d" n))
	(t (format "^%d" n))))


(defun mh-msg-filename (msg)
  "Returns a string containing the file name of the MESSAGE."
  (format "%s%d" mh-folder-filename msg))


(defun mh-msg-filenames (msgs folder)
  "Return a string of filenames for MSGS in FOLDER."
  (mapconcat (function (lambda (msg) (concat folder msg))) msgs " "))



(defun mh-find-path ()
   "Set mh-user-path to the user's Mail directory from  ~/.mh_profile."
   (if (equal (setq mh-user-path (mh-get-profile-field "Path:")) "")
       (setq mh-user-path "Mail/")
       (setq mh-user-path (format "%s/" mh-user-path)))
   (if (not (equal (substring mh-user-path 0 1) "/"))
       (setq mh-user-path (format "%s/%s" (getenv "HOME") mh-user-path))))


(defun mh-get-profile-field (field)
  "Return FIELD from the user's .mh_profile file."
  (save-window-excursion
    (if (not (file-exists-p "~/.mh_profile"))
	(error "Cannot find ~/.mh_profile file.  Run mh-install!"))
    (switch-to-buffer " *mh_temp*")	; Create if necessary
    (delete-region (point-min) (point-max))
    (insert-file-contents (expand-file-name "~/.mh_profile"))
    (mh-get-field field)))


(defun mh-get-cur-msg (folder)
  "Return the number of the 'cur' message in FOLDER."
  (save-excursion
    (set-buffer " *mh_temp*")
    (delete-region (point-min) (point-max))
    (mh-exec-cmd-output "pick" nil folder "cur")
    (string-to-int (buffer-substring (point-min) (point-max)))))


(defun mh-get-field (field)
  "Find and return the value of field FIELD in the current buffer.
Returns the empty string if the field is not in the message."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (search-forward field nil t)) "")
	  ((looking-at "[\t ]*$") "")
	  (t
	   (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
	   (let ((field (buffer-substring (match-beginning 1)
					  (match-end 1)))
		 (end-of-match (point)))
	     (forward-line)
	     (while (looking-at "[ \t]") (forward-line 1))
	     (backward-char 1)
	     (format "%s%s" field (buffer-substring end-of-match (point))))))))


(defun mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUE pairs in the current buffer.
Do not insert any pairs whose value is the empty string."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (cadr name-values)))
	(when (not (equal value ""))
	  (goto-char (point-min))
	  (cond ((not (re-search-forward (format "^%s" field-name) nil t))
		 (mh-goto-header-end 0)
		 (insert field-name " " value "\n"))
		(t
		 (end-of-line)
		 (insert " " value))))
	(setq name-values (cddr name-values))))))


(defun mh-position-on-field (field set-mark)
  "Set point to the end of the line beginning with FIELD.
Set the mark to the old value of point, if SET-MARK is non-nil."
  (if set-mark (push-mark))
  (goto-char (point-min))
  (mh-goto-header-end 0)
  (if (re-search-backward (format "^%s" field) nil t)
      (progn (end-of-line) t)
      nil))


(defun mh-goto-header-end (arg)
  "Find the end of the message header in the current buffer and position
the cursor at the ARG'th newline after the header."
  (if (re-search-forward "^$\\|^-+$" nil nil)
      (forward-line arg)))



;;; Build the folder-mode keymap:

(define-key mh-folder-mode-map "?" 'mh-msg-is-in-seq)
(define-key mh-folder-mode-map "%" 'mh-put-msg-in-seq)
(define-key mh-folder-mode-map "\e%" 'mh-delete-msg-from-seq)
(define-key mh-folder-mode-map "\C-Xn" 'mh-narrow-to-seq)
(define-key mh-folder-mode-map "\C-Xw" 'mh-widen)
(define-key mh-folder-mode-map "\eb" 'mh-burst-digest)
(define-key mh-folder-mode-map "\eu" 'mh-undo-folder)
(define-key mh-folder-mode-map "\e " 'mh-page-digest)
(define-key mh-folder-mode-map "\e\177" 'mh-page-digest-backwards)
(define-key mh-folder-mode-map "\ee" 'mh-extract-rejected-mail)
(define-key mh-folder-mode-map "\ef" 'mh-visit-folder)
(define-key mh-folder-mode-map "\ek" 'mh-kill-folder)
(define-key mh-folder-mode-map "\el" 'mh-list-folders)
(define-key mh-folder-mode-map "\ep" 'mh-renumber-folder)
(define-key mh-folder-mode-map "\es" 'mh-search-folder)
(define-key mh-folder-mode-map "\er" 'mh-rescan-folder)
(define-key mh-folder-mode-map "l" 'mh-print-msg)
(define-key mh-folder-mode-map "t" 'mh-toggle-summarize)
(define-key mh-folder-mode-map "c" 'mh-copy-msg)
(define-key mh-folder-mode-map ">" 'mh-write-msg-to-file)
(define-key mh-folder-mode-map "i" 'mh-inc-folder)
(define-key mh-folder-mode-map "x" 'mh-execute-commands)
(define-key mh-folder-mode-map "e" 'mh-execute-commands)
(define-key mh-folder-mode-map "r" 'mh-redistribute)
(define-key mh-folder-mode-map "f" 'mh-forward)
(define-key mh-folder-mode-map "s" 'mh-send)
(define-key mh-folder-mode-map "a" 'mh-answer)
(define-key mh-folder-mode-map "g" 'mh-goto-msg)
(define-key mh-folder-mode-map "\177" 'mh-previous-page)
(define-key mh-folder-mode-map " " 'mh-page-msg)
(define-key mh-folder-mode-map "." 'mh-show)
(define-key mh-folder-mode-map "u" 'mh-undo)
(define-key mh-folder-mode-map "!" 'mh-move-or-write-again)
(define-key mh-folder-mode-map "^" 'mh-move-msg)
(define-key mh-folder-mode-map "d" 'mh-delete-msg)
(define-key mh-folder-mode-map "p" 'mh-previous-line)
(define-key mh-folder-mode-map "n" 'mh-next-line)


;;; Build the letter-mode keymap:

(define-key mh-letter-mode-map "\C-C\C-Fb" 'mh-to-field)
(define-key mh-letter-mode-map "\C-C\C-Fc" 'mh-to-field)
(define-key mh-letter-mode-map "\C-C\C-Ff" 'mh-to-field)
(define-key mh-letter-mode-map "\C-C\C-Fs" 'mh-to-field)
(define-key mh-letter-mode-map "\C-C\C-Ft" 'mh-to-field)
(define-key mh-letter-mode-map "\C-C\C-Q" 'mh-fully-kill-draft)
(define-key mh-letter-mode-map "\C-C\C-W" 'mh-check-whom)
(define-key mh-letter-mode-map "\C-C\C-I" 'mh-insert-letter)
(define-key mh-letter-mode-map "\C-C\C-Y" 'mh-insert-cur-msg)
(define-key mh-letter-mode-map "\C-C\C-S" 'mh-insert-signature)
(define-key mh-letter-mode-map "\C-C\C-C" 'mh-send-letter)


;;; Build the pick-mode keymap:

(define-key mh-pick-mode-map "\C-C\C-C" 'mh-do-pick-search)
(define-key mh-pick-mode-map "\C-C\C-Fb" 'mh-to-field)
(define-key mh-pick-mode-map "\C-C\C-Fc" 'mh-to-field)
(define-key mh-pick-mode-map "\C-C\C-Ff" 'mh-to-field)
(define-key mh-pick-mode-map "\C-C\C-Fs" 'mh-to-field)
(define-key mh-pick-mode-map "\C-C\C-Ft" 'mh-to-field)
(define-key mh-pick-mode-map "\C-C\C-W" 'mh-check-whom)

