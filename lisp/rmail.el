;; "RMAIL" mail reader for Emacs.
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

;; Souped up by shane@mit-ajax based on ideas of rlk@mit-clio
;;   New features include attribute and keyword support, message
;;   selection by dispatch table, summary by attributes and keywords,
;;   expunging by dispatch table, sticky options for file commands.

(require 'mail-utils)
(provide 'rmail)

;; Flush this when released, its in loaddefs.
(defvar rmail-spool-directory "/usr/spool/mail/"
  "This is the name of the directory used by the system mailer for\n\
delivering new mail.  It's name should end with a slash.")
  
; these variables now declared in loaddefs
;(defvar rmail-dont-reply-to-names
;  nil
;  "*A regexp specifying names to prune of reply to messages.
;nil means dont reply to yourself.")
;(defvar rmail-ignored-headers
;   "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^message-id:\\|^summary-line:"
;   "*Gubbish headers one would rather not see.")
;(defvar rmail-file-name
;  (expand-file-name "~/RMAIL")
;  "")
;(defvar rmail-inbox-list 
;  '("/usr/spool/mail/$USER" "~/mbox")
; "")
;(defvar rmail-make-summary-line-function 'rmail-make-stupid-summary-line
;  "*Compute text of summary line for RMAIL.
;Should return a string (not including a terminating newline)
;Called with region narrowed to headers of message to be summarized")
;(defvar rmail-mmdf-inbox-list '() ;("~/mailbox")
;  "*List of files which contain new mail stored in mmdf format")

;; these may be altered by site-init.el to match the format of mmdf files
;;  delimitation used on a given host (delim1 and delim2 from the config
;;  files)

(defvar mmdf-delim1 "^\001\001\001\001\n"
  "Regexp marking the start of an mmdf message")
(defvar mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message")

(defvar rmail-message-filter nil
  "If non nil, is a filter function for new headers in RMAIL.
Called with region narrowed to unformatted header.")

(defvar rmail-mode-map nil "")

(defvar rmail-buffer-save nil "")

;; Message counters and markers.  Deleted flags.

(defvar rmail-current-message nil "")
(defvar rmail-total-messages nil "")
(defvar rmail-message-vector nil "")
(defvar rmail-deleted-vector nil "")

(defvar rmail-edit-map (append text-mode-map nil) "")

;; These are used by autoloaded rmail-summary.

(defvar rmail-summary-buffer nil "")
(defvar rmail-summary-valid nil "")
(defvar rmail-summary-mode-map nil "")
(defvar rmail-last-summary-type nil "")

;; `Sticky' default variables.

(defvar rmail-last-label nil "")
(defvar rmail-last-file (expand-file-name "~/XMAIL") "")

;; Global to all RMAIL buffers.  It exists primarily for the sake of
;; completion.  It is better to use strings with the label functions
;; and let them worry about making the label.

(defvar rmail-label-obarray (make-vector 47 0) "")

;; Named list of symbols representing valid message attributes in RMAIL.

(defvar rmail-attributes
  (cons 'rmail-keywords
	(mapcar '(lambda (s) (intern s rmail-label-obarray))
		'("deleted" "answered" "filed"))))

(defvar rmail-deleted-label (intern "deleted" rmail-label-obarray))

;; Named list of symbols representing valid message keywords in RMAIL.

(defvar rmail-keywords nil)


;; *** Rmail Mode ***

(defun rmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by  rmail-file-name  (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with filename as argument;
then performs rmail editing on that file,
but does not copy any new mail into the file."
  (interactive)
  (let* ((last-buffer (buffer-name))
	 (file-name (expand-file-name (or file-name-arg rmail-file-name)))
	 (existed (get-file-buffer file-name)))
    (find-file file-name)
    (rmail-mode)
    (setq rmail-buffer-save last-buffer)
    (if existed
	nil
      (if (= 0 (buffer-size))
	  (rmail-insert-rmail-file-header)))
    (goto-char (dot-max))
    (if file-name-arg
	(progn
	  (rmail-set-message-counters)
	  (rmail-show-message))
      (rmail-get-new-mail))))

(if rmail-mode-map
    nil
  (setq rmail-mode-map (make-keymap))
  (suppress-keymap rmail-mode-map)
  (define-key rmail-mode-map "." 'beginning-of-buffer)
  (define-key rmail-mode-map " " 'scroll-up)
  (define-key rmail-mode-map "\177" 'scroll-down)
  (define-key rmail-mode-map "n" 'rmail-move-forward-undeleted-message)
  (define-key rmail-mode-map "p" 'rmail-move-backward-undeleted-message)
  (define-key rmail-mode-map "\en" 'rmail-move-forward-message)
  (define-key rmail-mode-map "\ep" 'rmail-move-backward-message)
  (define-key rmail-mode-map "a" 'rmail-add-label)
  (define-key rmail-mode-map "k" 'rmail-kill-label)
  (define-key rmail-mode-map "d" 'rmail-delete-forward)
  (define-key rmail-mode-map "u" 'rmail-undelete-previous-message)
  (define-key rmail-mode-map "\eu" 'rmail-undelete-message)
  (define-key rmail-mode-map "e" 'rmail-expunge)
  (define-key rmail-mode-map "s" 'save-buffer)
  (define-key rmail-mode-map "g" 'rmail-get-new-mail)
  (define-key rmail-mode-map "h" 'rmail-summary)
  (define-key rmail-mode-map "l" 'rmail-summary-by-label)
  (define-key rmail-mode-map "L" 'rmail-summary-by-labels)
  (define-key rmail-mode-map "t" 'rmail-toggle-header)
  (define-key rmail-mode-map "m" 'rmail-mail-other-window)
  (define-key rmail-mode-map "r" 'rmail-reply)
  (define-key rmail-mode-map "c" 'rmail-continue)
  (define-key rmail-mode-map "f" 'rmail-forward)
  (define-key rmail-mode-map "F" 'rmail-find)
  (define-key rmail-mode-map "j" 'rmail-show-message)
  (define-key rmail-mode-map "o" 'rmail-output-to-rmail-file)
  (define-key rmail-mode-map "\C-o" 'rmail-output)
  (define-key rmail-mode-map "i" 'rmail-input)
  (define-key rmail-mode-map "q" 'rmail-quit)
  (define-key rmail-mode-map ">" 'rmail-last-message)
  (define-key rmail-mode-map "?" 'describe-mode)
  (define-key rmail-mode-map "\C-r" 'rmail-edit-current-message)
  (define-key rmail-mode-map "\C-d" 'rmail-delete-backward))

(defun rmail-mode ()
  "Rmail Mode is used by \\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move dot to front of this message (same as \\[beginning-of-buffer]).
Space	Scroll to next screen of this message.
Delete  Scroll to previous screen of this message.
n	Move to Next non-deleted message.
p	Move to Previous non-deleted message.
M-n	Move to Next message whether deleted or not.
M-p	Move to Previous message whether deleted or not.
F	Search for string and show message it is found in.
j	Jump to message specified by numeric position in file.
d	Delete this message, move to next nondeleted.
C-d	Delete this message, move to previous nondeleted.
u	Undelete previous message.  M-u undelete this message.
e	Expunge deleted messages.
s	Save the file (same as \\[save-buffer]).
g	Move new mail from system spool directory or mbox into this file.
m	Mail a message (same as \\[mail-other-window]).
c	Continue composing outgoing message started before.
r	Reply to this message.  Like m but initializes some fields.
f	Forward this message to another user.
o       Output this message to an RMAIL file (append it).
C-o	Output this message to a file (append it).
i	Input RMAIL file.  Run RMAIL on that file.
q       Quit out of RMAIL and save RMAIL file.
a	Add attribute to message.  It will be displayed in the mode line.
k	Kill attribute.  Remove an attribute from message.
h	Show headers buffer, with a one line summary of each message.
l	Like h only just messages with a particular label are summarized.
L	Like l only you can summarize by multiple labels.
t	Toggle header, show rmail header if unformatted or vice versa.
C-r	Edit the current message.  C-c to return to RMAIL.
>	Move to the last message in RMAIL file."
  (interactive)
  (rmail-variables)
  (rmail-mode-1)
  (and (boundp 'rmail-mode-hook)
       rmail-mode-hook
       (funcall rmail-mode-hook)))

(defun rmail-variables ()
  (kill-all-local-variables)
  (make-local-variable 'rmail-last-label)
  (make-local-variable 'rmail-inbox-list)
  (make-local-variable 'rmail-deleted-vector)
  (make-local-variable 'rmail-keywords)
  (make-local-variable 'rmail-summary-buffer)
  (make-local-variable 'rmail-summary-valid)
  (make-local-variable 'rmail-last-summary-type)
  (make-local-variable 'rmail-current-message)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'rmail-buffer-save)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (make-local-variable 'rmail-message-vector)
  (make-local-variable 'rmail-last-file))

(defun rmail-mode-1 ()
  (setq major-mode 'rmail-mode)
  (setq mode-name "RMAIL")
  (setq buffer-read-only t)
  (setq mode-line-format "--- Emacs: %b  %M  %[(%m)%] ----%3p-%-")
  (use-local-map rmail-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table))

(defun rmail-quit ()
  "Quit out of RMAIL."
  (interactive)
  (save-buffer)
  (bury-buffer (current-buffer))
  (switch-to-buffer rmail-buffer-save))

(defun rmail-input (filename)
  "Run RMAIL on FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  (rmail filename))


;; *** Rmail input ***

(defun rmail-get-new-mail ()
  "Move any new mail from the files specified by  rmail-inbox-list and
rmail-mmdf-inbox-list  into this RMAIL file."
  (interactive)
  (widen)
  (let ((odot (dot))
	(omax)
	(new-messages 0)
	(delete-files ())
	(buffer-read-only nil))
    (goto-char (dot-max))
    (skip-chars-backward " \t\n")	; just in case of brain damage
    (setq omax (dot-max))		; caused by require-final-newline
    (unwind-protect
	(save-restriction
	  (narrow-to-region (dot) (dot))
	  (let ((tem (rmail-snarf-Mail-format-inbox rmail-inbox-list)))
	    (setq new-messages (+ new-messages (car tem))
		  delete-files (append (cdr tem) delete-files)))
	  (narrow-to-region (dot) (dot))
	  (let ((tem (rmail-snarf-mmdf-format-inbox rmail-mmdf-inbox-list)))
	    (setq new-messages (+ new-messages (car tem))
		  delete-files (append (cdr tem) delete-files)))
	  (save-buffer)
	  (while delete-files
	    (condition-case ()
		(delete-file (car delete-files))
	      (file-error nil))
	    (setq delete-files (cdr delete-files)))
	  (goto-char (dot-min))
	  (forward-line 1))
      (if (= new-messages 0)
	  (progn (goto-char odot)
		 (message "(No new mail has arrived)"))
	(rmail-forget-messages)
	(goto-char (1+ omax))
	(rmail-set-message-counters)
	(message "%d new message%s read"
		 new-messages (if (= 1 new-messages) "" "s")))
      (rmail-show-message))))

(defun rmail-insert-inbox-text (files)
  (let (file tofile delete-files)
    (while files
      (setq file (substitute-in-file-name (car files))
	    tofile (concat file "~"))
      (if (and (not (file-exists-p tofile)) (file-exists-p file))
	  (progn
	    (if (equal (file-name-directory file) rmail-spool-directory)
		(progn
		  (setq tofile (expand-file-name "~/.newmail"))
		  (call-process (expand-file-name "movemail" exec-directory)
				nil nil nil file tofile))
	      (rename-file file tofile t))))
      (if (file-exists-p tofile)
	  (progn (skip-chars-backward " \t\n")    ; chomper might have munged
		 (delete-region (dot) (dot-max))  ; require-final-newline
		 (insert-file-contents tofile)    ; in a hook or elsewhere.
		 (setq delete-files (cons tofile delete-files))))
      (setq files (cdr files)))
    delete-files))

(defun rmail-snarf-Mail-format-inbox (files)
  (let ((delete-files (rmail-insert-inbox-text files))
	(count 0) start
	(case-fold-search nil))
    (while (not (eobp))
      (insert "\^L\n0,,\n")
      (setq start (dot))
      (save-excursion
	(save-restriction
	  (let ((header-end (if (search-forward "\n\n" nil t)
				(dot)
			      (error "Can't find end of headers in Mail file.")))
		has-from has-date)
	    (setq case-fold-search t)
	    (narrow-to-region start header-end)
	    (goto-char start)
	    (setq has-from (search-forward "\nFrom:" nil t))
	    (goto-char start)
	    (setq has-date (and (search-forward "\nDate:" nil t) (dot)))
	    (cond ((and (not has-from) has-date)
		   ;; kill "date:" line if "from:" line missing
		   (goto-char has-date)
		   (beginning-of-line)
		   (delete-region (dot)
				  (progn (forward-line 2) (dot)))))
	    (goto-char start)
	    (setq case-fold-search nil)
	    (if (re-search-forward
		 "^From \\([^ ]*\\)\\( \\|  \\)\\([^ ]*\\) \\([^ ]*\\) *\\([0-9]*\\) \\([0-9:]*\\) 19\\([0-9]*\\)\n" nil t)
		(if has-from
		    (replace-match "")
		  ;;>> ** ``bug'' ** has "est" timezone built in
		  ;;>> (actually the bug is with cretinous unix
		  ;;>>  software by pinheads for pinheads)
		  (replace-match
		   "Date: \\3, \\5 \\4 \\7 \\6 est\nFrom: \\1\n"))))))
      ;;>> unix mailers seem to insist on putting a newlines onto the
      ;;>> end of messages which (even deliberately) don't have them.
      ;;>> Not much we can do about that from the rmail end of things.
      (if (search-forward "\n\nFrom " nil t)
	  ;;>> don't need this hair
	  ;; "\n\nFrom [^ ]*\\( \\|  \\)[^ ]* [^ ]* [ :0-9]*$"
	  (beginning-of-line)
	(goto-char (dot-max)))
      (setq count (1+ count))
      (save-excursion
        (save-restriction
	  (narrow-to-region start (dot))
	  (goto-char (dot-min))
	  (while (search-forward "\n" nil t)       ; single character ""
	    (replace-match "\n^_")))) 	             ; 2 characters: "^" and "_"
      (insert ?))
    (cons count delete-files)))

;;>> Not sure if this is completely accurate.
;;>> Please mail bug-gnu-emacs if you know!
;;>> As far as I can tell, mmdf files are a sequence of messages each of
;;>>  which starts with mmdf-delim1 and is terminated by mmdf-delim2
;;>> I do not know if it is valid for anything to occur between the end
;;>>  delimiter of one message and the start delimiter of the next ---
;;>>  the following assumes not.
;;>> Will anybody ever have the brains to write a transparent mail file 
;;>>  format on unix? --- note that it is impossible to include mmdf-delim*
;;>>  in the message (the mmdf mailer smashes the first char in any match
;;>>  of these to be a space, with -no- attempt at quoting) barf.

(defun rmail-snarf-mmdf-format-inbox (files)
  (let ((delete-files (rmail-insert-inbox-text files))
	(count 0) start end
	(case-fold-search nil))
    (while (not (eobp))
      (insert "\^L\n0,,\n")
      (setq start (dot))
      (or (looking-at mmdf-delim1)
	  (error "Didn't find mmdf delim1"))
      (replace-match "")
      (or (re-search-forward mmdf-delim2 nil t)
	  (error "Didn't find mmdf delim2"))
      (replace-match "\^_")
      (setq end (1- (dot)))
      (goto-char start)
      (or (search-forward "\n\n" nil t)
	  (error "Can't find end of mmdf headers"))
      (save-excursion
        (save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (while (search-forward "\n" nil t)    ; single character ""
	    (replace-match "\n^_"))))	          ; 2 characters: "^" and "_"
      (setq count (1+ count))
      (goto-char (1+ end)))
    (cons count delete-files)))


;; *** Rmail Message Formatting and Header Manipulation ***

(defun rmail-reformat-message (beg end)
  (goto-char beg)
  (forward-line 1)
  (if (/= (following-char) ?0)
      (error "Bad format in RMAIL file."))
  (let ((buffer-read-only nil)
	(delta (- (buffer-size) end)))
    (delete-char 1)
    (insert ?1)
    (forward-line 1)
    (let ((str (buffer-substring (dot)
				 (progn (search-forward "\n\n" end 'move)
					(dot)))))
      (insert "*** EOOH ***\n")
      (narrow-to-region (dot) (- (buffer-size) delta))
      (insert str))
    (goto-char (dot-min))
    (if rmail-ignored-headers (rmail-clear-headers))
    (if rmail-message-filter (funcall rmail-message-filter))))

(defun rmail-clear-headers ()
  (if (search-forward "\n\n" nil t)
      (save-restriction
        (narrow-to-region (dot-min) (dot))
	(let ((buffer-read-only nil))
	  (while (let ((case-fold-search t))
		   (goto-char (dot-min))
		   (re-search-forward rmail-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (dot)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (dot))))))))

(defun rmail-toggle-header ()
  "Show original header if RMAIL header shown or vice versa."
  (interactive)
  (rmail-maybe-set-message-counters)
  (let ((temp (dot-min))
	(nmin (- (buffer-size) (dot-min)))
	(nmax (- (buffer-size) (dot-max)))
	(beg (rmail-msgbeg rmail-current-message))
	(end (rmail-msgend rmail-current-message))
	(buffer-read-only nil))
    (unwind-protect
	(progn (widen)
	       (goto-char beg)
	       (forward-line 1)
	       (if (= (following-char) ?1)
		   (progn (delete-char 1)
			  (insert ?0)
			  (goto-char temp)
			  (delete-region (progn (forward-line -1) (dot)) temp)
			  (setq temp (dot))
			  (let ((eoh (and (search-forward "\n\n" end t) (dot))))
			    (if eoh (kill-region temp eoh)))
			  (goto-char beg)
			  (forward-line 2)
			  (if (looking-at "Summary-Line:")
			      (forward-line 1))
			  (setq nmin (- (buffer-size) (dot))))
		 (rmail-reformat-message beg end)
		 (setq nmin (- (buffer-size) (dot-min)))))
      (widen)
      (narrow-to-region (- (buffer-size) nmin) (- (buffer-size) nmax)))))


;; *** Rmail Output ***

(defun rmail-insert-rmail-file-header ()
  (let ((buffer-read-only nil))
    (insert "Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.\n")))

(defun rmail-output-to-rmail-file (filename)
  "Append the current message to an RMAIL file. If the file does not
exist, ask if it should be created."
  (interactive (list (rmail-read-filename "Append message to RMAIL file ")))
  (rmail-maybe-set-message-counters)
  (save-excursion
    (if (not (file-exists-p filename))
	(if (yes-or-no-p
	     (concat "\"" filename "\" does not exist, create it? "))
	    (let ((file-buffer (create-file-buffer filename)))
	      (save-excursion
		(set-buffer file-buffer)
		(rmail-insert-rmail-file-header)
		(let ((require-final-newline nil))
		  (write-region (dot-min) (dot-max) filename t 1)))
	      (kill-buffer file-buffer))))
    (if (file-exists-p filename)
	(progn
	  (save-restriction
	    (widen)
	    (append-to-file (1+ (rmail-msgbeg rmail-current-message))
			    (1+ (rmail-msgend rmail-current-message))
			    filename))
	  (rmail-set-label "filed" t)))))
	    
(defun rmail-output (filename)
  "Append this message to FILE."
  (interactive (list (rmail-read-filename "Append message to file ")))
  (append-to-file (dot-min) (dot-max) filename)
  (rmail-set-label "filed" t))

(defun rmail-read-filename (prompt)
  (setq rmail-last-file
	(expand-file-name
	 (read-file-name (concat prompt
				 "("
				 (file-name-nondirectory rmail-last-file)
				 "): ")
			 (file-name-directory rmail-last-file)
			 rmail-last-file))))

;; *** Rmail Attributes and Keywords ***

(defun rmail-add-label (string)
  "Add STRING to labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  (interactive (list (rmail-read-label "Add label")))
  (rmail-set-label string t))

(defun rmail-kill-label (string)
  "Remove STRING from labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  (interactive (list (rmail-read-label "Remove label")))
  (rmail-set-label string nil))

(defun rmail-read-label (prompt)
  (if (not rmail-keywords) (rmail-parse-file-keywords))
  (let ((result
	 (completing-read (concat prompt
				  (if rmail-last-label
				      (concat " (default "
					      (symbol-name rmail-last-label)
					      "): ")
				    ": "))
			  rmail-label-obarray
			  nil
			  nil)))
    (if (string= result "")
	rmail-last-label
      (setq rmail-last-label (rmail-make-label result t)))))

(defun rmail-display-labels ()
  (let ((blurb ""))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (rmail-msgbeg rmail-current-message))
	(forward-line 1)
	(if (looking-at "[01],")
	    (progn
	      (narrow-to-region (dot) (progn (end-of-line) (dot)))
	      (if (search-backward ",," nil 'move)
		  (progn
		    (if (> (dot) (1+ (dot-min)))
			(setq blurb (buffer-substring (1+ (dot-min)) (dot))))
		    (if (> (- (dot-max) (dot)) 2)
			(setq blurb
			      (concat blurb
				      ";"
				      (buffer-substring (+ (dot) 2)
							(1- (dot-max))))))))))))
    (setq minor-modes
	  (list (cons 'foo
		      (concat rmail-current-message "/" rmail-total-messages
			      blurb))))))

;; Commented functions aren't used by RMAIL but might be nice for user
;; packages that do stuff with RMAIL.  Note that rmail-message-labels-p
;; is in rmailsum now.

;(defun rmail-message-attribute-p (attribute &optional n)
;  "Returns t if ATTRIBUTE on NTH or current message."
;  (rmail-message-labels-p (rmail-make-label attribute t) n))

;(defun rmail-message-keyword-p (keyword &optional n)
;  "Returns t if KEYWORD on NTH or current message."
;  (rmail-message-labels-p (rmail-make-label keyword t) n t))

;(defun rmail-message-label-p (label &optional n)
;  "Returns symbol if LABEL (attribute or keyword) on NTH or current message."
;  (rmail-message-labels-p (rmail-make-label label t) n 'all))

(defun rmail-attribute-p (s)
  (let ((symbol (rmail-make-label s)))
    (if (memq symbol (cdr rmail-attributes)) symbol)))

(defun rmail-keyword-p (s)
  (let ((symbol (rmail-make-label s)))
    (if (memq symbol (cdr (rmail-keywords))) symbol)))

(defun rmail-make-label (s &optional forcep)
  (cond ((symbolp s) s)
	(forcep (intern (downcase s) rmail-label-obarray))
	(t  (intern-soft (downcase s) rmail-label-obarray))))

(defun rmail-quote-label-name (label)
  (regexp-quote (symbol-name (rmail-make-label label t))))

(defun rmail-parse-labels ()
  (let ((keep-going t)
	(accumulated)
	(beg))
    (while (and keep-going (not (eobp)))
      (forward-char 1)
      (if (setq keep-going (and (/= (following-char) ?,) (not (eobp))))
	  (progn (setq beg (dot))
		 (skip-chars-forward "^,")
		 (setq accumulated
		       (cons (rmail-make-label (buffer-substring beg (dot)) t)
			     accumulated)))))
    accumulated))

(defun rmail-nuke-whitespace ()
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (dot-min))
      (while (re-search-forward "[ \t]" nil t)
	(replace-match "")))))

;; Not used by RMAIL but might be nice for user package.

;(defun rmail-parse-message-labels (&optional n)
;  "Returns labels associated with NTH or current RMAIL message.
;Results is a list of two lists.  The first is the message attributes
;and the second is the message keywords.  Labels are represented as symbols."
;  (let ((omin (- (buffer-size) (dot-min)))
;	(omax (- (buffer-size) (dot-max)))
;	(result))	
;    (unwind-protect
;	(save-excursion
;	  (let ((beg (rmail-msgbeg (or n rmail-current-message))))
;	    (widen)
;	    (goto-char beg)
;	    (forward-line 1)
;	    (if (looking-at "[01],")
;		(save-restriction
;		  (narrow-to-region (dot) (save-excursion (end-of-line) (dot)))
;		  (rmail-nuke-whitespace)
;		  (goto-char (1+ (dot-min)))
;		  (list (rmail-parse-labels) (rmail-parse-labels))))))
;      (narrow-to-region (- (buffer-size) omin)
;    			 (- (buffer-size) omax))
;      nil)))

(defun rmail-keywords ()
  (or rmail-keywords (rmail-parse-file-keywords)))

(defun rmail-parse-file-keywords ()
  (let ((omin (- (buffer-size) (dot-min)))
	(omax (- (buffer-size) (dot-max))))
    (unwind-protect
	(save-excursion
	  (widen)
	  (setq rmail-keywords nil)
	  (goto-char 1)
	  (if (not (search-forward "\nLabels:" (rmail-msgbeg 1) t))
	      (let ((buffer-read-only nil))
		(goto-char 1)
		(forward-line 1)
		(insert "Labels:\n"))
	    (save-restriction
	      (narrow-to-region (1- (dot)) (save-excursion (end-of-line) (dot)))
	      (rmail-nuke-whitespace)
	      (goto-char (dot-min))
	      (setq rmail-keywords (rmail-parse-labels))
	      (setq rmail-keywords (cons 'rmail-keywords rmail-keywords)))))
      (narrow-to-region (- (buffer-size) omin)
			(- (buffer-size) omax))
      rmail-keywords)))
      
(defun rmail-install-keyword (word)
  (let ((keyword (rmail-make-label word t))
	(keywords (rmail-keywords)))
    (if (not (or (rmail-attribute-p keyword)
		 (rmail-keyword-p keyword)))
	(let ((omin (- (buffer-size) (dot-min)))
	      (omax (- (buffer-size) (dot-max))))
	  (unwind-protect
	      (save-excursion
		(widen)
		(goto-char 1)
		(let ((case-fold-search t)
		      (buffer-read-only nil))
		  (search-forward "\nLabels:")
		  (delete-region (dot) (progn (end-of-line) (dot)))
		  (setcdr keywords (cons keyword (cdr keywords)))
		  (while (setq keywords (cdr keywords))
		    (insert (symbol-name (car keywords)) ","))
		  (delete-char -1)))
	    (narrow-to-region (- (buffer-size) omin)
			      (- (buffer-size) omax)))))
    keyword))

(defun rmail-set-label (l state &optional n)
  (rmail-maybe-set-message-counters)
  (if (not n) (setq n rmail-current-message))
  (let* ((attribute (rmail-attribute-p l))
	 (keyword (and (not attribute)
		       (or (rmail-keyword-p l)
			   (rmail-install-keyword l))))
	 (label (or attribute keyword)))
    (if label
	(let ((omax (- (buffer-size) (dot-max)))
	      (omin (- (buffer-size) (dot-min)))
	      (buffer-read-only nil)
	      (case-fold-search t))
	  (unwind-protect
	      (save-excursion
		(widen)
		(goto-char (rmail-msgbeg n))
		(forward-line 1)
		(if (not (looking-at "[01],"))
		    nil
		  (let ((start (1+ (dot)))
			(bound))
		    (narrow-to-region (dot) (progn (end-of-line) (dot)))
		    (setq bound (dot-max))
		    (search-backward ",," nil t)
		    (if attribute
			(setq bound (1+ (dot)))
		      (setq start (1+ (dot))))
		    (goto-char start)
		    (rmail-nuke-whitespace)
		    (if (re-search-forward
			   (concat "," (rmail-quote-label-name label) ",")
			   bound
			   'move)
			(if (not state) (replace-match ","))
		      (if state (insert (symbol-name label) ",")))
		    (if (eq label rmail-deleted-label)
			(rmail-set-message-deleted-p n state)))))
	    (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax))
	    (if (= n rmail-current-message) (rmail-display-labels)))))))	

;; *** Rmail Message Selection And Support ***

(defun rmail-msgend (n)
  (marker-position (aref rmail-message-vector (1+ n))))

(defun rmail-msgbeg (n)
  (marker-position (aref rmail-message-vector n)))

(defun rmail-forget-messages ()
  (unwind-protect
      (if (vectorp rmail-message-vector)
	  (let* ((i 0)
		 (v rmail-message-vector)
		 (n (length v)))
	    (while (< i n)
	      (move-marker (aref v i)  nil)
	      (setq i (1+ i)))
	    (rmail-forget-summary)))
    (setq rmail-message-vector nil)
    (setq rmail-deleted-vector nil)))

(defun rmail-maybe-set-message-counters ()
  (if (not (and rmail-deleted-vector
		rmail-message-vector
		rmail-current-message
		rmail-total-messages))
      (rmail-set-message-counters)))

(defun rmail-set-message-counters ()
  (rmail-forget-messages)
  (message "Counting messages...")
  (save-restriction
    (save-excursion
      (widen)
      (let* ((dot-save (dot))
	     (total-messages 0)
	     (messages-head (cons (progn (goto-char (dot-min)) (dot-marker)) nil))
	     (messages-tail messages-head)
	     (deleted-head (cons ?D nil))
	     (deleted-tail deleted-head))
	(rmail-set-message-counters-counter dot-save)
	(setq rmail-current-message (max 1 total-messages))
	(rmail-set-message-counters-counter)
	(setq rmail-total-messages total-messages)
	(search-forward "\n" nil t)
	(forward-char -1)
	(setq messages-tail (setcdr messages-tail (cons (dot-marker) nil))
	      rmail-message-vector (apply 'vector messages-head)
	      rmail-deleted-vector (concat deleted-head)))))
  (message "Counting messages...done"))
	
(defun rmail-set-message-counters-counter (&optional stop)
  (while (search-forward "\n\^L" stop t)
    (forward-char -2)
    (setq messages-tail (setcdr messages-tail (cons (dot-marker) nil)))
    (forward-char 1)
    (setq deleted-tail 
	  (setcdr deleted-tail
		  (if (save-excursion
			(forward-line 1)
			(let ((start (dot)))
			  (end-of-line)
			  (search-backward ",deleted," start t)))
		      (cons ?D nil)
		    (cons ?\ nil))))
    (if (zerop (% (setq total-messages (1+ total-messages)) 10))
	(message "Counting messages...%d" total-messages))))

(defun rmail-show-message (&optional n)
  "Show message number N (prefix argument), counting from start of file."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (widen)
  (if (zerop rmail-total-messages)
      (progn (ding)
	     (narrow-to-region (dot-min) (1- (dot-max)))
	     (setq minor-modes nil))
    (let (blurb)
      (if (not n)
	  (setq n rmail-current-message)
	(cond ((<= n 0)
	       (setq n 1
		     rmail-current-message 1
		     blurb "No previous message"))
	      ((> n rmail-total-messages)
	       (setq n rmail-total-messages
		     rmail-current-message rmail-total-messages
		     blurb "No following message"))
	      (t
	       (setq rmail-current-message n))))
      (let ((beg (rmail-msgbeg n))
	    (end (rmail-msgend n)))
	(goto-char beg)
	(if (search-forward "\n*** EOOH ***\n" end t)
	    (narrow-to-region (dot) end)
	  (rmail-reformat-message beg end))
	(goto-char (dot-min))
	(rmail-display-labels)
	(if blurb
	    (message blurb))))
    (goto-char (dot-min))
    (rmail-display-labels)))

(defun rmail-move-forward-message (n)
  "Show following message whether deleted or not.
With prefix argument N, moves forward N messages
or backward if N is negative."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (rmail-show-message (+ rmail-current-message n)))

(defun rmail-move-backward-message (n)
  "Show previous message whether deleted or not.
With prefix argument N, moves backward N messages
or forward if N is negative."
  (interactive "p")
  (rmail-move-forward-message (- n)))  

(defun rmail-move-forward-undeleted-message (n)
  "Show following non-deleted message.
With prefix argument N, moves forward N non-deleted messages
or backward if N is negative."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (let ((lastwin rmail-current-message)
	(current rmail-current-message))
    (while (and (> n 0) (< current rmail-total-messages))
      (setq current (1+ current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1- n))))
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1+ n))))
    (rmail-show-message lastwin)
    (if (< n 0)
	(message "No previous nondeleted message"))
    (if (> n 0)
	(message "No following nondeleted message"))))

(defun rmail-move-backward-undeleted-message (n)
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages
or forward if N is negative."
  (interactive "p")
  (rmail-move-forward-undeleted-message (- n)))

(defun rmail-last-message ()
  "Show last message in file."
  (interactive)
  (rmail-maybe-set-message-counters)
  (rmail-show-message rmail-total-messages))

(defun rmail-what-message ()
  (let ((where (dot))
	(low 1)
	(high rmail-total-messages)
	(mid (/ rmail-total-messages 2)))
    (while (> (- high low) 1)
      (if (>= where (rmail-msgbeg mid))
	  (setq low mid)
	(setq high mid))
      (setq mid (+ low (/ (- high low) 2))))
    (if (>= where (rmail-msgbeg high)) high low)))

(defun rmail-find (string)
  "Show message containing next match for STRING (after dot)."
  (interactive "sFind string: ")
  (rmail-select-message 'search-forward string))

(defun rmail-select-message (function &rest args)
  (rmail-maybe-set-message-counters)
  (let ((omin (dot-min))
	(omax (dot-max))
	(odot (dot))
	win)
    (unwind-protect
         (progn
	   (widen)
	   (apply function args)
	   (setq win t))
      (if win
	  (rmail-show-message (rmail-what-message))
	(goto-char odot)
	(narrow-to-region omin omax)))))

;; *** Rmail Message Deletion Commands ***

(defun rmail-message-deleted-p (n)
  (= (aref rmail-deleted-vector n) ?D))

(defun rmail-set-message-deleted-p (n state)
  (aset rmail-deleted-vector n (if state ?D ?\ )))

(defun rmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (rmail-set-label "deleted" t))

(defun rmail-undelete-message ()
  "Remove deletion mark from this message."
  (interactive)
  (rmail-set-label "deleted" nil))

(defun rmail-undelete-previous-message ()
  "Remove deletion mark from previous message, and select it."
  (interactive)
  (rmail-move-forward-message -1)
  (rmail-undelete-message))

(defun rmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward."
  (interactive "P")
  (rmail-set-label "deleted" t)
  (rmail-move-forward-undeleted-message (if backward -1 1)))

(defun rmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-delete-forward t))

(defun rmail-expunge ()
  "Actually erase all deleted messages in the file."
  (interactive)
  (message "Expunging deleted messages...")
  (rmail-maybe-set-message-counters)
  (rmail-forget-summary)
  (let* ((omax (- (buffer-size) (dot-max)))
	 (omin (- (buffer-size) (dot-min)))
	 (messages-head (cons (aref rmail-message-vector 0) nil))
	 (messages-tail messages-head)
	 (win))
    (unwind-protect
	(save-excursion
	  (widen)
	  (goto-char (dot-min))
	  (let ((counter 0)
		(number 1)
		(total rmail-total-messages)
		(new-message-number rmail-current-message)
		(buffer-read-only nil)
		(messages rmail-message-vector)
		(deleted rmail-deleted-vector))
	    (setq rmail-total-messages nil
		  rmail-current-message nil
		  rmail-message-vector nil
		  rmail-deleted-vector nil)
	    (while (<= number total)
	      (if (= (aref deleted number) ?D)
		  (progn
		    (delete-region
		     (marker-position (aref messages number))
		     (marker-position (aref messages (1+ number))))
		    (move-marker (aref messages number) nil)
		    (if (> new-message-number counter)
			(setq new-message-number (1- new-message-number))))
		(setq messages-tail
		      (setcdr messages-tail
			      (cons (aref messages number) nil)))
		(setq counter (1+ counter)))
	      (if (zerop (% (setq number (1+ number)) 10))
		  (message "Expunging deleted messages...%d" number)))
	    (setq messages-tail
		  (setcdr messages-tail
			  (cons (aref messages number) nil)))
	    (setq rmail-current-message new-message-number
		  rmail-total-messages counter
		  rmail-message-vector (apply 'vector messages-head)
		  rmail-deleted-vector (make-string (1+ counter) ?\ )
		  win t)))
      (message "Expunging deleted messages...done")
      (if (not win)
	  (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax)))
      (rmail-show-message))))


;; *** Rmail Mailing Commands ***

(defun rmail-mail-other-window ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (mail-other-window nil nil nil nil nil (current-buffer)))

(defun rmail-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (mail-other-window t))

(defun rmail-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  (interactive "P")
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-label "answered" t)
  (rmail-display-labels)
  (let (from reply-to cc subject date to)
    (save-restriction
      (widen)
      (search-backward "\n")
      (narrow-to-region (dot) (progn (search-forward "\n*** EOOH ***\n")
				     (beginning-of-line) (dot)))
      (setq from (mail-fetch-field "from")
	    reply-to (or (mail-fetch-field "reply-to") from)
	    cc (if (not just-sender) (mail-fetch-field "cc"))
	    subject (mail-fetch-field "subject")
	    date (mail-fetch-field "date")
	    to (or (mail-fetch-field "to")
		   ; (mail-fetch-field "apparently-to") ack gag barf
		   "")))
    (mail-other-window nil
      (mail-strip-quoted-names reply-to)
      subject
      (let ((stop-pos (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
	(concat (if stop-pos (substring from 0 stop-pos) from)
		"'s message of " date))
      (if just-sender
	  nil
	(let* ((cc-list (rmail-dont-reply-to
			  (mail-strip-quoted-names
			    (if (null cc) to (concat to ", " cc))))))
	  (if (string= cc-list "") nil cc-list)))
      (current-buffer))))

(defun rmail-forward ()
  "Forward the current message to another user."
  (interactive)
  (let ((forward-buffer (current-buffer)))
    (if (mail-other-window)
	(save-excursion
	  (goto-char (dot-max))
	  (forward-line 1)
	  (insert-buffer forward-buffer)))))


;; *** Rmail Edit Mode ***

(define-key rmail-edit-map "\C-c" 'rmail-cease-edit)
(define-key rmail-edit-map "\C-]" 'rmail-abort-edit)

(if (not (fboundp 'rmail-edit-current-message))
    (autoload 'rmail-edit-current-message "rmailedit"
	      "Edit the contents of the current message"
	      t))

;; *** Rmail Summary Mode ***

(defun rmail-forget-summary ()
  (setq rmail-summary-valid nil))

(if (not (fboundp 'rmail-summary))
    (progn
      (autoload 'rmail-summary
		"rmailsum"
		"Display a summary of all messages, one line per message."
		t)
      (autoload 'rmail-summary-by-label "rmailsum"
		"Display a summary of messages with LABEL, one line per message."
		t)
      (autoload 'rmail-summary-by-labels "rmailsum"
		"Display a summary of messages with LABELS, one line per message.
Enter labels separately.  Terminate input with empty string."
		t)))




