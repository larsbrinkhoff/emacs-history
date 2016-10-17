;; vm-frame.el: Support for FSF GNU Emacs 19 Frames for VM (View Mail)
;;
;; Copyright (C) 1993  Paul D. Smith <psmith@wellfleet.com>
;;
;; This package is written for VM version 5.32 or 5.33: other versions
;; may or may not work.
;;
;; VM (View Mail) is a very cool mail handling package for GNU Emacs.
;; VM is written and maintained by Kyle E. Jones <kyle@wonderworks.com>.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You didn't receive a copy of the GNU General Public License along
;; with this program; so, write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; LCD Archive Entry:
;;; vm-frame|Paul D. Smith|psmith@wellfleet.com|
;;; Support for multiple FSF GNU Emacs 19 frames with VM and VM-mail.|
;;; $Date: 1993/07/13 23:28:42 $|$Revision: 1.3 $||
;;
;; HISTORY:
;;   1.03 - 13 Jul 1993 psmith
;;   1.02 - 07 Jul 1993 psmith
;;   1.01 - 24 Jun 1993 psmith
;;   1.00:  Created: 22 June 1993 by Paul D. Smith <psmith@wellfleet.com>
;;
;;;----------------------------------------------------------------------------
;;
;; INSTALLATION:
;;
;;   Add this to your .emacs:
;;
;;     (require 'vm-frame)
;;
;;   and put this elisp file somewhere on your load-path.
;;
;;   NOTE: If you're using (setq mail-setup-hook ...) to insert personal
;;   hooks into mail-setup-hook instead of (add-hook 'mail-setup-hook ...),
;;   you must either change your .emacs to use add-hook (yay!) or add a
;;   call to (vm-frame-mail-setup) to your personal mail-setup-hook
;;   function (boo!)
;;
;;;----------------------------------------------------------------------------
;;
;; USAGE:
;;
;; Reading Mail:
;; -------------
;;
;;  Just use VM normally.  When you first invoke VM, a new Emacs frame
;;  will appear to hold it.  There's one frame for all of VM; visit
;;  folders, etc. in that buffer.
;;
;;  Don't quit VM when you're done, iconify the frame instead.  Only
;;  quit VM when you want to quit your Emacs.  Note you might want to
;;  get into the habit of saving your INBOX occasionally, just in case.
;;
;;  When you get new mail or want to use VM again, you can enter VM as
;;  before (M-x vm or whatever); if the VM frame already exists it
;;  will be made visible and raised to the top, and the primary inbox
;;  will be visited.
;;
;;  Or, if you like, you can bind 'vm-frame to a key; this function
;;  un-iconfies and raises the VM frame if it exists, but doesn't visit
;;  the primary inbox; if the VM frame doesn't exist it creates it and
;;  invokes VM, just like running M-x vm.  This is what I use (I bound
;;  this to F4).
;;
;;  Quitting out of the INBOX folder causes the VM frame to be deleted.
;;
;; Mouse Support:
;; --------------
;;
;;  The following mouse commands are supported in VM Summary windows:
;;
;;      mouse-1:  Select the message clicked on
;;      mouse-3:  Toggle mark on the message clicked on
;;
;;  I'm actively searching for cool mouse keybindings; if you have a
;;  suggestion for more bindings for mouse keys, please let me know!
;;
;; Sending Mail:
;; -------------
;;
;;  When sending mail, use the normal VM commands for replies,
;;  followups, forwards, etc.  Use vm-mail to send original mail.
;;
;;  All mail messages invoked through VM will appear in their own
;;  frame.  The title of the frame will be the same as the name of the
;;  VM mail buffer.
;;
;;  To send the message, select "Send Message" from the "Mail" menu, or
;;  use C-c C-c.  This sends the message, buries the message buffer, and
;;  deletes the message frame.  Sending without deleting ("Send, Keep
;;  Editing" from the "Mail" menu, or C-c C-s) doesn't delete the frame.
;;
;;  To kill a message without sending it, select "Kill Message" from
;;  the "Mail" menu, or use C-x k.  This is usually kill-buffer; in
;;  VM-mail buffers it's rebound to confirm, then kill the buffer *and*
;;  delete the message frame.
;;
;;;----------------------------------------------------------------------------
;;
;; History/Acknowledgments:
;;
;;;----------------------------------------------------------------------------
;;
;; 1.03:
;;
;; * Fixed mail-mode stuff so it peacefully coexists with the new
;;   mail-mode menu bars in Emacs 19.16.
;;
;; * Make the primary inbox always be first in the folders list
;;
;; * Fencepost the fset calls so it's safe to load vm-frame multiple
;;   times (I hope...)
;;
;; * Mike Chace <mikec@praxis.co.uk> suggested displaying the primary
;;   inbox in the Visit menu even if it isn't in vm-folder-directory.
;;
;; 1.02:
;;
;; * Created some initial keybindings for mouse clicks, along with helper
;;   functions.
;;
;; * Michael Kutzner <futzi@uni-paderborn.de> suggested frame creation
;;   and printer command configurability variables.
;;
;; * Chris Moore <Chris.Moore@src.bae.co.uk> suggested using (require 'vm),
;;   a fix for vm-frame-mail-kill in non-VM mail buffers, and a request
;;   that (vm) visit the primary inbox, which led me to develop the
;;   vm-frame function as an interactive alternative to (vm).
;;
;; * abraham@research.att.com (Per Abrahamsen) posted a generic fix to
;;   the menu bar items beeping if you posted them without selecting
;;   anything; I added his fix into my menu bar items.
;;
;; 1.00: Created.
;;
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;
;;			    Customize these:
;;
;;;----------------------------------------------------------------------------

;; This is a list of Frame parameters used to create the VM frame.
;; Modify it to whatever you like; try something like:
;;
;;  (setq vm-frame-alist '((top . -1) (left . -1)))
;;
(defvar vm-frame-alist nil
  "Alist of frame parameters used for creating the VM frame.")


;; This is a list of Frame parameters for the newly created mail frames.
;; Modify it to whatever you like; try something like:
;;
;;  (setq vm-frame-mail-alist '((height . 40) (top . -1) (left . 0)))
;;
(defvar vm-frame-mail-alist nil
  "Alist of frame parameters used for creating Mail frames.")


;; Set this to nil if you don't want to be asked if you want to quit VM
;;
(defvar vm-frame-confirm-quit nil
  "If t, asks if you want to quit the VM frame.  If nil, doesn't ask.")


;; Control how/if new frames are created; if you want cool pull-down
;; menus but don't want cool new frames, set these appropriately in your
;; .emacs
;;
(defvar vm-frame-no-frame nil
  "If t, doesn't create a new frame for VM.  If nil, makes a new frame.")

(defvar vm-frame-no-mail-frame nil
  "If t, doesn't create a new frame for outgoing VM mail messages.
If nil, makes a new frame.")


;; How to print a mail message (note! only used by the "print" pull-down
;; menu)
;;
(defvar vm-frame-print-command lpr-command
  "The command used to print VM mail messages.")


;; A few hooks--since I'm bummed at Kyle for not adding any to VM, I'll
;; try to be nice and add a few myself :)
;;
(defvar vm-frame-exit-folder-hook nil
  "Hooks to be called before a folder is exited.")

(defvar vm-frame-quit-vm-hook nil
  "Hooks to be called before VM is exited.
Note vm-frame-exit-folder-hook is also called first for the current folder.")


;; You might want to change this: this key runs a function which kills
;; the current mail buffer.  Normally I just used C-x k, but now we
;; have to delete the frame too, so... voila!  New function.
;;
(defun vm-frame-mail-setup ()
  (define-key mail-mode-map "\C-xk"   'vm-frame-mail-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;----------------------------------------------------------------------------
;;
;;		    Nothing to customize below here.
;;
;;;----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Yikes!  We have to load VM so we can override some functions it defines.
;; Why?  Because we have no hooks!  So, we use fset, the poor man's hook.
;;
(require 'vm)


;; What are we?
;;
(defconst vm-frame-version-number "1.03"
  "vm-frame's version number.")


;; Function to do nothing, and do it quietly...
;;
(defun vm-frame-nothing () (interactive))


;;;----------------------------------------------------------------------------
;;
;;		       Support for the VM frame.
;;
;;;----------------------------------------------------------------------------


;; Save some original functions--safeguard against multiple loadings
;;
(or (fboundp 'orig-vm)
    (fset 'orig-vm (symbol-function 'vm)))
(or (fboundp 'orig-vm-quit)
    (fset 'orig-vm-quit (symbol-function 'vm-quit)))


;; Holds VM's frame (if it exists)
;;
(defvar vm-frame-frame nil)


;; Brings the current VM frame, if it exists, to the foreground.  If it
;; doesn't exist, calls (vm) to create it.
;;
(defun vm-frame (dont-vm)
  "Raise VM frame.  Create it if it doesn't exist.
With an argument, don't invoke VM in the newly created frame: this form
is usually only used by (vm)."
  (interactive "P")
  (if (not vm-frame-no-frame)
      (if (not (frame-live-p vm-frame-frame))
	  (progn
	    (select-frame (setq vm-frame-frame
				(make-frame (append vm-frame-alist
						    '((name . "VM"))))))
	    (if (not dont-vm)
		(orig-vm)))
	(select-frame vm-frame-frame)
	(make-frame-visible)
	(raise-frame vm-frame-frame))))


;; New way to enter VM; if the frame doesn't exist we create it and call
;; VM, otherwise we just bring the frame to the foreground.
;;
;; I'm not sure how workable this is; it might be best to call orig-vm
;; regardless once the frame is up (basically just remove the "if arg"
;; part and always call orig-vm).  Your mileage may vary.
;;
(defun vm (&optional folder read-only)
  "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox causes any contents of the system mailbox to
be moved and appended to the resulting buffer.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' expunges deleted messages and saves the buffered folder to
disk.

See the documentation for vm-mode for more information."
  (interactive (list nil current-prefix-arg))
  (vm-frame t)
  (orig-vm folder read-only))


;; Since we're creating a new frame we need to delete it when we quit.
;; However, since we're only creating one frame for all VM folders, we
;; only want to delete it when we quit the last folder.
;;
;; We don't quite accomplish that here; instead we delete the frame
;; whenever we quit from the INBOX buffer.  Oh well.
;;
(defun vm-quit (&optional no-change)
  "Quit VM, saving changes and expunging deleted messages."
  (interactive)
  (run-hooks 'vm-frame-exit-folder-hook)
  (if (not (string= (buffer-name vm-mail-buffer)
		    (file-name-nondirectory vm-primary-inbox)))
      (orig-vm-quit no-change)
    (if (and vm-frame-confirm-quit
	     (not (y-or-n-p "Do you really want to quit VM? ")))
	(error "Aborted"))
    (run-hooks 'vm-frame-quit-vm-hook)
    (orig-vm-quit no-change)
    (if (frame-live-p vm-frame-frame)
	(delete-frame vm-frame-frame))
    (setq vm-frame-frame nil)))


;; Redefine vm-summary-mode so we use the new vm-summary-mode-map as a
;; keymap; this allows us to change the mouse commands in the Summary
;; windows without changing them in the message windows.
;;
(defun vm-summary-mode ()
  "Major mode for VM folder summaries.
This major mode uses the same keymap as vm-mode.  See the vm-mode documentation
for a list of available commands."
  (setq mode-name "VM Summary"
	major-mode 'vm-summary-mode
	mode-line-format vm-mode-line-format
	buffer-read-only t
	vm-summary-pointer nil
	truncate-lines t)
  (use-local-map vm-summary-mode-map))


;; If we're in summary mode, this function determines the message number
;; of the message header point is on, if any.  Returns nil if none, or
;; the message number as an integer.
;;
(defun vm-frame-message-num ()
  (save-excursion
    (beginning-of-line)
    (and (eq major-mode 'vm-summary-mode)
	 (looking-at "\\(->\\)? *\\([0-9]+\\) ")
	 (string-to-int (buffer-substring (match-beginning 2)
					  (match-end 2))))))


;; If point is on a valid message number in the summary mode, goto that
;; message.  Otherwise, just set point.
;;
(defun vm-frame-goto-message (event)
  "Goto the message clicked on with the mouse.
If not a message, just set point."
  (interactive "e")
  (mouse-set-point event)
  (let ((msg (vm-frame-message-num)))
    (if msg
	(vm-goto-message msg))))


;; If point is on a valid message number in the summary mode, mark that
;; message.  Otherwise, ignore.
;;
(defun vm-frame-mark-message (event)
  "Mark the message clicked on with the mouse.  Don't goto that message."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (let* ((msg-num (vm-frame-message-num))
	  (msg (if msg-num (car (nthcdr (1- msg-num) vm-message-list)) nil)))
      (if msg
	  (progn
	    (vm-set-mark-of msg (not (vm-mark-of msg)))
	    (vm-mark-for-display-update msg)
	    (vm-update-summary-and-mode-line))))))


;; Make a vm-summary mode keymap and rebind the mouse keys
;;
(defvar vm-summary-mode-map (cons 'keymap vm-mode-map)
  "Keymap for VM Summary mode.  Inherits from vm-mode-map.
Rebinding (non-mouse) keys in vm-mode-map will affect both modes.")

(define-key vm-summary-mode-map [mouse-1] 'vm-frame-goto-message)
(define-key vm-summary-mode-map [down-mouse-1] 'vm-frame-nothing)
(define-key vm-summary-mode-map [drag-mouse-1] 'vm-frame-nothing)

(define-key vm-summary-mode-map [mouse-2] 'vm-frame-nothing)

(define-key vm-summary-mode-map [mouse-3] 'vm-frame-mark-message)


;; Set up the new menus
;;
(define-key vm-mode-map [menu-bar] (make-sparse-keymap "vm-menu-bar"))

;; We don't want all the stuff on the normal menu bar.  I can't figure
;; out how to *cleanly* get rid of these menus, but this at least makes
;; them disappear.  There's a big space in the middle of the menu bar
;; though.
;;

(define-key vm-mode-map [menu-bar file] '("" . undefined))
(define-key vm-mode-map [menu-bar edit] '("". undefined))
(define-key vm-mode-map [menu-bar buffer] '("" . undefined))

;(define-key vm-mode-map [menu-bar file] 'undefined)
;(define-key vm-mode-map [menu-bar edit] 'undefined)
;(define-key vm-mode-map [menu-bar buffer] 'undefined)

(define-key vm-mode-map [menu-bar send]
  (cons "Send" (make-sparse-keymap "Send")))
(define-key vm-mode-map [menu-bar message]
  (cons "Message" (make-sparse-keymap "Message")))
(define-key vm-mode-map [menu-bar folder]
  (cons "Folder" (make-sparse-keymap "Folder")))
(define-key vm-mode-map [menu-bar visit]
  '("Visit" . vm-frame-menu-bar-folders))

;; Folder menu
;;
(define-key vm-mode-map [menu-bar folder 7] 'vm-frame-nothing)
(define-key vm-mode-map [menu-bar folder quit-no-save]
  '("Quit, no Save" . vm-frame-quit-no-change))
(define-key vm-mode-map [menu-bar folder quit-vm]
  '("Quit VM" . vm-frame-quit-vm))
(define-key vm-mode-map [menu-bar folder exit-no-save]
  '("Exit, no Save" . vm-quit-no-change))
(define-key vm-mode-map [menu-bar folder exit-folder]
  '("Exit Folder" . vm-frame-exit))
(define-key vm-mode-map [menu-bar folder reload]
  '("Reload Resources" . vm-load-rc))
(define-key vm-mode-map [menu-bar folder expunge]
  '("Expunge Folder" . vm-expunge-folder))
(define-key vm-mode-map [menu-bar folder save]
  '("Save Folder" . vm-save-folder))
(define-key vm-mode-map [menu-bar folder group]
  '("Group by..." . vm-frame-choose-group))
(define-key vm-mode-map [menu-bar folder summary]
  '("Summarize Folder" . vm-summarize))
(define-key vm-mode-map [menu-bar folder get]
  '("Get New Mail" . vm-get-new-mail))

(defun vm-frame-quit-vm (event)
  "Quit VM."
  (interactive "e")
  (vm-quit))

(defun vm-frame-exit (event)
  "Quit VM."
  (interactive "e")
  (vm-quit))

(defun vm-frame-quit-no-change (event)
  "Quit VM."
  (interactive "e")
  (vm-quit-no-change))

(put 'vm-frame-quit-vm 'menu-enable '(string= (buffer-name vm-mail-buffer)
					      (file-name-nondirectory
					       vm-primary-inbox)))
(put 'vm-frame-exit 'menu-enable '(not (string= (buffer-name vm-mail-buffer)
						(file-name-nondirectory
						 vm-primary-inbox))))
(put 'vm-frame-quit-no-change 'menu-enable '(string=
					     (buffer-name vm-mail-buffer)
					     (file-name-nondirectory
					      vm-primary-inbox)))
(put 'vm-quit-no-change 'menu-enable '(not (string=
					    (buffer-name vm-mail-buffer)
					    (file-name-nondirectory
					     vm-primary-inbox))))

;; Message menu
;;
(define-key vm-mode-map [menu-bar message 7] 'vm-frame-nothing)
(define-key vm-mode-map [menu-bar message burst]
  '("Burst Digest" . vm-burst-digest))
(define-key vm-mode-map [menu-bar message pipe]
  '("Pipe to..." . vm-pipe-message-to-command))
(define-key vm-mode-map [menu-bar message print]
  '("Print" . vm-frame-print))
(define-key vm-mode-map [menu-bar message command-mark]
  '("Use Marked" . vm-next-command-uses-marks))
(define-key vm-mode-map [menu-bar message unmark-all]
  '("Unmark All" . vm-clear-all-marks))
(define-key vm-mode-map [menu-bar message mark-all]
  '("Mark All" . vm-mark-all-messages))
(define-key vm-mode-map [menu-bar message unmark]
  '("Unmark" . vm-unmark-message))
(define-key vm-mode-map [menu-bar message mark]
  '("Mark" . vm-mark-message))
(define-key vm-mode-map [menu-bar message undelete]
  '("Un-Delete" . vm-undelete-message))
(define-key vm-mode-map [menu-bar message delete]
  '("Delete" . vm-delete-message))
(define-key vm-mode-map [menu-bar message toggle]
  '("Toggle Headers" . vm-expose-hidden-headers))
(define-key vm-mode-map [menu-bar message write]
  '("Write to File..." . vm-save-message-sans-headers))
(define-key vm-mode-map [menu-bar message save]
  '("Save in Folder..." . vm-frame-save-in-folder))

;; Send menu
;;
(define-key vm-mode-map [menu-bar send 7] 'vm-frame-nothing)
(define-key vm-mode-map [menu-bar send send-digest]
  '("Send Digest" . vm-send-digest))
(define-key vm-mode-map [menu-bar send resend]
  '("Resend Bounced" . vm-resend-bounced-message))
(define-key vm-mode-map [menu-bar send forward]
  '("Forward Message" . vm-forward-message))
(define-key vm-mode-map [menu-bar send followup-text]
  '("Followup with Text" . vm-followup-include-text))
(define-key vm-mode-map [menu-bar send followup]
  '("Followup to All" . vm-followup))
(define-key vm-mode-map [menu-bar send reply-text]
  '("Reply with Text" . vm-reply-include-text))
(define-key vm-mode-map [menu-bar send reply]
  '("Reply to Sender" . vm-reply))
(define-key vm-mode-map [menu-bar send mail]
  '("Send Message" . vm-mail))

;; Help menu
;;
(define-key vm-mode-map [menu-bar help 7] 'vm-frame-nothing)
(define-key vm-mode-map [menu-bar help vm-warranty]
  '("VM Warranty" . vm-show-no-warranty))
(define-key vm-mode-map [menu-bar help vm-copy]
  '("Copying VM" . vm-show-copying-restrictions))
(define-key vm-mode-map [menu-bar help vm-help]
  '("Help on VM" . vm-frame-help))

(defun vm-frame-help (event)
  "Displays VM help."
  (interactive "e")
  (let ((major-mode 'vm-mode))
    (describe-mode)))


;; Folders menu
;;
(defun vm-frame-choose-folder (event)
  "Pop up a menu of VM folders for selection with the mouse.
Returns the folder that you select."
  (interactive "e")
  (let ((folders (directory-files vm-folder-directory nil "[^~]$"))
	menu)
    (setq menu
	  (list "Folder Menu"
		(cons "Select Folder"
		      (let* ((tail folders)
			     (inbox (file-name-nondirectory vm-primary-inbox))
			     (exclude-re (concat "^\\(#.*\\|\\.\\.?\\|"
						 inbox
						 "\\)$"))
			     (head (cons (cons inbox inbox) nil)))
			(while tail
			  (let ((elt (car tail)))
			    (if (not (string-match exclude-re elt))
				(setq head (cons (cons elt elt) head))))
			  (setq tail (cdr tail)))
			(reverse head)))))
    (x-popup-menu (if (listp event) event (cons '(0 0) (selected-frame)))
		  menu)))

(defun vm-frame-menu-bar-folders (event)
  "Pop up a menu of VM folders for selection with the mouse.
This visits the folder that you select."
  (interactive "e")
  (let ((folder (vm-frame-choose-folder event)))
    (if folder
	(vm-visit-folder folder))))

(defun vm-frame-save-in-folder (event)
  "Save message in a VM folder.  Pop up a menu of VM folders for selection."
  (interactive "e")
  (let ((folder (vm-frame-choose-folder event)))
    (if folder
	(vm-save-message folder))))

;; Grouping menu
;;
(defun vm-frame-choose-group (event)
  "Pop up a menu of VM grouping types for selection with the mouse.
Groups the current folder with the method that you select."
  (interactive "e")
  (let (menu)
    (setq menu
	  (list "Grouping Menu"
		(cons "Select Grouping"
		      (let ((tail vm-supported-groupings-alist)
			    head)
			(while tail
			  (let ((elt (car (car tail))))
			    (setq head (cons (cons elt elt) head)))
			  (setq tail (cdr tail)))
			(reverse head)))))
    (let ((elt (x-popup-menu (if (listp event) event
			       (cons '(0 0) (selected-frame)))
			     menu)))
      (if elt (vm-group-messages elt)))))

;; Quick printing option
;;
(defun vm-frame-print (event)
  "Print a message to the default printer."
  (interactive "e")
  (vm-pipe-message-to-command vm-frame-print-command nil))


;;;----------------------------------------------------------------------------
;;
;;		       Support for vm-mail frames
;;
;;;----------------------------------------------------------------------------
;;
;; Sets up having all mail sending through VM (replies, followups,
;; forwards, and new mail sent with "vm-mail") to be created in a new
;; frame.
;;


;; Save some original functions--safeguard against multiple loadings
;;
(or (fboundp 'orig-vm-mail-internal)
    (fset 'orig-vm-mail-internal (symbol-function 'vm-mail-internal)))
(or (fboundp 'orig-vm-mail-send-and-exit)
    (fset 'orig-vm-mail-send-and-exit
	  (symbol-function 'vm-mail-send-and-exit)))


;; This hook adds the "kill" keybinding
;;
(add-hook 'mail-setup-hook 'vm-frame-mail-setup)


;; So we know whether to kill the frame when vm-frame-mail-kill is run.
;;
(defvar vm-frame-mail-p nil)
(make-variable-buffer-local 'vm-frame-mail-p)


;; Redefine vm-mail-internal.  I know, I know, whadda kludge.  Give me a
;; better idea...  This creates a new frame, then invokes the old
;; vm-mail-internal to do the scut work.
;;
(defun vm-mail-internal
     (&optional buffer-name to subject in-reply-to cc references newsgroups)
  (if (not vm-frame-no-mail-frame)
      (select-frame (make-frame
		     (append vm-frame-mail-alist
			     (list (cons 'name (or buffer-name
						   "*VM-mail*")))))))
  (orig-vm-mail-internal
   buffer-name to subject in-reply-to cc references newsgroups)
  (if (not vm-frame-no-mail-frame)
      	(setq vm-frame-mail-p t)))

;; Call vm-mail-send-and-exit, then punt the mail frame.
;;
(defun vm-mail-send-and-exit (arg)
  "Just like mail-send-and-exit except that VM flags the appropriate message(s)
as having been replied to, if appropriate.  Also deletes the created frame."
  (interactive "P")
  (let ((cframe (selected-frame)))
    (orig-vm-mail-send-and-exit arg)
    (delete-frame cframe)))


;; This function kills the current message without sending.  There's no
;; real counterpart in current VM or mail; everyone just killed the
;; buffer.  That don' work no more, because we have to trash the frame
;; too.
;;
(defun vm-frame-mail-kill (arg)
  (interactive "P")
  (if vm-frame-mail-p
      (if (or arg (yes-or-no-p "Kill this mail message? "))
	  (let ((cframe (selected-frame)))
	    (kill-buffer (current-buffer))
	    (delete-frame cframe)))
    (call-interactively 'kill-buffer)))


;; Set up the new menus
;;

;; Emacs' before 19.16 don't have a mail-mode-map menu-bar
(if (not (lookup-key mail-mode-map [menu-bar]))
    (define-key mail-mode-map [menu-bar] (make-sparse-keymap "mail-menu-bar")))

;; Add items to the appropriate menu
;;
(let ((mail-map (lookup-key mail-mode-map [menu-bar mail])))
  (if mail-map
      (progn
	(define-key-after mail-map [kill]
	  '("Kill Message" . vm-frame-mail-kill) 'send-stay)
	(define-key mail-map [send]
	  '("Send Message" . vm-mail-send-and-exit))
	(define-key mail-map [send-stay]
	  '("Send, Keep Editing" . vm-mail-send)))
    (define-key mail-mode-map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))
    (define-key mail-mode-map [menu-bar mail signature]
      '("Insert Signature" . mail-signature))
    (define-key mail-mode-map [menu-bar mail kill]
      '("Kill Message" . vm-frame-mail-kill))
    (define-key mail-mode-map [menu-bar mail send-stay]
      '("Send, Keep Editing" . vm-mail-send))
    (define-key mail-mode-map [menu-bar mail send]
      '("Send Message" . vm-mail-send-and-exit))
    (define-key mail-mode-map [menu-bar mail 7] 'vm-frame-nothing)))

;;; Provide ourselves:

(provide 'vm-frame)

