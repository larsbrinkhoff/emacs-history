;; Display time and load in mode line of Emacs.
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


(defvar display-time-process nil)

;Also defconst in loaddefs.el
;This is temporary for while Emacs 15 is still in use.
(defvar display-time-day-and-date nil
  "*Non-nil means M-x display-time should display day and date as well as time.")

(defun display-time ()
  "Display current time and load level in mode line of each buffer.
Updates automatically every minute."
  (interactive)
  (let ((live (and display-time-process
		   (eq (process-status display-time-process) 'run))))
    (if (not live)
	(save-excursion
	 (if display-time-process
	     (delete-process display-time-process))
	 (setq global-mode-string "time and load")
	 (setq display-time-process
	       (start-process "display-time" (current-buffer)
			      (expand-file-name "loadst" exec-directory)
			      "-n" "60"))
	 (process-kill-without-query display-time-process)
	 (set-process-sentinel display-time-process 'display-time-sentinel)
	 (set-process-filter display-time-process 'display-time-filter)))))

(defun display-time-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (setq global-mode-string ""))
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun display-time-filter (proc string)
  (let (idx)
    (while (setq idx (string-match "]." string))
      (setq string (substring string (1+ idx)))))
  (if display-time-day-and-date
      (setq string (concat (substring (current-time-string) 0 11) string)))
  (setq global-mode-string string)
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))
