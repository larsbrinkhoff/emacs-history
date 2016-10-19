;; Copyright (C) 1985 Free Software Foundation
;; Author Richard Mlynarik.

;; This file is part of GNU Emacs.

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

; Randomize the seed in the random number generator.
(random t)

; Important pinheaddery for GNU Emacs.
; Expects file LINES.YOW to be in ITS-style LINS format
;  (ie strings terminated by ascii 0 characters.  Leading whitespace ignored)
; Everything up to the first \000 is a comment.
(defun yow (&optional n interactive)
  "Return or display a Zippy quotation"
  (interactive
    (if current-prefix-arg
	(list (prefix-numeric-value current-prefix-arg) t)
      (list nil t)))
  (if (null yow-vector)
      (setq yow-vector (snarf-yows)))
  (let ((yow (aref yow-vector
		   (if n
		       n
		     (% (logand 0777777 (random)) (length yow-vector))))))
    (cond ((not interactive)
	   yow)
	  ((not (string-match "\n" yow))
	   (delete-windows-on (get-buffer-create "*Help*"))
	   (message yow))
	  (t
	   (message "Yow!")
	   (with-output-to-temp-buffer "*Help*"
	     (princ yow))))))

(defvar yow-vector nil "Pertinent pinhead statements")
(defun snarf-yows ()
  (message "Am I CONSING yet?...")
  (save-excursion
    (let ((buf (generate-new-buffer " yow"))
	  (result '()))
      (set-buffer buf)
      (insert-file-contents (expand-file-name "lines.yow" exec-directory))
      (goto-char (scan-buffer (point-min) 1 000))
      (while (not (eobp))
	(or (setq end (scan-buffer (point) 1 000))
	    (error "Invalid lines.yow file"))
	(if (if (looking-at "[^ \t\n\r\f]")
		(progn (forward-char 1) t)
	      (re-search-forward "[^ \t\n\r\f]" end t))
	    (setq result (cons (buffer-substring (1- (point)) (1- end))
			       result)))
	(goto-char end))
      (kill-buffer buf)
      (message "I have SEEN the CONSING!!" (length result))
      (apply 'vector (nreverse result)))))
