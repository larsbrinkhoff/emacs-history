;; Scramble text amusingly for Emacs.
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


(defun dissociated-press (&optional arg)
  "Dissociate the text of the current buffer.
Output goes in buffer named *Dissociation*,
which is redisplayed each time text is added to it.
Every so often the user must say whether to continue.
If ARG is positive, require ARG chars of continuity.
If ARG is negative, require -ARG words of continuity.
Default is 2."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 2))
  (let* ((inbuf (current-buffer))
	 (outbuf (get-buffer-create "*Dissociation*"))
	 (move-function (if (> arg 0) 'forward-char 'forward-word))
	 (move-amount (if (> arg 0) arg (- arg)))
	 (search-function (if (> arg 0) 'search-forward 'word-search-forward))
	 (last-query-point 0))
    (switch-to-buffer outbuf)
    (erase-buffer)
    (while
      (save-excursion
	(goto-char last-query-point)
	(vertical-motion (- (window-height) 4))
	(or (= (dot) (dot-max))
	    (and (progn (goto-char (dot-max))
			(y-or-n-p "Continue dissociation? "))
		 (progn
		   (message "")
		   (recenter 1)
		   (setq last-query-point (dot-max))
		   t))))
      (let (start end)
	(save-excursion
	 (set-buffer inbuf)
	 (setq start (dot))
	 (if (eq move-function 'forward-char)
	     (progn
	       (setq end (+ start (+ move-amount (logand 15 (random)))))
	       (if (> end (dot-max))
		   (setq end (+ 1 move-amount (logand 15 (random)))))
	       (goto-char end))
	   (funcall move-function
		    (+ move-amount (logand 15 (random)))))
	 (setq end (dot)))
	(let ((odot (dot)))
	  (insert-buffer-substring inbuf start end)
	  (save-excursion
	   (goto-char odot)
	   (end-of-line)
	   (and (> (current-column) fill-column)
		(do-auto-fill)))))
      (save-excursion
       (set-buffer inbuf)
       (if (eobp)
	   (goto-char (dot-min))
	 (let ((overlap
		(buffer-substring (prog1 (dot)
					 (funcall move-function
						  (- move-amount)))
				  (dot))))
	   (let (ranval)
	     (while (< (setq ranval (random)) 0))
	     (goto-char (1+ (% ranval (1- (dot-max))))))
	   (or (funcall search-function overlap nil t)
	       (let ((odot (dot)))
		 (goto-char 1)
		 (funcall search-function overlap odot t))))))
      (sit-for 0))))
