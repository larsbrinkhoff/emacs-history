
;;;
;;; Begin Active Hyper Stuff For .emacs file
;;;

(defun active-hyper:update-big-list ()
  (interactive)
  (find-file "active-hyper.menu.update.big.list")
  (delete-other-windows)
  (split-window-vertically)
  (let ((MSG "Site not added to Frequent List. ")
	(aha (concat "*    " (read-from-minibuffer "Topic of New Site (return if none) ")))
	(ahb (concat "**   " (read-from-minibuffer "Files at New Site (return if none) ")))
	(ahc (concat "***  " (read-from-minibuffer "Describe the Site/Files (return if none) ")))
	(ahd (concat "**** " (read-from-minibuffer "Complete the ange-ftp pathname " "\"/anonymous@"))))
  (if (y-or-n-p "Add this site to your frequent list? ")
      (progn
	(setq MSG "Added to Frequent List. ")
	(find-file "active-hyper.list.frq.otl")
	(goto-char (point-max))
	(insert "\n")
	(insert (concat "\n" aha "\n" ahb "\n" ahc "\n" ahd "\n\n"))
	(save-buffer)))
    (mail)
    (delete-other-windows)
    (mail-to)
    (insert "brannon@jove.cs.caltech.edu")
    (mail-subject)
    (insert (concat "New ftp Site "))
    (goto-char (point-max))
    (insert "
Hi Terrence, add this ftp site to the big ftp site list.

Note: this site will be added to the file active-hyper.list.new and will be
accessible  by clicking on the new sites button near the top of the file
active-hyper.list.big.otl. When a new release of HyperActiveFTP comes out it
will be transferred to the file named active-hyper.list.big.otl.

(I, Terrence thank you in advance for your contribution)
")

    (insert (concat "\n" aha "\n" ahb "\n" ahc "\n" ahd "\n"))
  (message (concat MSG "C-c C-c to send message to the moderator"))
  (kill-buffer "active-hyper.menu.update.big.list")))
	     

(defun active-hyper:get-hyper()
  (interactive)
  (if (one-window-p)
      (progn
	(find-file (concat ah-install-directory
			   "active-hyper.menu.main"))
	(goto-char (point-min)))
    (progn
      (find-file-other-window (concat ah-install-directory
				      "active-hyper.menu.main"))
      (goto-char (point-min))))
  (recenter '(0)))

(defun active-hyper:display-topics ()
  (goto-char (point-min))
  (while (re-search-forward "^\*[^*].*$" (point-max) "x")
    (hide-subtree))
  (goto-char (point-min))
  (while (re-search-forward "[\n]$" (point-max) t)
    (progn
      (backward-char 3)
      (replace-regexp "[\n]$" "" nil)))
  (goto-char (point-min))
  (while (re-search-forward "[<]+[(]+.*[)]+[>]+" (point-max) t)
    (progn
      (backward-char 8)
      (replace-regexp "[<]+[(]+.*[)]+[>]+" "" nil))))

(defun active-hyper:prevent-save ()
  (progn
    (define-key outline-mode-map"\C-x\C-s" 'active-hyper:nosave)
    (define-key outline-mode-map "\C-xs" 'active-hyper:nosave)))

(defun active-hyper:nosave()
  (interactive)
  (message "Do not attempt to save this buffer. Ever"))

(defun active-hyper:view-compressed-file (filename)
  (let ((E (make-temp-name "/tmp/")))
    (setq aha (concat E ".Z"))
    (setq max-lisp-eval-depth 1000)
    (copy-file filename aha t)
    (shell-command (concat "uncompress " aha))
    (kill-buffer "*Shell Command Output*")
    (delete-other-windows)
    (view-file E))
  (makunbound 'aha))
  

(defun active-hyper:sort-buffer ()
  (untabify (point-min) (point-max))
  (sort-lines nil (point-min) (point-max)))

(defun active-hyper:outline-quickdoc ()
  (goto-char (point-min))
  (insert "C-c C-s to show the full entry
<(Top Level)>
<(Show All Fields of Entries)>
<(Frequent FTP Sites)>")
  (goto-char (point-max))
  (insert "C-c C-s to show the full entry
<(Top Level)>
<(Show All Fields of Entries)>
<(Frequent FTP Sites)>")
  (insert (concat "\n" "<(Top of File)>")))
	  
(defun active-hyper:credits ()
  (delete-other-windows)
  (view-file "active-hyper.credits")
  (goto-char (point-min)))


;;;
;;; End Active Hyper Stuff
;;;
