(defun protect-innocence-hook ()
  (if (and (equal (file-name-nondirectory buffer-file-name) "sex.6")
	   (not (y-or-n-p "Are you over 18? ")))
      (progn
	(clear-visited-file-modtime)
	(setq buffer-file-name (concat (file-name-directory buffer-file-name)
				       "celibacy.1"))
	(erase-buffer)
	(insert-file-contents buffer-file-name t)
	(rename-buffer (file-name-nondirectory buffer-file-name)))))

(setq find-file-hooks (cons 'protect-innocence-hook find-file-hooks))
