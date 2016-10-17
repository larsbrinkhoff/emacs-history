;; sample linux default initialization file - rick sladkey

;; add a local lisp bin at the front of the load path

(if (= (length load-path) 1)
    (setq load-path (cons "/usr/emacs/lisp/local" load-path)))

;; handle any linux console if user hasn't done so already
;; make sure "linux.el" is placed in emacs/lisp/term/linux.el

(or term-setup-hook
    (setq term-setup-hook
	  (function (lambda ()
		      (if (string-match "^console$\\|^con[0-9]+x[0-9]+$\\|^xterm$"
					(getenv "TERM"))
			  (progn
			    (setq meta-flag t)
			    (load (concat term-file-prefix "linux") nil t)))
		      (and (fboundp 'enable-arrow-keys)
			   (enable-arrow-keys))))))
