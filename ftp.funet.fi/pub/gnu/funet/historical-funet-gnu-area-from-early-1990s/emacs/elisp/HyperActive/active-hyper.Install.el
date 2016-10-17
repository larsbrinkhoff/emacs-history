;;
;; hyperbole and ange-ftp must be loaded for this to work
;; just type M-x eval-current-buffer and we're off
;;

(setq max-lisp-eval-depth 2000)
(setq max-specpdl-size 2000)

(setq active-hyper-manifest
      '(
	".hypb" ".dired"
	"active-hyper.list.new"
	"README.HyperActiveFTP" "active-hyper.menu.install"
	"active-hyper.menu.update.big.list"
	"active-hyper.Admin.el" "active-hyper.Changelog" 
	"active-hyper.Install.el" "active-hyper.Makefile" 
	"active-hyper.credits"
	"active-hyper.documentation" "active-hyper.el"
	"active-hyper.menu.main" "active-hyper.menu.install"
	"data.bioftp.unibas.ch" "data.biological.scientists"
	"data.j" "data.kanji" "data.math.abstracts.database"
	"data.mobal" "data.netlib" "data.object.oriented.parallel"
	"data.paragon" "data.psfig" "data.symbmath" "data.txl"
	"data.usenet"
	"active-hyper.to.do"))

(setq ah-for-remote-install
      '(
	".hypb" ".dired"
	"active-hyper.list.new"
	"README.HyperActiveFTP" "active-hyper.menu.install"
	"active-hyper.menu.update.big.list"
	"active-hyper.Admin.el" 
	"active-hyper.Changelog" "active-hyper.Install.el"
	"active-hyper.Makefile" "active-hyper.credits"
	"active-hyper.documentation" "active-hyper.el"
	"active-hyper.list.big.otl" "active-hyper.list.frq.otl"
	"active-hyper.main.menu" "active-hyper.to.do"
	"data.bioftp.unibas.ch" "data.biological.scientists"
	"data.j" "data.kanji" "data.math.abstracts.database"
	"data.mobal" "data.netlib" "data.object.oriented.parallel"
	"data.paragon" "data.psfig" "data.symbmath" "data.txl"
	"data.usenet"
))

;(setq active-hyper-remote-dumpster "/anonymous@archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/packages/HyperActiveFTP/")
;(setq active-hyper-remote-dumpster "/brannon@nic.funet.fi:/pub/gnu/emacs/elisp/HyperActive/")

;;; ^^^--- dont forget the ENDING SLASH

;;; Where you want HyperActive FTP installed. 
;;; Two Necessaries:

;;; 1  DO NOT PLACE THIS BELOW A HYPERBOLE DIRECTORY. If you do, then when
;;; you wipe your current hyperbole directory to dowload a new version of
;;; hyperbole, you may wipe out HyperActive FTP also
;;; 2  Insure that the pathname ENDS WITH A SLASH.
(setq ah-install-directory "~/emacs/active-hyper/")


(defun active-hyper:local-install ()
  (interactive)
  (mapcar 'copy-from active-hyper-manifest)   ; copy over the basic files

  ;; add in the load of my elisp source code
  (find-file (concat ah-install-directory "active-hyper.requirements.el"))
  (erase-buffer)
  (goto-char (point-min))
  (eval-current-buffer)
  (setq aha 
	(concat
	 "(setq ah-install-directory \"" ah-install-directory "\")"))
  (setq ahb
	(concat
	 "(setq load-path (cons \"" ah-install-directory "\""
	 " load-path))"))
  (setq ahc "(require 'ange-ftp)")
  (setq ahd 
	(concat
	 "(load-file \""
	 (concat (expand-file-name ah-install-directory) "active-hyper.el")
	 "\")"))
  (insert (concat aha ahb ahc ahd))

  (save-buffer)
  (kill-buffer (current-buffer))
  (message "Ok, done. Just one more step."))

(defun copy-from (M)
  (copy-file (concat active-hyper-remote-dumpster M)
	     (concat ah-install-directory M) t))



;;; quasi hyper-makefile

(defun active-hyper:update-files()
  (setq active-hyper-update-files
	'(
	  "active-hyper.list.new"
	  "active-hyper.list.big.otl"
	  ".hypb"
	  "active-hyper.to.do"
	  "active-hyper.Changelog"
	  "active-hyper.documentation"))
  (mapcar 'copy-from active-hyper-update-files))
  
  

(defun remote-install ()
  (interactive)
  (mapcar 'copy-to ah-for-remote-install))

(defun copy-to (M)
  (copy-file (concat ah-install-directory M)
	     (concat active-hyper-remote-dumpster M) t))



(defun active-hyper:change-default()
  (let ((aha '())
	(ret nil))
    (while (eq ret nil)
      (progn
	(setq aha (read-from-minibuffer "Where should HyperActiveFTP be installed? " "~/emacs/active-hyper"))
	(setq ret (y-or-n-p (concat "Use " aha " as installation directory? ")))))
    (setq ah-install-directory aha))
  (message (concat "Making directory " ah-install-directory))
  (active-hyper:mkdir))

(defun active-hyper:mkdir ()
      (shell-command (concat
		      "mkdir "
		      (expand-file-name ah-install-directory)))
      (kill-buffer "*Shell Command Output*")
      (find-file "active-hyper.menu.install")
      (delete-other-windows)
      (message "Directory is made... Proceed to Step Two"))
  

