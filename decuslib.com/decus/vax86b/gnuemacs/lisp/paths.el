;; These are default settings for names of certain files and directories
;; that Emacs needs to refer to from time to time.

;; If these settings are not right, override them with `setq'
;; in site-init.el.  Do not change this file.

(defvar Info-directory "EMACS_INFODIR")

(defvar exec-directory "EMACS_DIR")

(defconst term-file-prefix "term_"
  "Startup does (load (concat term-file-prefix (getenv \"TERM\")))")
