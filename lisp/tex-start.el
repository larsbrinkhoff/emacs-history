; This file is for use by TeX82 (see man page) to allow switching to
;  Emacs at a line number given on the command line
; It assumes that it has been called by:
;	emacs -l tex-start -e startline linenumber file

(defun startline ()
  (setq args (cdr args))
  (find-file (car (cdr args)))
  (goto-char (dot-min))
  (forward-line (1- (string-to-int (car args))))
  (setq args nil))
