;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	This file handles replacements to be done when reading or writing
;;	a file.
;;	The variable 'my-auto-replacements' is assigned a list of the
;;	format:
;;		( (<major mode 1> . <replacements 1>)
;;		  (<major mode 2> . <replacements 2>)
;;			       ........
;;		  (<major mode n> . <replacements n>) )
;;
;;	    where  <replacements i> is a list of the format:
;;		( (<string 1 for user> <string 1 for file>)
;;		  (<string 2 for user> <string 2 for file>)
;;			       ........
;;		  (<string m for user> <string m for file>) )
;;
;;	For example, if we have:
;;	(setq my-auto-replacements '(
;;			(LaTeX-mode.(("å" "{\\aa}")
;;				     ("ä" "\\\"a")
;;				     ("ö" "\\\"o")))))
;;	all å, ä and ö will be stored as {\aa}, \"a and \"o on the file,
;;	respectively, if we are in LaTeX-mode.

(require 'cl)

(setq my-auto-replacements '(
     (LaTeX-mode.(("å" "{\\aa}")
		  ("ä" "\\\"a")
		  ("ö" "\\\"o")
		  ("Å" "{\\AA}")
		  ("Ä" "\\\"A")
		  ("Ö" "\\\"O")))
     (swetex-mode.(("å" "}")
		  ("ä" "{")
		  ("ö" "|")
		  ("Å" "]")
		  ("Ä" "[")
		  ("Ö" "\\")
		  ("é" "/'e")))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 	The hooks. They first lookup the list of replacements for
;;	the current major mode. Then they do "replace-string" on
;;	the buffer.
;;

(defun my-read-hook nil
     " "
     (my-read-hook1 (cdr (assoc major-mode my-auto-replacements))))

(defun my-write-hook nil
     " "
     (my-write-hook1 (cdr (assoc major-mode my-auto-replacements))))

(defun my-read-hook1 (r)
     (and r
          (replace-string (cadar r) (caar r))
	  (goto-char (point-min))  ; Top of file
          (my-read-hook1 (cdr r))))

(defun my-write-hook1 (r)
     (and r
          (replace-string (caar r) (cadar r))
	  (goto-char (point-min))  ; Top of file
          (my-write-hook1 (cdr r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	Push the read and write hooks onto the proper hook-lists
;;

(and (not (member 'my-read-hook find-file-hooks))
     (setq find-file-hooks (cons 'my-read-hook find-file-hooks)))

(and (not (member 'my-write-hook write-file-hooks))
     (setq write-file-hooks (cons 'my-write-hook write-file-hooks)))



