;; Convert texinfo files to info files.
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

(defvar texinfo-mode-syntax-table nil)

(let ((st (syntax-table)))
  (unwind-protect
   (progn
    (setq texinfo-mode-syntax-table (make-syntax-table))
    (set-syntax-table texinfo-mode-syntax-table)
    (modify-syntax-entry ?\" " ")
    (modify-syntax-entry ?\\ " ")
    (modify-syntax-entry ?\^q "\\")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    (modify-syntax-entry ?' "w"))
   (set-syntax-table st)))

(defun texinfo-mode ()
  "Major mode for editing texinfo files.
These are files that are input for TEX and also to be turned
into Info files by M-x texinfo-format-buffer.
These files must be written in a very restricted and
modified version of TEX input format.

As for editing commands, like text-mode except for syntax table,
which is set up so expression commands skip texinfo bracket groups."
  (interactive)
  (text-mode)
  (setq mode-name "Texinfo")
  (setq major-mode 'texinfo-mode)
  (set-syntax-table texinfo-mode-syntax-table)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^\b\\|^@\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^\b\\|^@\\|" paragraph-start))
  (and (boundp 'texinfo-mode-hook)
       texinfo-mode-hook
       (funcall texinfo-mode-hook)))

(defun texinfo-format-buffer ()
  "Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer
visiting the Info file names specified in the @setfilename command."
  (interactive)
  (message "Formatting Info file...")
  (let (texinfo-format-filename
	texinfo-example-start
	texinfo-command-start
	texinfo-command-end
	texinfo-command-name
	texinfo-stack
	outfile
	(fill-column fill-column)
	(input-buffer (current-buffer)))
    (save-excursion
      (goto-char (dot-min))
      (search-forward "@setfilename")
      (setq texinfo-command-end (dot))
      (setq outfile (texinfo-parse-line-arg)))
    (find-file outfile)
    (texinfo-mode)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (goto-char (dot-min))
    (while (search-forward "``" nil t)
      (replace-match "\""))
    (goto-char (dot-min))
    (while (search-forward "''" nil t)
      (replace-match "\""))
    (goto-char (dot-min))
    (while (search-forward "@" nil t)
      (if (= (following-char) ?@)
	  (delete-char 1)
	(setq texinfo-command-start (1- (dot)))
	(if (= (char-syntax (following-char)) ?w)
	    (forward-word 1)
	  (forward-char 1))
	(setq texinfo-command-end (dot))
	(setq texinfo-command-name
	      (intern (buffer-substring (1+ texinfo-command-start)
					texinfo-command-end)))
	(let ((cmd (get texinfo-command-name 'texinfo-format)))
	  (if cmd (funcall cmd)
	    (texinfo-unsupported)))))
    (cond (texinfo-stack
	   (goto-char (nth 2 (car texinfo-stack)))
	   (error "Unterminated @%s" (car (car texinfo-stack)))))
    (goto-char (dot-min))
    (while (search-forward "\^q" nil t)
      (delete-char -1)
      (forward-char 1))
    (goto-char (dot-min))
    (message "Formatting Info file...done.  Now save it.")))

(put 'begin 'texinfo-format 'texinfo-format-begin)
(defun texinfo-format-begin ()
  (texinfo-format-begin-end 'texinfo-format))

(put 'end 'texinfo-format 'texinfo-format-end)
(defun texinfo-format-end ()
  (texinfo-format-begin-end 'texinfo-end))

(defun texinfo-format-begin-end (prop)
  (setq texinfo-command-name (intern (texinfo-parse-line-arg)))
  (setq cmd (get texinfo-command-name prop))
  (if cmd (funcall cmd)
    (texinfo-unsupported)))

(defun texinfo-parse-line-arg ()
  (goto-char texinfo-command-end)
  (skip-chars-forward " ")
  (let ((start (dot)))
    (if (not (looking-at "[[{(]"))
	(progn
	 (end-of-line)
	 (setq texinfo-command-end (dot)))
      (setq start (1+ (dot)))
      (forward-list 1)
      (setq texinfo-command-end (dot))
      (forward-char -1))
    (prog1 (buffer-substring start (dot))
	   (if (eolp) (forward-char 1)))))

(defun texinfo-parse-arg-discard ()
  (prog1 (texinfo-parse-line-arg)
	 (texinfo-discard-command)))

(defun texinfo-format-parse-args ()
  (let ((start (1- (dot)))
	next beg end
	args)
    (search-forward "[")
    (while (/= (preceding-char) ?\])
      (skip-chars-forward " \t\n")
      (setq beg (dot))
      (re-search-forward "[],]")
      (setq next (dot))
      (forward-char -1)
      (skip-chars-backward " \t\n")
      (setq end (dot))
      (cond ((< beg end)
	     (goto-char beg)
	     (while (search-forward "\n" end t)
	       (replace-match " "))))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next))
    (if (eolp) (forward-char 1))
    (setq texinfo-command-end (dot))
    (nreverse args)))

(put 'setfilename 'texinfo-format 'texinfo-format-setfilename)
(defun texinfo-format-setfilename ()
  (let ((arg (texinfo-parse-line-arg)))
    (texinfo-discard-line)
    (insert "Info file "
	    arg
	    ", produced by texinfo-format-buffer\nfrom "
	    (if (buffer-file-name input-buffer)
		(concat "file "
			(file-name-nondirectory (buffer-file-name input-buffer)))
	      (concat "buffer " (buffer-name input-buffer)))
	    ?\n)
    (setq texinfo-format-filename arg)))

(put 'node 'texinfo-format 'texinfo-format-node)
(defun texinfo-format-node ()
  (let* ((args (texinfo-format-parse-args))
	 (name (nth 0 args))
	 (next (nth 1 args))
	 (prev (nth 2 args))
	 (up (nth 3 args)))
    (texinfo-discard-command)
    (or (bolp)
	(insert ?\n))
    (insert "\^_\nFile: " texinfo-format-filename
	    "  Node: " name)
    (if prev
	(insert ", Prev: " prev))
    (if up
	(insert ", Up: " up))
    (if next
	(insert ", Next: " next))
    (insert ?\n)))

(put 'menu 'texinfo-format 'texinfo-format-menu)
(defun texinfo-format-menu ()
  (texinfo-discard-line)
  (insert "* Menu:\n\n"))

(put 'menu 'texinfo-end 'texinfo-discard-line)
(defun texinfo-discard-line ()
  (goto-char texinfo-command-end)
  (or (eolp)
      (error "Extraneous text at end of command line."))
  (goto-char texinfo-command-start)
  (or (bolp)
      (error "Extraneous text at beginning of command line."))
  (delete-region (dot) (progn (forward-line 1) (dot))))

; @note [NODE, FNAME, NAME, FILE, DOCUMENT]
;or
; @xref [NODE, FNAME, NAME, FILE, DOCUMENT]
; -> *Note FNAME: (FILE)NODE
;   If FILE is missing,
;    *Note FNAME: NODE
;   If FNAME is empty and NAME is present
;    *Note NAME: Node
;   If both NAME and FNAME are missing
;    *Note NODE::
;   texinfo ignores the DOCUMENT argument.
; -> See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;   If FILE is specified, (FILE)NODE is used for xrefs.
;   If fifth argument DOCUMENT is specified, produces
;    See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;    of DOCUMENT
(put 'note 'texinfo-format 'texinfo-format-note)
(put 'xref 'texinfo-format 'texinfo-format-note)
(defun texinfo-format-note ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note ")
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (if (null (or fname (nth 3 args)))
	  (insert (car args) "::")
	(insert (or fname (car args)) ": ")
	(if (nth 3 args)
	    (insert "(" (nth 3 args) ")"))
	(insert (car args))))))

;@infonote[NODE, FNAME, FILE]
;Like @xref[NODE, FNAME,,FILE] in texinfo.
;In Tex, generates "See Info file FILE, node NODE"
(put 'infonote 'texinfo-format 'texinfo-format-infonote)
(defun texinfo-format-infonote ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))))

(put 'ichapter 'texinfo-format 'texinfo-format-chapter)
(put 'chapter 'texinfo-format 'texinfo-format-chapter)
(put 'iappendix 'texinfo-format 'texinfo-format-chapter)
(put 'appendix 'texinfo-format 'texinfo-format-chapter)
(put 'iunnumbered 'texinfo-format 'texinfo-format-chapter)
(put 'unnumbered 'texinfo-format 'texinfo-format-chapter)
(defun texinfo-format-chapter ()
  (texinfo-format-chapter-1 ?*))

(put 'isection 'texinfo-format 'texinfo-format-section)
(put 'section 'texinfo-format 'texinfo-format-section)
(put 'iappendixsection 'texinfo-format 'texinfo-format-section)
(put 'appendixsection 'texinfo-format 'texinfo-format-section)
(put 'iunnumberedsec 'texinfo-format 'texinfo-format-section)
(put 'unnumberedsec 'texinfo-format 'texinfo-format-section)
(defun texinfo-format-section ()
  (texinfo-format-chapter-1 ?=))

(put 'isubsection 'texinfo-format 'texinfo-format-subsection)
(put 'subsection 'texinfo-format 'texinfo-format-subsection)
(put 'iappendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'appendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'iunnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'unnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(defun texinfo-format-subsection ()
  (texinfo-format-chapter-1 ?-))

(put 'isubsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'subsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'iappendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'appendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'iunnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'unnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(defun texinfo-format-subsubsection ()
  (texinfo-format-chapter-1 ?.))

(defun texinfo-format-chapter-1 (belowchar)
  (let ((arg (texinfo-parse-line-arg)))
    (texinfo-discard-line)
    (insert arg ?\n "@SectionPAD " belowchar ?\n)
    (forward-line -2)))

(put 'SectionPAD 'texinfo-format 'texinfo-format-sectionpad)
(defun texinfo-format-sectionpad ()
  (let ((str (texinfo-parse-line-arg)))
    (texinfo-discard-line)
    (forward-char -1)
    (let ((column (current-column)))
      (forward-char 1)
      (while (> column 0)
	(insert str)
	(setq column (1- column))))
    (insert ?\n)))

(put '\. 'texinfo-format 'texinfo-format-\.)
(defun texinfo-format-\. ()
  (texinfo-discard-command)
  (insert "."))

(put '\: 'texinfo-format 'texinfo-format-\:)
(defun texinfo-format-\: ()
  (texinfo-discard-command))

;; @itemize pushes (itemize "COMMANDS" STARTPOS) on texinfo-stack.
;; @enumerate pushes (enumerate 0 STARTPOS).
;; @item dispatches to the texinfo-item prop of the first elt of the list.
;; For itemize, this puts in and rescans the COMMANDS.
;; For enumerate, this increments the number and puts it in.
;; In either case, it puts a Backspace at the front of the line
;; which marks it not to be indented later.
;; All other lines get indented by 5 when the @end is reached.

(defun texinfo-push-stack (check arg)
  (setq texinfo-stack
	(cons (list check arg texinfo-command-start)
	      texinfo-stack)))

(defun texinfo-pop-stack (check)
  (if (null texinfo-stack)
      (error "Unmatched @end %s" check))
  (if (not (eq (car (car texinfo-stack)) check))
      (error "@end %s matches @%s"
	     check (car (car texinfo-stack))))
  (prog1 (cdr (car texinfo-stack))
	 (setq texinfo-stack (cdr texinfo-stack))))

(put 'itemize 'texinfo-format 'texinfo-itemize)
(defun texinfo-itemize ()
  (texinfo-push-stack 'itemize (texinfo-parse-line-arg))
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'itemize 'texinfo-end 'texinfo-end-itemize)
(defun texinfo-end-itemize ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-line)
  (let ((stacktop
	 (texinfo-pop-stack 'itemize)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'enumerate 'texinfo-format 'texinfo-enumerate)
(defun texinfo-enumerate ()
  (texinfo-push-stack 'enumerate 0)
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'enumerate 'texinfo-end 'texinfo-end-enumerate)
(defun texinfo-end-enumerate ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-line)
  (let ((stacktop
	 (texinfo-pop-stack 'enumerate)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'table 'texinfo-format 'texinfo-table)
(defun texinfo-table ()
  (texinfo-push-stack 'table (texinfo-parse-line-arg))
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'ftable 'texinfo-format 'texinfo-ftable)
(defun texinfo-ftable ()
  (texinfo-push-stack 'table "3")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'description 'texinfo-format 'texinfo-description)
(defun texinfo-description ()
  (texinfo-push-stack 'table "1")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'table 'texinfo-end 'texinfo-end-table)
(put 'ftable 'texinfo-end 'texinfo-end-table)
(put 'description 'texinfo-end 'texinfo-end-table)
(defun texinfo-end-table ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-line)
  (let ((stacktop
	 (texinfo-pop-stack 'table)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; At the @end, indent all the lines within the construct
;; except those marked with backspace.  FROM says where
;; construct started.
(defun texinfo-do-itemize (from)
  (save-excursion
   (while (progn (forward-line -1)
		 (>= (dot) from))
     (if (= (following-char) ?\b)
	 (save-excursion
	   (delete-char 1)
	   (end-of-line)
	   (delete-char 6))
       (save-excursion (insert "     "))))))

(put 'item 'texinfo-format 'texinfo-item)
(defun texinfo-item ()
  (funcall (get (car (car texinfo-stack)) 'texinfo-item)))

(put 'itemize 'texinfo-item 'texinfo-itemize-item)
(defun texinfo-itemize-item ()
  (texinfo-discard-line)
  (insert "\b   " (nth 1 (car texinfo-stack)) " \n")
  (forward-line -1))

(put 'enumerate 'texinfo-item 'texinfo-enumerate-item)
(defun texinfo-enumerate-item ()
  (texinfo-discard-line)
  (let ((next (1+ (car (cdr (car texinfo-stack))))))
    (setcar (cdr (car texinfo-stack)) next)
    (insert ?\b (format "%3d. " next) ?\n))
  (forward-line -1))

(put 'table 'texinfo-item 'texinfo-table-item)
(defun texinfo-table-item ()
  (let ((arg (texinfo-parse-line-arg))
	(fontcode (aref (car (cdr (car texinfo-stack))) 0)))
    (texinfo-discard-line)
    (cond ((= fontcode ?3)
	   (insert "\b`" arg "'\n"))
	  ((= fontcode ?2)
	   (insert ?\b (upcase arg) ?\n))
	  (t
	   (insert ?\b arg ?\n)))
    (insert ?\n))
  (forward-line -2))

(put 'ifinfo 'texinfo-format 'texinfo-discard-command)
(defun texinfo-discard-command ()
  (delete-region texinfo-command-start texinfo-command-end))
(put 'ifinfo 'texinfo-end 'texinfo-discard-command)

(put 'iftex 'texinfo-format 'texinfo-format-iftex)
(defun texinfo-format-iftex ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end[ \n]iftex")
			(dot))))

(put 'endiftex 'texinfo-format 'texinfo-discard-command)

(put 'var 'texinfo-format 'texinfo-format-var)
(put 'heading 'texinfo-format 'texinfo-format-var)
(defun texinfo-format-var ()
  (insert (upcase (texinfo-parse-arg-discard)))
  (goto-char texinfo-command-start))

(put 'b 'texinfo-format 'texinfo-format-noop)
(put 'i 'texinfo-format 'texinfo-format-noop)
(put 'w 'texinfo-format 'texinfo-format-noop)
(defun texinfo-format-noop ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

(put 'code 'texinfo-format 'texinfo-format-code)
(put 'kbd 'texinfo-format 'texinfo-format-code)
(defun texinfo-format-code ()
  (insert "`" (texinfo-parse-arg-discard) "'")
  (goto-char texinfo-command-start))

(put 'defn 'texinfo-format 'texinfo-format-defn)
(put 'dfn 'texinfo-format 'texinfo-format-defn)
(defun texinfo-format-defn ()
  (insert "\"" (texinfo-parse-arg-discard) "\"")
  (goto-char texinfo-command-start))

(put 'bullet 'texinfo-format 'texinfo-format-bullet)
(defun texinfo-format-bullet ()
  (texinfo-discard-command)
  (insert "*"))

(put 'example 'texinfo-format 'texinfo-format-example)
(put 'quotation 'texinfo-format 'texinfo-format-example)
(put 'lisp 'texinfo-format 'texinfo-format-example)
(put 'display 'texinfo-format 'texinfo-format-example)
(put 'format 'texinfo-format 'texinfo-format-example)
(put 'flushleft 'texinfo-format 'texinfo-format-example)
(defun texinfo-format-example ()
  (if texinfo-example-start
      (error "@%s starts inside another such environment"
	     texinfo-command-name))
  (setq texinfo-example-start texinfo-command-start)
  (texinfo-discard-line))

(put 'example 'texinfo-end 'texinfo-end-example)
(put 'quotation 'texinfo-end 'texinfo-end-example)
(put 'lisp 'texinfo-end 'texinfo-end-example)
(put 'display 'texinfo-end 'texinfo-end-example)
(put 'format 'texinfo-end 'texinfo-end-example)
(put 'flushleft 'texinfo-end 'texinfo-end-example)
(defun texinfo-end-example ()
  (texinfo-discard-line)
  (if (null texinfo-example-start)
      (error "@end %s with no beginning" texinfo-command-name))
  (save-excursion
   (while (progn (forward-line -1)
		 (>= (dot) texinfo-example-start))
     (if (= (following-char) ?\b)
	 (delete-char 1)
       (save-excursion (insert "    ")))))
  (setq texinfo-example-start nil))

(put 'exdent 'texinfo-format 'texinfo-format-exdent)
(defun texinfo-format-exdent ()
  (texinfo-discard-command)
  (delete-region (dot)
		 (progn
		  (forward-word 1)
		  (skip-chars-forward " ")))
  (insert ?\b))

(put 'ctrl 'texinfo-format 'texinfo-format-ctrl)
(defun texinfo-format-ctrl
  (let ((str (texinfo-parse-arg)))
    (insert (logand 31 (aref str 0)))))

(put 'TeX 'texinfo-format 'texinfo-format-TeX)
(defun texinfo-format-TeX ()
  (texinfo-discard-command)
  (insert "TeX"))

(put 'refill 'texinfo-format 'texinfo-format-refill)
(defun texinfo-format-refill ()
  (texinfo-discard-command)
  (fill-paragraph nil))

;; Lots of bolio constructs do nothing in texinfo.

(put 'c 'texinfo-format 'texinfo-discard-line-with-args)
(put 'comment 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setx 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setq 'texinfo-format 'texinfo-discard-line-with-args)
(put 'settitle 'texinfo-format 'texinfo-discard-line-with-args)
(put 'defindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'synindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'cindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'findex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'vindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'kindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'hsize 'texinfo-format 'texinfo-discard-line-with-args)
(put 'parindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'lispnarrowing 'texinfo-format 'texinfo-discard-line-with-args)
(put 'itemindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'titlepage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'copyrightpage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'copyrightpage 'texinfo-end 'texinfo-discard-line-with-args)
(put 'headings 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-end 'texinfo-discard-line-with-args)
(put 'bye 'texinfo-format 'texinfo-discard-line)

(defun texinfo-discard-line-with-args ()
  (goto-char texinfo-command-start)
  (delete-region (dot) (progn (forward-line 1) (dot))))

;; Some cannot be handled

(defun texinfo-unsupported ()
  (error "%s is not handled by texinfo"
	 (buffer-substring texinfo-command-start texinfo-command-end)))
