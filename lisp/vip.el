;; VIP: A VI Package for GNU Emacs (version 2.7 of February 10, 1987)

;; Author: Masahiko Sato (ms@sail.stanford.edu).  In Japan, the author's
;; address is: masahiko@nttlab.ntt.junet
;; Please send suggestions and bug reports to one of the above addresses.

;; Execute info command by typing "M-x info" to get information on VIP.

;; external variables

(defvar emacs-local-map nil
  "Local map used in emacs mode. \(buffer specific\)")

(defvar insert-local-map nil
  "Local map used in insert command mode. \(buffer specific\)")
  
(make-variable-buffer-local 'emacs-local-map)
(make-variable-buffer-local 'insert-local-map)

(defvar insert-point nil
  "Remember insert point as a marker. \(buffer specific\)")

(set-default 'insert-point (make-marker))
(make-variable-buffer-local 'insert-point)

(defvar com-point nil
  "Remember com point as a marker. \(buffer specific\)")

(set-default 'com-point (make-marker))
(make-variable-buffer-local 'com-point)

(defvar current-mode nil
  "Current mode.  One of emacs-mode, vi-mode, insert-mode.")

(make-variable-buffer-local 'current-mode)
(set-default 'current-mode 'emacs-mode)

(defvar current-major-mode nil
  "current-major-mode is the major-mode vi considers it is now.
\(buffer specific\)")

(make-variable-buffer-local 'current-major-mode)

(defvar vi-last-shell-com nil
  "last shell command executed by ! command")

(defvar use-register nil
  "name of register to store deleted or yanked strings.")

(defvar d-com nil
  "If non-nil, it's value is a list (M-COM VAL COM), and is used to
re-execute last destrcutive command")

(defconst shift-width 8
  "*The number of colums shifted by > and < command.")

(defconst re-replace nil
  "*If t then do regexp replace, if nil then do string replace.")

(defvar d-char nil
  "The character remenbered by the vi \"r\" command")

(defvar f-char nil
  "for use by \";\" command")

(defvar F-char nil
  "for use by \".\" command")

(defvar f-forward nil
  "for use by \";\" command")
  
(defvar f-offset nil
  "for use by \";\" command")

(defconst vi-search-wrap-around t
  "*if t, search wraps around")

(defconst re-search nil
  "*if t, search is reg-exp search, otherwise vanilla search.")

(defvar s-string nil
  "last search string")

(defvar s-forward nil
  "if t, search is forward.")

(defconst vi-case-fold-search nil
  "*if t, search ignores cases.")

(defconst re-query-replace nil
  "*If t then do regexp replace, if nil then do string replace.")

(defconst vi-open-with-indent nil
  "*if t, indent when open a new line.")

(defconst help-in-insert-mode nil
  "*if t then C-h is bound to help-command in insert mode, if nil then it is
bound to delete-backward-char.")

(defvar vi-quote-string "> "
  "string inserted at the beginning of region")

(setq tags-file-name "TAGS")


;; basic set up

(global-set-key "\C-z" 'change-mode-to-vi)

(defmacro loop (count body)
  "(COUNT BODY) Execute BODY COUNT times."
  (list 'let (list (list 'count count))
	(list 'while (list '> 'count 0)
	      body
	      (list 'setq 'count (list '1- 'count)))))

(defun push-mark-silent (&optional location)
  "Set mark at location (point, by default) and push old mark on mark ring.
No message."
  (if (null (mark))
      nil
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point))))

(defun vi-goto-col (arg)
  "(ARG)  Go to ARG's column."
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (save-excursion
      (end-of-line)
      (if (> val (1+ (current-column))) (error "")))
    (if com (move-marker com-point (point)))
    (beginning-of-line)
    (forward-char (1- val))
    (if com (execute-com 'vi-goto-col val com))))

(defun refresh-mode-line ()
  "Redraw mode line."
  (set-buffer-modified-p (buffer-modified-p)))

(defun vi-copy-keymap (map)
  (if (null map) (make-sparse-keymap) (copy-keymap map)))


;; changing mode

(defun change-mode (new-mode)
  "(NEW-MODE)  Change mode to NEW-MODE.  NEW-MODE is either emacs-mode,
vi-mode, or insert-mode."
  (or (eq new-mode current-mode)
      (progn
	(cond ((eq new-mode 'vi-mode)
	       (if (eq current-mode 'insert-mode)
		   (progn
		     (vi-copy-region-as-kill (point) insert-point)
		     (repeat-insert-command))
		 (setq emacs-local-map (current-local-map)
		       insert-local-map (vi-copy-keymap (current-local-map))))
	       (change-mode-line "Vi:   ")
	       (use-local-map vi-command-mode-map))
	      ((eq new-mode 'insert-mode)
	       (move-marker insert-point (point))
	       (if (eq current-mode 'emacs-mode)
		   (setq emacs-local-map (current-local-map)
			 insert-local-map
			 (vi-copy-keymap (current-local-map))))
	       (change-mode-line "Insert")
	       (use-local-map insert-local-map)
	       (define-key insert-local-map "\e" 'change-mode-to-vi)
	       (define-key insert-local-map "\C-z" 'vi-ESC)
	       (define-key insert-local-map "\C-h"
		 (if help-in-insert-mode 'help-command 'delete-backward-char))
	       (define-key insert-local-map "\C-w" 'delete-backward-word))
	      ((eq new-mode 'emacs-mode) 
	       (change-mode-line "Emacs:")
	       (use-local-map emacs-local-map)))
	(setq current-mode new-mode)
	(refresh-mode-line))))

(defun vi-copy-region-as-kill (beg end)
  "(BEG END)  If BEG and END do not belong to the same buffer, it copies
empty region."
  (condition-case nil
      (copy-region-as-kill beg end)
    (error (copy-region-as-kill beg beg))))

(defun change-mode-line (string)
  "Assuming that the mode line format contains the string \"Emacs:\", this
function replaces the string by \"Vi:   \" etc."
  (setq mode-line-buffer-identification
	(list (concat string " %17b"))))

(defun vip-mode ()
  "Turn on VIP emulation of VI."
  (interactive)
  (change-mode-to-vi))

(defun change-mode-to-vi ()
  "()  Change mode to vi."
  (interactive)
  (change-mode 'vi-mode))

(defun change-mode-to-insert ()
  "()  Change mode to insert."
  (interactive)
  (change-mode 'insert-mode))

(defun change-mode-to-emacs ()
  "()  Change mode to emacs."
  (interactive)
  (change-mode 'emacs-mode))


;; escape to emacs mode termporarilly

(defun get-editor-command (l-map g-map &optional str)
  "(L-MAP G-MAP STR)  Read characters from keyboard until an editor command
is formed, using local keymap L-MAP and global keymap G-MAP.  If the
command is a self-insert-command, the character just read is returned
instead.  Optional string STR is used as initial input string."
  (let (char l-bind g-bind)
    (setq char
	  (if (or (null str) (string= str ""))
	      (read-char)
	    (string-to-char str)))
    (setq last-command-char char)
    (setq l-bind (binding-of char l-map))
    (if (null l-bind)
	;; since local binding is empty, we concentrate on global one.
	(progn
	  (setq g-bind (binding-of char g-map))
	  (if (null g-bind)
	      nil ;; return nil, since both bindings are void.
	    (if (keymapp g-bind)
		(get-editor-command nil g-bind (string-tail str))
	      (if (eq g-bind 'self-insert-command) char g-bind))))
      ;; local binding is nonvoid
      (if (keymapp l-bind)
	  ;; since l-bind is a keymap, we consider g-bind as well.
	  (progn
	    (setq g-bind (binding-of char g-map))
	    (if (null g-bind)
		(get-editor-command l-bind nil (string-tail str))
	      (if (keymapp g-bind)
		  ;; both bindings are keymap
		  (get-editor-command l-bind g-bind (string-tail str))
		;; l-bind is a keymap, so we neglect g-bind
		(get-editor-command l-bind nil (string-tail str)))))
	;; l-bind is a command
	(if (eq l-bind 'self-insert-command) char l-bind)))))

(defun binding-of (char map)
  "(CHAR MAP)  Return key-binding of CHAR under keymap MAP.  It is nil if
the binding is void, or a command, or a keymap"
  (let ((val (if (listp map)
		 (cdr (assq char map))
	       (aref map char))))
    (cond ((null val) nil)
	  ((keymapp val)
	   (if (symbolp val) (symbol-function val) val))
	  (t
	   ;; otherwise, it is a function which is either a real function or
	   ;; a keymap fset to val.
	   (let ((fun (symbol-function val)))
	     (if (or (null fun) (keymapp fun)) fun val))))))

(defun escape-to-emacs (arg &optional char)
  "(ARG &optional CHAR)  Escape to emacs mode and execute one emacs
command and then return to vi mode.  ARG is used as the prefix value
for the executed command.  If CHAR is given it becomes the first
character of the command."
  (interactive "P")
  (let (com (buff (current-buffer)) (first t))
    (if char (setq unread-command-char char))
    (setq prefix-arg arg)
    (while (or first (>= unread-command-char 0))
      ;; this while loop is executed until unread command char will be
      ;; exhausted.
      (setq first nil)
      (setq com (get-editor-command emacs-local-map global-map))
      (if (numberp com)
	  (loop (p-val prefix-arg) (insert-string (char-to-string com)))
	(command-execute com prefix-arg)))
    (setq prefix-arg nil)  ;; reset prefix arg
    ))

(defun message-conditions (conditions)
  "Print conditions as a message."
  (let ((case (car conditions)) (msg (cdr conditions)))
    (if (null msg)
	(message "%s" case)
      (message "%s %s" case (prin1-to-string msg)))
    (ding)))

(defun vi-ESC (arg) ""
  (interactive "P")
  (escape-to-emacs arg ?\e))

(defun vi-ctl-c (arg) ""
  (interactive "P")
  (escape-to-emacs arg ?\C-c))

(defun vi-ctl-x (arg) ""
  (interactive "P")
  (escape-to-emacs arg ?\C-x))

(defun vi-ctl-h (arg) ""
  (interactive "P")
  (escape-to-emacs arg ?\C-h))


;; prefix argmument for vi mode

;; In vi mode, prefix argument is a dotted pair (NUM . COM) where NUM
;; represents the numeric value of the prefix argument and COM represents
;; command prefix such as "c", "d", "m" and "y".

(defun vi-prefix-arg-value (char value com)
  "(CHAR VALUE COM)  Compute numeric prefix arg value.  Invoked by
CHAR.  VALUE is the value obtained so far, and COM is the command part
obtained so far."
  (while (and (>= char ?0) (<= char ?9))
    (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)))	    
    (setq char (read-char)))
  (setq prefix-arg value)
  (if com (setq prefix-arg (cons prefix-arg com)))
  (while (= char ?U)
    (describe-arg prefix-arg)
    (setq char (read-char)))
  (setq unread-command-char char))

(defun vi-prefix-arg-com (char value com)
  "Vi operator as prefix argument."
  (let ((cont t))
    (while (and cont
		(or (= char ?c) (= char ?d) (= char ?y)
		    (= char ?!) (= char ?<) (= char ?>) (= char ?=)
		    (= char ?#) (= char ?r) (= char ?R) (= char ?\")))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
	    (if (or (= char ?#) (= char ?\")) (error ""))
	    (setq com (cons char com))
	    (setq cont nil))
	;; if com is nil we set com as char, and read more.  again, if char
	;; is ", we read the name of register and store it in use-register.
	;; if char is !, =, or #, a copmlete com is formed so we exit while.
	(cond ((or (= char ?!) (= char ?=))
	       (setq com char)
	       (setq char (read-char))
	       (setq cont nil))
	      ((= char ?#)
	       ;; read a char and encode it as com
	       (setq com (+ 128 (read-char)))
	       (setq char (read-char))
	       (setq cont nil))
	      ((or (= char ?<) (= char ?>))
	       (setq com char)
	       (setq char (read-char))
	       (if (= com char) (setq com (cons char com)))
	       (setq cont nil))
	      ((= char ?\")
	       (let ((reg (read-char)))
		 (if (or (and (<= ?A reg) (<= reg ?z))
			 (and (<= ?1 reg) (<= reg ?9)))
		     (setq use-register reg)
		   (error ""))
		 (setq char (read-char))))
	      (t
	       (setq com char)
	       (setq char (read-char)))))))
  (if (atom com)
      ;; com is a single char, so we construct prefix-arg 
      ;; and if char is ?, describe prefix arg, otherwise exit by
      ;; pushing the char back
      (progn
	(setq prefix-arg (cons value com))
	(while (= char ?U)
	  (describe-arg prefix-arg)
	  (setq char (read-char)))
	(setq unread-command-char char))
    ;; as com is non-nil, this means that we have a command to execute
    (if (or (= (car com) ?r) (= (car com) ?R))
	;; execute apropriate region command.
	(let ((char (car com)) (com (cdr com)))
	  (setq prefix-arg (cons value com))
	  (if (= char ?r) (vi-region prefix-arg)
	    (vi-Region prefix-arg))
	  ;; reset prefix-arg
	  (setq prefix-arg nil))
      ;; otherwise, reset prefix arg and call appropriate command
      (setq value (if (null value) 1 value))
      (setq prefix-arg nil)
      (cond ((equal com '(?c . ?c)) (vi-line (cons value ?C)))
	    ((equal com '(?d . ?d)) (vi-line (cons value ?D)))
	    ((equal com '(?d . ?y)) (vi-yank-defun))
	    ((equal com '(?y . ?y)) (vi-line (cons value ?Y)))
	    ((equal com '(?< . ?<)) (vi-line (cons value ?<)))
	    ((equal com '(?> . ?>)) (vi-line (cons value ?>)))
	    ((equal com '(?! . ?!)) (vi-line (cons value ?!)))
	    ((equal com '(?= . ?=)) (vi-line (cons value ?=)))
	    (t (error ""))))))

(defun describe-arg (arg)
  (let (val com)
    (setq val (P-val arg)
	  com (getcom arg))
    (if (null val)
	(if (null com)
	    (message "Value is nil, and commmand is nil.")
	  (message "Value is nil, and command is %c." com))
      (if (null com)
	  (message "Value is %d, and command is nil." val)
	(message "Value is %d, and command is %c." val com)))))

(defun vi-digit-argument (arg)
  "Begin numeric argument for the next command."
  (interactive "P")
  (vi-prefix-arg-value last-command-char nil
		       (if (consp arg) (cdr arg) nil)))

(defun vi-command-argument (arg)
  (interactive "P")
  (condition-case conditions
      (vi-prefix-arg-com
       last-command-char   
       (cond ((null arg) nil)
	     ((consp arg) (car arg))
	     ((numberp arg) arg)
	     (t (error "strange arg")))
       (cond ((null arg) nil)
	     ((consp arg) (cdr arg))
	     ((numberp arg) nil)
	     (t (error "strange arg"))))
    (quit
     (setq use-register nil)
     (signal 'quit nil))))

(defun p-val (arg)
  "(ARG)  Get value part of prefix-argument ARG."
  (cond ((null arg) 1)
	((consp arg) (if (null (car arg)) 1 (car arg)))
	(t arg)))

(defun P-val (arg)
  "(ARG)  Get value part of prefix-argument ARG."
  (cond ((consp arg) (car arg))
	(t arg)))

(defun getcom (arg)
  "(ARG)  Get com part of prefix-argument ARG."
  (cond ((null arg) nil)
	((consp arg) (cdr arg))
	(t nil)))

(defun getCom (arg)
  "(ARG)  Get com part of prefix-argument ARG and modify it."
  (let ((com (getcom arg)))
    (cond ((equal com ?c) ?C)
	  ((equal com ?d) ?D)
	  ((equal com ?y) ?Y)
	  (t com))))


;; repeat last destructive command

(defun vi-append-to-register (reg start end)
  "Append region to text in register REG.
START and END are buffer positions indicating what to append."
  (set-register reg (concat (or (get-register reg) "")
			    (buffer-substring start end))))

(defun execute-com (m-com val com)
  "(M-COM VAL COM)  Execute command COM. The list (M-COM VAL COM) is set
to d-com for later use by vi-repeat"
  (let ((reg use-register))
    (if com
	(cond ((= com ?c) (vi-change com-point (point)))
	      ((= com (- ?c)) (vi-change-subr com-point (point)))
	      ((or (= com ?C) (= com (- ?C)))
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (if use-register
		     (progn
		       (cond ((and (<= ?a use-register) (<= use-register ?z))
			      (copy-to-register
			       use-register (mark) (point) nil))
			     ((and (<= ?A use-register) (<= use-register ?Z))
			      (vi-append-to-register
			       (+ use-register 32) (mark) (point)))
			     (t (setq use-register nil)
				(error "")))
		       (setq use-register nil)))
		 (delete-region (mark) (point)))
	       (open-line 1)
	       (if (= com ?C) (change-mode-to-insert) (yank)))
	      ((= com ?d)
	       (if use-register
		   (progn
		     (cond ((and (<= ?a use-register) (<= use-register ?z))
			    (copy-to-register
			     use-register com-point (point) nil))
			   ((and (<= ?A use-register) (<= use-register ?Z))
			    (vi-append-to-register
			     (+ use-register 32) com-point (point)))
			   (t (setq use-register nil)
			      (error "")))
		     (setq use-register nil)))
	       (setq last-command
		     (if (eq last-command 'd-command) 'kill-region nil))
	       (kill-region com-point (point))
	       (setq this-command 'd-command))
	      ((= com ?D)
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (if use-register
		     (progn
		       (cond ((and (<= ?a use-register) (<= use-register ?z))
			      (copy-to-register
			       use-register (mark) (point) nil))
			     ((and (<= ?A use-register) (<= use-register ?Z))
			      (vi-append-to-register
			       (+ use-register 32) (mark) (point)))
			     (t (setq use-register nil)
				(error "")))
		       (setq use-register nil)))
		 (setq last-command
		       (if (eq last-command 'D-command) 'kill-region nil))
		 (kill-region (mark) (point))
		 (if (eq m-com 'vi-line) (setq this-command 'D-command)))
	       (back-to-indentation))
	      ((= com ?y)
	       (if use-register
		   (progn
		     (cond ((and (<= ?a use-register) (<= use-register ?z))
			    (copy-to-register
			     use-register com-point (point) nil))
			   ((and (<= ?A use-register) (<= use-register ?Z))
			    (vi-append-to-register
			     (+ use-register 32) com-point (point)))
			   (t (setq use-register nil)
			      (error "")))
		     (setq use-register nil)))
	       (setq last-command nil)
	       (copy-region-as-kill com-point (point))
	       (goto-char com-point))
	      ((= com ?Y)
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (if use-register
		     (progn
		       (cond ((and (<= ?a use-register) (<= use-register ?z))
			      (copy-to-register
			       use-register (mark) (point) nil))
			     ((and (<= ?A use-register) (<= use-register ?Z))
			      (vi-append-to-register
			       (+ use-register 32) (mark) (point)))
			     (t (setq use-register nil)
				(error "")))
		       (setq use-register nil)))
		 (setq last-command nil)
		 (copy-region-as-kill (mark) (point)))
	       (goto-char com-point))
	      ((or (= com ?!) (= com (- ?!)))
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (shell-command-on-region
		  (mark) (point)
		  (if (= com ?!)
		      (setq vi-last-shell-com (vi-read-string "!"))
		    vi-last-shell-com)
		  t)))
	      ((= com ?=)
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (if (> (mark) (point)) (exchange-point-and-mark))
		 (indent-region (mark) (point) nil)))
	      ((= com ?<)
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (indent-rigidly (mark) (point) (- shift-width)))
	       (goto-char com-point))
	      ((= com ?>)
	       (save-excursion
		 (set-mark com-point)
		 (enlarge-region (mark) (point))
		 (indent-rigidly (mark) (point) shift-width))
	       (goto-char com-point))
	      ((>= com 128)
	       ;; this is special command #
	       (special-prefix-com (- com 128)))))
    (setq d-com (list m-com val (if (or (= com ?c) (= com ?C) (= com ?!))
				    (- com) com)
		      reg))))

(defun vi-repeat (arg)
  "(ARG)  Re-excute last destructive command.  d-com has the form
  (COM ARG CH REG), where COM is the command to be re-executed, ARG is the
argument for COM, CH is a flag for repeat, and REG is optional and if exists
is the name of the register for COM."
  (interactive "P")
  (if (eq last-command 'vi-undo)
      ;; if the last command was vi-undo, then undo-more
      (vi-undo-more)
    ;; otherwise execute the command stored in d-com.  if arg is non-nil
    ;; its prefix value is used as new prefix value for the command.
    (let ((m-com (car d-com))
	  (val (P-val arg))
	  (com (car (cdr (cdr d-com))))
	  (reg (nth 3 d-com)))
      (if (null val) (setq val (car (cdr d-com))))
      (if (null m-com) (error "No previous command to repeat."))
      (setq use-register reg)
      (funcall m-com (cons val com)))))

(defun special-prefix-com (char)
  "This command is invoked interactively by the key sequence #<char>"
  (cond ((= char ?c)
	 (downcase-region (min com-point (point)) (max com-point (point))))
	((= char ?C)
	 (upcase-region (min com-point (point)) (max com-point (point))))
	((= char ?g)
	 (set-mark com-point)
	 (global-execute))
	((= char ?q)
	 (set-mark com-point)
	 (quote-region))
	((= char ?s) (spell-region com-point (point)))))


;; undoing

(defun vi-undo () ""
  (interactive)
  (message "undo!")
  (undo-start)
  (undo-more 2)
  (setq this-command 'vi-undo))

(defun vi-undo-more () ""
  (message "undo more!")
  (undo-more 1)
  (setq this-command 'vi-undo))


;; utilities

(defun string-tail (str)
  (if (or (null str) (string= str "")) nil
    (substring str 1)))

(defun vi-yank-defun ()
  (mark-defun)
  (copy-region-as-kill (point) (mark)))

(defun enlarge-region (beg end)
  "(BEG END)  Enlarge region between BEG and END."
  (if (< beg end)
      (progn (goto-char beg) (set-mark end))
    (goto-char end)
    (set-mark beg))
  (beginning-of-line)
  (exchange-point-and-mark)
  (if (or (not (eobp)) (not (bolp))) (next-line 1))
  (beginning-of-line)
  (if (> beg end) (exchange-point-and-mark)))

(defun global-execute ()
  "Call last keyboad macro for each line in the region."
  (if (> (point) (mark)) (exchange-point-and-mark))
  (beginning-of-line)
  (call-last-kbd-macro)
  (while (< (point) (mark))
    (forward-line 1)
    (beginning-of-line)
    (call-last-kbd-macro)))

(defun quote-region ()
  "Quote region by inserting the user supplied string at the beginning of
each line in the region."
  (setq vi-quote-string
	(let ((str
	       (vi-read-string (format "quote string \(default \"%s\"\): "
				       vi-quote-string))))
	  (if (string= str "") vi-quote-string str)))
  (enlarge-region (point) (mark))
  (if (> (point) (mark)) (exchange-point-and-mark))
  (insert-string vi-quote-string)
  (beginning-of-line)
  (forward-line 1)
  (while (and (< (point) (mark)) (bolp))
    (insert-string vi-quote-string)
    (beginning-of-line)
    (forward-line 1)))

(defun end-with-a-newline-p (string)
  "Check if the string ends with a newline."
  (or (string= text "")
      (= (aref string (1- (length string))) ?\n)))

(defun vi-read-string (prompt &optional init)
  (setq save-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key minibuffer-local-map "\C-h" 'backward-char)
  (define-key minibuffer-local-map "\C-w" 'backward-word)
  (define-key minibuffer-local-map "\e" 'exit-minibuffer)
  (let (str)
    (condition-case conditions
	(setq str (read-string prompt init))
      (quit
       (setq minibuffer-local-map save-minibuffer-local-map)
       (signal 'quit nil)))
    (setq minibuffer-local-map save-minibuffer-local-map)
    str))


;; insertion commands

(defun repeat-insert-command ()
  "()  This function is called when mode changes from insertion mode to
vi command mode.  It will repeat the insertion command if original insertion
command was invoked with argument > 1."
  (let ((i-com (car d-com)) (val (car (cdr d-com))))
    (if (and val (> val 1)) ;; first check that val is non-nil
	(progn        
	  (setq d-com (list i-com (1- val) ?r))
	  (vi-repeat nil)
	  (setq d-com (list i-com val ?r))))))

(defun vi-insert (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-insert val ?r))
    (if com (loop val (yank))
      (change-mode-to-insert))))

(defun vi-append (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-append val ?r))
    (if (not (eolp)) (forward-char))
    (if (equal com ?r)
	(loop val (yank))
      (change-mode-to-insert))))

(defun vi-Append (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-Append val ?r))
    (end-of-line)
    (if (equal com ?r)
	(loop val (yank))
      (change-mode-to-insert))))

(defun vi-Insert (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-Insert val ?r))
    (back-to-indentation)
    (if (equal com ?r)
	(loop val (yank))
      (change-mode-to-insert))))

(defun vi-open-line (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-open-line val ?r))
    (let ((col (current-indentation)))
      (if (equal com ?r)
	  (loop val
		(progn
		  (end-of-line)
		  (newline 1)
		  (if vi-open-with-indent (indent-to col))
		  (yank)))
	(end-of-line)
	(newline 1)
	(if vi-open-with-indent (indent-to col))
	(change-mode-to-insert)))))

(defun vi-Open-line (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
  (setq d-com (list 'vi-Open-line val ?r))
  (let ((col (current-indentation)))
    (if (equal com ?r)
	(loop val
	      (progn
		(beginning-of-line)
		(open-line 1)
		(if vi-open-with-indent (indent-to col))
		(yank)))
      (beginning-of-line)
      (open-line 1)
      (if vi-open-with-indent (indent-to col))
      (change-mode-to-insert)))))

(defun vi-ctl-open-line (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-ctl-open-line val ?r))
    (if (equal com ?r)
	(loop val
	      (progn
		(open-line 1)
		(yank)))
      (open-line 1)
      (change-mode-to-insert))))

(defun vi-substitute (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (save-excursion
      (set-mark (point))
      (forward-char val)
      (if (equal com ?r)
	  (vi-change-subr (mark) (point))
	(vi-change (mark) (point))))
    (setq d-com (list 'vi-substitute val ?r))))
  
(defun vi-substitute-line (arg)
  (interactive "p")
  (vi-line (cons arg ?C)))


;; line command

(defun vi-line (arg)
  (let ((val (car arg)) (com (cdr arg)))
    (move-marker com-point (point))
    (next-line (1- val))
    (execute-com 'vi-line val com)))

(defun vi-yank-line (arg)
  "(ARG)  Yank ARG lines (in vi's sense)"
  (interactive "P")
  (let ((val (p-val arg)))
    (vi-line (cons val ?Y))))
    

;; region command

(defun vi-region (arg)
  (interactive "P")
  (let ((val (P-val arg))
	(com (getcom arg)))
    (move-marker com-point (point))
    (exchange-point-and-mark)
    (execute-com 'vi-region val com)))

(defun vi-Region (arg)
  (interactive "P")
  (let ((val (P-val arg))
	(com (getCom arg)))
    (move-marker com-point (point))
    (exchange-point-and-mark)
    (execute-com 'vi-Region val com)))
  
(defun vi-replace-char (arg)
  "(ARG)  Replace the following ARG chars by the character read."
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (setq d-com (list 'vi-replace-char val ?r))
    (vi-replace-char-subr (if (equal com ?r) d-char (read-char)) val)))

(defun vi-replace-char-subr (char arg)
  (delete-char arg t)
  (setq d-char char)
  (loop (if (> arg 0) arg (- arg)) (insert char))
  (backward-char arg))

(defun vi-replace-string ()
  "()  Replace string.  If you supply null string as the string to be
replaced, the query replace mode will toggle between string replace
and regexp replace."
  (interactive)
  (let (str)
    (setq str (vi-read-string
	       (if re-replace "Replace regexp: " "Replace string: ")))
    (if (string= str "")
	(progn
	  (setq re-replace (not re-replace))
	  (message (format "Replace mode changed to %s."
			   (if re-replace "regexp replace" "string replace"))))
      (if re-replace
	  (replace-regexp
	   str
	   (vi-read-string (format "Replace regexp \"%s\" with: " str)))
	(replace-string
	 str
	 (vi-read-string (format "Replace \"%s\" with: " str)))))))


;; basic cursor movement.  j, k, l, m commands.

(defun vi-forward-char (arg)
  "(ARG)  Move point right ARG characters (left if ARG negative).
On reaching end of buffer, stop and signal error."
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (forward-char val)
    (if com (execute-com 'vi-forward-char val com))))

(defun vi-backward-char (arg)
  "(ARG)  Move point left ARG characters (right if ARG negative).
On reaching beginning of buffer, stop and signal error."
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (backward-char val)
    (if com (execute-com 'vi-backward-char val com))))


;; word command

(defun vi-forward-word (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (forward-word val)
    (skip-chars-forward " \t\n")
    (if com
	(progn
	  (if (or (= com ?c) (= com (- ?c)))
	      (progn (backward-word 1) (forward-word 1)))
	  (if (or (= com ?d) (= com ?y))
	      (progn
		(backward-word 1)
		(forward-word 1)
		(skip-chars-forward " \t")))
	  (execute-com 'vi-forward-word val com)))))

(defun vi-end-of-word (arg)
  "move point to end of current word"
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (forward-char)
    (forward-word val)
    (backward-char)
    (if com
	(progn
	  (forward-char)
	  (execute-com 'vi-end-of-word val com)))))
			 
(defun vi-backward-word (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (backward-word val)
    (if com (execute-com 'vi-backward-word val com))))

(defun vi-forward-Word (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (re-search-forward "[^ \t\n]*[ \t\n]+" nil t val)
    (if com
	(progn
	  (if (or (= com ?c) (= com (- ?c)))
	      (progn (backward-word 1) (forward-word 1)))
	  (if (or (= com ?d) (= com ?y))
	      (progn
		(backward-word 1)
		(forward-word 1)
		(skip-chars-forward " \t")))
	  (execute-com 'vi-forward-Word val com)))))

(defun vi-end-of-Word (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (forward-char)
    (if (re-search-forward "[^ \t\n]+" nil t val) (backward-char))
    (if com
	(progn
	  (forward-char)
	  (execute-com 'vi-end-of-Word val com)))))

(defun vi-backward-Word (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (if (re-search-backward "[ \t\n]+[^ \t\n]+" nil t val)
	(forward-char)
      (goto-char (point-min)))
    (if com (execute-com 'vi-backward-Word val com))))

(defun vi-beginning-of-line (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (beginning-of-line val)
    (if com (execute-com 'vi-beginning-of-line val com))))

(defun vi-bol-and-skip-white (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (back-to-indentation)
    (if com (execute-com 'vi-bol-and-skip-white val com))))

(defun vi-goto-eol (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (end-of-line val)
    (if com (execute-com 'vi-goto-eol val com))))

(defun vi-next-line (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getCom arg)))
    (if com (move-marker com-point (point)))
    (next-line val)
    (setq this-command 'next-line)
    (if com (execute-com 'vi-next-line val com))))

(defun vi-next-line-at-bol (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getCom arg)))
    (if com (move-marker com-point (point)))
    (next-line val)
    (back-to-indentation)
    (if com (execute-com 'vi-next-line-at-bol val com))))

(defun vi-previous-line (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getCom arg)))
    (if com (move-marker com-point (point)))
    (next-line (- val))
    (setq this-command 'previous-line)
    (if com (execute-com 'vi-previous-line val com))))

(defun vi-previous-line-at-bol (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getCom arg)))
    (if com (move-marker com-point (point)))
    (next-line (- val))
    (back-to-indentation)
    (if com (execute-com 'vi-previous-line val com))))

(defun vi-change-to-eol (arg)
  (interactive "P")
  (vi-goto-eol (cons arg ?c)))

(defun vi-kill-line (arg)
  (interactive "P")
  (vi-goto-eol (cons arg ?d)))


;; moving around

(defun vi-goto-line (arg)
  (interactive "P")
  (let ((val (P-val arg)) (com (getCom arg)))
    (move-marker com-point (point))
    (set-mark (point))
    (if (null val)
	(goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- val)))
    (back-to-indentation)
    (if com (execute-com 'vi-goto-line val com))))

(defun vi-find-char (arg char forward offset)
  "(ARG CHAR FORWARD OFFSET)  Find ARG's occurence of CHAR on the
current line.  If FORWARD then search is forward, otherwise backward.
OFFSET is used to adjust point after search."
  (let ((arg (if forward arg (- arg))) point)
    (save-excursion
      (save-restriction
	(if (> arg 0)
	    (narrow-to-region
	     ;; forward search begins here
	     (if (eolp) (error "") (point))
	     ;; forward search ends here
	     (progn (next-line 1) (beginning-of-line) (point)))
	  (narrow-to-region
	   ;; backward search begins from here
	   (if (bolp) (error "") (point))
	   ;; backward search ends here
	   (progn (beginning-of-line) (point))))
	;; if arg > 0, point is forwarded before search.
	(if (> arg 0) (goto-char (1+ (point-min)))
	  (goto-char (point-max)))
	(setq point (scan-buffer (point) arg char))
	(if (or (and (> arg 0) (= point (point-max)))
		(and (< arg 0) (= point (point-min))))
	    (error ""))))
    (goto-char (+ point (if offset (if (> arg 0) -2 0) -1)))))

(defun vi-find-char-forward (arg)
  "(ARG)  Find char on the line.  If called interactively read the char
to find from the terminal, and if called from vi-repeat, the char
last used is used.  This behaviour is controlled by the sign of prefix
numeric value."
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq f-char (read-char)
	      f-forward t
	      f-offset nil)
      (setq val (- val)))
    (if com (move-marker com-point (point)))
    (vi-find-char val (if (> (p-val arg) 0) f-char F-char) t nil)
    (setq val (- val))
    (if com
	(progn
	  (setq F-char f-char);; remember new F-char
	  (forward-char)
	  (execute-com 'vi-find-char-forward val com)))))

(defun vi-goto-char-forward (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq f-char (read-char)
	      f-forward t
	      f-offset t)
      (setq val (- val)))
    (if com (move-marker com-point (point)))
    (vi-find-char val (if (> (p-val arg) 0) f-char F-char) t t)
    (setq val (- val))
    (if com
	(progn
	  (setq F-char f-char);; remember new F-char
	  (forward-char)
	  (execute-com 'vi-goto-char-forward val com)))))

(defun vi-find-char-backward (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq f-char (read-char)
	      f-forward nil
	      f-offset nil)
      (setq val (- val)))
    (if com (move-marker com-point (point)))
    (vi-find-char val (if (> (p-val arg) 0) f-char F-char) nil nil)
    (setq val (- val))
    (if com
	(progn
	  (setq F-char f-char);; remember new F-char
	  (execute-com 'vi-find-char-backward val com)))))

(defun vi-goto-char-backward (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq f-char (read-char)
	      f-forward nil
	      f-offset t)
      (setq val (- val)))
    (if com (move-marker com-point (point)))
    (vi-find-char val (if (> (p-val arg) 0) f-char F-char) nil t)
    (setq val (- val))
    (if com
	(progn
	  (setq F-char f-char);; remember new F-char
	  (execute-com 'vi-goto-char-backward val com)))))

(defun vi-semi-colon (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (vi-find-char val f-char f-forward f-offset)
    (if com
	(progn
	  (if f-forward (forward-char))
	  (execute-com 'vi-semi-colon val com)))))

(defun vi-comma (arg) ""
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if com (move-marker com-point (point)))
    (vi-find-char val f-char (not f-forward) f-offset)
    (if com
	(progn
	  (if f-forward (forward-char))
	  (execute-com 'vi-comma val com)))))


;; window scrolling etc.

(defun vi-other-window (arg)
  (interactive "p")
  (other-window arg)
  (or (not (eq current-mode 'emacs-mode))
      (string= (buffer-name (current-buffer)) " *Minibuf-1*")
      (change-mode-to-vi)))

(defun vi-window-top (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getCom arg)))
    (if com (move-marker com-point (point)))
    (move-to-window-line (1- val))
    (if com (execute-com 'vi-window-top val com))))

(defun vi-window-middle (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getCom arg)))
    (if com (move-marker com-point (point)))
    (move-to-window-line (+ (/ (1- (window-height)) 2) (1- val)))
    (if com (execute-com 'vi-window-middle val com))))

(defun vi-window-bottom (arg) ""
  (interactive "P")
  (let ((val (p-val arg))
	(com (getCom arg)))
    (if com (move-marker com-point (point)))
    (move-to-window-line (- val))
    (if com (execute-com 'vi-window-bottom val com))))

(defun vi-line-to-top (arg) ""
  (interactive "p")
  (recenter (1- arg)))

(defun vi-line-to-middle (arg) ""
  (interactive "p")
  (recenter (+ (1- arg) (/ (1- (window-height)) 2))))

(defun vi-line-to-bottom (arg) ""
  (interactive "p")
  (recenter (- (window-height) (1+ arg))))


;; paren match

(defun vi-paren-match (arg)
  "(ARG)  Go to the matching parenthesis."
  (interactive "P")
  (let ((com (getcom arg)))
    (cond ((looking-at "[\(\[{]")
	   (if com (move-marker com-point (point)))
	   (forward-sexp 1)
	   (if com
	       (execute-com 'vi-paren-match nil com)
	     (backward-char)))
	  ((looking-at "[])}]")
	   (forward-char)
	   (if com (move-marker com-point (point)))
	   (backward-sexp 1)
	   (if com (execute-com 'vi-paren-match nil com)))
	  (t (error "")))))


;; sentence and paragraph

(defun vi-forward-sentence (arg)
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (forward-sentence val)
    (if com (execute-com 'vi-forward-sentence nil com))))

(defun vi-backward-sentence (arg)
  (interactive "P")
  (let ((val (p-val arg))
	(com (getcom arg)))
    (if com (move-marker com-point (point)))
    (backward-sentence val)
    (if com (execute-com 'vi-backward-sentence nil com))))

(defun vi-forward-paragraph (arg)
  (interactive "P")
  (let ((val (p-val arg))
	(com (getCom arg)))
    (if com (move-marker com-point (point)))
    (forward-paragraph val)
    (if com (execute-com 'vi-forward-paragraph nil com))))

(defun vi-backward-paragraph (arg)
  (interactive "P")
  (let ((val (p-val arg))
	(com (getCom arg)))
    (if com (move-marker com-point (point)))
    (backward-paragraph val)
    (if com (execute-com 'vi-backward-paragraph nil com))))


;; scrolling

(defun vi-scroll (arg) ""
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
	(scroll-up)
	(setq arg (1- arg)))
    (while (> 0 arg)
      (scroll-down)
      (setq arg (1+ arg)))))

(defun vi-scroll-back (arg) ""
  (interactive "p")
  (vi-scroll (- arg)))

(defun vi-scroll-down (arg) ""
  (interactive "P")
  (if (null arg) (scroll-down (/ (window-height) 2))
    (scroll-down arg)))

(defun vi-scroll-down-one (arg) ""
  (interactive "p")
  (scroll-down arg))

(defun vi-scroll-up (arg) ""
  (interactive "P")
  (if (null arg) (scroll-up (/ (window-height) 2))
    (scroll-up arg)))

(defun vi-scroll-up-one (arg) ""
  (interactive "p")
  (scroll-up arg))


;; splitting window

(defun buffer-in-two-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically nil))


;; searching

(defun vi-search-forward (arg)
  "(ARG) Search a string forward.  ARG is used to find the ARG's occurence
of the string.  Default is vanilla search.  Search mode can be toggled by
giving null search string."
  (interactive "P")
  (let ((val (P-val arg)) (com (getcom arg)))
    (setq s-forward t
	  s-string (vi-read-string (if re-search "RE-/" "/")))
    (if (string= s-string "")
	(progn
	  (setq re-search (not re-search))
	  (message (format "Search mode changed to %s search."
			   (if re-search "regular expression"
			     "vanilla"))))
      (vi-search s-string t val)
      (if com
	  (progn
	    (move-marker com-point (mark))
	    (execute-com 'vi-search-next val com))))))

(defun vi-search-backward (arg)
  "(ARG) Search a string backward.  ARG is used to find the ARG's occurence
of the string.  Default is vanilla search.  Search mode can be toggled by
giving null search string."
  (interactive "P")
  (let ((val (P-val arg)) (com (getcom arg)))
    (setq s-forward nil
	  s-string (vi-read-string (if re-search "RE-?" "?")))
    (if (string= s-string "")
	(progn
	  (setq re-search (not re-search))
	  (message (format "Search mode changed to %s search."
			   (if re-search "regular expression"
			     "vanilla"))))
      (vi-search s-string nil val)
      (if com
	  (progn
	    (move-marker com-point (mark))
	    (execute-com 'vi-search-next val com))))))

(defun vi-search (string forward arg &optional no-offset init-point)
  "(STRING FORWARD COUNT &optional NO-OFFSET) Search COUNT's occurrence of
STRING.  Search will be forward if FORWARD, otherwise backward."
  (let ((val (p-val arg)) (com (getcom arg))
	(null-arg (null (P-val arg))) (offset (not no-offset))
	(case-fold-search vi-case-fold-search)
	(start-point (or init-point (point))))
    (if forward
	(condition-case conditions
	    (progn
	      (if (and offset (not (eobp))) (forward-char))
	      (if re-search
		  (progn
		    (re-search-forward string nil nil val)
		    (re-search-backward string))
		(search-forward string nil nil val)
		(search-backward string))
	      (push-mark start-point))
	  (search-failed
	   (if (and null-arg vi-search-wrap-around)
	       (progn
		 (goto-char (point-min))
		 (vi-search string forward (cons 1 com) t start-point))
	     (goto-char start-point)
	     (signal 'search-failed (cdr conditions)))))
      (condition-case conditions
	    (progn
	      (if re-search
		    (re-search-backward string nil nil val)
		(search-backward string nil nil val))
	      (push-mark start-point))
	  (search-failed
	   (if (and null-arg vi-search-wrap-around)
	       (progn
		 (goto-char (point-max))
		 (vi-search string forward (cons 1 com) t start-point))
	     (goto-char start-point)
	     (signal 'search-failed (cdr conditions))))))))

(defun vi-search-next (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if (null s-string) (error "No previous search string."))
    (vi-search s-string s-forward arg)
    (if com (execute-com 'vi-search-next val com))))

(defun vi-search-Next (arg)
  (interactive "P")
  (let ((val (p-val arg)) (com (getcom arg)))
    (if (null s-string) (error "No previous search string."))
    (vi-search s-string (not s-forward) arg)
    (if com (execute-com 'vi-search-Next val com))))


;; visiting and killing files, buffers

(defun vi-switch-to-buffer ()
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "switch to buffer \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer buffer)
    (change-mode-to-vi)))

(defun vi-switch-to-buffer-other-window ()
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "Switch to buffer \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer-other-window buffer)
    (change-mode-to-vi)))

(defun vi-kill-buffer ()
  (interactive)
  (let (buffer buffer-name)
    (setq buffer-name
	  (read-buffer
	   (format "Kill buffer \(%s\): "
		   (buffer-name (current-buffer)))))
    (setq buffer
	  (if (null buffer-name)
	      (current-buffer)
	    (get-buffer buffer-name)))
    (if (null buffer) (error "Buffer %s nonexistent." buffer-name))
    (if (buffer-modified-p buffer)
	(error "I can't kill modified buffer.")
      (kill-buffer buffer))))

(defun vi-find-file ()
  (interactive)
  (let (file)
    (setq file (read-file-name "visit file: "))
    (switch-to-buffer (find-file-noselect file))
    (change-mode-to-vi)))

(defun vi-find-file-other-window ()
  (interactive)
  (let (file)
    (setq file (read-file-name "Visit file: "))
    (switch-to-buffer-other-window (find-file-noselect file))
    (change-mode-to-vi)))

(defun vi-info-on-file ()
  "Give information of the file associated to the current buffer."
  (interactive)
  (message "\"%s\" line %d of %d"
	   (if (buffer-file-name) (buffer-file-name) "")
	   (1+ (count-lines (point-min) (point)))
	   (1+ (count-lines (point-min) (point-max)))))


;; yank and pop

(defun vi-yank (text)
  "yank TEXT silently."
  (save-excursion
    (push-mark-silent (point))
    (insert text)
    (exchange-point-and-mark))
  (skip-chars-forward " \t"))

(defun vi-put-back (arg)
  (interactive "P")
  (let ((val (p-val arg))
	(text (if use-register
		  (if (and (<= ?1 use-register) (<= use-register ?9))
		      (nth (- use-register 49) kill-ring-yank-pointer)
		    (get-register use-register))
		(car kill-ring-yank-pointer))))
    (if (null text)
	(if use-register
	    (let ((reg use-register))
	      (setq use-register nil)
	      (error "Nothing in register %c" reg))
	  (error "")))
    (setq use-register nil)
    (if (end-with-a-newline-p text)
	(progn
	  (next-line 1)
	  (beginning-of-line))
      (if (and (not (eolp)) (not (eobp))) (forward-char)))
    (setq d-com (list 'vi-put-back val nil use-register))
    (loop val (vi-yank text))))

(defun vi-Put-back (arg)
  (interactive "P")
  (let ((val (p-val arg))
	(text (if use-register
		  (if (and (<= ?1 use-register) (<= use-register ?9))
		      (nth (- use-register 49) kill-ring-yank-pointer)
		    (get-register use-register))
		(car kill-ring-yank-pointer))))
    (if (null text)
	(if use-register
	    (let ((reg use-register))
	      (setq use-register nil)
	      (error "Nothing in register %c" reg))
	  (error "")))
    (setq use-register nil)
    (if (end-with-a-newline-p text) (beginning-of-line))
    (setq d-com (list 'vi-Put-back val nil use-register))
    (loop val (vi-yank text))))

(defun vi-delete-char (arg)
  (interactive "P")
  (let ((val (p-val arg)))
    (setq d-com (list 'vi-delete-char val nil))
    (if use-register
	(progn
	  (if (and (<= ?A use-register) (<= use-register ?Z))
	      (vi-append-to-register
	       (+ use-register 32) (point) (- (point) val) nil)
	    (copy-to-register use-register (point) (- (point) val) nil))
	  (setq use-register nil)))
    (delete-char val t)))

(defun vi-delete-backward-char (arg)
  (interactive "P")
  (let ((val (p-val arg)))
    (setq d-com (list 'vi-delete-backward-char val nil))
    (if use-register
	(progn
	  (if (and (<= ?A use-register) (<= use-register ?Z))
	      (vi-append-to-register
	       (+ use-register 32) (point) (+ (point) val) nil)
	    (copy-to-register use-register (point) (+ (point) val) nil))
	  (setq use-register nil)))
    (delete-backward-char val t)))


;; join lines.

(defun vi-join-lines (arg)
  "(ARG)  Join this line to next, if ARG is nil.  Otherwise, join
 ARG lines"
  (interactive "*P")
  (let ((val (P-val arg)))
    (setq d-com (list 'vi-join-lines val nil))
    (loop (if (null val) 1 (1- val))
	  (progn
	    (end-of-line)
	    (if (not (eobp))
		(progn
		  (forward-line 1)
		  (delete-region (point) (1- (point)))
		  (fixup-whitespace)))))))


;; making small changes

(defun vi-change (beg end)
  (setq c-string
	(vi-read-string (format "%s => " (buffer-substring beg end))))
  (vi-change-subr beg end))

(defun vi-change-subr (beg end)
  (if use-register
      (progn
	(copy-to-register use-register beg end nil)
	(setq use-register nil)))
  (kill-region beg end)
  (setq this-command 'vi-change)
  (insert-string c-string))


;; query replace

(defun vi-query-replace ()
  "()  Query replace.  If you supply null string as the string to be
replaced, the query replace mode will toggle between string replace
and regexp replace."
  (interactive)
  (let (str)
    (setq str (vi-read-string
	       (if re-query-replace "Query replace regexp: "
		 "Query replace: ")))
    (if (string= str "")
	(progn
	  (setq re-query-replace (not re-query-replace))
	  (message "Query replace mode changed to %s."
		   (if re-query-replace "regexp replace" "string replace")))
      (if re-query-replace
	  (query-replace-regexp
	   str
	   (vi-read-string (format "Query replace regexp \"%s\" with: " str)))
	(query-replace
	 str
	 (vi-read-string (format "Query replace \"%s\" with: " str)))))))


;; marking

(defun vi-mark-beginning-of-buffer ()
  (interactive)
  (set-mark (point))
  (goto-char (point-min))
  (exchange-point-and-mark)
  (message "mark set at the beginning of buffer"))

(defun vi-mark-end-of-buffer ()
  (interactive)
  (set-mark (point))
  (goto-char (point-max))
  (exchange-point-and-mark)
  (message "mark set at the end of buffer"))

(defun vi-mark-point (char)
  (interactive "c")
  (cond ((and (<= ?a char) (<= char ?z))
	 (point-to-register (- char (- ?a ?\C-a))))
	((= char ?<) (vi-mark-beginning-of-buffer))
	((= char ?>) (vi-mark-end-of-buffer))
	((= char ?.) (push-mark))
	((= char ?,) (set-mark-command 1))
	((= char ?D) (mark-defun))
	(t (error ""))))

(defun vi-goto-mark (arg)
  (interactive "P")
  (let ((char (read-char)) (com (getcom arg)))
    (vi-goto-mark-subr char com nil)))

(defun vi-goto-mark-and-skip-white (arg)
  (interactive "P")
  (let ((char (read-char)) (com (getCom arg)))
    (vi-goto-mark-subr char com t)))

(defun vi-goto-mark-subr (char com skip-white)
  (cond ((and (<= ?a char) (<= char ?z))
	 (let ((buff (current-buffer)))
	   (if com (move-marker com-point (point)))
	   (goto-char (register-to-point (- char (- ?a ?\C-a))))
	   (if skip-white (back-to-indentation))
	   (change-mode-to-vi)
	   (if com
	       (if (equal buff (current-buffer))
		   (execute-com (if skip-white
				    'vi-goto-mark-and-skip-white
				  'vi-goto-mark)
				nil com)
		 (switch-to-buffer buff)
		 (goto-char com-point)
		 (change-mode-to-vi)
		 (error "")))))
	((and (not skip-white) (= char ?`))
	 (if com (move-marker com-point (point)))
	 (exchange-point-and-mark)
	 (if com (execute-com 'vi-goto-mark nil com)))
	((and skip-white (= char ?'))
	 (if com (move-marker com-point (point)))
	 (exchange-point-and-mark)
	 (back-to-indentation)
	 (if com (execute-com 'vi-goto-mark-and-skip-white nil com)))
	(t (error ""))))

(defun vi-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark)
  (back-to-indentation))

(defun vi-keyboard-quit ()
  (interactive)
  (setq use-register nil)
  (keyboard-quit))

(defun ctl-c-equivalent (arg)
  (interactive "P")
  (ctl-key-equivalent "\C-c" arg))

(defun ctl-x-equivalent (arg)
  (interactive "P")
  (ctl-key-equivalent "\C-x" arg))

(defun ctl-key-equivalent (key arg)
  (let ((char (read-char)))
    (if (and (<= ?A char) (<= char ?Z))
	(setq char (- char (- ?A ?\C-a))))
	  (setq prefix-arg arg)
	  (command-execute
	   (get-editor-command emacs-local-map global-map
			       (format "%s%s" key (char-to-string char))))))
  

;; commands in insertion mode

(defun delete-backward-word (arg)
  (interactive "p")
  (save-excursion
    (set-mark (point))
    (backward-word arg)
    (delete-region (point) (mark))))


;; key bindings

(set 'vi-command-mode-map (make-keymap))


(define-key vi-command-mode-map "\C-a" 'beginning-of-line)
(define-key vi-command-mode-map "\C-b" 'vi-scroll-back)
(define-key vi-command-mode-map "\C-c" 'vi-ctl-c)
(define-key vi-command-mode-map "\C-d" 'vi-scroll-up)
(define-key vi-command-mode-map "\C-e" 'vi-scroll-up-one)
(define-key vi-command-mode-map "\C-f" 'vi-scroll)
(define-key vi-command-mode-map "\C-g" 'vi-keyboard-quit)
(define-key vi-command-mode-map "\C-h" 'help-command)
(define-key vi-command-mode-map "\C-m" 'vi-scroll-back)
(define-key vi-command-mode-map "\C-n" 'vi-other-window)
(define-key vi-command-mode-map "\C-o" 'vi-ctl-open-line)
(define-key vi-command-mode-map "\C-u" 'vi-scroll-down)
(define-key vi-command-mode-map "\C-x" 'vi-ctl-x)
(define-key vi-command-mode-map "\C-y" 'vi-scroll-down-one)
(define-key vi-command-mode-map "\C-z" 'change-mode-to-emacs)
(define-key vi-command-mode-map "\e" 'vi-ESC)

(define-key vi-command-mode-map " " 'vi-scroll)
(define-key vi-command-mode-map "!" 'vi-command-argument)
(define-key vi-command-mode-map "\"" 'vi-command-argument)
(define-key vi-command-mode-map "#" 'vi-command-argument)
(define-key vi-command-mode-map "$" 'vi-goto-eol)
(define-key vi-command-mode-map "%" 'vi-paren-match)
(define-key vi-command-mode-map "&" 'vi-nil)
(define-key vi-command-mode-map "'" 'vi-goto-mark-and-skip-white)
(define-key vi-command-mode-map "(" 'vi-backward-sentence)
(define-key vi-command-mode-map ")" 'vi-forward-sentence)
(define-key vi-command-mode-map "*" 'call-last-kbd-macro)
(define-key vi-command-mode-map "+" 'vi-next-line-at-bol)
(define-key vi-command-mode-map "," 'vi-comma)
(define-key vi-command-mode-map "-" 'vi-previous-line-at-bol)
(define-key vi-command-mode-map "." 'vi-repeat)
(define-key vi-command-mode-map "/" 'vi-search-forward)

(define-key vi-command-mode-map "0" 'vi-beginning-of-line)
(define-key vi-command-mode-map "1" 'vi-digit-argument)
(define-key vi-command-mode-map "2" 'vi-digit-argument)
(define-key vi-command-mode-map "3" 'vi-digit-argument)
(define-key vi-command-mode-map "4" 'vi-digit-argument)
(define-key vi-command-mode-map "5" 'vi-digit-argument)
(define-key vi-command-mode-map "6" 'vi-digit-argument)
(define-key vi-command-mode-map "7" 'vi-digit-argument)
(define-key vi-command-mode-map "8" 'vi-digit-argument)
(define-key vi-command-mode-map "9" 'vi-digit-argument)

(define-key vi-command-mode-map ":" 'vi-ex)
(define-key vi-command-mode-map ";" 'vi-semi-colon)
(define-key vi-command-mode-map "<" 'vi-command-argument)
(define-key vi-command-mode-map "=" 'vi-command-argument)
(define-key vi-command-mode-map ">" 'vi-command-argument)
(define-key vi-command-mode-map "?" 'vi-search-backward)
(define-key vi-command-mode-map "j" 'vi-nil)

(define-key vi-command-mode-map "A" 'vi-Append)
(define-key vi-command-mode-map "B" 'vi-backward-Word)
(define-key vi-command-mode-map "C" 'ctl-c-equivalent)
(define-key vi-command-mode-map "D" 'vi-kill-line)
(define-key vi-command-mode-map "E" 'vi-end-of-Word)
(define-key vi-command-mode-map "F" 'vi-find-char-backward)
(define-key vi-command-mode-map "G" 'vi-goto-line)
(define-key vi-command-mode-map "H" 'vi-window-top)
(define-key vi-command-mode-map "I" 'vi-Insert)
(define-key vi-command-mode-map "J" 'vi-join-lines)
(define-key vi-command-mode-map "K" 'vi-kill-buffer)
(define-key vi-command-mode-map "L" 'vi-window-bottom)
(define-key vi-command-mode-map "M" 'vi-window-middle)
(define-key vi-command-mode-map "N" 'vi-search-Next)
(define-key vi-command-mode-map "O" 'vi-Open-line)
(define-key vi-command-mode-map "P" 'vi-Put-back)
(define-key vi-command-mode-map "Q" 'vi-query-replace)
(define-key vi-command-mode-map "R" 'vi-replace-string)
(define-key vi-command-mode-map "S" 'vi-switch-to-buffer-other-window)
(define-key vi-command-mode-map "T" 'vi-goto-char-backward)
(define-key vi-command-mode-map "U" 'vi-nil)
(define-key vi-command-mode-map "V" 'vi-find-file-other-window)
(define-key vi-command-mode-map "W" 'vi-forward-Word)
(define-key vi-command-mode-map "X" 'ctl-x-equivalent)
(define-key vi-command-mode-map "Y" 'vi-yank-line)
(define-key vi-command-mode-map "ZZ" 'save-buffers-kill-emacs)

(define-key vi-command-mode-map "[" 'vi-nil)
(define-key vi-command-mode-map "\\" 'escape-to-emacs)
(define-key vi-command-mode-map "]" 'vi-nil)
(define-key vi-command-mode-map "^" 'vi-bol-and-skip-white)
(define-key vi-command-mode-map "_" 'vi-nil)
(define-key vi-command-mode-map "`" 'vi-goto-mark)

(define-key vi-command-mode-map "a" 'vi-append)
(define-key vi-command-mode-map "b" 'vi-backward-word)
(define-key vi-command-mode-map "c" 'vi-command-argument)
(define-key vi-command-mode-map "d" 'vi-command-argument)
(define-key vi-command-mode-map "e" 'vi-end-of-word)
(define-key vi-command-mode-map "f" 'vi-find-char-forward)
(define-key vi-command-mode-map "g" 'vi-info-on-file)
(define-key vi-command-mode-map "h" 'vi-backward-char)
(define-key vi-command-mode-map "i" 'vi-insert)
(define-key vi-command-mode-map "j" 'vi-next-line)
(define-key vi-command-mode-map "k" 'vi-previous-line)
(define-key vi-command-mode-map "l" 'vi-forward-char)
(define-key vi-command-mode-map "m" 'vi-mark-point)
(define-key vi-command-mode-map "n" 'vi-search-next)
(define-key vi-command-mode-map "o" 'vi-open-line)
(define-key vi-command-mode-map "p" 'vi-put-back)
(define-key vi-command-mode-map "q" 'vi-nil)
(define-key vi-command-mode-map "r" 'vi-replace-char)
(define-key vi-command-mode-map "s" 'vi-switch-to-buffer)
(define-key vi-command-mode-map "t" 'vi-goto-char-forward)
(define-key vi-command-mode-map "u" 'vi-undo)
(define-key vi-command-mode-map "v" 'vi-find-file)
(define-key vi-command-mode-map "w" 'vi-forward-word)
(define-key vi-command-mode-map "x" 'vi-delete-char)
(define-key vi-command-mode-map "y" 'vi-command-argument)
(define-key vi-command-mode-map "zH" 'vi-line-to-top)
(define-key vi-command-mode-map "zM" 'vi-line-to-middle)
(define-key vi-command-mode-map "zL" 'vi-line-to-bottom)
(define-key vi-command-mode-map "z\C-m" 'vi-line-to-top)
(define-key vi-command-mode-map "z." 'vi-line-to-middle)
(define-key vi-command-mode-map "z-" 'vi-line-to-bottom)

(define-key vi-command-mode-map "{" 'vi-backward-paragraph)
(define-key vi-command-mode-map "|" 'vi-goto-col)
(define-key vi-command-mode-map "}" 'vi-forward-paragraph)
(define-key vi-command-mode-map "~" 'vi-nil)
(define-key vi-command-mode-map "\177" 'vi-delete-backward-char)

(define-key ctl-x-map "3" 'buffer-in-two-windows)
(define-key ctl-x-map "\C-i" 'insert-file)

(defun vip-version ()
  (interactive)
  (message "VIP version 2.7 of Feb 10, 1987"))


;; implement ex commands

(defvar ex-token-type nil
  "type of token.  if non-nil, gives type of address.  if nil, it
is a command.")

(defvar ex-token nil
  "value of token.")

(defvar ex-addresses nil
  "list of ex addresses")

(defvar ex-flag nil
  "flag for ex flag")

(defvar ex-buffer nil
  "name of ex buffer")

(defvar ex-count nil
  "value of ex count")

(defvar ex-g-flag nil
  "flag for global command")

(defvar ex-g-variant nil
  "if t global command is executed on lines not matching ex-g-pat")

(defvar ex-reg-exp nil
  "save reg-exp used in substitute")

(defvar ex-repl nil
  "replace pattern for substitute")

(defvar ex-g-pat nil
  "pattern for global command")

(defvar ex-map (make-sparse-keymap)
  "save commnads for mapped keys")

(defvar ex-tag nil
  "save ex tag")

(defvar ex-file nil)

(defvar ex-variant nil)

(defvar ex-offset nil)

(defvar ex-append nil)

(defun vi-nil ()
  (interactive)
  (error ""))

(defun looking-back (str)
  "(STR)  returns t if looking back reg-exp STR before point."
  (and (save-excursion (re-search-backward str nil t))
       (= (point) (match-end 0))))

(defun check-sub (str)
  "check if ex-token is an initial segment of STR"
  (let ((length (length ex-token)))
    (if (and (<= length (length str))
	     (string= ex-token (substring str 0 length)))
	(setq ex-token str)
      (setq ex-token-type "non-command"))))

(defun get-ex-com-subr ()
  "get a complete ex command"
  (set-mark (point))
  (re-search-forward "[a-z][a-z]*")
  (setq ex-token-type "command")
  (setq ex-token (buffer-substring (point) (mark)))
  (exchange-point-and-mark)
  (cond ((looking-at "a")
	 (cond ((looking-at "ab") (check-sub "abbreviate"))
	       ((looking-at "ar") (check-sub "args"))
	       (t (check-sub "append"))))
	((looking-at "[bh]") (setq ex-token-type "non-command"))
	((looking-at "c")
	 (if (looking-at "co") (check-sub "copy")
	   (check-sub "change")))
	((looking-at "d") (check-sub "delete"))
	((looking-at "e")
	 (if (looking-at "ex") (check-sub "ex")
	   (check-sub "edit")))
	((looking-at "f") (check-sub "file"))
	((looking-at "g") (check-sub "global"))
	((looking-at "i") (check-sub "insert"))
	((looking-at "j") (check-sub "join"))
	((looking-at "l") (check-sub "list"))
	((looking-at "m")
	 (cond ((looking-at "map") (check-sub "map"))
	       ((looking-at "mar") (check-sub "mark"))
	       (t (check-sub "move"))))
	((looking-at "n")
	 (if (looking-at "nu") (check-sub "number")
	   (check-sub "next")))
	((looking-at "o") (check-sub "open"))
	((looking-at "p")
	 (cond ((looking-at "pre") (check-sub "preserve"))
	       ((looking-at "pu") (check-sub "put"))
	       (t (check-sub "print"))))
	((looking-at "q") (check-sub "quit"))
	((looking-at "r")
	 (cond ((looking-at "rec") (check-sub "recover"))
	       ((looking-at "rew") (check-sub "rewind"))
	       (t (check-sub "read"))))
	((looking-at "s")
	 (cond ((looking-at "se") (check-sub "set"))
	       ((looking-at "sh") (check-sub "shell"))
	       ((looking-at "so") (check-sub "source"))
	       ((looking-at "st") (check-sub "stop"))
	       (t (check-sub "substitute"))))
	((looking-at "t")
	 (if (looking-at "ta") (check-sub "tag")
	   (check-sub "t")))
	((looking-at "u")
	 (cond ((looking-at "una") (check-sub "unabbreviate"))
	       ((looking-at "unm") (check-sub "unmap"))
	       (t (check-sub "undo"))))
	((looking-at "v")
	 (cond ((looking-at "ve") (check-sub "version"))
	       ((looking-at "vi") (check-sub "visual"))
	       (t (check-sub "v"))))
	((looking-at "w")
	 (if (looking-at "wq") (check-sub "wq")
	   (check-sub "write")))
	((looking-at "x") (check-sub "xit"))
	((looking-at "y") (check-sub "yank"))
	((looking-at "z") (check-sub "z")))
  (exchange-point-and-mark))

(defun get-ex-token ()
  "get an ex-token which is either an address or a command.
a token has type \(command, address, end-mark\) and value."
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (cond ((looking-at "[k#]")
	   (setq ex-token-type "command")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "[a-z]") (get-ex-com-subr))
	  ((looking-at "\\.")
	   (forward-char 1)
	   (setq ex-token-type "dot"))
	  ((looking-at "[0-9]")
	   (set-mark (point))
	   (re-search-forward "[0-9]*")
	   (setq ex-token-type
		 (cond ((string= ex-token-type "plus") "add-number")
		       ((string= ex-token-type "minus") "sub-number")
		       (t "abs-number")))
	   (setq ex-token (string-to-int (buffer-substring (point) (mark)))))
	  ((looking-at "\\$")
	   (forward-char 1)
	   (setq ex-token-type "end"))
	  ((looking-at "%")
	   (forward-char 1)
	   (setq ex-token-type "whole"))
	  ((looking-at "+")
	   (cond ((or (looking-at "+[-+]") (looking-at "+[\n|]"))
		  (forward-char 1)
		  (insert-string "1")
		  (backward-char 1)
		  (setq ex-token-type "plus"))
		 ((looking-at "+[0-9]")
		  (forward-char 1)
		  (setq ex-token-type "plus"))
		 (t
		  (error "Badly formed address"))))
	  ((looking-at "-")
	   (cond ((or (looking-at "-[-+]") (looking-at "-[\n|]"))
		  (forward-char 1)
		  (insert-string "1")
		  (backward-char 1)
		  (setq ex-token-type "minus"))
		 ((looking-at "-[0-9]")
		  (forward-char 1)
		  (setq ex-token-type "minus"))
		 (t
		  (error "Badly formed address"))))
	  ((looking-at "/")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^/]*/")
	       (re-search-forward "[^/]*\\(/\\|\n\\)")
	       (if (not (looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\/"))
		   (setq cont nil))))
	   (backward-char 1)
	   (setq ex-token (buffer-substring (point) (mark)))
	   (if (looking-at "/") (forward-char 1))
	   (setq ex-token-type "search-forward"))
	  ((looking-at "\\?")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^\\?]*\\?")
	       (re-search-forward "[^\\?]*\\(\\?\\|\n\\)")
	       (if (not (looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\\\?"))
		   (setq cont nil))
	       (backward-char 1)
	       (if (not (looking-at "\n")) (forward-char 1))))
	   (setq ex-token-type "search-backward")
	   (setq ex-token (buffer-substring (1- (point)) (mark))))
	  ((looking-at ",")
	   (forward-char 1)
	   (setq ex-token-type "comma"))
	  ((looking-at ";")
	   (forward-char 1)
	   (setq ex-token-type "semi-colon"))
	  ((looking-at "[!=><&~]")
	   (setq ex-token-type "command")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "'")
	   (setq ex-token-type "goto-mark")
	   (forward-char 1)
	   (cond ((looking-at "'") (setq ex-token nil))
		 ((looking-at "[a-z]") (setq ex-token (following-char)))
		 (t (error "Marks are ' and a-z")))
	   (forward-char 1))
	  ((looking-at "\n")
	   (setq ex-token-type "end-mark")
	   (setq ex-token "goto"))
	  (t
	   (error "illegal token")))))

(defun vi-ex ()
  "ex commands within vi."
  (interactive)
  (setq ex-g-flag nil
	ex-g-variant nil)
  (let ((com-str (vi-read-string ":")) (address nil) (cont t) (dot (point)))
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type "")
    (setq ex-addresses nil)
    (while cont
      (get-ex-token)
      (cond ((or (string= ex-token-type "command")
		 (string= ex-token-type "end-mark"))
	     (if address (setq ex-addresses (cons address ex-addresses)))
	     (execute-ex-command)
	     (save-window-excursion
	       (switch-to-buffer " *ex-working-space*")
	       (skip-chars-forward " \t")
	       (cond ((looking-at "|")
		      (forward-char 1))
		     ((looking-at "\n")
		      (setq cont nil))
		     (t (error "Extra character at end of a command")))))
	    ((string= ex-token-type "non-command")
	     (error (format "%s: Not an editor command" ex-token)))
	    ((string= ex-token-type "whole")
	     (setq ex-addresses
		   (cons (point-max) (cons (point-min) ex-addresses))))
	    ((string= ex-token-type "comma")
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    ((string= ex-token-type "semi-colon")
	     (if address (setq dot address))
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    (t (let ((ans (get-ex-address-subr address dot)))
		 (if ans (setq address ans))))))))

(defun ex-search-address (forward)
  "search pattern and set address"
  (if (string= ex-token "")
      (if (null s-string) (error "No previous search string")
	(setq ex-token s-string))
    (setq s-string ex-token))
  (if forward
      (progn
	(next-line 1)
	(re-search-forward ex-token))
    (next-line -1)
    (re-search-backward ex-token)))

(defun get-ex-pat ()
  "get a regular expression and set ex-variant if found"
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-g-variant (not ex-g-variant)
		ex-g-flag (not ex-g-flag))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at "/")
	(progn
	  (forward-char 1)
	  (set-mark (point))
	  (let ((cont t))
	    (while (and (not (eolp)) cont)
	      (re-search-forward "[^/]*\\(/\\|\n\\)")
	      ;;(re-search-forward "[^/]*/")
	      (if (not (looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\/"))
		  (setq cont nil))))
	  (setq ex-token
		(if (= (mark) (point)) ""
		  (buffer-substring (1- (point)) (mark))))
	  (backward-char 1))
      (setq ex-token nil))))

(defun get-ex-command ()
  "get an ex command"
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (if (looking-at "/") (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "[a-z]")
	   (get-ex-com-subr)
	   (if (string= ex-token-type "non-command")
	       (error "%s: not an editor command" ex-token)))
	  ((looking-at "[!=><&~]")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  (t (error "Could not find an ex command")))))

(defun get-ex-opt-gc ()
  "get an ex option g or c"
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (if (looking-at "/") (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "g")
	   (setq ex-token "g")
	   (forward-char 1)
	   t)
	  ((looking-at "c")
	   (setq ex-token "c")
	   (forward-char 1)
	   t)
	  (t nil))))

(defun default-ex-addresses (&optional whole-flag)
  "compute default addresses.  whole-flag means whole buffer."
  (cond ((null ex-addresses)
	 (setq ex-addresses
	       (if whole-flag
		   (cons (point-max) (cons (point-min) nil))
		 (cons (point) (cons (point) nil)))))
	((null (cdr ex-addresses))
	 (setq ex-addresses
	       (cons (car ex-addresses) ex-addresses)))))

(defun get-ex-address ()
  "get an ex-address and set ex-flag if a flag is found"
  (let ((address (point)) (cont t))
    (setq ex-token "")
    (setq ex-flag nil)
    (while cont
      (get-ex-token)
      (cond ((string= ex-token-type "command")
	     (if (or (string= ex-token "print") (string= ex-token "list")
		     (string= ex-token "#"))
		 (progn
		   (setq ex-flag t)
		   (setq cont nil))
	     (error "address expected")))
	    ((string= ex-token-type "end-mark")
	     (setq cont nil))
	    ((string= ex-token-type "whole")
	     (error "a trailing address is expected"))
	    ((string= ex-token-type "comma")
	     (error "Extra characters after an address"))
	    (t (let ((ans (get-ex-address-subr address (point))))
		 (if ans (setq address ans))))))
    address))

(defun get-ex-address-subr (old-address dot)
  "returns an address as a point"
  (let ((address nil))
    (cond ((string= ex-token-type "dot")
	   (setq address dot))
	  ((string= ex-token-type "add-number")
	   (save-excursion
	     (goto-char old-address)
	     (next-line (if (= old-address 0) (1- ex-token) ex-token))
	     (setq address (point))))
	  ((string= ex-token-type "sub-number")
	   (save-excursion
	     (goto-char old-address)
	     (next-line (- ex-token))
	     (setq address (point))))
	  ((string= ex-token-type "abs-number")
	   (save-excursion
	     (goto-char (point-min))
	     (if (= ex-token 0) (setq address 0)
	       (next-line (1- ex-token))
	       (setq address (point)))))
	  ((string= ex-token-type "end")
	   (setq address (point-max)))
	  ((string= ex-token-type "plus") t);; do nothing
	  ((string= ex-token-type "minus") t);; do nothing
	  ((string= ex-token-type "search-forward")
	   (save-excursion
	     (ex-search-address t)
	     (setq address (point))))
	  ((string= ex-token-type "search-backward")
	   (save-excursion
	     (ex-search-address nil)
	     (setq address (point))))
	  ((string= ex-token-type "goto-mark")
	   (save-excursion
	     (if (null ex-token)
		 (exchange-point-and-mark)
	       (goto-char (register-to-point (- ex-token (- ?a ?\C-a)))))
	     (setq address (point)))))
    address))

(defun get-ex-buffer ()
  "get a buffer name and set ex-count and ex-flag if found"
  (setq ex-buffer nil)
  (setq ex-count nil)
  (setq ex-flag nil)
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "[a-zA-Z]")
	(progn
	  (setq ex-buffer (following-char))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-int (buffer-substring (point) (mark))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "Illegal extra characters"))))

(defun get-ex-count ()
  (setq ex-variant nil
	ex-count nil
	ex-flag nil)
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-variant t)
	  (forward-char 1)))
    (skip-chars-forward " \t")
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-int (buffer-substring (point) (mark))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "Illegal extra characters"))))

(defun get-ex-file ()
  "get a file name and set ex-variant, ex-append and ex-offset if found"
  (setq ex-file nil
	ex-variant nil
	ex-append nil
	ex-offset nil)
  (save-window-excursion
    (switch-to-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-variant t)
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at ">>")
	(progn
	  (setq ex-append t
		ex-variant t)
	  (forward-char 2)
	  (skip-chars-forward " \t")))
    (if (looking-at "+")
	(progn
	  (forward-char 1)
	  (set-mark (point))
	  (re-search-forward "[ \t\n]")
	  (backward-char 1)
	  (setq ex-offset (buffer-substring (point) (mark)))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (set-mark (point))
    (re-search-forward "[ \t\n]")
    (backward-char 1)
    (setq ex-file (buffer-substring (point) (mark)))))

(defun execute-ex-command ()
  "execute ex command using the value of addresses."
  (cond ((string= ex-token "goto") (ex-goto))
	((string= ex-token "copy") (ex-copy nil))
	((string= ex-token "delete") (ex-delete))
	((string= ex-token "edit") (ex-edit))
	((string= ex-token "file") (vi-info-on-file))
	((string= ex-token "global") (ex-global nil))
	((string= ex-token "join") (ex-line "join"))
	((string= ex-token "k") (ex-mark))
	((string= ex-token "mark") (ex-mark))
	((string= ex-token "map") (ex-map))
	((string= ex-token "move") (ex-copy t))
	((string= ex-token "put") (ex-put))
	((string= ex-token "quit") (ex-quit))
	((string= ex-token "read") (ex-read))
	((string= ex-token "set") (ex-set))
	((string= ex-token "shell") (ex-shell))
	((string= ex-token "substitute") (ex-substitute))
	((string= ex-token "stop") (suspend-emacs))
	((string= ex-token "t") (ex-copy nil))
	((string= ex-token "tag") (ex-tag))
	((string= ex-token "undo") (vi-undo))
	((string= ex-token "unmap") (ex-unmap))
	((string= ex-token "v") (ex-global t))
	((string= ex-token "version") (vip-version))
	((string= ex-token "visual") (ex-edit))
	((string= ex-token "write") (ex-write nil))
	((string= ex-token "wq") (ex-write t))
	((string= ex-token "yank") (ex-yank))
	((string= ex-token "!") (ex-command))
	((string= ex-token "=") (ex-line-no))
	((string= ex-token ">") (ex-line "right"))
	((string= ex-token "<") (ex-line "left"))
	((string= ex-token "&") (ex-substitute t))
	((string= ex-token "~") (ex-substitute t t))
	((or (string= ex-token "append")
	     (string= ex-token "args")
	     (string= ex-token "change")
	     (string= ex-token "insert")
	     (string= ex-token "open")
	     )
	 (error (format "%s: no such command from VIP" ex-token)))
	((or (string= ex-token "abbreviate")
	     (string= ex-token "list")
	     (string= ex-token "next")
	     (string= ex-token "print")
	     (string= ex-token "preserve")
	     (string= ex-token "recover")
	     (string= ex-token "rewind")
	     (string= ex-token "source")
	     (string= ex-token "unabbreviate")
	     (string= ex-token "xit")
	     (string= ex-token "z")
	     )
	 (error (format "%s: not implemented in VIP" ex-token)))
	(t (error (format "%s: Not an editor command" ex-token)))))

(defun ex-goto ()
  "ex goto command"
  (if (null ex-addresses)
      (setq ex-addresses (cons (dot) nil)))
  (push-mark (point))
  (goto-char (car ex-addresses))
  (beginning-of-line))

(defun ex-copy (del-flag)
  "ex copy and move command.  del-flag means delete."
  (default-ex-addresses)
  (let ((address (get-ex-address))
	(end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (goto-char end)
    (save-excursion
      (set-mark beg)
      (enlarge-region (mark) (point))
      (if (or ex-g-flag ex-g-variant)
	  ;; global or it's variant
	  (progn
	    (if del-flag (kill-region (mark) (point))
	      (copy-region-as-kill (mark) (point)))
	    (save-window-excursion
	      (switch-to-buffer " *ex-working-space")
	      (delete-region (point-min) (point-max))
	      (insert-string (car kill-ring-yank-pointer))
	      (goto-char  (point-min))
	      (while (not (eobp))
		(end-of-line)
		(set-mark (point))
		(beginning-of-line)
		(if (re-search-forward ex-g-pat (mark) t)
		    (if ex-g-flag
			(next-line 1)
		      (beginning-of-line)
		      (delete-region (point) (1+ (mark))))
		  (if ex-g-variant
		      (next-line 1)
		    (beginning-of-line)
		    (delete-region (point) (1+ (mark))))))
	      (if del-flag (kill-region (point-min) (point-max))
		(copy-region-as-kill (point-min) (point-max)))))
	(if del-flag (kill-region (point) (mark))
	  (copy-region-as-kill (point) (mark))))
      (if ex-flag
	  (progn
	    (with-output-to-temp-buffer "*copy text*"
	      (princ
	       (if (or del-flag ex-g-flag ex-g-variant)
		   (car kill-ring-yank-pointer)
		 (buffer-substring (point) (mark)))))
	    (condition-case nil
		(progn
		  (vi-read-string "[Hit return to continue] ")
		  (save-excursion (kill-buffer "*copy text*")))
	      (quit
	       (save-excursion (kill-buffer "*copy text*"))
	       (signal 'quit nil))))))
      (if (= address 0)
	  (goto-char (point-min))
	(goto-char address)
	(next-line 1)
	(beginning-of-line))
      (insert-string (car kill-ring-yank-pointer))))

(defun ex-delete ()
  "ex delete"
  (default-ex-addresses)
  (get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (enlarge-region beg end)
      (let ((limit (point-marker)))
	(exchange-point-and-mark)
	(if (or ex-g-flag ex-g-variant)
	    (while (and (not (eobp)) (<= (point) limit))
	      (end-of-line)
	      (set-mark (point))
	      (beginning-of-line)
	      (let ((found (re-search-forward ex-g-pat (mark) t)))
		  (if (or (and ex-g-flag found)
			  (and ex-g-variant (not found)))
		      (if ex-count
			  (progn
			    (beginning-of-line)
			    (set-mark (point))
			    (next-line ex-count)
			    (delete-region (point) (mark)))
			(beginning-of-line)
			(delete-region (point) (1+ (mark))))
		    (goto-char (1+ (mark))))))))
      (if ex-count
	  (progn
	    (set-mark (point))
	    (next-line (1- ex-count)))
	(set-mark end))
      (enlarge-region (point) (mark))
      (if ex-flag
	  ;; show text to be deleted and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *delete text*"
	      (princ (buffer-substring (point) (mark))))
	    (condition-case conditions
		(vi-read-string "[Hit return to continue] ")
	      (quit
	       (save-excursion (kill-buffer " *delete text*"))
	       (error "")))
	    (save-excursion (kill-buffer " *delete text*")))
	(if ex-buffer
	    (if (and (<= ?A ex-buffer) (<= ex-buffer ?Z))
		(vi-append-to-register
		 (+ ex-buffer 32) (point) (mark) nil)
	      (copy-to-register ex-buffer (point) (mark) nil)))
	(delete-region (point) (mark))))))

(defun ex-edit ()
  "ex-edit"
  (get-ex-file)
  (if (and (not ex-variant) (buffer-modified-p) buffer-file-name)
      (error "No write since last change \(:e! overrides\)"))
  (change-mode-to-emacs)
  (switch-to-buffer
   (find-file-noselect (concat default-directory ex-file)))
  (change-mode-to-vi)
  (goto-char (point-min))
  (if ex-offset
      (progn
	(save-window-excursion
	  (switch-to-buffer " *ex-working-space*")
	  (delete-region (point-min) (point-max))
	  (insert ex-offset "\n")
	  (goto-char (point-min)))
	(goto-char (get-ex-address))
	(beginning-of-line))))

(defun ex-global (variant)
  "ex global command"
  (if variant
      (setq ex-g-flag nil
	    ex-g-variant t)
    (setq ex-g-flag t
	  ex-g-variant nil))
  (get-ex-pat)
  (if (null ex-token)
      (error "Missing regular expression for global command"))
  (if (string= ex-token "")
      (if (null s-string) (error "No previous search string")
	(setq ex-g-pat s-string))
    (setq ex-g-pat ex-token
	  s-string ex-token))
  (get-ex-command)
  (if (string= ex-token "global")
      (error "Global within global not allowed"))
  (if (null ex-addresses)
      (setq ex-addresses (cons (point-max) (cons (point-min) nil))))
  (execute-ex-command))

(defun ex-line (com)
  "ex line commands.  COM is join, shift-right or shift-left."
  (default-ex-addresses)
  (get-ex-count)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))) point)
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (enlarge-region beg end)
      (let ((limit (point-marker)))
	(exchange-point-and-mark)
	(if (or ex-g-flag ex-g-variant)
	    (while (and (not (eobp)) (<= (point) limit))
	      (end-of-line)
	      (set-mark (point))
	      (beginning-of-line)
	      (if (re-search-forward ex-g-pat (mark) t)
		  (if ex-g-flag
		      (if ex-count
			  (progn
			    (beginning-of-line)
			    (set-mark (point))
			    (next-line ex-count)
			    (ex-line-subr com (point) (mark)))
			(beginning-of-line)
			(ex-line-subr com (point) (1+ (mark))))
		    (goto-char (1+ (mark))))
		(if ex-g-variant
		    (if ex-count
			(progn
			  (beginning-of-line)
			  (set-mark (point))
			  (next-line ex-count)
			  (ex-line-subr com (point) (mark)))
		      (beginning-of-line)
		      (ex-line-subr com (point) (1+ (mark))))
		  (goto-char (1+ (mark))))))
      (if ex-count
	  (progn
	    (set-mark (point))
	    (next-line ex-count)))
      (if ex-flag
	  ;; show text to be joined and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *text*"
	      (princ (buffer-substring (point) (mark))))
	    (condition-case conditions
		(progn
		  (vi-read-string "[Hit return to continue] ")
		  (ex-line-subr com (point) (mark)))
	      (quit
	       (ding)))
	    (save-excursion (kill-buffer " *text*")))
	(ex-line-subr com (point) (mark)))))
      (setq point (point)))
    (goto-char (1- point))
    (beginning-of-line)))

(defun ex-line-subr (com beg end)
  (cond ((string= com "join")
	 (goto-char (min beg end))
	 (while (and (not (eobp)) (< (point) (max beg end)))
	   (end-of-line)
	   (if (and (<= (point) (max beg end)) (not (eobp)))
	       (progn
		 (forward-line 1)
		 (delete-region (point) (1- (point)))
		 (if (not ex-variant) (fixup-whitespace))))))
	((or (string= com "right") (string= com "left"))
	 (indent-rigidly
	  (min beg end) (max beg end)
	  (if (string= com "right") shift-width (- shift-width)))
	 (goto-char (max beg end))
	 (end-of-line)
	 (forward-char 1))))

(defun ex-mark ()
  "ex mark"
  (let (char)
    (if (null ex-addresses)
	(setq ex-addresses
	      (cons (point) nil)))
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (if (looking-at "[a-z]")
	  (progn
	    (setq char (following-char))
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (if (not (looking-at "[\n|]"))
		(error "Extra characters at end of \"k\" command")))
	(if (looking-at "[\n|]")
	    (error "\"k\" requires a following letter")
	  (error "Mark must specify a letter"))))
    (save-excursion
      (goto-char (car ex-addresses))
      (point-to-register (- char (- ?a ?\C-a))))))

(defun ex-map ()
  "ex map"
  (let (char string)
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (setq char (char-to-string (following-char)))
      (forward-char 1)
      (skip-chars-forward " \t")
      (if (looking-at "[\n|]") (error "Missing rhs"))
      (set-mark (point))
      (end-of-buffer)
      (backward-char 1)
      (setq string (buffer-substring (mark) (point))))
    (if (not (lookup-key ex-map char))
	(define-key ex-map char
	  (or (lookup-key vi-command-mode-map char) 'vi-nil)))
    (define-key vi-command-mode-map char
      (eval
       (list 'quote
	     (cons 'lambda
		   (list '(count)
			 '(interactive "p")
			 (list 'execute-kbd-macro string 'count))))))))

(defun ex-unmap ()
  "ex unmap"
  (let (char)
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (setq char (char-to-string (following-char)))
      (forward-char 1)
      (skip-chars-forward " \t")
      (if (not (looking-at "[\n|]")) (error "Macro must be a character")))
    (if (not (lookup-key ex-map char))
	(error "That macro wasn't mapped"))
    (define-key vi-command-mode-map char (lookup-key ex-map char))
    (define-key ex-map char nil)))

(defun ex-put ()
  "ex put"
  (let ((point (if (null ex-addresses) (point) (car ex-addresses))))
    (get-ex-buffer)
    (setq use-register ex-buffer)
    (goto-char point)
    (if (= point 0) (vi-Put-back 1) (vi-put-back 1))))

(defun ex-quit ()
  "ex quit"
  (let (char)
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (setq char (following-char)))
    (if (= char ?!) (kill-emacs t) (save-buffers-kill-emacs))))

(defun ex-read ()
  "ex read"
  (let ((point (if (null ex-addresses) (point) (car ex-addresses)))
	(variant nil) command file)
    (goto-char point)
    (if (not (= point 0)) (next-line 1))
    (beginning-of-line)
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (if (looking-at "!")
	  (progn
	    (setq variant t)
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (set-mark (point))
	    (end-of-line)
	    (setq command (buffer-substring (mark) (point))))
	(set-mark (point))
	(re-search-forward "[ \t\n]")
	(backward-char 1)
	(setq file (buffer-substring (point) (mark)))))
      (if variant
	  (shell-command command t)
	(insert-file file))))

(defun ex-set ()
  (eval (list 'setq
	      (read-variable "Variable: ")
	      (eval (read-minibuffer "Value: ")))))

(defun ex-shell ()
  "ex shell"
  (change-mode-to-emacs)
  (shell))

(defun ex-substitute (&optional repeat r-flag) 
  "ex substitute. if REPEAT use previous reg-exp which is ex-reg-exp or
s-string"
  (let (pat repl (opt-g nil) (opt-c nil) (matched-pos nil))
    (if repeat (setq ex-token nil) (get-ex-pat))
    (if (null ex-token)
	(setq pat (if r-flag s-string ex-reg-exp)
	      repl ex-repl)
      (setq pat (if (string= ex-token "") s-string ex-token))
      (setq s-string pat
	    ex-reg-exp pat)
      (get-ex-pat)
      (if (null ex-token)
	  (setq ex-token ""
		ex-repl "")
	(setq repl ex-token
	      ex-repl ex-token)))
    (while (get-ex-opt-gc)
      (if (string= ex-token "g") (setq opt-g t) (setq opt-c t)))
    (get-ex-count)
    (if ex-count
	(save-excursion
	  (if ex-addresses (goto-char (car ex-addresses)))
	  (set-mark (point))
	  (next-line (1- ex-count))
	  (setq ex-addresses (cons (point) (cons (mark) nil))))
      (if (null ex-addresses)
	  (setq ex-addresses (cons (point) (cons (point) nil)))
	(if (null (cdr ex-addresses))
	    (setq ex-addresses (cons (car ex-addresses) ex-addresses)))))
    (setq G opt-g)
    (let ((beg (car ex-addresses)) (end (car (cdr ex-addresses)))
	  (cont t) eol-mark)
      (save-excursion
	(enlarge-region beg end)
	(let ((limit (save-excursion
		       (goto-char (max (point) (mark)))
		       (point-marker))))
	  (goto-char (min (point) (mark)))
	  (if (or ex-g-flag ex-g-variant)
	      (while cont
		(end-of-line)
		(set-mark (point))
		(beginning-of-line)
		(let ((found (re-search-forward ex-g-pat (mark) t)))
		  (if (or (and ex-g-flag found)
			  (and ex-g-variant (not found)))
		      (progn
			(end-of-line)
			(setq eol-mark (dot-marker))
			(beginning-of-line)
			(if opt-g
			    (while (and (not (eolp))
					(re-search-forward pat eol-mark t))
			      (if (or (not opt-c) (y-or-n-p "Replace? "))
				  (progn
				    (setq matched-pos (point))
				    (replace-match repl))))
			  (if (re-search-forward pat eol-mark t)
			      (if (or (not opt-c) (y-or-n-p "Replace? "))
				  (progn
				    (setq matched-pos (point))
				    (replace-match repl)))))
			(end-of-line)
			(if (= (point) limit) (setq cont nil) (forward-char)))
		    (end-of-line)
		    (if (= (point) limit) (setq cont nil) (forward-char)))))
	    (while (< (point) limit)
	      (end-of-line)
	      (setq eol-mark (dot-marker))
	      (beginning-of-line)
	      (if opt-g
		  (progn
		    (while (and (not (eolp))
				(re-search-forward pat eol-mark t))
		      (if (or (not opt-c) (y-or-n-p "Replace? "))
			  (progn
			    (setq matched-pos (point))
			    (replace-match repl))))
		    (end-of-line)
		    (forward-char))
		(if (and (re-search-forward pat eol-mark t)
			 (or (not opt-c) (y-or-n-p "Replace? ")))
		    (progn
		      (setq matched-pos (point))
		      (replace-match repl)))
		(end-of-line)
		(forward-char)))))))
    (if matched-pos (goto-char matched-pos))
    (beginning-of-line)
    (if opt-c (message "done"))))

(defun ex-tag ()
  "ex tag"
  (let (tag)
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (set-mark (point))
      (skip-chars-forward "^ |\t\n")
      (setq tag (buffer-substring (mark) (point))))
    (if (not (string= tag "")) (setq ex-tag tag))
    (change-mode-to-emacs)
    (condition-case conditions
	(progn
	  (if (string= tag "")
	      (find-tag ex-tag t)
	    (find-tag-other-window ex-tag))
	  (change-mode-to-vi))
      (error
       (change-mode-to-vi)
       (message-conditions conditions)))))

(defun ex-write (q-flag)
  "ex write"
  (default-ex-addresses t)
  (get-ex-file)
  (if (string= ex-file "")
      (progn
	(if (null buffer-file-name)
	    (error "No file associated with this buffer"))
	(setq ex-file buffer-file-name))
    (setq ex-file (expand-file-name ex-file)))
  (if (and (not (string= ex-file (buffer-file-name)))
	   (file-exists-p ex-file)
	   (not ex-variant))
      (error (format "\"%s\" File exists - use w! to override" ex-file)))
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (enlarge-region beg end)
      (write-region (point) (mark) ex-file ex-append t)))
  (if (null buffer-file-name) (setq buffer-file-name ex-file))
  (if q-flag (save-buffers-kill-emacs)))

(defun ex-yank ()
  "ex yank"
  (default-ex-addresses)
  (get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (enlarge-region beg end)
      (exchange-point-and-mark)
      (if (or ex-g-flag ex-g-variant) (error "Can't yank within global"))
      (if ex-count
	  (progn
	    (set-mark (point))
	    (next-line (1- ex-count)))
	(set-mark end))
      (enlarge-region (point) (mark))
      (if ex-flag (error "Extra chacters at end of command"))
      (if ex-buffer
	  (copy-to-register ex-buffer (point) (mark) nil))
      (copy-region-as-kill (point) (mark)))))

(defun ex-command ()
  "execute shell command"
  (let (command)
    (save-window-excursion
      (switch-to-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (set-mark (point))
      (end-of-line)
      (setq command (buffer-substring (mark) (point))))
    (if (null ex-addresses)
	(shell-command command)
      (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
	(if (null beg) (setq beg end))
	(save-excursion
	  (goto-char beg)
	  (set-mark end)
	  (enlarge-region (point) (mark))
	  (shell-command-on-region (point) (mark) command t))
	(goto-char beg)))))

(defun ex-line-no ()
  "print line number"
  (message "%d"
	   (1+ (count-lines
		(point-min)
		(if (null ex-addresses) (point-max) (car ex-addresses))))))

(if (file-exists-p "~/.vip") (load "~/.vip"))

;; End of VIP
