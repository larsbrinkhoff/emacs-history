;;; News for gnu emacs

;;; Created Sun Mar 10,1985 at 21:35:01 ads and sundar
;;;  Should do the point pdl stuff sometime
;;; finito except pdl.... Sat Mar 16,1985 at 06:43:44
;;; lets keep the summary stuff out until we get it working ..
;;;  -sundar              Wed Apr 10,1985 at 16:32:06
;;; hack slash maim. mly Thu 18 Apr, 1985 06:11:14

(require 'mail-utils)

(defvar news-path "/usr/spool/news/"
  "The root directory below which all news files are stored.")
(defvar news-startup-file "$HOME/.newsrc" "Contains ~/.newsrc")
(defvar news-inews-program "/usr/local/inews"
  "Function to post news.")


;;; random headers that we decide to ignore.
(defvar news-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Followup-To:\\|^Expires:\\|^Date-Received:\\|^Organization:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:"
  "All random fields within the header of a message.")

(defvar news-mode-map nil)
(defvar news-read-first-time-p t)
;; Contains the (dotified) news groups of which you are a member. 
(defvar news-user-group-list nil)

(defvar news-current-news-group nil)
(defvar news-current-group-begin nil)
(defvar news-current-group-end  nil)

(defvar news-message-filter nil
  "User specifiable filter function that will be called during
formatting of the news file")

(defvar news-mode-group-string "Starting-Up"
  "Mode line group name info is held in this variable")
(defvar news-list-of-files nil
  "Global variable in which we store the list of files
associated with the current newsgroup")

;; association list in which we store lists of the form
;; (dotified-group-name (first last old-last))
(defvar news-group-article-assoc nil)
  
(defvar news-current-message-number 0 "Displayed Article Number")
(defvar news-total-current-group 0 "Total no of messages in group")

(defvar news-unsubscribe-groups ())
(defvar news-point-pdl () "List of visited news messages.")
(defvar news-no-jumps-p t)
(defvar news-buffer () "Buffer into which news files are read.")

(defmacro caar (x) (list 'car (list 'car x)))
(defmacro cadr (x) (list 'car (list 'cdr x)))
(defmacro cdar (x) (list 'cdr (list 'car x)))
(defmacro caddr (x) (list 'car (list 'cdr (list 'cdr x))))
(defmacro cadar (x) (list 'car (list 'cdr (list 'car x))))
(defmacro caadr (x) (list 'car (list 'car (list 'cdr x))))
(defmacro cdadr (x) (list 'cdr (list 'car (list 'cdr x))))

(defun rnews ()
  "Read netnews for groups for which you are a member and add or delete groups.
You can reply to articles posted and send articles to any group.
  Type Help m once reading news to get a list of rnews commands."
  (interactive)
  (switch-to-buffer (setq news-buffer (get-buffer-create "*news*")))
  (news-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)
  (set-buffer-modified-p t)
  (sit-for 0)
  (message "Getting new net news...")
  (news-set-mode-line)
  (news-get-new-news))


(defun news-set-minor-modes ()
  "Creates a minor mode list that has group name, total articles,
and attribute for current article."
  (setq minor-modes (list (cons 'foo
				(concat news-current-message-number
					"/"
					news-total-current-group
					(news-get-attribute-string))))))

(defun news-set-message-counters ()
  "Scan through current news-groups filelist to figure out how many messages
are there. Set counters for use with minor mode display."
    (if (null news-list-of-files)
	(setq news-current-message-number 0)))

(if news-mode-map
    nil
  (setq news-mode-map (make-keymap))
  (suppress-keymap news-mode-map)
  (define-key news-mode-map "." 'beginning-of-article)
  (define-key news-mode-map " " 'scroll-up)
  (define-key news-mode-map "\177" 'scroll-down)
  (define-key news-mode-map "n" 'news-next-message)
  (define-key news-mode-map "p" 'news-previous-message)
  (define-key news-mode-map "j" 'news-goto-message)
  (define-key news-mode-map "+" 'news-plus-n-messages)
  (define-key news-mode-map "-" 'news-minus-n-messages)
  (define-key news-mode-map "q" 'news-quit)
  (define-key news-mode-map "e" 'news-exit)
  (define-key news-mode-map "J" 'news-goto-news-group)
  (define-key news-mode-map "N" 'news-next-group)
  (define-key news-mode-map "P" 'news-previous-group)
  (define-key news-mode-map "l" 'news-list-news-groups)
  (define-key news-mode-map "?" 'describe-mode)
  (define-key news-mode-map "g" 'news-get-new-news)
  (define-key news-mode-map "m" 'news-post-news)
  (define-key news-mode-map "r" 'news-reply)
  (define-key news-mode-map "s" 'news-save-item-in-file)
  (define-key news-mode-map "t" 'news-show-all-headers)
  (define-key news-mode-map "u" 'news-unsubscribe-group)
  (define-key news-mode-map "U" 'news-unsubscribe-any-group))

(defun news-mode ()
  "News Mode is used by M-x news for editing News files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move dot to front of this news article (same as Meta-<).
Space	Scroll to next screen of this news article.
Delete  Scroll down previous page of this news article.
n	Move to Next news article.
p	Move to Previous news article.
j	Jump to news article specified by numeric position.
+	goto current article plus count.
-	goto current article minus count.
J       Jump to news group.
N       Goto next news group.
P       Goto previous news group.
l       List all the news groups with current-status.
?       Print this help message.
q	Quit without updating .newsrc file.
e	Exit updating .newsrc file.
s	Save the current article in the named file. (append if file exists)
m	Mail a news article.  Same as C-x 4 m.
r	Reply to this news article.  Like m but initializes some fields.
t	Show all the headers this news article originally had.
u       Unsubscribe from current newsgroup.
U       Unsubscribe from any newsgroup."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'news-read-first-timep-p)
  (make-local-variable 'news-current-news-group)
  (make-local-variable 'news-current-group-begin)
  (make-local-variable 'news-current-message-number)
  (make-local-variable 'news-total-current-group)
  (make-local-variable 'news-point-pdl)
  (setq news-current-news-group "??")
  (setq news-current-group-begin 0)
  (setq news-current-message-number 0)
  (setq major-mode 'news-mode)
  (setq mode-name "NEWS")
  (news-set-mode-line)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map news-mode-map)
  (setq news-read-first-time-p t)
  (setq news-group-article-assoc ())
  (setq local-abbrev-table text-mode-abbrev-table))

(defun string-subst-char (new old string)
  (let (index)
    (setq old (regexp-quote (char-to-string old))
	  string (substring string 0))
    (while (setq index (string-match old string))
      (aset string index new)))
  string)

;;; update read message number
(defmacro news-update-message-read (ngroup nno)
  (list 'setcar
	(list 'cdadr
	      (list 'assoc ngroup 'news-group-article-assoc))
	nno))

(defun news-parse-range (number-string)
  "Parse string representing range of numbers of he form <a>-<b>
to a list (a . b)"
  (let ((n (string-match "-" number-string)))
    (if n
	(cons (string-to-int (substring number-string 0 n))
	      (string-to-int (substring number-string (1+ n))))
      (setq n (string-to-int number-string))
      (cons n n))))

(defun news-get-new-news ()
  "Get new netnews if there is any for the current user."
  (interactive)
  (setq news-group-article-assoc ())
  (setq news-user-group-list ())
  (message "Looking up .newsrc file...")
  (let ((file (substitute-in-file-name news-startup-file))
	(temp-user-groups ()))
    (save-excursion
      (let ((newsrcbuf (find-file-noselect file))
	    start end endofline tem)
	(set-buffer newsrcbuf)
	(goto-char 0)
	(while (search-forward ": " nil t)
	  (setq end (dot))
	  (beginning-of-line)
	  (setq start (dot))
	  (end-of-line)
	  (setq endofline (dot))
	  (setq tem (buffer-substring start (- end 2)))
	  (let ((range (news-parse-range
			 (buffer-substring end endofline))))
	    (setq temp-user-groups (cons tem temp-user-groups)
		  news-group-article-assoc
		    (cons (list tem (list (car range)
					  (cdr range)
					  (cdr range)))
			  news-group-article-assoc))))
	(kill-buffer newsrcbuf)))      
    (setq temp-user-groups (nreverse temp-user-groups))
    (message "Prefrobnicating...")
    (switch-to-buffer news-buffer)
    (setq news-user-group-list temp-user-groups)
    (while (and temp-user-groups
		(not (news-read-files-into-buffer
		       (car temp-user-groups) nil)))
      (setq temp-user-groups (cdr temp-user-groups)))
    (if (null temp-user-groups)
	(message "No news is good news.")
      (message ""))))

(defun news-list-news-groups ()
  "Display all the news groups to which you belong."
  (interactive)
  (if (null news-user-group-list)
      (message "No user groups read yet!")
    (let ((buffer-read-only ()))
      (setq mode-line-format "--%%--[q: to goback, space: scroll-forward, delete:scroll-backward] %M --%--")
      (local-set-key " " 'scroll-up)
      (local-set-key "\177" 'scroll-down)
      (local-set-key "q" 'news-get-back)
      (goto-char 0)
      (save-excursion
        (erase-buffer)
	(insert
	  "News Group        Msg No.       News Group        Msg No.\n")
	(insert
	  "-------------------------       -------------------------\n")
	(let ((temp news-user-group-list)
	      (flag nil)
	  (while temp
	    (let ((item (assoc (car temp) news-group-article-assoc)))
	      (insert (car item))
	      (indent-to (if flag 20 52))
	      (insert (int-to-string (cadr (caddr item))))
	      (if flag
		  (insert "\n")
		  (indent-to 33))
	      (setq temp (cdr temp) flag (not flag))))))))))

(defun news-get-back ()
  "Called when you quit from seeing the news groups list."
  (interactive)
  (let ((buffer-read-only ()))
    (erase-buffer)
    (local-set-key "q" 'news-quit)
    (news-set-mode-line)
    (news-read-in-file
      (concat news-path
	      (string-subst-char ?/ ?. news-current-news-group)
	      "/" (int-to-string news-current-message-number)))))


;; Mode line hack
(defun news-set-mode-line ()
  "Set mode line string to something useful."
  (setq mode-line-format 
         (concat "--%1*%1*-News(1): ("
		 news-current-news-group
		 "),Mesg: ["
		 (if (integerp news-current-message-number)
		     (int-to-string news-current-message-number)
		   "??")
		 "/"
		 (if (integerp news-current-group-end)
		     (int-to-string news-current-group-end)
		   news-current-group-end)
		 "] %M ----%3p-%-")))


(defun news-goto-news-group (gp)
  "Takes a string and goes to that news group."
;  (interactive (completing-read "NewsGroup: "))
  (interactive "sNewsGroup: ")
  (message "Jumping to news group %s..." gp)
  (news-go-to-news-group gp)
  (message "Jumping to news group %s... done." gp))

(defun news-go-to-news-group (gp)
  (let ((grp (assoc gp news-group-article-assoc)))
    (if (null grp)
	(error "No such newsgroup: %s" gp)
      (news-update-message-read news-current-news-group
				(cdar news-point-pdl))
      (news-read-files-into-buffer  (car grp) nil)
      (news-set-mode-line))))

(defun news-goto-message (arg)
  "Goes to the article ARG in current newsgroup."
  (interactive "p")
  (if (null current-prefix-arg)
      (setq arg (read-no-blanks-input "Go to article: " "")))
  (news-go-to-message arg))

(defun news-go-to-message (arg)
  (if (stringp arg) (setq arg (string-to-int arg)))
  (let ((file (concat news-path
		      (string-subst-char ?/ ?. news-current-news-group)
		      "/" arg)))
    (if (file-exists-p file)
	(let ((buffer-read-only ()))
	  (if (= arg (1+ (cdar news-point-pdl)))
	      (setcdr (car news-point-pdl) arg))
	  (setq news-current-message-number arg)
	  (news-read-in-file file)
	  (news-set-mode-line))
      (error "Article %d nonexistent" arg))))


(defun news-move-to-message (arg)
  "Given arg move forward or backward within the news group."
  (let ((no (+ arg news-current-message-number)))
    (if (or (< no news-current-group-begin) 
	    (> no news-current-group-end))
	(cond ((= arg 1)
	       (message "Moving to next news group...")
	       (news-next-group)
	       (message "Moving to next news group... done"))
	      ((= arg -1)
	       (message "Moving to previous news group...")
	       (news-read-files-into-buffer
		 (caadr (news-get-motion-lists news-current-news-group
					       news-user-group-list))
		 t)
	       (message "Moving to previous news group... done"))
	      (t (error "article out of range?")))
      (let ((plist (news-get-motion-lists
		     (int-to-string news-current-message-number)
		     news-list-of-files)))
	(if (< arg 0)
	    (news-go-to-message (nth (1- (- arg)) (car (cdr plist))))
	    (news-go-to-message (nth (1- arg) (car plist))))))))


(defun news-next-message ()
  "Goes to the next article within a newsgroup."
  (interactive)
  (let ((cg news-current-news-group))
    (news-move-to-message 1)
    (if (not (equal cg news-current-news-group))
	(while (null news-list-of-files)
	  (news-next-group)))))

(defun news-previous-message ()
  "Goes to the previous article within a newsgroup."
  (interactive)
  (let ((cg news-current-news-group))
    (news-move-to-message -1)
    (if (not (equal cg news-current-news-group))
	(while (null news-list-of-files)
	  (news-previous-group)))))

(defun news-plus-n-messages (num)
  "Moves forward n articles within a newsgroup."
  (interactive "p")
  (news-move-to-message num))

(defun news-minus-n-messages (num)
  "Moves backward n articles within a newsgroup."
  (interactive "p")
  (news-move-to-message (- num)))


(defun news-move-to-group (arg)
  "Given arg move forward or backward to a new newsgroup."
  (let ((cg news-current-news-group))
    (let ((plist (news-get-motion-lists cg news-user-group-list)))
      (if (< arg 0)
	  (news-go-to-news-group (nth (1- (- arg)) (cadr plist)))
	  (news-go-to-news-group (nth arg (car plist)))))))

(defun news-next-group ()
  "Moves to the next user group."
  (interactive)
  (message "Moving to next group...")
  (news-move-to-group 0)
  (message "Moving to next group... done."))

(defun news-previous-group ()
  "Moves to the previous user group."
  (interactive)
  (message "Moving to previous group...")
  (news-move-to-group -1)
  (message "Moving to previous group... done."))

(defun news-get-motion-lists (arg listy)
  "Given a msgnumber/group this will return a list of two lists;
one for moving forward and one for moving backward."
  (let ((temp listy)
	(result ()))
    (catch 'out
      (while temp
	(if (equal (car temp) arg)
	    (throw 'out (cons (cdr temp) (list result)))
	  (setq result (nconc (list (car temp)) result))
	  (setq temp (cdr temp)))))))

;; miscellaneous io routines
(defun news-read-in-file (filename)
  (erase-buffer)
  (let ((start (dot)))
  (insert-file-contents filename)
  (news-convert-format)
  (goto-char start)
  (forward-line 1)
  (if (eobp)
      (message "(Empty file?)")
      (goto-char start))))

(defun news-convert-format()
  (save-excursion
    (save-restriction
      (let* ((start (dot))
	     (end (condition-case ()
		      (progn (search-forward "\n\n") (dot))
		    (error "Can't find eoh")))
	     has-from has-date)
       (cond (end
	      (narrow-to-region start end)
	      (goto-char start)
	      (setq has-from (search-forward "\nFrom:" nil t))
	      (cond ((and (not has-from) has-date)
		     (goto-char start)
		     (search-forward "\nDate:")
		     (beginning-of-line)
		     (kill-line) (kill-line)))
	      (news-delete-headers start)
	      (goto-char start)))))))

(defun news-delete-headers(pos)
  (goto-char pos)
  (while (re-search-forward news-ignored-headers nil t)
    (beginning-of-line)
    (delete-region (dot)
		   (progn (re-search-forward "\n[^ \t]")
			  (forward-char -1)
			  (dot)))))

(defun news-quit ()
  "Quit news reading without updating newsrc file."
  (interactive)
  (if (y-or-n-p "Quit without updating .newsrc? ")
      (progn (kill-buffer news-buffer)
	     ;(kill-buffer ".newsrc")
	     (message "%s left unmolested"
		      (substitute-in-file-name news-startup-file)))
    (message "")))

(defun news-exit ()
  "Quit news reading session and update the newsrc file."
  (interactive)
  (if (y-or-n-p "Do you really wanna quit reading news ? ")
      (progn (message "Updating .newsrc...")
	     (news-update-newsrc-file)
	     (message "Updating .newsrc... done")
	     (kill-buffer news-buffer)
	     ;(kill-buffer ".newsrc")
	     (message "Now do some real work"))
    (message "")))

(defun news-update-newsrc-file ()
  "Updates the newsrc file in the users home dir."
  (let ((newsrcbuf (find-file-noselect
		     (substitute-in-file-name news-startup-file)))
	group)
    (save-excursion
      (news-update-message-read news-current-news-group
				(cdar news-point-pdl))
      (switch-to-buffer newsrcbuf)
      (while news-user-group-list
	(setq group (assoc (car news-user-group-list)
			   news-group-article-assoc))
	(if (= (cadr (cadr group)) (caddr (cadr group)))
	    nil
	  (goto-char 0)
	  (if (search-forward (concat (car group) ": ") nil t)
	      (kill-line nil)
	    (insert (car group) ": \n") (backward-char 1))
	  (insert (int-to-string (car (cadr group))) "-"
		  (int-to-string (cadr (cadr group)))))
	(setq news-user-group-list (cdr news-user-group-list)))
     (while news-unsubscribe-groups
       (setq group (assoc (car news-unsubscribe-groups)
			  news-group-article-assoc))
       (goto-char 0)
       (if (search-forward (concat (car group) ": ") nil t)
	   (progn
	      (backward-char 2)
	      (kill-line nil)
	      (insert "! " (int-to-string (car (cadr group)))
		      "-" (int-to-string (cadr (cadr group))))))
       (setq news-unsubscribe-groups (cdr news-unsubscribe-groups)))
     (save-buffer)
     (kill-buffer (current-buffer)))))


(defun news-unsubscribe-any-group (group)
  "Removes the group from the list of groups that you currently subscribe too."
  (interactive "sUnsubscribe from group: ")
  (news-unsubscribe-internal group))

(defun news-unsubscribe-group ()
  "Removes the user from current group."
  (interactive)
  (if (y-or-n-p "Do you really want to unsubscribe from this group ? ")
      (news-unsubscribe-internal news-current-news-group)))

(defun news-unsubscribe-internal (group)
  (let ((tem (assoc group news-group-article-assoc)))
    (if tem
	(progn
	  (setq news-unsubscribe-groups (cons group news-unsubscribe-groups))
	  (news-update-message-read group (cdar news-point-pdl))
	  (if (equal group news-current-news-group)
	      (news-next-group))
	  (message "Member-p of %s ==> nil" news-current-news-group))
      (error "No such group: %s" group))))

(defun news-save-item-in-file (file)
  "Save the current article that is being read into a file."
  (interactive "FSave item in file: ")
  (write-file file))


(defun news-get-pruned-list-of-files (gp-list end-file-no)
  "Given a news group it does an ls to give all files in the news group.
The arg must be in slashified format."
  (let ((file-directory (concat news-path (string-subst-char ?/ ?. gp-list)))
	tem)
    (setq news-list-of-files nil)
    (if (not (file-directory-p file-directory))
	nil
      (setq news-list-of-files
	    (setq tem (cdr (cdr		; (cdr (cdr ...)) strips "." and ".."
			     (directory-files file-directory)))))
      (while tem
	(if (or (not (string-match "^[0-9]*$" (car tem)))
		(<= (string-to-int (car tem)) end-file-no))
	    (setq news-list-of-files (delq (car tem) news-list-of-files)))
	(setq tem (cdr tem)))
      (if (null news-list-of-files)
	  (progn (setq news-current-group-end 0)
		 nil)
	(setq news-list-of-files
	      (sort news-list-of-files
		    '(lambda (x y)
		       (< (string-to-int x)
			  (string-to-int y)))))
	(setq news-current-group-end (string-to-int
				       (elt news-list-of-files
					    (1- (length news-list-of-files)))))
	news-list-of-files))))

(defun news-read-files-into-buffer (group reversep)
  (let* ((files-start-end (cadr (assoc group news-group-article-assoc)))
	 (start-file-no (car files-start-end))
	 (end-file-no (cadr files-start-end))
	 (buffer-read-only nil))

    (setq news-current-news-group group)
    (news-get-pruned-list-of-files group end-file-no)

    ;; should be a lot smarter than this if we have to move
    ;; around correctly.
    (setq news-point-pdl (list (cons (car files-start-end)
				     (cadr files-start-end))))
    (if (null news-list-of-files)
	(progn (erase-buffer)
	       (setq news-current-group-end end-file-no)
	       (setq news-current-group-begin end-file-no)
	       (setq news-current-message-number end-file-no)
	       (news-set-mode-line)
	       (insert "No new articles in " group " group.")
	       nil)
      (setq news-current-group-begin (string-to-int (car news-list-of-files)))
      (if reversep
	  (setq news-current-message-number news-current-group-end)
	(if (> (string-to-int (car news-list-of-files)) end-file-no)
	    (setcdr (car news-point-pdl) (string-to-int
					   (car news-list-of-files))))
	(setq news-current-message-number news-current-group-begin))
      (news-set-message-counters)
      (news-set-mode-line)
      (news-read-in-file (concat news-path
				 (string-subst-char ?/ ?. group)
				 "/"
				 (int-to-string
				   news-current-message-number)))
      (news-set-message-counters)
      (news-set-mode-line)
      t)))


;;; Replying and posting news items are done by these functions.
;;; imported from rmail and modified to work with rnews ...
;;; Mon Mar 25,1985 at 03:07:04 ads@mit-hermes.
;;; this is done so that rnews can operate independently from rmail.el and sendmail and
;;; dosen't have to autoload these functions.

;;;>> Nuked by Mly to autoload those functions again, as the duplication of
;;;>>  code was making maintenance too difficult.

(defvar news-reply-mode-map () "Mode map used by news-reply.")

(defun news-reply-mode ()
  "Major mode for editing news to be posted on netnews.
Like Text Mode but with these additional commands:
C-c C-s, C-c C-c news-inews (send the message and exit)
C-c t  mail-to  (move to To: field)	C-c s  mail-subject (move to Subj:)
C-c b  mail-bcc (move to Bcc: field)    C-c c  mail-cc  (move to Cc: field)
C-c y  mail-yank-original (insert msg being replied to)."
  (interactive)
  ;; require...
  (or (fboundp 'mail-setup) (load "sendmail"))
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table text-mode-syntax-table)
  (or news-reply-mode-map
      (progn (setq news-reply-mode-map (make-keymap))
	     (define-key news-reply-mode-map "\C-c?" 'describe-mode)
	     (define-key news-reply-mode-map "\C-ct" 'mail-to)
	     (define-key news-reply-mode-map "\C-cb" 'mail-bcc)
	     (define-key news-reply-mode-map "\C-cc" 'mail-cc)
	     (define-key news-reply-mode-map "\C-cs" 'mail-subject)
	     (define-key news-reply-mode-map "\C-cy" 'mail-yank-original)
	     (define-key news-reply-mode-map "\C-c\C-c" 'news-inews)
	     (define-key news-reply-mode-map "\C-c\C-s" 'news-inews)))
  (use-local-map news-reply-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'news-reply-mode)
  (setq mode-name "News")
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^--text follows this line--$\\|"
				paragraph-start))
  (setq paragraph-separate (concat "^--text follows this line--$\\|"
				   paragraph-separate))
  (and (boundp 'news-reply-mode-hook)
       news-reply-mode-hook
       (funcall news-reply-mode-hook)))

(defun news-setup (to subject in-reply-to newsgroups replybuffer)
  (setq mail-reply-buffer replybuffer)
  (let ((mail-setup-hook nil))
    (mail-setup to subject in-reply-to nil replybuffer)
    (save-excursion
      (goto-char (dot-max))
      (search-backward "\nSubject:")
      (insert "\nNewsgroups: " (or newsgroups ""))))
  (and (boundp 'news-setup-hook)
       news-setup-hook
       (funcall news-setup-hook)))
   
(defun news-inews ()
  "Send a news message using inews."
  (interactive)
  (let* (newsgroups subject
	 (case-fold-search nil))
    (save-restriction
      (goto-char (dot-min))
      (search-forward "\n--text follows this line--\n")
      (narrow-to-region (dot-min) (dot))
      (setq newsgroups (mail-fetch-field "newsgroups")
	    subject (mail-fetch-field "subject")))
    (message "Posting...")
    (call-process-region (dot) (dot-max) 
			 news-inews-program nil 0 nil
			 "-t" subject
			 "-n" newsgroups)
    (message "Posting... done")
    (set-buffer-modified-p nil)
    (delete-windows-on (current-buffer))
    (and (fboundp 'bury-buffer) (bury-buffer (current-buffer)))))
		       
(defun news-reply ()
  "Reply to the current message.
While composing the reply, use \\[mail-yank-original] to yank the original message into it."
  (interactive)
  (let (from reply-to cc subject date to newsgroups
	(buffer (current-buffer)))
    (save-restriction
      (narrow-to-region (dot-min) (progn (search-forward "\n\n")
					 (- (dot) 2)))
      (setq from (mail-fetch-field "from")
	    reply-to (or (mail-fetch-field "reply-to") from)
	    subject (mail-fetch-field "subject")
	    date (mail-fetch-field "date")
	    newsgroups (mail-fetch-field "newsgroups"))
      (pop-to-buffer "*post-news*")
      (news-reply-mode)
      (erase-buffer)
      (news-setup
        reply-to
	subject
	(let ((stop-pos (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
	  (concat (if stop-pos (substring from 0 stop-pos) from)
		  "'s message of "
		  date))
	newsgroups
	buffer))))

(defun news-post-news ()
  "Post a new news article."
  (interactive)
  (pop-to-buffer "*post-news*")
  (news-reply-mode)
  (erase-buffer)
  (news-setup () () () () ()))
