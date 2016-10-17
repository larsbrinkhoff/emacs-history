;;; active-hyper admininistrative functions


(setq debug-on-error t)

(setq active-hyper-site-regexp "\\(\\w+\\(\\.\\w+\\)*\\.\\(edu\\|net\\|com\\|mil\\|gov\\|arpa\\)\\)")

(setq active-hyper-big-file "~/emacs/hyperbole/ActiveListBig.otl")

(defun ah:comp-archives-to-wrolo ()
  "With current-buffer a gnus article or vm-format mail message, parse
out the relevant information to create as much of an wrolo.el entry as
possible." 
  (interactive)
  (ah:define-topic)
  (ah:define-reference)
  (ah:define-ftp-site)
  (ftpg-print-hostname))

(defun ah:define-reference ()
  (goto-char (point-min))
  (search-forward "From:" (point-max))
  (beginning-of-line nil)
  (let ((start (point))
	(end (progn
	       (end-of-line nil)
	       (point))))
    (setq ah-reference (buffer-substring start end))))

(defun ah:define-topic ()
  (goto-char (point-min))
  (search-forward "Subject" (point-max))
  (let ((start (progn
		 (search-forward "] ")
		 (point)))
	(end (progn
	       (end-of-line nil)
	       (point))))
    (setq ah-topic (buffer-substring start end))))

;;; very useful code obtained from Rodney Peck II (rodney@ipl.rpi.edu)

;-----------------------
;; grab-ftp.el
;; a thing to find ftp references and make the ftp connect automatically.
;; (c)1990 Rodney Peck II   rodney@ipl.rpi.edu
;; 
;; please mail changes you might make back to me.  I reserve the right
;; to call this my own, but it will be released to the FSF when it's done.
;; Image Processing Lab, Rensselaer Polytechnic Institute
;; $Header: /home/rodney/elisp/RCS/grab-ftp.el,v 1.2 90/02/07 19:41:00 rodney Exp $

(defun ah:define-ftp-site ()
  "brannon removed the set-buffer command"
  (interactive)
  (setq ah-ftp-site 
		  (save-excursion
		    (let ((host (ftpg-find-hostname))
			  (number (ftpg-internet-number)))
		      (list host number))))
  (setq ah-ftp-site 
	(concat
	 "/anonymous@"
	 (if (car ah-ftp-site)
	     (car ah-ftp-site)
	   (if (cdr ah-ftp-site)
	       (cdr ah-ftp-site)
	     ""))
	 ":"))
  (setq ah-ftp-file
	(if
	    (setq M (progn
		      (goto-char (point-min))
		      (if (search-forward "Archive-name:" (point-max) nil)
			  (next-line 2)
			 (progn (goto-char (point-min) (next-line 5))))
		      (previous-line 1) (beginning-of-line 1)
		      (search-forward "/" (point-max) nil)
		      (search-backward " " (point-min) nil)
		      (let ((start (point))
			    (end (progn
				   (re-search-forward "\\(/[A-Za-z0-9._]+\\)+"))))
			(buffer-substring start (point)))))
	    M
	"")))
  
	  
	  
    
(defun ftpg-print-hostname ()
  (interactive)
  (message (format "host: %s  number: %s file: %s site: %s" (ftpg-find-hostname) (ftpg-internet-number) ah-ftp-file ah-ftp-site)))

(defun ftpg-internet-number ()
  (save-excursion
    (goto-char (point-min))		; top of buffer
    (if (re-search-forward "\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" 
			   (point-max) t) ; match internet
	(progn
	  (setq L (point))
	  (buffer-substring (match-beginning 1) (match-end 1))))))

(defun carmemberp (thing lst)
  (cond ((null lst) nil)
	((equal (car (car lst)) thing) t)
	(t (member thing (cdr lst)))))

;; cl.el

(defun member (item list)
  "Look for ITEM in LIST; return first link in LIST whose car is `eql' to ITEM."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (endp ptr)))
      (cond ((eql item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defun endp (x)
  "t if X is nil, nil if X is a cons; error otherwise."
  (if (listp x)
      (null x)
    (error "endp received a non-cons, non-null argument `%s'"
	   (prin1-to-string x))))


;;

(defun skip-over-regexp (reg bound)
    (while
	(re-search-forward reg bound t)
      nil)
    (next-line 1))

(defvar sigpt nil)  

(defvar ftpg-syntax-table
  (let ((ours (standard-syntax-table)))
    (modify-syntax-entry ?- "w" ours)
    ours)
  "the syntax table we use to find hostnames and stuff easier")
	
(defun ftpg-find-hostname ()
  (save-excursion
	(let ((old-syntax (syntax-table))
	      (signature-point
	 (progn 
	   (goto-char (point-min))		; top of buffer
	   (if (re-search-forward "^-- ?$" (point-max) t)
	       (point) (point-max)))))
	  (set-syntax-table ftpg-syntax-table)
	  (setq sigpt signature-point)
	  (goto-char (point-min))		; top of buffer
	  (next-line 2) ; brannon
	  (let ((hosts) (host) (scores))
	    (while 
		(re-search-forward active-hyper-site-regexp		 
		 signature-point t) ; match hostname
		 (setq host
		       (downcase (buffer-substring
				  (match-beginning 1) (match-end 1))))
		 (if (not (carmemberp host hosts))
		     (setq hosts (cons (list host (point))
				       hosts))))
	    (set-syntax-table old-syntax)
	    (let ((best-score (point-max))
		  (best-hostname)
		  (score)
		  (hostname)
		  (position))
	      (while hosts
		(setq host (car hosts))
		(setq hosts (cdr hosts))
		(setq hostname (car host))
		(setq position (car (cdr host)))
		(goto-char position)
		(setq score (- position 
			       (if (re-search-backward
				    "ftp\\|on\\|from\\|via\\|archive-site"
				    (point-min) t)
				   (point) (point-min))))
		(if (< score best-score)
		    (progn (setq best-score score)
			   (setq best-hostname hostname))))
	      best-hostname)))))

(defun ftpg-connect-host ()
  (interactive)
  (process-send-string "shell" 
		       (format "ftp %s\n" (ftpg-find-hostname) hostname)))


;;; quasi hyper-makefile

(setq active-hyper-manifest
      '(".dired" ".hypb" "active-hyper.Admin.el" 
	"active-hyper.Changelog" "active-hyper.Install.el"
	"active-hyper.Makefile" "active-hyper.credits"
	"active-hyper.documentation" "active-hyper.el"
	"active-hyper.list.big.otl" "active-hyper.list.frq.otl"
	"active-hyper.main.menu" "active-hyper.to.do"
	"active-hyper.usenet"))

(setq ah-dest-dir "/anonymous@klotho.cs.caltech.edu:/pub/")

(defun remote-install ()
  (interactive)
  (mapcar 'copy-to ah-for-remote-install))

(defun copy-to (M)
  (copy-file M (concat active-hyper-manifest M)))
	
