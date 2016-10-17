;;---------------------------------------------------------------------------
;;
;; Functions to change the characterset from English to Swedish and vice versa
;; TeX added.
;;
;;---------------------------------------------------------------------------
(defun toswe nil
     "Swedish chars without esc"
      (local-set-key "[" 'iso-adieresis-insert)
      (local-set-key "]" 'iso-aring-insert)
      (local-set-key "\\" 'iso-odieresis-insert)
      (local-set-key "{" 'iso-Adieresis-insert)
      (local-set-key "}" 'iso-Aring-insert)
      (local-set-key "|" 'iso-Odieresis-insert))

(defun totex nil
     "Swedish chars without esc"
      (local-set-key "[" 'TeX-adieresis-insert)
      (local-set-key "]" 'TeX-aring-insert)
      (local-set-key "\\" 'TeX-odieresis-insert)
      (local-set-key "{" 'TeX-Adieresis-insert)
      (local-set-key "}" 'TeX-Aring-insert)
      (local-set-key "|" 'TeX-Odieresis-insert))

(defun toeng nil
       "  "
       (local-set-key "[" 'self-insert-command)
       (local-set-key "]" 'self-insert-command)
       (local-set-key "\\" 'self-insert-command)
       (local-set-key "{" 'self-insert-command)
       (local-set-key "}" 'self-insert-command)
       (local-set-key "|" 'self-insert-command))

;;---------------------------------------------------------------------------
;;
;; Always have Swedish characters as Meta-[ ] \
;;
;;---------------------------------------------------------------------------
(define-key esc-map "[" 'iso-adieresis-insert)
(define-key esc-map "]" 'iso-aring-insert)
(define-key esc-map "\\" 'iso-odieresis-insert)
(define-key esc-map "{" 'iso-Adieresis-insert)
(define-key esc-map "}" 'iso-Aring-insert)
(define-key esc-map "|" 'iso-Odieresis-insert)
(define-key esc-map "e" 'iso-eacute-insert)

;;---------------------------------------------------------------------------
;;
;; The character-inserting functions
;;
;;---------------------------------------------------------------------------
(defun iso-Aring-insert nil
  "Insert AA."
  (interactive)
  (insert "\305"))

(defun iso-aring-insert nil
  "Insert aa."
  (interactive)
  (insert "\345"))

(defun iso-Adieresis-insert nil
  "Insert AE."
  (interactive)
  (insert "\304"))

(defun iso-adieresis-insert nil
  "insert ae."
  (interactive)
  (insert "\344"))

(defun iso-Odieresis-insert nil
  "Insert OE."
  (interactive)
  (insert "\326"))

(defun iso-odieresis-insert nil
  "insert oe."
  (interactive)
  (insert "\366"))

(defun iso-eacute-insert nil
  "insert eacute."
  (interactive)
  (insert "\351"))


;;;;;
(defun TeX-Aring-insert nil
  "Insert AA."
  (interactive)
  (insert "{\\AA}"))

(defun TeX-aring-insert nil
  "Insert aa."
  (interactive)
  (insert "{\\aa}"))

(defun TeX-Adieresis-insert nil
  "Insert AE."
  (interactive)
  (insert "\\\"A"))

(defun TeX-adieresis-insert nil
  "insert ae."
  (interactive)
  (insert "\\\"a"))

(defun TeX-Odieresis-insert nil
  "Insert OE."
  (interactive)
  (insert "\\\"O"))

(defun TeX-odieresis-insert nil
  "insert oe."
  (interactive)
  (insert "\\\"o"))
