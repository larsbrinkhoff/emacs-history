;; Copyright (C) 1985 Free Software Foundation
;; Written by Dick King (king@kestrel).

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;;; This is a rudimentry backquote package written by D. King,
 ;;; king@kestrel, on 8/31/85.  (` x) is a macro
 ;;; that expands to a form that produces x.  (` (a b ..)) is
 ;;; a macro that expands into a form that produces a list of what a b
 ;;; etc. would have produced.  Any element can be of the form
 ;;; (, <form>) in which case the resulting form evaluates
 ;;; <form> before putting it into place, or (,@ <form>), in which
 ;;; case the evaluation of <form> is arranged for and each element
 ;;; of the result (which must be a (possibly null) list) is inserted.
;;; As an example, the immediately following macro push (v l) could
 ;;; have been written 
;;;    (defmacro push (v l)
;;;         (` (setq (, l) (cons (,@ (list v l))))))
 ;;; although
;;;    (defmacro push (v l)
;;;         (` (setq (, l) (cons (, v) (, l)))))
 ;;; is far more natural.  The magic atoms ,
 ;;; and ,@ are user-settable and list-valued.  We recommend that
 ;;; things never be removed from this list lest you break something
 ;;; someone else wrote in the dim past that comes to be recompiled in
 ;;; the distant future.

;;; LIMITATIONS: tail consing is not handled correctly.  Do not say
 ;;; (` (a . (, b))) - say (` (a (,@ b)))
 ;;; which works even if b is not list-valued.
;;; No attempt is made to handle vectors.  (` [a (, b) c]) doesn't work.
;;; Sorry, you must say things like
 ;;; (` (a (,@ 'b))) to get (a . b) and 
 ;;; (` ((, ',) c)) to get (, c) - [(` (a , b)) will work but is a bad habit]
;;; I haven't taught it the joys of nconc.
;;; (` atom) dies.  (` (, atom)) or anything else is okay.

;;; BEWARE BEWARE BEWARE
 ;;; inclusion of (,atom) rather than (, atom) or (,@atom) rather than
 ;;; (,@ atom) will result in errors that will show up very late.
 ;;; This is so crunchy that I am considering including a check for
 ;;; this or changing the syntax to ... ,(<form>).  RMS: opinion?


(provide 'backquote)

;;; a raft of general-purpose macros follows.  See the nearest
 ;;; Commonlisp manual.
(defmacro push (v l)
  "Pushes evaluated first form onto second unevaluated object
a list-value atom"
  (list 'setq l (list 'cons v l)))

(defmacro caar (l)
  (list 'car (list 'car l)))

(defmacro cadr (l)
  (list 'car (list 'cdr l)))

(defmacro cdar (l)
  (list 'cdr (list 'car l)))

(defmacro cddr (l)
  (list 'cdr (list 'cdr l)))


;;; These two advertised variables control what characters are used to
 ;;; unquote things.  I have included , and ,@ as the unquote and
 ;;; splice operators, respectively, to give users of MIT CADR machine
 ;;; derivitive machines a warm, cosy feeling.

(defconst backquote-unquote '(,)
  "*A list of all objects that stimulate unquoting in `.  Memq test.")


(defconst backquote-splice '(,@)
  "*A list of all objects that stimulate splicing in `.  Memq test.")


;;; This is the interface 
(defmacro ` (form)
  "(` FORM) Expands to a form that will generate FORM.
FORM is `almost quoted' -- see backquote.el for a description."
  (bq-make-maker form))

;;; We develop the method for building the desired list from
 ;;; the end towards the beginning.  The contract is that there be a
 ;;; variable called state and a list called tailmaker, and that the form
 ;;; (cons state tailmaker) deliver the goods.  Exception - if the
 ;;; state is quote the tailmaker is the form itself.
;;; This function takes a form and returns what I will call a maker in
 ;;; what follows.  Evaluating the maker would produce the form,
 ;;; properly evaluated according to , and ,@ rules.
;;; I work backwards - it seemed a lot easier.  The reason for this is
 ;;; if I'm in some sort of a routine building a maker and I switch
 ;;; gears, it seemed to me easier to jump into some other state and
 ;;; glue what I've already done to the end, than to to prepare that
 ;;; something and go back to put things together.
(defun bq-make-maker (form)
  "Given one argument, a `mostly quoted' object, produces a maker.
See backquote.el for details"
  (let ((tailmaker (quote nil)) (qc 0) (ec 0) (state nil))
    (mapcar 'bq-iterative-list-builder (reverse form))
    (and state
	 (cond ((eq state 'quote)
		(list state tailmaker))
	       ((= (length tailmaker) 1)
		(funcall (cadr (assq state bq-singles)) tailmaker))
	       (t (cons state tailmaker))))))

;;; There are exceptions - we wouldn't want to call append of one
 ;;; argument, for example.
(defconst bq-singles '((quote bq-quotecar)
		       (append car)
		       (list bq-make-list)
		       (cons bq-id)))

(defun bq-id (x) x)

(defun bq-quotecar (x) (list 'quote (car x)))

(defun bq-make-list (x) (cons 'list x))

;;; fr debugging use only
;(defun funcalll (a b) (funca
