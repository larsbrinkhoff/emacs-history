;;; tmenu.el --- emulate the Lucid menu features.

;; Keywords: emulations

;;; Commentary:

;; We don't emulate the variable current-menubar
;; because there is no real way to do that.

;;; Code:

(defvar add-menu-item-count 0)


;; Return a menu keymap corresponding to a Lucid-style menu list
;; MENU-ITEMS, and with name MENU-NAME.
(defun make-lucid-menu-keymap (menu-name menu-items)
  (let ((menu (make-sparse-keymap menu-name)))
    ;; Process items in reverse order,
    ;; since the define-key loop reverses them again.
    (setq menu-items (reverse menu-items))
    (while menu-items
      (let* ((item (car menu-items))
	     (callback (if (vectorp item) (aref item 1)))
	     command enabler name)
	(cond ((stringp item)
	       (setq command nil)
	       (setq name item))
	      ((listp item)
	       (setq command
		     (make-lucid-menu-keymap (car item) (cdr item)))
	       (setq name (car item)))
	      (t
	       (setq command (make-symbol (format "menu-function-%d"
						  add-menu-item-count)))
	       (setq enabler (make-symbol (format "menu-function-%d-enabler"
						  add-menu-item-count)))
	       (setq add-menu-item-count (1+ add-menu-item-count))
	       (put command 'menu-enable enabler)
	       (set enabler (aref item 2))
	       (setq name (aref item 0))	       
	       (if (symbolp callback)
		   (fset command callback)
		 (fset command (list 'lambda () '(interactive) callback)))))
	(define-key menu (vector (intern name)) (cons name command)))
      (setq menu-items (cdr menu-items)))
    menu))


(defun popup-menu (menu-desc)
  "Pop up the given menu.
A menu is a list of menu items, strings, and submenus.

The first element of a menu must be a string, which is the name of the
menu.  This is the string that will be displayed in the parent menu, if
any.  For toplevel menus, it is ignored.  This string is not displayed
in the menu itself.

A menu item is a vector of three or four elements:

 - the name of the menu item (a string);
 - the `callback' of that item;
 - whether this item is active (selectable);
 - and an optional string to append to the name.

If the `callback' of a menu item is a symbol, then it must name a command.
It will be invoked with `call-interactively'.  If it is a list, then it is
evaluated with `eval'.

The fourth element of a menu item is a convenient way of adding the name
of a command's ``argument'' to the menu, like ``Kill Buffer NAME''.

If an element of a menu is a string, then that string will be presented in
the menu as unselectable text.

If an element of a menu is a string consisting solely of hyphens, then that
item will be presented as a solid horizontal line.

If an element of a menu is a list, it is treated as a submenu.  The name of
that submenu (the first element in the list) will be used as the name of the
item representing this menu on the parent.

The syntax, more precisely:

   form		:=  <something to pass to `eval'>
   command	:=  <a symbol or string, to pass to `call-interactively'>
   callback 	:=  command | form
   active-p	:=  <t or nil, whether this thing is selectable>
   text		:=  <string, non selectable>
   name		:=  <string>
   argument	:=  <string>
   menu-item	:=  '['  name callback active-p [ argument ]  ']'
   menu		:=  '(' name [ menu-item | menu | text ]+ ')'
"
  (let ((menu (make-lucid-menu-keymap (car menu-desc) (cdr menu-desc)))
	(pos (mouse-position))
	answer)
    (setq answer (x-popup-menu (list (list (nth 1 pos) (nthcdr 2 pos))
				     (car pos))
			       menu))
    (call-interactively
     (lookup-key menu
		 (vector answer)))))

(popup-menu
 '( "foo!!" ["x" xxx t] ["y" (message "Hi") nil] ("m1" ["z" zzz t]) "bar"))

;;; tmenu.el ends here
