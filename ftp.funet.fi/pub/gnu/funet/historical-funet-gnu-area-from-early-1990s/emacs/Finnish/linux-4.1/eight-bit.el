;;; eight-bit.el
;;; allow eight-bit input without quoting
;;; Copyright Rick Sladkey <jrs@world.std.com>

;;; This file is not part of GNU Emacs but is distributed
;;; under the same conditions as GNU Emacs.

;;; Load this file from your ~/.emacs and all keystrokes
;;; that generate characters with the high bit set will
;;; be input directly into the buffer without quoting.

;;; You will lose the normal Emacs meta-key functionality
;;; unless your meta key generates ESC followed by the
;;; character.  Using ESC key commands will still work.

;;; You should choose eight-bit-prefix-char to be any
;;; unused control character.  By default, Emacs leaves
;;; Control-\ and Control-^ undefined.

(defun eight-bit-self-insert-command (arg)
  "Like self-insert-command but adds the eighth bit."
  (interactive "p")
  (let ((key (string-to-char (substring (this-command-keys) -1))))
    (insert (make-string arg (logior key ?\200)))))

(defvar eight-bit-prefix-char ?\C-\\
  "Character to be used as the meta-key prefix char.
It can be used directly to input meta-fied versions of ordinary characters.
It should not be ESC.")

(fset 'eight-bit-map (make-vector 128 'eight-bit-self-insert-command))
(setq meta-prefix-char eight-bit-prefix-char)
(global-set-key (char-to-string eight-bit-prefix-char) 'eight-bit-map)
