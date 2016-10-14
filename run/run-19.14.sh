rm README-c++
rm README-sol2
(cd lisp
 mv c++-mode-1.el cplus-md1.el
 mv c++-mode.el cplus-md.el
 mv c++-mode.elc cplus-md.elc
 rm cmulisp.el cmulisp.elc
 (cd forms-mode
  mv * ..)
 rmdir forms-mode
 rm term/ChangeLog)	# this has been integrated into lisp/ChangeLog
(cd src/s
 rm 3700.h)
