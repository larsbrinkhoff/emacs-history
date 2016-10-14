rm -f etc/MSDOS
rm -f lib-src/getdate.c
rm -f lib-src/getdate.y
rm -f lib-src/timer.c
rm -f lib-src/wakeup.c
mv src/s/lignux.h src/s/gnu-linux.h
### delete and rebuild these, to avoid confusing the byte-compiler.
rm -f lisp/ediff-mult.elc lisp/ediff-util.elc lisp/gnus-cache.elc lisp/gnus.elc
