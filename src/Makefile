# Makefile for GNU Emacs.
# Copyright (C) 1985 Richard M. Stallman.

# This file is part of GNU Emacs.

# GNU Emacs is distributed in the hope that it will be useful,
# but without any warranty.  No author or distributor
# accepts responsibility to anyone for the consequences of using it
# or for whether it serves any particular purpose or works at all,
# unless he says so in writing.

# Everyone is granted permission to copy, modify and redistribute
# GNU Emacs, but only under the conditions described in the
# document "GNU Emacs copying permission notice".   An exact copy
# of the document is supposed to have been given to you along with
# GNU Emacs so that you can know how you may redistribute it all.
# It should be in a file named COPYING.  Among other things, the
# copyright notice and this notice must be preserved on all copies.


LIBES=
CFLAGS= -g -Demacs -DDisplayDebug
# DO NOT use -R.  There is a special hack described in lastfile.c
# which is used instead.  Some initialized data areas are modified
# at initial startup, then labeled as part of the text area when
# Emacs is dumped for the first time, and never changed again.

# You may replace -g with -O if you don't believe in debugging.

# -Demacs is needed to make some files produce the
# correct version for use in Emacs.
# -DDisplayDebug can be removed safely;
# that would make one or two files a little smaller.

.SUFFIXES: .v

.v.o:
	co -q $<
	${CC} ${CFLAGS} -c $*.c
	${RM} -f $*.c

# lastfile must follow all files
# whose initialized data areas should be dumped as pure by dump-emacs.
obj=    dispnew.o scroll.o xdisp.o window.o \
	term.o cm.o TrmXTERM.o \
	emacs.o keyboard.o macros.o keymap.o sysdep.o \
	buffer.o insdel.o marker.o \
	minibuf.o fileio.o dired.o filemode.o userid.o \
	cmds.o casefiddle.o indent.o search.o regex.o undo.o \
	alloc.o data.o doc.o editfns.o callint.o \
	eval.o fns.o print.o read.o doprnt.o \
	abbrev.o syntax.o unexec.o mocklisp.o bytecode.o \
	process.o callproc.o \
	malloc.o tparam.o \
	lastfile.o

lisp=	../lisp/simple.elc ../lisp/files.elc ../lisp/window.elc \
	../lisp/indent.elc ../lisp/loaddefs.el \
	../lisp/startup.elc ../lisp/lisp.elc \
	../lisp/page.elc ../lisp/register.elc \
	../lisp/paragraphs.elc ../lisp/lisp-mode.elc \
	../lisp/text-mode.elc ../lisp/fill.elc \
	../lisp/c-mode.elc ../lisp/isearch.elc \
	../lisp/replace.elc ../lisp/abbrev.elc \
	../lisp/buff-menu.elc

# just to be sure the sh is used
SHELL=/bin/sh

all: xemacs

xemacs: temacs ../etc/DOCSTR ${lisp}
	./temacs -batch -l inc-version
	./temacs -batch -l loadup dump

../etc/DOCSTR: ${obj} ${lisp} auxdoc.c
	../etc/make-docfile auxdoc.c ${obj} ${lisp} ../lisp/version.el > ../etc/DOCSTR

temacs: crt0.o ${obj} alloca.o
	ld -X -o temacs crt0.o ${obj} -ltermlib alloca.o ${LIBES} -lc -lg

paths.h: paths.h.dist
	@sh -c "if test -r $@; then echo $? is newer than $@; exit 1;\
	  else exit 0; fi"
	cp $? $@

config.h: config.h.dist
	@sh -c "if test -r $@; then echo $? is newer than $@; exit 1;\
	  else exit 0; fi"
	cp $? $@

term.o : term.c termchar.h termhooks.h termopts.h config.h cm.h
TrmXTERM.o : TrmXTERM.c dispextern.h config.h

# This file is really needed only on certain machines
# but there is no way to conditionalize it here.
# So in the file are conditionals to make it empty
# where it is not needed.

alloca.o : alloca.s config.h
	/lib/cpp -E alloca.s > alloca.t
	as -o alloca.o alloca.t
	rm alloca.t

# Nearly all the following files depend on lisp.h,
# but it is not included as a dependency because
# it is so often changed in ways that do not require any recompilation
# and so rarely changed in ways that do require any.

abbrev.o : abbrev.c buffer.h commands.h config.h 
buffer.o : buffer.c syntax.h buffer.h commands.h window.h config.h 
callint.o : callint.c window.h commands.h buffer.h config.h 
callproc.o : callproc.c paths.h buffer.h commands.h config.h 
casefiddle.o : casefiddle.c commands.h buffer.h config.h 
cm.o : cm.c cm.h termhooks.h
cmds.o : cmds.c syntax.h buffer.h commands.h config.h 
crt0.o : crt0.c 
dired.o : dired.c commands.h buffer.h config.h 
dispnew.o : dispnew.c commands.h window.h buffer.h dispextern.h termchar.h termopts.h config.h
doc.o : doc.c config.h
doprnt.o : doprnt.c
editfns.o : editfns.c window.h buffer.h config.h 
emacs.o : emacs.c commands.h config.h 
fileio.o : fileio.c window.h buffer.h config.h 
filemode.o : filemode.c 
indent.o : indent.c window.h indent.h buffer.h config.h termchar.h termopts.h
insdel.o : insdel.c window.h buffer.h config.h 
keyboard.o : keyboard.c termchar.h termhooks.h termopts.h buffer.h commands.h window.h macros.h config.h 
keymap.o : keymap.c buffer.h commands.h config.h 
lastfile.o : lastfile.c 
macros.o : macros.c window.h buffer.h commands.h macros.h 
malloc.o : malloc.c 
marker.o : marker.c buffer.h config.h 
minibuf.o : minibuf.c syntax.h window.h buffer.h commands.h config.h 
mocklisp.o : mocklisp.c config.h
process.o : process.c process.h buffer.h window.h config.h 
regex.o : regex.c syntax.h buffer.h config.h regex.h 
scroll.o : scroll.c termchar.h config.h dispextern.h
search.o : search.c regex.h commands.h buffer.h syntax.h config.h 
syntax.o : syntax.c syntax.h buffer.h commands.h config.h 
sysdep.o : sysdep.c config.h dispextern.h termhooks.h termchar.h termopts.h
tparam.o : tparam.c
undo.o : undo.c buffer.h commands.h undo.h config.h 
unexec.o : unexec.c config.h
userid.o : userid.c 
window.o : window.c indent.h commands.h window.h buffer.h config.h 
xdisp.o : xdisp.c macros.h commands.h indent.h buffer.h dispextern.h termchar.h window.h config.h 

# The files of Lisp proper

alloc.o : alloc.c window.h buffer.h config.h 
bytecode.o : bytecode.c buffer.h config.h 
data.o : data.c buffer.h config.h 
eval.o : eval.c commands.h config.h
fns.o : fns.c commands.h config.h
print.o : print.c process.h window.h buffer.h config.h 
read.o : read.c buffer.h paths.h config.h 
