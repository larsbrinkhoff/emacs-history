/* Mark end of data space to dump as pure,ld -X -o temacs crt0.o dispnew.o scroll.o xdisp.o window.o  term.o cm.o TrmXTERM.o  emacs.o keyboard.o macros.o keymap.o sysdep.o  buffer.o insdel.o marker.o  minibuf.o fileio.o dired.o filemode.o userid.o  cmds.o casefiddle.o indent.o search.o regex.o undo.o  alloc.o data.o doc.o editfns.o callint.o  eval.o fns.o print.o read.o doprnt.o  abbrev.o syntax.o unexec.o mocklisp.o bytecode.o  process.o callproc.o  malloc.o tparam.o  lastfile.o -ltermlib alloca.o  -lc -lg
s described in the
document "GNU Emacs copying permission notice".   An exact copy
of the document is supposed to have been given to you along with
GNU Emacs so that you can know how you may redistribute it all.
It should be in a file named COPYING.  Among other things, the
copyright notice and this notice must be preserved on all copies.  */


/* How this works:

 Fdump_emacs dumps everything up to my_edata as text space (pure).

 The files of Emacs are written so as to have no initialized
 data that can ever need to be altered except at the first startup.
 This is so that those words can be dumped as sharable text.

 It is not possible to exercise such control over library files.
 So it is necessary to refrain from making their data areas shared.
 Therefore, this file is loaded following all the files of Emacs
 but before library files.
 As a result, the symbol my_edata indicates the point
 in data space between data coming from Emacs and data
 coming from libraries.
*/

int my_edata = 0;
