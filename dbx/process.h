#ifndef process_h
#define process_h

typedef struct Process *Process;

Process process;

#define DEFSIG -1

#include "machine.h"

process_init(/*  */);
Word reg(/* n */);
setreg(/* n, w */);
start(/* argv, infile, outfile */);
arginit(/*  */);
newarg(/* arg */);
inarg(/* filename */);
outarg(/* filename */);
run(/*  */);
cont(/* signo */);
fixintr(/*  */);
resume(/* signo */);
stepc(/*  */);
next(/*  */);
stepto(/* addr */);
printstatus(/*  */);
printloc(/*  */);
Boolean notstarted(/* p */);
Boolean isfinished(/* p */);
Integer errnum(/* p */);
Integer exitcode(/* p */);
iread(/* buff, addr, nbytes */);
iwrite(/* buff, addr, nbytes */);
dread(/* buff, addr, nbytes */);
dwrite(/* buff, addr, nbytes */);
pstep(/* p */);
psigtrace(/* p, sig, sw */);
unsetsigtraces(/* p */);
printptraceinfo(/*  */);
#endif
