#ifndef machine_h
#define machine_h
typedef unsigned int Address;
typedef unsigned char Byte;
typedef unsigned int Word;

#define NREG 16

#define ARGP 12
#define FRP 13
#define STKP 14
#define PROGCTR 15

#define BITSPERBYTE 8
#define BITSPERWORD (BITSPERBYTE * sizeof(Word))

#define nargspassed(frame) argn(0, frame)

#include "source.h"
#include "symbols.h"

Address pc;
Address prtaddr;

printinst(/* lowaddr, highaddr */);
printninst(/* count, addr */);
Address printdata(/* lowaddr, highaddr, format */);
printndata(/* count, startaddr, format */);
printvalue(/* v, format */);
printerror(/*  */);
endprogram(/*  */);
dostep(/* isnext */);
Address nextaddr(/* startaddr, isnext */);
setbp(/* addr */);
unsetbp(/* addr */);
Boolean isbperr(/*  */);
beginproc(/* p, argc */);
#endif
