#ifndef mappings_h
#define mappings_h
#include "machine.h"
#include "source.h"
#include "symbols.h"

typedef struct {
    Address addr;
    String filename;
    Lineno lineindex;		/* index to first linetab entry */
} Filetab;

typedef struct {
    Lineno line;
    Address addr;
} Linetab;

Filetab *filetab;
Linetab *linetab;

#define NOADDR ((Address) -1)	/* no address for line or procedure */

String srcfilename(/* addr */);
Lineno srcline(/* addr */);
Lineno linelookup(/* addr */);
Address objaddr(/* line, name */);
newfunc(/* f, addr */);
Symbol whatblock(/* addr */);
ordfunctab(/*  */);
clrfunctab(/*  */);
dumpfunctab(/*  */);
#endif
