/* m- file for the Tandem Integrity S2.  */

#include "m-mips.h"

/* This overrides some of the usual support for the mips and system V.3.  */

/* The S2 does not know about utimes() */
#define USE_UTIME

/* The operating system apparently defines TIOCGETC
   but it doesn't work.  */
#undef BROKEN_TIOCGETC

/* rs@ai.mit.edu said this was necessary for it to work.  However, some
   user of this machine ought to try to get subprocesses to work.  */
#undef subprocesses
