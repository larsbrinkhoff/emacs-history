#ifndef main_h
#define main_h

#define isterm(file)	(interactive or isatty(fileno(file)))

#include <sgtty.h>

typedef struct {
    struct sgttyb sgttyb;
    struct tchars tchars;
    int lmode;
    struct ltchars ltchars;
    int fcntl_flags;
    int fcntl_owner;
  } Ttyinfo;

Boolean coredump;		/* true if using a core dump */
Boolean runfirst;		/* run program immediately */
Boolean interactive;		/* standard input IS a terminal */
Boolean lexdebug;		/* trace yylex return values */
Boolean tracebpts;		/* trace create/delete breakpoints */
Boolean traceexec;		/* trace process execution */
Boolean tracesyms;		/* print symbols as their read */
File corefile;			/* File id of core dump */
init(/*  */);
reinit(/* argv, infile, outfile */);
erecover(/*  */);
savetty(/* f, t */);
restoretty(/* f, t */);
quit(/* r */);
#endif
