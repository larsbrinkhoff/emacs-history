#ifndef source_h
#define source_h
typedef int Lineno;

String cursource;
Lineno curline;
Lineno cursrcline;

#define LASTLINE 0		/* recognized by printlines */

#include "lists.h"

List sourcepath;
printlines(/* l1, l2 */);
String findsource(/* filename */);
File opensource(/* filename */);
setsource(/* filename */);
getsrcpos(/*  */);
printsrcpos(/*  */);
edit(/* filename */);
#endif
