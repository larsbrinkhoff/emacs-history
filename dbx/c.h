#ifndef c_h
#define c_h
# include "tree.h"
c_init(/*  */);
Boolean c_typematch(/* type1, type2 */);
c_printdecl(/* s */);
c_listparams(/* s */);
c_printval(/* s */);
String c_classname(/* s */);
Node c_buildaref(/* a, slist */);
int c_evalaref(/* s, i */);
#endif
