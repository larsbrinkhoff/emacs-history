#ifndef fortran_h
#define fortran_h
fortran_init(/*  */);
Boolean fortran_typematch(/* type1, type2 */);
fortran_printdecl(/* s */);
fortran_listparams(/* s */);
fortran_printval(/* s */);
String fortran_classname(/* s */);
Node fortran_buildaref(/* a, slist */);
int fortran_evalaref(/* s, i */);
#endif
