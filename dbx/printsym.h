#ifndef printsym_h
#define printsym_h
String classname(/* s */);
printentry(/* s */);
printexit(/* s */);
printcall(/* s, t */);
printrtn(/* s */);
printparams(/* f, frame */);
Boolean should_print(/* s */);
printv(/* s, frame */);
printname(/* f, s */);
printwhich(/* f, s */);
printwhereis(/* f, s */);
printdecl(/* s */);
psym(/* s */);
printval(/* t */);
printrecord(/* s */);
printarray(/* a */);
prtreal(/* r */);
printchar(/* c */);
#endif
