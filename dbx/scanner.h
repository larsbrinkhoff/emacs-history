#ifndef scanner_h
#define scanner_h
typedef int Token;
String initfile ;
scanner_init(/*  */);
Token yylex(/*  */);
yyerror(/* s */);
gobble(/*  */);
setinput(/* filename */);
Boolean popinput(/*  */);
Boolean isstdin(/*  */);
shellline(/*  */);
beginshellmode(/*  */);
print_token(/* f, t */);
#endif
