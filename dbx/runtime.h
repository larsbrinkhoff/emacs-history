#ifndef runtime_h
#define runtime_h
typedef struct Frame *Frame;

#include "machine.h"
Frame findframe(/* f */);
Address return_addr(/*  */);
pushretval(/* len, isindirect */);
Address locals_base(/* frp */);
Address args_base(/* frp */);
Word savereg(/* n, frp */);
Word argn(/* n, frp */);
Address fparamaddr(/* a */);
wherecmd(/*  */);
dump(/*  */);
findbeginning(/* f */);
Address firstline(/* f */);
runtofirst(/*  */);
Address lastaddr(/*  */);
Boolean isactive(/* f */);
callproc(/* procnode, arglist */);
procreturn(/* f */);
popenv(/*  */);
flushoutput(/*  */);
#endif
