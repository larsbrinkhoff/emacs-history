#ifndef eval_h
#define eval_h

#include "machine.h"

#define STACKSIZE 20000

typedef Char Stack;

#define push(type, value) { \
    ((type *) (sp += sizeof(type)))[-1] = (value); \
}

#define pop(type) ( \
    (*((type *) (sp -= sizeof(type)))) \
)

#define alignstack() { \
    sp = (Stack *) (( ((int) sp) + sizeof(int) - 1)&~(sizeof(int) - 1)); \
}

Stack stack[STACKSIZE];
Stack *sp ;
Boolean useInstLoc ;
eval(/* p */);
evalcmdlist(/* cl */);
rpush(/* addr, len */);
Boolean canpush(/* n */);
pushsmall(/* t, v */);
long popsmall(/* t */);
Boolean cond(/* p */);
Address lval(/* p */);
trace(/* p */);
stop(/* p */);
assign(/* var, exp */);
gripe(/*  */);
help(/*  */);
setout(/* filename */);
unsetout(/*  */);
Boolean isredirected(/*  */);
#endif
