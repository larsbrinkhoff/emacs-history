#ifndef coredump_h
#define coredump_h
#define coredump_readin(m, r, s) coredump_xreadin(&(m), r, &(s))

#include "machine.h"
coredump_xreadin(/* mask, reg, signo */);
coredump_close(/*  */);
coredump_readtext(/* buff, addr, nbytes */);
coredump_readdata(/* buff, addr, nbytes */);
#endif
