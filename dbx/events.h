#ifndef events_h
#define events_h
typedef struct Event *Event;
typedef struct Breakpoint *Breakpoint;

Boolean inst_tracing;
Boolean single_stepping;
Boolean isstopped;

#include "symbols.h"

Symbol linesym;
Symbol procsym;
Symbol pcsym;
Symbol retaddrsym;

#define addevent(cond, cmdlist) event_alloc(false, cond, cmdlist)
#define event_once(cond, cmdlist) event_alloc(true, cond, cmdlist)

bpinit(/*  */);
Event event_alloc(/* istmp, econd, cmdlist */);
delevent(/* id */);
Symbol tcontainer(/* exp */);
Boolean canskip(/* f */);
status(/*  */);
printevent(/* e */);
bpfree(/*  */);
Boolean bpact(/*  */);
traceon(/* inst, event, cmdlist */);
traceoff(/* id */);
printnews(/*  */);
callnews(/* iscall */);
printifchanged(/* p */);
stopifchanged(/* p */);
trfree(/*  */);
fixbps(/*  */);
setallbps(/*  */);
unsetallbps(/*  */);
#endif
