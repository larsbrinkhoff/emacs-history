#ifndef tree_h
#define tree_h
#include "lists.h"

typedef struct Node *Node;
typedef Node Command;
typedef List Cmdlist;

#include "operators.h"
#include "symbols.h"
#include "events.h"

#define MAXNARGS 5

struct Node {
    Operator op;
    Symbol nodetype;
    union treevalue {
	Symbol sym;
	Name name;
	long lcon;
	double fcon;
	String scon;
	Node arg[MAXNARGS];
	struct {
	    Node cond;
	    Cmdlist actions;
	} event;
	struct {
	    Boolean inst;
	    Event event;
	    Cmdlist actions;
	} trace;
	struct {
	    Boolean source;
	    Boolean skipcalls;
	} step;
	struct {
	    String mode;
	    Node beginaddr;
	    Node endaddr;
	    Integer count;
	} examine;
    } value;
};

#define evalcmd(cmd) eval(cmd)
#define cmdlist_append(cmd, cl) list_append(list_item(cmd), nil, cl)

Node build(/* op, args */);
Cmdlist buildcmdlist(/* cmd */);
Node amper(/* p */);
Node concrete(/* p */);
printcmd(/* f, cmd */);
prtree(/* f, p */);
tfree(/* p */);
Boolean tr_equal(/* t1, t2 */);
#endif
