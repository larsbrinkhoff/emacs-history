#ifndef symbols_h
#define symbols_h
typedef struct Symbol *Symbol;

#include "machine.h"
#include "names.h"
#include "languages.h"

/*
 * Symbol classes
 */

typedef enum {
    BADUSE, CONST, TYPE, VAR, ARRAY, PTRFILE, RECORD, FIELD,
    PROC, FUNC, FVAR, REF, PTR, FILET, SET, RANGE, 
    LABEL, WITHPTR, SCAL, STR, PROG, IMPROPER, VARNT,
    FPROC, FFUNC, MODULE, TAG, COMMON, TYPEREF
} Symclass;

typedef enum { R_CONST, R_TEMP, R_ARG, R_ADJUST } Rangetype; 

struct Symbol {
    Name name;
    Language language;
    Symclass class : 8;
    Integer level : 8;
    Symbol type;
    Symbol chain;
    union {
	int offset;		/* variable address */
	long iconval;		/* integer constant value */
	double fconval;		/* floating constant value */
	struct {		/* field offset and size (both in bits) */
	    int offset;
	    int length;
	} field;
	struct {		/* common offset and chain; used to relocate */
	    int offset;         /* vars in global BSS */
	    Symbol chain;
	} common;
	struct {		/* range bounds */
            Rangetype lowertype : 16; 
            Rangetype uppertype : 16;  
	    long lower;
	    long upper;
	} rangev;
	struct {
	    int offset : 16;	/* offset for of function value */
	    Boolean src : 8;	/* true if there is source line info */
	    Boolean inline : 8;	/* true if no separate act. rec. */
	    Address beginaddr;	/* address of function code */
	} funcv;
	struct {		/* variant record info */
	    int size;
	    Symbol vtorec;
	    Symbol vtag;
	} varnt;
    } symvalue;
    Symbol block;		/* symbol containing this symbol */
    Symbol next_sym;		/* hash chain */
};

/*
 * Basic types.
 */

Symbol t_boolean;
Symbol t_char;
Symbol t_int;
Symbol t_real;
Symbol t_nil;

Symbol program;
Symbol curfunc;

#define symname(s) ident(s->name)
#define codeloc(f) ((f)->symvalue.funcv.beginaddr)
#define isblock(s) (Boolean) ( \
    s->class == FUNC or s->class == PROC or \
    s->class == MODULE or s->class == PROG \
)

#define nosource(f) (not (f)->symvalue.funcv.src)
#define isinline(f) ((f)->symvalue.funcv.inline)

#include "tree.h"

/*
 * Some macros to make finding a symbol with certain attributes.
 */

#define find(s, withname) \
{ \
    s = lookup(withname); \
    while (s != nil and not (s->name == (withname) and

#define where /* qualification */

#define endfind(s) )) { \
	s = s->next_sym; \
    } \
}

Symbol symbol_alloc(/*  */);
symbol_dump(/* func */);
symbol_free(/*  */);
Symbol newSymbol(/* name, blevel, class, type, chain */);
Symbol insert(/* name */);
Symbol lookup(/* name */);
dumpvars(/* f, frame */);
symbols_init(/*  */);
Symbol maketype(/* name, lower, upper */);
Symbol rtype(/* type */);
Integer level(/* s */);
Symbol container(/* s */);
Address address(/* s, frame */);
defregname(/* n, r */);
findtype(/* s */);
Integer size(/* sym */);
Boolean isparam(/* s */);
Boolean isvarparam(/* s */);
Boolean isvariable(/* s */);
Boolean ismodule(/* s */);
Boolean isbuiltin(/* s */);
Boolean compatible(/* t1, t2 */);
Boolean istypename(/* type, name */);
Boolean isambiguous(/* s */);
assigntypes(/* p */);
Node dot(/* record, fieldname */);
Node subscript(/* a, slist */);
int evalindex(/* s, i */);
chkboolean(/* p */);
unmkstring(/* s */);
Symbol which(/* n */);
Symbol findfield(/* fieldname, record */);
Boolean getbound(/* s,off,type,valp */);
#endif
