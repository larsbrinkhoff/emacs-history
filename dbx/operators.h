#ifndef operators_h
#define operators_h
typedef struct {
    char numargs;
    char opflags;
    String opstring;
} Opinfo;

typedef enum {
    O_NOP,
    O_NAME, O_SYM, O_LCON, O_FCON, O_SCON,
    O_RVAL, O_INDEX, O_INDIR, O_DOT,
    O_COMMA,

    O_ITOF, O_ADD, O_ADDF, O_SUB, O_SUBF, O_NEG, O_NEGF,
    O_MUL, O_MULF, O_DIVF, O_DIV, O_MOD,

    O_AND, O_OR,

    O_LT, O_LTF, O_LE, O_LEF, O_GT, O_GTF, O_GE, O_GEF,
    O_EQ, O_EQF, O_NE, O_NEF,

    O_ALIAS,		/* rename a command */
    O_ASSIGN,		/* assign a value to a program variable */
    O_CALL,		/* call a procedure in the program */
    O_CATCH,		/* catch a signal before program does */
    O_CHFILE,		/* change (or print) the current source file */
    O_CONT,		/* continue execution */
    O_DEBUG,		/* invoke a dbx internal debugging routine */
    O_DELETE,		/* remove a trace/stop */
    O_DUMP,		/* dump out variables */
    O_EDIT,		/* edit a file (or function) */
    O_FUNC,		/* set the current function */
    O_GRIPE,		/* send mail to debugger support person */
    O_HELP,		/* print a synopsis of debugger commands */
    O_IGNORE,		/* let program catch signal */
    O_LIST,		/* list source lines */
    O_PRINT,		/* print the values of a list of expressions */
    O_PSYM,		/* print symbol information */
    O_RUN,		/* start up program */
    O_SKIP,		/* skip the current line */
    O_SOURCE,		/* read commands from a file */
    O_STATUS,		/* display currently active trace/stop's */
    O_STEP,		/* execute a single line */
    O_STOP,		/* stop on an event */
    O_STOPI,		/* stop on an event at an instruction boundary */
    O_TRACE,		/* trace something on an event */
    O_TRACEI,		/* trace at the instruction level */
    O_WHATIS,		/* print the declaration of a variable */
    O_WHERE,		/* print a stack trace */
    O_WHEREIS,		/* print all the symbols with the given name */
    O_WHICH,		/* print out full qualification of a symbol */
    O_EXAMINE,		/* examine program instructions/data */

    O_ADDEVENT,		/* add an event */
    O_ENDX,		/* end of program reached */
    O_IF,		/* if first arg is true, do commands in second arg */
    O_ONCE,		/* add a "one-time" event, delete when first reached */
    O_PRINTCALL,	/* print out the current procedure and its arguments */
    O_PRINTIFCHANGED,	/* print the value of the argument if it has changed */
    O_PRINTRTN,		/* print out the routine and value that just returned */
    O_PRINTSRCPOS,	/* print out the current source position */
    O_PROCRTN,		/* CALLPROC completed */
    O_QLINE,		/* filename, line number */
    O_STOPIFCHANGED,	/* stop if the value of the argument has changed */
    O_STOPX,		/* stop execution */
    O_TRACEON,		/* begin tracing source line, variable, or all lines */
    O_TRACEOFF,		/* end tracing source line, variable, or all lines */

    O_TYPERENAME,	/* state the type of an expression */

    O_LASTOP
} Operator;

/*
 * Operator flags and predicates.
 */

#define null 0
#define LEAF 01
#define UNARY 02
#define BINARY 04
#define BOOL 010
#define REALOP 020
#define INTOP 040

#define isbitset(a, m)	((a&m) == m)
#define isleaf(o)	isbitset(opinfo[ord(o)].opflags, LEAF)
#define isunary(o)	isbitset(opinfo[ord(o)].opflags, UNARY)
#define isbinary(o)	isbitset(opinfo[ord(o)].opflags, BINARY)
#define isreal(o)	isbitset(opinfo[ord(o)].opflags, REALOP)
#define isint(o)	isbitset(opinfo[ord(o)].opflags, INTOP)
#define isboolean(o)	isbitset(opinfo[ord(o)].opflags, BOOL)

#define degree(o)	(opinfo[ord(o)].opflags&(LEAF|UNARY|BINARY))
#define nargs(o)	(opinfo[ord(o)].numargs)

Opinfo opinfo[] ;
#endif
