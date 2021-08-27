/* global definitions for EMACS editor */

/* EMACS_MODES: c !fill */

int curln;		/* current line */
int column;		/* collumn */
char *clptr;		/* ptrs[curln,column] */
int nlines;		/* number of lines in buffer */
int kline;		/* line to kill to */
char *klptr;		/* ptrs[kline][kcol] */
int kcol;		/* collumn ... */
int mklbuf;		/* temporary for makeline macro */
int SREGION;		/* Size screen region affected by command */

/* macro arguments */

#define NMVAR 10			/* number of local variables */
int *marg;				/* macro argument pointer */

/* buffer stuff */

#define BLEN 512			/* length of each block */
#define NBLOCK 16			/* number of buffer blocks */
char bbuf[NBLOCK] [BLEN];		/* storage for buffer */

/* macro definitions */

#define EOL '\n'
#define INMEM(xnumb) (ptrs[xnumb] &01)
#define OUTMEM(xnumb) ((ptrs[xnumb]&01) == 0)
#define mkline(lineno) (((lineno<nlines)&&((mklbuf=ptrs[lineno])&01)) ? &(bbuf[0][mklbuf]) : mkl(lineno))

char *UP;					/* cursor up line */
char *DOWN;
char *BACK;
char *FORWARD;
char *HOME;
char *CLEAR;
char *CLREST;
char *CLINE;
char *BELL;
char *CURAD;
char *NOP;
char *LOPEN;
char *LDEL;
char *INSERTC;
char *OSERTC;
char *DELC;
char *SSCROLL;
char *RSCROLL;
char *CR;
char *SCREG;
char *SCINIT;
int XBASE;
int YBASE;
int SCRWID;
int SCRNLIN;
int SCRWRAP;
int VCOST;
int SRCADD;
int MI;

/* global buffer definitions */


#define LSMALL 16			/* smallest line size */

extern unsigned end[];			/* get types right! */
#define ptrs end			/* all references to ptrs go to end */

int NPTRS;				/* number of pointers */

/* character definitions */

#define NCHARS 384		/* number of editor characters */
#define META 0200
#define CTLX 0400
#define ICHAR 16384			/* interpreter characters */

#define MTA(mtach) ('mtach'+0200)	/* make meta char */
#define CTRL(ch) (ch&037)
#define ESC 033
#define NEWLINE 037
#define RUB 0177


/* screen display data */

int mline;
int mcol;
int SCRLINES;				/*  number of lines in window */
int ECHOL;				/* line for prompting */
int MODLN;				/* line for buffer and file data */
int RARE;				/* raw input (overrides all others */


/* statistics */

long nmput;				/* calls to mputc */
long noutc;				/* actual characters output */
int ninch;				/* number of characters input */
int ntwrite;				/* number of terminal writes */
int nbwrite;				/* number of buffer writes */
int nbseek;				/* number of seeks of buffer */
int nbread;				/* number of buffer reads */
int nmkline;				/* number of makeline calls */


/* error function severity */

#define WARN 0				/* warning only */
#define NORM 1				/* normal (possibly recoverable) */
#define FATAL 2				/* fatal error */
extern int errno;

/* function definitions */

char *mkl();
char *ckline();
char *fname();
char *bname();
char *getname();
/*VARARGS*/ char *execl();

/* lint definitions */

#ifdef lint
#define IGNORE(x) if(x);
#else
#define IGNORE(x) (x)
#endif
