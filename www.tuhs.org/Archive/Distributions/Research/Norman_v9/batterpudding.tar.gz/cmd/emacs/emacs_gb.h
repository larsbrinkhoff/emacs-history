
/* EMACS_MODES: c !fill */


#ifdef uts
#define u370
#endif
#ifdef u370
#define PORTEXT
#endif
#ifdef OWNER
#define EXTERN
#else
#define EXTERN extern
#endif
#ifdef PC
#define pdp11 1
EXTERN int errno;
#else
#define CRYPTO				/* Encryption ON */
#endif
#ifdef ux3
#define NDLAY 1
#endif
#ifdef bsd			/* CRC */
#define NDLAY 1			/* CRC */
#endif				/* CRC */

EXTERN int curln;               /* current line */
EXTERN int column;              /* collumn */
EXTERN char *clptr;             /* ptrs[curln,column] */
EXTERN int nlines;              /* number of lines in buffer */
EXTERN int kline;               /* line to kill to */
EXTERN char *klptr;             /* ptrs[kline][kcol] */
EXTERN int kcol;                /* collumn ... */
EXTERN int mklbuf;              /* temporary for makeline macro */
EXTERN int SREGION;             /* Size screen region affected by command */
EXTERN int brkflg;		/* flag denoting a break received */

EXTERN int PICMODE;			/* Picture mode */
EXTERN int NODEL;			/* No deletion (blank overwrite */


/* macro arguments */

#define NMVAR 10                        /* number of local variables */
EXTERN int *marg;                               /* macro argument pointer */

/* buffer stuff */

#ifdef pdp11
#define BLEN 512                        /* length of each block */
#define BSHIFT 9							/* log2 of block length */
#define NBLOCK 17                       /* number of buffer blocks */
#else
#define BLEN 1024                        /* length of each block */
#ifdef v8
#undef BSHIFT
#endif
#define BSHIFT 10							/* log2 of block length */
#ifdef bsd
#define NBLOCK 16			/* Large, since it's only virtual memory */

#else
#define NBLOCK 10                       /* number of buffer blocks */
#endif
#endif
EXTERN char bbuf[NBLOCK] [BLEN];        /* storage for buffer */

#define MAXEL 512			/* Max line length */
#define MAXELSH 9			/* log2(MAXEL) */
#define LSMALL 16                       /* smallest line size */
#define LSSHIFT 4			/* log2(lsmall) */

/* Macro Hooks table */

/* These definitions must match those in ecomp.c */

#define Pre_Read_Hook 1
#define Post_Read_Hook 2
#define Pre_Write_Hook 3
#define Load_Macro_Hook 4
#define Read_Name_Hook 5
#define Mode_Line_Hook 6
#define Exit_Emacs_Hook 7
#define Leave_Buffer_Hook 8
#define Enter_Buffer_Hook 9

#define NHOOKS 10
EXTERN int hooks[NHOOKS];

#ifdef pdp11
#define FNLEN 128
#else
#define FNLEN 256
#endif
EXTERN char fnbuf[FNLEN];

/* macro definitions */

#define EOL '\n'
#define INMEM(xnumb) (ptrs[xnumb] &01)
#define OUTMEM(xnumb) ((ptrs[xnumb]&01) == 0)
#define mkline(lineno) (((lineno<nlines)&&((mklbuf=ptrs[lineno])&01)) ? &(bbuf[0][mklbuf]) : mkl(lineno))

EXTERN char *UP;
EXTERN char *DOWN;
EXTERN char *BACK;
EXTERN char *FORWARD;
EXTERN char *HOME;
EXTERN char *CLEAR;
EXTERN char *CLREST;
EXTERN char *CLINE;
EXTERN char *BELL;
EXTERN char *CURAD;
EXTERN char *TMAP;
EXTERN char *SMAP;
EXTERN char *NOP;
EXTERN char *LOPEN;
EXTERN char *LDEL;
EXTERN char *INSERTC;
EXTERN char *INSERTM;
EXTERN char *OSERTC;
EXTERN char *INSERTP;
EXTERN char *DELC;
EXTERN char *SSCROLL;
EXTERN char *RSCROLL;
EXTERN char *CR;
EXTERN char *SCREG;
EXTERN char *ULINE;
EXTERN char *UEND;
EXTERN int EOVER;
EXTERN char *SCINIT;
EXTERN char *VEXIT;
EXTERN char *RELUP;
EXTERN char *RELDOWN;
EXTERN char *RELFORW;
EXTERN char *RELBACK;
EXTERN char *CLDEL;
EXTERN char *CLSCROLL;
EXTERN char *CRSCROLL;
EXTERN char *CLOPEN;
EXTERN int CMPON;
EXTERN int XBASE;
EXTERN int YBASE;
EXTERN int SCRWID;
EXTERN int SCRNLIN;
EXTERN int SCRWRAP;
EXTERN int VCOST;
EXTERN int SRCADD;
EXTERN int MI;
EXTERN int IN;
EXTERN int DELMODE;
EXTERN int TERMIQ;

/* character type table */

#define PLAIN 0
#define CONTRL 1
#define TAB 2
#define BACKSP 3
#define UL 4

/* Emacs data directory */

EXTERN char em_dir[64];		/* emacs data directory (expanded) */

/* global buffer definitions */


#ifdef PC
#define MPTRS 4096
EXTERN unsigned ptrs[MPTRS];
#else
extern unsigned end[];                  /* get types right! */
#define ptrs end                        /* all references to ptrs go to end */
/*VARARGS*/ char *execl();
#endif PC

EXTERN int NPTRS;                               /* number of pointers */

/* character definitions */

#define NCHARS 384              /* number of editor characters */
#define ISIZE 256			/* Maximum number of builtins */

#define META 0200
#define CTLX 0400

#define CTRLA 01
#define CTRLB 02
#define CTRLC 03
#define CTRLD 04
#define CTRLE 05
#define CTRLF 06
#define CTRLG 07
#define CTRLH 010
#define CTRLI 011
#define CTRLJ 012
#define CTRLK 013
#define CTRLL 014
#define CTRLM 015
#define CTRLN 016
#define CTRLO 017
#define CTRLP 020
#define CTRLQ 021
#define CTRLR 022
#define CTRLS 023
#define CTRLT 024
#define CTRLU 025
#define CTRLV 026
#define CTRLW 027
#define CTRLX 030
#define CTRLY 031
#define CTRLZ 032
#define CTRLBRAK 035
#define CTRLBACK 034
#define ESC 033
#define NEWLINE 037
#define RUBOUT 0177
#define MTA(mtach) ('mtach'+0200)       /* make meta char */
#define CTRL(ch) (ch&037)



/* Keyboard key bindings */


EXTERN unsigned short map_it[NCHARS];

/* The following definitions define the mappings for the characters
 * in "doit" and map_it */

#define CBEEP 0				/* no binding */
#define CMETA 1				/* metizing characters */
#define CNUMB 2				/* numbers */
#define CCTLX 3				/* control -x  */
#define CEXIT 4				/* control - z */
#define CCTLU 5				/* control - u */
#define CMARG 6				/* macro argument or variable */
#define CLRES 7				/* Last Result */
#define CMNUS 8				/* minus */
#define NIFUNC 9			/* # of ifunc chars */

#define CINSERTC 9			/* self-inserts */
#define CBEGIN 10			/* start of line */
#define CBACK 11			/* back one character */
#define CFDEL 13			/* ^D */
#define CENDL 14			/* ^E */
#define CFORW 15			/* ^F */
#define CEQUIT 16			/* ^G */
#define CNEWLINE 19			/* return */
#define CEKILL 20			/* ^K */
#define CREFRESH 21			/* ^L */
#define CQUOTE 25			/* ^Q */
#define CRSRCH 26			/* ^R */
#define CFSRCH 27			/* ^S */
#define CXPOSE 28			/* ^T */
#define CYANK 31			/* ^Y */
#define CBDEL 50			/* delete back */
/* screen display data */

EXTERN int mline;
EXTERN int mcol;
EXTERN int SCRLINES;                            /*  number of lines in window */
EXTERN int ECHOL;                               /* line for prompting */
EXTERN int MODLN;                               /* line for buffer and file data */
EXTERN int RARE;                                /* raw input (overrides all others */


/* statistics */

EXTERN long nmput;                              /* calls to mputc */
EXTERN long noutc;                              /* actual characters output */
EXTERN int ninch;                               /* number of characters input */
EXTERN int ntwrite;                             /* number of terminal writes */
EXTERN int nbwrite;                             /* number of buffer writes */
EXTERN int nbseek;                              /* number of seeks of buffer */
EXTERN int nbread;                              /* number of buffer reads */
EXTERN int nmkline;                             /* number of makeline calls */
EXTERN unsigned myuid;                          /* my user ID */
EXTERN unsigned mypid;			/* my process id */
EXTERN unsigned mymask;			/* my umask */
EXTERN int splfile;			/* File number for command splicing */

/* mail and time stuff */

EXTERN int newmail;
EXTERN int mailcnt;
EXTERN int disptime;


/* error function severity */

#define WARN 0                          /* warning only */
#define NORM 1                          /* normal (possibly recoverable) */
#define FATAL 2                         /* fatal error */
extern int errno;

/* function definitions */

EXTERN char *mkl();
EXTERN char *ckline();
EXTERN char *fname();
EXTERN char *bname();
EXTERN char *getname();


/* lint definitions */

#ifdef lint
#define IGNORE(x) if(x);
#else
#define IGNORE(x) (x)
#endif
