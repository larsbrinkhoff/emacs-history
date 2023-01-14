/* emacs display definitions */


/* EMACS_MODES: c !fill */

int wbase = 0;			/* line number of base of window */
int w1base = 0;			/* base of window 1 */

char osert = 0;				/* flag indicating insert char mode */
char umode = 0;				/* flag indicating overstrike mode */
int junked = 0;				/* display junky */
int keepg = 0;				/* amount of stuff to keep between scrolls */

int acost;				/* cost of absolute positioning */
int lUP;				/* cost of UP */
int lDOWN;				/* cost of DOWN */
int lBAK;				/* cost of BACK */
int lFOR;				/* cost of FORWARD */
int lCR;
int dccost;							/* Cost of 1 delete character */
int iccost;

/* two window mode stuff */

int wscrlines;			/* window scrlines */
int cwind = 0;			/* current window */
int windb[2];			/* buffer for windows */
int disbuf[2] = {-1,-1};		/* buffer number in display */
int wmaxln[2];				/* maxln in each buffer */
int wminln[2];				/* minln in each buffer */
int twowind = 0;			/* flag for two window mode */
int woff = 0;				/* extra lines in winndow 2 */
int hcol = 0;				/* Horizontal offset (picture mode */
int hrem;

int lgripe;				/* Flag indicating when it's time to complain about line size */
int minln;			/* first line of display*/
int maxln;			/* last line of display */
int lastln;			/* screen line of maxln */
int nln;			/* new line screen position */
int ncol;			/* ditto for col. */
int dsize;			/* size of area above the cursor */
int psx;
int psy;
int scrlin;
int scrcol;
int scrow;
int ttywarp;				/* tty warp factor (stty speed) */
int BLANK;				/* space or zero */

/* character type table (here because it can't be initted everywhere */

char ctype[128] = {
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	TAB,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	UL,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	CONTRL,
};

int metal = 2;				/* Extra Length of meta characters */

/* display data */

#define TTYLEN 512		/* total area for tty data strings */
char ttystrings[TTYLEN];
int ttyptr;				/* allocation offset in ttyp */


#ifdef PORTEXT

/* The following should be used if the C compiler does not lay out
 * external declarations in exactly the order declared.  This array
 * fixes this by assembling all of the addresses in the right order.
 * It may get some warnings in some implementations.  These
 * variables must be in exactly the same order as the capability
 * strings in ttydata.  This declaration may produce warnings from
 * some compilers, which can be ignored */

int *parmptr[] = {
	(int *) &UP,
	(int *) &DOWN,
	(int *) &BACK,
	(int *) &FORWARD,
	(int *) &HOME,
	(int *) &CLEAR,
	(int *) &CLREST,
	(int *) &CLINE,
	(int *) &BELL,
	(int *) &CURAD,
	(int *) &TMAP,
	(int *) &SMAP,
	(int *) &NOP,
	(int *) &LOPEN,
	(int *) &LDEL,
	(int *) &INSERTC,
	(int *) &INSERTM,
	(int *) &OSERTC,
	(int *) &INSERTP,
	(int *) &DELC,
	(int *) &SSCROLL,
	(int *) &RSCROLL,
	(int *) &CR,
	(int *) &SCREG,
	(int *) &ULINE,
	(int *) &UEND,
	(int *) &EOVER,
	(int *) &SCINIT,
	(int *) &VEXIT,
	(int *) &RELUP,
	(int *) &RELDOWN,
	(int *) &RELFORW,
	(int *) &RELBACK,
	(int *) &CLDEL,
	(int *) &CLSCROLL,
	(int *) &CRSCROLL,
	(int *) &CLOPEN,
	(int *) &CMPON,
	(int *) &XBASE,
	(int *) &YBASE,
	(int *) &SCRWID,
	(int *) &SCRNLIN,
	(int *) &SCRWRAP,
	(int *) &VCOST,
	(int *) &SRCADD,
	(int *) &MI,
	(int *) &IN,
	(int *) &DELMODE,
	0,
};

#endif

/* The order in this string must match that in emacs_gb.h. and the above list */ 

#ifndef PC
char *ttydata ="updobcndhoclcdceblcmtmtMpcaldlicimeiipdcsfsrcrcsulueeovsverurdrrrlDLSFSRALCMbxbycoliamvcrcmiindm";
#endif
char *endput = "____________________";

int REALBOT;				/* Real bottom of screen */
int REALWID;				/* Real width */

#define SCRCONT '!'
int TABSTOP = 8;

#ifdef pdp11
#define NSCRLIN 25			/* max screen lines */
#else
#define NSCRLIN 72			/* max screen lines */
#endif
#define NSCRCOL 128			/* max screen columns */
#ifndef PC
char cmap[NSCRLIN] [NSCRCOL];
#endif
int scrjnk[NSCRLIN];	/* column of last non-white character */
int scrmap[NSCRLIN+1];	/* line number of line in file on screen */
int fmap[NSCRLIN];	/* File Status Map */
#define LGOOD 1000			/* line is good marker (max length) */

#define MODHACK -1		/* File Line of Mode Line */
#define ECHOHACK -2		/* File Line of Echo Line */

#ifdef pdp11
#define SCRMSK 077777			/* line number filed of scrmap */
#define SCRCNL 0100000			/* continuation bit of scrmap */

/* special paramter to sputl to fix rest of lines */

#define REST 20000

#else
#define SCRMSK 07777777			/* line number filed of scrmap */
#define SCRCNL 010000000		/* continuation bit of scrmap */

/* special paramter to sputl to fix rest of lines */

#define REST 1000000

#endif
/* modes */

int LNOMOD = 1;
int LNOWID = 4;
int NOBEL = 0;
int NOTABS = 0;
int PMODE = 0;
extern int TABMD;

/* display heuristics */

#define CFILL 30			/* average chars/line */
#define PATIENCE 2000			/* number of millisecends of */
					/* output to buffer before */
					/* looking for type ahead */

extern char *myname;
extern char version[];
extern int curbf;
extern int BACKP;
int bit8;

/* Insert/delete parameters */

#ifdef u370

/* 370's have fast cpu's, slow I/O, thus spend more time looking for ins/del */

#define ILOOK 11			/* Insert look-ahead */
#define DLOOK 11			/* ditto for delete char */
#else
#define ILOOK 9				/* Insert look-ahead */
#define DLOOK 9				/* ditto for delete char */
#endif
#define lINSC 5		/* Number of characters that must match for inserting */
#define lDELC 5		/* ditto for delc */
 
/* Default Terminal Description */

char *nulltty = "cl=\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\nbs=\nco=80\nli=24\ndo=\\n\ncr=\nbl=\n";

/* trace stuff */


#define TSIZE 200

/* int trcbuf[TSIZE]; */

/* int tnumb;*/

#define TRACE(event)
/*#define TRACE(event) if (tnumb<TSIZE) trcbuf[tnumb++] = event; else trcbuf[(tnumb=1)-1] = event;*/


#define VADJUST 1

/* external declarations */

extern int OVERW;
extern curbf;
extern int LNOMOD;
extern int fbkno;
extern int hipt[];
extern int lowpt[];
extern int timemd;
extern int numarg;
extern int DOCOMP;
extern char *fname();
extern char *bname();
extern char *mstrcpy();
extern char *strcpy();
extern char *getname();
extern char *expenv();

#ifdef TERMCAP
#define PUTS putpad
#define TPARM(x,y,z) tgoto(x,z,y)
#define WAIT1 "100"
char *dumbterm = "dumb";
#else
#ifdef TERMINFO
#define PUTS putpad
#define TPARM tparm
#define WAIT1 "$<100>"
char *dumbterm = "dumb";
#else
#define PUTS eprintf
#define WAIT1 "%100p"
#define TPARM(x,y,z) x,y,z
#endif
#endif

/* Blit ioctl's here, since they are harmless and the include file isn't always there */

struct jwinsize
{
	char	bytesx, bytesy;	/* Window size in characters */
	short	bitsx, bitsy;	/* Window size in bits */
};

#ifdef bsd
#define	IOCPARM_MASK	0x7f		/* parameters must be < 128 bytes */
#define	IOC_OUT		0x40000000	/* copy out parameters */
#define	_IOR(x,y,t)	(IOC_OUT|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)|y)
#define	JWINSIZE		_IOR(j, 5, struct jwinsize)
#else
#define JTYPE ('j'<<8)
#define	JWINSIZE	(JTYPE|5)
#endif
#ifdef PC

/* definitions required to control PC display */

#define VIDEO 0x10
#define REG(HIGH,LOW) (256*HIGH)+LOW
#define PAGE_0	0
#define PC_NULL	0
#define BELL	7
int NORMATB = 7;

#define POS_CUR	2
#define SCRL_UP 6
#define WR_ACHR 9
#define WR_TTY	14
#endif
