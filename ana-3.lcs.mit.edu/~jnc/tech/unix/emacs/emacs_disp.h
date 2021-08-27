/* emacs display definitions */


/* EMACS_MODES: c !fill */

int wbase = 0;			/* line number of base of window */
int before = 10;		/* lines before . */
int after = 10;			/* lines after . */
int w1base = 0;			/* base of window 1 */

char ldchar;				/* last clobbered character */
char ldcol;				/* collumn of ldchar */
char osert = 0;				/* flag indicating insert char mode */
int junked = 0;				/* display junky */
int keepg = 0;				/* amount of stuff to keep between scrolls */

int acost;				/* cost of absolute positioning */
int lUP;				/* cost of UP */
int lDOWN;				/* cost of DOWN */
int lBAK;				/* cost of BACK */
int lCR;

/* two window mode stuff */

int wscrlines;			/* window scrlines */
int cwind = 0;			/* current window */
int windb[2];			/* buffer for windows */
int disbuf[2] = {-1,-1};		/* buffer number in display */
int wmaxln[2];				/* maxln in each buffer */
int wminln[2];				/* minln in each buffer */
int twowind = 0;			/* flag for two window mode */
int woff = 0;				/* extra lines in winndow 2 */

int minln;			/* first line of display*/
int maxln;			/* last line of display */
int lastln;			/* screen line of maxln */
int nln;			/* new line screen position */
int ncol;			/* ditto for col. */
int psx;
int psy;
int saveline;
int savecol;
int scrlin;
int scrcol;
int scrow;
int ttywarp;				/* tty warp factor (stty speed) */

/* display data */

#define TTYLEN 256			/* total area for tty data strings */
char ttystrings[TTYLEN];
int ttyptr;				/* allocation offset in ttyp */

struct sparm {
	char *t_pname;
	int *t_padd;
};

/* NOTE -- the following for some inexplicable reason does not work.  all 
 * of the address entries come out 0!.  For this reason, the tty stuff is
 * done in a kludgy waay 

struct sparm ttydxta[] = {
	"up",&UP,
	"do",&DOWN,
	"bc",&BACK,
	"nd",&FORWARD,
	"ho",&HOME,
	"cl",&CLEAR,
	"cd",&CLREST,
	"ce",&CLINE,
	"bl",&BELL,
	"cm",&CURAD,
	"pc",&NOP,
	"al",&LOPEN,
	"dl",&LDEL,
	"ic",&INSERTC,
	"ei",&OSERTC,
	"dc",&DELC,
	"sf",&SSCROLL,
	"sr",&RSCROLL,
	"cr",&CR,
	"cs",&SCREG,
	"vs",&SCINIT,
	"bx",&XBASE,
	"by",&YBASE,
	"co",&SCRWID,
	"li",&SCRNLIN,
	"am",&SCRWRAP,
	"vc",&VCOST,
	"rc",&SRCADD,
	"mi",&MI,
	0,0,
};
*/
/* The order in this string must match that in emaacs_gb.h.  Furthermore*/
/* all tty parameters must take up the space of an INT */

char *ttydata ="updobcndhoclcdceblcmpcaldliceidcsfsrcrcsvsbxbycoliamvcrcmi";

char *endput = "____________________";



#define SCRCONT '!'
int TABSTOP = 8;

#define NSCRLIN 60			/* max screen lines - increased! */
#define NSCRCOL 128			/* max screen columns */
char cmap[NSCRLIN] [NSCRCOL];
int scrjnk[NSCRLIN];	/* column of last non-white character */
int scrmap[NSCRLIN+1];	/* line number of line in file on screen */
int fmap[NSCRLIN];	/* File Status Map */
#define LGOOD 1000			/* line is good marker (max length) */

#define MODHACK -1		/* File Line of Mode Line */
#define ECHOHACK -2		/* File Line of Echo Line */



/* character type table */

#define PLAIN 0
#define CONTRL 1
#define TAB 2
#define BACKSP 3

char ctype[128] = {
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	BACKSP,	TAB,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	CONTRL,
};

/* special paramter to sputl to fix rest of lines */

#define REST 20000

/* modes */

int LNOMOD = 1;
int LNOWID = 4;
int NOBEL = 0;
int NOTABS = 0;;
extern int TABMD;

/* display heuristics */

#define CFILL 30			/* average chars/line */
#define PATIENCE 2000			/* number of millisecends of */
					/* output to buffer before */
					/* looking for type ahead */

extern char *myname;
extern char *version;
extern int curbf;
extern int BACKP;

/* display macros */

#define mputc(chr) (((mcol<scrjnk[mline]) && (chr == cmap[mline][mcol]))? mcol++ : mptc(chr))

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
extern int timemd;
extern int numarg;
extern char *fname();
extern char *bname();
extern char *termdir;

#ifdef TERMCAP
#define PUTS putpad
#else
#define PUTS eprintf
#endif
