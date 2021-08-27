/* buffer definitions for emacs */

/* EMACS_MODES: c !fill */

int BUFILE;			/* buffer file number */
int BUFEND;		/* next free line of buffer */
#define bufname bbfname[curbf]
#define filename bfilname[curbf]
#define bufmod bbfmod[curbf]
#define GC_GRACE 20	/* number of lines to keep during gc */

/* kill stack stuff */

#define KBSIZE 512
#define NKILLP 16
#define KBLIM 262144L			/* size of kill buffer */

long kstk[NKILLP+1];
char kbuf[KBSIZE];
int kbapp = 0;				/* append kill buffer flag */
int nkp = -1;
long kend = 0L;
long kbwrt = 0L;
int kptr = 0;
long kbase = 0L;
int kbmod = 0;
int kfile = 0;
int crashes = 0;

/* block buffer stuff */


#define BFACT 32
#define BMASK 0177740
#define BRESID 037
#define BSHIFT 9
#define LSSHIFT 4			/* shift for lsmall */

#ifdef DIRED
int diron = 1;
#endif

int fbkno;				/* first block buffer */
int macptr;				/* macro storage pointer */

int sblk;
int curblk;
int mostwrit;
int nxtflsh;


char bstat[NBLOCK];
unsigned bblock[NBLOCK];

/* multiple buffer stuff */

#define NBUF 12

int btmpfile[NBUF]= {0,0,0,0,0,0,0,0};		/* temp file frn */
int btmpfree[NBUF]= {0,0,0,0,0,0,0,0};		/* temp file free pointer */
int bcurln[NBUF] = {0,0,0,0,0,0,0,0};		/* current line */
int bcolumn[NBUF];					/* current column */
int bnlines[NBUF];					/* number of lines */
char bbfmod[NBUF];
char bbfname[NBUF] [64];				/* buffer names */
char bfilname[NBUF] [64];				/* buffer file name */

int curbf = 0;					/* current buffer number */

#define MAXEL 512

#define bnext() (blockp<blocke ? *blockp++: bnxt())
#define bput(chr) if(blockp<blocke) *blockp++ = (chr) ; else bpt(chr)

/* trace stuff */


#define TSIZE 200

/*int trcbuf[TSIZE];*/
/*int tnumb;*/

/*#define TRACE(event) if (tnumb<TSIZE) trcbuf[tnumb++] = event; else trcbuf[(tnumb=1)-1] = event;*/
#define TRACE(event)

#define TRSEEK 0
#define TRREAD 1
#define TRWRIT 2
#define TRMAKE 3
#define TRCKEX 4
#define TRCKCP 5
#define TRMKEM 6
#define TRBGRAB 7

extern int VERBOSE;
char *expenv();
extern char *mkl();
extern int NSCHAR;
extern int SAVEMD;
extern char mdchar[];	
extern char *endput;
extern char *getenv();
extern char *myname;
extern char *version;
extern int twowind;			/* two window mode */
extern int windb[];
extern int READONLY;
