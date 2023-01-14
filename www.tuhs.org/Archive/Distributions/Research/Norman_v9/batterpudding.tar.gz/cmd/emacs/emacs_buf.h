/* buffer definitions for emacs */

/* EMACS_MODES: c !fill */

int BUFILE;			/* buffer file number */
int BUFEND;		/* next free line of buffer */
#define bufname bbfname[curbf]
#define filename bfilname[curbf]
#define bufmod bbfmod[curbf]
#define diron dirbuf[curbf]

/* Undo information */

#define NUNDO 64			/* Number of undo segments kept */

long undostack[NUNDO];
int undop;
int unseg;

/* Format of an entry.  This should be done with fields, but it
 * doesn't work on some machines since the fields are bigger than ints */

#define UNDMASK 7			/* Mask for type field */
#define UNDSHIFT 3			/* Shift to extract parameter */

#define UNDEL 1				/* next item is a deletion */
#define UNINS 2				/* next item is an insertion */
#define UNMUL 3				/* Next item is a multiple undo */
#define UNBAD 4				/* Next item is a partial multiple */

/* kill stack stuff */

#define KBSIZE 512
#define NKILLP 16
#ifdef PC
#define KBLIM 8192L
int BINMODE = 0;
#else
#define KBLIM 262144L			/* size of kill buffer */
#endif PC
int kbdfile = 0;

/* File size limit -- rejects huge files */

#ifdef PC
#define MAXFS 140000
#else
#ifdef u370
#define MAXFS 1500000
#else
#define MAXFS 750000
#endif
#endif
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
int EOFNL = 1;				/* If 1, force newline on EOF */

/* Cryptography */

#ifdef CRYPTO
int crypt = 0;
char cryptkey[10];
long bufkey;
#endif

/* block buffer stuff */


#ifdef PC
/* Stupid compiler won't do compile time computations */

#define BFACT 32
#define BMASK 0777
#define BRESID 037
#define BRMASK 0177740
#define BRSHIFT 5
#else
#define BFACT (BLEN/LSMALL)
#ifdef v8
#undef BMASK
#endif
#define BMASK (BLEN-1)
#define BRESID (BFACT-1)
#define BRMASK (~BRESID)
#define BRSHIFT (BSHIFT-LSSHIFT)
#endif

/* sbrk rounding factor */

#ifdef univac
#define	BRKMSK	0377777777000
#else
#ifdef pdp11
#define BRKMSK 0177000
#else
#define BRKMSK ~0777
#endif
#endif

int fbkno;				/* first block buffer */
int macptr;				/* macro storage pointer */

int sblk;
int curblk;
int mostwrit;
int nxtflsh;


char bstat[NBLOCK];
unsigned bblock[NBLOCK];

int hipt[NBLOCK];
int lowpt[NBLOCK];
int xline;

/* multiple buffer stuff */

#define NBUF 12
#ifdef PC
char *BTEMPATH = "c";
#else
#ifndef BTEMPATH
#define BTEMPATH "/tmp"
#endif
#endif PC
int btmpfile[NBUF]= {0,0,0,0,0,0,0,0};		/* temp file frn */
int btmpfree[NBUF]= {0,0,0,0,0,0,0,0};		/* temp file free pointer */
int bcurln[NBUF] = {0,0,0,0,0,0,0,0};		/* current line */
int bcolumn[NBUF];					/* current column */
int bnlines[NBUF];					/* number of lines */
char bbfmod[NBUF];
time_t mtime[NBUF];
#ifdef DIRED
char dirbuf[NBUF];
#endif
char bbfname[NBUF] [FNLEN];		/* buffer names */
char bfilname[NBUF] [FNLEN];		/* buffer file name */

int curbf = 0;					/* current buffer number */

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
extern char version[];
char serial[10] = {' ',' ',' ',' ',' ',' ',' ',' ','0',0};
extern int READONLY;
extern int USILENT;
extern int savelink;			/* Mode fails to ask on links */
extern int etrace;
