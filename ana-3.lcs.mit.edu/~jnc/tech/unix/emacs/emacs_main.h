/* declarations for emacs editor */
/* EMACS_MODES: c !fill */


/* interpreter definitions */

/* commands handled by the interpreter are mapped to ICHAR+command type */
/* in the do_it array.  ICHAR must be defined to be bigger than the total*/
/* macro definition space, so as to avoid ambiguity, but small enough to */
/* fit in 15 bits, so as to avoiid sign problems */

#define CMETA (ICHAR+1)			/* metizing characters */
#define CNUMB (ICHAR+3)			/* numbers */
#define CCTLX (ICHAR+5)			/* control -x  */
#define CEXIT (ICHAR+7)			/* control - z */
#define CCTLU (ICHAR+9)			/* control - u */
#define CMARG (ICHAR+11)		/* macro argument or variable */
#define CLRES (ICHAR+13)		/* Last Result */
#define CMNUS (ICHAR+15)			/* minus */
#define DO(TKN) ((int (*)())TKN)	/* black magic */
/* dispatch table */

	extern upl();
	extern downl();
	extern gquit();
	extern qrep();
	extern eabort();
	extern mfwrd();
	extern kfwrd();
	extern mbwrd();
	extern kbwrd();
	extern top();
	extern bot();
	extern insertc();
	extern tabc();
	extern quote();
	extern refresh();
	extern beep();
	extern isrch();
	extern begin();
	extern endl();
	extern openl();
	extern nl();
	extern fdel();
	extern bdel();
	extern fred();
	extern fright();
	extern ekill();
	extern mark();
	extern nxtpage();
	extern lstpage();
	extern mkill();
	extern yank();
	extern reyank();
	extern bux();
	extern ux();
	extern forw();
	extern back();
	extern capnxt();
	extern capwrd();
	extern afsep();
	extern xpose();
	extern mapch();
	extern help();
	extern ttype();
	extern pickup();
	extern absgoto();
	extern cment();
	extern cbrak();
	extern modisp();
	extern ssent();
	extern esent();
	extern stats();
	extern uline();
	extern mailit();
	extern toppage();
	extern rsrch();
	extern rqrep();
	extern numchar();
	extern life();
	extern wallc();
	extern filler();
	extern litchr();
	extern macex();
	extern unmod();
	extern mbegin();
	extern setvar();

/* control -x commands */

extern fright();
extern fred();
extern beep();
extern fsave();
extern exch();
extern gquit();
extern quit();
extern chmode();
extern cwd();
extern edbuf();
extern cpbuf();
extern buflist();
extern rmbuf();
extern twind();
extern onewind();
extern owind();
extern inpsh();
extern bfsend();
extern buflng();
extern macro();
extern bfchr();
extern compar();
extern cond();
extern mcase();
extern iter();
extern recurse();
extern pbfname();
extern pfnname();
extern gparam();
extern kdup();
extern kflush();
extern kexch();
extern ldmac();
extern cmpst();
extern wgrow();
extern didle();
extern kapp();
extern rnbuf();
extern pvname();
extern xqt();
extern suspend();

int (*do_it[NCHARS]) () = {

	mark,	begin,	back,	capnxt,	fdel,	endl,	forw,	beep,
	bdel,	tabc,	nl,	ekill,	refresh,nl,	downl,	openl,
	upl,	quote,	isrch,	isrch,	xpose,	DO(CCTLU),nxtpage,mkill,
	DO(CCTLX),yank,	DO(CEXIT),DO(CMETA),beep,DO(CMARG),DO(CLRES),beep,
	afsep,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,DO(CMNUS),insertc,insertc,
	DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),
	DO(CNUMB),DO(CNUMB),insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,insertc,insertc,insertc,
	insertc,insertc,insertc,insertc,insertc,cbrak,insertc,bdel,

/* META characters */
	

	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	toppage,mailit,	beep,	beep,
	beep,	litchr,	rqrep,	rsrch,	beep,	beep,	beep,	beep,
	xqt,	beep,	beep,	beep,	beep,	setvar,	beep,	beep,
	mark,	ux,	filler,	life,	bux,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	DO(CMNUS),beep,	cment,
	DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),DO(CNUMB),
	DO(CNUMB),DO(CNUMB),mapch,beep,	top,	beep,	bot,	help,
	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	numchar,beep,	beep,	uline,
	beep,	ssent,	mbwrd,	capwrd,	kfwrd,	esent,	mfwrd,	absgoto,
	beep,	beep,	beep,	beep,	beep,	modisp,	beep,	beep,
	pickup,	quote,	qrep,	stats,	ttype,	beep,	lstpage,wallc,
	macex,	reyank,	eabort,	mbegin,	beep,	DO(CEXIT),unmod,kbwrd,

/* control -x characters */


	beep,	beep,	buflist,gquit,	cwd,	recurse,edbuf,	beep,
	beep,	inpsh,	beep,	rmbuf,	ldmac,	chmode,	rnbuf,	owind,
	beep,	bfchr,	fred,	fsave,	bfsend,	didle,	pvname,	fright,
	exch,	beep,	suspend,beep,	beep,	beep,	wgrow,	beep,
	beep,	mcase,	beep,	beep,	beep,	kexch,	cmpst,	beep,
	beep,	beep,	beep,	kapp,	beep,	kflush,	beep,	beep,
	beep,	onewind,twind,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	gparam,	buflng,	kdup,	beep,
	beep,	beep,	pbfname,beep,	beep,	beep,	pfnname,beep,
	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	beep,	iter,	beep,
	beep,	beep,	cpbuf,	beep,	macro,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	chmode,	beep,	beep,
	beep,	beep,	beep,	beep,	beep,	beep,	beep,	beep,
	beep,	beep,	beep,	beep,	cond,	beep,	compar,	beep,
};

#ifdef CMON
int cmcnt[256] = {0};				/* command counters */
int cxcnt[128] = {0};
char *cmpath = SDIR/cmcnt";		/* plain command counters */
char *cxpath = SDIR/cxcnt";		/* control x command counters */
#endif

#ifdef DIRED
char dired_args[16] = {'-','a','l',0,0,0,0,0,0,0,0,0,0,0,0,0};
#endif
	long oclock = 0;
int newmail = 0;
int mailcnt = 0;
extern int CKMAIL;

int numarg;				/* numeric argument flag */
 
char *myname;
char *version = "4.2";

extern char *getenv();
extern int timemd;
extern char *ctime();
extern char *fname();
extern char casem[];
extern eabort();
extern dclean();
extern ddel();
extern dundel();
extern dview();
extern crash();
extern int NSCHAR;
extern int SAVEMD;
extern int SAVECHAR;
extern int VERBOSE;
extern int BUFEND;
extern int TABMD;
extern char *endput;
extern int curbf;
extern int READONLY;
