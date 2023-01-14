/* declarations for emacs editor */
/* EMACS_MODES: c !fill */

/* interpreter definitions */

/* There are 3 arrays that take part in the implementation of the
 * emacs command interpreter.  The "runit" array is a list of
 * pointers to C functions that implement emacs commands.  It is
 * ordered arbitrarily, not related to the command sequence.  The
 * "doit" array is an array of NCHARS byte codes.  Each byte code
 * specifies the function bound to the corresponding byte code
 * either as an internal interpreter function (codes < NIFUNC) or as
 * one of the functions in the runit array.  The codes in this
 * array are fixed, and implement the base level command bindings
 * for emacs.  The codes in this array are also used to locate the
 * help messages.  Each code maps to a corresponding help message. */

/* The third array is what allows the user to re-bind key
 * definitions and bind keys to macros.  It is the "map_it" array. 
 * This is initialized to the same as doit.  Each entry of this
 * array can either be a built in command (C function or interpreter
 * command), if less than NCHARS, or a macro pointer, if > NCHARS. */

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
	extern rquote();
	extern mquote();
	extern refresh();
	extern beep();
	extern fisrch();
	extern risrch();
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
	extern lownxt();
	extern capwrd();
	extern afsep();
	extern xpose();
	extern mapch();
	extern help();
	extern ttype();
	extern undo();
	extern pickup();
	extern absgoto();
	extern go_xy();
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
	extern wallc();
	extern filler();
	extern litchr();
	extern macex();
	extern unmod();
	extern mbegin();
	extern setvar();
	extern envget();
	extern stopjob();
#ifdef PC
	extern ctfile();
	extern mapk();
#else
#define ctfile beep
#define mapk beep
#endif

/* control -x commands */

extern fright();
extern fred();
extern fsave();
extern exch();
extern gquit();
extern quit();
extern chmode();
extern cwd();
extern edbuf();
extern cpbuf();
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
extern gkarm();
extern glob();
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
extern getsharg();
extern strtkbd();
extern endkbd();
extern exkbd();
extern setkey();
extern trce();
extern brkproc();
#ifdef COMPRESS
extern loadtbl();
#else
#define loadtbl beep
#endif

#ifdef DIRED
extern dcdel();
extern ddel();
extern dundel();
extern dview();
#define DCDEL dcdel
#define DDEL ddel
#define DUNDEL dundel
#define DVIEW dview
#else
#define DCDEL beep
#define DDEL beep
#define DUNDEL beep
#define DVIEW beep
#endif




/* The run-it table */



int (*runit[]) () = {
/* 9 */	insertc,begin,	back,	capnxt,	fdel,	endl,	forw,	beep,
/*17 */	tabc,	afsep,	nl,	ekill,	refresh,downl,	openl,	upl,
/*25 */	rquote,	risrch,	fisrch,	xpose,	nxtpage,mkill,	yank,	mbwrd,
/*33 */	capwrd,	kfwrd,	mfwrd,	qrep,	lstpage,eabort,	kbwrd,	top,
/*41 */	bot,	gquit,	fred,	fsave,	fright,	exch,	mark,	help,
/*49 */	ux,	bdel,	reyank,	ttype,	bfsend,	absgoto,cment,	modisp,
/*57 */	chmode,	ssent,	esent,	cpbuf,	edbuf,	rmbuf,	onewind,owind,
/*65 */	twind,uline,	bux,	cwd,	inpsh,	mailit,	toppage,rsrch,
/*73 */	rqrep,	mquote,	numchar,pickup,	wallc,	stats,	cbrak,	DDEL,
/*81 */	DCDEL,	DUNDEL,	DVIEW,	filler,	macro,	compar,	cond,	litchr,
/*89 */	iter,	gparam,	mbegin,	pbfname,pfnname,recurse,kdup,	kflush,
/*97 */	kexch,	ldmac,	macex,	cmpst,	wgrow,	didle,	mcase,	unmod,
/*105*/	bfchr,	mapch,	setvar,	kapp,	rnbuf,	buflng,	pvname,	xqt,
/*113*/	getsharg,gkarm,	envget,	strtkbd,endkbd,	exkbd,	go_xy,	lownxt,
/*121*/	trce,	setkey,	glob,	undo,	stopjob,brkproc,ctfile,	mapk,
/*129*/	loadtbl,
};


/* Global interpreter bindings */

unsigned char doit[NCHARS] = {
	
	47,	10,	11,	12,	13,	14,	15,	16,
	50,	17,	19,	20,	21,	19,	22,	23,
	24,	25,	26,	27,	28,	CCTLU,	29,	30,
	CCTLX,	31,	CEXIT,	CMETA,	0,	CMARG,	CLRES,	0,
	18,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	CMNUS,	18,	9,
	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,
	CNUMB,	CNUMB,	9,	9,	9,	9,	9,	9,

	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	9,	9,	9,
	9,	9,	9,	9,	9,	79,	9,	50,
	
/* META characters */
	
	0,	0,	0,	0,	0,	0,	0,	0,
	39,	0,	0,	0,	71,	70,	0,	0,
	0,	88,	73,	72,	0,	0,	0,	0,
	112,	0,	125,	0,	0,	107,	0,	0,
	47,	49,	84,	0,	67,	0,	0,	0,
	0,	0,	0,	0,	0,	CMNUS,	0,	55,
	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,	CNUMB,
	CNUMB,	CNUMB,	106,	0,	40,	0,	41,	48,
	
	0,	0,	0,	0,	0,	115,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	75,	0,	0,	66,
	0,	58,	32,	33,	34,	59,	35,	54,
	0,	0,	0,	0,	120,	56,	0,	0,
#ifdef PC
	76,	74,	36,	78,	127,	124,	37,	77,
#else
	76,	74,	36,	78,	52,	124,	37,	77,
#endif
	99,	51,	38,	91,	0,	CEXIT,	104,	39,
	
/* Control - X characters */
	
	0,	113,	60,	42,	68,	94,	61,	126,
	0,	69,	0,	62,	98,	57,	109,	64,
	0,	105,	43,	44,	53,	102,	111,	45,
	46,	0,	0,	0,	0,	0,	101,	0,
	0,	103,	0,	123,	0,	97,	100,	0,
	116,	117,	0,	108,	0,	96,	0,	0,
	0,	63,	65,	0,	0,	0,	0,	0,
#ifdef PC
	0,	0,	128,	0,	90,	110,	95,	0,
#else
	0,	0,	0,	0,	90,	110,	95,	0,
#endif

	114,	0,	92,	0,	0,	118,	93,	0,
	0,	0,	0,	0,	129,	0,	0,	0,
	0,	0,	0,	0,	121,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	89,	0,
	0,	0,	0,	0,	85,	0,	0,	119,
	0,	0,	0,	122,	0,	57,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,	
	0,	0,	0,	0,	87,	0,	86,	0,
};

/* character help explanations */

/* note -- the text of the help instructions is kept in helpstrings
 * and helpfile in uncompiled and compiled format.  make_help is the
 * compiler for the helpstrings file. */

#define HINSERT 9		/* index of self inserting chars */

extern char hdate[];
int helfile = 0;
#define HELSIZE 128

/* ****** NOTE -- it is no longer necessary to change the pathnames. */
/* EMACS will put its error messages, help messages, and statistics in */
/* the directory where the sources are if you don't modify these definitions */


#ifdef PC
#define SDIR ""
#endif
char *emd_source = SDIR;


#ifdef CMON

int cmcnt[NCHARS] = {0};		/* command counters */
#endif

#ifdef DIRED
char dired_args[16] = {'-','a','l',0,0,0,0,0,0,0,0,0,0,0,0,0};
#endif
	long oclock = 0;

int numarg;				/* numeric argument flag */
int etrace;				/* Trace flag */

char *myname;
extern char version[];

char **sharg;				/* shell argument list */
int nsharg;				/* argument counter */
char *svname;				/* saved myname for long return */
#ifdef PC
int retjmp[3];				/* */
#else
jmp_buf retjmp;				/* return environment for break */
extern char *ctime();
#endif

extern char *getenv();
extern int junked;
extern int timemd;
extern char *fname();
extern char casem[];
extern eabort();
extern crash();
extern char *mstrcpy();
extern char *nscan();
extern int NSCHAR;
extern int SAVEMD;
extern int SAVECHAR;
extern int VERBOSE;
extern int BUFEND;
extern int TABMD;
extern char *endput;
extern int curbf;
extern int READONLY;
#ifdef DIRED
#define diron dirbuf[curbf]
extern char dirbuf[];
extern dclean();
extern ddel();
extern dundel();
extern dview();
#endif
#ifdef CRYPTO
extern long bufkey;
#endif
