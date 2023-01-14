#include <stdio.h>
#ifdef ux3
#include <termio.h>
#else
#include <sgtty.h>
#endif
/* EMACS_MODES: c !fill */
char *getenv();
/* macro definitions */

#define EOL '\n'

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
char *TMAP;
char *SMAP;
char *NOP;
char *LOPEN;
char *LDEL;
char *INSERTC;
char *INSERTM;
char *OSERTC;
char *INSERTP;
char *DELC;
char *SSCROLL;
char *RSCROLL;
char *CR;
char *SCREG;
char *ULINE;
char *UEND;
int EOVER;
char *SCINIT;
char *VEXIT;
char *RELDOWN;
char *RELUP;
char *RELFORW;
char *RELBACK;
int XBASE;
int YBASE;
int SCRWID;
int SCRNLIN;
int SCRWRAP;
int VCOST;
int SRCADD;
int MI;
int IN;
int DELMODE;

/* character definitions */

#define META 0200
#define MTA(mtach) ('mtach'+0200)	/* make meta char */
#define ESC 033
#define NEWLINE 037
#define RUB 0177

/* DISPLAY MODE PARAMETERS */

int WRAPON = 0;				/* perform ! processing at EOL */
int INSON = 0;				/* use INSERTC/DELC */

/* screen display data */

char ttobuf[BUFSIZ];

int drain;
int mline;
int mcol;
int SCRLINES;				/*  number of lines in window */
int ECHOL;				/* line for prompting */
int MODLN;				/* line for buffer and file data */



/* statistics */

long nmput;				/* calls to mputc */
long noutc;				/* actual characters output */
int ninch;				/* number of characters input */
int ntwrite;				/* number of terminal writes */
int nbwrite;				/* number of buffer writes */
int nbseek;				/* number of seeks of buffer */
int nbread;				/* number of buffer reads */
int nmkline;				/* number of makeline calls */

extern int errno;

/* function definitions */

char *getname();
/*VARARGS*/ char *execl();

/* lint definitions */

#ifdef lint
#define IGNORE(x) if(x);
#else
#define IGNORE(x) (x)
#endif

/* emacs display definitions */


/* EMACS_MODES: c !fill */

char ldchar;				/* last clobbered character */
char ldcol;				/* collumn of ldchar */
char osert = 0;				/* flag indicating insert char mode */
int acost;				/* cost of absolute positioning */
int lUP;				/* cost of UP */
int lDOWN;				/* cost of DOWN */
int lBAK;				/* cost of BACK */
int lCR;

int psx;
int psy;
int saveline;
int savecol;
int scrlin;
int scrcol;
int ttywarp;				/* tty warp factor (stty speed) */

/* display data */

#define TTYLEN 256			/* total area for tty data strings */
char ttystrings[TTYLEN];


struct sparm {
	char *t_pname;
	int *t_padd;
};

struct sparm ttydata[] = {
	"up",(int *) &UP,
	"do",(int *) &DOWN,
	"bc",(int *) &BACK,
	"nd",(int *) &FORWARD,
	"ho",(int *) &HOME,
	"cl",(int *) &CLEAR,
	"cd",(int *) &CLREST,
	"ce",(int *) &CLINE,
	"bl",(int *) &BELL,
	"cm",(int *) &CURAD,
	"tm",(int *) &TMAP,
	"tM",(int *) &SMAP,
	"pc",(int *) &NOP,
	"al",(int *) &LOPEN,
	"dl",(int *) &LDEL,
	"ic",(int *) &INSERTC,
	"im",(int *) &INSERTM,
	"ei",(int *) &OSERTC,
	"ip",(int *) &INSERTP,
	"dc",(int *) &DELC,
	"sf",(int *) &SSCROLL,
	"sr",(int *) &RSCROLL,
	"cr",(int *) &CR,
	"cs",(int *) &SCREG,
	"ul",(int *) &ULINE,
	"ue",(int *) &UEND,
	"eo",&EOVER,
	"vs",(int *) &SCINIT,
	"ve",(int *) &VEXIT,
	"bx",&XBASE,
	"by",&YBASE,
	"co",&SCRWID,
	"li",&SCRNLIN,
	"am",&SCRWRAP,
	"vc",(int *) &VCOST,
	"rc",(int *) &SRCADD,
	"mi",(int *) &MI,
	"in",(int *) &IN,
	"dm",&DELMODE,
	"ru",(int *) &RELUP,
	"rd",(int *) &RELDOWN,
	"rl",(int *) &RELFORW,
	"rr",(int *) &RELBACK,
	0,0,
};

char *endput = "____________________";



#define SCRCONT '!'
int TABSTOP = 8;

#define NSCRLIN 48			/* max screen lines */
#define NSCRCOL 128			/* max screen columns */
char cmap[NSCRLIN] [NSCRCOL];
int scrjnk[NSCRLIN];	/* column of last non-white character */



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


/* display heuristics */

#define CFILL 30			/* average chars/line */
#define PATIENCE 2000			/* number of millisecends of */
					/* output to buffer before */
					/* looking for type ahead */


/* display macros */

#define mputc(chr) (((mcol<scrjnk[mline]) && (chr == cmap[mline][mcol]))? mcol++ : mptc(chr))

/* trace stuff */


char *termdir = SDIR/terminals/%s";


/* Terminal I/O modes, sgttyb for before unix 3.0, termio for later */

int SREGION = 24;

#ifdef ux3
struct termio ttyjunk;
#else
struct sgttyb ttyjunk;
#endif

int ttyerase = '#';
int ttykill = '@';
int ttyintr = '';
int ttyeof = '';

#define NBAUD 16			/* number of baud rates */
char charms[NBAUD]  = {			/* ms per char in various rates */
	100,
	100,				/* 50 */
	100,				/* 75 */
	100,				/* 110 */
	70,				/* 134 */
	66,				/* 150 */
	50,				/* 200 */
	33,				/* 300 */
	16,				/* 600 */
	8,				/* 1200 */
	5,				/* 1800 */
	4,				/* 2400 */
	2,				/* 4800 */
	1,				/* 9600 */
	1,				/* EXTA */
	1,				/* EXTB */
};
	
xgo(x,y)

{
	ldchar = 0;
	mline = x;
	mcol = y;
}


/* beep -- obvious */

beep()

{
	PUTS(BELL);			/* print a bell */
}

/* move both the display matrix pointer and the actual display to the
 * specified line and column */

mgo(x,y)

register x,y;

{

	sgo(mline = x,mcol = y);
}


/* move the display cursor to the specified destination.  sgo attempts
 * to optimize the movement, using single character or absolute
 * positioning */

sgo(x,y)

register x,y;

{
	int mx,my;
	int xcost;						/* cost with all relative movement */
	int ycost;						/* cost with carriage return */
									/* acost is cost of absolute positioning */
	
/* calculate relative costs of various movements.  cost functions  */
/* automatically indicate that un-doable motions have infinite cost */
	

	
	
	/* calculate xcost and ycost */

	mx = x-scrlin;
	if (mx<0) {
		mx = -mx;
		xcost = mx*lUP;
	} else xcost = mx*lDOWN;

	ycost = y + xcost + lCR;
	
	my = y-scrcol;
	if (my<0) {
		my = -my;
		xcost +=my*lBAK;
	} else xcost += my;


	if (acost < ycost) {
		if (acost < xcost) {
									/* do absolute positioning */
			if (CURAD) {	/* have absolute addrs */
				if (SRCADD) {
					eprintf(CURAD,x+XBASE,y+YBASE);
				} else { 
					eprintf(CURAD,y+YBASE,x+XBASE);
				}
			} else {	/* have relative addrs */
				if (x>scrlin) printf (RELDOWN,mx);
				if (x<scrlin) printf (RELUP,mx);
				if (y>scrcol) printf (RELFORW,my);
				if (y<scrcol) printf (RELBACK,my);
			}
			scrlin=x;
			scrcol=y;
			return;
		}		/* else relative is cheap, do it */
	} else {
		if (osert && (MI == 0)) {
			unsert();
		}
		if (ycost < xcost) {	/* do carriage return processing */
			PUTS(CR);		/* carriage return */
			scrcol = 0;
						/* fall through to finish with relative motion */
		}
	}

	while (x != scrlin) {
		if (x < scrlin) {
			PUTS(UP);
			scrlin--;
		} else {
			PUTS(DOWN);
			scrlin++;
		}
	}

	/* now correct row */

	while (y != scrcol) {
		if (y < scrcol) {
			PUTS(BACK);
			scrcol--;
		} else {
			if (FORWARD == NULL)  {
				if (osert) {
					unsert();
				}
				x = cmap[scrlin] [scrcol];
				if ((x == 0)|| (scrjnk[scrlin]<=scrcol)) x=cmap[scrlin] [scrcol] = ' ';
				putit(x);	 /* re-write */
			} else {
				PUTS(FORWARD);
			}
			scrcol++;
		}
	};
	return;
}

/* unsert -- LEAVE insert character mode */

unsert()
{
	eprintf(OSERTC); /* can't stay inserting */
	osert = 0;
}

/* a simple guide to all of the various putc routines in this program: 
 * xputc puts a character out, translating control and meta characters
 * to prefix sequences, and calling sputc to put out the individual
 * characters.  sputc checks for end of line, and if so wraps to the
 * next line.  sputc calls mputc to output characters.  mputc updates
 * the next character in the display to be whaat is put out.  Display
 * takes place only if the character on the screen is not that called for
 * already. */


mptc(c)
register c;
{
	nmput++;			/* count for stats */
	if ((c == ' ') && (mcol >= scrjnk[mline])) {

		cmap[mline] [mcol++] = c;
		return;
	}
	if ((mcol != scrcol) || (mline != scrlin)) sgo(mline,mcol);
	if (DELC && INSON && (c == cmap [mline] [mcol+1]) && (mcol+3 <scrjnk[mline]) && (ldchar == 0)) {
		register i;
		SREGION=scrjnk[mline]-mcol; /* number of char's gobbled */
		if (DELMODE && (osert == 0)) {
			eprintf(INSERTM);
			osert++;
		}
		eprintf(DELC); 	/*clobber next char */
		for (i = mcol; i <=scrjnk[mline];i++) {
			cmap[mline] [i] = cmap[mline] [i+1];
		}
		scrjnk[mline]--;
	} else {
		if ((INSERTC || INSERTM) && INSON && (c == ldchar) && (mcol == ldcol+1) && (mcol<scrjnk[mline])) {
			register i;
			if (INSERTM && (osert == 0)) {
				eprintf(INSERTM); 	 /* open space */
				osert = 1;
			}
			if (scrjnk[mline] >SCRWID) scrjnk[mline]--;
			for (i = scrjnk[mline]++; i >= mcol;i--) {
				cmap[mline] [i+1] = cmap[mline] [i];
			}
			if (INSERTC) eprintf(INSERTC);
			cmap[mline] [mcol] = 0;
		} else {
			if (osert) {
				unsert();
			}
		}
		putit(c);
		if (osert && INSERTP) eprintf(INSERTP);
		if (ldchar == 0) ldcol = mcol;		/* remember where it was */
		ldchar = cmap[mline][mcol]; /* save last clobber */
		if ((scrcol++ >= SCRWID) && SCRWRAP) {
			scrlin++;
			scrcol=0;
		}
	}
	cmap[mline] [mcol] = c;
	while (scrjnk[mline] <mcol) cmap[mline] [scrjnk[mline]++] = ' ';
	if (++mcol> scrjnk[mline]) scrjnk[mline] = mcol;
}

/* getname prompts for a string, using ps, and inputs a string */

/* rubout and @ can be used to edit the string as enterred, and  causes
 * a quit, returning no input string
 * ^Y causes the current buffer name to be brought out
 */
#define FNLEN 128
char fnbuf[FNLEN];


char *
getname(ps)

register char *ps;

{
	register i;
	register char c;
	char *xp;

	fnbuf[i=0] = 0;	
	for (;;) {
		prompt1("%s%s",ps,fnbuf); /* display prompt */
		mgo(mline,mcol);
		c = mgetchar();
	
		if ((c == '') || (c == ttyintr)) {
			beep();
			unprompt();
			return(NULL);
		}
		if ((c == '') || (c == '\n')) {
			unprompt();
			return(fnbuf);
		}
		if (c == ttyerase) {
			if (i) {
				i--;
				fnbuf[i] = 0;
			} else beep();
			continue;
		}
		if (c  == ttykill) {
			fnbuf[i=0]=0;
			continue;
		}
		if (c == '') {
			c = 0177 & getchar();
		}
		fnbuf[i++] = c;
		if (i >= FNLEN) {
			beep();
			--i;
		}
		fnbuf[i] = 0;
		continue;
	}
}

/* putout outputs a string (like eprintf) at the current position */

/* position is advanced one line.  If the position overflows the screen,
 * -MORE- is printed, and input  is read.  Any character except ^G
 * continues the display, ^G quits by returning -1 */

/*VARARGS1*/

putout(string,arg1,arg2,arg3,arg4,arg5,arg6)
char *string;
{
	if (mline>=SCRLINES) {
		prompt1("--  MORE --");
		if ((mgetchar()) == ttykill) return(-1);
		unprompt();
		mline=0;		/* TOP */
	}
	prompt(mline,string,arg1,arg2,arg3,arg4,arg5,arg6);
	clrl();
	mgo(++mline,0);
	return(0);
}

/* put out a string on ECHOL */
/*VARARGS1*/

prompt1(string,arg1,arg2,arg3,arg4,arg5)
char *string;
{
	prompt(ECHOL,string,arg1,arg2,arg3,arg4,arg5);
}

/* put out a string at a specified line */

/*VARARGS2*/

prompt(ecl,string,arg1,arg2,arg3,arg4,arg5,arg6)

char *string;
register int ecl;

{
	char pbuf[256];

	if (mline<=SCRLINES) {
		saveline = mline;
		savecol = mcol;
	}
	mline = ecl;
	mcol = 0;
	seprintf(pbuf,string,arg1,arg2,arg3,arg4,arg5,arg6);
	sputs(pbuf);
	psx = mline;
	psy = mcol;
	clrl();
}

/* clear out prompt */

unprompt()

{

	if (mline<=SCRLINES) {		/* if position to save */
		saveline = mline;
		savecol = mcol;
	}
	mgo(ECHOL,0);
	clrl();
	mgo(saveline,savecol);
}


/* return to the position saved before the last prompt */

goback()

{
	mgo(saveline,savecol);
}

/* put one character on the screen checking for end of screen line */

sputc(c)

register c;
{

	register i;

	if (mcol == SCRWID) {
		if (WRAPON) {
			if (mline>=NSCRLIN-1) {
				mline--; /* don't run overboard */
			} else {
				mputc(SCRCONT);
				mline++;
			}
			ldchar = 0;		/* reset ldchar */
			mcol = 0;
		} else {
			mputc(c);
			mline++;
			mcol=0;
			ldchar=0;
			return;
		}
	}
	mputc(c);
}

/* put one character in the display, checking for control and meta chars. */

xputc(c)
register c;
{
	register i;

	c &= 0377;
	if (c & META) {
		sputc('M');
		sputc('-');
		c-= META;
	}
	switch(ctype[c]) {
		char oc;
		case PLAIN:
			if ((!ULINE)||(!EOVER)) {
				sputc(c);
				return;
			}
			if (scrjnk[mline] <= mcol) {
				sputc(c);
				return;
			}
			oc = cmap[mline][mcol] & 0177 ;
			if ((oc == '_') && ((c & 0177) != ' ') 
			&& ((c & 0177) != '_')) {
				sputc(0200 | c);
				return;
			}
			sputc(c);
			return;
		case BACKSP:
			if ((ULINE)&&(EOVER)) {
				if (mcol != 0) xgo(mline,mcol-1);
				return;
			}
			/* NO BREAK HERE */
		case CONTRL:
			sputc('^');
			sputc(c^0100);
			return;

		case TAB:
			i = TABSTOP-(mcol%TABSTOP);
			while (i--) sputc(' ');
			return;

		}
}



/* clear the rest of the line, checking to see if the line previously
 * displayed corresponds to the one displayed here now */

clrl()

{
	register x;
	register y;
	register z;
	int xline;
	int xcol;

	ldchar = 0;			/* wipe out last char */
	x = mline;
	z = mcol;
	y = scrjnk[mline] - mcol;
	if (y > 0) {
		sgo(mline,mcol);			/* go for real */
		if (CLINE) {
			SREGION=y;	/* Number of characters cleared */
			eprintf(CLINE);
		} else {
			xline = mline;
			xcol = mcol;
			while (y--) mputc (' ');
			mgo(xline,xcol);
		}
		scrjnk[x] = z;
	}
}

/* put a string on the screen, translating control and meta */

sputs(xp)
register char *xp;
{
	register c;
	while (c= *xp++) {
		if (c == NEWLINE) {
			if (mline < NSCRLIN) mline++; /* don't overflow */
			mcol = 0;
			xputc('	');
		} else xputc(c);
	}
}

clear()

{
	register int i;
	
	eprintf(CLEAR);
	for (i = 0; i < NSCRLIN; i++) scrjnk[i] = 0;
	scrlin = scrcol = mline = mcol = 0;
}


/* Refresh the screen.  Clears and then restores what we think is there */

rfrsh()
{
	register xline;
	register xcol;
	
	eprintf(CLEAR);
	scrlin=scrcol=0;
	
	for (xline = 0; xline < SCRNLIN; xline++) {
		for (xcol = 0; xcol < scrjnk[xline]; xcol++) {
			if (cmap[xline][xcol]) {
				sgo(xline,xcol);
				putit(cmap[xline][xcol]);
				if ((scrcol++ >= SCRWID) && SCRWRAP) {
					scrlin++;
					scrcol=0;
				}
			}
		}
	}
	ldchar = 0;
}

sdelay(ms)

register int ms;			/* milliseconds of delay */
{
	register i;
	for (i = 0; i < ms;) {
		PUTS(NOP);		/* idle */
		i+=ttywarp;		/* milliseconds/character */
	}
}

ttype()		/* set terminal type */

{
register char *mp;

	mp = getname("Terminal Type? ");
	if (mp == NULL) return;
	sttype(mp);
}

int ttyptr = 0;

/* terminal description file parser: */

/* terminal description file contains lines with 

	parameter=data
	
 * where data is either a number of a string */


ttyparse(mp)
char *mp;

{
	register FILE *file;
	char xbuf[128];
	char optbuf[128];
	register char *cp;
	register int c;
	struct sparm *parmp;
	
	int parm;
	
	
/* find the terminal file and open it */
	
 	seprintf(xbuf,termdir,mp);	/* terminal file */
	
	file = fopen(xbuf,"r");
	
	if (file == NULL) file = fopen(mp,"r");
	
	if (file == NULL) {
		eprintf ("Can't use terminal type %s\n", mp);
		return(0);
	}
	
	/* first find what option we are setting */

nextparm: cp = optbuf;
	while ((c = getc(file)) != '=') {
		if (c == '\n') goto nextparm; /* comment line */
		if (c == EOF) {
			fclose(file);
			return(1); /* abort during option scan */
		}
		*cp++=c;
	}
	*cp = 0;
	
/* look up parameter in parameter table */
	
	for (parmp = ttydata; parmp->t_pname; parmp++) {
		if ((optbuf[0]==parmp->t_pname[0])&& (optbuf[1]==parmp->t_pname[1])) {
			
			c = getc(file);
			if ((c >= '0') && (c <= '9')) {
				parm = 0;
				while ((c >= '0') && (c <= '9')) {
					parm = 10*parm + (c-'0');
					c = getc(file);
				}
				*(parmp->t_padd) = parm;
				if (c != EOF) goto nextparm;
				fclose(file);
				return(1);
			}
			*(parmp->t_padd) = ((int) &ttystrings[ttyptr]);
			
			while ((c != EOF) && (c != EOL)) {
				if (c == '\\') {
					c = getc(file);
					if (c == 'n') c = '\n';
				}
				
				ttystrings[ttyptr++] = c;
				c = getc(file);
			}
			ttystrings[ttyptr++] = 0;
			if (c != EOF) goto nextparm;
		}
	}
	goto nextparm;
}

PUTS(string)

char *string;

{
	if (!drain) while(*string) putchar(0177 & *string++);
}
sttype(mp)
register char *mp;

{
	register struct sparm *parmp;
	
/* First, initialize the tty data */
	


	ttyptr = 0;
	for (parmp = ttydata; parmp->t_pname; parmp++) {
		*(parmp->t_padd) = 0;
	}
	SCRLINES=20;
	SCRWID=80;			/* so we won't bomb */
	
	if (ttyparse(mp)) {
 
		SCRWID--;

		if (SCRNLIN>NSCRLIN) {
			SCRNLIN=NSCRLIN;
		}
		if (SCRWID>NSCRCOL-1) {
			SCRWID=NSCRCOL-1;
		}
		if (VCOST == 0) VCOST = 1;
		SCRLINES = SCRNLIN-4;
		ECHOL = SCRNLIN-1;
		MODLN = SCRNLIN-3;

		if (CURAD) {
			acost = dcost(CURAD);
		} else {
			acost = dcost(RELUP)+dcost(RELFORW);
		}
		lUP = dcost(UP);
		lDOWN = dcost(DOWN);
		lBAK = dcost(BACK);
		lCR = dcost(CR);
		if (OSERTC && (INSERTM == 0)) {
			INSERTM=INSERTC;		/* old style insert modes */
			INSERTC=0;
		}
		if (SCREG) LOPEN = SCREG; /* make sure we use SCREG */
		if ((NOP == NULL) || (*NOP == 0)) NOP = "\200"; /* null pads get lost */

		if (SCINIT) {
			eprintf(SCINIT); /* initialize screen */
		}
		clear();
	}
}

/* dcost -- calculate display cost of a string */

dcost(sp)
register char *sp;
{
	register int dc;
	
	if (sp == NULL) return(1000); /* infinite cost for missing capability */
	
	dc = 0;
	while (*sp) {
		if (*sp++ != '%') dc++;
	}
	return(dc);
}

/* yes or no question */
/*VARARGS1*/
int
gyn(string,arg1,arg2,arg3)

char *string;
char *arg1;
char *arg2;
char *arg3;
{
	register char c;
	
	while (1) {
		prompt(ECHOL,string,arg1,arg2,arg3);
		sgo(mline,mcol);
		c = mgetchar();
		switch(c) {
			
		case 'y':
		case 'Y':
		case ' ':
			return(1);
		case '':	/* This is necessary to insure exit */
		case 'n':
		case 'N':
		case '':
			return(0);
		case '':
			return(-1);
		default:
			prompt(ECHOL-1,"y for yes, n for no, ^G to quit");
		}
	}
};

/* mtop -- move to top of display for message output */

mtop()
{
	mgo(0,0);			/* for messages */
}
	
uncook()
{

/* UNIX 3 code thanks to J. Langer and M. Plotnick */

#ifdef ux3
	struct termio nttyjunk;

	ioctl(1, TCGETA, &ttyjunk);
	nttyjunk=ttyjunk;

#ifdef u370

	nttyjunk.c_iflag |= (BRKINT|ISTRIP|IGNPAR);
	nttyjunk.c_iflag &= 
		~(IGNBRK|IGNPAR|PARMRK|IXON|INLCR|IGNCR|ICRNL|INPCK);
	/* accept break, no crnl mapping, no ^S^Q */ 
	nttyjunk.c_oflag = 0;		/* no delays, no crlf mapping */
	nttyjunk.c_cflag |= CS8 ;
	nttyjunk.c_lflag &= ~(ISIG|ECHO|ICANON); /* no echo, signals,
					    or erase/kill processing */
#else
	nttyjunk.c_iflag |= (BRKINT|ISTRIP);
	nttyjunk.c_iflag &= ~(IGNBRK|PARMRK|INLCR|ICRNL|IGNCR|IXON|IXOFF);
	/* accept break, no crnl mapping, no ^S^Q */ 
	nttyjunk.c_oflag = 0;		/* no delays, no crlf mapping */
	nttyjunk.c_lflag &= ~(ISIG|ECHO|ICANON); /* no echo, signals,
					    or erase/kill processing */
#endif
	nttyjunk.c_cc[VMIN] =  1;	/* return after every character read */
	nttyjunk.c_cc[VTIME] = 1;
	ttywarp = charms[ttyjunk.c_cflag&CBAUD]; /* milliseconds for character */

	ioctl(1, TCSETAW, &nttyjunk);
	ioctl(1, TCXONC,1);		/* Force tty back on */
	
#else

	struct sgttyb nttyjunk;
	
	gtty (1,&ttyjunk);
	nttyjunk=ttyjunk;
	nttyjunk.sg_flags &= (~ECHO);	/* it was so SIMPLE in the old days */
	nttyjunk.sg_flags |= (RAW);
	ttywarp = charms[ttyjunk.sg_ospeed]; /* milliseconds for char */
	stty(1,&nttyjunk);
#endif

#ifndef RT
	ttyerase =	ttyjunk.c_cc[VERASE];
	ttykill =	ttyjunk.c_cc[VKILL];
	ttyeof =	ttyjunk.c_cc[VEOF];
	ttyintr =	ttyjunk.c_cc[VINTR];
#else
	ttyerase =	ttyjunk.sg_erase;
	ttykill =	ttyjunk.sg_kill;
	ttyeof =	'\004';
	ttyintr =	'';
#endif
	if (VEXIT) eprintf(SCINIT); /* RE-init terminal */
}

/* out of raw mode */

cook()
{
	if (VEXIT) eprintf(VEXIT);
	fflush(stdout);			/* force output */
#ifdef ux3
	ioctl(1,TCSETAW, &ttyjunk);
	ioctl(1, TCXONC,1);		/* Force tty back on */
#else
	stty(1,&ttyjunk);
#endif
}


/*VARARGS2*/

char *
nscan(stptr,ret)

register char *stptr;
register int *ret;
{
	register c;

	*ret = 0;
	while (((c = *stptr)>='0') && (c <= '9')) {
		stptr++;
		*ret = *ret*10+(c-'0');
	}
	return(stptr);
}


char *
strcpy(cp,cp1)

register char *cp;
register char *cp1;
{
	while (*cp++ = *cp1++);
	return(cp-1);
}

seprintf(string,fmt, x1)
register char *string;
register char *fmt;
unsigned x1;
{
	int c;
	int width;
	register unsigned int *adx;
	extern char *strcpy();
	
	adx = &x1;
loop:
	while((c = *fmt++) != '%') {
		*string++ = c;
		if(c == '\0') {
			return;
		}
	}
	width = 0;
	c = *fmt++;
	if ((c >= '0') && (c <= '9')) {
		fmt = nscan(fmt-1,&width);
		c = *fmt++;
	}
	
	switch(c) {
	case 'd':
	case 'D':
	case 'o':
	case 'O':
		{
			register int b;
			long n;
			long n1;
			register int i;
			char dstack[20];

			b = (((c=='o') || (c == 'O'))? 8: 10); /* number base */
			if ((c == 'o') || (c == 'd')) {
				n = (long) (*adx);
				if (n > 32768L) n = n-65536L; /* sign correction */
			} else {
				n = *((long *) adx);
				adx += ((sizeof(n)-sizeof(i))/sizeof(i));
			}
			i = 0;
			if (n < 0) {
				n = -n;
				*string++ = '-';
			}
			
			do {
				n1 = n/b;
				dstack[i++] = (short) (n-(n1*b));
				n = n1;
			} while (n != 0);	/* figure number */
			if ((b == 8) && ((i !=1 ) || (dstack[0] != 0))) dstack[i++]=0;
			while (i<width) dstack[i++] = 0;
			while (i > 0) *string++ =(dstack[--i] + '0');	/* print number */
			}
		break;
	case 'P':
		width *= SREGION;
					/* Fall through */
	case 'p':
		while (width > 0) {
			*string++ = *NOP;
			width -= ttywarp;
		}
		adx--;
		break;
	case 's':
		string = strcpy(string,(char *)*adx);
		break;
	case 'm':
	case 'M':
		{
			char *cp;
			if (c=='m') {
				cp = &TMAP[width * (*adx)];
			} else {
				cp = &SMAP[width * (*adx)];
			}
			for (c = 0; c < width; c++) {
				if (*cp) *string++ = *cp++;
			}
		}
		break;
	case 'c':
		c = *adx;
		if (c) {
			*string++ = c;
		} else {
			*string++ = '^';
			*string++ = '@'; /* punt */
		}
		break;
	case '%':
		*string++ ='%';
		adx--;
		break;
	default:
		break;
	}
	adx++;
	goto loop;
}

mgetchar()
{
	fflush(stdout);			/* force output */
	return(0177 & getchar());
}

/*VARARGS1*/

eprintf(string,a1,a2,a3,a4,a5,a6,a7)

register char *string;

{
	char pbuf[1024];
	seprintf(pbuf,string,a1,a2,a3,a4,a5,a6,a7);
	PUTS(pbuf);
}



xprintf(sp,ap1,ap2,ap3,ap4,ap5,ap6)
register char *sp;
{
	char sbuf[0400];		/* buffer */
	register c;
	int x;
	

	sprintf(sbuf,sp,ap1,ap2,ap3,ap4,ap5,ap6);
	sp = sbuf;
	while (c = *sp++) {
		if (c == '\n') {
			clrl();
			if (++mline >= ECHOL) {
				mline=0;
			}
			mcol=0;
		} else {
			xputc(c);
		}
	}
}

die(arg)
int arg;
{
	mgo(SCRNLIN-1,0);
	clrl();
	cook();
	if (arg) abort(arg);
	else exit(0);
}
ttystart()
{
	int i;
	char *tp;
	
	setbuf(stdout,ttobuf);

	uncook();
	for (i = 0; i < 16; i++) {
		signal(i,die);
	}
	tp = getenv("TERM");
	if (tp == NULL) ttype();
	else sttype(tp);
}

insrtc(c)
int c;
{
	if (INSERTC == NULL) return(0);
	INSON = 1;
	ldchar = c;
	ldcol = mcol-1;
	mputc(c);
	INSON = 0;
	return(1);
}
delc()
{
	register i;

	if (DELC == NULL) return(0);
	SREGION=scrjnk[mline]-mcol; /* number of char's gobbled */
	if (DELMODE && (osert == 0)) {
		eprintf(INSERTM);
		osert++;
	}
	eprintf(DELC); 	/*clobber next char */
	for (i = mcol; i <=scrjnk[mline];i++) {
		cmap[mline] [i] = cmap[mline] [i+1];
	}
	scrjnk[mline]--;
	return(1);
}
	
/* adjust vertical position of line -- open (or close) lines on */
/* the screen  argument is the number of lines to add (or drop). */

vadjust(xline,tline,x)

register x;
int xline;
int tline;
{
	register i;
	register j;
	int oldx;

	if (LOPEN == NULL) return(0);
	
	oldx = xline;
	
	if (x<0) {
		x = -x;
		i = 1;
	} else i = 0;
	SREGION=tline-oldx;		/* effected region */
	if (i) {			/* if deleting lines */
		sgo(xline,0);
		
		if (SCREG) {		/*if vt100 stype scrolling */
			eprintf(SCREG,oldx+XBASE,tline+XBASE); /*define region*/
			scrlin = scrcol = 0;
			sgo(tline,0);
			for (i = 0; i < x; i ++) {
				eprintf(SSCROLL);
			}
			eprintf(SCREG,XBASE,SCRNLIN);
			scrlin = scrcol = 0;
		} else {
			for (i = 0; i < x; i++) {
				eprintf(LDEL);
			}
			sgo(tline-x+1,0);
			for (i = 0; i < x; i++) {
				eprintf(LOPEN);
			}
		}
		sgo(oldx,0);
		vshift (oldx,tline,x);
	} else {
		if (SCREG) {		/* if vt100 style scrolling */

			eprintf(SCREG,oldx+XBASE,tline+XBASE); /*define region */
			scrlin = scrcol = 0; /* vt100 dies */
			sgo(xline,0);
			for (i = 0; i < x; i ++) {
				eprintf(RSCROLL);
			}
			eprintf(SCREG,XBASE,SCRNLIN);
			scrlin = scrcol = 0;
		} else {
			sgo(tline+1-x,0);
			for (i = 0; i < x;i++) eprintf(LDEL);
			sgo(oldx,0);
			for (i = 0; i < x; i++) eprintf(LOPEN);
		}
		mgo(oldx,0);
		
		vshift(oldx,tline,-x);
	}
	return(1);
}


/* sscroll -- try to fix display by scrolling */

sscroll(x)

register int x;

{
	register int i;

	
	if (SSCROLL == NULL) return(0);


	sgo(SCRNLIN-1,0);			/* to bottom */
	SREGION=SCRNLIN;			/* number of lines effected */
	for (i = 0; i < x; i++) {
		eprintf(SSCROLL); /* scroll screen */
	}
	vshift (0,SCRNLIN-1,x);
}

/* vshift -- shift the display image from top to bottom (inclusive) by x */


vshift(top,bottom,x)

int top;
int bottom;
int x;

{
	register i;
	register j;
	char *cp1;
	char *cp2;
	int *jnkptr;
	int start;
	int stop;
	register int off;
	
	if (x > 0) {
		off = 1;
		start = top;
		stop = bottom+1;
	} else {
		off = -1;
		start = bottom;
		stop = top-1;
	}
	for (i = start,jnkptr = scrjnk+i; i != stop-x; i+=off,jnkptr+=off) {
		*jnkptr = *(jnkptr+x);
		cp1 = cmap[i];
		cp2 = cmap[i+x];
		for (j = 0; j < *jnkptr; j++) {
			*cp1++ = *cp2++;
		}
	}
	while (i != stop) {
		if (off > 0) {
			*jnkptr++ = 0;
			i++;
		} else {
			*jnkptr-- = 0;
			i--;
		}
	}
}


/* print an underscored character.  */

pu(c)
register char c;
{
	register oc;
	c &= 0177;
	if ((c == 0)||(c == 040)) {
					/* bare underscore */
			putchar('_');
			return;		/* just put out the underscore */
	}
	if (UEND == 0) {
		eprintf(ULINE,c);
	} else {
		eprintf(ULINE); /* enter "underscore mode" */
		putchar(c);
		eprintf(UEND);
	}
}

putit(c)
char c;
{
	if (drain) return;
	if (c & 0200) {
		pu(c);
		return;
	} 
	putchar(0177 & c);
}
