/* EMACS_MODES: c !fill */

/* Expand global variables here */

#define OWNER 1

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <setjmp.h>
#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_main.h"

#ifdef ux3
#include <fcntl.h>
#endif

/* screen editor for unix */



/*		Main Loop		*/

/* interprets characters, setting arguments and meta.  When a
 * 'virtual character' is entered, it is executed via the do_it array */

char *cprompt[2] = {"M-","^X"};


/* VARARGS */

edit(edisp,echar,earg)

int edisp;				/* disposition flag */
int echar;				/* optional character for disp = 2*/
int earg;				/* optional argument for disp = 3*/
{
	register int c;			/* virtual character being interpreted */
	
	register int realc;		/* last real character read */
	int metf;
	int dochar;
	register int arg;
	int eresult;			/* result of last command */
	
/* Keywords: user-interface argument-processing:40 macro-programming:30 */
/* Keywords: key-bindings:30 display-update:5 */
	
#ifdef pdp11
#define BUFMAX 077000			/* Buffer file size at which to GC */
#else
#define BUFMAX 0777000			/* Bigger for VAX, since not constrainted by address space */
#endif

	for (;;) {
		arg = 1;
		metf = 0;
		numarg = 0;
		if (BUFEND > BUFMAX) collect(); /* garbage collection */
		if (edisp == 2) {	/* execute command */
			if ((echar < 0) || (echar > 0600)) echar = CTRLG;
			c = echar;
			arg = earg;
			edisp = 0;		/* do one command */
			goto dispose;
		}

re_get:
		if (infrn >= 0) {
			
			if (SAVEMD) {
				if (NSCHAR++>SAVECHAR) {
					IGNORE(fsave(0));
					NSCHAR=0;
				}
			}
			if (etrace == 0) disup();		/* update display */
			if ((VERBOSE) && (MOREIN == 0) && ((numarg) || (metf))){
				if (metf) {
					if (numarg) {
						if (numarg == 8) prompt1("%o %s: ",arg,cprompt[metf>>8]);
						else prompt1("%d %s: ",arg,cprompt[metf>>8]);
					} else {
						prompt1("%s: ",cprompt[metf>>8]);
					}
				} else {
					if (numarg == 8) prompt1 ("%o: ", arg);
					else prompt1("%d: ", arg);
				}
				c = (getchar()) + metf;
				unprompt();
			} else c = (getchar()) + metf;
#ifdef CMON
			if (infrn == 0) cmcnt[c]++;		/* count command statistics */
#endif
			dochar = map_it[c];
		} else {

/* The following code for macros bypasses the normal getchar for speed! */
			
			c = metf + Mgetchar();

dispose:				/* dispose of character command */
			dochar = doit[c];
			if (dochar == 0) { /* no default binding, check for macro */
				dochar = map_it[c];
				if (dochar < ISIZE) dochar = 0; /* Only for macros */
			}
		}


		if (dochar >= ISIZE) {
			eresult = xmac(dochar-ISIZE,arg,c);
		} else if (dochar >= NIFUNC) {
			eresult = (*runit[dochar-NIFUNC]) (arg,c);
		} else {
			metf = c & META;
			realc = c & META-1; /* set up real character and meta flags; */
			switch(dochar) {
					
			case CMETA:
				metf=META;
				goto re_get;
			case CCTLX:
				metf = CTLX;
				goto re_get;
			case CEXIT:
				return(arg);
			case CLRES:
				arg = eresult;
				numarg = 10;
				goto re_get;
			case CCTLU:
				arg *= 4;
				if (numarg == 0) numarg = 1;
				goto re_get;
			case CMARG:
				if ((arg >= 0) && (arg < NMVAR) && (marg != NULL)) {
					arg = marg[arg];
					numarg = 10;
				}
				goto re_get;
			case CMNUS:
				if ((numarg == 1) || (metf && (numarg == 0))) {
					arg = -1;
					metf = 0;
					numarg = -1;
					goto re_get;
				}
				eresult = insertc(arg,c);
				break; /* other minus's insert */
			case CNUMB:
				if (metf || numarg) {
					metf = 0;
					if (numarg>1) {
						arg = arg*numarg+realc-'0';
						if (arg < 0) arg -= 2*(realc-'0');
					} else {
						arg = realc-'0';
						if (numarg < 0) {
							arg = -arg;
						}
						numarg = (realc == '0') ? 8:10;
					}
					goto re_get;
				}
				eresult = insertc(arg,c);
				break;		/* some numbers self insert */
			case CBEEP: beep();
			}

		}
		if (etrace) {
			int tc;
			
			if (c & 0400) {
				putout ("^X%c	%d	%d",c-0400,arg,eresult);
			} else {
				putout ("%c	%d	%d",c,arg,eresult);
			}
			mflush(stdout); /* Force output! */
			read(0,&tc,1); /* pause */
			switch(tc&0177) {
			
			case 'e':
				etrace = 0;
				recurse(1); /* Recursively edit */
				etrace = 1;
				break;
			case '?':
				{
					char hbuf[128];
					
					helpin(c,hbuf);
					putout(hbuf);
					break;
				}

			case CTRLG:
			case CTRLZ:
				return(eresult); /* Bomb out of here */
			}
		}
		if (edisp == 0) return(eresult);
	}
}

trce()
{
/* Keywords: commands macro-programming */

	mtop();
	putout("Command	Argument	Result");
	etrace = 1;
	edit(0);
	etrace = 0;
}

/* issrch - is character a search invocation */

/* returns 1 for ^S, -1 for ^R, and 0 for others */

issrch(chr)
int chr;

{
/* Keywords: searching key-bindings */
	
	if (map_it[chr] ==  27) return(1);
	if (map_it[chr] == 26) return(-1);
	return(0);
}
isquote(chr)
int chr;
{
/* Keywords: key-bindings quoting */
	if (map_it[chr] == 25) return(1);
	return(0);
}

/* xqt -- execute a command through argument 0. */

/* the character command designated by macro argument 0 is run with the  */
/* current argument */

xqt(arg)

int arg;
{
/* Keywords: commands macro-programming:40 */

	int res;
	if (marg == NULL) return(0);
	pushin(NULL);		/* if command needs input, from tty */
	res = edit(2,marg[0],arg);		/* run command */
	inpop();
	return(res);
}
/* ckmail -- check for user mail */

ckmail()
{
/* Keywords: user-interface:50 mail-processing unix-interface */
	
	extern int CKMAIL;
#ifndef PC
	register char *cp;
	struct stat statb;
	static time_t mailtime;


	if (cp = getenv("MAIL")) {
		if ((stat(cp,&statb)>=0) && (statb.st_size>30) ) {
			if (statb.st_mtime != mailtime) {
				newmail = -1;
				mailtime = statb.st_mtime;
			} else {
				newmail = 1;
			}
		} else {
			if (newmail) disptime = 1; /* Force redisplay of mail/time line */
			newmail = 0;
		}
	}
#endif
	mailcnt = CKMAIL;
}


/* dtime -- display the time on the screen */

long clock;

dtime(flag)

int flag;
{
/* Keywords: user-interface:50 time-processing unix-interface */
	
#ifndef PC
register char *tp;

	

	if (timemd) {
		time(&clock); /* get time */
		if ((clock-oclock > 60) || flag) {
			tp = ctime(&clock); /* convert */
			tp[16] = 0;	/* wipe out newline */
			disptime = 1;
		}
	}
#endif PC
}

/* break interrupt handler */

bkfg()
{
/* Keywords: break-handling unix-interface user-interface:10 */
	
	signal(SIGINT,bkfg);
#ifdef bsd
	dclear();			/* Berkeley flushes tty output on break */
#endif	
	brkflg++;			/* just flag what happened */
}



/* init -- initialize editor data */

/* initializes EMACS, goes into raw mode, creates the buffer main, and
 * reads in the file specified by the argument to the emacs command */


init()

{
	register int i;
	
/* Keywords: unix-interface internal-initialization filenames:10 */
	
	procbuf = -1;			/* Initialize global */
	uncook();
/*	for (i = 0; i < 16; i++) signal(i,eabort); /* trap various problems */

/* some signals cause attempt to save */

	signal (SIGTERM,crash);
	signal (SIGHUP,crash);
	signal (SIGINT,bkfg);
	signal (SIGPIPE,SIG_IGN);
	for (i = 0; i < 128; i++) casem[i] = i; /* search map */
	ioinit();
	curln = 1;
	column = 0;
	IGNORE(chgbuf("Main"));

/* The following must take place after I/O is initialized and the 
 *	terminal is in raw mode but before any initializations that
 *	use the emacs home directory */
	
	strcpy(em_dir,expenv(emd_source)); /* expand emacs home directory */
}
brkit()					/* break interrupt handler */
{
	int res;
/* Keywords: unix-interface break-handling user-interface:50 */
	
	brkflg=0;
bkagain:res = error(WARN,74);		/* BREAK!! */
	
	if (res > 0) return;
	
	if (res == 0) {
					/* recursive edit time */
		recurse(1);		/* Hope that this works out OK */
		goto bkagain;
	}
	
	marg=NULL;			/* wipe out macro backtrace */
	myname = svname;		/* save name */
	longjmp(retjmp,1);		/* EXIT!!! */
}

main(argc, argv)

int argc;
char *argv [];

{
	char itbuf[128];
#ifdef DIRED
	extern int LNOMOD;
	extern int FILLMD;
	extern int OVERW;
	extern int NODEL;
#endif
	extern int fbkno, macptr;
	int line;
	register char *ap;
	char *cp;
	int tset;

#ifdef MONITOR

#define MONSIZ 5000
	short monbuf[MONSIZ];
	extern int etext;

	monitor(2,&etext,monbuf,MONSIZ,0); /* profile on */
#endif

/* Keywords: command-line-processing:90 user-interface:10 */
/* Keywords: internal-initialization terminal-initialization:30 */
/* Keywords: dired:20 */

					/* find our name */

#ifdef PC
	myname = "PC EMACS";
#else
	for (ap= myname = argv[0]; *ap; ap++) {
		if (*ap>0140) *ap = (*ap) - 040;
		if (*ap == '/') myname = ap+1; /* Seek to last '/' */
	}
#endif PC	
	svname=myname;			/* save myname */
	
	NPTRS = 0;			/* no memory */

	
/* special initialization for DIRED */

#ifdef DIRED

#define CTX(xchar) ((xchar)+0400)

 	LNOMOD = 0;
 	FILLMD = 0;
	OVERW = 1;
	NODEL = 1;
	
	doit['d'] = doit['D'] = 81;
	doit[CTRLD] = doit[CTRLK] = doit[RUBOUT] =80;
	doit['u'] = doit['U'] = 82;
	doit['e'] = doit['E'] = 83;
	
#endif
	mapch(0);			/* initialize key bindings */
	fbkno = macptr = 0;			/* no macros */
#ifndef PC
	myuid = getuid();		/* get my user ID */
	mypid = getpid();
	mymask = umask(0);
	umask(mymask&077);		/* Make sure I can read/write my own files */
	
#endif PC
#ifdef CRYPTO

/* Set up for encrypting the buffer */

	time (&bufkey);
	tset = myuid & 077777;		/* Make sure it's +! */
	while (tset) {
		bufkey = bufkey * bufkey + tset;
		tset = tset >> 1;
	}
#endif	
	init();				/* initialize */
#ifdef PC
	sttype(NULL);
	tset = 1;
#else
	if ((cp=getenv("TERM"))!= NULL) {
		sttype(cp);		/* set up for terminal */
		tset = 1;
	} else {
		sttype(NULL);		/* default terminal type */
		tset = 0;
	}
#endif
	nsharg = argc-1;
	sharg = argv+1;			/* pointer to first argument */
		
	if (setjmp(retjmp)) goto main_loop;
	
	if (nsharg && streq(*sharg,".i")) {
		**sharg = '-';		/* user's init only */
	} else {
#ifdef PC
		if (infile("c:emacs.ini")<=0) {
			infile("emacs.ini");
		}
#else
#ifdef DIRED
		if (infile("$HOME/.dired_init")<=0) { /* run user's init file */
#else
		if (infile("$HOME/.emacs_init")<=0) { /* run user's init file */
			seprintf(itbuf,"%s/.emacs_init",em_dir);
			infile (itbuf); /* run default init file */
#endif
		}
#endif
	}
	if (nsharg && streq(*sharg,"-i")) {
		sharg+=2;
		nsharg-=2;
		infile(sharg[-1]);
	}
	if (tset == 0) ttype();	
	line = 0;
#ifdef DIRED
	if ((nsharg) && (**sharg == '-')) {
		strcpy(dired_args+3,((*sharg)+1));
		nsharg;
		sharg++;
	}
#else 
	if (nsharg&& (**sharg == '+')) {
		cp = nscan(((*sharg)+1),&line);
		sharg++;
		nsharg--;
	}
#ifdef CRYPTO
	if (nsharg && (streq(*sharg,"-x"))) {
		sharg++;
		nsharg--;
		setkey(1,1);		/* Ask user for key */
	}
#endif
#endif
	if (nsharg) {
		readin(*sharg,1);
		nsharg--;
		sharg++;
#ifdef DIRED
	} else readin (".",1);
#else
	}
	if (line) absgoto(line);
#endif
main_loop:setjmp(retjmp);			/* establish return point */
#ifdef PC
	prompt(MODLN+1,"Proprietary and a Trade Secret of Bell Laboratories, No Unauthorized Copying");
#endif
	while (1) {
		edit(1);
		gquit();			/* quit if done */
	}
}

/* mapch -- map a character */

mapch(arg)
int arg;
{
	register int fromc;
	register int toc;
	register char *fromp;
	char *fromac;
	extern char *maclook();

	if (arg == 0) {
		for (toc = 0; toc < NCHARS; toc++) map_it[toc] = doit[toc];
		return;
	}
/* Keywords: commands key-bindings user-interface */
	
	toc = gechar("Map character");
	switch(arg) {
		
	case 1:
	
		fromc = gechar("To command");
		mapkey(toc,fromc);
		break;
	case 2:
	case 4:
		fromp = getname ("To macro: ");
		if (fromac = maclook(fromp)) {
			map_it[toc] = (fromac-&bbuf[0][0]+ISIZE);
		} else error(WARN,51,fromp); /* Macro not found */
		break;
	}
}

mapkey(toc,fromc)
int toc;
int fromc;

/* Keywords: key_bindings user_interface:10  */
{
	map_it[toc] = doit[fromc];
}

#ifdef ASSKEY
asskey(string,key)
char *string;
int key;


/* Keywords: key_bindings:50 terminal_initialization terminal_parameters */

{
	int tokey;
	
	if (string == NULL) return;
	tokey = *string++;
	if (tokey == ESC) {
		tokey = 0200 + *string++;
	}
	if (tokey == CTRLX) {
		tokey == 0400 + *string++;
	}
	if (*string) return;
	
	if (map_it[tokey]) return;
	mapkey(tokey,key);
}
#endif

#ifdef PC
mapk(arg)
int arg;
{
	char *cp;
	extern char *rrxtab;
	int c;
/* Keywords: commands key-bindings  PC-only */
	cp = &rrxtab[2*arg];
	c = gechar("command");
	if (c & 0400) {
		*cp++ = '';
		c-=0400;
	}
	if (c & META) {
		*cp++ = 033;
		c -= META;
	}
	*cp = c;
}
#endif		
/* help prompts for a character, and displays the function of that
character.  * displays all non self inserting and defined character
commands, ^X prompts for another character to describe a ^X sequence */

help()		/* help function */

{
	register int c;
	register int x;
	char hbuf[HELSIZE];

/* Keywords: help-messages commands user-interface character-display:10 */
	
	prompt1("M-?: ");
	c = getchar();
	if (c == ESC) c = (getchar()) + META;
	mtop();
	if (c == '*') {
		for (c = 0; c < 0377; c++) {
			if ((x=map_it[c]) && (x != HINSERT)) {
				helpin(c,hbuf);
				if(putout("The character '%c'  %s",c, hbuf)) return;
			}
		}
	} else {
		helpin(c,hbuf);
		putout("The character '%c'  %s",c, hbuf);
	}
	putout (endput);
	if (c != 030) {
		IGNORE(contin());
		if (helfile>0) close(helfile);
		helfile = 0;
		return;
	}
	c = getchar();
	if (c != '*') {
		helpin(c+CTLX,hbuf);
		putout ("The sequence '^X%c' %s", c, hbuf);
	} else for (c =0; c<128; c++) {
		if (map_it[c+CTLX]) {
			helpin(c+CTLX,hbuf);
			if(putout ("The sequence '^X%c' %s", c, hbuf)) return;
		}
	}
	putout (endput);
	IGNORE(contin());
	if (helfile>0) {
		close(helfile);
		helfile = 0;
	}
	return;
}

/* wall chart -- produce a wall chart of all emacs characters */


wallc()
{
	register int i;
	register int x;
	int oldln,oldcol;
	int oldtab;
	char hbuf[HELSIZE];
	
/* Keywords: help-messages commands insertion character-display:10 */

	
	oldtab = TABMD; 
	oldln = curln;
	oldcol = column;
	TABMD = 0;			/* flush C mode */
	putin(myname);
	putin(" version ");
	putin(version);
	putin(" Date: ");
	putin(hdate);
	nl(2);
	for (i = 0; i < NCHARS; i++) {

		if (i == CTLX) {
			nl(1);
			putin("Control-X commands:");
			nl(2);
		}
		if ((x=map_it[i])&& (x != 9)){

			if (i&CTLX) {
				cput(CTRLX);
				cput(i-CTLX);
			} else {
				cput(i);
			}
			put(':');
			put('	');
			helpin(i,hbuf);
			putin(hbuf);
			nl(1);
		}
	}
	if (helfile>0) {
		close(helfile);
		helfile = 0;
	}
	unins(oldln,oldcol);
	TABMD = oldtab;
}


/* print an error message and wait for response.  ^Z exits emacs
 * immediately, ^G tries  to stop the current command by returning 1
 * from error, any other character just continues (returns 0)*/

/*VARARGS1*/

error(sev,enumb,arg1,arg2,arg3)

int sev;
int enumb;
int arg1;
int arg2;
int arg3;

{
	char c;
	int cc;
	long skadd;
	char string[HELSIZE];
	char errpath[128];
	int errfile;
/* Keywords: error-messages unix-interface user-interface */

	unprompt();			/* remove spurious messages */
	if (SCRWID==0) sttype(NULL); /* Fix Null terminal type */

#ifdef PC
	if (enumb == 0) enumb = 13;
	errfile = open("a:error.blk",4);	    /* open errmsg file */
	if (errfile < 0) errfile = open("b:error.blk",4);
#else
	seprintf(errpath,"%s/errfile",em_dir);
	errfile = open(errpath,0);	/* open errmsg file */
#endif	 PC
	skadd = (long) (enumb-1) * HELSIZE; /* 0 is no error */


	if (errfile>0) {
		 do {
			 lseek(errfile,skadd,0);
			 cc = read(errfile,string,HELSIZE);
		 } while ((cc != HELSIZE) && (errno == 4));
		 close(errfile);
	} else {
		seprintf(string,"Can't access %s/errfile for error %d",em_dir,enumb);
	}
	mtop();
	putout(string,arg1,arg2,arg3);
	putout(endput);
	beep();
	mflush(stdout);			/* flush output */
reread:
#ifdef PC
	rawread(&c);
#else	
	cc = 0;
	if (no_io) {
		c = CTRLZ;
	} else {
		
		while (read(0,&c,1) !=1) {	/* always from tty */
			c=CTRLZ;		/* Default to quit */
			if (++cc > 60) break; 	/* Quit after 1 hour */
		}
	}
#endif
	if ((c&0177) == CTRLBRAK) eabort(0);	/* crash dump */
	if (sev == FATAL) crash(0);
	switch(c&0177) {

	case CTRLG:
	case RUBOUT:
		while (infrn) inpop();	/* get out of any init files */
		return(-1);
	case 'n':
		return(1);				/* CTRL -G */
	case CTRLZ: gquit();		/* get out of emacs */

	case CTRLS:
	case CTRLQ:
		goto reread;		/* Ignore ^S/^Q from terminal */

	default: return(0);		/* normal return */
	}
}


char *		
hmap(helno)
/* Keywords help-messages macro-programming user-interface:10 */

int helno;
{
	register char *macp;
	macp = &(bbuf[0][helno-2]);
	while (*(--macp));
	return(macp+1);
}

helpin(helno,hbuf)

register int helno;
register char *hbuf;

{
/* Keywords: help-messages unix-interface  */
	
	register int cc;
	long skadd;
	char helname[128];
	
	helno = map_it[helno];
	if (helno>ISIZE) {
		strcpy(hbuf,hmap(helno-ISIZE));
		return;
	}
	if (helfile == 0) {
#ifdef PC
		helfile = open ("a:help.blk",4);
		if (helfile < 0) helfile = open ("b:help.blk",4);
#else
		seprintf(helname,"%s/helpfile",em_dir); /* construct path */
		helfile = open (helname,0);
		if (helfile < 0) {
			helfile = 0;
			seprintf(hbuf,"Can't open help message file: %s",helname);
		}
#endif PC
	}
	skadd = (long) (helno) * HELSIZE;
#ifdef PC
	if (helfile > 0) {
		lseek(helfile,skadd,0);
		cc = read(helfile,hbuf,HELSIZE);
	}
#else
	do {
		lseek(helfile,skadd,0);
		cc = read(helfile,hbuf,HELSIZE);
	} while ((cc != HELSIZE) && (errno == 4));
#endif PC
}


statout()

{
#ifndef PC
	char stbuf[256];
	register int fid;
	register int i;
	char fbuf[128];
/* Keywords: user-interface:10 statistics exit-processing */
	
	struct tstruct {
		long usrtime;
		long systime;
		long xt1;
		long xt2;
	} tbf;
	
	times(&tbf);

	seprintf(stbuf,"%d %D %D %d %D %d %d %d %d %d\n",
		myuid,tbf.usrtime,tbf.systime,ninch,noutc,
		nmkline,nbread,nbwrite,nbseek,TERMIQ);
	seprintf(fbuf,"%s/s%s",em_dir,version);

#ifdef ux3
	fid = open(fbuf,O_WRONLY+O_APPEND);
	if (fid) {
#else
	fid = open(fbuf,1);
	if (fid) {
		lseek(fid,0L,2);
#endif ux3
		write(fid,stbuf,lng(stbuf));
		close(fid);
	}
#ifdef CMON
	seprintf(fbuf,"%s/cmcnt",emd_source);
	fid = open (fbuf,1);
	if (fid>=0) {
		lseek(fid,0L,2);
		write(fid,cmcnt,sizeof(cmcnt));
		close(fid);
	}
#endif CMON
#ifdef MONITOR
	monitor(0);
#endif MONITOR
#endif PC
}
#ifdef DIRED

char *

fxname(line,fbuf)

int line;
char *fbuf;
/* Keywords: dired user-interface file-selection */

{
	register char *xp;
	register char *lp;
	register char *fp;

	lp = mkline(line)+leng(line)-1;
	while (*lp == ' ') lp--;
	fp = lp;
	while ((*fp != ' ') || ((fp[-3] != ':') && (fp[-3] != '9'))) fp--;
	xp = fbuf;
	while (fp != lp) *xp++= *(++fp);
	*xp = 0;
	return(fbuf);
}
dcdel(count,arg)
/* Keywords: dired commands deletion */

{
	if ((diron == 0) || (column)) insertc(count,arg); /* not at BOL */
	else ddel(count);
}

/* delete an entry */

ddel(count,arg)

register int count,arg;
/* Keywords: dired commands deletion */

{
	if (diron) {
		while (count--) {
			move(curln,0);
			if (*clptr != EOL) insertc(1,'D');
			if (curln<nlines)move(curln+1,0);
		}
	} else {
		switch(arg) {
		case CTRLD:
			fdel(count,arg);
			break;
		case CTRLH:
		case RUBOUT:
			bdel(count,arg);
			break;
		case CTRLK:
			ekill(count,arg);
			break;
		default: beep();	/* I don't know how we got here */
		}
	}
}

dundel(count,arg)

/* Keyword dired commands undoing */

register int count;
{
	if ((diron == 0) || column) {
		return(insertc(count,arg));
	}
	while (count--) {
		if (*clptr != EOL) insertc(1,' ');
		if (curln<nlines) move(curln+1,0);
	}
}

dview(count,arg)
/* Keywords: dired commands recursive-editing */

{

	char fxb[64];
	char dxb[128];
	int obuf,nbuf;
	
	if (column || (diron == 0)) insertc(count,arg);
	else {
		seprintf(dxb,"%s/%s",fname(),fxname(curln,fxb));
		obuf = curbf;
		if (chgbuf(dxb) && readin(dxb,1)) {
			nbuf = curbf;
			recurse(1);
			if (nbuf != curbf) chbuf(nbuf);
			if (diron) fsave(0);
		}
		if (obuf != curbf) {
			nbuf = curbf;
			chbuf(obuf);
			klbfr(nbuf);
		}
	}
};

catstr(dp,sp1,sp2)

/* concatenate */

/* Keywords: string-handling */

register char *dp;
register char *sp1;
register char *sp2;

{
	while (*dp++ = *sp1++);
	*(dp-1)='/';
	while (*dp++ = *sp2++);
}
dclean()
/* Keywords: dired exit-processing:10 user-interface:40 deletion:30 unix-interface */

{
	register int i;
	register char *lp;
	register int status;
	char *cp;
	int ndel;
	char obuf[16];
	int ouid,guid;
	char *cp1,*cp2;
	extern int cstatus;	
	char nbuf[256];
	
	
	if (streq(fname(),".passwd")) return; /* don't edit .passwd! */
	ndel = 0;
	for (i = 1; i <= nlines; i++) {
		lp = mkline(i);
		if ((*lp == 'D') || (lp[1] == 'M')|| (lp[2] == 'O')){
			if (ndel == 0) {
				clear();
				putout ("Editing the following files from directory %s:", fname());
				putout ("");
			}
			putout ("%c%c%c	%s",lp[0],lp[1],lp[2],fxname(i,nbuf));
			ndel++;
		}
	}
	if (ndel == 0) return;
	i = gyn("OK?");
	if (i==0) return(0);
	if (i<0) quit();
	
	cp = mstrcpy(nbuf,fname());
	*cp++ = '/';
	for (i = 1; i < nlines; i++) {
		lp = mkline(i);
		if (*lp == 'D') {
			fxname(i,cp);
			if (lp[4] == 'd') {
				char xbuf[256];

				seprintf(xbuf,"rm -fr %s",nbuf);
				unx(xbuf,4); /* Run rm command on it */
				status = cstatus;
				errno = 66; /* Can't remove directory */
			} else {
				status = unlink (nbuf);
			}
			if (status){
				error (WARN,errno,nbuf);
			} else {
				move(i,0);
				ekill(1); /* kill line */
			}
		} else {
			if (lp[1] == 'M') {
				fxname(i,cp);
				status = chmod (nbuf,mkmode(lp+5));
				if (status) {
					error (WARN,errno,nbuf);
				} else {
					move(i,1);
					insertc(1,' ');
				}
			}
			if (lp[2] == 'O') {
				fxname(i,cp);
				cp1 =obuf;
				cp2 = lp+19;
				while ((*cp1++ = *cp2++) != ' ');
				*(--cp1) = 0;
				if ((ouid = gtuid(obuf))== -1) goto badpw;
				lp = mkline(i);
				cp1 = obuf;
				cp2 = lp+28;
				while ((*cp1++ = *cp2++) != ' ');
				*(--cp1) = 0;
				if ((guid = gtuid(obuf))== -1) {
badpw:					error (WARN,67,obuf,nbuf);
					continue;
				}
				lp = mkline(i);
				status = chown(nbuf,ouid,guid);
				if (status) {
					error (WARN,errno,nbuf);
				} else {
					move(i,2);
					insertc(1,' ');
				}
			}
		}
	}
	unmod(1);			/* buffer is now up to date */
	return(1);
}

/* mkmode -- translate from ls -l style mode representation to */
/* a 16 bit mode number */

/* mode bit table.  one entry per character, contains characters followed */
/* by bit number to turn on */

char *modebits[10] = {
	"r\011",
	"w\010",
	"x\007s\014s\007",
	"r\006",
	"w\005",
	"x\004s\013s\004",
	"r\003",
	"w\002",
	"x\001t\012t\001",
	0};

mkmode(sp)
char *sp;

/* Keywords: dired file-modes user-interface:10 unix-interface:30 */


{
	int newmode;
	char *mp;
	char **mep;
	
	newmode = 0;
	mep = modebits;
	while (mp = *mep++) {
		while (*mp) {
			if (*mp == *sp) {
				newmode |= (1<< (mp[1]-1));
			}
			mp+= 2;
		}
		sp++;
	}
	return(newmode);
}

/* gtuid -- get uid from user name */

/* **** NOTE, we can't use getpwnam, because it uses standard I/O */

gtuid(name)

register char *name;
/* Keywords: dired unix-interface:10 password-file user-id-processing */

{
	int oldbf;
	unsigned uid;
	char xbuf[100];
	register char *cp;
	oldbf = curbf;
	
	chgbuf (".passwd");
	if (nlines < 10) {
		readin ("/etc/passwd",1);
	}
	seprintf (xbuf,"^%s:",name);
	
	if (rgsrch(1,0,xbuf,0,1)) {
		srch(kline,kcol,":",1);
		srch(kline,kcol+1,":",1);
		uid = 0;
		cp = mkline(kline)+kcol+1;
		while (*cp != ':') {
			uid = (uid*10)+(*cp-'0');
			cp++;
		}
	} else uid = -1;
	chbuf(oldbf);
	return(uid);
}
#endif
