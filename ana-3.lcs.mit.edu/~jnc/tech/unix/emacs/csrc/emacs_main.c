#include <signal.h>
#ifdef	ux6
#include <inode.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_main.h"
#include "emacs_help.h"

extern int kbapp;
int szflg;			/* save  buffer 0 without asking */

/* EMACS_MODES: c !fill */



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
	
	
	
#define BUFMAX 077000			/* getting too close */

	for (;;) {
		arg = 1;
		metf = 0;
		numarg = 0;
		kbapp >>= 1;		/* kludge for appending kills */
		if (edisp == 2) {	/* execute command */
			c = echar;
			realc = echar&0177;
			metf = echar & 0600;
			arg = earg;
			edisp = 0;		/* do one command */
			goto dispose;
		}

		if (SAVEMD) {
			if (NSCHAR++>SAVECHAR) {
				if (modded() && (READONLY == 0)&& *fname()) IGNORE(fsave());
				move(curln,column); /* make sure we get back */
				NSCHAR=0;
			}
		}

		disup();		/* update display */

re_get:
		
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
			c = (realc =getchar()) + metf;
			unprompt();
		} else c = (realc = getchar()) + metf;

/*		if (timemd) dtime(0); /* display time */
		if (newmail && (infrn == 0)) {
			prompt(ECHOL-1,"You have mail");
			beep();
			goback();
			newmail = 0;
		}
		if (BUFEND > BUFMAX) collect(); /* garbage collection */

/* note -- the following code, and the macro definition facility */
/* depends on the fact that subroutine addresses have even addresses */
			
dispose:				/* dispose of character command */

#ifdef CMON
		cmcnt[c]++;		/* count command statistics */
#endif
		if ((dochar= (int) (do_it[c]))&01) {

		/* c is a macro invocation or interpreter character */
			if (dochar < ICHAR) {
				eresult = xmac(dochar>>1,arg,c);
			} else {
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
				}
				eresult = insertc(arg,c);		/* some numbers self insert */
			}
		} else {
			eresult = (*do_it[c]) (arg,c);
		}
		if (edisp == 0) return(eresult);
	}
}

/* xqt -- execute a command through argument 0. */

/* the character command designated by macro argument 0 is run with the  */
/* current argument */

xqt(arg)

int arg;
{
	
	return(edit(2,marg[0],arg));		/* run command */
}
/* ckmail -- check for user mail */

ckmail()
{
	register char *cp;
#ifdef	ux6					/* Unix V6? */
	struct inode statb;
#else
	struct stat statb;
#endif

	if (cp = getenv("MAIL")) {
#ifdef	ux6
		if ((stat(cp,&statb)>=0) && (statb.size0||statb.size1)) {
#else
		if ((stat(cp,&statb)>=0) && statb.st_size ) {
#endif
			newmail = 1;
		}

	}


	mailcnt = CKMAIL;
}


/* dtime -- display the time on the screen */

long clock;

dtime(flag)

int flag;
{
	register char *tp;
	int x;
	int y;
	
	if (timemd) {
		/*if (flag)*/ time(&clock); /* get time */
		if ((clock-oclock > 60) || flag) {
			tp = ctime(&clock); /* convert */
			tp[16] = 0;	/* wipe out newline */
			x = mline;
			y = mcol;
			prompt(MODLN+1,tp); /* output */
			oclock = clock;
			mgo(x,y);
		}
	}
}

ticker()
{
	time(&clock);			/* get time now */
	signal(SIGALRM,ticker);
/*	alarm(60);*/
}


/* init -- initialize editor data */

/* initializes EMACS, goes into raw mode, creates the buffer main, and
 * reads in the file specified by the argument to the emacs command */


init()

{
	register int i;
	

	uncook();
	for (i = 0; i < 16; i++) signal(i,eabort); /* trap various problems */

/* some signals cause attempt to save - others are just ignored */

	signal (SIGINT, SIG_IGN);
	signal (SIGQUIT, SIG_IGN);
	signal (SIGTERM,crash);
	signal (SIGHUP,crash);
	for (i = 0; i < 128; i++) casem[i] = i; /* search map */
	ioinit();
	curln = 1;
	column = 0;
	IGNORE(chgbuf("Main"));

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
	extern dcdel();
#endif
	extern int fbkno, macptr;
	register char *ap;
	register char *np;
	char *cp;
	int tset;


#ifdef MONITOR

#define MONSIZ 5000
	short monbuf[MONSIZ];
	extern int etext;

	monitor(2,&etext,monbuf,MONSIZ,0); /* profile on */
#endif

	
					/* find our name */

	for (ap= myname = argv[0]; *ap; ap++) {
		if (*ap>0140) *ap = (*ap) - 040;
	}
	
	NPTRS = 0;			/* no memory */
	
	
/* special initialization for DIRED */

#ifdef DIRED

#define CTX(xchar) ((xchar)+0400)

 	LNOMOD = 0;
 	FILLMD = 0;
	OVERW = 1;
	do_it[CTX('')] = do_it[CTX('')] = dclean;
	helpc[CTX('')] = helpc[CTX('')] = 78;
	do_it['d'] = do_it['D'] = dcdel;
	do_it[''] = do_it[''] = do_it[''] =ddel;
	helpc['d'] = helpc['D'] = helpc[''] = helpc[''] = helpc[''] = 75;
	do_it['u'] = do_it['U'] = dundel;
	helpc['u'] = helpc['U'] = 76;
	do_it['e'] = do_it['E'] = dview;
	helpc['e'] = helpc['E'] = 77;
	
#endif
	fbkno = macptr = 0;			/* no macros */
	init();				/* initialize */
	if ((cp=getenv("TERM"))!= NULL) {
		sttype(cp);		/* set up for terminal */
		tset = 1;
	} else {
		sttype("unknown");		/* default terminal type */
		tset = 0;
	}
#ifndef DIRED
	if ((ap = getenv("HOME")) != NULL) {
		np = itbuf;
		while (*np++ = *ap++);
		strcpy(--np,"/.emacs_init");
		if (pushin(itbuf)){
			edit(1);		/* run init function */
			inpop();
		}
	}
	szflg = 0;			/* no auto-save */
	if (streq(argv[1],"-s")) {	/* but maybe he wants */
		argv+=1;
		argc-=1;
		szflg++;
		}

	if (streq(argv[1],"-i")) {
		argv+=2;
		argc-=2;
		if (pushin(argv[0])) {
			edit(1);
			inpop();
		}
	}
#endif
	if (tset == 0) ttype();	
	
#ifdef DIRED
	if ((argc >1) && (argv[1][0] == '-')) {
		strcpy(dired_args+3,argv[1]+1);
		argc--;
		argv++;
	}
#endif
	if (argc > 1) {
		readin(argv[1],1);
#ifdef DIRED
	} else readin (".",1);
#else
	}
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
	int hc;
	int *hx;
	register int fromc;
	register int toc;
	
	toc = gechar("Map character");
	fromc = gechar("To command");
	
/* note **** the following assignments will cause type complaints from */
/* the portable C compiler.  This is not important */
	
	hc = helpc[fromc];
	hx = (int *)do_it[fromc];

	helpc[toc] = hc;
	do_it[toc] = (int (*)())hx;
}


		
/* help prompts for a character, and displays the function of that
character.  * displays all non self inserting and defined character
commands, ^X prompts for another character to describe a ^X sequence */

help()		/* help function */

{
	register c;
	register x;
	char hbuf[HELSIZE];
	
	prompt1("M-?: ");
	c = getchar();
	if (c == ESC) c = (getchar()) + META;
	mtop();
	if (c == '*') {
		for (c = 0; c < 0377; c++) {
			if ((x=helpc[c]) && (x != HINSERT)) {
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
		if (helfile) close(helfile);
		helfile = 0;
		return;
	}
	c = getchar();
	if (c != '*') {
		helpin(c+CTLX,hbuf);
		putout ("The sequence '^X%c' %s", c, hbuf);
	} else for (c =0; c<128; c++) {
		if (helpc[c+CTLX]) {
			helpin(c+CTLX,hbuf);
			if(putout ("The sequence '^X%c' %s", c, hbuf)) return;
		}
	}
	putout (endput);
	IGNORE(contin());
	if (helfile) close(helfile);
	helfile = 0;
	return;
}

/* wall chart -- produce a wall chart of all emacs characters */


wallc()
{
	register i;
	register x;
	int oldtab;
	char hbuf[HELSIZE];
#ifdef	DIRED
	extern int diron;
#endif
	
	oldtab = TABMD; 
	TABMD = 0;			/* flush C mode */
#ifdef	DIRED
	diron = 0;			/* it's not a directory */
#endif
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
		if ((x=helpc[i]) && (x != HINSERT)) {

			if (i&CTLX) {
				cput('');
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
	close(helfile);
#ifdef	DIRED
	diron = 1;
#endif
	TABMD = oldtab;
	helfile = 0;
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
	int errfile;

	errfile = open(errpath,0);	/* open errmsg file */
	
	skadd = (long) (enumb-1) * HELSIZE; /* 0 is no error */
	seprintf(string,"Can't access error file for error %d",enumb);
	do {
		lseek(errfile,skadd,0);
		cc = read(errfile,string,HELSIZE);
	} while ((cc != HELSIZE) && (errno == 4));

	close(errfile);
	mtop();
	putout(string,arg1,arg2,arg3);
	putout(endput);
	beep();
	mflush(stdout);			/* flush output */
	while(read(0,&c,1) != 1);	/* always from tty */
	if ((c&0177) == '') eabort();	/* crash dump */
	if (sev == FATAL) crash();
	switch(c&0177) {

	case '':
	case '':
		while (infrn) inpop();	/* get out of any init files */
		return(1);				/* CTRL -G */
	case '': gquit();		/* get out of emacs */
	
	default: return(0);		/* normal return */
	}
}
		


helpin(helno,hbuf)

register int helno;
register char *hbuf;

{
	register cc;
	long skadd;
	
	cc = (int) (do_it[helno]);
	if ((cc & 01) && (cc < ICHAR)) {
		strcpy(hbuf,bbuf[0]+(cc>>1)-helpc[helno]);
		return;
	} else {
		helno = helpc[helno];
	}
	if (helfile == 0) helfile = open (helname,0);
	if ((helfile < 0) && (errno  == 4)) helfile = open(helname,0);
	
	skadd = (long) (helno & 0177) * HELSIZE;
	
	do {
		lseek(helfile,skadd,0);
		cc = read(helfile,hbuf,HELSIZE);
	} while ((cc != HELSIZE) && (errno == 4));
}


statout()

{
	char stbuf[256];
	int uid;
	int fid;
	char fbuf[128];
	
	struct tstruct {
		long usrtime;
		long systime;
		long xt1;
		long xt2;
	} tbf;
	
	times(&tbf);
	uid = getuid();
	
	seprintf(stbuf,"%d %D %D %d %D %d %d %d %d\n",
		uid,tbf.usrtime,tbf.systime,ninch,noutc,
		nmkline,nbread,nbwrite,nbseek);
	seprintf(fbuf,statpath,version);

	fid = open(fbuf,1);
	if (fid) {
		lseek(fid,0L,2);
		for (uid = 0; stbuf[uid] ;uid++);
		write(fid,stbuf,uid);
		close(fid);
	}
#ifdef CMON
	fid = open (cmpath,1);
	if (fid) {
		lseek(fid,0L,2);
		write(fid,cmcnt,512);
		close(fid);
	}
	fid = open (cxpath,1);
	if (fid) {
		lseek(fid,0L,2);
		write(fid,cxcnt,256);
		close(fid);
	}
#endif
#ifdef MONITOR
	monitor(0);
#endif
}
#ifdef DIRED

char *

fxname(line,fbuf)

int line;
char *fbuf;

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

dexec(fn,line)

register char *fn;
register int line;
{
	char cline[256];
	char fxb[64];
	int stat;
	
	clear();
	seprintf (cline,"%s %s/%s", fn, fname(), fxname(line,fxb));
	cook();
	stat = system (cline);
	uncook();
	clear();
	return(stat);
};


dcdel(count,arg)
{
	if (column) insertc(count,arg); /* not at BOL */
	else ddel(count);
}

/* delete an entry */

ddel(count)

register count;
{
	while (count--) {
		move(curln,0);
		if (*clptr != EOL) insertc(1,'D');
		if (curln<nlines)move(curln+1,0);
	}
}

dundel(count,arg)

register count;
{
	if (column) {
		return(insertc(count,arg));
	}
	while (count--) {
		if (*clptr != EOL) insertc(1,' ');
		if (curln<nlines) move(curln+1,0);
	}
}

dview(count,arg)

{
	if (column) insertc(count,arg);
	else {
		if (clptr[4] == 'd') dexec("dired",curln);
		else dexec("emacs",curln);
	}
};

catstr(dp,sp1,sp2)

/* concatenate */

register char *dp;
register char *sp1;
register char *sp2;

{
	while (*dp++ = *sp1++);
	*(dp-1)='/';
	while (*dp++ = *sp2++);
}
dclean()

{
	register i;
	register char *lp;
	register status;
	char *cp;
	int ndel;
	char obuf[16];
	int ouid,guid;
	char *cp1,*cp2;
	
	char nbuf[256];
	
	
	if (streq(fname(),".passwd")) return; /* don't edit .passwd! */
	clear();
	ndel = 0;
	for (i = 1; i <= nlines; i++) {
		lp = mkline(i);
		if ((*lp == 'D') || (lp[1] == 'M')|| (lp[2] == 'O')){
			if (ndel == 0) {
				putout ("Editing the following files from directory %s:", fname());
				putout ("");
			}
			putout ("%c%c%c	%s",lp[0],lp[1],lp[2],fxname(i,nbuf));
			ndel++;
		}
	}
	if (ndel == 0) return;
	if (gyn("OK?")<= 0) return (0);;
	
	cp = strcpy(nbuf,fname());
	*cp++ = '/';
	for (i = 1; i < nlines; i++) {
		lp = mkline(i);
		if (*lp == 'D') {
			fxname(i,cp);
			if (lp[4] == 'd') {
				status = dexec ("rm -fr",i);
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
	unmod();			/* buffer is now up to date */
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

{
	int oldbf;
	unsigned uid;
	extern int diron;
	char xbuf[100];
	register char *cp;
	oldbf = curbf;
	
	chgbuf (".passwd");
	if (nlines < 10) {
		diron = 0;
		readin ("/etc/passwd",1);
		diron = 1;
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
