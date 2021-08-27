#include "emacs_io.h"
#include <signal.h>
#include "emacs_gb.h"
#include "emacs_cmds.h"
/* EMACS_MODES: c, !fill */

/* move forward COUNT characters */

forw(count)

register count;
{
	register retval;
	if((retval = findf(count))== 0) beep(); /* couldn't go all the way */
		
	move(kline,kcol);
	return(retval);
}

/* backward COUNT characters */

back(count)

register count;

{
register retval;

	if((retval = findb(count)) == 0) beep();		/* find new position */
	move (kline,kcol);
	return(retval);
}

/* move to previous line, same collumn */

upl (count) 

register count;

{

	if (curln-count < 1) {
		if (count == 1) {
			beep();
			return(0);
		} else curln = 1;
	} else curln -= count;
	mvc(curln,column);
	return(1);
}
/* move down one line, same column */

downl (count)

register count;

{
register retval;

	if ((curln += count) > nlines) {
		curln = nlines+NLRUN;
		beep();
		retval = 0;
	} else retval = 1;
	mvc(curln,column);
	return(retval);
}


/* abort EMACS */

eabort()

{
	cook();
	mflush(stdout);
	signal (SIGIOT,SIG_DFL);
	abort();
}


/* exit EMACS */


quit()

{
	clear();
	cook();
	mflush(stdout);
	statout();
	exit(0);
}

/* exit EMACS gracefully */

gquit()

{
	if(bclean()== 0)quit();
}

/* kill line */

/* if the count is one, and if there is text on the line beyond column,
 * only that text is killed 
 * if count is one, and column is at end of line, the end of line is killed */
/* if count is greater than one, the next count lines (and their end of
 * lines) are killed */

/* all killed text is put into the kill stack */


ekill (count)

register count;

{
	register l;
	
	if (numarg == 0) {
		if ((l=leng(curln)) > column) {
			return(delc(l-column));
		}
	}
	kline = curln+count;
	kcol = 0;
	return(tkill());
}

/* goto beginning of current line */

begin()

{
	move(curln,0);
}

/* goto end of line */

endl()

{
	mvc (curln,10000);		/* mvc will adjust line length */
}
/* insert the next character, whatever it is */

/* note that newlines inserted ths way act just like unquoted newlines */

quote(count,qchar)

register qchar;
register count;

{
	register c;

	while(count--) {
		if ((VERBOSE)&& (MOREIN == 0)) prompt1("%d %c: ",count+1,qchar);
		
		c = getchar();
		c = c | (qchar & META);
		insertc(1,c);
		if (VERBOSE && (MOREIN == 0)) {
			unprompt();
		}
		disup();
	}
}

/* numchar -- convert argument to a character to insert */

numchar(count)

register count;
{
	insertc(1,count);		/* insert the count */
}


/* deletes count characters going forward */

fdel(count)

{
	IGNORE(findf(count));
	return(tkill());
}

/* deletes count characters going backward */

bdel(count)

register count;

{
	IGNORE(findb(count));
	return(tkill());
}

/* file write command */


int fright(arg)

int arg;
{
	register char *np;

	if ((np = expenv(getname("Write file? "))) != NULL) {
		return(wout(np,(arg != 1)));
	}
	return(0);
}

/* fred -- read a file */

fred(arg)

int arg;
{
	register char *np;

	if ((np = expenv(getname("Read File? "))) != NULL) {
		readin(np,arg);
		return(1);
	} else return(0);
}

/* forward words -- leaves kline, kcol at spot that is count words
 *forward */


wordf(count)

register count;

{
	kmark();

	while (count--) {
		if (skipf(WRDSEP) || skipf(WRDCHR)) {
			return;
		}
	}
}

/* skips kline, kcol forward until a character without type bit on is
 * found */


skipf(bit)

register bit;

{
	while (bits[*klptr] & bit) {
		if (mfk()) return(1);
	}
	return(0);
}


/* move forward count words */

mfwrd(count)

register count;

{
	wordf(count);
	move(kline,kcol);
}

/* kill next count words */



kfwrd(count)

register count;

{
	wordf(count);
	return(tkill());
}

/* skip kline, kcol back until a character without type bit is found */



skipb(bit)

register bit;

{
	do {
		if (mbk()) return(1);
	} while (bits[*klptr] &bit);
	return(0);
}

/* backward count words, leaves pointer in kline, kcol */

wordb(count)

register count;

{
	kmark();
	while (count--) {
		if ((skipb(WRDSEP) || skipb(WRDCHR))) {
			return;
		}
	}
	IGNORE(mfk());
}

/* move back count words */

mbwrd(count)

register count;

{
	wordb(count);
	move(kline,kcol);
}

/* kill back count words */

kbwrd(count)


register count;

{
	wordb(count);
	return(tkill());
}

/* kmark -- set kline,kcol and klptr */

kmark()
{
	kline = curln;
	kcol = column;
	klptr = mkline(kline)+kcol;
}


/* move kline,kcol forward one */

mfk()

{
	if ((*klptr++)!=EOL ) {
		++kcol;
		return(0);
	}
	if (kline<nlines) {
		++kline;
		klptr = mkline(kline);
		kcol=0;
		return(0);
	}
	return(1);
}

/* move kline, kcol back one character */

mbk()
{
	if ((kcol--)>0) {
		klptr--;
		
		return(0);
	}
	if (kline>1) {
		--kline;
		klptr = mkline(kline) +	(kcol = leng(kline));
		return(0);
	}
	kcol = 0;
	return(1);
}

	/* forward to end of sentence */

	/* leaves resulting pointer in kline, kcol */

fsent () 
{
	kmark();

	while (mfk() == 0) {
		if ((bits[*klptr] & SENTE) && (bits[klptr[1]]&WHITE)) {
			return(1);
		}
	}
	return(0);
}

/* back one sentence, leaves pointer in kline, kcol */

bsent()
{
	
	kmark();

	IGNORE(skipb(WRDSEP));
	while (mbk() == 0) {
		if (kcol == 0) {
			if (kline>1) {
				if (bits[*mkline(kline-1)] & SENTE) return(1);
			}
		}
		if ((bits[*klptr] & SENTE) && (bits[klptr[1]] & WHITE)) {
			IGNORE(skipf(WRDCHR));
			IGNORE(skipf(WRDSEP));
			return(1);
		}
	}
	return(0);
}

/* move back count sentences */

ssent(count)
register count;
{
	while (count--&& bsent())
		move(kline,kcol);
	move (kline, kcol);
}

/* move forward count sentences */

esent(count)
register count;
{
	while (count--)
	if (fsent())
		move(kline,kcol+1);
	else {
		move(kline,kcol);
		break;
	}
}


/* move to top of file */

top()
{
	move(1,0);
}


/* move to end of file */

bot()
{
	mvc(nlines,10000);
}

/* new line handler */

/* First, finishes off comment on the current line (if any) */

/* Then, if the next line is non-empty, it creates an empty next line */
/* next, the next line is tab adjusted (if in C mode ) */

/* now, any text on the current line beyond the current position, it is
 * moved to the end of the next line */

/* finally, the pointer is moved to the (tab adjusted) start of the next
 * line */

/* if count > 1, then count-1 blank lines will be inserted in between
 * the current line and the 'next' line */

nl(count)

register count;

{

	if ((comln == curln) && (RARE == 0)) {
		putin(" */");
	}
	comln = 0;
	if ((count == 1) && (NLINS==0) && (isblank(curln+1))&& ((RARE == 0) || (clptr[column] == EOL))) {
		if (clptr[column] != EOL) {
			kbapp = 0;	/* don't append */
			ekill(1);	/* kill rest of line */
			move(curln+1,0);
			retrv();	/* bring back rest of last line */
			kpop();		/* get rid of junk */
			move(curln-1,10000); /* back to end of last line */
		}
	} else {
		openl(count);
	}
	if (TABMD&& (RARE == 0)) tabjust(curln+count);
	else move(curln+count,0);
}

/* sets mark at current position */

mark(mnumb)

{
	MARK *markp;
	markp = markptr(mnumb);
	markp->markl = curln;
	markp->markc = column;
}

/* returns mark pointer */

MARK *markptr(mnumb)

register int  mnumb;
{
	
	if (mnumb > NMARKS) {
		error(WARN,44);
		mnumb = curbf;
	}
	if ((mnumb == 1) && (numarg == 0)) mnumb = curbf;
	return(&marks[mnumb]);
}


/* kills text between current position and mark position */


mkill(mnumb)

{
	MARK *markp;
	
	markp = markptr(mnumb);
	kline = markp->markl;
	if (kline < 1) kline = 1;
	if (kline > nlines) kline = nlines;
	kcol = leng(kline);
	if (markp->markc < kcol) kcol = markp->markc;
	return(tkill());
}

/* retrieves text from the kill stack and inserts it (count times ) */



yank(count)

register count;
{
	register result;
	
	mark(curbf);
	while (count--) {
		result = retrv();
	}
	return(result);
}

/* kills the marked region, removes the top item from the kill stack,
 * and then inserts the next area from the kill stack */


reyank(count)		/* yank again */
register count;

{
	kbapp = 0;
	mkill(curbf);	/* flush last insertion */
	kpop();		/* first pop the mkill */
	kpop();		/* now pop that last insertion */
	yank(count);
}

/* search subroutine */

/* searches forward or backward for a string, starting at the 
 * given position.  If the string is found, 1 is returned, and the start
 * of the match is left in kline, kcol.  Return of 0 indicates no match */



srch(sline,scol,sp,direct)

int sline;
int scol;
char *sp;
int direct;

{
	register char *lp;
	register char *cp;
	register l;
	int c;
	
	lp = sp;
	while (c = *lp) *lp++ = casem[c&0177]; /* map string to allowed case */
	lp = mkline(sline)+scol;
	while (1) {
		cp = sp;
		l = sline;
		while (*cp) {
			if ((c = casem[(*lp++)&0177]) != *cp++) goto again;
			if (c == EOL) {
				if (l == nlines) {
					if (direct>0) return(0);
					goto again;
				}
				++l;
				lp = mkline(l);
			}
		}
		kline = sline;
		kcol = scol;
		return(1);

again:		lp = mkline(sline)+scol;
		if (direct>0) {
			do {			
				if (*lp++ !=EOL) scol++;
				else {
					if (sline == nlines) return(0);
					sline++;
					scol = 0;
					lp = mkline(sline);
				}
			} while (casem[*lp&0177] != *sp);
		} else {

			do {
				if (--scol<0) {
					if (sline == 1) return(0);
					scol = leng(--sline);
					lp = mkline(sline)+scol;
				} else lp--;
			} while (casem[*lp&0177] != *sp);
		}
	}
}

/* incremental search command (either direction)

/* NOTE -- this command assumes that search and reverse search are on 
 * and ,  if not, it may not start the search in the right direction */

isrch(oldln,chr)
int oldln;				/* this is not used as an
					 * argument, done this way
					 * for lint */
char chr;
{
	int oldcol;
	register char *ilp;
	register c;
	int lx;
	int ly;
	int d;
	int missed;
	char sst[40];
	char *xp;

	d = ((chr == '') ? 1 : -1);
	ilp = sst;
	missed = 0;
	lx = oldln = curln;
	ly = oldcol = column;
	if (infrn < 0) {			/* if in a macro */
		xp = getname(NULL);		/* search argument */
		ilp = strcpy(ilp,xp);
	}
	while (1) {
		*ilp = 0;
		if (missed == 0) {
			psrch(d,sst);		/* prompt */
			if (infrn == 0) mflush(stdout);
			if (srch(curln,column,sst,d) == NULL) {
				beep();
				move(lx,ly);
				prompt1("Failing Search: %s", sst);
				missed = 1;
			} else {
				move(lx = kline,ly = kcol);
				if (ilp!=sst)strcpy (presst,sst);
				missed = 0;
			}
		}
		if (infrn < 0) return(missed == 0); /* macro invocation */
		disup();
		if (missed == 0) {
			psrch(d,sst); /* restore prompt */
			goback();
		}
		c = getchar();
		if ((c == '')||(c == '')) {
			if (ilp!=sst) ilp--;
			missed=0;
			move (oldln,oldcol);
			continue;	/* next loop */
		} 
		if (c == 7) {
			move (oldln,oldcol);
			unprompt();
			beep();
			return(0);
		}
		if (c == ESC) {
goout:			unprompt();
			return(missed == 0);
		}
		if ((c == '') || (c == '')) {
			if (chr == c) {
				if (ilp == sst) {
					ilp = strcpy(sst,presst);
					continue;
				} else {
					if (missed) {
						beep();
						continue;
					}
					if (d > 0) forw(1);
					else back(1);
				}
			} else {
				missed=0;
				chr = c;
				d = -d;
			}
			continue;
		}
		if (c== '') {
			c = getchar(); /* skip all other processing */
		} else {
			if (c == '') c = '\n';
			if ((c < 040) && (c != '\n')) {
				ungetch(c);
				goto goout;
			} 
		}
		if (missed) {
			beep();
			continue;
		}
		*ilp++ = c;
	}
}

/* psrch -- print search prompt */

psrch(dir,sstp)
register int dir;
char *sstp;
{
	if (dir>0) prompt1("Search: %s",sstp);
	else prompt1("Reverse Search: %s",sstp);
}

/* handle a separator character in auto fill mode */

/* if fill mode is on, and the current position is beyond FILLCOL, a
 * newline is inserted before the last word.  Otherwise, just insert the
 * character at the current position */


afsep(count,chr)

register count;
register chr;

{
	register cc;

	if (infrn < 0) return(1);		/* no dice in macro */
	if (FILLMD && (RARE == 0)) {
		mline=0;
		findpos(clptr,column);
		if (mline || (mcol>FILLCOL)|| (column > FILLCOL)) {
			insertc(count,chr);
			back(count);
			cc = 0;
			do {
				cc++;
			} while ((cc < column) &&
				(((bits[clptr[column-cc]] & WHITE) == 0) ||
				(clptr[column-cc+1] == '.') ||
				(clptr[column-cc+1] == '\'')));
			if (cc == column) {  /* whole line is one word */
				forw(count);
				kbapp = 0;
				bdel(1);
				kpop(); /* remove junk */
				nl(1);
				return(1);
			}
			back(cc);
			kbapp = 0;
			fdel(1);
			kpop();
			if (comln == curln) {		/* if in comment */
				putin(" * ");
				back(3);
				cc += 3;
				comln = 0;
				nl(1);
				comln = curln;
			} else nl(1);
			forw(cc);
			return(1);
		}
	}
	insertc(count,chr);
	return(1);
}

/* unix escape */

char *shell = "sh";			/* name of shell processor */

ux(arg)

int arg;
{
	register char *sp;		

	do {
		sp = (getname("command line? "));
	} while (sp && 	(unx(sp,(arg != 1))== 0));
}


/* general unix escape --  */

/* flag = 0 means just run it */
/* flag = 1 means run and feed it the buffer */
/* flag = 2 means run and replace .exec with the result */
/* flag = 3 means run and append result to .exec */

unx(cmd,flag)

register char *cmd;
register int flag;
{
	struct pipes {
		int rdpipe;
		int wrpipe;
	} piped;
	FILE tinbuf;
	FILE *infile;
	int status;
	int (*istat)();
	int (*qstat)();
	register i;
	int c;
	int obuf;
	int pid;
	
	if (flag >= 2) {		/* if .exec in the business */
		obuf = curbf;		
		if (chgbuf(".exec") == 0) return(0);
	}

	if (USILENT == 0) {
		clear();
		putout("%s",cmd);
		mflush(stdout);
		istat = signal(SIGINT, SIG_IGN);
		qstat = signal(SIGQUIT, SIG_IGN);
		cook();
	}
	if (flag) pipe(&piped);		/* make pipes if we need them */
	if ((pid = fork()) == 0) {	/* child process */
		
		if (flag == 1) {
			close(0);
			dup(piped.rdpipe);
		}
		if (flag >= 2) {	/* if writing to .exec */
			close(1);
			dup(piped.wrpipe);
			close(2);
			dup(piped.wrpipe);
		} else if (USILENT) { /* silent, throw away output */
			close(1);
			open("/dev/null",2);
		}
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT,SIG_DFL);
		xclose(2);	/* close all irrelavent files */
		dup(1);		/* restore stderr */

		if (streq(cmd,shell)) execl("/bin/sh","sh","-i",0);
		else {
			execl("/bin/sh", "sh", "-c", cmd, 0);
		}
		_exit(127);
	}

/* parent process (the one that remains EMACS) */
	
	if (flag == 1) {		/* if sending the buffer */
		close(piped.rdpipe);
		infile = fdopen(&tinbuf,piped.wrpipe,"w");
		for (i = 1; i <=nlines; i++) {
			cmd = mkline(i);
			while ((c = *cmd++) != EOL) {
				putc(c,infile);
			}
			if (i != nlines) putc(EOL,infile);
		}
		mclose(infile);
	}
	if (flag >= 2) {		/* if writing */
		close(piped.wrpipe);
		infile = fdopen(&tinbuf,piped.rdpipe,"r");
		if (flag == 2) bufinit();
		else {
			bot();	/* append */
			if (column != 0) nl(1);
			putin(cmd);
			nl(2);
		}
		readsub(infile,flag-1,cmd,1-USILENT);
		chbuf(obuf);
	}

/* wait for child to finish */

	while ((i = wait(&status)) != pid && i != -1);		

	
/* child is now done, go back to normal EMACS */
	
	if (USILENT == 0) {
		uncook();
		signal(SIGINT, istat);
		signal(SIGQUIT, qstat);
		junked++;
	}
	return(contin());
}

bux(arg)

int arg;
{
	register char *sp;
	do {
		sp = (getname("command line? "));
	} while (sp && 	(unx(sp,2+(arg != 1))== 0));
}

/* query replace */
/* prompts for strings and does conditional replacement */

qrep()
{
	iqrep(0);			/* no regular expressions */
}

rqrep()
{
	iqrep(1);			/* regular expression version */
}


iqrep(regular)

int regular;
{

	char sstring[128];
	char rstring[128];
	register char *sp;
	int bsflag = 0;
	int ask = 1;
	int show = 1;
	int l = 1;
	register c;
	
	if((sp = getname("From? "))== NULL) return;
	if (*sp == NULL) sp = presst;
	strcpy(sstring,sp);
	if ((sp = getname("To? ")) == NULL) return;
	strcpy(rstring,sp);

	while (1) {
		if (regular) { 
			if (rgsrch(curln,column,sstring,0,1)== 0) break;
		} else {
			if (srch(curln,column,sstring,1)== 0) break;
		}
		if ((l == 0) && (curln == kline) && (column == kcol)) {
			error(WARN,72,sstring,rstring);
			goto rdone;
		}
		
		strcpy(presst,sp);	/* save successful search */
		move(kline,kcol);
		if (show) {
			disup();
		}
		if (ask){
			c = getchar();
		} else c = 'y';
		switch(c) {

case '':	beep();
		unprompt();		/* wipe out help message */
		return;
case 'R':	show = 0;
case 'r':	ask = 0;		/* do rest */
				/* fall through to do this one too */
case ' ':
case 'y':
case 'Y':
		kbapp = 0;
		if (regular) {
			l = (loc2-column);
		} else {
			l = (lng(sstring));
		}
		fdel(l);
		for (sp = rstring; *sp; sp++) {
			switch(*sp) {			
			case '\\':
				if (!bsflag) {
					bsflag = 1;
					continue;
				}
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
				if (bsflag && regular && regrep(*sp-'1')) {
					bsflag = 0;
					continue; /* \number style replace */
				}
/* fall through to default treatment of numbers	*/

			default:
				put(*sp);
				bsflag = 0;
				break;
			case '&':
				if (bsflag) {
					put(*sp);
					bsflag = 0;
					continue;
				} else yank(1);
			}
		}
		kpop();			/* clean up our stack */
		if (show) {
			disup();	/* before next search */
			mflush(stdout);
		}
		break;
case '':
case '':
case 'n':
case 'N':
		forw(1);
		break;
default:
		beep();
case '?':
		prompt1("Query replace -- 'y' to replace, 'n' to skip, ^G to quit,'R' or 'r' for rest");
		goback();
	}
	}
rdone:	prompt1("Replace Done");
	goback();
}
/* length of a string */

lng(sp)

char *sp;

{
	register char *spo;

	spo = sp;
	while(*spo++);
	return(spo-sp-1);
}

/* capitalize next count characters */

capnxt(count)

register count;

{
	register c;

	while (count--) {
		c = (clptr[column]);
		if ((c >= 'a') && (c <= 'z')) {
			kbapp = 0;
			fdel(1);
			kpop();
			put(c-040);	/* map down */
		} else if (forw(1)==0) break; /* check for EOF */
	}
}

/* capitalize first character of the next word */

capwrd(count)

register count;

{
	while (count--) {
		kmark();
		IGNORE(skipf(WRDSEP));
		move(kline,kcol);	/* to next word */
		capnxt(1);
		mfwrd(1);
	}
}

/* exchange current position and the marked position */

exch(mnumb)

{
	register x;
	register y;
	register MARK *markp;
	
	markp = markptr(mnumb);
	x = markp->markl;
	if (x < 1) x = 1;
	y = markp->markc;
	if ((x == curln) && (y == column)) {
		return(0);
	}
	if (x > nlines) x = nlines;
	mark(mnumb);
	mvc(x,y);
	return(1);
}

/* exchange next two chacters in the buffer and move forward one */

xpose(count)

register count;

{
	kbapp = 0;
	fdel(1);
	forw(count);
	yank(1);
	kpop();
	back(1);
}

/* changes a mode parameter.  Mode parameters are of three types,
 * string, boolean, or integer.

 * Integer modes set from the count argument, boolean modes turn on if
 * count is one, off otherwise, and string modes prompt for new value

 * chmode returns the previous value of the mode set */


chmode(count)
register count;
{
	register char *mp;
	register i;
	char mfield;
	int retval;
	
	if ((mp=getname("Mode? ")) != NULL) {
		if (*mp == '\0') {
			modisp();
			return(0);
		}
		for (i = 0; i < NMODES; i++) {
			if (streq(mdata[i].modename,mp)) {
				retval = *mdata[i].modeloc; /* old value */
				switch(mdata[i].modetype) {
				case ONOFF:
					if (count == 1) *mdata[i].modeloc = 1;
					else *mdata[i].modeloc = 0;
					break;
				case INT:
					*mdata[i].modeloc = count;
					break;
				}
			if (timemd) ticker();		/* start timing ticker */
			else alarm(0);			/* turn off if time mode is off */

			if ((mfield = mdata[i].moderset)&DSIZE) {
				SCRNLIN = SCRLINES+4;
				ECHOL = SCRNLIN-1;
				MODLN = SCRNLIN-3;
			}
			if (mfield & CSE) {
				if (NOCASE) count = 'a'-'A';
				else count = 0;
				for (i = 'A'; i <= 'Z'; i++) {
					casem[i] = i+count;
				}
			}
			if (mfield&DISPLAY) fclear(); /* force re-display */
			return(retval);
			}
		}
		IGNORE(error (WARN,45,mp));
	}
	return(0);
}

char *MDHEAD = "EMACS_MODES:";

bfmodes()				/* set modes from buffer */

{
	register char *cp;
	register char *cp1;
	char mdbuf[10];
	register i;
	int onoff;
	
	if ((nlines>0) && (srch((nlines<10) ? nlines: 10,0,MDHEAD,-1))) {

			 /* if modes set */

		for (cp = mkline(kline)+kcol+10; *cp != EOL;) {
			cp1 = mdbuf;
			while ((*cp != EOL) && (*cp != '!') && ((bits[*cp] & WRDCHR) == 0)) cp++;
			if (*cp == '!') {
				onoff = 0; 
				cp++;
			} else onoff = 1;
			
			while (bits[*cp] & WRDCHR) {
				*cp1++ = *cp++;
			}
			if (*cp == '=') {
				cp++;
				cp = nscan(cp,&onoff);
			}
			
			*cp1 = 0;
			for (i = 0; i < NMODES; i++) {
				if (streq(mdata[i].modename,mdbuf)) {
					*mdata[i].modeloc = onoff;
					goto found;
				}
			}
found:			continue;
		}
	}
}
			


/* display all active modes.  Values displayed for integer and string
 * modes, all on boolean modes are displayed */

modisp()
{
	register i;
	
	mtop();
	for (i = 0; i < NMODES; i++) {
		switch(mdata[i].modetype) {

		case ONOFF:
			if (*mdata[i].modeloc) putout("%s mode",mdata[i].modename);
			break;
		case INT:
			putout("%s = %d",mdata[i].modename,*mdata[i].modeloc);
			break;
		}
	}
	putout (endput);
	IGNORE(contin());
}

/* compare two strings */

streq(cp,cp1)

register char *cp;
register char *cp1;

{
	while (*cp) if (*cp++ != *cp1++) return(0);
	if (*cp1) return(0);
	return(1);
}

char *
strcpy(cp,cp1)

register char *cp;
register char *cp1;
{
	while (*cp++ = *cp1++);
	return(cp-1);
}

/* push the marked region onto the kill stack without killing it */


pickup(mnumb)
{
	register MARK *markp;
	

	markp = markptr(mnumb);

	if ((markp->markl < curln) || ((markp->markl == curln) && (markp->markc < column))) {
		killstk(markp->markl,markp->markc,curln,column);
	} else {
		killstk(curln,column,markp->markl,markp->markc);
	}
}

/* go to beginning of count line */

absgoto(count)
register count;
{
	move((count<nlines)?count:nlines, 0);
}

/* begin a C coment.  comment begins in COMCOL, if current position is
 * not column 0, otherwise begins in column 0.  the next newline will end
 * the coment */

cment()

{
	
	if (column) {
		disup();
		if (mcol<comcol) {
			while (mcol<comcol) {
				put('	');
				disup();
			}
		} else {
			put (' ');
		}
	}
	putin ("/* ");
	comln = curln;
}

/* adjust the indentation of the current line to be consistent with that
 * in the last line.  */

tabjust(lno)
register lno;

{
	register lln;
	register tabno;
	char *llp;
	int c;

	tabno = 0;
	for (lln = lno-1; lln > 0; lln--) {
		llp = mkline(lln);
		while (llp[tabno] == '	') ++tabno;
		if ((llp[tabno]!= EOL) && (llp[tabno] != '/')) {
			while ((c = llp[tabno])!=EOL) {
				switch(c) {
				case '{':
					++tabno;
					break;
				default:
					++llp;
				}
			}
			goto tabout;
		}
		tabno = 0;
	}
tabout:
	move(lno,0);
	while ((c = clptr[column]) == '	') {
		tabno--;
		column++;
	}
	if ((c == '}') && tabno) tabno--;
	if (tabno) {
		if (tabno>0) tabc(tabno,'	');
		else bdel(-tabno);
	} else { 
		sputl(curln,column,curln);
		move(curln,column);
	}
}

/* } handler, re-sets the indentation back one level */

cbrak(count,brace)

register count;
register brace;
{
	if (TABMD &&(RARE == 0) && column && (clptr[column-1] == '	')){
		bdel(1);
	};
	insertc(count,brace);
}


/* returns 1 if next line contains no non blank (or tab) characters */

isblank(line)

register line;

{
	register char *lp;

	if ((line <= nlines) && (ptrs[line] == 0)) return(1);
	lp = mkline(line);
	while (*lp!=EOL) {
		if ((bits[*lp++] & WHITE) == 0) return(0);
	}
	return(1);
}

/* change working dir */

cwd()
{
	char *np;
	np = expenv(getname("Directory?"));
	if (np) {
		if (*np == 0)
			np = getenv("HOME"); /* null means home */
		if(chdir(np)) IGNORE(error(WARN,errno,np));
	}
}

stats()
{
	
	mtop();
	putout ("%d chars from terminal",ninch);
	putout ("%D calls to mputc",nmput);
	putout ("%D chars written by mputc",noutc);
	putout ("%d terminal writes", ntwrite);
	putout ("%d calls to makeline",nmkline);
	putout ("%d buffer reads",nbread);
	putout ("%d file writes",nbwrite);
	putout ("%d file seeks", nbseek);
	putout ("%d characters of buffer left", (NBLOCK*BLEN)-macptr);
	IGNORE(contin());
}

uline(count)

register count;

{
	while (count--) {
		kmark();
		IGNORE(skipf(WRDSEP));	/* skip to begginning of word */
		move(kline,kcol);
		while (bits[*klptr] & WRDCHR) {
			put('_');
			put('');
			forw(1);
		}
	}
}

inpsh()
{
	register char *fp;
	
	if (fp=expenv(getname("Input file? "))) {
		if (pushin(fp)) {
			edit(1);
			inpop();
		}
		else IGNORE(error(WARN,errno,fp));
	}
}

/* send the contents of the buffer as mail */

mailit()
{
	char cmdbuf[256];
	register char *mp;
	register char *mp1;
	char *mlhead;

	mp = strcpy(cmdbuf,"mail");

	mlhead = "TO:  CC:  ";
	while (*mlhead) {
		top();
		mlhead[4] = 0;
		while(srch(curln,column,mlhead,1) != 0) {
			move(kline,kcol+3);
			if (kcol != 0) continue;
			if (((mp-cmdbuf)+leng(curln)-column) > 250) {
				IGNORE(unx(cmdbuf,1)); /* partial list */
				mp = strcpy(cmdbuf,"mail");
				move(curln,column);
			}
			mp1 = clptr+column;
			while (*mp1 != EOL) *mp++ = *mp1++; /* copy mailing list */
			*mp = 0;
		}
		mlhead += 5;
	}
	if (mp-cmdbuf < 6) {
		IGNORE(error (WARN,46));
		return;
	}
	IGNORE(unx(cmdbuf,1));		/* send to mail command */
}

/* contin -- ask user to continue */


contin()
{
	register c;
	if (infrn < 0) return(1);	/* continue always in a macro */
	prompt1("Continue?");
 	c = getchar();
	prompt1("");
	if ((c == 'y') || (c == ' ') || (c == '\n') || (c == '\015')) return(1);
	else return(0);
}

/* buffer length and current position */


/* If invoked from tty, prints buffer status info on the terminal.  If */
/* invoked from a macro, returns a value dependent on its argument */

#define VLIN 0				/* return line number */
#define VCOL 1				/* return column number */
#define VDLIN 2				/* display line */
#define VDCOL 3				/* display column */


buflng(arg)

int arg;
{
	long flng;
	long fpos;	
	char xbuf[128];
	register i;
	if (infrn < 0) {
				/* If from macro, return useful info */
		switch(arg) {
			
		case VCOL: return(column);
		case VLIN: return(curln);
		case VDCOL:
		case VDLIN:
			if (findline(curln) == 0) return(-1);
			mline=nln;
			if (findpos(clptr,column) == 0) return(-1);
			if (arg == VDLIN) return(nln);
			else return(mcol);
		}
	}
	flng = 0L;
	for (i = 1; i <= nlines; i++) {
		if (i != 1) flng++;	/* linefeed from last line */
		if (i == curln) fpos = flng + column;
		flng += leng(i);
	}
	clptr=mkline(curln);
	seprintf(xbuf,"next char: %o, line: %d/%d:  pos:  %D/%D column: %d",
		clptr[column],curln,nlines-1,fpos,flng,column);
	prompt1(xbuf);
	return(0);
}

filler()
{
	register i;
	register char *cp;
	register j;
	
	for (i = 1; i <= nlines; ) {
		cp = mkline(i);
		if ((*cp == '.') || (*cp == '\'') || (*cp == EOL)) {
			++i;
			goto fdone;
		}
		if ((j =leng(i)) <= FILLCOL) {
			if (i >= nlines) return;
			++i;
			cp = mkline(i);
			if ((*cp == '.') || (*cp == '\'') || (*cp == EOL)) {
				++i;
				goto fdone;
			} else {
				move(i--,0);
				bdel(1);
				put(' ');
				goto fdone;
			}
		} else {
			for (j = FILLCOL; (j> 0) &&
				(((bits[cp[j]] & WHITE) ==0) ||
				(cp[j+1] == '.') || (cp[j+1] == '\''));j--);
			if (j > 0) {
				move(i,j);
				fdel(1);
				openl(1);
				forw(1);
				while (*clptr == ' ') fdel(1); /* remove spaces */

			}
			i++;
		}
	fdone: continue;
	}
}

/* macro the region */

macro(arg)
int arg;
{


	register mychar;	
	register balance;
	register int machr;
	int compos;
	extern beep();
	
	if (arg != 1) {
		for (mychar = 0; mychar < NCHARS; mychar++) {
			if (((int) (do_it[mychar])&01) && (((int)do_it[mychar]) < ICHAR)) {
				do_it[mychar] = (int *) beep;
				helpc[mychar] = 0;
			}
		}
		nmac = 0;
		macptr = 0;
		fbkno = 0;
	}
	top();
		
	while (curln<nlines) {

		balance = 0;
		machr = clptr[column]&0377;
		if (machr == '') machr = (clptr[++column]&0377)+0200;
		if (machr == '') {
			machr = (clptr[++column]&0377)+0400;
		}
		forw(1);
		macname[nmac++] = compos = macptr;
		if (clptr[column] == '') { /* if comment */
			++column;
			while (clptr[column] != EOL) {
				pshchr(clptr[column++]);
			}
			pshchr(0);
			forw(1);	/* skip newline */
		}
		if (nmac >= NMAC) nmac--;

		do_it[machr] = (int *)((macptr*2)+1); /* command ppointer */
		helpc[machr] = macptr-compos;

		while (curln < nlines) {
			mychar = clptr[column]&0377;
			if (mychar != '') {
				if (mychar == MTA({)) balance++;
				else if (mychar == MTA(})) balance--;
				pshchr(mychar);
				forw(1);
				if ((mychar == '') && (clptr[column] == EOL)) {
					if (balance) {
						error(WARN,47,curln);
					}
					goto macdone; /* end of macro definition */
				}
			} else {
				move(curln+1,0); /* skip comment */
			}
		}
		error (WARN,48);
		pshchr('');		/* escape from bad macro */

macdone:	forw(1);		/* on to next macro */
	}
}

ldmac(arg)
int arg;
{
	register int oldbuf;
	register int tmpbuf;
	
	oldbuf = curbf;
	chgbuf("...");			/* temporary buffer */
	if (fred(1)) {			/* read file */
		macro(arg);
	}
	tmpbuf = curbf;
	chbuf(oldbuf);
	klbfr(tmpbuf);			/* kill temp buffer */
}
/* macro programming commands */

/* these commands perform various utility functions for macro commands */

bfchr()					/* get a character from the buffer */
{
	return(clptr[column]);
}

litchr()				/* literal character */

{
	return(getchar());
}

compar(ctype)				/* general comparison */

/* comparison type definitions (pecular arangement for convenient */
/* specification via ^U */


#define CLE 0				/* <= */
#define CEQ 1				/* = */
#define CLS 2				/* < */
#define CGT 3				/* > */
#define CNE 4				/* != */
#define CGE 5				/* >= */
#define COR 6				/* OR */
#define CAND 7				/* AND */
#define CNOT 8				/* NOT */
#define CFALSE 9			/* FALSE */
#define CTRUE 10			/* TRUE */
#define CPLUS 11			/* arg1 + arg2 */
#define CMINUS 12			/* arg1 - arg2 */
#define CTIMES 13			/* arg1 * arg2 */
#define CDIV 14				/* arg1 / arg2 */
#define CREM 15				/* arg1 % arg2 */

register int ctype;
{
	register int r1;
	register int r2;
	
	if (ctype == CFALSE) return(0);
	if (ctype == CTRUE) return(1);

	r1 = edit(0);			/* first arg */

	if (ctype == CNOT) return(r1 == 0);
	
	r2 = edit(0);			/* second arg */
	
	switch(ctype) {
		
	case CLE: return(r1<=r2);
	case CEQ: return(r1==r2);
	case CLS: return(r1<r2);
	case CGT: return(r1>r2);
	case CNE: return(r1!=r2);
	case CGE: return(r1>=r2);
	case CAND: return(r1 && r2);
	case COR: return(r1 || r2);
	case CPLUS: return(r1+r2);
	case CMINUS: return(r1-r2);
	case CTIMES: return (r1*r2);
	case CDIV: return(r1/r2);
	case CREM: return(r1%r2);
default:return(0);
	}
}


pscan(lev)				/* parenthesis scan */

int lev;				/* returns when lev+paren level = 0 */
{
	register int c;
	
	while (lev) {
		c = getchar();
		
		if ((c == ('')) || (c == MTA(q)) || (c == MTA())) {
			c = getchar();
			continue;
		}
		if (c == MTA({)) lev++;
		else if (c == MTA(})) lev--;
	}
}


cond(arg)					/* conditional execute */

int arg;
{
	register int c;
	
	if ((c = getchar()) != MTA({)) {
					/* must be M-{ */
		return(error(NORM,49));
	}
	while ((c = getchar()) != MTA(})) {
		while ((c != MTA({)) && (c !=MTA(}))) c = getchar();
		if (c == MTA(})) return(0); /* failed */
		if (edit(0)) {
			c = edit(1); /* scan command */
			pscan(1);	/* skip to end of cond */
			return(c);
		} else pscan(1);	/* skip to end of block */
	}
	return(0);			/* cond failed */
}

/* macro case statement:  syntax is ^x!M-{<expr>
 *					M-{<char><sequence>M-}
 *					...
 *				      M-}
 */


mcase(arg)
{
	register cchar;
	register c;
	
	while ((cchar =getchar()) != MTA({));
	cchar = edit(0);		/* match expression */
	
	while ((c = getchar()) != MTA(})) {
		while ((c != MTA({)) && (c !=MTA(}))) c = getchar();
		if (c == MTA(})) return(0); /* failed */
		c = getchar();
		if ((c == cchar) || (c == 0377)) {
			c = edit(1); /* scan command */
			pscan(1);	/* skip to end of case */
			return(c);
		} else pscan(1);	/* skip to end of block */
	}
	return(0);			/* case failed */
}
	
/* mbegin -- begin block in macro */

mbegin(arg)
int arg;
{
	return(edit(1));		/* push command */
}

/* iteration handler */

iter()
{
	register c;
	register char *savptr;
	
	while ((c =getchar()) != MTA({)) {
		if ((c != '	') && (c != ' ') && (c != EOL)) {
			return(error (NORM,50));
		}
	}
	savptr = inget();
	while (edit(0)) {		/* test condition */
		
		c = edit(1);		/* execute body */
		
		inset(savptr);		/* backup */
		
	}
	pscan(1);			/* skip body */
	return(c);
}

/* gparam -- get a parameter and push into the kill stack */

/* with an argument of 1, it takes the parameter in line, with */
/* other arguments, it takes the parameter from the tty, prompting with */
/* the in line parameter4 */


gparam(arg)				 /* get a parameter */

int arg;				/* with 1 arg, from macro text, else from tty */

{
	char pbuf[128];
	register char *pb;
	register pbch;
	
	for (pb = pbuf;;) {
		*pb++ = pbch = getchar();
		if (pbch == '') {
			--pb;
			*pb++ = getchar();
		}
		if (pbch == EOL) break;
	}
	*(--pb) = 0;			/* pickup prompt message */
	if (arg != 1)	{
		pushin(NULL);
		if (arg <= 0) {
					/* single character */
			prompt1(pbuf);
			if (arg < 0) {
				goback();
				pbch = getchar();
				unprompt();
			}
			inpop();
			return(pbch);
		}
		pb = getname(pbuf);
		if (pb == NULL) pb = "";
		inpop();
	} else pb = pbuf;
	stkstr(pb);
	return(lng(pb));
}


char *
maclook(mp)
char *mp;
{
	register int i;
	register char *mp1;
	register char *mp2;
	
	
/* now look up macro name */
	
	for (i = 0; i < nmac; i++) {
		for (mp1 = &(bbuf[0][macname[i]]), mp2 = mp; *mp1++ == *mp2++;) {
			if ((*mp1 == ' ') || (*mp2 == 0)) {
				while (*mp1++);
				return(mp1);
			}
		}
	}
	return(NULL);
}

/* macro call by name */


macex(arg,ivchar)

int arg;
int ivchar;
{
	char mbuf[100];
	register char *mp1;
	register char *mp3;
	register c;
	
	mp1 = mbuf;
	if (infrn < 0) {
		while ((c = getchar()) != EOL) *mp1++ = c;
		*mp1 = 0;
		mp3 = mbuf;
	} else {
		mp3 = getname ("Macro name? ");
	}
	if (mp3 == NULL) return(0);	/* aborted */
	if (mp1 = maclook(mp3)) {
		return(xmac(mp1-bbuf[0],arg,ivchar));
	} else {
		error (WARN,51,mp3);
		return(NULL);
	}
}

/* xmac -- execute a macro */


xmac(macp,arg,ivchar)

register int macp;
register int arg;
int ivchar;
{
	int mvars[NMVAR];		/* macro variables */
	int *omarg;			/* old pointer to mvars */
	
	omarg = marg;
	marg = mvars;
	mvars[1]=arg;			/* record macro argument */
	mvars[0] = ivchar;		/* invoking character */
	pshmac(macp);
	arg = edit(1);
	marg = omarg;
	inpop();
	return(arg);
}

/* setvar -- set a local variable */

setvar(arg)
register int arg;
{
	register int oldvar;

	if ((arg <NMVAR) && (arg >0)) {
		oldvar = marg[arg];
		marg[arg] = edit(0); /* set new value */
		return(oldvar);
	} else {
		beep();
		return(0);
	}
}

/* string compare */

/* same argument conventions as other comparisons */

cmpst(arg)

register int arg;
{
	char buf[256];
	
	register char *bp1;
	register char *bp2;
	
	bp1 = getname("");		/* get first string */
	
	strcpy(buf,bp1);
	
	bp2 = getname("");
	bp1 = buf;
	
	while ((*bp1 == *bp2) && *bp1) {
		bp1++;
		bp2++;
	}
	switch (arg) {
		
	case CLE: return(*bp1<=*bp2);
	case CEQ: return(*bp1==*bp2);
	case CLS: return(*bp1<*bp2);
	case CGT: return(*bp1>*bp2);
	case CNE: return(*bp1!=*bp2);
	case CGE: return(*bp1>=*bp2);
	default:  return(0);
	}
}


/* envexp -- expand environment variables in a string */

/* expansion takes place in the caller's string buffer */
/* pointer is returned for convenience only */

char *
expenv(str)
register char *str;
{
	char strtemp[128];
	char vartemp [64];
	register char *cp1;
	char *cp2;
	register c;
	
	if (str == NULL) return(NULL);
	strcpy(strtemp,str);		/* copy to temp */
	cp1 = strtemp;
	str = fnbuf;			/* always copy back into file name */
	while (c = *cp1++) {
		if (c == '$') {
			cp2 = vartemp;
			while (bits[c=((*cp1++)&0377)]&WRDCHR) {
				*cp2++ = c;
			}
			cp1--;		/* backspace pointer */
			*cp2 = 0;
			if (cp2 = getenv(vartemp)) {
				str = strcpy(str,cp2);
			} else {
				*str++='$';
				str = strcpy(str,vartemp);
			}
		} else {
			*str++ = c;
		}
	}
	*str++ = 0;
	return(fnbuf);
}
