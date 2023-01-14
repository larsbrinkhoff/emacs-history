/* EMACS_MODES: c, !fill */
#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_cmds.h"
#include <signal.h>
#ifdef ux3
#include <fcntl.h>
#endif

/* lext -- extend line up to position if necessary */

lext(line,col)
/* Keywords: picture-mode movement insertion */
{
	mvc (line,col);
	if (column<col) insertc(col-column,' ');
}

dput(c)
/* Keywords: insertion modes:10 overwrite-mode */
{
	if (NODEL) insertc(1,c);
	else put(c);
}

/* move forward COUNT characters */
/* Keywords: movement commands forwards character-at-a-time picture-mode:20 */
forw(count)

register int count;
{
	register int retval;

	if (PICMODE) {
		if (count>0) {
			lext(curln,count+column);
			return(1);
		} else {
			column += count;
			if (column<0) {
				beep();
				column=0;
				return(0);
			} else return(1);
		}
	}
	if (count < 0) {
		retval = findb(-count);
	} else {
		retval = findf(count);
	}
	if (retval == 0) beep(); /* couldn't go all the way */
	move(kline,kcol);
	return(retval);
}

/* backward COUNT characters */

back(count)
/* Keywords: movement commands forwards character-at-a-time */
register int count;

{
	forw(-count);			/* do it this way */
}

/* move to previous line, same collumn */

upl (count) 

/* Keywords: commands backwards:30 upward-movement movement text-lines */
register int count;

{

	if (curln-count < 1) {
		if (count == 1) {
			beep();
			return(0);
		} else curln = 1;
	} else curln -= count;
	if (PICMODE) lext(curln,column);
	else mvc(curln,column);
	return(1);
}
/* move down one line, same column */

downl (count)
/* Keywords: commands forwards:30 downward-movement movement text-lines */
register int count;

{
register int retval;

	if (((curln += count) > nlines)&& (PICMODE == 0)) {
		curln = nlines+NLRUN;
		beep();
		retval = 0;
	} else retval = 1;
	if (PICMODE) lext(curln,column);
	else mvc(curln,column);
	return(retval);
}


/* abort EMACS */

int aborts = 0;
eabort(sig)
int sig;
{
/* Keywords: internal-errors unix-interface commands */
	if (sig){
		signal(sig,SIG_DFL); /* Prevent looping */
	}
	signal (SIGIOT,SIG_DFL);
	cook();
	if (aborts>3) exit(-1);
	aborts++;
#ifdef MINFILES
	rmtemp();
#endif
#ifdef PC
	rmtemp();
	exit(-1);
#else
#ifdef bsd
	sigsetmask(0);			/* ARGH, set mask to allow abort! */
#endif
	abort();
	exit(-1);
#endif PC
}


/* exit EMACS */


quit()

{
/* Keywords: commands exit-processing user-interface:20 unix-interface:30 */
	clear();
	cook();
	statout();
#ifdef MINFILES
	rmtemp();			/* flush temp files */
#endif
#ifdef PC
	rmtemp();
#else
	flushproc();
#endif
	exit(0);
}

/* exit EMACS gracefully */

gquit()
/* Keywords: commands exit-processing user-interface:20 unix-interface:30 macro-hooks:10 */
{
	if (hooks[Exit_Emacs_Hook]) if (hook(Exit_Emacs_Hook) == 0) return;
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

register int count;

/* Keywords: commands deletion killstack:10 text-lines */

{
	register int l;
	int opic,onodel;
	
	if (numarg == 0) {
		if ((l=leng(curln)) > column) {
			return(delc(l-column));
		}
	}
	opic = PICMODE;
	onodel =NODEL;
	PICMODE=NODEL=0;		/* Allow line kills! */
 
	kline = curln+count;
	kcol = 0;
	l= tkill();
	PICMODE=opic;
	NODEL=onodel;
	return(l);
}

/* goto beginning of current line */

begin()
/* Keywords: text-lines commands movement backwards */
{
	move(curln,0);
}

/* goto end of line */

endl()
/* Keywords: text-lines commands movement backwards */
{
	mvc (curln,10000);		/* mvc will adjust line length */
}

mquote(arg)
int arg;
/* Keywords: quoting commands insertion */
{
	quote(arg,0200);
}
rquote(arg)
int arg;
/* Keywords: quoting commands insertion */
{
	quote(arg,0);
}
	
/* insert the next character, whatever it is */

/* note that newlines inserted ths way act just like unquoted newlines */

quote(count,metf)

register int metf;
register int count;

{
/* Keywords: quoting commands insertion */
	register int c;

	while(count--) {
		if ((VERBOSE)&& (MOREIN == 0)) prompt1("%d ^Q: ",count+1);
		
		c = getchar();
		c = c | metf;
		insertc(1,c);
		if (VERBOSE && (MOREIN == 0)) {
			unprompt();
		}
		disup();
	}
}

/* numchar -- convert argument to a character to insert */

numchar(count)
/* Keywords: quoting commands insertion argument-processing */
register int count;
{
	insertc(1,count);		/* insert the count */
}


/* deletes count characters going forward */

fdel(count)
/* Keywords: commands deletion forwards character-at-a-time */
{
	IGNORE(findf(count));
	return(tkill());
}

/* deletes count characters going backward */

bdel(count)

register int count;
/* Keywords: commands deletion backwards character-at-a-time */
{
	IGNORE(findb(count));
	return(tkill());
}

/* file write command */


int fright(arg)

int arg;
{
	register char *np;
/* Keywords: files commands buffers:20 filenames:10 writing */
	if ((np = expenv(getname("Write file? "))) != NULL) {
		return(wout(np,arg));
	}
	return(0);
}

/* fred -- read a file */

fred(arg)

int arg;
{
/* Keywords: files commands buffers:20 filenames:10 reading */
	
	register char *np;

	if ((np = expenv(getname("Read File? "))) != NULL) {
		return(readin(np,arg));
	} else return(0);
}

/* forward words -- leaves kline, kcol at spot that is count words
 *forward */


wordf(count)

register int count;

{
	kmark();

/* Keywords: commands:10 forwards movement:50 deletion:50 word-oriented-commands */
	
	while (count--) {
		if (skipf(WRDSEP) || skipf(WRDCHR)) {
			return;
		}
	}
}

/* skips kline, kcol forward until a character without type bit on is
 * found */


skipf(bit)

register int bit;
/* Keywords: forwards sentence-commands word-oriented-commands movement:20 commands:10 */
{
	while (bits[*klptr] & bit) {
		if (mfk()) return(1);
	}
	return(0);
}


/* move forward count words */

mfwrd(count)
/* Keywords: commands forwards movement word-oriented-commands */
register int count;

{

	wordf(count);
	move(kline,kcol);
}

/* kill next count words */



kfwrd(count)
/* Keywords: commands forwards deletion word-oriented-commands */
register int count;

{
	wordf(count);
	return(tkill());
}

/* skip kline, kcol back until a character without type bit is found */



skipb(bit)

register int bit;
/* Keywords: sentence-commands word-oriented-commands movement:20 backwards commands:10 */
{
	do {
		if (mbk()) return(1);
	} while (bits[*klptr] &bit);
	return(0);
}

/* backward count words, leaves pointer in kline, kcol */

wordb(count)

register int count;
/* Keywords: commands:10 backwards movement:10 deletion:20 word-oriented-commands */
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

register int count;
/* Keywords: commands backwards movement word-oriented-commands */
{
	wordb(count);
	move(kline,kcol);
}

/* kill back count words */

kbwrd(count)


register int count;
/* Keywords: commands backwards deletion word-oriented-commands */
{
	wordb(count);
	return(tkill());
}

/* kmark -- set kline,kcol and klptr */

kmark()

/* Keywords: commands:5 movement:50 deletion:50 character-at-a-time:20 word-oriented-commands:20 sentence-commands:20 */

{
	kline = curln;
	kcol = column;
	klptr = mkline(kline)+kcol;
}


/* move kline,kcol forward one */

mfk()
/* Keywords: commands:5 movement:50 deletion:50 character-at-a-time:20 word-oriented-commands:20 sentence-commands:20 forwards */
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
/* Keywords: commands:5 movement:50 deletion:50 character-at-a-time:20 word-oriented-commands:20 sentence-commands:20 backwards */
{
	if ((kcol--)>0) {
		klptr--;
		
		return(0);
	}
	if (kline>1) {
		--kline;
		kcol = leng(kline);
		klptr = mkline(kline) +	kcol;
		return(0);
	}
	kcol = 0;
	return(1);
}

	/* forward to end of sentence */

	/* leaves resulting pointer in kline, kcol */

fsent () 

/* Keywords: commands:10 forwards movement:40 sentence-commands */

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

/* Keywords: commands:10 backwards movement:40 sentence-commands */

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
register int count;

/* Keywords: commands backwards movement sentence-commands */

{
	while (count--) {
		if(bsent()) move(kline,kcol);
	}
}

/* move forward count sentences */


esent(count)
register int count;

/* Keywords: commands forwards movement sentence-commands */

{
	while (count--) {
		if (fsent() == 0) break;
		move(kline,kcol+1);
	}
}


/* move to top of file */

top()
/* Keywords: files:20 buffers:20 movement upward-movement commands */
{
	move(1,0);
}


/* move to end of file */

bot()

/* Keywords: files:20 buffers:20 movement downward-movement commands */

{
	mvc(nlines,10000);
}

/* new line handler */

/* First, finishes off comment on the current line (if any) */

/* Next, if the current buffer has a sub-process, the line is sent to the sub-process */

/* Then, if the next line is non-empty, it creates an empty next line */
/* next, the next line is tab adjusted (if in C mode ) */

/* now, any text on the current line beyond the current position, it is
 * moved to the end of the next line */

/* finally, the pointer is moved to the (tab adjusted) start of the next
 * line */

/* if count > 1, then count-1 blank lines will be inserted in between
 * the current line and the 'next' line */

nl(count)

/* Keywords: commands C-mode:30 insertion text-lines comments:50 shell-escape:10 */



register int count;

{

	if (RARE == 0) {
		if (comln == curln) putin(" */");
#ifndef PC
		if (curbf == procbuf){
			MARK *mp;
		
			mp = markptr(curbf);
			if ((mp->markl == curln) && (mp->markc <= column)) {

/* If this is the last line on which output was done from */
/* the sub-process, then send from mark and re-mark */
			
				sendproc (clptr+mp->markc, column+1-mp->markc);
				mp->markl = curln+1;
				mp->markc = 0;
			} else {
				sendproc(clptr,column+1);
			}
		}
#endif
	}
	comln = 0;
	if ((count != 1) || (NLINS) || (isblank(curln+1)==0) || (clptr[column] != EOL)) {
		openl(count);
	}
	if (TABMD&& (RARE == 0)) tabjust(curln+count);
	else move(curln+count,0);
}

/* sets mark at current position */

mark(mnumb)

/* Keywords: marking regions commands */

{
	MARK *markp;
	markp = markptr(mnumb);
	markp->markl = curln;
	markp->markc = column;
}

/* returns mark pointer */

MARK *markptr(mnumb)

register int  mnumb;

/* Keywords: commands:10 marking:50 regions deletion:50 movement:50 */
{
	
	if (mnumb >= NMARKS) {
		error(WARN,44);
		mnumb = curbf;
	}
	if ((mnumb == 1) && (numarg == 0)) mnumb = curbf;
	return(&marks[mnumb]);
}


/* kills text between current position and mark position */


mkill(mnumb)
int mnumb;
/* Keywords: commands deletion regions */
{
	register MARK *markp;
	
	markp = markptr(mnumb);
	kline = markp->markl;
	if (kline < 1) kline = 1;
	if (kline > nlines) kline = nlines;
	kcol = leng(kline);
	if (markp->markc < kcol) kcol = markp->markc;
	if (PICMODE) {
					/* This pain is to get the
					 * mark to work  in picture
					 * mode so that everything
					 * up to and including the
					 * marked position is killed */
		if (kcol > column) {
			kcol++;
		} else {
			forw(1);
		}
	}
	return(tkill());
}

/* retrieves text from the kill stack and inserts it (count times ) */


int yline;
int ycol;

yank(count)

/* Keywords: regions commands insertion killstack retrieval popping:10 */

register int count;
{
	register int result;
	register int unp;
	
	unp = unstart();
	mark(curbf);
	while (count--) {
		result = retrv();
	}
	unend(unp);
	yline = curln;
	ycol = column;			/* Save, so we know when not to re-yank */

	return(result);
}

/* kills the marked region, removes the top item from the kill stack,
 * and then inserts the next area from the kill stack */


reyank(count)		/* yank again */
register int count;
/* Keywords: regions commands insertion killstack retrieval deletion popping:10 */
{
	register int unp;

	if ((curln != yline) || (column != ycol)) {
		error (WARN,84);	/* Prevent some nastyness */
		return;
	} 
	kbapp = 0;
	unp = unstart();
	mkill(curbf);	/* flush last insertion */
	kpop();		/* first pop the mkill */
	kpop();		/* now pop that last insertion */
	yank(count);
	unend(unp);
}

/* search subroutine */

/* searches forward or backward for a string, starting at the 
 * given position.  If the string is found, 1 is returned, and the start
 * of the match is left in kline, kcol.  Return of 0 indicates no match */

/* Keywords: searching */

srch(sline,scol,sp,direct)

int sline;
int scol;
char *sp;
int direct;

{
	register char *lp;
	register char *cp;
	register int l;
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

again:		if (brkflg) brkit();
		lp = mkline(sline)+scol;
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

fisrch(arg)
int arg;
/* Keywords: searching commands forwards key-bindings:10 */
{
	return(isrch(1,arg));
}
risrch(arg)
int arg;
/* Keywords: searching commands backwards key-bindings:10 */

{
	return(isrch(-1,arg));
}
/* incremental search command (either direction) */


isrch(d,arg)
int d;
int arg;

/* Keywords: commands:80 searching user-interface:60 key-bindings:50 */


{
	int oldln;
	int oldcol;
	register char *ilp;
	register int c;
	int lx;
	int ly;
	int nd;
	int missed;
	char sst[40];
	char *xp;

	ilp = sst;
	missed = 0;
	lx = oldln = curln;
	ly = oldcol = column;
	if (infrn < 0) {			/* if in a macro */
		xp = getname(NULL);		/* search argument */
		ilp = mstrcpy(ilp,xp);
	}
	while (1) {
		*ilp = 0;
		if (missed == 0) {
			psrch(d,sst);		/* prompt */
			if (infrn == 0) mflush(stdout);
			if (srch(curln,column,sst,d) == NULL) {
				move(lx,ly);
				if (infrn >= 0) {
					beep();
					prompt1("Failing Search: %s", sst);
				}
				missed = 1;
			} else {
				move(lx = kline,ly = kcol);
				if (ilp!=sst)strcpy (presst,sst);
				missed = 0;
			}
		}
		if (infrn < 0) return(missed == 0); /* macro invocation */
		disup();
		if ((missed == 0) && (incnt == 0)) {
			psrch(d,sst); /* restore prompt */
			mgo(nln,ncol); /* goback to display position */
		}
		c = getchar();
		if ((c == CTRLH)||(c == RUBOUT)) {
			if (ilp!=sst) ilp--;
			missed=0;
			move (oldln,oldcol);
			continue;	/* next loop */
		} 
		if (c == CTRLG) {
			move (oldln,oldcol);
			unprompt();
			beep();
			return(0);
		}
		if (srch_nl?((c == '\n') || (c == '\r')) : (c == ESC)) {
goout:			unprompt();
			return(missed == 0);
		}
		if (nd=issrch(c)) {
			if (nd == d) {
				if (ilp == sst) {
					ilp = mstrcpy(sst,presst);
					continue;
				} else {
					if (missed) {
						beep();
						continue;
					}
					forw(d);
				}
			} else {
				missed=0;
				d = nd;
			}
			continue;
		}
		if (isquote(c)) {
			c = getchar(); /* skip all other processing */
		} else {
			if (c == CTRLM) c = '\n';
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

/* Keywords: mode-line prompting searching */

{
	if (dir>0) prompt1("Search: %s",sstp);
	else prompt1("Reverse Search: %s",sstp);
}

/* handle a separator character in auto fill mode */

/* if fill mode is on, and the current position is beyond FILLCOL, a
 * newline is inserted before the last word.  Otherwise, just insert the
 * character at the current position */


afsep(count,chr)

int count;
int chr;

/* Keywords: commands:40 modes:30 text-filling comments:20 C-mode:10 */

{
	register int cc;
	int newcol;
	int oldcol;
	char lbuf[MAXEL];		/* KLUDGE! */
	register char *p1;
	register char *p2;
	
/* whitespace characters in a macro expansion must be quoted to be
 * self-inserting.  The following checks for non-zero character
 * count, input from macro, and a whitespace character to be
 * inserted.  Under these conditions, no inserting is done. */
	
	if (count && (infrn < 0)&& (WHITE&bits[chr])) return(1);


	
	if (FILLMD && (RARE == 0)) {
		insertc(count,chr);
		oldcol = column;
		cc = leng(curln) - column;
		mline=0;
		findpos(clptr,column);
		if (mline || (mcol>FILLCOL)|| (column > FILLCOL)) {

again:			newcol = column;
			
			do {
				column--;
			} while ((column) &&
				(((bits[clptr[column]] & WHITE) == 0) ||
				(clptr[column+1] == '.') ||
				(clptr[column+1] == '\'')));
			mline=0;
			findpos(clptr,column);
			if (mline || (mcol>FILLCOL)|| (column > FILLCOL)) goto again;



/* Now then, do the work.  We are sitting at the right column to break the line */
/* First, save the rest of the line.  Then, kill it, then, do a newline,  */
/* now, bring it back */
			
			
			if (column == 0) column = newcol; /* 1 word line */
			if (column == oldcol) return(1); /* Do absolutely nothing */
			p1 = clptr+column+1; /* Eat the whitespace character */
			
			p2 = lbuf;
			while ((*p2++= *p1++)!=EOL); /* copy line */
			sputl(curln,column,curln); /* Flag line is bad! */
			clptr[column]=EOL; /* truncate line */
			if (comln == curln) {
				comln = -1;
				nl(1);
				cment(1);
				*(clptr+column-3)=' ';
			} else nl(1);
			p1 = lbuf;
			while (*p1 != EOL) put(*p1++); /* restore line */
			forw(-cc);   /* go back to right place. */
		}
		return(1);
	}
	insertc(count,chr);
	return(1);
}

/* unix escape */

char *shell = "sh";			/* name of shell processor */

ux(arg)

int arg;
{
/* Keywords: commands buffers:20 unix-interface shell-escape */

	register char *sp;		

	if (SAVEMD) fsave(0);
	do {
		sp = (getname("command line? "));
	} while (sp && 	(unx(sp,(arg != 1))== 0));
	return(cstatus);
}

#ifndef PC

char *
nvmatch(s1, s2)
register char *s1, *s2;

/* Keywords: unix-interface shell-escape environment-variables */
{
	while(*s1 == *s2++)
		if(*s1++ == '=')
			return(s2);
	if(*s1 == '\0' && *(s2-1) == '=')
		return(s2);
	return(NULL);
}

/* Environment fixer-upper, adds definition of filename to the
 * environment and returns a pointer to it. */

extern char **environ;

char **
fixenv(defp)
register char *defp;
/* Keywords: unix-interface shell-escape environment-variables */

{
	register char **xp;
	register char **yp;
	int envd = 0;
	
	xp =((char **)  &bbuf[0][0]);		/* Overlay screen */
	yp = environ;
	
	while (*xp = *yp++) {
		if (nvmatch(defp,*xp++)) {
			xp[-1] = defp;
			envd=1;
		}
	}
	if (envd==0) {
		*xp++ = defp;
		*xp = NULL;
	}
	xp =((char **)  &bbuf[0][0]);		/* Overlay screen */
	return(xp);
}
#endif


/* general unix escape --  */

/* flag = 0 means just run it */
/* flag = 1 means run and feed it the buffer */
/* flag = 2 means run and replace .exec with the result */
/* flag = 3 means run and append result to .exec */
/* flag = 4 means run and bring results back in fnbuf */
/* flag = 5 means run and hook up input and output pipes */
/* flag = 6 means run it and hook up output to splfile, leave input in splfile */
/* flag = 7 means run it and hook up input from splfile, leave output in splfile */

#define UNEEDSIN 1
#define UNEEDSOUT 2
#define UDOTEXEC 4
#define UNOSCREEN 8
#define URETBUF 16
#define UPROCESS 32
#define USENDBUF 64
#define UREADBUF 128
#define UINITBUF 256
#define USPLIN 512
#define USPLOUT 1024

int unxflags[8] = {
	0,
	UNEEDSIN+USENDBUF,
	UNEEDSOUT+UDOTEXEC+UREADBUF+UINITBUF,
	UNEEDSOUT+UDOTEXEC+UREADBUF,
	UNEEDSOUT+UNOSCREEN+URETBUF,
	UNEEDSIN+UNEEDSOUT+UNOSCREEN+UPROCESS,
	UNEEDSIN+USPLOUT+UNOSCREEN,
	UNEEDSOUT+USPLIN+UNOSCREEN
};


unx(cmd,flag)
/* Keywords: commands:80 buffers:10 files:10 environment-variables:20 unix-interface shell-escape dired:10 sub-processes:40 encryption:20 */
register char *cmd;
register int flag;

{
#ifdef PC
	return(1);			/* Don't do this, just exit */
#else
	struct pipes {
		int rdpipe;
		int wrpipe;
	} piped1,piped2;
	FILE tinbuf[1];
	FILE *infile;
	int (*istat)();
	int (*qstat)();
	register int i;
	int c;
	int obuf;
	int pid;
	char **evp;
	char *obname;
	char evbuf[256];
	char *myshell;
	
	if (*cmd == 0) return(1);	/* Don't even bother with null commands */
	obname=fname();
	cstatus  = -1;			/* start off bad */
	flag = unxflags[flag];		/* Translate to bit-coded value */
	
	if ((flag & UPROCESS) && procpid) flushproc(); /* Zap old process */

	if (flag&UDOTEXEC) {		/* if .exec in the business */
		obuf = curbf;		
		if (chgbuf(".exec") == 0) return(0);
	}

	if ((USILENT == 0) && ((flag&UNOSCREEN) == 0)) {
		clear();
		putout("%s",cmd);
		istat = signal(SIGINT, SIG_IGN);
		qstat = signal(SIGQUIT, SIG_IGN);
		cook();
	}
	if (flag & UNEEDSIN) {
		if (pipe(&piped1)) {
			error (WARN,errno,cmd);
			goto done;
		}
	}
	
	if (flag & UNEEDSOUT) {
		if (pipe (&piped2)) {
			error (WARN,errno,cmd);
			if (flag & UNEEDSIN) {
				close(piped1.rdpipe);
				close(piped1.wrpipe);
			}
			goto done;
		}
	}
	if ((pid = fork()) == 0) {	/* child process */
		
		if (flag &UNEEDSIN) {
			close(0);
			dup(piped1.rdpipe);
		} else if (flag & USPLIN) {
			close(0);
			dup(splfile);
 		} else if ((USILENT)|| (flag & URETBUF)) {
			close(0);
			open("/dev/null",0);
		}
		if (flag &UNEEDSOUT) {	/* if writing to .exec */
			close(1);
			dup(piped2.wrpipe);
		} else if (flag & USPLOUT) {
			close(1);
			dup(splfile);

		} else if (USILENT) { /* silent, throw away output */
			close(1);
			open("/dev/null",2);
		}
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT,SIG_DFL);
		xclose(2);	/* close all irrelavent files */
		dup(1);		/* restore stderr */

		myshell = getenv("SHELL");
		if ((myshell == NULL)|| (*myshell == '\0')) myshell = "/bin/sh";
		umask(mymask);		/* Restore user's umask */
		seprintf(evbuf,"filename=%s",obname);
		evp = fixenv(evbuf);		/* Add filename definition */
		
		if (streq(cmd,shell)) execl(myshell,shell,"-i",0,evp);
		execle(myshell, shell, "-c", cmd, 0,evp);
		execle("/bin/sh",shell, "-c", cmd, 0,evp);
		_exit(127);
	}

/* parent process (the one that remains EMACS) */
	
	if (pid== -1)  {
		error(WARN,73,cmd);
		goto done;
	}
	if (flag &UPROCESS) {
		close (piped1.rdpipe);
		close (piped2.wrpipe);
		inproc = piped2.rdpipe;
		outproc = piped1.wrpipe;
#ifdef ux3
		fcntl(inproc,F_SETFL,O_NDELAY); /* Turn on non-blocking I/O */
		ioset(1);		/* Set timeout on terminal short */
#endif
		procbuf = curbf;
		procpid = pid;
		return(1);
	}
	if (flag & USPLIN) {
		close(splfile);
		splfile = piped2.rdpipe;
		close(piped2.wrpipe);
		return(1);
	}
	if (flag & USPLOUT) {
		close(splfile);
		splfile = piped1.wrpipe;
		close(piped1.rdpipe);
		return(1);
	}
	if (flag&USENDBUF) {		/* if sending the buffer */
		close(piped1.rdpipe);
		infile = fdopen(tinbuf,piped1.wrpipe,"w");
		for (i = 1; i <=nlines; i++) {
			cmd = mkline(i);
			while ((c = *cmd++) != EOL) {
				putc(c,infile);
			}
			if (i != nlines) putc(EOL,infile);
		}
		mclose(infile);
	}
	if (flag&(UREADBUF|URETBUF)) {		/* if writing */
		close(piped2.wrpipe);
		infile = fdopen(tinbuf,piped2.rdpipe,"r");
		if (flag&UINITBUF) bufinit();
		else if (flag&UREADBUF) {
			bot();	/* append */
			if (column != 0) nl(1);
			putin(cmd);
			nl(2);
		}
		if (flag&URETBUF) {
			cmd = fnbuf;
			i = 127;
			while (i--) {
				c = getc(infile);
				if ((c=='\0')||(c == '\n')||(c==' ')||(c=='\t')|| (c == EOF)) break;
				*cmd++ = c;
			}
			*cmd = 0;	/* EOS */
			mclose(infile);
		} else {
			i = ((flag & UINITBUF) != 0);
			readsub(infile,i,cmd,1-(USILENT|NOECHO));
			chbuf(obuf);
		}
	}
		/*  wait for child to finish */

	/* must wait for child else it becomes a <defunct> process */

	while ((i = wait(&cstatus)) != pid && i != -1);		

	if (flag&URETBUF) return(cstatus);
	/* child is now done, go back to normal EMACS */
	mailcnt = 0;
	
done:	if ((USILENT == 0) && ((flag&UNOSCREEN) == 0)) {
		uncook();
		signal(SIGINT, istat);
		signal(SIGQUIT, qstat);
		junked++;
	}
	return(contin());
#endif	
}

bux(arg)

int arg;

/* Keywords: commands buffers unix-interface shell-escape */

{
	register char *sp;
	register int flag;
	if (SAVEMD) fsave(0);
       	if (arg == 0) flag = 5;
	else if (arg == 1) flag = 2;
	else flag = 3;
	
	do {
		sp = (getname("command line? "));
	} while (sp && 	(unx(sp,flag)== 0));
	return(cstatus);
}

/* query replace */
/* prompts for strings and does conditional replacement */

qrep(gflag)
int gflag;
/* Keywords: commands query-replace */
{
	iqrep(0,gflag);			/* no regular expressions */
}

rqrep(gflag)
int gflag;
/* Keywords: commands query-replace regular-expressions */
{
	iqrep(1,gflag);			/* regular expression version */
}


iqrep(regular,gflag)

int regular;
int gflag;

/* Keywords: query-replace regular-expressions:20 user-interface key-bindings:20 commands:10 */


{

	char sstring[128];
	int dir = 1;
	register char *sp;
	int bsflag = 0;
	int ask = 1;
	int show = 1;
	int l = 1;
	int stop = 0;
	int upt;
	register int c;
	int oldln;
	int oldcol;
	
	if((sp = getname("From? "))== NULL) return;
	if (*sp == NULL) sp = presst;
	strcpy(sstring,sp);
	if ((sp = getname("To? ")) == NULL) return;
	if (!streq(sp,"%")) strcpy(rstring,sp); /* Copy over return string unless it is a single % */
	oldln = curln;
	oldcol = column;
	upt = unstart();
	while (1) {
		if (regular) { 
			if (rgsrch(curln,column,sstring,0,dir)== 0) break;
		} else {
			if (srch(curln,column,sstring,dir)== 0) break;
		}
		dir = 1;
		if ((l == 0) && (curln == kline) && (column == kcol)) {
			error(WARN,72,sstring,rstring);
			goto rdone;
		}
		
 		prompt1("From %s To %s",sstring,rstring);
		strcpy(presst,sstring);	/* save successful search */
		move(kline,kcol);
		if (show) {
			disup();
		}
		if (ask){
			c = getchar();
			donttime=0;
		} else c = 'y';
		switch(c) {

case '.':	stop = 1;		/* stop after this one */
		goto rep_it;		/* make this replacement. */
case ESC:	sp = getname("Replace with: ");
		if (sp) {
			strcpy(rstring,sp);
			goto rep_it;
		}
case CTRLG:	beep();
done:		unprompt();		/* wipe out help message */
		unend(upt);
		return;

case '<':		
		move (oldln,oldcol);	/* return to last replacement */
		goto done;


case 'R':	show = 0;
case 'r':	ask = 0;		/* do rest */
		NOBEL++;		/* Inhibit ding on zero length deletes */
		/* fall through to do this one too */
case ' ':
case 'y':
case 'Y':
rep_it:		kbapp = 0;
		oldln = curln;
		oldcol = column;
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
				dput(*sp);
				bsflag = 0;
				break;
			case '&':
				if (bsflag) {
					dput(*sp);
					bsflag = 0;
					continue;
				} else {
					yank(1);
					unpop(1);
				}
			}
		}
		unins(oldln,oldcol); /* Make an undo entry */
		kpop();			/* clean up our stack */
		if (stop) goto done;
		if (show) {
			disup();	/* before next search */
			mflush(stdout);
		}
		break;
case CTRLH:
case RUBOUT:
case 'n':
case 'N':
		forw(1);
		break;
case 'p':
case 'b':	
		dir = -1;
		forw(-1);
		break;
	default:
		beep();
case '?':
		donttime=1;		/* Avoid time mode */
		prompt3("Query replace -- 'y' to replace, 'n' to skip, ^G to quit,'R' or 'r' for rest");

	}
	if (gflag>1) move(kline+1,0);	/* only do first one */
	}
rdone:	prompt1("Replace Done");
	if (ask==0) NOBEL--;		/* Re-enable warning on 0 length deletes */
	unend(upt);
}
/* length of a string */

lng(sp)

char *sp;
/* Keywords: string-handling length */
{
	register char *spo;

	spo = sp;
	while(*spo++);
	return(spo-sp-1);
}

/* capitalize next count characters */

capnxt(count)

register int count;

/* Keywords: commands character-at-a-time forwards capitalization */
{
	register int c;
	int oldover;
	while (count--) {
		c = (clptr[column]);
		if ((c >= 'a') && (c <= 'z')) {
			oldover=OVERW;
			OVERW=1;
			insertc(1,c-040);
			OVERW=oldover;
		} else if (forw(1)==0) break; /* check for EOF */
	}
}


/* lowercase  next count characters */

lownxt(count)

register int count;

/* Keywords: commands character-at-a-time forwards capitalization */
{
	register int c;
	int oldover;
	while (count--) {
		c = (clptr[column]);
		if ((c >= 'A') && (c <= 'Z')) {
			oldover=OVERW;
			OVERW=1;
			insertc(1,c+040);
			OVERW=oldover;
		} else if (forw(1)==0) break; /* check for EOF */
	}
}

/* capitalize first character of the next word */

capwrd(count)

register int count;

/* Keywords: commands word-oriented-commands forwards capitalization */
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

/* Keywords: commands regions marking movement */
{
	register int x;
	register int y;
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

register int count;

/* Keywords: commands forwards movement:10 transposition */
{
	int oldnodel,oldpic;

	register int undp;
	
	oldnodel=NODEL;
	oldpic=PICMODE;
	NODEL=0;
	PICMODE=0;
	kbapp = 0;
	undp = unstart();
	fdel(1);
	forw(count);
	yank(1);
	unend(undp);
	kpop();
	forw(-1);
	NODEL=oldnodel;
	PICMODE=oldpic;
}

/* changes a mode parameter.  Mode parameters are of three types,
 * string, boolean, or integer.

 * Integer modes set from the count argument, boolean modes turn on if
 * count is one, off otherwise, and string modes prompt for new value

 * chmode returns the previous value of the mode set */


chmode(count)
register int count;

/* Keywords: modes commands assignment */
{
	register char *mp;
	
	if ((mp=getname("Mode? ")) != NULL) {
		if (*mp == '\0') {
			modisp(numarg);
			return(0);
		}
		return(setmode(mp,count,1));
	}
	return(0);
}

char *MDHEAD = "EMACS_MODES:";

bfmodes()				/* set modes from buffer */
/* Keywords: modes buffers file-modes assignment:10 */
{
	register char *cp;
	register char *cp1;
	char mdbuf[20];
	int onoff;
	
	kline = 11;
	if (kline > nlines) kline = nlines;
	while ((kline > 0) && srch(kline,leng(kline),MDHEAD,-1)) {
			 /* if modes set */

		for (cp = mkline(kline)+kcol+12; *cp != EOL;) {
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
			if (mdbuf[0]) setmode(mdbuf,onoff,0); /* Set up mode */
		}
		kline--;
	}
}
			

setmode(mp,count,fudge)
register char *mp;
int count;
int fudge;
/* Keywords: modes macro-programming:10 assignment time-handling:20 display-format:10 */
{
	register int i;
	extern char cbuf[];
	int retval;
	int mfield;
	
	for (i = 0; i < NMODES; i++) {
		if (streq(mdata[i].modename,mp)) {
			retval = *mdata[i].modeloc; /* old value */
			switch(mdata[i].modetype) {
			case ONOFF:
				if (fudge && (count != 1)) count = 0;
				*mdata[i].modeloc = count;
				break;
			case INT:
				*mdata[i].modeloc = count;
				break;
			}
			if ((mfield = mdata[i].moderset)&DSIZE) {
				SCRNLIN = SCRLINES+4;
				setsize();
			}
/* customize the behavior of ^H in word commands */
		
			if (mfield|CTYPE) {
				if (BACKP) {
					bits[CTRLH] = bits['_'] = WRDCHR;
					ctype[CTRLH] = BACKSP;
				} else {
					bits[CTRLH] = bits['_'] = WRDSEP;
					ctype[CTRLH] = CONTRL;
				}
				if (NOTABS) ctype[CTRLI] = CONTRL;
				else ctype[CTRLI] = TAB;
				if (bit8) metal = 0;
				else metal = 2;
			}
			if (mfield & CSE) {
				if (NOCASE) count = 'a'-'A';
				else count = 0;
				for (i = 'A'; i <= 'Z'; i++) {
					casem[i] = i+count;
				}
			}
			if (timemd == 0) *cbuf = 0; /* Nullify current time */
			if (mfield&DISPLAY) {
				fclear(); /* force re-display */
				if (PICMODE==0) hcol=0;
			}
			return(retval);
		}
	}
	IGNORE(error (WARN,45,mp));
	return(0);
}


modval(mp)
char *mp;
/* Keywords: commands macro-programming modes */
{
	register int i;


	for (i = 0; i < NMODES; i++) if (streq(mdata[i].modename,mp)) return(*mdata[i].modeloc);
	return(0);
}


/* display all active modes.  Values displayed for integer and string
 * modes, all on boolean modes are displayed */

modisp(arg)
register int arg;
/* Keywords: informational-displays modes commands user-interface */
{
	register int i;
	char *mp;

	if ((arg < 0) && (mp = getname("Mode? "))) return(modval(mp));

	mtop();
	for (i = 0; i < NMODES; i++) {
		switch(mdata[i].modetype) {

		case ONOFF:
			if (*mdata[i].modeloc) putout("%s mode is on",mdata[i].modename);
			else if (arg) putout ("%s mode is off",mdata[i].modename);
			break;
		case INT:
			putout("%s = %d",mdata[i].modename,*mdata[i].modeloc);
			break;
		}
	}
	putout (endput);
	return(contin());
}

/* compare two strings */

streq(cp,cp1)

register char *cp;
register char *cp1;

/* Keywords: string-handling comparison */
{
	while (*cp) if (*cp++ != *cp1++) return(0);
	if (*cp1) return(0);
	return(1);
}

char *
mstrcpy(cp,cp1)

/* Keywords: assignment string-handling */
register char *cp;
register char *cp1;
{
	while (*cp++ = *cp1++);
	return(cp-1);
}

/* push the marked region onto the kill stack without killing it */


pickup(mnumb)

/* Keywords: regions commands killstack stacking */


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
register int count;
/* Keywords: commands movement text-lines */

{
	move((count<nlines)?count:nlines, 0);
}

/* begin a C coment.  comment begins in COMCOL, if current position is
 * not column 0, otherwise begins in column 0.  the next newline will end
 * the coment */

cment()

/* Keywords: C-mode commands comments */

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
	putin ("\057* ");		/* it's a slash, for stupid compilers! */
	comln = curln;
}

/* adjust the indentation of the current line to be consistent with that
 * in the last line.  */

tabjust(lno)
register int lno;
/* Keywords: insertion C-mode commands:10 */
{
	register int lln;
	register int tabno;
	char *llp;
	int c;

	tabno = 0;
	for (lln = lno-1; lln > 0; lln--) {
		llp = mkline(lln);
		while (llp[tabno] == '	') ++tabno;
		if ((llp[tabno]!= EOL) && (llp[tabno+1] != '*')) {
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
		else {
			bdel(-tabno);
			kpop();
			unpop(1);
		}
	} else { 
		sputl(curln,column,curln);
		move(curln,column);
	}
}

/* } handler, re-sets the indentation back one level */

cbrak(count,brace)

register int count;
register int brace;
/* Keywords: C-mode insertion commands */
{
	if (TABMD &&(RARE == 0) && column && (*(clptr+column-1) == '	')){
		bdel(1);
		kpop();
		unpop(1);
	};
	insertc(count,brace);
}


/* returns 1 if next line contains no non blank (or tab) characters */

isblank(line)

register int line;
/* Keywords: commands:10 line-representation:50 */
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

/* Keywords: commands directories unix-interface */
{
#ifndef PC
	char *np;
	np = expenv(getname("Directory? "));
	if (np) {
		if (SAVEMD) fsave(0);
		if (*np == 0) np = getenv("HOME"); /* null means home */
		if(chdir(np)) {
			IGNORE(error(WARN,errno,np));
			return(0);
		}
		return(1);
	}
	return(0);
#endif PC
}

stats()
{
/* Keywords: commands informational-displays statistics */
	
#ifdef COMPRESS
	extern long coutc;
#endif
	mtop();
	putout ("%d chars from terminal",ninch);
	putout ("%D calls to mputc",nmput);
	putout ("%D chars written by mputc",noutc);
#ifdef COMPRESS
	putout ("%D chars after compression",coutc);
#endif
	putout ("%d terminal writes", ntwrite);
	putout ("%d calls to makeline",nmkline);
	putout ("%d buffer reads",nbread);
	putout ("%d file writes",nbwrite);
	putout ("%d file seeks", nbseek);
	putout ("%d characters of buffer left", (NBLOCK*BLEN)-macptr);
	IGNORE(contin());
}

uline(count)

/* Keywords: word-oriented-commands commands underlining */
register int count;

{
	while (count--) {
		kmark();
		IGNORE(skipf(WRDSEP));	/* skip to begginning of word */
		move(kline,kcol);
		while (bits[*klptr] & WRDCHR) {
			put('_');
			put(CTRLH);
			forw(1);
		}
	}
}

infile(fn)

/* Keywords: commands:40 files keyboard-macros:10 command-line-processing:10 command-files */
register char *fn;
{
	if (pushin(expenv(fn))) {
		edit(1);
		inpop();
		return(1);
	} else return(0);
}


inpsh(arg)
int arg;
{
	register char *fp;
/* Keywords: commands files keyboard-macros:10 command-files */

	if (fp=getname("Input file? ")) {
		if (infile(fp)==0 )  {
			if (arg > 0) IGNORE(error(WARN,errno,fp));
			return(0);
		}
	}
	return(1);
}

/* send the contents of the buffer as mail */

/* mtch -- see if current line matches header */

mtch(cp)
register char *cp;
/* Keywords: mail-processing string-handling:20 comparison:40 */

{
	register char *cl;
	cl = clptr;
	while (*cp) if (*cp++ != *cl++) return(0);
	return(1);
}

mailit()

/* Keywords: mail-processing unix-interface commands */


{
	char cmdbuf[256];
	register char *mp;
	register char *mp1;
	int mailstat;
	char *mailcom;
#ifdef POSTHACK
	char *s;
	register int c;
	extern char *strrchr();
#endif	
	mailstat = 0;
#ifndef PC
#ifdef bsd
#define DEFMAIL "/usr/lib/sendmail -t"
#else
#define DEFMAIL "mail"
#endif
	if ((mailcom=getenv("MAILER"))==NULL) mailcom=DEFMAIL;
	
	mp = mstrcpy(cmdbuf,mailcom);

#ifdef POSTHACK
	/* find the start of the mail command name */
	if ((s = strrchr(mailcom, '/')) != NULL) {
		++s;
	}
	else {
		s = mailcom;
	}
	/* if this is post or Berkeley mail */
	if (streq(s, "post") || streq(s, "Mail")) {
		top();
		while (curln < 10 && *clptr != EOL) {
			
			/* if there is a subject line in the message */
			if (mtch("Subject: ")) {
				
				/* add -s 'subject' to the mail command */
				strcpy(mp, " -s '");
				mp += 5;
				mp1 = clptr + 9; /* skip "Subject: " */
				while ((c = *mp1) != EOL) {
					if (c == '\'') { /* ' becomes '\'' */
						*mp++ = '\'';
						*mp++ = '\\';
						*mp++ = '\'';
						*mp++ = '\'';
					}
					else {
						*mp++ = c;
					}
					++mp1;
				}
				*mp++ = '\'';
				*mp = 0;
				break;
			}
			move(curln+1,0);
		}
	}
	/* get the list of addressees */
#endif
	top();
	while (curln < 10 && *clptr != EOL) {
		if (mtch("TO: ") || mtch("To: ") || mtch("CC: ") || mtch("Cc: ")) {
			if (clptr[1] == 'O') clptr[1] = 'o';
			if (clptr[1] == 'C') clptr[1] = 'c';
			column = 3;
			if (((mp-cmdbuf)+leng(curln)-column) > 250) {
				mailstat += unx(cmdbuf,1); /* partial list */
				mp = mstrcpy(cmdbuf,mailcom);
				move(curln,column);
			}
			mp1 = clptr+column;
			while (*mp1 != EOL) *mp++ = *mp1++; /* copy mailing list */
			*mp = 0;
		}
		move(curln+1,0);
	}
	mp1 = cmdbuf+4;
	while (*mp1) if (*mp1++ != ' ') { /* Check for non-null destination list */
		
		mailstat += unx(cmdbuf,1);		/* send to mail command */
		return(mailstat == 0);
	}
	IGNORE(error (WARN,46));
#endif
	return(0);
}

/* contin -- ask user to continue */


contin()
{
/* Keywords: informational-displays:20 user-interface:10 prompting:40 */
	register int c;
	if (infrn < 0) return(1);	/* continue always in a macro */
	prompt1("Continue?");
 	c = getchar();
	prompt1("");
	if ((c == 'y') || (c == ' ') || (c == '\n') || (c == '\015')) return(1);
	if ('n' == c) return(0);
	if (c == CTRLZ) {
		gquit();
		return(1);
	}
	ungetch(c);
	return(0);
}

/* buffer length and current position */


/* If invoked from tty, prints buffer status info on the terminal.  If */
/* invoked from a macro, returns a value dependent on its argument */

#define VLIN 0				/* return line number */
#define VCOL 1				/* return column number */
#define VDLIN 2				/* display line */
#define VDCOL 3				/* display column */
#define VMINLN 4			/* First file line on screen */
#define VMAXLN 5			/* ditto last */
#define VWTOP 6				/* First screen line of window */
#define VWBOT 7				/* ditto last */

buflng(arg)

int arg;
{
/* Keywords: commands macro-programming:40 buffers statistics */
	
	long flng;
	long fpos;	
	char xbuf[128];
	extern int minln;
	extern int maxln;
	register int i;
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
			if (arg == VDLIN) return(nln-wbase);
			else return(mcol);
		case VMINLN: return(minln);
		case VMAXLN: return(maxln);
		case VWTOP: return(wbase);
		case VWBOT: return(SCRLINES-1);
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

filler(arg)
/* Keywords: regions:50 text-filling commands */
int arg;
{
	register int i;
	register char *cp;
	register int j;
	int unp;
	
	unp = unstart();	
	if (arg != 1) {
					/* fill region only */
		register MARK *markp;
		markp = markptr(curbf);
		if ((i = markp->markl) > curln) fillreg(curln,i,1);
		else fillreg(i,curln,1);
	} else {
		undel();
		killstk(1,0,nlines,0);
		kpop();
		for (i = 1; i < nlines;i++ ) {
			cp = mkline(i);
			if ((*cp != '.') && (*cp != '\'') && (*cp != EOL)) {
				j = i;
				while (i < nlines) {
					cp = mkline(i);
					if ((*cp == '.') || (*cp == '\'') || (*cp == EOL)) break;
					i++;
				}
				fillreg(j,i,0);
				i = curln+1;
			}
		}
		unins(1,0);
	}
	unend(unp);
}

fillreg(i,j,u)
register int i;
register int j;
int u;
{
/* Keywords: commands:10 text-filling regions:20 undo:10 */

	int onlin,ofill;
	onlin = NLINS;
	ofill = FILLMD;
	FILLMD = NLINS = 1;	/* Turn on "rigid_newline" mode */
	move(i,0);
	if (u) {

		/* Make an undo record for this region */
		
		undel();
		killstk(i,0,j,0);
		kpop();
	}
	while (curln < j) {
		endl();
		do {
			i = curln;
			afsep(0,0);/* Note that we don't really put anything in */
			if (i != curln) j++;
		} while (i != curln);
		if (curln < j-1) {
			fdel(1);
			kpop();
			unpop(1);
			while ((clptr[column] == ' ') || (clptr[column] == '	')) {
				fdel(1);
				kpop();
				unpop(1);
			}
			put(' ');
			j--;
		} else break;
	}
	if (u) {
		move(j,0);
		unins(i,0);		/* Make a second undo record */
	}
	NLINS = onlin;	
	FILLMD = ofill;
}

/* macro the region */

macro(arg)
int arg;
{
/* Keywords: key-bindings memory-allocation:10 parsing symbol-handling commands macro-hooks:20 */

	register int mychar;	
	register int balance;
	register int machr;
	int oldpic;
	int compos;
	int hookp;
	int hash,ename;
#ifdef u370
	int beepbeep();
#else	
	extern int beep();
#endif
	
	oldpic=PICMODE;
	PICMODE=0;
	bot();
	back(2);
	if (clptr[column] != CTRLZ) {

/* File does not end in ^Z, Try to catch a potential disaster */
		
		error(WARN,80,fname());
		PICMODE=oldpic;
		return(0);
	}
	if (arg != 1) {
		for (mychar = 0; mychar < NCHARS; mychar++) {
			if (map_it[mychar] > ISIZE) {
				map_it[mychar] = 0;
			}
			if (mychar < NMAC) machash[mychar] = 0;
			if (mychar < NHOOKS) hooks[mychar] = 0;

		}
		macptr = 0;
		fbkno = 0;
	}
	top();
		
	while (curln<nlines) {

		balance = 0;
		machr = clptr[column]&0377;
		if (machr == ESC) machr = (clptr[++column]&0377)+0200;
		if (machr == CTRLX) {
			machr = (clptr[++column]&0377)+0400;
		}
		if (machr == CTRLZ) {
			hookp =  (clptr[++column]&0377);
			if (hookp > NHOOKS) {
				hookp = 0; /* Just ignore the bad ones */
				--column; 
			}
		}
		forw(1);
		if (macptr == 0) pshchr(0); /* Can't use the first location! */
		compos = macptr;
		pshchr(0);		/* Leave room for linkage chain */
		pshchr(0);
		hash = 0;
		ename = 1;
		if (clptr[column] == CTRLBACK) { /* if comment */
			pshchr(0);
			++column;
			while (clptr[column] != EOL) {
				if ((clptr[column] == ' ')||(clptr[column] == '	')) ename = 0;
				if (ename) hash += clptr[column];
				pshchr(clptr[column++]);
			}
			pshchr(0);
			forw(1);	/* skip newline */
		}
		hash %= NMAC;
		bbuf[0][compos] = machash[hash];
		bbuf[0][compos+1] = machash[hash]>>8;
		machash[hash] = compos;
		if (machr == CTRLZ) hooks[hookp] = macptr; /* Define hook */
		else {
					/* Define character command */

			map_it[machr] = macptr+ISIZE;
		}
		while (curln < nlines) {
			mychar = clptr[column]&0377;
			if (column == 0) {
				while ((mychar==' ') || (mychar=='	')) {
					column++;
					mychar = clptr[column]&0377;
				}
			}
			if (mychar != CTRLBACK) {
				if (mychar == MTA({)) balance++;
				else if (mychar == MTA(})) balance--;
				pshchr(mychar);
				forw(1);
				if ((mychar == CTRLZ) && (clptr[column] == EOL)) {
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
		pshchr(CTRLG);		/* escape from bad macro */

macdone:	forw(1);		/* on to next macro */
	}
	PICMODE=oldpic;
	if (hooks[Load_Macro_Hook]) {
		hook(Load_Macro_Hook);
		hooks[Load_Macro_Hook] = 0;
	}
	return(1);
}

ldmac(arg)
int arg;
/* Keywords: commands files reading macro-programming:20 */
{
	register int oldbuf;
	register int tmpbuf;
	char *obname;
	int oldbin;	
	int err;
	
	if (arg>0) err = 1;
	else {
		err = 0;
		arg = 1;
	}
	obname = fname();		/* Old file name */
	oldbuf = curbf;
	chgbuf("...");			/* temporary buffer */
	strcpy(fname(),obname);		/* Restore file name */
#ifdef PC
	oldbin = BINMODE;
	BINMODE = 1;
#endif PC	
	if (fred(err)) {			/* read file */
		macro(arg);
		err = 1;
	} else {
		err = 0;
	}
#ifdef PC
	BINMODE=oldbin;
#endif PC
	tmpbuf = curbf;
	chbuf(oldbuf);
	klbfr(tmpbuf);			/* kill temp buffer */
	return(err);
}
/* macro programming commands */

/* these commands perform various utility functions for macro commands */

bfchr()					/* get a character from the buffer */
{
/* Keywords: macro-programming quoting character-at-a-time buffers:40 */
	return(clptr[column]&0377);	/* Trim sign bits! */
}

litchr()				/* literal character */

{
/* Keywords: macro-programming quoting */
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
#define CDTOS 16			/* arg1 convert to string on kill stack */
#define CSTOD 17			/* convert ks to decimal and return */
#define CBAND 18			/* Bitwise and */
#define CBOR 19				/* Bitwise or */
#define CXOR 20				/* Bitwise xor */
#define CUNGET 21			/* Unget character */
#define CPENDING 22			/* number of characters pending */

/* Keywords: macro-programming arithmetic:50 comparison:50 */

register int ctype;
{
	register int r1;
	register int r2;
	char *sp;
	char sbuf[20];
	int res;
	
	if (ctype == CFALSE) return(0);
	if (ctype == CTRUE) return(1);
	if (ctype == CSTOD) {
		sp = getname("Number?");
		nscan(sp,&res);
		return(res);
	}
	if (ctype == CPENDING) {
		ttfill();		/* Read any pending tty input */
		return(ttcnt);
	}
	r1 = edit(0);			/* first arg */

	if (ctype == CNOT) return(r1 == 0);
	if (ctype == CDTOS) {
		seprintf(sbuf,"%d",r1);
		stkstr(sbuf);
		return(r1);
	}
	if (ctype == CUNGET) {
		pushin(0);		/* Force to tty */
		ungetch(r1);
		inpop();
		return(1);
	}
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
	case CDIV: if (r2) return(r1/r2);
		else return(0);
	case CREM: if (r2) return(r1%r2);
		else return(0);
	case CBAND: return(r1&r2);
	case CBOR: return(r1|r2);
	case CXOR: return (r1^r2);
default:return(0);
	}
}


pscan(lev)				/* parenthesis scan */
/* Keywords: macro-programming parsing control-flow:50 */
register int lev;				/* returns when lev+paren level = 0 */
{
	register int c;
	register int ctlx;
	ctlx = 0;
	while (lev) {
		c = Mgetchar();
		if (c == CTRLX) {
			ctlx++;
			continue;
		}
		if (!ctlx && ((c == (CTRLQ)) || (c == MTA(q)) || (c == (CTRLQ+META)))) {
			c = Mgetchar();
			continue;
		}
		ctlx = 0;
		if (c == MTA({)) lev++;
		else if (c == MTA(})) lev--;
	}
}


cond()					/* conditional execute */
/* Keywords: control-flow parsing :5 conditionals macro-programming */
{
	register int c;
	
	MACEXIT;			/* Only in a macro */
	
	if ((c = Mgetchar()) != MTA({)) {
					/* must be M-{ */
		return(error(NORM,49));
	}
	while ((c = Mgetchar()) != MTA(})) {
		while ((c != MTA({)) && (c !=MTA(}))) c = Mgetchar();
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


mcase()



/* Keywords: cases parsing:5 control-flow macro-programming */
{
	register int cchar;
	register int c;

	MACEXIT;	

	while ((cchar =Mgetchar()) != MTA({));
	cchar = edit(0);		/* match expression */
	
	while ((c = Mgetchar()) != MTA(})) {
		while ((c != MTA({)) && (c !=MTA(}))) c = Mgetchar();
		if (c == MTA(})) return(0); /* failed */
		c = Mgetchar();
		if ((c == cchar) || (c == 0377)) {
			c = edit(1); /* scan command */
			pscan(1);	/* skip to end of case */
			return(c);
		} else pscan(1);	/* skip to end of block */
	}
	return(0);			/* case failed */
}
	
/* mbegin -- begin block in macro */

mbegin()


/* Keywords: macro-programming parsing:20 control-flow */

{
	return(edit(1));		/* push command */
}

/* iteration handler */

iter()
{
/* Keywords: macro-programming control-flow parsing:10 loops */

	register int c;
	register char *savptr;
	
	MACEXIT;
	
	while ((c =Mgetchar()) != MTA({)) {
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


/* Keywords: macro-programming string-handling:20 string-variables:50 filenames:10 prompting:40 reading:20 killstack:20 */
{
	char pbuf[128];
	register char *pb;
	register int pbch;
	
	MACEXIT;
	for (pb = pbuf;;) {
		*pb++ = pbch = Mgetchar();
		if (pbch == CTRLQ){
			--pb;
			*pb++ = Mgetchar();
		}
		if ((pbch == EOL)|| (pbch==CTRLZ)) break;
		if (pb>=(pbuf+128)) pb--; /* Truncate pb */
	}
	*(--pb) = 0;			/* pickup prompt message */
	return(gpm(arg,pbuf));		/* pickup up parameter */
}

/* gkarm -- like gparm, except input is from the kill stack. */


gkarm(arg)

/* Keywords: macro-programming string-handling:20 string-variables:50 filenames:10 prompting:40 reading:20 killstack:20 */
int arg;
{
	char *pb;
	
	pb = getname("");		/* pick up parameter */
	
	return(gpm(arg,pb));
}


gpm(arg,pbuf)
register int arg;
register char *pbuf;

/* Keywords: macro-programming string-handling:20 string-variables:50 filenames:10 prompting:40 reading:20 killstack:20 */
{
	register char *pb;
	char pmpt[128];
	int pbch;
	
	if (arg == 3) {
		eprintf("%s",pbuf);	/* Raw print, shouldn't mung screen */
		
		return(1);
	}
	if (arg != 1)	{
		pushin(NULL);
		if (arg <= 0) {
					/* single character */
			prompt1("%s",pbuf);
			if (arg < 0) {
				mgo(nln,ncol);
				pbch = getchar();
				unprompt();
			}
			inpop();
			return(pbch);
		}
		strcpy(pmpt,pbuf);	/* In case prompt is in pbuf already */
		pb = getname(pmpt);
		if (pb == NULL) pb = "";
		inpop();
	} else pb = pbuf;
	stkstr(pb);
	return(lng(pb));
}


char *
maclook(mp)
char *mp;

/* Keywords: macro-programming:20 symbol-handling macro-invocation global-variables:50 */
{
	int hash,ohash;
	register char *mp1,*mp2;
	
/* now look up macro name */
	
	mp1 = mp;
	hash = 0;
	while (*mp1) hash += *mp1++;
	hash = hash %NMAC;
	ohash = hash;
	hash = machash[hash];
	while (hash) {
		for (mp1=mp,mp2= &bbuf[0][hash+3]; *mp1++ == *mp2++; ) {
			if ((*mp1 == 0) && ((*mp2 == 0)||(*mp2 == ' ')|| (*mp2 == '	'))) {
				while (*mp2++);
				return(mp2);
			}
		}
		hash = (bbuf[0][hash]&0377) + (bbuf[0][hash+1] <<8);
	}
	return(NULL);
}

/* macro call by name */


macex(arg,ivchar)

int arg;
int ivchar;

/* Keywords: macro-invocation macro-programming modes:10 reading:40 */
{
	char mbuf[100];
	register char *mp1;
	register char *mp3;
	register int c;
	
	mp1 = mbuf;
	if (infrn < 0) {
		while ((c = Mgetchar()) != EOL) *mp1++ = c;
		*mp1 = 0;
		mp3 = mbuf;
	} else {
		mp3 = getname ("Macro name? ");
	}
	if (mp3 == NULL) return(0);	/* aborted */
	if (mp1 = maclook(mp3)) {
run:		return(xmac(mp1-bbuf[0],arg,ivchar));
	} else {
		if (AUTOLOAD && (infrn < 0)) {

/* Autoload mode,  If an undefined macro is called from a macro, it */
/* tries to load it from a standard library */

			char buf[128];
#ifdef PC
			seprintf (buf,"%s",mp3);
#else
			seprintf(buf,"~EMACS/macros/%s",mp3);
#endif
			stkstr(buf);
			if (ldmac(-1) &&(mp1 = maclook(mp3))) {
loaded:				prompt(ECHOL,"Autoloaded %s",buf);
				goto run;
			}
#ifdef PC
			seprintf(buf,"c:%s",mp3);
#else
			seprintf(buf,"$EMACS_LIB/%s",mp3);
#endif
			stkstr(buf);
			if (ldmac(-1) && (mp1 = maclook(mp3))) goto loaded;
		}
		error (WARN,51,mp3);
		return(NULL);
	}
}
hook(hookp)
int hookp;

/* Keywords: macro-programming macro-invoction:50 macro-hooks */
{
	register int oldhook;
	int result;
	char xfnbuf[FNLEN];
	
	strcpy(xfnbuf,fnbuf);		/* Preserve temp buffer, in case it's wiped by the macro */
	
	oldhook = hooks[hookp];
	hooks[hookp] = 0;			/* Prevent recursion */
	result = xmac (oldhook,0,0);
	hooks[hookp] = oldhook;
	strcpy(fnbuf,xfnbuf);
	return(result);
}
/* xmac -- execute a macro */


xmac(macp,arg,ivchar)

register int macp;
register int arg;
int ivchar;
{
/* Keywords: macro-programming macro-invocation local-variables:60 argument-processing:20 */
	
	int mvars[NMVAR];		/* macro variables */
	int *omarg;			/* old pointer to mvars */
	
	omarg = marg;
	marg = mvars;
	mvars[1]=arg;			/* record macro argument */
	mvars[0] = ivchar;		/* invoking character */
	pshmac(macp);
	if (etrace) putout("Entering macro: %s",hmap(macp));

	arg = edit(1);

	if (etrace) putout ("Exiting macro: %s",hmap(macp));
	marg = omarg;
	inpop();
	return(arg);
}

/* setvar -- set a local variable */

setvar(arg)
register int arg;
{
/* Keywords: assignment macro-programming local-variables */
	register int oldvar;

	if ((arg <NMVAR) && (arg >=0)) {
		oldvar = marg[arg];
		marg[arg] = edit(0); /* set new value */
		return(oldvar);
	} else {
		beep();
		return(0);
	}
}

/* getsharg -- get argument from shell command line */

/* with argument of 1, gets one argument and pops. */
/* with other argument, returns argument but does not pop */

/* Value returned is 1 unless there are no more command line options. */


getsharg(arg)
int arg;
{
/* Keywords: argument-processing:10 command-line-processing macro-programming */
	extern char **sharg;
	extern int nsharg;
	
	if (nsharg) {
		
		stkstr(*sharg);
		if (arg==1) {
			nsharg--;
			sharg++;
		}
		return(1);
	}
	return(0);
}


	
/* string operations */

/* compares same argument conventions as other comparisons */

#define CAPP 7				/* Arg 2 appended to arg1 */
#define CINDEX 8			/* index of arg 2 in arg1 */
#define SBINARY 8
#define CSUBSTR 9			/* string indexed by next 2 commands */
#define CLENGTH 10			/* String length */
#define SUNARY 10
#define CPTR 11
#define CSTRING 12
#define CPRINTF 13			/* formatted I/O */

cmpst(arg)

register int arg;

/* Keywords: macro-programming comparison string-variables string-handling:30 conversions:20 */
{
	char buf[256];
	
	register char *bp1;
	register char *bp2;
	char *bp3;
	int i,j;
	
	if (arg <= SUNARY)bp1 = getname("");		/* get first string */ 
	
	if (arg <= SBINARY) {
		strcpy(buf,bp1); 
		bp1 = buf;		
		bp2 = getname("");
	}
	
	if (arg <= CGE) {
		while ((*bp1 == *bp2) && *bp1) {
			bp1++;
			bp2++;
		}
	}
	switch (arg) {
		
	case CLE: return(*bp1<=*bp2);
	case CEQ: return(*bp1==*bp2);
	case CLS: return(*bp1<*bp2);
	case CGT: return(*bp1>*bp2);
	case CNE: return(*bp1!=*bp2);
	case CGE: return(*bp1>=*bp2);
	case CAPP:
		strcpy(bp2+lng(bp2),bp1);
		stkstr(bp2);
		return(1);
	case CINDEX:
		i = 0;
		bp3 = bp2;
		while (*(bp2=bp3+i)) {
			bp1 = buf;
			while (*bp1++ == *bp2++) {
				if (*bp1 == 0) return(i);
			}
			i++;
		}
		return(-1);
	case CSUBSTR: 
		i = edit(0);
		j = edit(0);
		bp1[j]=0;
		stkstr(bp1+i);
		return(1);
	case CLENGTH: return(lng(bp1));
	case CPTR: return(kgptr());
	case CSTRING:
		i = edit(0);
		kput(i);
		return(1);
	case CPRINTF:
					/* Printf like formatting. */
		{
			int pts[20],len;
			char buf2[256];
			bp1=bp2=getname(""); /* Format string */

/* First, collect all of the parameters */
			
			i = 0;
			while(*bp1) {
				if (bp1[0] == '%') {
					if (bp1[1] == '%') bp1++;
					else pts[i++] = kgptr(); /* Collect one */
				}
				bp1++;
			}
			strcpy(buf2,bp2);
			bp2 = buf2;
			bp1 = buf;
			while (*bp2) {
				if (*bp2 == '%') {
					bp2 = nscan(bp2+1,&j);
					if (*bp2 == '%') {
						*bp1++ = *bp2++;
						continue;
					}
					kput(pts[--i]);
					bp3 = getname(""); /* get the param */
					len = lng(bp3);
					
					switch(*bp2++) {
						
					case 'l': bp1 = mstrcpy(bp1,bp3);
						j = j-len;
						if (j>0) while (j--) *bp1++ = ' ';
						break;
					case 'r':
						j = j-len;
						if (j>0) while (j--) *bp1++ = ' ';
						 bp1 = mstrcpy(bp1,bp3);
						break;
					case 'c':
						len = (j-len);
						if (len>= 0) {
							j = len/2;
							len = len-j;
							while (j--) *bp1++ = ' ';
						}
						bp1 = mstrcpy(bp1,bp3);
						if (len> 0) while (len--) *bp1++ = ' ';
						break;
					}
				} else *bp1++ = *bp2++;
			}
			*bp1 = 0;
			stkstr(buf);
			return(1);
		}
	default:  return(0);
	}
}


/* Global variable -- global variables are dynamically bound by
 * names.  They are 2 or 4 bytes long and always stored in order of
 * lsB first.   */

glob(arg)
int arg;
{
/* Keywords: global-variables assignment:50 macro-programming */
	int size;
	char *name;
	char *where;
	int hash;
	
	
	if (name= getname ("Global variable name? ")) {
		if ((where = maclook(name)) == NULL) {
			if (arg == 0) return(0);
			where = name;
			hash = 0;
			while (*where) hash += *where++;
			hash = hash %NMAC;
			pshchr(machash[hash]);
			pshchr(machash[hash]>>8);
			machash[hash] = macptr-2;
			where = name;
			pshchr(0);
			while (*where) pshchr(*where++);
			pshchr(0);
			for (size=0; size<sizeof(size); size++) pshchr(0);
			where = maclook(name);
		}
		switch(arg) {

		case 0:
			return(1);
			break;
		case 1:
			hash = 0;
 			for (size=0; size<sizeof(size); size++) {
				hash+= ((*where++)&0377) << (size*8);
			}
			return(hash);
		case 2:
			hash = edit(0);
			for (size=0; size<sizeof(size); size++) {
				*where++ = hash;
				hash = hash >>8;
			}
			break;
		}
	}
	return(0);
}
/* envexp -- expand environment variables in a string */

/* expansion takes place in the caller's string buffer */
/* pointer is returned for convenience only */

char *pwpath = "/etc/passwd";

char *
expenv(str)
register char *str;
/* Keywords: environment-variables unix-interface user-interface:20 shell-escape:10 */
{
	char strtemp[128];
	char vartemp [64];
	register char *cp1;
	char *cp2;
	register int c;
	int oc;
	
	if (str == NULL) return(NULL);
		
	cp1 = strtemp;
	cp2 = str;
	while (*cp1++ = *str) {
		if ((*str== '`')||(*str=='*')||(*str=='{')||(*str=='[')||((*str++)=='?')) {
			seprintf(strtemp,"exec echo %s",cp2);
			unx(strtemp,4);			/* expand through the shell */
			return(fnbuf);
		}
	}
	cp1 = strtemp;
	str = fnbuf;			/* always copy back into file name */
	while (c = *cp1++) {
		if ((c == '$') || (c == '~')) {

/* Environment variable or `logdir` */
			
			oc = c;
			cp2 = vartemp;
			while (bits[c=((*cp1++)&0377)]&WRDCHR) {
				*cp2++ = c;
			}
			cp1--;		/* backspace pointer */
			*cp2 = 0;
			if (oc == '$') {
				cp2 = getenv(vartemp); /* environment variable */
			} else {
#ifndef PC
/* Home Directory */
			
				if (*vartemp == 0) {
					cp2 = getenv("HOME"); /* Bare ~ means home */
				} else if (streq(vartemp,"EMACS")) {
					cp2 = em_dir;
				} else if (streq(vartemp,"exptools") &&
					(cp2 = getenv("TOOLS")) && *cp2) {
					;
				} else {
					FILE pwfile[1];
					FILE *pwptr;
					char *vp;
					
					cp2 = NULL;
					pwptr = xopen(pwfile,pwpath,"r");
					if (pwptr) {
						while ((c = getc(pwptr))!= EOF) {
							if (c == '\n') {
								vp = vartemp;
/* Try to match the specified user ID */
								while (*vp++ == (c = getc(pwptr)));
								if ((vp[-1] == 0) && (c == ':')) {
/* Found it, now skip 4 colons, and copy next field into vartemp; */
									c = 0;
									while (c < 4) if (getc(pwptr) == ':') c++;
									cp2 = vartemp;
									while ((c = getc(pwptr)) != ':') *cp2++=c;
									*cp2++=0;
									cp2=vartemp;
									break; /* Break out of password scan */
								}
							}
						}
						mclose(pwptr);
					} else error(WARN,errno,pwpath);
				}
#endif PC
			}
			if (cp2 != NULL) {	
				str = mstrcpy(str,cp2);
			} else {
				*str++ = oc;
				str = mstrcpy(str,vartemp);
			}
		} else {
			*str++ = c;
		}
	}
	*str++ = 0;
	return(fnbuf);
}
/*
 * envget	takes a string and expands it into the string
 *		stored in the shell variable by the same name.
 *		If the shell variable did not exist, then envget
 *		returns NULL.
 */

envget()
/* Keywords: commands environment-variables */

{
	char * ptr;
	
	if (ptr = getenv(getname("Environment variable: ")))  {
		stkstr(ptr);
		return(1);
	}
	else {
		stkstr("");
		return(0);
	}
}

stopjob ()
{
	char x;
	clear();
	cook();
#if (defined (bsd) || defined (v8))
	kill(mypid,SIGSTOP);
#else
	eprintf ("Type ^Z to suspend emacs and <return> to continue");
	read(0,&x,1);
#endif
	uncook();
	clear();
}

#ifdef u370
beepbeep()
{
	extern int beep();
}
#endif

