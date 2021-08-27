#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_disp.h"

/* EMACS_MODES: c !fill */

/* character insert */

/* inserts count occurances of the character c in the file */



insertc(count,c)

register char c;
register count;

{

	while (count--) {
		if ((OVERW) && (c != EOL) && (clptr[column] != EOL)) {
			fdel(1);	/* overwrite mode */
			put(c);
		} else {
			if ((INSERTC) && (clptr[column] != EOL) && (MOREIN == 0)&& (clptr[column] != c)){
				ldchar = c; /* fake out mptc */
				ldcol = mcol-1; /* make it think that */
						/* c was just squeezed out */
			}
			put(c);
		}
	}
}

/* insert one character into the file at current position */

put(c)

register char c;

{

	register char tc;
	register xcol;
	
	if (c == EOL) {
		nl(1);
		return;
	}
	xcol=column;
	while (c!= EOL) {
		tc = clptr[xcol];
		clptr[xcol++] = c;
		c = tc;
	}
	modify(curln);
	clptr = ckline(curln,xcol);
	clptr[xcol] = EOL;
	sputl(curln,column,curln);	/* fix this line */
	move(curln,column+1);
}

/* tabc -- tab command */
/* inserts a tab unless notabs mode is on, in which case */
/* it inserts spaces up to next tabstop. */

tabc(count,arg)

register int count;
int arg;
{
	if (infrn < 0) return(1);		/* no dice in macro */
	if (NOTABS) {
		while (count--) {
			do {
				put(' ');
			} while (column%TABSTOP);
		}
	} else {
		insertc(count,arg);
	}
}

/* mvc -- move with checking for c<=length(l) */



mvc(l,c)
register l;
register c;
{
	register x;

	if ((x = leng(l)) < c) move(l,x);
	else move (l,c);
}
/* move to new position in the file */

move (newln,newcol)
register newln;
register newcol;

{
	
	if (newln < 1) newln = 1;
	if (newln >= NPTRS) {
		if (!growbuf(newln)) {
			newln = NPTRS-1;
		}
	}
	curln = newln;
	clptr = mkline(curln);
	column = newcol;
	return;
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
	int xcost;		/* cost with all relative movement */
	int ycost;		/* cost with carriage return */
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
#ifdef TERMCAP
			putpad(tgoto(CURAD, y, x));
#else
			if (SRCADD) {
				eprintf(CURAD,x+XBASE,y+YBASE);
			} else { 
				eprintf(CURAD,y+YBASE,x+XBASE);
			}
#endif
			scrlin=x;
			scrcol=y;
			return;
		}			/* else relative is cheap, do it */
	} else {
		if (osert && (MI == 0)) {
			unsert();	/* LEAVE insert character mode */
		}
		
		if (ycost < xcost) {
					/* do carriage return processing */
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
				putchar(cmap[scrlin] [scrcol]); /* re-write */
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
	PUTS(OSERTC); /* can't stay inserting */
	osert = 0;
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
	int *mapptr;
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
	for (i = start,mapptr = scrmap+i,jnkptr = scrjnk+i; i != stop-x; i+=off,mapptr+=off,jnkptr+=off) {
		*jnkptr = *(jnkptr+x);
		*mapptr = *(mapptr+x);
		cp1 = cmap[i];
		cp2 = cmap[i+x];
		for (j = 0; j < *jnkptr; j++) {
			*cp1++ = *cp2++;
		}
	}
	while (i != stop) {
		if (off > 0) {
			*mapptr++ = *jnkptr++ = 0;
			i++;
		} else {
			*mapptr-- = *jnkptr-- = 0;
			i--;
		}
	}
}
	
/* adjust vertical position of line -- open (or close) lines on */
/* the screen  argument is the number of lines to add (or drop). */

vadjust(x,xline)

register x;
int xline;
{
	register i;
	register j;
	int oldx;

	
	oldx = xline;
	
	TRACE(VADJUST);
	TRACE(curln);
	TRACE(x);
	if (x<0) {
		x = -x;
		i = 1;
	} else i = 0;
	if (((j = lastln-xline-x) <= 0)||
	 ((x*VCOST)>(j*ttywarp*CFILL))) return; /* nothing to save */
	SREGION=SCRLINES-oldx;		/* effected region */
	if (i) {			/* if deleting lines */
		sgo(xline,0);
		
		if (SCREG) {		/*if vt100 stype scrolling */
			PUTS(SCREG,oldx+XBASE,SCRLINES+XBASE-1); /*define region*/
			scrlin = scrcol = 0;
			sgo(SCRLINES-1,0);
			for (i = 0; i < x; i ++) {
				PUTS(SSCROLL);
			}
			PUTS(SCREG,XBASE,SCRNLIN);
			scrlin = scrcol = 0;
		} else {
			for (i = 0; i < x; i++) {
				PUTS(LDEL);
			}
			sgo(SCRLINES-x,0);
			for (i = 0; i < x; i++) {
				PUTS(LOPEN);
			}
		}
		sgo(oldx,0);
		vshift (oldx,SCRLINES-1,x);
	} else {
		if (SCREG) {		/* if vt100 style scrolling */

			PUTS(SCREG,oldx+XBASE,SCRLINES+XBASE-1); /*define region */
			scrlin = scrcol = 0; /* vt100 dies */
			sgo(xline,0);
			for (i = 0; i < x; i ++) {
				PUTS(RSCROLL);
			}
			PUTS(SCREG,XBASE,SCRNLIN);
			scrlin = scrcol = 0;
		} else {
			sgo(SCRLINES-x,0);
			for (i = 0; i < x;i++) PUTS(LDEL);
			sgo(oldx,0);
			for (i = 0; i < x; i++) PUTS(LOPEN);
		}
		mgo(oldx,0);
		
		vshift(oldx,SCRLINES-1,-x);
	}
}


/* sscroll -- try to fix display by scrolling */

sscroll(x)

register int x;

{
	register int i;

	
	if (SSCROLL == NULL || twowind || (SCRLINES-x < 3) || (curbf != disbuf[cwind])) return;


	sgo(SCRNLIN-1,0);			/* to bottom */
	SREGION=SCRNLIN;			/* number of lines effected */
	for (i = 0; i < x; i++) {
		PUTS(SSCROLL); /* scroll screen */
	}
	vshift (0,SCRNLIN-1,x);
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
	if (DELC && (c == cmap [mline] [mcol+1]) && (mcol+3 <scrjnk[mline]) && (ldchar == 0)) {
		register i;
		SREGION=scrjnk[mline]-mcol; /* number of char's gobbled */
		PUTS(DELC); 	/*clobber next char */
		for (i = mcol; i <=scrjnk[mline];i++) {
			cmap[mline] [i] = cmap[mline] [i+1];
		}
		scrjnk[mline]--;
	} else {
		if (INSERTC && (c == ldchar) && (mcol == ldcol+1) && (mcol<scrjnk[mline])) {
			register i;
			if (osert == 0) PUTS(INSERTC); 	 /* open space */
			if (scrjnk[mline] >SCRWID) scrjnk[mline]--;
			for (i = scrjnk[mline]++; i >= mcol;i--) {
				cmap[mline] [i+1] = cmap[mline] [i];
			}
			if (OSERTC) osert=1;	/* insert character mode is on */
			cmap[mline] [mcol] = 0;
		} else {
			if (osert) {
				unsert();
			}
		}
		putchar(c);
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

/* delete chars -- delete characters starting at . */

/* all characters must be on the same line */

delc(count)

register count;

{
	register char *cp1;
	register char *cp2;

	killstk(curln,column,curln,column+count); /* stack even on 0 */
	if (count == 0) {
		beep();
		return(0);			/* nothing to modify */
	}

	cp1 = clptr+column;
	cp2 = cp1+count;
	while ((*cp1++ = *cp2++) != EOL);
	sputl(curln,column,curln);
	modify(curln);
	move(curln,column);
	return(1);
}

/* open up count blank lines in the file */

openl(count)

register count;

{

	register i;
	register char *lp;
	char *lp1;
	char *lp2;
	int oldcol;
	int oldln;

	/* first fix the file */

	if (nlines+count >= NPTRS) {
		if (!growbuf(nlines+count)) {
			return;
		}
	}
	for (i = nlines; i > curln; i--) {
		ptrs[i+count] = ptrs[i];
	}
	nlines += count;
	for (i = curln+1; i <= curln+count; i++) ptrs[i] = 0;
	if (clptr[column] != EOL) {
		ckline(curln+count,leng(curln)-column);
		holdin(curln,curln+count);
		lp1 = mkline(curln+count);
		lp2 = lp = mkline(curln) + column;
		while ((*lp1++ = *lp++)!= EOL);
		*lp2 = EOL;
	}
	modify(curln);
	modify(curln+count);

	/* now fix the display */

	oldln = curln;
	oldcol = column;
	if (TABMD && (RARE == 0)) {
		tabjust(curln+count);
	}
	if (findline(oldln)) {
		if (oldcol) nln++;	/* don't adjust the current line */
					/* unless the whole line moved */
		for (i = nln; i < SCRLINES; i++) {
			if (scrmap[i]) scrmap[i] += count;
		}
	}
	sputl(oldln,oldcol,REST);
	move(oldln,oldcol);
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

	if (infrn < 0) {		/* if in macro */
		if(retrvs(fnbuf,FNLEN)>=0){
			return(fnbuf);
		} else return(NULL);
	}
	fnbuf[i=0] = 0;	
	for (;;) {
		prompt1("%s%s",ps,fnbuf); /* display prompt */
		mgo(mline,mcol);
		c = getchar();
		switch(c) {
	
		case '':
		case '':
			beep();
			unprompt();
			return(NULL);
		case '':
			xp = mkline(curln);
			goto wrdin;
		case '':
			xp = fname();
wrdin:			while ((*xp!= 0) && (*xp != EOL)) {
				fnbuf[i++] = c = *xp++;
				if (i >= FNLEN) {
					beep();
					--i;
				}
				fnbuf[i] = 0;
			}
			continue;

		default:
			if (c >= ' ') {
regchar:			fnbuf[i++] = c;
				if (i >= FNLEN) {
					beep();
					--i;
				}
				fnbuf[i] = 0;
				continue;
			}
			beep();
			continue;
		case '\n':
		case '\r':
		case ESC:
			unprompt();
			return(fnbuf);
		case '':
			clear();
			continue;
		case '':
		case '':
			if (i) {
				i--;
				fnbuf[i] = 0;
			} else beep();
			continue;
		case '':
			c = getchar();
			goto regchar;
			
		case '@':
		case '':
			fnbuf[i=0]=0;
			continue;
			
		}
	}
}

/* gechar: get EMACS character: */
/* returns 0200+c for meta chars, 0400+c for ^X chars */


gechar(ps)
register char *ps;

{
	register int c;
	
	prompt1("%s: ",ps);
	c = getchar();
	if (c == ESC) {
		prompt1("%s: M-",ps);
		c = 0200+getchar();
	}
	if (c == '') {
		prompt1("%s: ",ps);
		c = 0400+getchar();
	}
	unprompt();
	return(c);
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
		if ((getchar()) == 7) return(-1);
		unprompt();
		mline=0;		/* TOP */
	}
	scrow = 0;
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
	if (infrn) return;		/* prompt only if input from stdin */
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
	scrow=0;			/* prevent display wipeout */
	clrl();
}

/* clear out prompt */

unprompt()

{
	if (infrn) return;		/* unprompt only if input from termial */
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


/* multi-line deleter */

/* deletes the text between current position, and kline, kcol */
/* all deleted text is stacked */

tkill()

	/* arguments are kline,kcol */
{
	int oldcol;
	register x;
	register y;
	register char *cp;
	char *cp1;

	if (kline < 1) kline = 1;
	if (kline >nlines) {
		kline = nlines;
		kcol = leng(kline);
	}
	if ((kline < curln) || ((kline == curln) && (kcol < column))) {
		x = curln;
		y = column;
		move (kline, kcol);
		kcol = y;
		kline = x;
	}


	if (kline == curln) {
		return(delc (kcol-column));		/* cheap out */
	}

	oldcol = column;
	killstk(curln,column,kline,kcol);
	ckline(curln,column+leng(kline)-kcol);
	holdin(curln,kline);
	cp1 = mkline(curln)+column;
	cp = mkline(kline)+kcol;
	while ((*cp1++ = *cp++)!= EOL) column++;
	modify(curln);

	/* current line is fixed, fix rest */

	x = kline-curln;

	for (y = curln+1+x; y <= nlines; y++) {
		ptrs[y-x] = ptrs[y];
	}
	nlines -= x;

	if (findline(curln)) {
		if (oldcol) nln++; 	/* don't adjust the current line */
					/* unless the whole line moved */
		for (y = nln; y < SCRLINES; y++) {
			if(scrmap[y]>=(curln+x)) {
				scrmap[y] -= x;
			} else scrmap[y] = 0; /* line is now dead */
		}
	}
	sputl(curln,oldcol,REST);
	move(curln,oldcol);
	return(1);
}

/* refresh the screen */

/* clear and force refresh */

refresh(arg)

register unsigned arg;

{
	register lno;
	
	if (numarg == 0) {
		clear();
		return;
	}

	lno = curln-arg;
	if (lno < 1) lno = 1;
	fclear();			/* just blew up file */
	if (disbuf[cwind] != curbf) {
		
/* NOTE:  This is a pain!  display is out of date because refresh was
 * called from a macro, If we don't do something here, disup will pick
 * a new window when we change buffers.  What we do is not very
 * good, but it works?
 */

		if (junked || ((twowind == 0) && (CLINE == 0))) {
			clear();	/* dumb terminal or command */
		} else {
			xclear();	/* others, just clobber map */
		}
		disbuf[cwind] = curbf;		/* avoid reset in disup */
	}
	minln = lno;
	maxln = curln+1;		/* we will get at least this far */

}

/* put one character on the screen checking for end of screen line */

sputc(c)

register c;
{

	register i;

	if (mcol == SCRWID) {

		scrmap[mline] = scrow;
		if (mline>=NSCRLIN-1) {
			mline--; /* don't run overboard */
		} else {
			mputc(SCRCONT);
			mline++;

/* The following code checks for overflow from a line into a line with */
/* multiple display lines */
			
			if ((scrmap[mline] != scrow) && (mline < NSCRLIN-1)) {
				if (scrmap[mline+1] == scrmap[mline]) scrmap[mline+1] = 0;
			}
		}
		ldchar = 0;		/* reset ldchar */
		mcol = 0;

		for (i = 0; i < LNOMOD*LNOWID; i++) mputc(' ');
	}
	mputc(c);
}

/* mark a region of the displayy for fixing.  Arguments are the  */
/* first line and (file) character position to fix, and the last line */

sputl(from, col, to)

int col;
register int from;
int to;
{
	register int *fmp;
	int wwid;
	
	if (disbuf[cwind] != curbf) return; /* not in this window */
	from -= minln;
	wwid = maxln - minln;
	fmp = fmap;
	to -= minln;
	if (to > wwid) to = wwid;
	if ((from >= 0) && (from <= wwid)) {
		if (fmp[from] > col) fmp[from]= col;
	}
	while (++from<=to) if (from >= 0) fmp[from]=0;
	return;
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
		case PLAIN:
			sputc(c);
			return;
		case BACKSP:
			if (BACKP) {
				if (mcol) mcol--;
				return;
			}
	/*  Fall through to handle backspace in normal mode */
			
		case CONTRL:
do_contrl:		sputc('^');
			sputc(c^0100);
			return;
		case TAB:
			if (NOTABS) goto do_contrl;
			i = TABSTOP-((mcol-(LNOMOD*LNOWID))%TABSTOP);
			while (i--) sputc(' ');
			return;
		}
}

/* cpuut -- put a character in the file, like xputc */



cput(c)
register c;

{
	c &= 0377;
	if (c & META) {
		put('M');
		put('-');
		c-= META;
	}
	switch(ctype[c]) {

	case PLAIN:
		put(c);
		break;
	default:
		put('^');
		put(c^0100);
		break;
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
	scrmap[mline] = scrow;
	if (y > 0) {
		sgo(mline,mcol);			/* go for real */
		if (CLINE) {
			SREGION=y;	/* Number of characters cleared */
			PUTS(CLINE);
		} else {
			xline = mline;
			xcol = mcol;
			while (y--) mputc (' ');
			mgo(xline,xcol);
		}
		scrjnk[x] = z;
	}
}

/* findline -- find a line on the screen */

/* findline makes no judgement as to whether or not the line is correct  */
/* whether or not the screen is OK up to the line */

findline(line)
register int line;

{
	register int i;
	
	if (disbuf[cwind] == curbf) {
		for (i = wbase; i < SCRLINES; i++) {
			if (scrmap[i] == line) {
				nln = i;
				return(1);
			}
		}
	}
	return(0);
}

/* find the screen address of a file */

/* The result is to set mcol and mline and return 1 if found
 * found, otherwise 0 is returned */


findpos(line,col)

register char *line;
register col;

{
	register i;
	char c;
	
	mcol = LNOMOD*LNOWID;
	for (i = 0; i < col; i++) {
		if ((c= line[i]) == EOL) return(0);
		if (c & META) {
			mcol+=2;
			c-=META;
		}
		switch(ctype[c]) {

		case PLAIN:
			mcol++;
			break;
		case BACKSP:
			if (BACKP) {
				mcol--;
				break;
			}
		case CONTRL:
fnd_ctrl:			mcol+=2;
			break;
		case TAB:
			if (NOTABS) goto fnd_ctrl;
			mcol = mcol+TABSTOP-((mcol-(LNOMOD*LNOWID))%TABSTOP);
			break;
		}
		if (mcol>SCRWID) {
			if (++mline>SCRLINES) return(0);
			mcol = mcol-SCRWID;
			if (LNOMOD) mcol+= LNOWID;
		}
	}
	return(1);
}
/* print a line number(x) */

lnumb(x)

{
	register i;
	char tbuf[8];
	
	for (i=0; i<8; i++) tbuf[i]=0;
	seprintf(tbuf,"%d",x);
	mcol = 0;						/* make sure were at start of line */
	for (i=0; i<LNOWID; i++) {
		if (tbuf[i]) sputc(tbuf[i]);
		else sputc(' ');
	}
}

/* put a string on the screen, translating control and meta */

sputs(xp)
register char *xp;
{
	register c;
	while (c= *xp++) {
		if (c == NEWLINE) {
			scrmap[mline] = scrow; /* mark line */
			if (mline < NSCRLIN) mline++; /* don't overflow */
			mcol = 0;
			xputc('	');
		} else xputc(c);
	}
}

/* putin -- insert string into file */


putin(xp)

register char *xp;
{
	register c;
	RARE = 1;
	while (c = *xp++) {
		if (c == NEWLINE) {	/* Note that NEWLINE != '\n' */
			nl(1);
			put('	');
		}
		else put(c);
	}
	RARE = 0;
}

/* nxtpag -- move to next page */

nxtpage(count)

register count;

{

	register line;
	
	
	line = (maxln+1+((count-1)*(before+after)))-keepg;
	if ((line <= nlines) || ((maxln < nlines ) && (line = nlines-10))) {
		move(line,0);
		toppage();
		line +=before;
		if (line>nlines) line = nlines;
		move(line,0);
		return(1);
	} else {
		beep();
		return(0);
	}
}

/* move to top of page (current line becomes first line)  */

toppage()


{
	numarg = 1;
	refresh(0);			/* refresh screen */
}

/* move to previous page */

lstpage(count)

register count;

{
	register line;
	register wsize;
	
	wsize = SCRLINES-wbase;
	line = (minln+(wsize>>1)-count*(wsize))+keepg;
	if (line<1) line=1;
	move(line,0);
}

clear()

{
	register int i;
	
	PUTS(CLEAR);
	for (i = 0; i < NSCRLIN; i++) scrmap[i] = scrjnk[i] = 0;
	scrlin = scrcol = mline = mcol = 0;
	disbuf[0]=disbuf[1]= -1;
	junked = minln = maxln = 0;
}


/* didle (display idle) update display and idle for the specified time */

didle(count)

int count;
{
	
	if (count > 10) count = 10;	/* limit to 10 seconds */
	pushin(NULL);			/* make sure we will update */
	disup();
	mflush(stdout);
#ifdef TERMCAP
	while (count--) PUTS ("1000");	
#else
	while (count--) PUTS ("%1000p");
#endif
	inpop();
}

/* disup() update display.  disup should be called whenever you want the */
/* to be up to date.  disup does not update if there is more input */
/* available from the current input source. */

/* This is the high level display algorithm, which translates the
 * buffer into a screen image.  The pparameters affecting the algorithm are
 * 
 * scrmap[SCRNLIN] 	map from the display line to the file line on
 * 			that line of the display.
 * fmap[SCRNLIN]	map from the file indicating what the smallest
 * 			column in the file containing undisplayed changes
 * 			is.  A special value (LGOOD) indicates that
 * 			the whole line is good.  This avoids looking at
 * 			the lines in the file that are known to be ok.
 * minln		The file line appearing at the top of the current
 * 			window.
 * maxln		The last file line appearing in this window.
 * lastln		The screen line on which maxln is displayed.
 * curbf[2]		The buffer displayed in each window.
 * before		The number of lines displayed before curln.
 * after		(Set equal to before, not used).
 * 
 * The basic algorithm is to firsst check to see if the window is properly
 * positioned to hold the cursor.  If not, a new window is chosen and minln
 * reset.  Then, each line that should be displayed is examined for
 * changes.  Any lines needing updating are redisplay.  A side effect of
 * the scan is to find the screen address of the current line.  If the
 * screen address is found, the mode line and window splitting line are
 * updated (if necessary), and any other lines are blanked.  If the cursor
 * wasn't found, before is cut in half, and the entire display is
 * re-created. This can happen with lots of lines that overflow, pushing
 * the current line off the display.  Repeated failure causes  the can't
 * find cursor error, which indicates either a bug or that the current line
 * doesn't fit in the window.
 */

/* status display */

char mdchar[2] = {'=', '>'};


disup() 				/* force update display */

{


	register int col;
	int fixed;
	register char *cp;
	int *fmp;
	
	if (MOREIN) return;
	fixed = 0 ;
	nln = ncol = -1;

/* Check to see if what is on the screen is from the same buffer.  If
 * not, don't try to re-use it
 */
	if (disbuf[cwind] != curbf) {
		minln = 0;		/* force repositioning */
		if (CLINE || twowind) {
			xclear();	/* force redisplay */
		} else {
			clear();	/* try to optimize dumb terminal */
		}
		disbuf[cwind] = curbf;		/* mark buffer up to date */
	}

/* See if the current line is on the screen.  The check is not absolutely
 * perfect.  If the current line is not on the screen, a new position
 * is picked.  Mis-guessing will be caught eventually.
 */


	if ((minln == 0) ||(curln <minln) || ((curln> maxln) && (curln-maxln+lastln>=SCRLINES))) {

					/* display position is bad */

		if (junked) {
			clear();
			disbuf[cwind] = curbf;		/* mark buffer up to date */
		}
badpos:
		if(fixed) {
			before = (before >> 1);
			if (before == 0) error(FATAL,42); /* can't find cursor */

		} else {
			before = (SCRLINES-wbase)/2;
			fixed = 1;
		}
		after = before;
		minln = curln-before;
		if (minln < 1) minln = 1;
		fclear();		/* file bad */
		maxln = curln;
	}
	

/* Now for the real work, check all of the lines.  A line can be bad if
 * scrmap indicates that its not in the right place on the display
 * or if fmap indicates that it may have changed since being put up.
 */
	fmp = &fmap[-minln];
	mcol = 0;
	for (scrow = minln, mline = wbase; (scrow <= nlines) && (mline < SCRLINES);scrow++) {
		if ((scrmap[mline] != scrow)|| (fmp[scrow] < LGOOD))  {

/* line is bad, first try to bring it into position by scrolling or by
 * inserting/deleting lines.  This could result in the display being
 * corrected.
 */

			if ((mline == 0) && (scrmap[0] != scrow) && SSCROLL) {
	/* try to scroll the screen up with newlines */
				
				register x;
				for (x = 1; x < SCRLINES; x++) {
					if (scrmap[x] == scrow) {
						sscroll(x);
						break;
					}
				}
			}
			if ((scrow<maxln) && (scrmap[mline]!= scrow)&& LOPEN) {
				register  x;
				
				 /* try to adjust screen by
				 * opening and closing lines */
				
				if (scrmap[mline] < scrow) {
					for (x = mline; x < SCRLINES; x++) {
						if(scrmap[x]== scrow) {
							vadjust(mline-x,mline);
							break;
						}
					}
				} else {
					vadjust(scrmap[mline]-scrow,mline);
				}
			}

/* Now fix the display of this line.  fmap indicates where we start */


			cp = mkline(scrow);
			if ((scrmap[mline] == scrow) && (col =fmp[scrow])) {
				if (col == LGOOD) goto skipit;
				if ((scrow == curln) && (col > column)) {
					col = column;
					if (col == 0) goto numbline;
				}
				findpos(cp,col);
				cp +=col;
			} else {
numbline:				if(LNOMOD) lnumb(scrow);
				col = 0;
			}
			while (*cp != EOL) {
				if ((col++ == column) && (scrow == curln)) {
					if (mline<SCRLINES) nln = mline;
					ncol = mcol;
				}
				xputc(*cp++);
			}
			if ((col == column) && (scrow == curln)) {
				nln = mline;
				ncol = mcol;
			}
			clrl();
			mline++;
			mcol = 0;
			fmp[scrow] = LGOOD;

/* if we have NDELAY I/O available, see if there is any type ahead */
			
			if (_stdout._cnt *ttywarp > PATIENCE) {
				ttfill(); /* try to fill tty buffer */
				if (MOREIN) {
					clptr = mkline(curln);
					return;
				}
			}

		} else {

/* line on the screen is OK, see if the current cursor position is in it */
			

skipit:			if (scrow == curln) {
				cp = mkline(curln);
					if (findpos(cp,column)) {
					nln = mline;
					ncol = mcol;
				}
			}
			while (scrmap[mline] == scrow) mline++;
			mcol = 0;
		}
/* done re-creating the display, now clean up the rest of the lines */

	}
	maxln = scrow-1;		/* last line on the screen */
	wminln[cwind] = minln;
	wmaxln[cwind] = maxln;
	lastln = mline-1;		/* last display line used */

	if (twowind && (mline == SCRLINES) && (cwind == 0)) {
		prompt(mline,endput);
		scrmap[mline]=0;
	}
	if ((scrow > nlines) || (mline == SCRLINES)) {
		while (mline < SCRLINES+1) {
			if ((twowind == 0) || (mline < SCRLINES) || cwind) {
				if (scrjnk[mline])clrl();
				scrmap[mline++]=0;
			} else mline++;
		}
	}
	if (nln >= 0) {			/* if found current line */
		mgo(nln,ncol);
	} else {
		goto badpos;		/* oops, blew it agaiin */
	}
	if (scrmap[MODLN] != MODHACK) {
		prompt(MODLN,"%s %s  (%d) %s %c %s", myname,version,bfnumb(),bname(),mdchar[modded()], fname());
		scrmap[MODLN] = MODHACK;
		goback();
		if (twowind) {
			col =(wscrlines/2-woff-1);
			prompt(col,endput);
			scrmap[col] = 0; /* SPPML */
			goback();
		}
		if (timemd) dtime(1); /* restore time display */
	}
	clptr = mkline(curln);		/* make sure we get this one */
}

/* display the mode line */

dispmod()

{
	scrmap[MODLN] = 0;
}


ttype()		/* set terminal type */

{
register char *mp;

	mp = getname("Terminal Type? ");
	if (mp == NULL) return;
	sttype(mp);
}

#ifdef TERMCAP

char PC;
char *BC;
char *TE;

 sttype(mp)
 register char *mp;
 {
 	char tbuf[1024];
 	static char bufspace[512];
 	char *pt = bufspace;
 	char *tgetstr();
 	if (tgetent(tbuf, mp) < 1) {
 		IGNORE(error("Can't use terminal type %s",mp));
 		eabort();
 	}
 	UP	= tgetstr("up", &pt);
 	DOWN	= tgetstr("do", &pt);
 	if (DOWN == NULL)
 		DOWN = "\n";
 	BACK	= tgetstr("bc", &pt);
 	if (BACK == NULL && tgetflag("bs"))
 		BACK = "\b";
 	BC = BACK;
 	FORWARD	= tgetstr("nd", &pt);
 	CLEAR	= tgetstr("cl", &pt);
 	CLINE	= tgetstr("ce", &pt);
 	BELL	= "\7";
 	CURAD	= tgetstr("cm", &pt);
 	NOP	= tgetstr("pc", &pt);
 	PC	= *NOP;
 	LOPEN	= tgetstr("al", &pt);
 	LDEL	= tgetstr("dl", &pt);
 	INSERTC	= tgetstr("im", &pt);
 	if (INSERTC == NULL)
 		INSERTC	= tgetstr("ic", &pt);
 	OSERTC	= tgetstr("ei", &pt);
 	DELC	= tgetstr("dc", &pt);
	RSCROLL = tgetstr("sr", &pt);
 	SSCROLL	= tgetstr("nl", &pt);
	if (SSCROLL == NULL) SSCROLL = tgetstr("sf",&pt);
 	SCREG	= tgetstr("cs", &pt);
	CR	= tgetstr("cr", &pt);
	if (CR == NULL) {
		if (tgetflag("nc") == 0)  CR = "";
	}
 	SCINIT	= tgetstr("ti", &pt);
 	TE	= tgetstr("up", &pt);
 	MI	= tgetstr("mi", &pt);
 	SCRWID	= tgetnum("co") - 1;
 	SCRNLIN	= tgetnum("li");
 	SCRWRAP	= tgetflag("am");
	if (SCRNLIN>NSCRLIN) {
 		error (WARN,69,mp);
 		SCRNLIN=NSCRLIN;
	}
 	if (SCRWID>NSCRCOL-1) {
 		error (WARN,69,mp);
 		SCRWID=NSCRCOL-1;
	}
 	if (VCOST == 0) VCOST = 1;
 	SCRLINES = SCRNLIN-4;
 	ECHOL = SCRNLIN-1;
 	MODLN = SCRNLIN-3;

 	acost = dcost(CURAD);
 	lUP = dcost(UP);
 	lDOWN = dcost(DOWN);
 	lBAK = dcost(BACK);
 	lCR = dcost(CR);
	if (SCREG) LOPEN = SCREG; /* make sure we use SCREG */
	if ((NOP == NULL) || (*NOP == 0)) NOP = "\200"; /* null pads get lost */

 	putpad(SCINIT); /* initialize screen */
 	clear();
 }
 
 /* Like putchar but a function which can be passed to putpad */
 pchar(c)
{
 	putchar(c);
}
 /*
  * Print out string str with padding.
  */
 putpad(str)
 char *str;
 {
 	if (str)
 		tputs(str, SREGION, pchar);
}
#else
/* terminal description file parser: */

/* terminal description file contains lines with 

	parameter=data
	
 * where data is either a number of a string */


ttyparse(mp)
char *mp;

{
	register FILE *file;
	char xbuf[128];
	FILE ttyfile;
	char optbuf[128];
	register char *cp;
	register int c;
	int *parmp;
	
	int parm;
	
	
/* find the terminal file and open it */
	
	seprintf(xbuf,termdir,mp);	/* terminal file */
	file = fopen(&ttyfile,xbuf,"r");
	if (file == NULL) {
		error (WARN,43,mp);
		return(0);
	}
	
	/* first find what option we  are setting */

nextparm: cp = optbuf;
	while ((c = getc(file)) != '=') {
		if (c == '\n') goto nextparm; /* comment line */
		if (c == EOF) {
			mclose(file);
			return(1); /* abort during option scan */
		}
		*cp++=c;
	}
	*cp = 0;
	
/* look up parameter in parameter table */
	
	for (parmp = ((int * )&UP), cp = ttydata; *cp; parmp++,cp+=2) {
		if ((optbuf[0]==cp[0])&& (optbuf[1]==cp[1])) {
			
			c = getc(file);
			if ((c >= '0') && (c <= '9')) {
				parm = 0;
				while ((c >= '0') && (c <= '9')) {
					parm = 10*parm + (c-'0');
					c = getc(file);
				}
				*parmp = parm;
				if (c != EOF) goto nextparm;
				mclose(file);
				return(1);
			}
			*parmp = ((int) &ttystrings[ttyptr]);
			
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
/*	error(WARN,68,optbuf);		Don't complain about non-existent capabilities */
	
	goto nextparm;
}

sttype(mp)
register char *mp;

{
	int *parmp;
	
/* First, initialize the tty data */
	


	ttyptr = 0;
	for (parmp = ( (int *) &UP); parmp <= &SRCADD; parmp++) {
		*parmp=0;
	}
	SCRLINES=20;
	SCRWID=80;			/* so we won't bomb */
	CURAD = "";
	
	if (ttyparse(mp)) {
 
		SCRWID--;
		if (SCRNLIN>NSCRLIN) {
			error (WARN,69,mp);
			SCRNLIN=NSCRLIN;
		}
		if (SCRWID>NSCRCOL-1) {
			error (WARN,69,mp);
			SCRWID=NSCRCOL-1;
		}
		if (VCOST == 0) VCOST = 1;
		SCRLINES = SCRNLIN-4;
		ECHOL = SCRNLIN-1;
		MODLN = SCRNLIN-3;

		acost = dcost(CURAD);
		lUP = dcost(UP);
		lDOWN = dcost(DOWN);
		lBAK = dcost(BACK);
		lCR = dcost(CR);
		if (SCREG) LOPEN = SCREG; /* make sure we use SCREG */
		if ((NOP == NULL) || (*NOP == 0)) NOP = "\200"; /* null pads get lost */
		if (SCINIT) {
			PUTS(SCINIT); /* initialize screen */
		}
		clear();
	}
}
#endif
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
		if (infrn < 0) {
			pushin(NULL);
			c = getchar();
			inpop();
		} else c = getchar();
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

/* beep -- obvious */

beep()

{
	if (!NOBEL) PUTS(BELL);			/* print a bell */
}

/* mtop -- move to top of display for message output */

mtop()
{
	mgo(0,0);			/* for messages */
}

/* fclear -- declare the contents of the file invalid */

fclear()
{
	register i;
	scrmap[MODLN] = 0;		/* wipe out mode line */
	for (i = 0; i < NSCRLIN; i++) fmap[i] = 0;
}


/* xclear -- declare the contents of the screen as junk */


xclear()
{
	register i;

	for(i = 0; i < SCRNLIN; i++) scrmap[i]=0;
}

/* two window mode -- enter two window mode, prompt user for buffer */

twind()
{
	if (twowind) return;
	twowind = 1;
	wscrlines = SCRLINES;		/* remember real size */
	cwind = 0;
	owind();			/* enter second window */
	cpbuf();			/* and go to buffer */
}

onewind()
{
	if (twowind) {
		fclear();		/* file map bad */
		twowind = 0;
		cwind = 0;		/* in window 0 */
		wbase = 0;
		SCRLINES = wscrlines;
	}
}

owind()				/* switch windows */

{
	if (!twowind) return;
	fclear();
	windb[cwind] = curbf;
	wminln[cwind] = minln;
	wmaxln[cwind] = maxln;
	
	switch(cwind) {
		
	case 0:
		cwind = 1;
		w1base = wbase = (wscrlines/2)-woff;
		SCRLINES = wscrlines;
		break;
	case 1:
		cwind = 0;
		wbase = 0;
		w1base = (wscrlines/2)-woff;
		SCRLINES=w1base-1;
		break;
	}
	
	minln = wminln[cwind];
	maxln = wmaxln[cwind];
	chbuf(windb[cwind]);
}

wgrow(arg)
int arg;
{
	if (twowind && (SCRLINES-wbase < wscrlines)) {
		fclear();		/* file map bad */
		if (cwind)  {
			wbase-=arg;
			w1base=wbase;
			woff+=arg;
		} else {
			SCRLINES+=arg;
			w1base+=arg;
			woff-=arg;
		}
	}
}
life()

{
#ifdef LIFE
	int i,j,x;
	register char *cp;
	register char *cp1;
	register char *cp2;
	int b,d,p;
	int c;
	char *emptyl="                                                                                 ";
	p=b=d=0;
	OVERW = 1;
	LNOMOD = 0;
	clear();
	for (i = 0; i <=SCRLINES; i++) {
		cp = ckline(i,SCRWID-1);
		for (j = 0; j < SCRWID-1; j++) {
			c = cp[j];
			if (c == EOL) cp[j+1] = EOL;
			if (c != '*')  cp[j] = ' ';
			else p++;
		}
		cp[j] = EOL;
	}
	
	disup();
	move(1,0);
	sputl(1,0,SCRLINES);
	while (gyn("Total: %d, births %d, deaths %d",p,b,d) > 0) {
		p=b=d=0;
		for (i = 1; i < SCRLINES;i++) {
			cp = mkline(i);
			cp1 = mkline(i-1);
			cp2 = mkline(i+1);
			if (i == 1) cp1 = emptyl;
			while (*(cp+3) != EOL) {
				cp++;
				cp1++;
				cp2++;
				x = (07&(*(cp1-1)))+
				(07&(*(cp1)))+
				(07&(*(cp1+1)))+
				(07&(*(cp-1)))+
				(07&(*(cp+1)))+
				(07&(*(cp2-1)))+
				(07&(*(cp2)))+
				(07&(*(cp2+1)));
				if (*cp == '*') {
					if ((x == 4) || (x == 6)) {
						p++;
						(*cp) += 0200;
					} else {
						d++;
					}
				} else {
					if (x == 6) {
						(*cp) += 0200;
						b++;
						p++;
					}
				}
			}
		}
		for (i = 1; i < SCRLINES;i++) {
			cp = mkline(i);
			mcol = 1;
			mline = i-1;
			while (*(++cp) != EOL) {
				if (*cp&0200) {
					*cp = '*';
					mputc('*');
				} else {
					*cp = ' ';
					mputc(' ');
				}
			}
		}
	}
	unprompt();
	move(curln,column);
#endif
}
