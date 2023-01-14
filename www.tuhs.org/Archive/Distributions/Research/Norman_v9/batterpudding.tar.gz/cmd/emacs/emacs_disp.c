/* EMACS_MODES: c !fill */

#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_disp.h"
#ifdef TERMINFO
#include <sys/types.h>
#include <sys/termio.h>
#define SGTTY struct termio
#include <term.h>
#endif

/* character insert */

/* inserts count occurances of the character c in the file */



insertc(count,c)

register char c;
register int count;

{

	if (count < 0) return;			/* Reject bad count */
	while (count--) {
		if ((OVERW) && (c != EOL) && (clptr[column] != EOL)) {

			clptr[column] = c;
			sputl(curln,column,curln);	/* fix this line */
			modify(curln);
			column++;
		} else {
			if(put(c)==0) break;
		}
	}
}

/* insert one character into the file at current position */

put(c)

register char c;
/* Keywords: commands:10 insertion line-representation:50 */
{

	register char tc;
	register int xcol;
	
	if (c == EOL) {
		nl(1);
		return(1);
	}
	xcol=column;
	while (c!= EOL) {
		tc = clptr[xcol];
		clptr[xcol++] = c;
		c = tc;
	}
	modify(curln);
	clptr = ckline(curln,xcol);
	if (clptr == NULL) {
		clptr = mkline(curln);
		*(clptr+xcol-1) = EOL;
		return(0);
	}
	clptr[xcol] = EOL;
	sputl(curln,column,curln);	/* fix this line */
	move(curln,column+1);
	return(1);
}

/* tabc -- tab command */
/* inserts a tab unless notabs mode is on, in which case */
/* it inserts spaces up to next tabstop. */

tabc(count,arg)

register int count;
int arg;

/* Keywords: commands insertion */
{
	if (infrn >= 0) {		/* no dice in macro */
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
	return(1);
}

/* mvc -- move with checking for c<=length(l) */



mvc(l,c)
register int l;
register int c;

/* Keywords: commands:40 movement */
{
	register int x;

	if ((x = leng(l)) < c) move(l,x);
	else move (l,c);
}
/* move to new position in the file */

move (newln,newcol)
register int newln;
register int newcol;

{
/* Keywords: movement buffer-allocation:20 */
	
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

register int x,y;

{

	sgo(mline = x,mcol = y);
}


/* move the display cursor to the specified destination.  sgo attempts
 * to optimize the movement, using single character or absolute
 * positioning */
#ifdef PC
sgo(x,y)
register int x,y;
{

	video(REG(POS_CUR,0),REG(PAGE_0,0),0,REG(x,y));
	
}
#else
sgo(x,y)
/* Keywords: screen-handling the-screen-cursor */
register int x,y;

{
	int mx,my;
	int xcost;		/* cost with all relative movement */
	int ycost;		/* cost with carriage return */
				/* acost is cost of absolute positioning */
	
/* calculate relative costs of various movements.  cost functions  */
/* automatically indicate that un-doable motions have infinite cost */
	
	
	if (umode) unline();		/* out of "underline mode" */

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

	if (xcost == 0) return;		/* Catch spurious calls to sgo */

	if (acost < ycost) {
		if (acost < xcost) {
					/* do absolute positioning */
			if (CURAD) {
				absmove(x,y);
			} else {	/* have relative addrs */
				if (x>scrlin) eprintf (RELDOWN,mx);
				if (x<scrlin) eprintf (RELUP,mx);
				if (y>scrcol) eprintf (RELFORW,my);
				if (y<scrcol) eprintf (RELBACK,my);
			}
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
			if ((FORWARD == NULL)|| (FORWARD[1] && (osert==0)))  {
				if (osert) {
					unsert();
				}
				x = cmap[scrlin] [scrcol];
				if (x == 0) x=cmap[scrlin] [scrcol] = ' ';
				if (x & 0200) pu(x); /* underlined character */
				else {
					if (umode) unline();
					putchar(x); /* re-write to move; */
				}
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
/* Keywords: screen-handling terminal-parameters insertion character-at-a-time */
	PUTS(OSERTC); /* can't stay inserting */
	osert = 0;
}

unline()
{
/* Keywords: screen-handling terminal-parameters underlining */
	PUTS(UEND);
	umode = 0;
}

/* vshift -- shift the display image from top to bottom (inclusive) by x */


vshift(top,bottom,x)
/* Keywords: scrolling the-screen-map terminal-parameters:20 display-update:80 */
int top;
int bottom;
int x;

{
	register int i;
	register int j;
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

register int x;
int xline;
/* Keywords: display-update:90 deletion:50 insertion:50 terminal-parameters:90 the-screen-map scrolling:20 screen-lines screen-handling */
{
	register int i;
	register int j;
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
		
		if (SCREG && CURAD) {		/*if vt100 stype scrolling */
			PUTS(TPARM(SCREG,oldx+XBASE,SCRLINES+XBASE-1)); /*define region*/
			absmove(0,0);
			sgo(SCRLINES-1,0);
			do_n(CLSCROLL,SSCROLL,x); /* Scroll up x lines */
			PUTS(TPARM(SCREG,XBASE,REALBOT+XBASE-1));
			absmove(oldx,0);
		} else {
			do_n(CLDEL,LDEL,x);
			sgo(SCRLINES-x,0);
			do_n(CLOPEN,LOPEN,x);
		}
		sgo(oldx,0);
		vshift (oldx,SCRLINES-1,x);
	} else {
		if (SCREG && CURAD && RSCROLL) {		/* if vt100 style scrolling */

			PUTS(TPARM(SCREG,oldx+XBASE,SCRLINES+XBASE-1)); /*define region */
			absmove(xline,0);
			do_n(CRSCROLL,RSCROLL,x);
			PUTS(TPARM(SCREG,XBASE,REALBOT+XBASE-1));
			absmove(oldx,0);
		} else {
			sgo(SCRLINES-x,0);
			do_n(CLDEL,LDEL,x);
			sgo(oldx,0);
			do_n(CLOPEN,LOPEN,x);
		}
		mgo(oldx,0);
		
		vshift(oldx,SCRLINES-1,-x);
	}
	SREGION=1;
}

do_n(mulcap,sincap,n)
char *mulcap;
register char *sincap;
register int n;

/* Keywords: scrolling:20 screen-lines:40 terminal-parameters argument-processing screen-handling */

{
	if (mulcap && (n>1)) {
					/* If we can do all at once */
#ifdef TERMINFO
		putpad(tparm(mulcap,n));
#else
		PUTS(mulcap,n);
#endif		
	} else {
		while (n--) PUTS(sincap);
	}
}

absmove(x, y)
int	x;
int	y;
{
/* Keywords: terminal-parameters the-screen-cursor screen-handling */
	
#ifdef TERMINFO
	putpad(tparm(CURAD, x, y));
#else
#ifdef TERMCAP
	putpad(tgoto(CURAD, y, x));
#else
	if (SRCADD) {
		eprintf(CURAD,x+XBASE,y+YBASE);
	} else {
		eprintf(CURAD,y+YBASE,x+XBASE);
	}
#endif
#endif
	scrlin=x;
	scrcol=y;
}

/* sscroll -- try to fix display by scrolling */

sscroll(x)

register int x;
/* Keywords: terminal-parameters scrolling screen-handling */
{
	register int i;

	
	if (SSCROLL == NULL || twowind || (SCRLINES-x < 3) || (curbf != disbuf[cwind])) return;


	sgo(REALBOT-1,0);			/* to bottom */
	SREGION=REALBOT;			/* number of lines effected */
	do_n(CLSCROLL,SSCROLL,x);
	SREGION=1;
	vshift (0,SCRNLIN-1,x);
	if (timemd) disptime = 1;	/* Wiped out time display */
}

#endif PC

/* delete chars -- delete characters starting at . */

/* all characters must be on the same line */

delc(count)

register int count;

/* Keywords: deletion line-representation killstack:50 stacking:50 commands:20 */

{
	register char *cp1;
	register char *cp2;

	undel();
	killstk(curln,column,curln,column+count); /* stack even on 0 */
	if (count == 0) {
		beep();
		return(0);			/* nothing to modify */
	}
	cp1 = clptr+column;
	cp2 = cp1+count;
	if (NODEL) {
		while (cp1<cp2) *cp1++=' ';
	} else {
		while ((*cp1++ = *cp2++) != EOL);
	}
	sputl(curln,column,curln);
	modify(curln);
	move(curln,column);
	return(1);
}

/* open up count blank lines in the file */

openl(count)

register int count;


/* Keywords: insertion line-representation memory-allocation:30 text-lines the-screen-map:20 screen-lines:50 */

{

	register int i;
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
	if (curln<nlines) {
		for (i = fbkno; i < NBLOCK; i++) {
			if (hipt[i]>curln) hipt[i]+=count;
			if (lowpt[i]>curln) lowpt[i]+=count;
		}
		for (i = nlines; i > curln; i--) {
			ptrs[i+count] = ptrs[i];
		}
	}
	nlines += count;
	for (i = curln+1; i <= curln+count; i++) ptrs[i] = 0;
	if (clptr[column] != EOL) {
		if(ckline(curln+count,leng(curln)-column)!=NULL) {
			holdin(curln,curln+count);
			lp1 = mkline(curln+count);
			lp2 = lp = mkline(curln) + column;
			while ((*lp1++ = *lp++)!= EOL);
			*lp2 = EOL;
		}
	}
	modify(curln);
	modify(curln+count);

	/* now fix the display */

	oldln = curln;
	oldcol = column;
	if (TABMD && (RARE == 0)) {
		tabjust(curln+count);
	}
	if (disbuf[cwind] == curbf) {

/* Adjust scrmap to reflect the change.  All lines past oldln in the
 * current buffer that show on the screen are bumped up by count.  The
 * current line is bumped only if the whole line moved */
		
		for (i = wbase; i < SCRLINES; i++) {
			if ((scrmap[i]) && ((nln = scrmap[i]&SCRMSK) >= oldln)) {
				if ((nln != oldln) || (oldcol == 0)) scrmap[i] += count;
			}
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

char *
getname(ps)

register char *ps;

/* Keywords: user-interface prompting key-bindings:50 reading:50 filenames:50 commands:20 text-lines:40 the-screen-cursor:30 macro-hooks:10 */

{
	register int i;
	register char c;
	int cnt = 1;
	char *xp;

	if (infrn < 0) {		/* if in macro */
		if(retrvs(fnbuf,FNLEN)>=0){
			return(fnbuf);
		} else return(NULL);
	}
	if (hooks[Read_Name_Hook]) {
		stkstr(ps);
		if (hook(Read_Name_Hook) && (retrvs(fnbuf,FNLEN) >= 0)) {
			return(fnbuf);
		}else return(NULL);
	}
	fnbuf[i=0] = 0;	
	for (;;) {
		if (infrn == 0 ){
			prompt(ECHOL,"%s%s",ps,fnbuf); /* display prompt */
			if (c=fnbuf[i]) { /* If there is stuff past the cursor */

/* UGH,  To find out where the cursor goes, we must  duplicate a lot */
/* of ugly code out of prompt! */
				
				char pbuf[256];
				mline = ECHOL;
				mcol = 0;
				fnbuf[i]=0;
				seprintf(pbuf,"%s%s",ps,fnbuf);
				fnbuf[i]=c;
				xputl(pbuf,0);
			}
			mgo(mline,mcol);
		}
		cnt = 1;
getbak:		donttime=1;		/* Avoid time & mail */
		c = getchar();
		donttime=0;		/* Restore time display */
		switch(map_it[c]) {

		case CQUOTE:			
			c = getchar();
			goto regchar;

		case CCTLU:
			cnt*=4;
			goto getbak; /* Bypass re-initilizing cnt! */
			
		case CEQUIT:
		case CEXIT:
			beep();
			unprompt();
			return(NULL);
		case CCTLX:
			xp = mkline(curln);
			goto wrdin;
		case CYANK:
			xp = fname();
wrdin:			while ((*xp!= 0) && (*xp != EOL)&& (i<FNLEN-1)) {
				cram(fnbuf,i++,*xp++);
			}
			continue;
		default:


/* anything that isn't otherwise mapped comes here.  Most just insert,  */
/* but we do check for '@' */
		
			if (c == '@') {
				fnbuf[0]=0;
				i = 0;
				continue;
			}
			
			if (c>=' ') {
regchar:			if (i<FNLEN-1) cram(fnbuf,i++,c);
				continue;
			} else {
				if ((c != CTRLJ) && (c != CTRLM)) {
					beep;
					continue;
				}
			}
			
/* Fall through to handle newlines re-mapped by the user */
				
		case CNEWLINE:
		case CMETA:
			unprompt();
			if (infrn == 0) mflush(stdout); /* Indicate that we got it */
			return(fnbuf);
		case CREFRESH:
			clear();
			continue;
		case CBDEL:
			while (cnt--) {
				if (i) {
					i--;
					mstrcpy(fnbuf+i,fnbuf+i+1);
				} else beep();
			}
			continue;
		case CFDEL:
			while (cnt--) {
				if (fnbuf[i]) {
					mstrcpy(fnbuf+i,fnbuf+i+1);
				} else beep();
			}
			continue;
		case CXPOSE:
			if (fnbuf[i] && fnbuf[i+1]) {
				c = fnbuf[i];
				fnbuf[i]=fnbuf[i+1];
				fnbuf[++i] = c;
			}
			break;
		case CFORW:
			while (cnt--) {
				if (fnbuf[i]) {
					i++;
				} else beep();
			}
			continue;
		case CBEGIN:
			i = 0;
			continue;
		case CENDL:
			i = lng(fnbuf);
			continue;
		case CBACK:
			i-=cnt;
			if (i<0) {
				beep();
				i=0;
			}
			continue;
		case CEKILL:
			fnbuf[i]=0;
			continue;
		}
	}
}

/* Cram character into the middle of a filename string */

cram(sp,pos,cchar)
register char *sp;
register int pos,cchar;
/* Keywords: user-interface prompting string-handling:40 */

{

	int nc;
	
	while (cchar) {
		if (pos == FNLEN-1) {
			beep();
			break;
		}
		nc = sp[pos];
		sp[pos]=cchar;
		pos++;
		cchar=nc;
	}
	sp[pos]=0;
}

/* gechar: get EMACS character: */
/* returns 0200+c for meta chars, 0400+c for ^X chars */


gechar(ps)
register char *ps;
/* Keywords: user-interface character-at-a-time prompting:40 ^-and-M-processing:70 */
{
	register int c;
	
	prompt1("%s: ",ps);
	c = getchar();
	if (map_it[c] == CMETA) {
		prompt1("%s: M-",ps);
		c = 0200+getchar();
	}
	if (map_it[c] == CCTLX) {
		prompt1("%s: ^X",ps);
		c = 0400+getchar();
	}
	unprompt();
	return(c);
}
/* putout outputs a string (like eprintf) at the current position */

/* position is advanced by one line.  If the position overflows the screen
 * -MORE- is printed, and input  is read.  Any character except ^G
 * continues the display, ^G quits by returning -1 */

/*VARARGS1*/

putout(string,arg1)
char *string;
/* Keywords: prompting:10 informational-displays MORE-processing  */
{
	if (mline>=SCRLINES) {
		prompt1("--  MORE --");
		if ((infrn == 0) && ((getchar()) == CTRLG)) return(-1);
		unprompt();
		mline=0;		/* TOP */
	}
	prompt2(mline,string,&arg1);
	clrl();
	mgo(++mline,0);
	return(0);
}

/* put out a string on ECHOL */
/*VARARGS1*/

prompt1(string,arg1)
char *string;
/* Keywords: prompting */

{
	if (infrn) return;		/* prompt only if input from stdin */
	prompt2(ECHOL,string,&arg1);
	if (mcol) sgo(mline,mcol);	/* Move cursor only if something is there! */
}

/* Keywords: informational-displays time-processing:50 mail-processing:50 */

prompt3(string,arg1)
char *string;
{
	if (infrn) return;
	prompt2(ECHOL-1,string,&arg1);
	scrmap[ECHOL-1] = ECHOHACK;	/* Flag for next unprompt */
}
/* put out a string at a specified line */

/*VARARGS2*/

prompt(ecl,string,arg1)
int ecl;
char *string;
int arg1;
/* Keywords: informational-displays mode-line user-interface:20 */
{
	prompt2(ecl,string,&arg1);
}

/* Internal subroutine for all prompting */

prompt2(ecl,string,argl)
char *string;
register int ecl;
unsigned int *argl;

/* Keywords: prompting informational-displays the-screen-map:40 */

{
	char pbuf[256];

	mline = ecl;
	scrmap[mline] = 0;		/* wipe out previous display */
	mcol = 0;
	sxprintf(pbuf,string,argl);
	xputl(pbuf,0);
	psx = mline;
	psy = mcol;
	clrl();

}
	
/* clear out prompt */

unprompt()
/* Keywords: prompting informational-displays the-screen-map:40 deletion:10 */
{
	if (infrn) return;		/* unprompt only if input from termial */
	mline = ECHOL;
	mcol = 0;
	clrl();
	if (scrmap[ECHOL-1] == ECHOHACK) {
		mline = ECHOL-1;
		clrl();
	}
	if (timemd) disptime = 1;	/* put time back */
}

/* multi-line deleter */

/* deletes the text between current position, and kline, kcol */
/* all deleted text is stacked */

tkill()
/* Keywords: deletion picture-mode:50 killstack:50 stacking:50 the-screen-map:60 */
	/* arguments are kline,kcol */
{
	int oldcol;
	register int x;
	register int y;
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
	undel();
	killstk(curln,column,kline,kcol);
	if (PICMODE) {
		int mincol;
		int maxcol;
		int ln;
		register int c;

		ln = curln;
		if (kcol<column) {
			mincol=kcol;
			maxcol=column;
		} else {
			mincol=column;
			maxcol=kcol;
		}
		while (ln<=kline) {
			cp = mkline(ln);
			c = 0;
			if (NODEL) {
				while (c<maxcol) {
					if (cp[c]==EOL) break;
					if (c>=mincol) cp[c] = ' ';
					c++;
				}
			} else {
				int lln;
				
				lln = leng(ln);
				if (lln > mincol) {
					if (lln < maxcol) {
						cp[mincol]=EOL;
					} else {
						c = mincol;
						while (1) {
							cp[c]=(*(cp+c+maxcol-mincol));
							if (*(cp+(c++))==EOL) break;
						}
					}
				}
			}
			modify(ln);
			ln++;
		}
		sputl(curln,column,REST);
		mvc(curln,column);
		return(1);
	}
	if(ckline(curln,column+leng(kline)-kcol) == NULL) return(0);
	holdin(curln,kline);
	cp1 = mkline(curln)+column;
	cp = mkline(kline)+kcol;
	while ((*cp1++ = *cp++)!= EOL) column++;
	modify(curln);

	/* current line is fixed, fix rest */

	x = kline-curln;

/* Adjust block ranges to account for movement in lines.  high point
 * shifts if its above the deleted region.  Low point shifts if it's
 * in or above */
	
	if (curln+x<nlines) {
		for (y = fbkno; y < NBLOCK; y++) {
			if (hipt[y]>curln+x) hipt[y]-=x;
			if (lowpt[y]>curln) lowpt[y]-=x;
		}
	}
	
	for (y = curln+1+x; y <= nlines; y++) {
		ptrs[y-x] = ptrs[y];
	}
	nlines -= x;

	if (disbuf[cwind] == curbf) {

/* Adjust scrmap to reflect the change.  All lines past oldln in the
 * current buffer that show on the screen are bumped up by count.  The
 * current line is bumped only if the whole line moved */
		
		for (y = wbase; y < SCRLINES; y++) {
			if ((scrmap[y]) && ((nln = scrmap[y]&SCRMSK) >= curln)) {
				if ((nln == curln) && oldcol) continue; /* dont wipe out curln */
				if (nln >= (curln+x)) scrmap[y]-=x; /* map down for lines past the deletion */
				else scrmap[y] = 0; /* this line has been killed */
			}
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
/* Keywords: display-update commands the-screen-cursor:40 windows:20 */
{

	
	if (numarg) dspage(curln-arg,0);
	else clear();
}

/* mark a region of the displayy for fixing.  Arguments are the  */
/* first line and (file) character position to fix, and the last line */

sputl(from, col, to)

int col;
register int from;
int to;
{
/* Keywords: screen-handling:60 commands:30 display-update:30 deletion:50 insertion:50 */
	register int *fmp;
	int wwid;
	
	if (disbuf[cwind] != curbf) return; /* not in this window */
	from -= minln;
	wwid = SCRLINES-wbase;
	fmp = fmap;
	to -= minln;
	if (to > wwid) to = wwid;
	if ((from >= 0) && (from <= wwid)) {
		if (fmp[from] > col) fmp[from]= col;
	}
	while (++from<=to) if (from >= 0) fmp[from]=0;
	return;
}

/* Display output routines.  This is probably less clean than the
 * previous version, but handles underlining and also does a better
 * job with insert/delete character. */

/* The functions are:

	cflush(buf,n) -- put n-mcol characters starting at buf+mcol on the
			screen at current mline,mcol.  This is
			assumed not to overflow one line.
			
	xputl(line,flag,col) -- The hard work.  Maps a string to one
			or more screen lines.  The exact nature of
			the mapping is different for buffer lines
			than for strings, as indicated by flag.
*/


/* Cflush is the main vehicle for the escape of characters.  All
 * charcters that actaually print (as opposed to terminal
 * control) go through here.  The general strategy is that if the
 * desired character is by some odd chance there already, nothing
 * happens.  If the desired character is a space and the terminal is
 * beyond the end of the line, nothing happens.  If insert or delete
 * characters will help bring cp into line with what's on the
 * screen, then it will use them as necessary.  Underlining is
 * handled, including the pain of making it work on various
 * brain-damaged terminals. */

/* These general principals followed in a way that is optimized for
 * common cases, including:  display of a whole line that is already
 * on the screen.  Display of a whole line that does not correspond
 * to what's on the screen, and display of one new character. */


#ifdef PC
#define EOM 0				/* End of string marker */
#else
#define EOM '\337'			/* End of line marker */
#endif PC

cflush(cp,n)

register char *cp;
int n;

/* Keywords: character-at-a-time:40 insertion:30 deletion:30 character-output screen-handling terminal-parameters:20 */
{
	register char *mp;
	register int x;
	register int y;
	int z;
	int jnk;
	int stop;
	
	cp[n] = EOM;			/* Mark end of line */


#ifdef PC
	mcol += vout(mline,mcol,cp+mcol);
	if (scrjnk[mline]< mcol) scrjnk[mline]=mcol;
#else	
	mp = cmap[mline];		/* character map pointer */
	x = jnk = scrjnk[mline];
	while (x < n) mp[x++] = BLANK; /* Clear out the spaces we will write */
	x = mcol;			/* Get mcol into a register */
	stop = 0;
	while (1) {
		while (mp[x]== cp[x]) x++; /* skip rapidly over same stuff */
		if ((mp[x] == 0) && (cp[x] == ' ')) {
			x++;
			continue;
		}
		if ((cp[x] == EOM)) {
			mcol = x;
			if (x < jnk) x = jnk;
			scrjnk[mline] = x;

			if ((scrcol > REALWID) && SCRWRAP) {
				scrlin++;
				scrcol=0;
			}
			return;
		}
		
/* Character was not there, we must do some work here! */

		nmput++;			/* count for stats */
		if ((x != scrcol) || (mline != scrlin)) sgo(mline,x);

/* First, see if we have enough left to worry about insert/delete */
		
		if (jnk-x >= lDELC) {
			if (stop == 0) {

				/* Find the last matching position */
				
				stop = n-1;
				if (stop >= jnk) stop = jnk;
				while ((mp[stop] == 0) && (cp[stop] == ' ')) stop--;
				while (mp[stop] == cp[stop]) stop--;
				stop++;
			}
			
/* Try to use DELC.  Must have at least lDELC characters match within
 * DLOOK characters of the current position. */
  
/* Note:   If there are ANY nulls in the characters to be deleted, we */
/*	   Can't use DELC, since the concept-style terminals won't delete*/
			

			if (DELC&& mp[x]) {
	
				y = 1;
				while (y < DLOOK && mp[x+y]) {
					if (mp[x+y] == cp[x] ) {
						z = dccost * y;
						if (y+z+x >stop) goto NoDEL;
						if (strncmp(cp+x,mp+x+y,z)) goto NoDEL;
		 /* Should be next_del, but this is faster */
		
						/* Delete y characters from under the cursor */
	
						SREGION=SCRWID-x; /* number of char's gobbled */
						if ((DELMODE==1) && (osert == 0)) {
							PUTS(INSERTM);	/* enter "delete mode, UGH!!!" */
							osert = 1;
						}
						if ((DELMODE==2) && osert) {
							unsert();  /* Must not be in insert mode */
						}
						if (x > jnk) jnk = x;
						while (y--) {
	
							PUTS(DELC); 	/*clobber next char */
							jnk = lshift(scrlin,x,jnk-1);
						}
						stop = 0;	/* Don't know where to stop any more */
						SREGION=1;
						x += z;		/* This many matched */
						goto donec;	/* Process next character */
					}
next_del:				y++;
				}
			}


/* Now see if insertion will help.  It will be used if lINSC
 * characters can be matched within ILOOK characters of the cursor. */

NoDEL:		
			if ((INSERTC || INSERTM) && (stop > x+lINSC) && ((cp[x] & 0200) == 0)) {
				y = 1;
				while ((y<ILOOK) && (x+y < stop) && ((cp[x+y]&0200) == 0)) {
					if (cp[x+y] == mp[x]) {

/* Found a match, now figure out how much we should have to see before inserting */
						
						if (INSERTC) z = y * iccost;
/* If inserting is per-character, then each one costs iccost */
/* Otherwise, it costs iccost to go into and out of icmode */
						
						else {
							if (osert) z = 0;
							else z = iccost;
						}
/* After a successful insert, we will need cursor motion, so add that too. */
						z += acost;
						if ((x+y+z)>stop) goto NOINS;
						if (strncmp(cp+x+y,mp+x,z)) goto NOINS;



/* Now insert y characters at point, taking from cp and putting out to
 * screen.  Note that underscores and overstrikes are rejected
 * above, so that nothing here will overstrike.  We just blast out
 * the characters. */
						if (x > jnk) jnk = x;
						SREGION=jnk-x; /* number of char's moved */
						if (umode) unline();
						if ((osert == 0) && INSERTM) {
							PUTS(INSERTM); /* insert mode */
							osert++;
						}

						while (y--) {
							if (INSERTC) PUTS(INSERTC); /* insert prepare */
							putchar(cp[x]);
							if (INSERTP) PUTS(INSERTP); /* insert pad */
							jnk = rshift(scrlin,scrcol+1,mp[scrcol]);
							mp[scrcol++] = cp[x++];
						}
						x+= z;
						SREGION=1;
						stop = 0;	/* Stop is no good any more */
						goto donec;		/* process next character */
					}
next_ins:			y++;
				}
			}
		}
NOINS:	if (osert) {
			unsert();
		}

/* Underscore processing.  relevant variables are EOVER and ULINE. 
 * EOVER should be one if writing over an underlined position clears
 * it, otherwise, make it zero, and we will clear the line to get
 * rid of unwanted underscores.  ULINE is a string parameter that
 * contains whatever is needed to print the character with an
 * underline.  If the terminal underscores naturally, then ULINE is
 * %c<BS>_.  If the terminal has an underscore character mode,
 * then ULINE is underscore mode on, %c, underscore mode off.  If
 * the terminal has an underscore single character command, then
 * ULINE is underscore character %c.  This is inefficient in
 * underscoring giant blocks of text, but this should be rare */


		if (cp[x]&0200){
			pu(cp[x]); /* underlined character */
		} else {
			if (umode) unline();
			if ((mp[x]&0200)&& (!EOVER)) { /* If re-writing won't erase */
				PUTS(CLINE); /* wipe line */
				while (jnk > scrcol) mp[--jnk] = BLANK; /* Wipeout array */
			}
			putchar(cp[x]);
		}
		mp[scrcol++] = cp[x++];
donec:	continue;		
	}
}

/* print an underscored character.  */


pu(c)
register char c;
{
/* Keywords: screen-handling:20 underlining character-output:40 */
	
	register int oc;
	if (osert) unsert();
	c &= 0177;
	if ((c == 0)|| (c == 040)) {
					/* bare underscore */
		oc = cmap[mline][mcol]&0137;  /* Map space to null */
		if (EOVER && (UEND || (oc == 0))) {
			if (umode) unline();
			putchar ('_');
			return;		/* just put out the underscore */
		}
		c = ' ';		/* make sure that we blank over whats there */
	}
	if (UEND == 0) {
		eprintf(ULINE,c);
	} else {
		if (umode == 0) eprintf(ULINE); /* enter "underscore mode" */
		putchar(c);
		umode = 1;
	}
}



/* lshift -- left shift the screen to account for delete character */

lshift (line,col,limit)
int line;
register int col; 
register int limit; 

/* Keywords: character-output the-screen-map deletion */
{
	register char *lp;
	int x;
	
	lp = cmap[line];
	while (col < limit) {
		if ((lp[col]=lp[col+1]) == 0) break;
		col++;
	}
	lp[col] = 0;
	if (IN && (col == SCRWID)) {
		x = lp[col] = cmap[++line] [0];
		limit++;
		if (x) scrjnk[line] = lshift(line,0,scrjnk[line]-1);
	}
	return(limit);
}

/* rshift -- right shift the screen to account for insert character */

rshift (line,col,x)
int line;			/* effected line */
int col;			/* column to start in */
register int x;			/* displaced character that goes in col */
{
/* Keywords: character-output the-screen-map insertion */
	
	int y;
	int jnk;
	register char *stop;
	register char *lp;
	
	lp = cmap[line];
	jnk = scrjnk[line];
	if (jnk <=SCRWID) lp[jnk] = 0;
	
	stop = lp+SCRWID;
	lp += col;
	
	while (x  && (lp<= stop)) {
		y = *lp;
		*lp++ = x;
		x = y;
	}
	if (IN) {
		y = lp - cmap[line];
		if (y > jnk) jnk = y;
		if (x) rshift(line+1,0,x);
	} else {
		if (jnk < SCRWID) jnk++;
	}
	scrjnk[line] = jnk;
	return(jnk);
#endif
}

/* put a "logical line" of characters on the screen at the current position */

/* NOTE:  This is not very clean, but seems necessary to handle all of the
 * various strange possibilities for displaying characters.  We must
 * process a line at a time in order to handle things like
 * underscoring, by which multiple characters map to a single
 * display position.  The general principals here are:  that there
 * two ways of calling it, one with a C string to be printed, and
 * one with a buffer string to be printed.  C strings end in nulls,
 * and cause a special character to be mapped to embedded newlines. 
 * Buffer strings end in newlines, and map normally.  Underscoring
 * is inidicated by the presence of the meta (0200) bit on
 * characters in the display map, so that mapping is done here.  To
 * do this, a whole line is buffered for cput.  Underscoring will
 * work fine so long as the underscores are done one at a time, or
 * the underscores don't wrap lines.  Control and meta characters
 * map to their special emacs equivalents.
 */


xputl(cp,flg,col)

register char *cp;			/* string pointer */
int flg;				/* disposition flag */
int col;

/* Keywords: character-output the-screen-map:20 screen-handling conversions:50 underlining:20 the-screen-cursor:10 ^-and-M-processing:40 */

{
	char cbuf[256];			/* line buffer */
	register int i;
	register int b;
	int mb;
	int a;

	mb = b = mcol;				/* buffer pointer */

	while (1) {
		if (b > mb) mb = b;
		if (b > SCRWID) {

/* Line has wrapped.  Put a ! on the end, and wrap anything that is left
 * over to the next line */

			i = cbuf[SCRWID];
			a = cbuf[SCRWID+1];

/* i is the character overwritten by the !, a is the character */
/* overwritten by the EOM in cflush */

			if (mline>=SCRNLIN-1) {

/* Wrap from last line on screen, fold back up and don't write in
 * the last position! */
				cflush(cbuf,SCRWID);
				mline--; /* don't run overboard */
			} else {
				cbuf[SCRWID]='!';
				cflush(cbuf,SCRWID+1);
				if (flg && PICMODE) {
					if (col>0) hrem = 1+column-col;
					return;
				}
				mline++;
				scrmap[mline] = scrow+SCRCNL; /* mark continuation */
			}
			mcol = 0;
			b = 0; 
			while(b < LNOMOD*LNOWID) cbuf[b++] = ' ';
			cbuf[b++] = i;
			i = SCRWID+2;
			if (SCRWID+1<mb) cbuf[b++] = a;
			while (i < mb) {
				cbuf[b++] = cbuf[i++];
			}
			mb = b;		/* RESET */
		}

/* Process Next character */
		
		i = (*cp++)&0377;		/* next char */
		
		if (flg) {		/* if buffer invocation */
			if (col++ == column) {
				nln = mline;
				ncol = b;
			}
			if (i == EOL) {
				cflush(cbuf,mb);
				return; /* done */
			}
		} else {
			if (i == 0) {
				cflush(cbuf,mb);
				return; /* done */
			}
			if (i == NEWLINE) { /* embedded newline */
				cflush(cbuf,b);
				clrl();
				mb = b = 0;
				if (mline < SCRNLIN) mline++; /* don't overflow */
				scrmap[mline] = 0;
				mcol = 0;
				continue; /* re-loop */
			}
		}
		
		if ((i & META)&& (bit8== 0))  {
			cbuf[b++] ='M';
			cbuf[b++] = '-';
			if (b>mb) mb=b;
			i-= META;
		}
reswitch:	switch(ctype[i&0177]) {
		case UL:
			if (ULINE) i = 0200; /* terminal underlines */

			/* else just a plain old character */
			
		case PLAIN:
			if ((b < mb) && ((i & 0200) || (cbuf[b] & 0200))){
				i |= cbuf[b]; /* overstrike */
			}
			cbuf[b++] = i;
			break;

		case BACKSP:
			if (b>mcol) b--;
			break;
			
	/*Fall through to handle backspace in normal mode */
			
		case CONTRL:
			
			cbuf[b++] = '^';
			i = i^0100;
			goto reswitch; /* Handle things like ^_ correctly! */
		case TAB:
			i = TABSTOP-((b-(LNOMOD*LNOWID))%TABSTOP);
			while (i--) {
				cbuf[b++] = ' ';
			}
			break;
		}
	}
}

/* cput -- put a character in the file, like xputl */


cput(c)
register int c;
/* Keywords: insertion help-messages ^-and-M-processing */
{
	c &= 0377;
	if (c & META) {
		put('M');
		put('-');
		c-= META;
	}
	switch(ctype[c]) {

	case UL:
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
/* Keywords: prompting:10 display-format:20 informational-displays:30 screen-lines clearing */
{
	register int x;
	register int y;
	register int z;
	char *xln;
	int xcol;
#ifdef PC
	char buf[130];
#endif
	x = mline;
	z = mcol;
	y = scrjnk[mline] - mcol;
	if (y > 0) {
#ifdef PC
		buf[y]=0;
		for (x = 0; x < y; x++) buf[x] = ' ';
		x = mline;
		vout(mline,mcol,buf);
#else
		sgo(mline,mcol);			/* go for real */
		if (CLINE) {
			PUTS(CLINE);
		} else {
			xln = cmap[mline];
			xcol = mcol;
			while (y--) {
				if (xln[xcol] && xln[xcol] != ' ') {
					sgo(mline,xcol);
					putchar(' '); /* wipe line */
					if ((scrcol++ >= REALWID) && SCRWRAP) {
						scrlin++;
						scrcol=0;
					}
				}
				xcol++;
			}
		}
#endif		
		scrjnk[x] = z;
	}
}

/* findline -- find a line on the screen */

/* findline makes no judgement as to whether or not the line is correct  */
/* whether or not the screen is OK up to the line */

findline(line)
register int line;

/* Keywords: the-screen-cursor screen-lines screen-handling the-screen-map:10 */

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
register int col;

/* Keywords: the-screen-cursor ^-and-M-processing:50 screen-lines screen-handling the-screen-map:10 picture-mode:10 */
{
	register int i;
	char c;
	
	mcol = LNOMOD*LNOWID;
	i = 0;
	while (i < hcol) if (line[i++] == EOL) return(0);
	for (i = hcol; i < col; i++) {
		if ((c= line[i]) == EOL) return(0);
		if (c & META) mcol += metal;
		switch(ctype[c&0177]) {

		case PLAIN:
		case UL:
			mcol++;
			break;
		case BACKSP:
			if (mcol) mcol--;
			break;
		case CONTRL:
			mcol+=2;
			break;
		case TAB:
			mcol = mcol+TABSTOP-((mcol-(LNOMOD*LNOWID))%TABSTOP);
			break;
		}
		if (mcol>SCRWID) {
			if (PICMODE) {
				hrem = col-i;
				return(0);
			}
			if (++mline>SCRLINES) return(0);
			mcol = mcol-SCRWID;
			if (LNOMOD) mcol+= LNOWID;
		}
	}
	return(1);
}

/* The following function scans the text above "line" to compute
 * it's physical size.  The scan continues until either hstop physical
 * screen lines are found or the line lstop is reached.  The line at
 * which the scan stopped is returned, and the screen area is left
 * in the global "dsize" */

areal(line,hstop,lstop)
/* Keywords: screen-lines display-update:30 screen-handling */


int line;
int hstop;
int lstop;

{
	register char *lp;
	register int col;
	register int c;
	
	dsize = 0;
	while ((line > lstop) && (dsize < hstop)) {
		--line;
		dsize++;
		if (PICMODE) continue; /* Picture mode gets one line/line */
		
		lp = mkline(line);
		col = LNOMOD*LNOWID;
		while ((c = *lp++) != EOL) {
			if (c & META) col += metal;
			switch(ctype[c&0177]) {
	
			case PLAIN:
			case UL:
				col++;
				break;
			case BACKSP:
				if (col) col--;
				break;
			case CONTRL:
				col+=2;
				break;
			case TAB:
				col = col+TABSTOP-((col-(LNOMOD*LNOWID))%TABSTOP);
				break;
			}
			if (col>SCRWID) {
				dsize++;
				col = col-SCRWID;
				if (LNOMOD) col+= LNOWID;
			}
		}
	}
	return(line);
}

/* print a line number(x) */

lnumb(x)

/* Keywords: line-numbers informational-displays screen-handling */
{
	register int i;
	char tbuf[8];
	
	for (i=0; i<8; i++) tbuf[i]=0;
	seprintf(tbuf,"%d",x);
	mcol = 0;						/* make sure were at start of line */
	
	for (i=0; i<LNOWID; i++) {
		if (tbuf[i] == 0) tbuf[i] = ' ';
	}
	cflush(tbuf,LNOWID);		/* output to terminal */
}
/* putin -- insert string into file */


putin(xp)

register char *xp;

/* Keywords: informational-displays help-messages:50 error-messages:50 screen-handling */
{
	register int c;
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

/* dspage  -- re-position display with (line) first.  If  */
/* the move argument is non-zero, the cursor is moved into the middle */
/* of the new display */

dspage(line,moves)
int line;
int moves;

/* Keywords: commands display-update:30 the-screen-cursor scrolling */
{
	if (line<1) {
		if (minln > 1) line=1;
		else {
			beep();
			return(0);
		}
	}
	if (line > nlines) {
		if (maxln < nlines) line = nlines;
		else {
			beep();
			return(0);
		}
	}
	fclear();			/* just blew up file */
	if (disbuf[cwind] != curbf) {
		
/* NOTE:  This is a pain!  display is out of date because we were
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
	minln = line;
	maxln = line+SCRLINES-wbase; /* This is just a guess, but should make it to disup all right. */
	if (maxln > nlines) maxln = nlines;
	if (moves) move((minln+maxln)/2,0);
	return(0);
}
/* nxtpag -- move to next page */

nxtpage(count)

register int count;

/* Keywords: display-update:20 the-screen-cursor:30 movement commands forwards paging */

{

	return(dspage((maxln+1+((count-1)*(SCRLINES-wbase)))-keepg,1));
}

/* move to top of page (current line becomes first line)  */

toppage()

/* Keywords: display-update:20 the-screen-cursor:30 movement scrolling commands backwards */
{
	dspage(curln,0);
}

/* move to previous page */

lstpage(count)

register int count;

/* Keywords: display-update:20 the-screen-cursor:30 movement commands backwards upward-movement:50 paging */
{
	return(dspage((minln-count*(SCRLINES-wbase))+keepg,1));
}

clear()
/* Keywords: clearing screen-handling the-screen-map:40 terminal-parameters:20 terminal-initialization:30 */
{
	register int i;
#ifdef PC

	video(REG(SCRL_UP,0),REG(NORMATB,0),0,REG(SCRNLIN,SCRWID));
#else
	PUTS(CLEAR);
#endif
	for (i = 0; i < SCRNLIN; i++) scrmap[i] = scrjnk[i] = 0;
	scrlin = scrcol = mline = mcol = 0;
	disbuf[0]=disbuf[1]= -1;
	junked = minln = maxln = 0;
	if (timemd) disptime = 1;	/* Wiped out time display */
}


/* didle (display idle) update display and idle for the specified time */

didle(count)

int count;
{
/* Keywords: macro-programming display-update:10 time-processing:20 screen-handling */
	
	if (count > 100) count = 100;	/* limit to 10 seconds */
	pushin(NULL);			/* make sure we will update */
	disup();
	mflush(stdout);
#ifndef PC
	while (count--) PUTS (WAIT1);
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
 * dahead		Flag indicating that we are displaying the line 
 * 			with the cursor ahead of schedule.
 * 
 * The basic algorithm is to firsst check to see if the window is properly
 * positioned to hold the cursor.  If not, a new window is chosen and minln
 * reset.  Then, each line that should be displayed is examined for
 * changes.  Any lines needing updating are redisplay.  A side effect of
 * the scan is to find the screen address of the current line.  If the
 * screen address is found, the mode line and window splitting line are
 * updated (if necessary), and any other lines are blanked.  If the cursor
 * wasn't found, it tries once again to display starting with the current 
 * line.  This can happen only if the line before the current line or the 
 * current line is gigantic, wrapping lots of times around the screen.  
 * Repeated failures signal a fatal error and indicate a bug or an 
 * enormous current line.
 */

/* status display */

char mdchar[2] = {'=', '>'};


disup() 				/* force update display */

{

/* Keywords: clearing:5 screen-handling display-update scrolling:10 insertion:10 deletion:10 */

/* Keywords: lookahead:20 the-screen-cursor:20 the-screen-map:20 screen-lines:10 mode-line:20 macro-hooks:10 */
	register int col;
	int fixed;
	register char *cp;
	long pm;	
	int *fmp;
	int dahead;
	char pmbuf[10];
	char hcbuf[10];
	char smap[NSCRLIN];
	int smapped;
	
	if (MOREIN) return;
	fixed = smapped = 0 ;
	dahead = 0;
	nln = ncol = hrem = -1;

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
		if (hrem>0) {
			hcol = hcol+4+(hrem-hrem%4);
			hrem = -1;
			fclear();
			goto retry;
		}
		if(fixed++) {
			if (fixed > 2) error(FATAL,42); /* can't find cursor */
			minln = curln;
		} else {
			minln = areal(curln,(SCRLINES-wbase)/2,1);
		}
		fclear();		/* file bad */
		if (SCREG || LOPEN) {
			if ((minln > 1) && (minln < curln)) dahead = wbase+dsize;
		}
		maxln = curln;
	}

	if (column<hcol) {
		hcol = column-column%4;
		dispmod();
		fclear();
	}
	
/* Now for the real work, check all of the lines.  A line can be bad if
 * scrmap indicates that its not in the right place on the display
 * or if fmap indicates that it may have changed since being put up.
 */

retry:	fmp = &fmap[-minln];
	mcol = 0;
	for (scrow = minln, mline = wbase; (scrow <= nlines) && (mline < SCRLINES);scrow++) {
		if (scrmap[mline] != scrow) {

/* line is bad, first try to bring it into position by scrolling or by
 * inserting/deleting lines.  This could result in the display being
 * corrected.
 */
#ifndef PC
			if ((smapped == 0) && (LOPEN || (SSCROLL && (mline == 0)))) {
				

/* There is some hope for scrolling, build the display map. */
/* We scan remaining screen lines for ones that may be of use.   */
				
/*	smap[line-minln] = screen line (if any)  */
/* 	scrmap[line] = 0 if known to be useless */

				register int x;

				for (x = 0; x < SCRLINES; x++) smap[x] = 0;
				smapped = scrow+SCRLINES-mline-1;
				for (x = mline+1; x < SCRLINES; x++) {
					col = scrmap[x];
					if ((col & SCRCNL) == 0) {
						if ((col<scrow) || (col > smapped)) {
							scrmap[x] = 0; /* Useless */
						} else {
							smap[col-minln] = x;
						}
					}
				}
			}
			
	/* try to scroll the screen up with newlines */
				
			if ((mline == 0) && SSCROLL && (col=smap[scrow-minln])) {
				sscroll(col);
				smapped = 0;
			} else if(LOPEN) {
				
				/* try to adjust screen by
				 * opening and closing lines */
				
				if (col=smap[scrow-minln]) {
					vadjust(mline-col,mline);
					smapped = 0;
				} else if (((col=scrmap[mline]&SCRMSK)>scrow) && (col <= nlines)) {
					smapped = areal(col,SCRLINES-mline,scrow);
					vadjust(dsize,mline);
					smapped = 0;
				}
			}

#endif
			goto fixline;
		}
		if (fmp[scrow] < LGOOD)  {

fixline: 

/* Check for display-ahead.  If we are on the first line, jump ahead
 * to the current line and show it first, being careful to return
 * when appropriate */


			if (dahead>0) {

/* Pick line for display ahead.  Note that in some cases, the
 * current line is on the screen, in which case we just use it.  If
 * not, we try the middle of the screen.  If this is marked as
 * belonging to some other line, we just ignore dahead. */

				if (findline(curln)) mline = nln;
				else {
					if (scrmap[dahead]) {
						dahead = 0;
						goto disline; 
					}
					if ((scrmap[dahead-1]&SCRMSK) != curln-1) {
						mgo(dahead-1,0);
						scrmap[mline]=0;
						clrl();
					}
					if ((scrmap[dahead+1]&SCRMSK) != curln+1) {					
						mgo(dahead+1,0);
						scrmap[mline]=0;
						clrl();
					}
					mline = dahead;
				}
				dahead = -1;
				scrow = curln;
			}
disline:

/* Now fix the display of this line.  fmap indicates where we start */


			cp = mkline(scrow);
			if ((scrmap[mline] == scrow) && (col =fmp[scrow])) {
				if (col == LGOOD) goto skipit;
				if ((scrow == curln) && (col > column)) {
					col = column;

				}

/* Note that if we have backspace mode on, then we can't start the
 * line in the middle, because the line may have backspaces that
 * span the cursor, or have had them in its previous form.  The only
 * safe thing to do is re-format the whole thing and let cflush
 * throw most of it away. */

				if ((col == 0) || BACKP) goto numbline;
				if (col<hcol) col=hcol;
				if (findpos(cp,col)==0) goto badpos;
				cp +=col;
			} else {
				scrmap[mline] = scrow; /* mark line */
numbline:			if(LNOMOD) lnumb(scrow);
				col = 0;
				while (col < hcol) {
					if (*cp==EOL) break;
					cp++;
					col++;
				}
			}
			xputl(cp,1,((curln==scrow)?col:-1000)); /* process line */
			clrl();
			mline++;
			mcol = 0;
			fmp[scrow] = LGOOD;

/* if we have NDELAY I/O available, see if there is any type ahead */
			
			if (((int) _stdout._cnt *ttywarp) > PATIENCE) {
				ttfill(); /* try to fill tty buffer */
				if (MOREIN) {
					clptr = mkline(curln);
					if (maxln < scrow) maxln = scrow;
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
				} else {
					goto badpos;
				}
			}
			mline++;
			while ((scrmap[mline]) == (scrow|SCRCNL)) mline++;
			mcol = 0;
		}
		if (dahead<0) {
			dahead = 0;
			nln = -1;	/* Make sure we find this line later */
			mline = wbase;
			scrow = minln;
			goto disline; /* If displaying ahead, return to finish display */
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
	if (nln < 0) {
		goto badpos;		/* oops, blew it agaiin */
	}
	if ((scrmap[MODLN] != MODHACK)||PMODE) {

/* Pre-format sections for display-percent mode and horizontal scrolling */

		if (hcol) {
			seprintf(hcbuf,"<%d ",hcol);
		} else hcbuf[0] = 0;
		if (PMODE) {
			pm = (nlines == 1)?1:nlines-1;
			pm = (100L*((long) curln-1))/pm;
			seprintf (pmbuf," %D%%",pm);
		} else pmbuf[0] = 0;
		if (hooks[Mode_Line_Hook]) {
			hook(Mode_Line_Hook);
			retrvs(fnbuf,FNLEN);
			prompt(MODLN,"%s",fnbuf);
		} else {
			
			prompt(MODLN,"%s%s %s  (%d) %s %c %s%s",hcbuf,myname,version,bfnumb(),bname(),mdchar[modded()], fname(),pmbuf);
		}
		if (lgripe && (infrn == 0)) {
			prompt(ECHOL,"I can only use %d lines by %d columns of your screen",SCRNLIN,SCRWID+1);
			lgripe = 0;
		}
		scrmap[MODLN] = MODHACK;
		if (twowind) {
			col = w1base-1;
			prompt(col,endput);
			scrmap[col] = 0; /* SPPML */
		}
		mgo(nln,ncol); 		/* Restore cursor */
	}
	clptr = mkline(curln);		/* make sure we get this one */
	mgo(nln,ncol);
}

/* Initialize screen size */

setsize()
/* Keywords: modes:50 screen-handling display-format terminal-initialization:10 */
{
	
/* Set safe defaults for height and width, and set dependent parameters */

	if (SCRNLIN<=4) SCRNLIN=5;
	if (SCRNLIN>NSCRLIN) {
 		SCRNLIN=NSCRLIN;
 		lgripe++;
	}
 	if (SCRWID>NSCRCOL-1) {
 		lgripe++;
 		SCRWID=NSCRCOL-1;
	}
	if (SCRWID > REALWID) {
		lgripe++;
		SCRWID = REALWID;
	}
	SCRLINES=SCRNLIN-4;
	ECHOL = SCRNLIN-1;
	MODLN = SCRNLIN-3;
}

/* display the mode line */

dispmod()
/* Keywords: mode-line */
{
	scrmap[MODLN] = 0;
}


ttype()		/* set terminal type */

{
/* Keywords: commands terminal-initialization terminal-modes:20 terminal-parameters:50 */
	
	register char *mp;
#ifndef PC
	mp = expenv(getname("Terminal Type? "));
	if (mp == NULL) return;
	sttype(mp);
#endif
}
#if defined(TERMCAP) || defined(TERMINFO)
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
	if (str) {
		tputs(str, SREGION, pchar);
	}
}

#endif
#if defined(TERMINFO)

sttype(mp)
register char *mp;
{
#ifndef SINGLE
	extern struct term _first_term; /* statically allocated terminal structure */
#endif	
	char tbuf[1024];
	static char bufspace[512];
	char *xp;
	char *pt = bufspace;
	int	rc =  1;

	cur_term = 0;			/* Force setupterm to use default */
	if (mp == NULL) mp = dumbterm;
	setupterm(mp,  1, &rc);
	if (rc != 1)  {
		if (mp == dumbterm) {
			write(1,"\n\r\n\rI Can't understand your terminal type\n\r",43);
			quit();
		}
		cur_term = & _first_term; /* Restore terminal pointer */
		IGNORE(error(WARN,86,mp,em_dir));
		return;
	}
	UP	= cursor_up;
	RELUP	= parm_up_cursor;
	DOWN	= cursor_down;
	if (DOWN == NULL)
		DOWN = "\n";
	RELDOWN	= parm_down_cursor;
	BACK	= cursor_left;
	RELBACK	= parm_left_cursor;
	FORWARD	= cursor_right;
	RELFORW	= parm_right_cursor;
	CLEAR	= clear_screen;
	CLINE	= clr_eol;
	BELL	= bell;
	CURAD	= cursor_address;
	NOP	= pad_char;
	LOPEN	= insert_line;
	CLOPEN	= parm_insert_line;
	LDEL	= delete_line;
	CLDEL	= parm_delete_line;
	INSERTC	= insert_character;
	INSERTM	= enter_insert_mode;
	OSERTC	= exit_insert_mode;
	INSERTP = insert_padding;
	IN	= insert_null_glitch;
	/*
	DELMODE	= enter_delete_mode; /* Note that DM is a flag in emacs * /
	*/
	DELC	= delete_character;
	RSCROLL = scroll_reverse;
	SSCROLL	= scroll_forward;
	CLSCROLL= parm_index;
	CRSCROLL= parm_rindex;
	if ((SSCROLL == NULL) && (!eat_newline_glitch)) SSCROLL = "\n";
	SCREG	= change_scroll_region;

	/* UNDERLINING */

	if (transparent_underline) {
		ULINE = "%c_"; /* underline this way */
		EOVER = erase_overstrike;
	}
	else if (underline_char) {
		/* underscore by magic word */
		ULINE = pt;
		pt = strcpy(pt,"%c");
		pt = strcpy(pt,underline_char);
		EOVER = !ceol_standout_glitch;
		pt++;
	} else if (xp = enter_underline_mode) { /* underscore mode */
		ULINE=xp;
		UEND= exit_underline_mode;
		EOVER = !ceol_standout_glitch;
	}
	CR	= carriage_return;
	SCINIT	= enter_ca_mode;
	VEXIT   = exit_ca_mode;
	MI	= move_insert_mode;
	if ((CURAD == NULL) && (CURAD = cursor_mem_address)) SSCROLL=NULL;

	/* Use memory addressing and no scrolling if forced  */

	SCRWID	= columns - 1;
	SCRNLIN	= lines;
	SCRWRAP	= auto_right_margin;
#ifdef JWINSIZE

	/*
	 * Courtesy of Mark Horton - CDB
	 *
	 * ioctls for Blit - you may need to #include <jioctl.h>
	 * This ioctl defines the window size and overrides what
	 * it says in the terminal description file.
	 */
	{
		struct jwinsize w;

		if (ioctl(1, JWINSIZE, &w) != -1) {
			SCRNLIN = w.bytesy;
			SCRWID = w.bytesx - 1;
		}
	}
	if (getenv ("LINES"))
		IGNORE (nscan (getenv ("LINES"), &SCRNLIN));
	if (getenv ("COLUMNS"))
	{
		IGNORE (nscan (getenv ("COLUMNS"), &SCRWID));
		SCRWID--;
	}

#endif
	REALWID=SCRWID;
	REALBOT=SCRNLIN;
	setsize();
	if (IN) BLANK= 0;
	else BLANK= ' ';
	if (VCOST == 0) VCOST = 1;
	if (CURAD) {
		acost = dcost(CURAD);
	} else {
		acost = dcost(RELUP)+dcost(RELFORW);
	}
	dccost = dcost(DELC);
	if (INSERTM) iccost = dcost(INSERTM)+dcost(OSERTC);
	else iccost = dcost (INSERTC);
	lUP = dcost(UP);
	lDOWN = dcost(DOWN);
	lBAK = dcost(BACK);
	lCR = dcost(CR);
	if ((NOP == NULL) || (*NOP == 0)) NOP = "\200"; /* null pads get lost */
	if (ULINE) BACKP = 1;		/* Set backspace mode if terminal underscores */
	vinit();
	TERMIQ = 0;		/* Compute terminal's IQ */
	if (CURAD) TERMIQ += 1;
	if (CLINE) TERMIQ += 2;
	if (LOPEN || SCREG) TERMIQ += 4;
	if (INSERTC || INSERTM) TERMIQ += 8; 
	clear();
}

/* for costing out terminfo capabilities */
static int TIcount;
/*ARGSUSED*/

int TIcost(c)
	char	c;
{
	++TIcount;
}
#else
#ifdef TERMCAP

char PC;
char *BC;
char *TE;

sttype(mp)
register char *mp;
 {
	char tbuf[1024];
	static char bufspace[512];
	char *xp;
	char *pt = bufspace;
	char *tgetstr();
	if (mp == NULL) mp = dumbterm;
	if (tgetent(tbuf, mp) < 1) {
		if (mp == dumbterm) {
			write(1,"\n\r\n\rI Can't understand your terminal type\n\r",43);
			quit();
		}
		IGNORE(error(WARN,86,mp,em_dir));
		return;
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
	if(NOP != NULL) PC = *NOP;
	LOPEN	= tgetstr("al", &pt);
	CLOPEN	= tgetstr("AL", &pt);
	LDEL	= tgetstr("dl", &pt);
	CLDEL	= tgetstr("DL", &pt);
	INSERTC	= tgetstr("ic", &pt);
	INSERTM	= tgetstr("im", &pt);
	OSERTC	= tgetstr("ei", &pt);
	INSERTP = tgetstr("ip", &pt);
	IN	= tgetflag("in", &pt);
	DELMODE	= (tgetstr("dm", &pt)==NULL); /* Note that DM is a flag in emacs */
	DELC	= tgetstr("dc", &pt);
	RSCROLL = tgetstr("sr", &pt);
	SSCROLL	= tgetstr("nl", &pt);
	CLSCROLL = tgetstr("SF", &pt);	
	if (SSCROLL == NULL) SSCROLL = tgetstr("sf",&pt);
	if ((SSCROLL == NULL) && (tgetflag("xn")== NULL)) SSCROLL = "\n";
	SCREG	= tgetstr("cs", &pt);

/* UNDERLINING */

	if (tgetflag("ul")) {
		ULINE = "%c_"; /* underline this way */
		EOVER = tgetflag("eo");
	}
	else if (xp = tgetstr("uc",&pt)) {
					/* underscore by magic word */
		ULINE = pt;
		pt = mstrcpy(pt,"%c");
		pt = mstrcpy(pt,xp);
		EOVER = (tgetflag("xs") == 0);
		pt++;
	} else if (xp = tgetstr("us",&pt)) { /* underscore mode */
		ULINE=xp;
		UEND= tgetstr("ue",&pt);
		EOVER = (tgetflag("xs") == 0);
	}
	CR	= tgetstr("cr", &pt);
	if (CR == NULL) {
		if (tgetflag("nc") == 0)  CR = "\015";
	}
	SCINIT	= tgetstr("ti", &pt);
	TE	= tgetstr("up", &pt);
	MI	= tgetflag("mi", &pt);
	SCRWID	= tgetnum("co") - 1;
	SCRNLIN	= tgetnum("li");
	SCRWRAP	= tgetflag("am");
#ifdef JWINSIZE

	/*
	 * Courtesy of Mark Horton - CDB
	 *
	 * ioctls for Blit - you may need to #include <jioctl.h>
	 * This ioctl defines the window size and overrides what
	 * it says in the terminal description file.
	 */
	{
		struct jwinsize w;

		if (ioctl(1, JWINSIZE, &w) != -1) {
			SCRNLIN = w.bytesy;
			SCRWID = w.bytesx - 1;
		}
	}
	if (getenv ("LINES"))
		IGNORE (nscan (getenv ("LINES"), &SCRNLIN));
	if (getenv ("COLUMNS"))
	{
		IGNORE (nscan (getenv ("COLUMNS"), &SCRWID));
		SCRWID--;
	}

#endif
	REALWID=SCRWID;
	REALBOT=SCRNLIN;
	setsize();
 	if (VCOST == 0) VCOST = 1;
	if (IN) BLANK= 0;
	else BLANK= ' ';
	if (CURAD) {
		acost = dcost(CURAD);
	} else {
		acost = dcost(RELUP)+dcost(RELFORW);
	}
	dccost = dcost(DELC);
	if (INSERTM) iccost = dcost(INSERTM)+dcost(OSERTC);
	else iccost = dcost (INSERTC);
 	lUP = dcost(UP);
 	lDOWN = dcost(DOWN);
 	lBAK = dcost(BACK);
 	lCR = dcost(CR);
	if (SCREG) LOPEN = SCREG; /* make sure we use SCREG */
	if ((NOP == NULL) || (*NOP == 0)) NOP = "\200"; /* null pads get lost */
	if (ULINE) BACKP = 1;		/* Set backspace mode if terminal underscores */
	vinit();			/* Initialize screen */
	
	TERMIQ = 0;		/* Compute terminal's IQ */
	if (CURAD) TERMIQ += 1;
	if (CLINE) TERMIQ += 2;
	if (LOPEN || SCREG) TERMIQ += 4;
	if (INSERTC || INSERTM) TERMIQ += 8; 
 	clear();
}
#else
#ifndef PC

/* terminal description file parser: */

/* terminal description file contains lines with 

	parameter=data
	
 * where data is either a number of a string */


ttyparse(mp)
char *mp;

/* Keywords: conversions:20 terminal-parameters terminal-initialization reading */

{
	register FILE *file;
	char xbuf[128];
	FILE ttyfile[1];
#define OPTSIZE 256
	char optbuf[OPTSIZE];
	register char *cp;
	register int c;
#ifdef PORTEXT
	int parmp;
#else
	int *parmp;
#endif	
	int parm;
	
	
/* find the terminal file and open it */
	
	if (mp) {
		seprintf(xbuf,"%s/terminals/%s",em_dir,mp);	/* terminal file */
		file = xopen(ttyfile,xbuf,"r");
		if (file == NULL) {
			file = xopen(ttyfile,mp,"r"); /* Try full pathname */
		}
		if (file == NULL) {
badterm:		error (WARN,43,mp,em_dir);
			return(0);
		}
	} else {

/* This code restores the terminal to a "sane" default */
		
		file = ttyfile;
		ttyfile[0]._frn = open("/dev/null",0);
		ttyfile[0]._cnt = lng(nulltty);
		ttyfile[0]._ptr = nulltty;
	}
	/* first find what option we  are setting */

nextparm: cp = optbuf;
	while ((c = getc(file)) != '=') {
		if (c == '\n') goto nextparm; /* comment line */
		if (c == EOF) {
			mclose(file);
			return(1); /* abort during option scan */
		}
		if (cp-optbuf >= OPTSIZE) goto badterm; /* OOPS! */
		*cp++=c;
	}
	*cp = 0;
	
/* look up parameter in parameter table */
	
#ifdef PORTEXT
	for (parmp = 0, cp = ttydata; *cp; parmp++,cp+=2) {
#else
	for (parmp = ((int * )&UP), cp = ttydata; *cp; parmp++,cp+=2) {
#endif
		if ((optbuf[0]==cp[0])&& (optbuf[1]==cp[1])) {
			
			c = getc(file);
			if ((c >= '0') && (c <= '9')) {
				parm = 0;
				while ((c >= '0') && (c <= '9')) {
					parm = 10*parm + (c-'0');
					c = getc(file);
				}
#ifdef PORTEXT
				*(parmptr[parmp]) = parm;
#else
				*parmp = parm;
#endif
				if (c != EOF) goto nextparm;
				mclose(file);
				return(1);
			}
#ifdef PORTEXT
			*(parmptr[parmp]) = ((int) &ttystrings[ttyptr]);
#else			
			*parmp = ((int) &ttystrings[ttyptr]);
#endif			
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
#endif
sttype(mp)
register char *mp;

/* Keywords: terminal-parameters terminal-initialization terminal-modes:20 string-handling:20 statistics:10 */



{
#ifdef PORTEXT
	int parmp;
#else
	int *parmp;
#endif
/* First, initialize the tty data */
	
	ttyptr = 0;
#ifdef PORTEXT
	for (parmp = 0; parmptr[parmp]; parmp++) {
		*(parmptr[parmp])=0;
	}
#else
	for (parmp = ( (int *) &UP); parmp <= &DELMODE; parmp++) {
		*parmp=0;
	}
#endif
#ifdef PC

	SCRWID = 79;
	SCRNLIN = 24;
	ECHOL = 23;
	MODLN = 21;
	SCRLINES = 20;
	CLINE = "";
	clear();
#else	
	if (ttyparse(mp)) {

#ifdef JWINSIZE

		/*
		 * Courtesy of Mark Horton - CDB
		 *
		 * ioctls for Blit - you may need to #include <jioctl.h>
	 	 * This ioctl defines the window size and overrides what
		 * it says in the terminal description file.
		 */
		{
			struct jwinsize w;

			if (ioctl(1, JWINSIZE, &w) != -1) {
				SCRNLIN = w.bytesy;
				SCRWID = w.bytesx;
			}
		}
/*
		if (getenv ("LINES"))
			IGNORE (nscan (getenv ("LINES"), &SCRNLIN));
		if (getenv ("COLUMNS"))
		{
			IGNORE (nscan (getenv ("COLUMNS"), &SCRWID));
		}
*/
#endif
		SCRWID--;
		REALWID=SCRWID;
		REALBOT=SCRNLIN;
		setsize();
		if (IN) BLANK= 0;
		else BLANK= ' ';
		if (VCOST == 0) VCOST = 1;
		if (CURAD) {
			acost = dcost(CURAD);
		} else {
			acost = dcost(RELUP)+dcost(RELFORW);
		}
		dccost = dcost(DELC);
		if (INSERTM) iccost = dcost(INSERTM)+dcost(OSERTC);
		if (INSERTM) iccost = 1;
		else iccost = dcost (INSERTC);

		lUP = dcost(UP);
		lDOWN = dcost(DOWN);
		lBAK = dcost(BACK);
		lCR = dcost(CR);
		if (ULINE) BACKP = 1;		/* Set backspace mode if terminal underscores */
		if (OSERTC && (INSERTM == 0)) {
			INSERTM=INSERTC;		/* old style insert modes */
			INSERTC=0;
		}
		if (SCREG) LOPEN = SCREG; /* make sure we use SCREG */
		if ((NOP == NULL) || (*NOP == 0)) NOP = "\200"; /* null pads get lost */
		vinit();
		TERMIQ = 0;		/* Compute terminal's IQ */
		if (CURAD) TERMIQ += 1;
		if (CLINE) TERMIQ += 2;
		if (LOPEN || SCREG) TERMIQ += 4;
		if (INSERTC || INSERTM) TERMIQ += 8; 

		clear();
	}
#endif
}
#endif
#endif
#ifndef PC
/* dcost -- calculate display cost of a string */

dcost(sp)
register char *sp;
{
/* Keywords: terminal-parameters terminal-initialization */
	register int dc;
	char dcbuf[512];		/* buffer for string */
	
	if (sp == NULL) return(1000); /* infinite cost for missing capability */
#ifdef TERMINFO
	TIcount=0;
	tputs(tparm(sp,10,10),10,TIcost);
	return(TIcount);
#else
#ifdef TERMCAP	
	dc = 0;
	while (*sp) {
		if (*sp++ != '%') dc++;
	}
	return(dc);
#else
	seprintf(dcbuf,sp,10,10);	/* expand a 'typical' example */
	return(lng(dcbuf));		/* return its length */
#endif	
#endif
}
#endif
/* yes or no question */
/*VARARGS1*/
int
gyn(string,arg1)

char *string;
char *arg1;
{
/* Keywords: prompting user-interface yes-no-questions */
	
	register char c;
	
	while (1) {
		prompt2(ECHOL,string,&arg1);
		sgo(mline,mcol);
		if (infrn < 0) {
			pushin(NULL);
			c = getchar();
			inpop();
		} else c = getchar();
		donttime = 0;		/* Re-enable time mode */
		switch(c) {
			
		case 'y':
		case 'Y':
		case ' ':
			return(1);
		case CTRLZ:	/* This is necessary to insure exit */
		case 'n':
		case 'N':
		case RUBOUT:
			unprompt();
			return(0);
		case CTRLG:
			unprompt();
			return(-1);
		default:
			donttime = 1; /* Avoid time mode */
			prompt3("y for yes, n for no, ^G to quit");
		}
	}
};

/* beep -- obvious */

beep()

{
	
/* Keywords: bell-ringing terminal-parameters:20 commands:20 error-messages:10 */
	
#ifdef PC

	if (!NOBEL) video(REG(WR_TTY,BELL),REG(PAGE_0,NORMATB),0,0);
	
	
#else
	if (!NOBEL) PUTS(BELL);			/* print a bell */
#endif	
}

/* mtop -- move to top of display for message output */

mtop()
{
/* Keywords: the-screen-cursor informational-displays MORE-processing */
	mgo(0,0);			/* for messages */
}

/* fclear -- declare the contents of the file invalid */

fclear()

/* Keywords: screen-handling the-screen-map clearing terminal-initialization:20 */

{
	register int i;
	scrmap[MODLN] = 0;		/* wipe out mode line */
	for (i = 0; i < NSCRLIN; i++) fmap[i] = 0;
}

/* dclear-- cause screen to be cleared at next refresh */

dclear()
{
/* Keywords: screen-handling clearing break-handling:20 */
	junked++;
	minln=0;
}

/* xclear -- declare the contents of the screen as junk */


xclear()
{
/* Keywords: screen-handling the-screen-map clearing terminal-initialization:20 */
	register int i;

	for(i = 0; i < SCRNLIN; i++) scrmap[i]=0;
}

/* two window mode -- enter two window mode, prompt user for buffer */

twind()
{
/* Keywords: windows commands screen-handling:20 */

	if (twowind) return;
	twowind = 1;
	wscrlines = SCRLINES;		/* remember real size */
	w1base = (wscrlines/2)-woff; /* Compute start of window 2 */
	cwind = 0;
	owind();			/* enter second window */
	cpbuf(1);			/* and go to buffer */
}

onewind()
{
/* Keywords: windows commands screen-handling:20 */
	
	if (twowind) {
		fclear();		/* file map bad */
		twowind = 0;
		cwind = 0;		/* in window 0 */
		wbase = 0;
		SCRLINES = wscrlines;
	}
}

/* Return buffer in other window or -1 */

windbuf()

/* Keywords: windows commands buffers killing: */
{
	if (twind) return(windb[1-cwind]);
	else return(-1);
}
owind()				/* switch windows */
	/* returns -1 if not in two window mode, 0 if different buffer,
	and 1 if same buffer */
/* Keywords: windows commands screen-handling:20 */
{
	if (!twowind) return(-1);
	fclear();
	windb[cwind] = curbf;
	wminln[cwind] = minln;
	wmaxln[cwind] = maxln;
	
	switch(cwind) {
		
	case 0:
		cwind = 1;
		wbase = w1base;
		SCRLINES = wscrlines;
		break;
	case 1:
		cwind = 0;
		wbase = 0;
		SCRLINES=w1base-1;
		break;
	}
	
	minln = wminln[cwind];
	maxln = wmaxln[cwind];
	lastln = SCRLINES-1;		/* This is not accurate, but shouldn't matter much */
	chbuf(windb[cwind]);
	if (windb[0]==windb[1]) return(1);
	return(0) ;
}

wgrow(arg)
register int arg;
/* Keywords: windows screen-lines commands insertion:50 screen-handling:20 */
{
	fclear();		/* file map bad */
	if (twowind) {
		if (cwind) w1base -= arg;
		else w1base += arg;
		if (w1base < 2) w1base = 2;
		if (w1base > wscrlines-1) w1base = wscrlines-1;
		woff = (wscrlines/2)-w1base;
		if (cwind) {
			wbase = w1base;
		} else {
			SCRLINES = w1base-1;
		}
	} else {
		arg += SCRLINES;
		if ((arg > 0) && (arg < MODLN)) SCRLINES = arg;
	}
}

/* Mousing Specials */

/* Goto screen position at line x, collumn y.  arg is decoded as:
 *	x = arg mod 128.
 *	y = arg /128.
			(128 is more lines than I can imagine on a screen.)
			
 * Will switch windows, but fails if x is an illegal position. */

go_xy(arg)
int arg;
/* Keywords: screen-positioning commands macro-programming:40 */
{
	register int x,y,c;
	int linpos;

	x = arg&127;
	y = ((unsigned) arg) >>7;
	if ((x <wbase) || (x>= SCRLINES)) {
		if (twowind) {
			if ((x >= 0) && (x < wscrlines)) {
				owind();
			} else return(0); /* Can't mouse out of the display */
		} else return(0);
	}
	linpos = scrmap[x]&SCRMSK;
	if (linpos== 0) {
		if (PICMODE && (maxln == nlines)) {
			linpos = maxln+x-lastln;/* If nothing there, punt to end of file */
			move(linpos,0);
		} else if (x > lastln) {
			linpos = maxln;
		} else return(0); 	/* PUNT */
	}
	
	if (findline(linpos)) mline = nln;
	else mline = x;

	column = 0;
	mcol = LNOMOD*LNOWID-hcol;
	if (y<mcol) y = mcol;
	clptr=mkline(linpos);
	while ((mline < x) || ((mline == x) && (mcol <= y))) {
		c = (clptr[column++]&0377);
		if (c == EOL) {
			if (PICMODE) {
				curln=linpos;
				column--;
				insertc(y-mcol+1,' ');
			}
			break;
		}
		if (c & META) mcol+= metal;
		switch(ctype[c& 0177]) {

		case PLAIN:
		case UL:
			mcol++;
			break;
		case BACKSP:
			if (mcol) mcol--;
			break;
		case CONTRL:
			mcol += 2;
			break;
		case TAB:
			mcol+= TABSTOP-((mcol-(LNOMOD*LNOWID))%TABSTOP);
			break;
		}
		if (mcol>SCRWID) {
			mline++;
			mcol = mcol-SCRWID;
			if (LNOMOD) mcol+= LNOWID;
		}
	}
	move(linpos,column-1);
	return(1);
}

vinit()
/* Keywords: terminal-parameters terminal-initialization character-output */
{
#ifndef PC
#ifdef COMPRESS
	DOCOMP=CMPON;
#endif

#ifdef TERMINFO
	if (cur_term == NULL) return; /* Catch if called too soon */
	if (enter_ca_mode)  {
		putpad(enter_ca_mode);
	}
	if (keypad_xmit)  {
		putpad(keypad_xmit);
	}
#else
	if (SCINIT) PUTS(SCINIT);
#endif
#endif
}

vexit ()
{
/* Keywords: terminal-parameters exit-processing character-output */
#ifndef PC
#ifdef COMPRESS
	DOCOMP=0;
#endif
#ifdef TERMINFO
	if (exit_ca_mode)  {
		putpad(exit_ca_mode);
		}
	if (keypad_local)  {
		putpad(keypad_local);
	}
#else
	if (VEXIT) PUTS(VEXIT);
#endif
#endif
}
