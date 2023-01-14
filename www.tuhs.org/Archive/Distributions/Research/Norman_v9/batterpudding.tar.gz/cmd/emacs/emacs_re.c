#include "emacs_gb.h"
#include "emacs_io.h"


/* EMACS_MODES: c !fill */

#define GETC() (*sp++)
#define PEEKC() (*sp)
#define UNGETC(c) (--sp)
#define RETURN(c) return(NULL);
#define ERROR(c) return(c);

extern char casem[];
extern char bits[];

#define RESIZE 256

/* basic comparison types */

#define CCHR 1				/* single char */
#define CDOT 2				/* any char */
#define CCL 3				/* multiple guess */
#define CBLST 4				/* last expression */
#define CDOL 5				/* $ */
#define CEOF 6				/* end of line */

/* markers */

#define CBRA 8				/* \( marker */
#define CKET 9				/* \) marker */
#define CBWD 10				/* start of word */
#define CEWD 11				/* end of word */

/* modifiers */

#define BASTYP 15			/* mask for base type */

#define RNGE 16				/* range of types */

#define	NBRA	9

#define PLACE(c)	ep[c >> 3] ^= bittab[c & 07]
#define ISTHERE(c)	(ep[c >> 3] & bittab[c & 07])

char	*braslist[NBRA];
char	*braelist[NBRA];
int	nbra, ebra;
char	*loc1;
int closed;
int	loc2;

int	low;
int	size;

char	bittab[] = {
	1,
	2,
	4,
	8,
	16,
	32,
	64,
	128
};

int
compile(sp, ep)
register char *ep;
register char *sp;

/* Keywords: regular-expressions searching parsing */
{
	register int c;
	char *endbuf;
	char *lastep = sp;
	char bracket[NBRA], *bracketp;
	char neg;
	int lc;
	int i, cflg;

	lastep = NULL;
	bracketp = bracket;
	closed = nbra = ebra = 0;

	endbuf = ep + RESIZE;

	for (;;) {
		if (ep >= endbuf)
			ERROR(62);
		if((c = GETC()) != '*' && (c != '+') &&
			((c != '\\') || (PEEKC() != '{')))
			lastep = ep;
		if (c == 0) {
			*ep++ = CEOF;
			RETURN(ep);
		}
		switch (c) {

		case '.':
			*ep++ = CDOT;
			continue;

		case '\n':
			ERROR(55);
		case '*':
			if (lastep==0 || *lastep==CBRA || *lastep==CKET)
				goto defchar;
			*lastep |= RNGE;
			*ep++=0;
			*ep++=255;
			continue;
		case '+':
			if (lastep==0 ) 
				goto defchar;
			*lastep |= RNGE;
			*ep++ = 1;
			*ep++ = 255;
			continue;

		case '$':
			if(PEEKC() != 0)
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			if(&ep[17] >= endbuf)
				ERROR(62);

			*ep++ = CCL;
			lc = 0;
			if((c = GETC()) == '^') {
				neg = -1;
				c = GETC();
			} else neg = 0;
			for(i = 0; i < 16; i++)
				ep[i] = neg;


			do {
				if(c == '\0')
					ERROR(61);
				if(c == '-' && (lc != 0) && (PEEKC() != ']')) {
					c = GETC();
				} else lc = c;
				while(lc <= c) {
					PLACE(lc);
					lc++;
				}
			} while((c = GETC()) != ']');
			ep[1] &= 0373; /* make sure that newline is not matched */
			ep += 16;			
			continue;

		case '\\':
			switch(c = GETC()) {

			case '(':
				if(nbra >= NBRA)
					ERROR(57);
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;

			case ')':
				if(bracketp <= bracket || ++ebra != nbra)
					ERROR(56);
				*ep++ = CKET;
				*ep++ = *--bracketp;
				closed++;
				continue;

			case '<':
				*ep++ = CBWD;
				continue;
			case '>':
				*ep++ = CEWD;
				continue;
				
			case '{':
				if(lastep == (char *) (0))
					goto defchar;
				*lastep |= RNGE;
				cflg = 0;
			nlim:
				c = GETC();
				if (c == '\\') {
					i = 255; /* infinity */
				} else {
					i = 0;
					while('0' <= c && c <= '9') {
						i = 10 * i + (c - '0');
						c = GETC();
					}
				}
				if (i > 255) ERROR(52);
				*ep++ = i;
				if (c == ',') {
					if(cflg++) ERROR(58);
					goto nlim; /* get 2'nd number */
				}
				if((c != '\\') || (GETC() != '}')) ERROR(59);
				if(!cflg)	/* one number */
					*ep++ = i;
				else if((ep[-1] & 0377) < (ep[-2] & 0377))
					ERROR(60);
				continue;

			case '\n':
				ERROR(55);

			case 'n':
				c = '\n';
				goto defchar;

			default:
				if(c >= '1' && c <= '9') {
					if((c -= '1') >= closed)
						ERROR(54);
					*ep++ = CBLST;
					*ep++ = c;
					continue;
				}
			}
			/* Drop through to default to use \ to turn off special chars */

		defchar:
		default:
			lastep = ep;
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
}


advance(lp, ep)
register char *lp, *ep;

/* Keywords: regular-expressions searching star-processing:10 */

{
	register char *curlp;
	char c;
	int typ;
	char *bbeg;
	int ct;

	for (;;) switch (typ = *ep++) {

	case CCHR:
		if (*ep++ == casem[(*lp++)&0177])
			continue;
		return(0);

	case CDOT:
		if (*lp++ != EOL)
			continue;
		return(0);

	case CBWD:
		if (bits[lp[-1]] & 01) continue;
		return(0);
	case CEWD:
		if (bits[*lp] & 01) continue;
		return(0);
	case CDOL:
		if (*lp==EOL) {
			continue;
		}
		return(0);

	case CEOF:
		loc2 = lp-klptr;
		return(1);

	case CCL:
		c = casem[*lp++ & 0177];
		if(ISTHERE(c)) {
			ep += 16;
			continue;
		}
		return(0);
	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CCHR|RNGE:
		c = *ep++;
		getrnge(ep);
grnge:		ct = 0;
		curlp = lp;
		while(ct < size) {
			switch(typ&BASTYP) {
				
			case CCHR: if(casem[(*lp++)&0177] != c) goto broke;
				break;
			case CDOT: if (*lp++ == EOL) goto broke;
				break;
			case CCL: c = casem[(*lp++)&0177];
				if(!ISTHERE(c)) goto broke;
				break;
			}
			if (++ct == low) curlp = lp;
		}
broke:		if (ct < low) return(0); /* too few */

		if (size == ct) lp++;	/* didn't do last compare */
		if ((typ&BASTYP) == CCL) ep += 16;
		ep += 2;

		while (lp > curlp) {
			if (advance(--lp, ep)) return(1);
		}
		return(0);
	case CDOT|RNGE:
		getrnge(ep);
		goto grnge;
	case CCL|RNGE:
		getrnge(ep + 16);
		goto grnge;
	case CBLST:
		bbeg = braslist[*ep];
		ct = braelist[*ep++] - bbeg;

		if(ecmp(bbeg, lp, ct)) {
			lp += ct;
			continue;
		}
		return(0);

	case CBLST|RNGE:
		bbeg = braslist[*ep];
		ct = braelist[*ep++] - bbeg;
		ep+=2;
		curlp = lp;
		while(ecmp(bbeg, lp, ct))
			lp += ct;

		while(lp >= curlp) {
			if(advance(lp, ep))	return(1);
			lp -= ct;
		}
		return(0);
	}
}

getrnge(str)
register char *str;
/* Keywords: regular-expressions searching:20 star-processing */

{
	low = *str++ & 0377;
	size = *str&0377;
	if (size == 255) size = 2000;
}

ecmp(a, b, count)
register char	*a, *b;
register int count;

/* Keywords: star-processing regular-expressions searching:10 */

{
	while(count--)
		if(*a++ != *b++)	return(0);
	return(1);
}



rsrch(arg)

/* Keywords: searching regular-expressions commands key-bindings:10 */

int arg;
{
	register int dir;
	extern char presst[];
	register char *xp;
	register char c;
	int ol,oc;
	int x,y;	
	
	if (arg < 0) {
		dir = -1;
		arg = -arg;
	} else {
		dir = 1;
	}
	ol = curln;
	oc = column;
	if (xp = getname("expr: ")) {	/* get expression */
		if (*xp == 0) xp = presst; /* last string */
		prompt1("expr: %s",xp);
		while(rgsrch(curln,column,xp,(arg == 1),dir)) {
			move(kline,kcol);
			ol = curln;
			oc = column;
			strcpy(presst,xp);
			if (infrn == 0) {
				disup();
				x = mline;
				y = mcol;
				prompt1("expr: %s",xp);
				mgo(x,y);
				c = getchar();
			} else return(1);
			
			if (x=issrch(c)) {
				dir=x;
				forw(dir);
			} else {
				unprompt();
				ungetch(c);
				return(1);
			}
		}
		prompt1("Search failed");
		move(ol,oc);
		if (infrn >=0) beep();
	}
	return(0);
}

/* rgsrch -- regular expression search */

rgsrch(froml,fromc,cp,wrap,dir)
int froml;
int fromc;
int wrap;
register int dir;
char *cp;

/* Keywords: regular-expressions searching query-replace:20 */

{
	register int en;
	char rebuf[RESIZE];
	register int beg;
	char *lp;
	
	lp = cp;
	while (beg = *lp) *lp++ = casem[beg&0177]; /* map string to allowed case */
	if (*cp == '^') {
		cp++;
		beg = 1;
	} else beg = 0;
	if (en = compile(cp,rebuf)) { /* if format error */
		error(WARN,en);
		return(0);
	}
	kline = froml;
	kcol = fromc;

	while (1) {
		if (brkflg) brkit();
		klptr = mkline(kline);
		if (beg) {
			if ((kcol == 0) || (dir<0)) {
				kcol = 0;
				if(advance(klptr,rebuf)) {
					loc1 = klptr+kcol;
					return(1);
				}
			}
		} else { 
			while (1) {
				if ((rebuf[0] != CCHR) || (rebuf[1] == casem[klptr[kcol]&0177])) {

					if (advance(klptr+kcol, rebuf)) {

/* The following line rejects an attempt to match the last newline in the buffer */
						if ((kline == nlines) && klptr[kcol] == EOL) goto nomatch;
						loc1 = klptr+kcol;
						return(1);
					}
				}
				if (dir>0) {
					if (klptr[kcol] == EOL) goto nomatch;
					else kcol++;
				} else {
					if (kcol == 0) goto nomatch;
					else kcol--;
				}
				if ((kline == froml) && (kcol == fromc)) return(0);
			}
		}
nomatch:	kline += dir;
		if (wrap) {
			if (kline > nlines) kline = 1;
			if (kline < 1) kline = nlines;

		} else {
			if ((kline > nlines) || (kline <1)) return(0);
		}
		if (beg || (dir>0))  kcol = 0;
		else kcol = leng(kline);
		if (wrap && (kline == froml) && (kcol == fromc)) return(0);
		if (beg && (kline == froml)) return(0);
	}
}			

/* regrep -- replace sub expression */
/* assumes last deleted text is in the kill stack, pointers set up */

regrep(i)

register int i;

/* Keywords: query-replace commands regular-expressions */


{
	register int x;
	register int y;
	
	if (i <= closed) {
		yank(1);		/* bring back everything */
		y = braslist[i]-loc1;	/* length to kill */		
		back(y);	/* adjust mark position correctly */
		
		exch(1);		/* back to beggining */
		fdel(y);		 /* kill first part */
		kpop();
		forw(braelist[i]-braslist[i]);
		mkill(1);		/* now kill the rest */
		kpop();
		unpop(3);		/* Remove from the undo stack */
		return(1);
	} else return(0);
}
