#ifdef	ux6
#include <inode.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <signal.h>
#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_buf.h"

extern	int	szflg;

/* EMACS_MODES: c !fill */

/* leng * length of a line in the buffer */

leng(line)

register line;

{

	register char *lp;
	register char *olp;

	olp = lp = mkline(line);
	while (*lp++ != EOL);
	return(lp-olp-1);
}

allout ()				/* break all links */

{
	register unsigned *outp;
	register int xptr;
	register int q;
	
	for (outp = ptrs+nlines; outp > ptrs; outp--) {
		xptr = *outp;
		if (xptr&01) {
			q = xptr-1;
			*outp = ((bblock[q>>BSHIFT]<<(BSHIFT-LSSHIFT)) + ((q&(BLEN-1))>>LSSHIFT))<<1;
		}
	}
}
	

/* mkline -- make a pointer to the current line */

/* pages in the line if out of core, makes a null string for an empty
line */

char *

ckline(line,len)
int line;
int len;
{
	register char *lp;
	char *lp1;
	register char *q;
	register fileadd;
	int x;
	int s;
	
	lp = mkline(line);
	if (len <= (LSMALL*(*(lp-1))-2)) return(lp);
	
					/* current line is too small */
	

	if ((len+2) >= MAXEL) error(FATAL,33,line); /* line too long */
	s = (len+1+LSMALL)>>LSSHIFT;
	x = ptrs[line]-1;
	
	fileadd = (bblock[x>>BSHIFT]<<(BSHIFT-LSSHIFT)) + ((x&(BLEN-1))>>LSSHIFT);
	
	if ((fileadd + bbuf[0][x]) == BUFEND) {
		if ((fileadd & BMASK) == ((fileadd+s-1)&BMASK)) {
			
					/* current line extends in place */
			BUFEND = fileadd + s;
			bbuf [0] [x] = s;
			TRACE(TRCKEX);
			TRACE(line);
			TRACE(s);

			return (&bbuf[0][x+1]);
		}
	}
	
					/* must copy to new buffer */
	
	if ((BUFEND&BMASK) != ((BUFEND-1+s)&BMASK)) {
		BUFEND = (BUFEND&BMASK)+BFACT;
	}
	ptrs[0] = BUFEND<<1;
	BUFEND += s;
	TRACE(TRCKCP);
	TRACE(line);
	TRACE(s);
	holdin(0,line);			/* get both lines in memory */
	q = &(bbuf[0][ptrs[line]]);	/* old line buffer */
	lp = lp1 = &(bbuf[0][ptrs[0]]);		/* new buffer */
	*(lp-1) = s;
	s = (*(q-1)*LSMALL);		/* old length */
	while (s--) if ((*lp++ = *q++) == EOL) break;
	ptrs[line] = ptrs[0];
	ptrs[0] = 0;
	modify(line);
	return(lp1);
}

char *

mkl(line)
register line;

{
	register char *p;
	register i;
	int block;
	int newf;
	
	while (line>nlines) {
		sputl(nlines+1,0,line);
		if (line >= NPTRS) {
			if (!growbuf(line)) return(NULL);
		}
		nlines++;
		ptrs[nlines] = 0;
		if (bufmod==0) {
			bufmod = 1;
			dispmod();
		}
	}

	if (INMEM(line)) return (&bbuf[0][ptrs[line]]);

	/* no current line */

	nmkline++;			/* count for stats */

	TRACE(TRMAKE);
	TRACE(line);

	if (BUFILE == NULL) {
		BUFILE = gtemp(curbf); /* get a temp file */
		BUFEND = BFACT;		/* remember 0 == NULL */
	}
	if (ptrs[line] == 0) {
		ptrs[line] = (BUFEND++)<<1; /* assign new buffer */
		newf = 1;
		TRACE(TRMKEM);
	} else newf = 0;
	block = ((ptrs[line]))>>(BSHIFT-LSSHIFT+1);

/* first see if desired block is in memory already */

	for (i = fbkno; i < NBLOCK; i++) if (block == bblock[i]) {
		goto bfill;
	}

					/* now try to swap in */
	
	for (i = fbkno; i <= NBLOCK+1; i++) {
		if (++nxtflsh >= NBLOCK) nxtflsh = fbkno;
		if (bstat[nxtflsh] == 0) {
			bgrab(nxtflsh,block); /* get block in */
			i = nxtflsh;
			goto bfill;
		} else bflush(nxtflsh);	/* force out block */
	}
	
/* can't find a buffer to flush */

	error(FATAL,34);		/* File buffers broken */

	
bfill:
	p= &bbuf[i][LSMALL*((ptrs[line]>>1)&BRESID)+1];
	ptrs[line] = p-bbuf[0];
	if (newf) {
		*(p-1) = 1;		/* line size */
		*(p) = EOL;
		modify(line);		/* make sure line is flushed */
	}
	return(p);
}


/* gtemp -- get a temp file, and unlink it */

gtemp(fn)
int fn;
{
	char tfile [64];
	register int tf;
	seprintf(tfile,"/tmp/em.%o.%o",fn,getpid());
	while ((tf = creat(tfile,0600)) < 0) {
		if ((errno != 4) && (errno != 23)) break;
	}
	close(tf);
	while ((tf = open(tfile,2)) < 0) {
		if ((errno != 4) && (errno != 23)) break;
	}
	if (tf == NULL) {
		error(FATAL,errno,"temp file");
	}
	unlink(tfile);	/* lose the file */
	return(tf);
}
/* utility program to insure that both lines are in memory before proceeding */

holdin(line1,line2)

register line1,line2;

{
	register char *x;			/* to make lint happy */

	while (OUTMEM(line1) || OUTMEM(line2)) {
		x = mkline(line1);
		x = mkline(line2);
	}
}

/* growbuf -- grow the amount of buffer storage */
growbuf(iline)

int iline;

{
	register unsigned *ndadd;
	register int line;

	
	line = (iline&0177000)+01000;		/* round up to next multiple
					 * of 512 */
	
	ndadd = &ptrs[line];		/* new need address */
	if (brk(ndadd) != 0) {
		line = iline+4;		/* try for less */
		ndadd = &ptrs[line];
		if (brk(ndadd) != 0) {
			error(NORM,35); /* Too Many lines */
			return(0);
		}
	}
	NPTRS = line;
	return(1);

}



		
/* bgrab -- fill a specific buffer with a specific block */

bgrab(x,blkno)

register x;
int blkno;
{
	register base;
	register unsigned *p;
	unsigned short bsblock;
	int rstat;
	long seekpos;
	
	TRACE(TRBGRAB);
	TRACE(x);
	TRACE(blkno);
	if (bblock[x]) {		/* if block is occupied */
		if (bstat[x]) bflush(x);
		base = x*BLEN+1;
		bsblock = bblock[x]<<(1+BSHIFT-LSSHIFT);

		for (p= &ptrs[nlines]; p>ptrs; p--) {
			if((*p &01) && (((*p - base)&(~(BLEN-1))) == NULL)) {
				*p = bsblock + ((*p - base)>>(LSSHIFT-1));
			}
		}
			/* break all links to block */

 	}
	bblock[x] = blkno;
	if (blkno&& (blkno<=mostwrit)) {
		if (blkno != sblk) { /* if we must seek */
			TRACE(TRSEEK);
			TRACE(sblk);
			TRACE(blkno);
			seekpos = (long) BLEN * (long) blkno;
			nbseek++;
			lseek(BUFILE,seekpos,0);
		}
		nbread++;
		TRACE(TRREAD);
		TRACE(x);
		TRACE(blkno);
		while ((rstat =read(BUFILE,bbuf[x],BLEN)) != BLEN) {
			if (errno != 4) break;
		}
		if (rstat != BLEN) error(FATAL,errno,"buffer file");
		sblk = blkno+1;
	}
}

/* bflush -- flush contents of a buffer */ 

bflush(x)

register x;
{
	long seekpos;
	if (bstat[x]) {

		if (sblk != bblock[x]) {
			seekpos = (long) bblock[x] * (long) BLEN;
			TRACE(TRSEEK);
			TRACE(sblk);
			TRACE(bblock[x]);
			nbseek++;
			lseek(BUFILE,seekpos,0);
		}
		nbwrite++;
		TRACE(TRWRIT);
		TRACE(x);
		TRACE(bblock[x]);
		while(write (BUFILE,bbuf[x],BLEN) != BLEN) {
			if (errno != 4)	error(FATAL,errno,"buffer file");
		}
		sblk = bblock[x]+1;
		if (bblock[x] > mostwrit) mostwrit = bblock[x];
		bstat[x] = 0;
	}
}

/* initializes line buffer free list, ptrs array to empty */



bufinit()

{
	register i;
	register unsigned *p;

/* now set up buffer stuff */


	for (p = &ptrs[NPTRS-1]; p >= ptrs; p--) { /* clear ptrs */
		*p = 0;
	}

	/* set up free buffer line list */

	for (i = fbkno; i < NBLOCK; i++) bstat[i] = bblock[i] = 0;
	nxtflsh= NBLOCK;			/* initial buffer to use */
	nlines = 1;
	mostwrit = 0;
	sblk = 0;
	curln = 1;
	column = 0;
	curblk = 0;
	if (BUFILE) BUFEND = BFACT;	/* re-use buffer */
}

/* wout -- write out file */

wout(name,aflag)

char *name;
int aflag;
{
	FILE outbuf;
	register FILE *outfile;
	register i;
	char backb[128];		/* buffer for name of backup file */
	char *backp;
	char *bp;
	register char *cp;
#ifdef	ux6
	struct inode inode;
#else
	struct stat inode;		/* status buffer */
#endif

	if (*name == NULL) return(0);
	bp = name;
	cp = backp = backb;
	while (*cp = *bp++) {
		if (*cp++ == '/') backp = cp;
	}
	strcpy(backp,".emacs");
	
#ifdef	ux6
	inode.flags = 0644;		/* default mode, rw-r-r */
	inode.nlinks = 1;		/* number of links */
	stat(name,&inode);		/* get old file modes */
					/* don't care if file not there */
	if ((inode.flags&0200) == 0) { /* file wasn't writeable */
		if (gyn("File not writeable, write anyway?")<=0) return(0);
	}

	if (aflag) {
		outfile = fopen (&outbuf,name,"a");
		if (outfile) goto wfile1; /* if file was there */
	}
	if (inode.nlinks != 1) {
		i = gyn("Overwrite existing file (y), or make new file (n) ?");
		if (i < 0) return(0);
		if (i > 0) goto wfile;
	}
#else
	inode.st_mode = 0644;		/* default mode, rw-r-r */
	inode.st_nlink = 1;		/* number of links */
	stat(name,&inode);		/* get old file modes */
					/* don't care if file not there */
	if ((inode.st_mode&0200) == 0) { /* file wasn't writeable */
		if (gyn("File not writeable, write anyway?")<=0) return(0);
	}

	if (aflag) {
		outfile = fopen (&outbuf,name,"a");
		if (outfile) goto wfile1; /* if file was there */
	}
	if (inode.st_nlink != 1) {
		i = gyn("Overwrite existing file (y), or make new file (n) ?");
		if (i < 0) return(0);
		if (i > 0) goto wfile;
	}
#endif
	
			
	link (name,backb);		/* make backup link to file in case */
					/* we crash.  Old copy will be in  
					 * .emacs if crash occurs during write */
	unlink(name);			/* remove old file */

wfile:	outfile = fopen (&outbuf,name,"w");
wfile1:	if (outfile == NULL) {
		error(WARN,errno,name);
		return(0);
	}
	prompt1("Wait");
	goback();
	mflush(stdout);
	for (i = 1; i <= nlines; i++) {
		if (i != 1) {
			putc('\n',outfile);
		}
		for (cp = mkline(i); *cp != EOL; cp++){
			 putc(*cp,outfile);
		}
	}
	mclose(outfile);
	unlink(backb);			/* remove backup link */
#ifdef	ux6
	chmod(name,inode.flags);
#else
	chmod(name,inode.st_mode);	/* set modes right */
#endif
	prompt1("Written: %s", name);
	NSCHAR=0;
	goback();
	bufmod = 0;
	strcpy(filename,name);
	dispmod();
	return(1);
}

/* read in a file into the buffer */
/* re-initializes buffer first */


readin(fn,reinit)

char *fn;
int reinit;
{
	FILE *filein;
	FILE finbuf;
#ifdef DIRED
	extern char dired_args[];
	struct pstruct {
		int infid;
		int outfid;
	} piped;
#endif

	if (fn[0] == 0) fn =  filename;			/* default to previous file */
	if (fn[0] == 0) return;		/* no default */

	filein = fopen(&finbuf,fn,"r");
	if (filein == NULL) {
		if (reinit>0) error(WARN,errno,fn);
		if (filename[0] == 0) {
			strcpy(filename,fn); /* copy name */
			dispmod();
		}
		return;
	}
#ifdef DIRED
	if (diron) {
		mclose(filein);
		pipe(&piped);			/* make piped */
		if (fork() == 0) {		/* if child */
			close(piped.infid);
			close(1);
			dup(piped.outfid);
			close(piped.outfid);
			execl("/bin/ls","ls",dired_args,fn,0);
			exit(0);
		} else {
			close(piped.outfid);
			filein = fdopen(&finbuf,piped.infid,"r");
			if (filein == NULL) {
				error(FATAL,errno,"");
			}
		}
	}
#endif
	if (VERBOSE) {
		prompt1("Wait");
		mflush(stdout);
	}
	if (reinit == -1) reinit = 1;
	if (reinit == 1) bufinit();
	readsub(filein,reinit,fn,0);		/* do the reading */
	unprompt();
	ttfill();			/* try to read ahead*/
	return;
}


readsub(filein,reinit,fn,cpyflag)

int reinit;
int cpyflag;
register FILE *filein;
char *fn;
{
	int c;
	int i;
	register char *tp;
	char fbuf[MAXEL];
	register char *fp;
	int warnl;
	
	i = curln;
	if (reinit != 1) {
		RARE = 1;			/* turn off trouble */
		while ((c = getc(filein)) != EOF) {
			if (cpyflag) write(1,(char *)&c,1); /* echo */
			put(c);
			if (c == EOL) clptr = mkline(curln);
		}
	} else {
		fp = &fbuf[0];	
		warnl = -1;			/* last "line too long" */
		while ((c = getc(filein)) != EOF) {
			if (cpyflag) write(1,(char *)&c,1); /* echo to terminal */

			if (c == '\n') {
				tp = ckline(i,fp-fbuf);
				*fp = EOL;
				fp = &fbuf[0];
				while ((*tp++ = *fp++) != EOL);
				fp = &fbuf[0];
				if (++i >= NPTRS-1) {
					if (!growbuf(i)) {
						error(NORM,35);
						goto eof;
					}
				}
			} else {
#ifdef DIRED
				if (diron) while (fp-fbuf < 4) *fp++ = ' '; /* make room for markings */

#endif
				*fp++ = c;
				if (fp >= fbuf+MAXEL-2) {
					fp--;
					if (i == warnl) continue;
					warnl = i;
					if(error(NORM,33,i)) goto eof;
					continue;
				}
			}
		}
eof:		tp = ckline(i,fp-fbuf);
		*fp = EOL;
		fp = &fbuf[0];
		while ((*tp++ = *fp++) != EOL);
		curln = 1;
		column = 0;
		strcpy(filename,fn);
		bufmod = 0;		/* buffer up to date */
	}
	mclose(filein);
	NSCHAR = 0;
	bfmodes();	/* set buffer modes */
	fclear();	/* file is now bad */
	RARE = 0;	/* normal modes back on */
}


/* findf -- go forward to find row and columns */

/* leaves kline, kcol pointing at the character count characters ahead
of curln, column */


findf(count)

register count;

{
	kmark();
	while (count--) {
		if (*klptr++ != EOL) kcol++;
		else {
			if (kline == nlines) return(0);
			kcol = 0;
			++kline;
			klptr = mkline(kline);
		}
	}
	return(1);
}

/* findb -- go back n chars */

/* leaves kline, kcol pointing at the character count characters before
the current character */

findb(count)

register count;

{
	kline = curln;
	kcol = column;
	
	while (count) {
		if (count > kcol) {
			count-=(kcol+1);	/*plus one for the nl */
			kcol = 0;
			if (kline == 1) return(0);
			kline--;
			kcol = leng(kline);
		} else {
			kcol -= count;
			return(1);
		}
	}
	return(1);
}


/* mark a line as modified (also re-displays mode line if necessary) */


modify(line)

register line;

{
	register char *lp;
	
	if (INMEM(line)) {
		bstat[(ptrs[line])>>BSHIFT] = 1;
	}
#ifdef DIRED
	if (diron) {
		lp = mkline(line);
		if ((column > 4) && (column < 14)) {
			lp[1] = 'M';
		} else if ((column > 18) && (column < 36)) {
			lp[2] = 'O';
		}
		sputl(line,1,line);	/* refresh */
	}
#endif
	if (bufmod == 0) {
		bufmod++;
		dispmod();
	}
}

/* stack text segment on kill stack */

/* the kill stack holds up to NKILLP segments, or a total of KBSIZE
 * characters.  killstk adds the segment delimited by its arguments to
 * this stack */

killstk(a,b,c,d)

register a;
register b;
int c;
int d;

{
	register char *ap;
	
	if (kbapp) {			/* if appending */
		kend--;			/* backspace over the \0 */
	} else {
		if (nkp == NKILLP)  {	/* no more kill pointers available */
			mvdown();
		}
		nkp++;
		kstk[nkp] = kend;		/* start of saved text */
	}
	kget(kend);
	ap = mkline(a);
	while ((a < c) || (b < d) ) {
		if ((kbuf[kptr] = ap[b++]) == EOL) {
			++a;
			ap = mkline(a);
			b = 0;
		}
		kbmod = 1;
		if (++kptr>=KBSIZE) {
			knext();
		}
	}
	kbapp = 2;			/* turn on append for now */
	kbuf[kptr++] = 0;
	kbmod = 1;
	if (kptr>=KBSIZE) {
		knext();
	}
	kend = (long)kptr + kbase;
}
stkstr(sp)
register char *sp;
{
	register char c;
	
	if (kbapp) {			/* if appending */
		kend--;			/* backspace over the \0 */
	} else {
		if (nkp == NKILLP)  {	/* no more kill pointers available */
			mvdown();
		}
		nkp++;
		kstk[nkp] = kend;		/* start of saved text */
	}
	kget(kend);
	do {
		c = kbuf[kptr++] = *sp++;
		kbmod = 1;
		if (kptr>=KBSIZE) knext();
	} while (c);
	kend = (long)kptr + kbase;
	kbapp = 0;
}

/* kapp -- append to kill buffer */

kapp()
{
	kbapp = 2;			/* force next kill to append */
}

/*knext -- next kill buffer*/


knext()
{
	long nbase;
	
	nbase = kbase + (long) KBSIZE;
	if (nbase >= KBLIM) nbase = 0;
	kget(nbase);
}


/* kget get next kill buffer */

kget(nbase)
long nbase;
{
	register long xbase;
	
	xbase = nbase &( (long) -KBSIZE);
	if (xbase == kbase) {
		kptr = nbase - kbase;
	}
	if (kfile == 0) {
		kfile = gtemp(NBUF);
	}
	if (kbmod) {
		lseek(kfile,kbase,0);
		while(write(kfile,kbuf,KBSIZE) != KBSIZE) {
			if (errno != 4)	error(FATAL,errno,"kill buffer");
		}
		kbwrt = kbase+KBSIZE;
		kbmod = 0;
	}
	if (nbase < kbwrt) {
		lseek(kfile,xbase,0);
		while(read(kfile,kbuf,KBSIZE) != KBSIZE) {
			if (errno != 4) {
				error(FATAL,errno,"kill buffer");
				return;
			}
		}
	}
	kbase = xbase;
	kptr = nbase-xbase;
	return;
}

/* mvdown pushes the bottom text segment off of the kill stack */



mvdown()		/* move down kill stack */

{
	register i;
	for (i = 0; i <= nkp; i++) kstk[i] = kstk[i+1];
	nkp--;
}

/* retrv retrieves the top text segment from the kill stack and inserts
  * it into the buffer at the current location */


retrv()	/* retrieve from kill stack */

{
	char c;

	if (nkp <0) return(0);		/* nothing in the kill stack */
	RARE = 1;			/* turn off trouble */

	kget(kstk[nkp]);
	while (c= kbuf[kptr]) {
		put(c);
		if (++kptr >= KBSIZE) {
			knext();
		}
	}
	RARE = 0;			/* normal modes back on */
	return(1);				/* done */
}

retrvs(strp,mc)			/* retrieve from kill stack */

register char *strp;
register int mc;
{
	register int nc;

	nc = 0;


	kget(kstk[nkp]);
	while (*strp++ = kbuf[kptr]) {
		if (++nc >= mc) {
			*(--strp) = 0; /* finish off string */
			error(WARN,63); /* file name too big */
			break;		/* break loop */
		}
		if (++kptr >= KBSIZE) {
			knext();
		}
	}
	kpop();				/* pop the kill stack */
	return(nc);				/* done */
}

/* kpop pops the top item off of the kill stack */
kpop()

{
	if (nkp>-1) {
		nkp--;
		return(1);
	} else return(0);
}

/* fsave saves the buffer if it has been modified since last read or write */


fsave()
{
	if (READONLY) {
		error(WARN,71);
		return(0);
	}
	if (*filename== 0) {
		if (infrn == 0) return(fright(1)); /* no file name */
		error(WARN,65,bufname); /* Don't read the file name in a macro */
		return(0);
	}
	if (bufmod) {
		return(wout(filename,0));
	} else {
		prompt1("File %s is up to date",filename);
		goback();
		return(1);
	}
}

/* fname, bname, and modded return the current file, buffer, and buffer
 * modified flags */


char *
fname()
{
	return(filename);
}
char *
bname()
{
	return(bufname);
}
int
bfnumb()
{
	return(curbf);
}
int
modded()
{
	if (bufmod) return(1);
	else return(0);
}

/* chbuf moves to a new buffer denoted by the buffer number of its
 * argument */

/* each buffer has a filename, buffername, modified flag, current
 * position, and size,  The ptrs array of a non-active buffer is stored in
 * the temp file for the buffer.
 */

chbuf(x)

register x;

{
	register i;
	register rx;
	unsigned cpointer;

	
	if (SAVEMD && bufmod && *filename) IGNORE(fsave()); /* save buffer */

	for (i = fbkno; i < NBLOCK; i++) bflush(i);	
	allout();
	NSCHAR = 0;
	btmpfile[curbf] = BUFILE;
	btmpfree[curbf] = BUFEND;
	bcurln[curbf] = curln;
	bcolumn[curbf] = column;
	if (BUFILE) {
		while (1) {
			lseek(BUFILE,(long)((long) BUFEND * (long) LSMALL),0);
			rx = write(BUFILE,(char *)ptrs,(nlines+1)*(sizeof cpointer));
			if (rx == (nlines+1)*(sizeof cpointer)) break;
		}
	}
	bnlines[curbf] = nlines;

	bufinit();
	
	BUFEND = btmpfree[x];
	mostwrit = ((BUFEND-1)/BFACT);
	BUFILE = btmpfile[x];
	curbf = x;
	nlines = bnlines[x];
	if (BUFILE) {
		while (1) {
			lseek(BUFILE, (long)((long) BUFEND * (long) LSMALL),  0);
			rx = read(BUFILE,(char *)ptrs, (nlines+1)*(sizeof cpointer));
			if (rx ==(nlines+1)*(sizeof cpointer)) break;
		}
	}
	
	bfmodes();			/* set buffer modes */
	move(bcurln[x],bcolumn[x]);
}

/* chgbuf changes buffers by name.  If no buffer of the right name
 * exists, one is made */


chgbuf(sp)

char *sp;

{
	register ep = -1;
	register i;
	
	if (*sp == 0) {
		error(WARN,36);
		return(0);
		}
	for (i = 0; i < NBUF; i++) {
		if (streq(sp,bbfname[i])) {
			chbuf(i);
			return(1);
		}
		if ((ep < 0) && (bbfname[i] [0] == 0)) ep = i;
	}
	i = aint(sp);			/* try numeric buffer name */
	if ((i < NBUF) && (i >= 0)) {
		if (bbfname[i] [0] == 0) {
			inibuf(i);
			seprintf(bbfname[i],"%d",i); /* name buffer */
		}
		chbuf(i);
		return(1);
	}
	if (ep>=0) {
		inibuf(ep);
		if (streq(sp,"...")) seprintf(bbfname[ep],"%d",ep);
		else strcpy(bbfname[ep],sp);
		chbuf(ep);
		return(1);
	} else {
		error(WARN,37);
		return(0);
	}
}

/* inibuf -- initialize buffer data */

inibuf(ep)
register ep;				/* buffer number  */

{
	bbfname[ep] [0] = 0;
	btmpfree[ep] = 0;
	bcurln[ep] = bcolumn[ep] = 0;
	bnlines[ep] = 0;
	bbfmod[ep] = 0;
	bfilname[ep] [0] = 0;
}

/* edbuf is the find file command, it asks for a filename, finds a
 * buffer containing the file if it exists, otherwise it makes a new buffer
 * to hold the file */

edbuf(arg)

int arg;
{
	register i;
	char *sp;
#ifdef DIRED
	char nbuf[256];
#endif
	
	sp = expenv(getname("Filename to Find? "));
	if (sp == NULL) return;
#ifdef DIRED
	if (*sp != '/') {
		catstr(nbuf,filename,sp); /* relative path */
		if ((nbuf[0] == '.') && (nbuf[1] == '/')) sp = nbuf+2;
		else sp = nbuf;
	}
#endif
	for (i = 0; i < NBUF; i++) {
		if (streq(bfilname[i], sp)) {
			chbuf(i);
			return;
		}
		if (streq(bbfname[i], sp)) {
			IGNORE(error(WARN,38,sp));
			return;
		}
	}
	if (chgbuf(sp)) readin(sp,arg);
	return;
}

/* cpbuf is the change buffer command, it prompts for a buffer name and
 * mmoves to  it unless the name is '*', in which case a list of active
 * buffers is displayed */

cpbuf()

{
	register char *cp;	
	
again:	cp = getname("Buffer name? ");
	if (cp) {
		if ((*cp == 0) || (*cp == '*')) {
			if (buflist()== 0) goto again;
			return;
		}
		IGNORE(chgbuf(cp));
	}
}

buflist()				/* list active buffers */
{
	register i;
	char c;
	
	mtop();
	putout("Buffers used:");
	putout ("");
	for(i = 0; i < NBUF; i++) {
		if (bbfname[i] [0] ) {
			if (i == curbf) c = '*';
			else c = ' ';
			putout ("%c (%d) %s  %c  %s",
			c,i,bbfname[i],mdchar[bbfmod[i]], bfilname[i]);
		}
	}
	putout (endput);
	return(contin());
}


/* fndbuf finds a buffer with a given name */


fndbuf()
{
	register i;
	register char *bn;
	
again:	bn = getname("Buffer Name? ");
	
	if (bn == NULL) return(-1);
	if ((*bn == 0) || (*bn == '*')) {
		if(buflist()) return(-1);
		else goto again;
	}
	for (i = 0; i < NBUF; i++) {
		if (streq(bbfname[i], bn)) return(i);
	}
	i = aint(bn);			/* try numeric buffer name */
	if ((i <= NBUF) && (i >= 0) && (bbfname[i][0])) return(i);
	IGNORE(error(WARN,39,bn));
	return(-1);
}


/* bufmove -- pipe text from one buffer to another */

/* sends the text in the region to a specified buffer */

bfsend(arg)

int arg;
{
	register i;
	register j;
	
	
	i = fndbuf();
	if (i >= 0) {
		pickup(arg);		/* pick up the region */
		j = curbf;
		chbuf(i);
		retrv();
		kpop();
		chbuf(j);		/* back to old buffer */
	}
}

/* rnbuf -- rename buffer or buffer file */


rnbuf(arg)

int arg;
{
	register char *sp;
	sp = expenv(getname("New Name? "));
	if ((sp== NULL) || (*sp == NULL)) return;
	if (arg == 1) {			/* change buffer name */
		strcpy(bbfname[curbf],sp);
	} else {
		strcpy(bfilname[curbf],sp);
	}
}

/* rmbuf prompts for a buffer name and removes the buffer */


rmbuf()

{
	register i;
	
	if ((i = fndbuf()) >= 0) {
		klbfr(i);
		return(1);
	} else return(0);
}

klbfr(i)

register i;
{
	if (i == curbf) {
		IGNORE(error (WARN,40));
		return;
	}
	if (twowind && ((i == windb[1]) || (i == windb[0]))) {
		onewind();		/* killing other window */
	}
	if (btmpfile[i]) {
		close(btmpfile[i]);
		btmpfile[i] = 0;
	}
	inibuf(i);
}

/* clean up unwritten buffers.  bclean checks for unwritten buffers, and
 * if any are found, writes them out if the user wishes */

bclean()

{
	register i;
	
	bbfmod[curbf] = bufmod;
	bnlines[curbf] = nlines;

	for (i = 0; i < NBUF; i++) {
		if ((bbfmod[i]) && (bbfname[i] [0]) &&
			(bnlines[i] > 1) &&
			(streq(bbfname[i],".exec") == 0)){
				
#ifdef DIRED
			chbuf(i);
			if (dclean() <= 0) return(-1);
#else


			if ((szflg == 0) || (i != 0)) {
			switch (gyn("buffer %s modified since last write to file %s, write?",
				&(bbfname[i][0]),&(bfilname[i][0]))) {
			case 0:
				break; /* no  */
			case 1:
				/* yes answer */
				chbuf(i);
				if(fsave()<= 0) return(-1);
				break;
			case -1:
			default:
				unprompt(); /* clean up msg */
				return(-1);
			}
			}
			else {
			chbuf(0);
			if(fsave()<= 0) return(-1);
			}
#endif
		}
	}
	return(0);
}


crash(arg)					/* handle crashes */

int arg;				/* sometimes is reason for crash */
{
	register i;
	register char *home;
	
	signal(SIGHUP,SIG_IGN);
	signal(SIGINT,SIG_IGN);		/* go into our shell */

	if (crashes++) eabort();	/* one crash per customer */
	home = getenv("HOME");
	
	bbfmod[curbf] = bufmod;

	for (i = 0; i < NBUF; i++) {
		if ((bbfmod[i]) && (bbfname[i] [0])){
			seprintf(bfilname[i],"%s/emacs%d",home,i);
			seprintf(bfilname[i],"emacs%d",i);
			chbuf(i);
			IGNORE(fsave());
		}
	}
	quit();
}

/* collect -- garbage collection of buffer file */

collect()

{
	int oldcol;
	int oldln;
	
	
	prompt1("Garbage collecting buffer");
	mflush(stdout);
	
	oldln = curln;
	oldcol = column;
	
	top();
	kline = nlines;
	kcol = leng(kline);
	tkill();			/* kill all text into kill buffer */
	
	bufinit();			/* reinit buffer storage */
	top();
	retrv();
	kpop();
	move(oldln,oldcol);
	unprompt();
}

/* pshchr -- push a character into the macro buffer */

pshchr(ch)

register ch;

{
	if (macptr >= fbkno*BLEN) {
		if (fbkno+2<NBLOCK) {
			bgrab(fbkno++,0); /* grab another buffer */
			if (nxtflsh < fbkno) nxtflsh = fbkno;
		} else error(FATAL,41); /* too many macros */
	}
	bbuf[0][macptr++] = ch;
}

pbfname()				/* put buffer name in kill stack*/
{
	stkstr(bname());
}
pfnname()				/* put file name in kill stack */
{
	stkstr(fname());
}

pvname()				/* put version in kill stack */
{
	stkstr(version);;
}

recurse()				/* call emacs recursively */
{
	long svkst[NKILLP+1];		/* kill buffer save area */
	int svnkp;
	register int i;
	char *omyname;
	char nmbuf[64];
	int eresult;
	
	for (i = 0; i <= NKILLP; i++) svkst[i] = kstk[i];
	svnkp = nkp;
	pushin(NULL);		/* back to the tty */
	omyname = myname;
	seprintf(nmbuf,"%s*",myname);
	myname = nmbuf;
	dispmod();
	eresult = edit(1);
	myname = omyname; /* pop name */
	dispmod();
	inpop();			/* pop input */
	for (i = 0; i <= NKILLP; i++) kstk[i] = svkst[i];
	nkp = svnkp;
	return(eresult);
}

kdup(arg)				/* duplicate argument */

int arg;				/* level to duplicate */

{
	register long dupkp;
	
	if (arg > nkp) return(0);
	dupkp = kstk[nkp-arg];
	if (nkp == NKILLP) mvdown(); /* make room */
	kstk[++nkp] = dupkp;
	return(1);
}

kflush(count)				/* flush kill stack */

register int count;

{
	while (count--) {
		if (kpop() == 0) return(0);
	}
	return(1);
}

kexch(count)				/* exchange kill stack */

{
	register long dupkp;
	
	if (count<(nkp+1)) {
		dupkp = kstk[nkp];
		kstk[nkp] = kstk[nkp-count];
		kstk[nkp-count] = dupkp;
		return(1);
	} else return(0);
}

/* unmod -- mark or change buffer modified flag */

unmod(arg)
{
	if (arg == 1) bufmod = 0;
	else if (arg > 1) bufmod = 1;
	dispmod();
	return(bufmod);
}
