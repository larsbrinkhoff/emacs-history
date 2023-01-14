/* EMACS_MODES: c !fill */
#include <signal.h>
#include <sys/types.h>
#ifndef PC
#include <sys/stat.h>
#endif PC
#include "emacs_io.h"
#include "emacs_gb.h"
#include "emacs_buf.h"



/* leng * length of a line in the buffer */

leng(line)

register int line;

{
/* Keywords: line-representation:10 length */
	
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
/* Keywords: buffer-representation:50 line-representation:5 paging buffer-changing:10 */
	
	for (outp = ptrs+nlines; outp > ptrs; outp--) {
		xptr = *outp;
		if (xptr&01) {
			q = xptr-1;
			*outp = ((bblock[q>>BSHIFT]<<BRSHIFT) + ((q&BMASK)>>LSSHIFT))<<1;
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
/* Keywords: buffer-representation:50 line-representation:50 paging:20 buffer-allocation */

{
	register char *lp;
	char *lp1;
	register char *q;
	register int fileadd;
	int x;
	int s;
	
	lp = mkline(line);
	if (len <= (LSMALL*(*(lp-1))-2)) return(lp);
	
					/* current line is too small */
	

	if ((len+2) >= MAXEL) {
		error(WARN,33,line,MAXEL-1); /* report problems */
		return(NULL);		/* line too long, let caller handle */
	}
	s = (len+1+LSMALL)>>LSSHIFT;
	x = ptrs[line]-1;
	
	fileadd = (bblock[x>>BSHIFT]<<BRSHIFT) + ((x&BMASK)>>LSSHIFT);
	
	if ((fileadd + bbuf[0][x]) == BUFEND) {
		if ((fileadd & BRMASK) == ((fileadd+s-1)&BRMASK)) {
			
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
	
	if ((BUFEND&BRMASK) != ((BUFEND-1+s)&BRMASK)) {
		BUFEND = (BUFEND&BRMASK)+BFACT;
	}
	ptrs[0] = BUFEND<<1;
	BUFEND += s;
	TRACE(TRCKCP);
	TRACE(line);
	TRACE(s);
	xline = line;			/* Mark the real line in ptrs[0] */
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
register int line;

{
	register char *p;
	register int i;
	int block;
	int newf;
/* Keywords: buffer-representation paging line-representation buffer-allocation */
	
	while (line>nlines) {
		sputl(nlines+1,0,line);
		if (line >= NPTRS) {
			if (!growbuf(line)) return(NULL);
		}
		nlines++;
		ptrs[nlines] = 0;
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
	block = ((ptrs[line]))>>(BRSHIFT+1);

/* first see if desired block is in memory already */

	for (i = fbkno; i < NBLOCK; i++) if (block == bblock[i]) {
		goto bfill;
	}

					/* now try to swap in */
	
	for (i = fbkno; i <= NBLOCK+1; i++) {
		if (++nxtflsh >= NBLOCK) nxtflsh = fbkno;
		if (bstat[nxtflsh] == 0) {
			bgrab(nxtflsh,block); /* get block in (won't fail)*/
			i = nxtflsh;
			if (line)lowpt[i]=hipt[i]=line;
			else lowpt[i]=hipt[i]=xline;
			goto bfill;
		} else bflush(nxtflsh);	/* try to force out block */
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
	if (line==0) line = xline;
	if (line<lowpt[i])lowpt[i]=line;
	if (line > hipt[i])hipt[i] = line;
	return(p);
}


/* gtemp -- get a temp file, and unlink it */

gtemp(fn)
int fn;
{
/* Keywords: temporary-files filenames:50 */
	
	char tfile [64];
	register int tf;
#ifdef PC
retry:	seprintf(tfile,"%s:emt.%d",BTEMPATH,fn);
	tf = creat(tfile,6);
	if (tf < 0) {
		ctfile();
		goto retry;
	}
#else
	seprintf(tfile,"%s/em.%o.%o",BTEMPATH,fn,getpid());
	while ((tf = creat(tfile,0600)) < 0) {
		if ((errno != 4) && (errno != 23)) break;
	}
	close(tf);
	while ((tf = open(tfile,2)) < 0) {
		if ((errno != 4) && (errno != 23)) break;
	}
	if (tf < 0) {
		error(FATAL,errno,"temp file");
	}
#ifdef MINFILES
	if (fn == NBUF) unlink(tfile);	/* lose the file */
#else
	unlink(tfile);	/* lose the file */
#endif
#endif
	return(tf);
}

#ifdef PC
rmtemp()
{
/* Keywords: temporary-files filenames:20 */
	
	char tfile[64];
	int i;
	btmpfile[curbf] = BUFILE;		/* set up for current buffer */
	for (i = 0; i < NBUF; i++) if (btmpfile[i]) close(btmpfile[i]);
	if (kfile) close(kfile);
	for (i = 0; i <= NBUF; i++) {
		seprintf(tfile,"%s:emt.%d",BTEMPATH,i);
		unlink(tfile);
	}
}

/* Called on out of space errors, asks for file to delete and continues */

mkspace()
{
	char *cp;
	char filebuf[64];
	char tmp[128];
/* Keywords: PC-only temporary-files deletion:10 user-interface:50 */
	
	if (error(WARN,76,BTEMPATH) == 0)  {
		cp = getname("File to delete on temp file disk?");
		if (cp) {
			mstrcpy(tmp,fnbuf); /* Preserve fnbuf */
			seprintf(filebuf,"%s:%s",BTEMPATH,cp);
			if (unlink(filebuf) == 0) {
				mstrcpy(fnbuf,tmp);
				return(1);
			}
			mstrcpy(fnbuf,tmp);
		}
	}
	return(0);
}
#endif PC

#ifdef MINFILES
rmtemp()
{
/* Keywords: temporary-files filenames:20 */
	char tfile [64];
	register int fn;
	btmpfile[curbf] = BUFILE;		/* set up for current buffer */
	for (fn = 0; fn < NBUF; fn++) {
		if (btmpfile[fn]) {
			rftmp(fn);
		}
	}
	rftmp(NBUF);
}
rftmp(fn)
register int fn;
{
/* Keywords: temporary-files filenames:20 MINFILES-version */
	char tfile[64];
	seprintf(tfile,"%s/em.%o.%o",BTEMPATH,fn,getpid());
	unlink(tfile);
}

otemp(fn)
int fn;
{
/* Keywords: temporary-files filenames:20 MINFILES-version */
	char tfile [64];
	register int tf;
	seprintf(tfile,"%s/em.%o.%o",BTEMPATH,fn,getpid());
	while ((tf = open(tfile,2)) < 0) {
		if ((errno != 4) && (errno != 23)) break;
	}
	if (tf < 0) {
		error(FATAL,errno,"temp file");
	}
	return(tf);
}
#endif 

/* utility program to insure that both lines are in memory before proceeding */

holdin(line1,line2)

register int line1,line2;
/* Keywords: paging line-representation:10 */

{

	while (OUTMEM(line1) || OUTMEM(line2)) {
		 mkline(line1);
		 mkline(line2);
	}
}

/* growbuf -- grow the amount of buffer storage */
growbuf(iline)

int iline;
/* Keywords: buffer-representation memory-allocation */

{
	register unsigned *ndadd;
	register int line;

	
	line = (iline&BRKMSK)+01000;		/* round up to next multiple
					 * of 512 */
	
	ndadd = &ptrs[line];		/* new need address */
#ifdef PC
	if (line >= MPTRS) {
#else
	if (brk(ndadd) != 0) {
#endif PC
		line = iline+4;		/* try for less */
		ndadd = &ptrs[line];
#ifdef PC
		if (line >= MPTRS) {
#else
		if (brk(ndadd) != 0) {
#endif PC
			error(NORM,35); /* Too Many lines */
			return(0);
		}
	}
	NPTRS = line;
	return(1);

}



		
/* bgrab -- fill a specific buffer with a specific block */

bgrab(x,blkno)
/* Keywords: buffer-representation paging paging-I/O encryption:10 */

register int x;
int blkno;
{
	register int base;
	register unsigned *p;
	register unsigned *endp;
#ifndef pdp11
	unsigned bsblock;
#else
	unsigned short bsblock;
#endif
	int rstat;
	long seekpos;
	
	TRACE(TRBGRAB);
	TRACE(x);
	TRACE(blkno);
	if (bblock[x]) {		/* if block is occupied */
		if (bstat[x]) {
			if (bflush(x)== 0) return(0);
		}
		base = x*BLEN+1;
		bsblock = bblock[x]<<(1+BRSHIFT);
		if (hipt[x] > nlines) hipt[x]=nlines; /* Make sure it's in bounds! */
		if (lowpt[x] < 0) lowpt[x] = 0; /* likewise */
		endp = &ptrs[lowpt[x]];		
		for (p= &ptrs[hipt[x]]; p>=endp; p--) {
			if((*p &01) && (((*p - base)&(~BMASK)) == NULL)) {
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
		if (rstat != BLEN) error(FATAL,errno,"reading buffer");
#ifdef CRYPTO
		if (bbuf[x][0]&0200) {

/* First byte of buffer is always a count, and thus shouldn't have the */
/* 0200 bit on.  This insures that enciphered bufferes are deciphered */

			cryptic(&bbuf[x][0]);
			bbuf[x][0] &= 0177; /* Make sure high order bit is off */
		}
#endif
		sblk = blkno+1;
	}
	return(1);
}
#ifdef CRYPTO
cryptic(cp)
register long *cp;			/* Types deliberately mismatched */
{
/* Keywords: buffer-representation encryption paging:10 */
	
	long x;
	register long *endp;
	
	endp = cp + BLEN/sizeof(x);
	while (cp < endp) *cp++ ^= bufkey;
}
#endif
/* bflush -- flush contents of a buffer */ 

bflush(x)
/* Keywords: buffer-representation paging paging-I/O encryption:10 */

register int x;
{
	
	long seekpos;
	if (bstat[x]) {
retry:
		if (sblk != bblock[x]) {
			seekpos = (long) bblock[x] * (long) BLEN;
			TRACE(TRSEEK);
			TRACE(sblk);
			TRACE(bblock[x]);
			nbseek++;
			lseek(BUFILE,seekpos,0);
		}
#ifdef CRYPTO
		if (crypt) {
			cryptic(&bbuf[x][0]);
			bbuf[x][0] |= 0200;
		}
#endif CRYPTO
		while(write(BUFILE,bbuf[x],BLEN) != BLEN) {
#ifdef PC
			if (mkspace()) {
				bblock[x] = -1; /* Don't know where we are */
				goto retry;
			}
			return(0);
#else
			if (errno != 4)	error(FATAL,errno,"buffer file");
#endif			
		}
#ifdef CRYPTO
		if (bbuf[x][0]&0200) {
			cryptic(&bbuf[x][0]);
			bbuf[x][0] &= 0177; /* Make sure high order bit is off */
		}
#endif CRYPTO
		nbwrite++;
		TRACE(TRWRIT);
		TRACE(x);
		TRACE(bblock[x]);
		sblk = bblock[x]+1;
		if (bblock[x] > mostwrit) mostwrit = bblock[x];
		bstat[x] = 0;
	}
	return(1);
}

/* initializes line buffer free list, ptrs array to empty */



bufinit()
/* Keywords: buffer-changing:10 buffer-representation buffer-allocation:50 */

{
	register int i;
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
#ifdef PC

/* 	change temp file -- change place where temp files are put. */


ctfile()
{
/* Keywords: PC-only buffer-representation temporary-files filenames:10 */

	char *cp;
	cp = getname("Tempfile Directory? ");
	if (cp) {
		*BTEMPATH = *cp;
	}
	if (mostwrit == 0) {
					/* Re-allocate current buffer */
		close(BUFILE);
		BUFILE = gtemp(curbf);
	}
}

#endif PC
/* wout -- write out file */

wout(name,aflag)
/* Keywords: writing buffer-representation:50 commands:10 PC-only:10 macro-hooks:10 */
/* Keywords: encryption:20 unix-interface:10 dired:20 user-interface:10 */

char *name;
int aflag;
{
	FILE outbuf[1];
	register FILE *outfile;
	register int i;
	int nosave;			/* return value from backup link */
	char backb[128];		/* buffer for name of backup file */
	char *backp;
	char *bp;
	register char *cp;
#ifndef PC
	struct stat inode;		/* status buffer */
#endif
	int status;
	int xstat;	
	
	if (hooks[Pre_Write_Hook]) {
		stkstr(name);
		if (hook(Pre_Write_Hook)==0) {
			unprompt();
			return(0);
		}
		retrvs(fnbuf,FNLEN);
		name=fnbuf;
	}

#ifdef DIRED
	if (diron) return(dclean());
#endif
	if (*name == NULL) return(0);
	bp = name;
	cp = backp = backb;
	while (*cp = *bp++) {
		if (*cp++ == '/') backp = cp;
	}
	strcpy(backp,"EMACS_save");
	nosave = 1;
	prompt1("Wait");
#ifndef PC
	mflush(stdout);
	status = stat(name,&inode);		/* get old file modes */
					/* don't care if file not there */
/*	if ((status==0) && (((inode.st_mode &0200) == 0) || (inode.st_uid != myuid))) {*/
	if (status == 0) {
		if (inode.st_mode & S_IFDIR)  {/* Writing a directory, yuck */
			error(WARN,81,name,name,filename);
			return(0);
		}
		if (access(name,02) != 0) {
			if (gyn("File not explicitly writeable, write anyway?")<=0) return(0);
		}
	}
#endif PC
	if ((aflag>1)||(aflag<-1)) {
		outfile = xopen (outbuf,name,"ba");
		if (status == 0) goto wfile1; /* if file was there */
	}
#ifndef PC
	if ((status == 0) && streq(filename,name) && 
	(mtime[bfnumb()]) && (mtime[bfnumb()] != inode.st_mtime)) {
			
		error(WARN,78,name);
		return(0);
	} 
	/* don't prompt if user wants file links saved */
	if (savelink) goto wfile;
	if ((status== 0) && (inode.st_nlink != 1)) {

		i = gyn("File %s is linked to another, write to both (y) or only to %s ?",name,name);
		if (i < 0) return(0);
		if (i > 0) goto wfile;
	}
	
	nosave = streq(name,backb);	/* see if we are trying to link backup */
	if (nosave==0) {
		nosave = link (name,backb);

		/* make backup link to file in case */
		/* we crash.  Old copy will be in  
		 * EMACS_save if crash occurs during write */
		if (nosave == 0) unlink(name);	/* remove old file */
	}
#endif PC
wfile:	outfile = xopen (outbuf,name,"bw");
wfile1:	if (outfile == NULL) {
		error(WARN,errno,name);
		return(0);
	}
	prompt1("Writing");
	mflush(stdout);

#ifdef CRYPTO
	if (crypt) {
		char crbuf[32];
		seprintf(crbuf,"crypt %s",cryptkey);
		splfile = outfile ->_frn;
		if (unx(crbuf,6)<0) return(0);
		outfile->_frn = splfile;
	}
#endif	

	for (i = 1; i <= nlines; i++) {
		if (i != 1) {
#ifdef PC
			if (BINMODE == 0) putc(CTRLM,outfile);
#endif PC
			putc('\n',outfile);
		}
		cp = mkline(i);
		while (*cp != EOL) {
			if (PICMODE && (*cp == ' ')) {
				register int x;
				for (x = 1; cp[x]==' '; x++);
				if (cp[x] == EOL) break;
				while (x--) {
					putc(*cp,outfile);
					cp++;
				}
			} else {
				putc(*cp,outfile);
				cp++;
			}
		}
	}
	if (EOFNL) {
		cp = mkline(nlines);
		if (*cp!=EOL) {
			putc('\n', outfile);
#ifdef PC
			if (BINMODE == 0) putc(CTRLM,outfile);
#endif PC
		}
	}
#ifdef PC
	if (BINMODE == 0) putc('\032',outfile);
	mclose(outfile);
#else
	if (mclose(outfile)) {		/* Error occurred during write */
					/* Try to put back old file */
		if (nosave==0) { /* error writing saved file */
			unlink(name);
			link(backb,name); /* Put everything back */
			unlink(backb);
			prompt1("No write: %s", name);
			return(0);
		}
		prompt1("File errors: %s", name);
		mtime[bfnumb()] = 0; 
		return(0);
	}

#ifdef CRYPTO
	if (crypt) wait(&xstat);	/* Eliminate zombies */
#endif
	if (nosave == 0) unlink(backb);  /* remove backup link */

	if (status == 0) {
		chmod(name,inode.st_mode);/* set modes right */
		chown(name,inode.st_uid,inode.st_gid); /* set owner and group */
	} else {
		chmod(name,0666 & ~mymask);/* Modes by umask */
	}
#endif PC
	if ((aflag<2)&&(aflag>-2)) {
		prompt1("Written: %s", name);
	}
	else prompt1("Appended: %s", name);
 	NSCHAR=0;
	if (aflag>0) {
		strcpy(filename,name);
		bufmod = 0;
	}
#ifndef PC
	if (streq(filename,name)) {
		
		if (stat(name,&inode) != 0) {
			error(WARN,errno,name);
		} else {
			mtime[bfnumb()] = inode.st_mtime;
		}
	}
#endif
	dispmod();
	move(curln,column);		/* Restore state information */
	return(1);
}

/* read in a file into the buffer */
/* re-initializes buffer first */


readin(fn,reinit)

char *fn;
int reinit;
{

/* Keywords: reading encryption:20 PC-only:10 dired:10 unix-interface:10 commands:10 macro-hooks:10 */

	FILE *filein;
	FILE finbuf[1];
#ifndef PC
	struct stat inode;
	int xstat;
#endif
#ifdef DIRED
	extern char dired_args[];
#endif

	if (fn[0] == 0) fn =  filename;			/* default to previous file */
	if (fn[0] == 0) return(0);		/* no default */

	if (VERBOSE) {
		prompt1 ("Wait");
		mflush(stdout);
	}
	if (hooks[Pre_Read_Hook]) {
		stkstr(fn);
		if (hook(Pre_Read_Hook) == 0) {
			unprompt();
			return(0);
		}
		retrvs(fnbuf,FNLEN);
		fn = fnbuf;
	}
	filein = xopen( finbuf,fn,"br");
	if (filein == NULL) {
		unprompt();
		if (reinit>0) error(WARN,errno,fn);
		if (filename[0] == 0) {
			strcpy(filename,fn); /* copy name */
			dispmod();
		}
		return(0);
	}
#ifndef PC
	if (fstat(filein->_frn,&inode) != 0) {
		error(WARN,errno,fn);
		return(0);
	}
	if (inode.st_size >= MAXFS) { /* Check for grossly huge file */
		error(WARN,77,fn);
		return(0);
	}
#ifndef DIRED
	if (inode.st_mode & S_IFDIR) {
					/* Reading a directory, check to be sure */
		if (error(WARN,82,fn)) return(0);
					/* go ahead and read it if the user wants to */
	}
#endif
#endif
#ifdef DIRED
	if (inode.st_mode & S_IFDIR) {
		char dircom[128];
		diron = 1;
		splfile = filein ->_frn;
		seprintf(dircom,"ls %s %s", dired_args,fn);
		if (unx(dircom,7) < 0) return(0);
		filein->_frn = splfile;
	} else diron = 0;
#endif
	if (VERBOSE) {
		prompt1("Reading");
		mflush(stdout);
	}
	if (reinit == -1) reinit = 1;
	if (reinit == 1) bufinit();
#ifdef CRYPTO
	if (crypt) {
		char crbuf[32];
		seprintf(crbuf,"crypt %s",cryptkey);
		splfile = filein->_frn;
		if (unx(crbuf ,7)<0) return(0);
		filein->_frn = splfile;
	}
#endif	
	readsub(filein,reinit,fn,0);		/* do the reading */
#ifdef CRYPTO
	if (crypt) wait(&xstat);			/* Eliminate zombies */
#endif	
#ifdef DIRED
	if (diron) wait(&xstat);	/* Eliminate zombies */
#endif	
#ifndef PC
	if (streq(filename,fn)) {
		struct stat ninode;
		mtime[bfnumb()] = inode.st_mtime;
		if (stat(fn,&ninode) != 0) {
			error(WARN,errno,fn);
		} else {
			if ((ninode.st_mtime != inode.st_mtime)||
			    (ninode.st_ino   != inode.st_ino))  {
				error(WARN,79,fn);
			}
		}
	}
#endif
	unprompt();
	ttfill();			/* try to read ahead*/
	if (hooks[Post_Read_Hook]) hook (Post_Read_Hook);
	return(1);
}


readsub(filein,reinit,fn,cpyflag)
/* Keywords: reading buffer-representation:5 unix-interface:20 dired:30 file-modes:10 */

int reinit;
#ifndef pdp11
register
#endif
int cpyflag;
register FILE *filein;
char *fn;
{
	register int c;
	int i;
	char fbuf[MAXEL];
	register char *fp;
	register char *endbuf;
	int warnl;
	
	i = curln;
	if (reinit != 1) {
		RARE = 1;			/* turn off trouble */
		while ((c = getc(filein)) != EOF) {
#ifdef PC
			if (BINMODE == 0){
				if (c == '\032') break;
				if (c == '\n') bdel(1); /* Wipe out spurious cr */
				
			}
#endif PC
			if (cpyflag) putchar(c); /* output character */
			put(c);
			if (c == EOL) clptr = mkline(curln);
		}
	} else {
		if (BUFILE == NULL) {
			BUFILE = gtemp(curbf); /* get a temp file */
		}
		BUFEND = BFACT;		/* remember 0 == NULL */
		fp = &fbuf[0];	
		endbuf = fbuf+MAXEL-2;
		warnl = -1;			/* last "line too long" */
		while ((c = getc(filein)) != EOF) {
#ifdef PC
			if (BINMODE == 0){
				if (c == '\032') break;
				if (c == '\n') fp--; /* Wipe out trailing CR */
			}
#endif PC
			if (cpyflag) putchar(c); /* echo to terminal */

			if (c == '\n') {
				*fp = EOL;
				if (addline(fbuf,fp-fbuf,i++)==0) goto eof;
				fp = &fbuf[0];
			} else {
#ifdef DIRED
				if (diron) while (fp-fbuf < 4) *fp++ = ' '; /* make room for markings */

#endif
				*fp++ = c;
				if (fp >= endbuf) {
					fp--;
					if (i == warnl) continue;
					warnl = i;
					if(error(NORM,33,i,MAXEL-1)) goto eof;
					continue;
				}
			}
		}
eof:		*fp = EOL;
		addline(fbuf,fp-fbuf,i);
		move(1,0);
		strncpy(filename,fn,FNLEN); /* Copy name written */
		bufmod = 0;		/* buffer up to date */
	}
	mclose(filein);
	if (cpyflag) mflush(stdout);	/* Force output to come out */
	NSCHAR = 0;
	bfmodes();	/* set buffer modes */
	fclear();	/* file is now bad */
	RARE = 0;	/* normal modes back on */
}

/* Express access to the buffer.  This code plunks in one line at
 * the end of the buffer, manipulating all appropriate data, much
 * swifter than the usual ckline-bgrab sequence.  What is assumed by
 * this function is that it is called after bufinit and no other
 * intervening buffer manipulations.  It leaves the ptrs array set
 * up with no lines in memory, but with the last n blocks of the
 * file in bbuf and properly initialized.   */




addline(cp,len,line)
register char *cp;
int len;
int line;
{
/* Keywords: buffer-representation:50 line-representation:20 reading:10 buffer-allocation:20 */

	register char *lp;
	register int x;
	register int s;

	if (line >= NPTRS) {
		if (!growbuf(line)) {
			error(NORM,35);
			return(0);
		}
	}
	nlines=line;
	
	s = (len+1+LSMALL)>>LSSHIFT;
	if (((BUFEND-1) & BRMASK) != ((BUFEND+s-1)&BRMASK)) {

/* Need to go into the next block */
		
		nxtflsh++;
		if (nxtflsh>=NBLOCK) nxtflsh=fbkno;
		x = nxtflsh;
		bflush(x);
		BUFEND=((BUFEND-1)&BRMASK)+BFACT;
		bstat[x]=1;
		lowpt[x]=hipt[x]=line;
		bblock[x]=(BUFEND>>BRSHIFT);
	}
	ptrs[nlines] = BUFEND<<1;
	lp = &bbuf[nxtflsh][((BUFEND&BRESID)<<LSSHIFT)];
	*lp++ = s;			/* save size */
	while ((*lp++ = *cp++) != EOL); /* copy the line */
	BUFEND += s;
	return(1);
}



/* findf -- go forward to find row and columns */

/* leaves kline, kcol pointing at the character count characters ahead
of curln, column */


findf(count)

register int count;
/* Keywords: buffer-representation:10 movement:20 commands:10 forwards */

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
/* Keywords: buffer-representation:10 movement:20 commands:10 backwards */
findb(count)

register int count;

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
/* Keywords: buffer-representation:10 dired:20 mode-line:10 deletion:10 insertion:10 */

register int line;

{
	
	if (INMEM(line)) {
		bstat[(ptrs[line])>>BSHIFT] = 1;
	}
#ifdef DIRED
	if (diron) {
		register char *lp;
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
/* Keywords: killstack stacking picture-mode:10 deletion:10 */

register int a;
register int b;
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

	if (PICMODE) {
		int x;
		if (b>d) {
			x=d;
			d=b;
			b=x;
		}
		while (a <= c) {
			ap = mkline(a);
			a++;
			for (x = 0; x < d; x++) {
				if (x<b) {
					if (*ap != EOL) ap++;
				} else {
					if (*ap != EOL) {
						kbuf[kptr++] = *ap++;
					} else {
						kbuf[kptr++] = ' ';
					}
					kbmod=1;
					if (kptr>=KBSIZE) knext();
				}        
			}         
			if (a<=c) {
				kbmod=1;
				kbuf[kptr++] = EOL;
				if (kptr>=KBSIZE) knext();
			}        
		}         
	} else {    
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
	}          
	kbapp = 0;			/* don't turn on append for now */
	kbuf[kptr++] = 0;
	kbmod = 1; 
	if (kptr>=KBSIZE) {
		knext();  
	}          
	kend = (long)kptr + kbase;
}
stkstr(sp)
/* Keywords: killstack stacking macro-programming:50 */

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

kput(ptr)
unsigned int ptr;
{
/* Keywords: killstack stacking string-variables macro-programming:20 */
	
	long real_ptr;
	int x;

	if (sizeof(x) < 4) {		/* If int's are only 16 bits, adjust ptr */
		
		real_ptr = kend & 0177777;
		if (real_ptr > ptr) real_ptr = ((long) ptr) + (kend & 037777600000);
		else real_ptr = ((long) ptr) + (kend & 037777600000) -65536;
	} else real_ptr = ptr;

	if ((real_ptr < 0) || (real_ptr >= kend)) {
		error (WARN,82);
		real_ptr = 0;
	}
	if (nkp == NKILLP)  {	/* no more kill pointers available */
		mvdown();
	}
	nkp++;
	kstk[nkp] = real_ptr;		/* start of saved text */
}
kgptr()
{
/* Keywords: killstack stacking insertion:10 retrieval:10 string-variables:10 */
	if (nkp>-1) {
		nkp--;
		return(kstk[nkp+1]);
	} else {
		return(0);			/* Error, too many pops */
	}
}

	

/* kapp -- append to kill buffer */

kapp()
{
/* Keywords: commands killstack appending */
	
	kbapp = 2;			/* force next kill to append */
}

/*knext -- next kill buffer*/


knext()
{
	long nbase;
/* Keywords: killstack paging */
	
	nbase = kbase + (long) KBSIZE;
	if (nbase >= KBLIM) nbase = 0;
	kget(nbase);
}


/* kget get next kill buffer */

kget(nbase)
long nbase;
{
/* Keywords: killstack paging paging-I/O */
	
	register long xbase;
	int st;
	
#ifdef	univac						/* MZ */
	xbase = nbase & 0xffffffe00;	/* Univac uses one's compliment. */
#else					/* MZ */
	xbase = nbase &( (long) -KBSIZE);
#endif					/* MZ */

	if (xbase == kbase) {
		kptr = nbase - kbase;
		return;
	}
	if (kfile == 0) {
		kfile = gtemp(NBUF);
	}
	if (kbmod) {
retry:		lseek(kfile,kbase,0);
		while(write(kfile,kbuf,KBSIZE) != KBSIZE) {
#ifdef PC
			if (mkspace()) goto retry;
			break;
#else
			if (errno != 4)	error(FATAL,errno,"kill buffer");
#endif
		}
		if (kbase >= kbwrt) kbwrt = kbase+KBSIZE;
		kbmod = 0;
	}
	if (nbase < kbwrt) {
		lseek(kfile,xbase,0);
		while((st=read(kfile,kbuf,KBSIZE) != KBSIZE)) {
			if (errno != 4) {
				error(FATAL,errno,"reading kill buffer");
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
/* Keywords: killstack stacking */

{
	register int i;
	for (i = 0; i <= nkp; i++) kstk[i] = kstk[i+1];
	nkp--;
}

/* retrv retrieves the top text segment from the kill stack and inserts
  * it into the buffer at the current location */


retrv()	/* retrieve from kill stack */
/* Keywords: commands killstack picture-mode:20 insertion:10 retrieval */

{
	register char c;
	register int oldcol,oldln;
	
	if (nkp <0) return(0);		/* nothing in the kill stack */
	RARE = 1;			/* turn off trouble */
	oldln = curln;
	oldcol = column;
	kget(kstk[nkp]);
	while (c= kbuf[kptr]) {
		if (PICMODE) {
			if (c==EOL) {
				move(++curln,0);
				lext(curln,oldcol);
			} else {
				insertc(1,c);
			}
		} else {
			if(put(c)==0) break;
		}
		if (++kptr >= KBSIZE) {
			knext();
		}
	}
	RARE = 0;			/* normal modes back on */
	unins(oldln,oldcol);
	return(1);				/* done */
}

retrvs(strp,mc)			/* retrieve from kill stack */
/* Keywords: commands killstack macro-programming retrieval */

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
	if (etrace) {
		putout("Retrieved %s from kill stack",strp-nc-1);
	}
	return(nc);				/* done */
}

/* kpop pops the top item off of the kill stack */
kpop()
/* Keywords: commands killstack stacking popping */

{
	if (nkp>-1) {
		nkp--;
		return(1);
	} else return(0);
}

/* fsave saves the buffer if it has been modified since last read or write */


fsave(arg)
register int arg;
{
/* Keywords: commands buffers files writing:50 saving dired:10 */
	
	if ((arg == 0) && (streq(bbfname[curbf],".exec"))) return(0);

#ifdef DIRED
	if (diron) return(dclean());
#endif
	if (READONLY) {
		if (arg) error(WARN,71);
		return(0);
	}
	if (bufmod && (*filename== 0)) {
		if (infrn == 0) return(fright(1)); /* no file name */
		error(WARN,65,bufname); /* Don't read the file name in a macro */
		return(0);
	}

	if (bufmod) {
		return(wout(filename,1));
	} else {
		if (arg == 0) return(1);
		prompt1("File %s is up to date",filename);
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

register int x;
/* Keywords: buffers:50 buffer-representation buffer-changing unix-interface macro-hooks:10 */

{
	register int i;
	register int rx;
	unsigned cpointer;

	
	if (SAVEMD) IGNORE(fsave(0)); /* save buffer */
	if (hooks[Leave_Buffer_Hook]) hook(Leave_Buffer_Hook);
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
#ifdef PC
			mkspace();
#endif
		}
	}
	bnlines[curbf] = nlines;
#ifdef MINFILES
	if (BUFILE) close(BUFILE);
	if (btmpfile[x]) btmpfile[x] = otemp(x);
#endif	
	curbf = x;
	bufinit();
	
	BUFEND = btmpfree[x];
	mostwrit = ((BUFEND-1)/BFACT);
	BUFILE = btmpfile[x];
	nlines = bnlines[x];
	if (BUFILE) {
		while (1) {
			lseek(BUFILE, (long)((long) BUFEND * (long) LSMALL),  0);
			rx = read(BUFILE,(char *)ptrs, (nlines+1)*(sizeof cpointer));
			if (rx ==(nlines+1)*(sizeof cpointer)) break;
			if (errno != 4) error (FATAL,errno,"Reading line pointers");
		}
	}
	
	bfmodes();			/* set buffer modes */
	move(bcurln[x],bcolumn[x]);
	if (bnlines[x] == 0) bufmod = 0; /* EMPTY BUFFERS CAN'T HAVE BEEN MODIFIED!!!! */
	if (hooks[Enter_Buffer_Hook]) hook(Enter_Buffer_Hook);
}

/* chgbuf changes buffers by name.  If no buffer of the right name
 * exists, one is made */


chgbuf(sp)
/* Keywords: buffers commands:10 buffer-representation:20 */

char *sp;

{
	int i;
	i = finbuf(sp,1);
	if (i >= 0) {
		chbuf(i);
		return(1);
	}
	return(0);
}


finbuf(sp,crflg)
/* Keywords: commands:20 buffers:50 files:50 reading:20 buffer-changing */

char *sp;
int crflg;
{
	register int ep = -1;
	register int i;
	
	if (*sp == 0) {
		error(WARN,36);
		return(-1);
		}
	for (i = 0; i < NBUF; i++) {
		if (streq(sp,bbfname[i])) {
			return(i);
		}
		if ((ep < 0) && (bbfname[i] [0] == 0)) ep = i;
	}
	i = aint(sp);			/* try numeric buffer name */
	if ((i < NBUF) && (i >= 0)) {
		if (bbfname[i] [0] == 0) {
			inibuf(i);
			seprintf(bbfname[i],"%d",i); /* name buffer */
		}
		return(i);
	}
	if (crflg == 0) return(-1);	/* Don't create new buffer */
	if (crflg < 0) {		/* Complain about buffer */
		error(WARN,39,sp);
		return(-1);
	}
	
/* Buffer does not now exist, create it */
	
	if (ep>=0) {
		inibuf(ep);
		if (streq(sp,"...")) seprintf(bbfname[ep],"%d",ep);
		else strcpy(bbfname[ep],sp);
		return(ep);
	} else {
		error(WARN,37,NBUF);
		return(-1);
	}
}

/* inibuf -- initialize buffer data */

inibuf(ep)
register int ep;				/* buffer number  */
/* Keywords: buffer-representation buffers commands:10 */

{
	bbfname[ep] [0] = 0;
	btmpfree[ep] = 0;
	bcurln[ep] = bcolumn[ep] = 0;
	bnlines[ep] = 0;
	bbfmod[ep] = 0;
	bfilname[ep] [0] = 0;
	mtime[ep] = 0;
}

/* edbuf is the find file command, it asks for a filename, finds a
 * buffer containing the file if it exists, otherwise it makes a new buffer
 * to hold the file */

edbuf(arg)
/* Keywords: commands buffers buffer-changing reading:10 files dired:20 */

int arg;
{
	register int i;
	int retv;
	int exists;
	char *sp;
#ifdef DIRED
	char nbuf[256];
#endif
	exists = 0;
	sp = expenv(getname("Filename to Find? "));
	if (sp == NULL) return(0);
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
			if (arg > 1) return(readin(sp,1));
			return(1);
		}
		if (streq(bbfname[i], sp)) {
			exists = 1;
		}
	}
	if (exists) retv = chgbuf("...");
	else retv = chgbuf(sp);
	if (retv) retv=readin(sp,arg);
	return(retv);
}

/* cpbuf is the change buffer command, it prompts for a buffer name and
 * mmoves to  it unless the name is '*', in which case a list of active
 * buffers is displayed */

cpbuf(arg)
/* Keywords: commands buffer-changing files:10 */
int arg;
{
	int i;
	i = fndbuf(arg);
	if (arg && (i >= 0)) chbuf(i);
	return(i>= 0);
}
buflist()				/* list active buffers */
/* Keywords: commands buffers user-interface informational-displays */

{
	register int i;
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


/* fndbuf finds/creates a buffer with a given name */


fndbuf(crflg)
int crflg;
{
/* Keywords: commands:20 buffers files:20 windows:10 */
	

	register char *bn;
	
again:	bn = getname("Buffer Name? ");
	
	if (bn == NULL) return(-1);
	if ((*bn == 0) || (*bn == '*')) {
		if(buflist()) return(-1);
		else goto again;
	}
	return(finbuf(bn,crflg));
}	

/* bufmove -- pipe text from one buffer to another */

/* sends the text in the region to a specified buffer */

bfsend(arg)

int arg;
{
	register int i;
	register int j;
	
/* Keywords: commands buffers regions insertion:20 buffer-changing:50 */
	
	i = fndbuf(1);
	if (i >= 0) {
		pickup(arg);		/* pick up the region */
		j = curbf;
		chbuf(i);
		if (i == procbuf) {
			bot(); /* Force insertion to the bottom */
			mark(curbf); /* Force mark to end of buffer */
		}
		retrv();
		kpop();
#ifndef PC
		if (i == procbuf) {
			exch(curbf);
			while (curln < nlines) {
				sendproc (mkline(curln)+column,leng(curln)+1-column);
				curln++;
				column=0;
			}
			sendproc (mkline(curln)+column,leng(curln)-column);
			bot();
		}
#endif
		chbuf(j);		/* back to old buffer */
	}
}

/* rnbuf -- rename buffer or buffer file */


rnbuf(arg)

int arg;
/* Keywords: commands buffers:50 files:50 naming */

{
	register char *sp;
	int bn;
	
	sp = expenv(getname("New Name? "));
	if ((sp== NULL) || ((arg == 1) && (*sp == NULL))) return;
	if (arg == 1) {			/* change buffer name */
		bn = finbuf(sp,0);
		if ((bn >= 0) && (bn != curbf)) {
					/* (allow change to current name */
			error(WARN,38,sp); /* Buffer name in use */
			return;
		}
		strcpy(bbfname[curbf],sp);
	} else {
		strcpy(bfilname[curbf],sp);
		mtime[curbf]=0;		/* Buffer wasn't read from this file */

	}
	dispmod();
}

/* rmbuf prompts for a buffer name and removes the buffer */


rmbuf()
/* Keywords: buffers commands deletion */

{
	register int i;
	
	if ((i = fndbuf(-1)) >= 0) {
		klbfr(i);
		return(1);
	} 
	return(0);
}
klbfr(i)

register int i;
/* Keywords: commands:10 buffers buffer-representation:20 deletion unix-interface:10 shell-escape:20 */

{
	if (i == curbf) {
		IGNORE(error (WARN,40));
		return;
	}
#ifndef PC
	if (i == procbuf) flushproc();
#endif
	if (i == windbuf()) onewind();		/* killing other window */

	if (btmpfile[i]) {
#ifdef MINFILES
		rftmp(i);
#else
		close(btmpfile[i]);
#endif
		btmpfile[i] = 0;
	}
	inibuf(i);
}

/* clean up unwritten buffers.  bclean checks for unwritten buffers, and
 * if any are found, writes them out if the user wishes */

bclean()
/* Keywords: dired:20 saving:70 exit-processing files:30 user-interface:10 */

{
	register int i;
	
	bbfmod[curbf] = bufmod;
	bnlines[curbf] = nlines;
#ifdef DIRED
	if (diron) fsave(0);
#endif	
	for (i = 0; i < NBUF; i++) {
		if ((bbfmod[i]) && (bbfname[i] [0]) &&
			(bnlines[i] > 1) &&
			(READONLY == 0) &&
			(streq(bbfname[i],".exec") == 0)){


			if (SAVEMD) {
				chbuf(i);
				fsave(0);
				
			} else {
				switch (gyn("buffer %s modified since last write to file %s, write?",
					&(bbfname[i][0]),&(bfilname[i][0]))) {
				case 0:
					break; /* no  */
				case 1:
					/* yes answer */
					chbuf(i);
					if(fsave(1)<= 0) return(-1);
					break;
				case -1:
				default:
					unprompt(); /* clean up msg */
					return(-1);
				}
			}
		}
	}
	return(0);
}

crash(arg)					/* handle crashes */

int arg;				/* sometimes is reason for crash */
{
/* Keywords: internal-errors saving unix-interface:30 files:20 */
#ifndef PC	
	register int i;
	register char *home;
	register int x = 0;

	signal(SIGHUP,SIG_IGN);
	signal(SIGINT,SIG_IGN);		/* go into our shell */

	if (arg == SIGHUP) {
		no_io = 1;	/* Make sure we do no I/O */
		close(0);
		close(1);
		close(2);
		open("/dev/null",2);
		dup(0);
		dup(0);
	}
	if (crashes++) eabort(0);	/* one crash per customer */
	home = getenv("HOME");

	bbfmod[curbf] = bufmod;

	for (i = 0; i < NBUF; i++) {
		if ((bbfmod[i]) && (bbfname[i] [0])){
			seprintf(bfilname[i],"%s/emacs%d",home,i);
			chbuf(i);
			mtime[curbf] = 0; /* Don't complain about previous file! */
			IGNORE(fsave(0));
			x++;		/* flag that we saved one */
		}
	}
	if (x) {
		USILENT++;
		infrn = -1;		/* Make sure that unx doesn't ask questions */
		unx("echo $LOGTTY your emacs buffers are in $HOME/emacs[0-9]* | mail $LOGNAME &",0);
	}
#endif
	quit();
}

/* collect -- garbage collection of buffer file */

collect()

/* Keywords: buffer-allocation buffer-representation killstack:20 */
{
	int oldcol;
	int oldln;
	int onlns;
	
	prompt1("Garbage collecting buffer");
	mflush(stdout);
	
	oldln = curln;
	oldcol = column;
	
	top();
	onlns = kline = nlines;
	kcol = leng(kline);
	tkill();			/* kill all text into kill buffer */
	
	bufinit();			/* reinit buffer storage */
	top();
	retrv();
	if (onlns != curln) {
		error(WARN,75);		/* buffer too large for emacs */
	}
	kpop();
	unpop(2);			/* Ignore for undo */
	move(oldln,oldcol);
	unprompt();
}

/* pshchr -- push a character into the macro buffer */

pshchr(ch)

register int ch;
/* Keywords: memory-allocation:10 macro-programming:10 paging:20 */
/* Keywords: buffer-allocation:20 buffer-representation:20 */
{
	if (macptr >= fbkno*BLEN) {
		if ((fbkno+2<NBLOCK) && bgrab(fbkno++,0)){
			/* grab another buffer */
			if (nxtflsh < fbkno) nxtflsh = fbkno;
			clptr=mkline(curln); /* Restore line pointer */
		} else error(FATAL,41); /* too many macros */
	}
	bbuf[0][macptr++] = ch;
}

pbfname()				/* put buffer name in kill stack*/
{
/* Keywords: commands macro-programming naming buffers */
	stkstr(bname());
}
pfnname()				/* put file name in kill stack */
/* Keywords: commands macro-programming naming files */
{
	stkstr(fname());
}

pvname(count)				/* put version in kill stack */
int count;
{
/* Keywords: commands macro-programming naming versions */
	if (count) stkstr(version);
	else stkstr(serial);
}

recurse(arg)				/* call emacs recursively */
int arg;
/* Keywords: user-interface:10 macro-programming:90 key-bindings:10 mode-line:5 recursive-editing commands:50 */
{
	long svkst[NKILLP+1];		/* kill buffer save area */
	int svnkp;
	register int i,c;
	char *omyname;
	char nmbuf[64];
	int eresult;
	

	if (arg == 0) {
		register char *map;
		map = getname(""); /* Termination map */
		pushin(NULL);		/* back to the tty */
		while (1) {
			disup();
			c = (getchar()&0177);

/* Map is a bit map of 4 bit bytes, each represented as a hex digit */
			
			i = map[c>>2];
			if (i>'9') i = 9+(i&017);
			else i = i - '0';
			if (8 & (i<<(c&3))) put(c);
			else {
				inpop();
				return(c);
			}
		}
	}
	for (i = 0; i <= NKILLP; i++) svkst[i] = kstk[i];
	svnkp = nkp;
	omyname = myname;
	pushin(NULL);		/* back to the tty */
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
/* Keywords: commands macro-programming killstack string-variables:20 stacking */
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
/* Keywords: commands macro-programming killstack string-variables:20 popping */
{
	while (count--) {
		if (kpop() == 0) return(0);
	}
	return(1);
}

kexch(count)				/* exchange kill stack */
/* Keywords: commands macro-programming killstack string-variables:20 */
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
/* Keywords: mode-line:20 buffers:80 file-modes:10 files:20 macro-programming:50 */
{
	if (arg == 1) bufmod = 0;
	else if (arg > 1) bufmod = 1;
	dispmod();
	return(bufmod);
}

#ifdef PC
#define KBDFILE "c:emk"
#define KBDMODE 1
#else
#define KBDFILE "$HOME/.emacs_kbd"
#define KBDMODE 0666
#endif
strtkbd()
{
/* Keywords: commands macro-programming:20 filenames:10 unix-interface:40 keyboard-macros */
	if (kbdfile) endkbd();
	kbdfile = creat(expenv(KBDFILE),KBDMODE);
	return(kbdfile);
}
endkbd()
{
/* Keywords: commands macro-programming:20 filenames:10 unix-interface:40 keyboard-macros */
	if (kbdfile) close(kbdfile);
	kbdfile=0;
}
exkbd(arg)
register int arg;
/* Keywords: commands macro-programming:20 filenames:10 unix-interface:40 keyboard-macros */
{
	while (arg-- > 0) infile(KBDFILE);
}
setkey()
/* Keywords: commands files file-modes:50 encryption */
{
#ifdef CRYPTO
	char *kp;
	kp = getname("Encryption Key? ");
	if (kp && *kp) {
		strcpy(cryptkey,kp);
		crypt = 1;
	} else crypt = 0;
#else
	beep();
#endif
}
#ifndef PC
access(path, amode)
char *path;
int amode;
/* Keywords: file-modes files reading:20 writing:20 unix-interface */
{
	struct stat stb;
	register int uid;

	/*** This nonsense would not be necessary if ***/
	/*** the saccess in sys2.c would do the check based on ***/
	/*** the effective uid and gid instead of the real ones ***/

	if( stat(path, &stb) < 0 ) {
		return(-1);
	}

	if( (uid=geteuid())==0 || amode==0 ) {
		/*** super user or existence check only ***/

		return(0);
	}

	if( uid == stb.st_uid ) {
		/*** uid's match ***/

		amode <<= 6;
	} else {
		if( getegid() == stb.st_gid ) {
			/*** gid's match ***/

			amode <<= 3;
		}
	}

	if( (stb.st_mode&amode) == amode ) {
		return(0);
	}
	return(-1);
}

#endif PC


undoit(n,doit)

/* Keywords: undo insertion:50 deletion:50 killstack:20 */

int n;
int doit;
{
	int unline;
	int uncol;
	long unparm;
	int untype;
	

	uncol = undostack[--n] & (MAXEL-1);
	unline = undostack[n]>>MAXELSH;
	move(unline,uncol);
	unparm = undostack[--n]>>UNDSHIFT;
	untype = undostack[n] & UNDMASK;

	switch(untype) {
		
	case UNINS:
		if (doit) {
			kline = unparm >>MAXELSH;
			kcol = unparm & (MAXEL-1);
			tkill();
			kpop();
		}
		break;
	case UNBAD:			/* Multiple undo that exceeds limits */
	
		if (error(WARN,85)) return(n);

	case UNMUL:			/* Multi-segment undo */
		while (unparm>0) {
			untype = undoit(n,doit);
			uncol = n-untype;
			if (uncol<0) uncol += NUNDO;
			unparm -= uncol/2;
			n = untype;
		}
		break;
	case UNDEL:
		if (doit) {
			if (nkp == NKILLP)  {	/* no more kill pointers available */
			mvdown();
			}
			nkp++;
			kstk[nkp] = unparm;
			retrv();
		}
		break;
	}
	if (n<= 0) n = NUNDO;
	return(n);
}

unadd()
{
/* Keywords: undo stacking */
	
	undostack[undop++] = ((long) curln) * MAXEL + column;
}

unins(stline,stcol)

/* Keywords: undo insertion stacking */

{
	long n;
	if (undop >= NUNDO) {
		undop = 0;
		unseg++;
	}
	n = ((long) stline) * MAXEL + stcol;
	undostack[undop++] = UNINS+ (n<<UNDSHIFT);
	unadd();
}

undel()

/* Keywords: undo deletion stacking */

{
	if (undop >= NUNDO) {
		undop = 0;
		unseg++;
	}
	undostack[undop++] = UNDEL+ (kend<<UNDSHIFT);
	unadd();
}

unstart()
{
	return (undop+unseg*NUNDO);
}
unend(unp)
register int unp;
{
	int untype;
	
	unp = (undop+unseg*NUNDO)-unp;
	if (unp >= NUNDO/2) {
		untype= UNBAD;
		unp = NUNDO/2-2;
	} else untype = UNMUL;
	unp = unp / 2;
	if (unp == 1) return;		/* No point in storing extra indirection */
	if (undop >= NUNDO) undop = 0;
	undostack[undop++] = untype + (unp<<UNDSHIFT);
	unadd();
}

undo(arg)

/* Keywords: commands undo */

{
	int undp;
	int unp;
	
	unp = unstart();			/* Set up for undoing undo */
	undp = undop;
	while (arg--) {
/* Only the last undo really undoes anything */
		
		undp = undoit(undp,(arg == 0));
	}
	unend(unp);
}
unpop(n)
{
	undop -= 2*n;
	if (undop < 0) undop += NUNDO;
}
