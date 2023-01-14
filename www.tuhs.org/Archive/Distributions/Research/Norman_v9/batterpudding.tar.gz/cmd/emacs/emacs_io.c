/* EMACS_MODES: c !fill */
#ifdef v8
#include <sys/types.h>
#endif
#include "emacs_gb.h"
#include "emacs_io.h"
#ifndef PC
#include <signal.h>
#endif
#ifdef bsd
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <errno.h>
#ifdef ux3
#include <termio.h>
#else
#include <sgtty.h>
#endif

#ifdef bsd

/* The following defines turn on the use of the select system call
 * and Cbreak mode  for berkeley unix.  If you are running a version
 * of berkeley unix without these features, taken them out */

#define SELECT
#define CBRAKE
#define FLIM
#endif
#ifdef ux3
#define FLIM
#endif


#ifdef COMPRESS
long coutc = 0;
int DOCOMP=0;
char *trtab[128] = {
	
#include "compress.h"
};

#endif

#ifdef NDLAY
#include <sys/types.h>
#ifdef ux3
#include <fcntl.h>
#endif ux3

int ndfrn;				/* file for ndelay I/O */
#endif

extern int SAVEMD;
extern int curbf;
extern char *getenv();

#ifdef PC
#define READM (0)
#define APPEM (1)
#define WRITEM (1)
#else
#define READM 0
#define APPEM 1
#define WRITEM 0666

int READ_WAIT = 255;
int read_miss;
#endif
/* TTY buffer */

/* if unix3.0 (or later) system with no wait raw-mode I/O, use a big tty */
/* buffer to allow type ahead and avoid excessive re-display */

#if (defined(ux3) || defined(bsd) || defined(v8))
#define TTLOOK 16
#else
#define TTLOOK 1
#endif
int ttcnt ;
char ttbuf[18];				/* Make large */
char *ttptr;
int ungc = 0;
extern int ttywarp;

/* Interrupted system command flag */
int irupt = 0;
#define MAXIRUPT 100

/* controlification stuff */

int ctlify;
int CONCHAR = 036;			/* ^^ */
ioinit()
{
/* Keywords: internal-initialization standard-I/O files:20 */
	register char *ttname;
	
	xclose(3);			/* close all files */
	_stdout._frn = 1;
	_stdout._cnt = 0;
	_stdout._flags = _OUTPUT;
	incnt = inlev = infrn = 0;
	infrn = NULL;
	brkflg = 0;
	inbuf = _inbuf[0];
#ifdef NDLAY
#ifdef ux3

#ifdef pdp11
#define PDPTEST (ttywarp<2)
#else
#define PDPTEST 0
#endif

/* IF we have NDELAY I/O, close standard error and open it with ndelay */

	close(2);			/* close standard error */
	

	if ((ttname=getenv("LOGTTY"))==NULL) ttname="/dev/tty";
	if (PDPTEST || (open(ttname,O_RDWR+O_NDELAY)<0)) {
			dup(1);
			ndfrn = 0;
	} else ndfrn = 2;
	
					/* if we can't get tty, restore frn2  */
#endif ux3
#ifdef (defined(bsd) || defined(v8))
/*
 * we can see how many chars await us in bsd4.1,
 * so set ndfrn if the tty is fast enough for us. - CRC
 */
	ndfrn = 2;	/* CRC */

#endif (bsd || v8)		/* CRC */

#endif 				/* CRC */
}
int FLOWMIN=0;				/* Mode variable must be present */

#ifdef PC

cook()
{
}
uncook()
{
}
#else
/* into raw mode */


/* Terminal I/O modes, sgttyb for before unix 3.0, termio for later */


int xon;

#ifdef ux3
struct termio ttyjunk;
struct termio nttyjunk;
#else
struct sgttyb ttyjunk;
struct sgttyb nttyjunk;
#endif

#ifdef CBRAKE
/* Additional goodies for 4.2BSD */

int ldisc;				/* Line discipline */
struct tchars tchars;			/* Basic characters */
int locmode;				/* Local mode */
struct ltchars ltchars;			/* And at last, local modes */
struct ltchars zchars = {
	-1,-1,-1,-1,-1,-1 };			/* Zero characters for ioctl */
struct tchars fcoff = {0,-1,-1,-1,-1,-1};
struct tchars fcon = {0,-1,021,023,-1,-1};
int mylmode = LLITOUT|LDECCTQ;		/* Literal output, ^Q starts ^S */
#endif


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
	
uncook()
{

/* UNIX 3 code thanks to J. Langer and M. Plotnick */

/* Keywords: the-terminal internal-initialization:40 terminal-initialization terminal-modes unix-interface:50 */
#ifdef ux3


	ioctl(1, TCGETA, &ttyjunk);
	nttyjunk=ttyjunk;

#ifdef u370

	nttyjunk.c_iflag |= (BRKINT|ISTRIP|IGNPAR);
	nttyjunk.c_iflag &= 
		~(IGNBRK|IGNPAR|PARMRK|IXON|INLCR|IGNCR|ICRNL|INPCK);
	/* accept break, no crnl mapping, no ^S^Q */ 
	nttyjunk.c_oflag = 0;		/* no delays, no crlf mapping */
/*	nttyjunk.c_cflag |= CS8 ;*/
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
	nttyjunk.c_cc[VMIN] =  0;	/* return after every character read */

/* Setting VMIN to 0 causes emacs to time out input every 25.5 seconds,
 * updating the display of time (if time mode is on) and received
 * mail.  Setting it to 1 will cause emacs to wait indefinetely for
 * input.  */

	nttyjunk.c_cc[VTIME] = READ_WAIT; /* Or after 25.5 seconds */
	ttywarp = charms[ttyjunk.c_cflag&CBAUD]; /* milliseconds for character */

	ioctl(1, TCSETAW, &nttyjunk);
	ioctl(1, TCXONC,1);		/* Force tty back on */
	xon = 0;						/* Xon/XOff is now off */

#else

	
	ioctl(1,TIOCGETP,&ttyjunk);
	nttyjunk=ttyjunk;
#ifdef CBRAKE
	nttyjunk.sg_flags = CBREAK;
	ioctl(1,TIOCSETN,&nttyjunk);

	/* Now for the real fun, get all of the other 4.2BSD goodies */
	
	ioctl(1,TIOCGETC,&tchars);
	ioctl(1,TIOCSETC,&fcoff);
	ioctl(1,TIOCLGET,&locmode);
	ioctl(1,TIOCLBIS,&mylmode);
	ioctl(1,TIOCGLTC,&ltchars);
	ioctl(1,TIOCSLTC,&zchars);
	xon = 0;
#else
	nttyjunk.sg_flags &= (~ECHO);	/* it was so SIMPLE in the old days */
	nttyjunk.sg_flags |= (RAW);
	ioctl(1,TIOCSETP,&nttyjunk);
#endif
	ttywarp = charms[ttyjunk.sg_ospeed]; /* milliseconds for char */
#endif

	vinit();	

	/* Anything above 2500 baud may cause problems */
	/* If the user has not selected a limit we'll set one */
/* Commented out, remove this line to enable it
	if ((ttywarp < 4)&&(FLOWMIN==0)) FLOWMIN = 64;  /*  */

}

cook()
{
/* Keywords: the-terminal terminal-modes exit-processing:50 unix-interface:50 */
	extern int osert,umode;
	
/* First, return the terminal to a sane state */

	if (no_io) return;
	if (umode) unline();
	if (osert) unsert();
	vexit();
	mflush(stdout);			/* force output */
#ifdef ux3
	ioctl(1,TCSETAW, &ttyjunk);
	ioctl(1, TCXONC,1);		/* Force tty back on */
	xon = 1;						/* XON/XOFF is now ON */
#else
#ifdef CBRAKE
	ioctl(1,TIOCSETN,&ttyjunk);
	ioctl(1,TIOCSETC,&tchars);
	ioctl(1,TIOCLSET,&locmode);
	ioctl(1,TIOCLBIC,&mylmode);
	ioctl(1,TIOCSLTC,&ltchars);
	xon = 1;
#else
ioctl(1,TIOCSETP,&ttyjunk);
#endif
#endif
}
#endif

/*VARARGS2*/

char *
nscan(stptr,ret)

register char *stptr;
register int *ret;

/* Keywords: standard-I/O:20 conversions parsing:20 reading:20 */
{
	register int c;

	*ret = 0;
	while (((c = *stptr)>='0') && (c <= '9')) {
		stptr++;
		*ret = *ret*10+(c-'0');
	}
	return(stptr);
}


seprintf(string,fmt,x)
char *string;
char *fmt;
unsigned x;
{
	sxprintf(string,fmt,&x);	/* Depends on arg list format */
}

/* Internal printf formatter */

sxprintf(string,fmt, adx)
register char *string;
register char *fmt;
register unsigned int *adx;
{
/* Keywords: standard-I/O:50 formatting writing:20 terminal-parameters:20 */
	int c;
	int width;

	extern char *mstrcpy();
	
	if (fmt == NULL) fmt = "";

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
				if (sizeof(n) != sizeof(i)) {
					if (n > 32768L) n = n-65536L; /* sign correction */
				}
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
			while (i > 0) {
#ifdef PC
				char cq;
				cq = dstack[--i];
				cq += '0';
				*string++ = cq;
#else				
				*string++ = (dstack[--i] + '0');	/* print number */
#endif
			}
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
		string = mstrcpy(string,(char *)*adx);
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
		c = *adx + width;
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
		error(WARN,70);		/* Bad character in printf */
	}
	adx++;
	goto loop;
}
char *inget()
{
/* Keywords: command-files:10 */
	if (infrn < 0)return(inptr);
	else return(NULL);
}
inset(newptr)
char *newptr;
/* Keywords: command-files:10 */
{
	if (infrn < 0) inptr = newptr;
}

#ifdef SELECT
int sreadfd = 1;
struct timeval stimeout = {30,0};	/* timeout interval */
#endif
#ifdef v8
fd_set sreadfd;
#endif
int
getchar()
{
/* Keywords: the-terminal standard-I/O terminal-modes:10 reading mail-processing:20 time-processing:20 prompting:30 conversions:10 PC-only:10 keyboard-macros:20 unix-interface:10 */
	
	extern int timemd;
	extern int newmail;
	extern int mailcnt;
	extern int kbdfile;
	extern char ctype[];
	extern char cbuf[];
	register int c;

	
	if (no_io) return(CTRLZ);	/* Flush I/O */
	if (brkflg) brkit();		/* handle break interrupt */
	if (incnt == 0) {
		while (1) {
			errno = 0;
			if (infrn) {
				inptr = inbuf;
				incnt = read(infrn,inbuf,INLOOK);
			} else {

/* The following code expects read from a raw mode tty port to
 * return whenever at least one character has been received or a
 * timeout expires.  It assumes that if it gets 0 characters, the
 * timer expired, unless it happens too many times in a row. */

reread:				ttptr = ttbuf+2;
#ifdef PC
				ttcnt = incnt = rawread(ttbuf+2);
#else

				read_miss = 0;
				if (donttime) goto notime; /* Avoid time and mail */
				
/* First, update time and mail */

				if (timemd) dtime(0);
				if ((mailcnt>=0) && (--mailcnt<=0)) {
					ckmail();
				}

/* Display them if necessary */
				if (newmail) {
					int x,y;
					x = mline;
					y = mcol;
					prompt(ECHOL-1,"%s You have mail",cbuf);
					if (newmail < 0) beep();
					mgo(x,y);
					newmail = 1;
				} else if (disptime)  {
				
					int x,y;
					x = mline;
					y = mcol;
					prompt(ECHOL-1,cbuf);
					disptime=0;
					mgo(x,y);
				}
notime:
/* Now try to read some standard input */
				
				mflush(stdout);
#ifdef FLIM
				if (xon && (FLOWMIN>=0)) {	/* If xon/xoff is on, turn it off */
#ifdef CBRAKE

					quietout();
					ioctl(1,TIOCSETC,&fcoff);
#else
					nttyjunk.c_iflag &= ~(IXON|IXANY);
					ioctl(1,TCSETAW,&nttyjunk);
#endif
					xon = 0;
				}
#endif
#ifdef SELECT
				if (inproc){
					sreadfd = 1+(1<<inproc);
					ttcnt = select(inproc+1,&sreadfd,NULL,NULL,&stimeout);
					if (sreadfd > 1) readproc();
				} else {
					sreadfd = 1; /* must set up each time since select clobbers it */
					ttcnt = select(1,&sreadfd,NULL,NULL,&stimeout);
				}
				if ((sreadfd & 1) && (brkflg == 0)) ttcnt = incnt = read(0,ttbuf+2,TTLOOK);
				else  {
					incnt = 0;
					errno = EINTR; /* Make it look like the read timed out! */

				}
#else
#ifdef v8
				if (inproc){
					FD_ZERO(sreadfd);
					FD_SET(0, sreadfd);
					FD_SET(inproc, sreadfd);
					ttcnt = select(inproc+1,&sreadfd,NULL,30000);
					if (FD_ISSET(inproc,sreadfd))
						readproc();
				} else {
					FD_ZERO(sreadfd);
					FD_SET(0, sreadfd);
					ttcnt = select(1,&sreadfd,NULL,30000);
				}
				if (FD_ISSET(0,sreadfd) && (brkflg == 0))
					ttcnt = incnt = read(0,ttbuf+2,TTLOOK);
				else  {
					incnt = 0;
					errno = EINTR; /* Make it look like the read timed out! */

				}
#else
				ttcnt = incnt = read(0,ttbuf+2,TTLOOK);
				if (inproc) readproc(); /* Process any input */
#ifdef ux3
				if (ttcnt) ioset(10);
				else ioset(READ_WAIT<<1); /* Back off read timeout next time */
#endif ux3
#endif v8
#endif SELECT
				if (brkflg) {
					incnt=ttcnt=0;
					brkit();
					disup();
					goto reread; /* Try again for a charactger */
					
				}

				if (incnt == 0) {
					mailcnt = 0; /* Expire mail timer */
					if (++read_miss < 1000) goto reread;
				}
#endif PC
			}
			ninch+= incnt;		/* count for stats */
			if (incnt >= 0) break;
			if ((errno != EINTR)|| (++irupt > MAXIRUPT)) break;
		}

	}

	if (infrn < 0) {		/* if in macro */
		incnt++;
	}
	if (infrn) {
		if (incnt-- >0)  return(*inptr++ & 0377);
	} else {
		if ((incnt= --ttcnt) >= 0)  {
			if (ungc) {
				ungc--;
				return(*ttptr++ & 0377);
			}
			c = (*ttptr++ & 0177);
			if (ctlify && (c == CONCHAR)) {
				ctlify = 0; /* don't controlify again */
				
				c = getchar();
				if ((c>= 'a') && (c <= 'z')) c -= 040;
				if ((ctype[c] == PLAIN) ||(ctype[c] == UL)) c ^= 0100;
				ctlify++;
			}
			if (kbdfile) {
				char x;
				x=c;
				write(kbdfile,&x,1);
			}
			return(c);
		}
	}
	return(CTRLZ);
}
#ifndef PC
#ifdef ux3

/* Set non-blocking terminal timeout appropriately */

/* Note -- this is a very imperfect simulation of the select system
 * call in 4.2bsd for system V unix.  What is done is that if no
 * process is running, the timeout is set to 25.5 seconds.  If a
 * process is running, the timeout is decreased after every piece of
 * input is sent, and rises as reads time out, returning eventually
 * to 25.5 seconds.  Thus quick processes get good results, but slow
 * ones can get dismal response.  Emacs polls the sub-process(s)
 * each time it reads  from the tty, so the timeout just dictates
 * how often this happens if there is no tty input. */


ioset(limit)
int limit;
{
/* Keywords: unix-interface reading shell-escape sub-processes look-ahead: 10*/

	if (READ_WAIT == 0) return;	/* No non-blocking I/O available */
	if (limit > 255) limit = 255; /* Clamp it */
	
	if (READ_WAIT == limit) return;
	READ_WAIT = limit;
	nttyjunk.c_cc[VTIME] = READ_WAIT;
	ioctl(1, TCSETA, &nttyjunk);	

}
#endif

/* Process input from process in the window */

readproc ()
{
/* Keywords: unix-interface reading shell-escape sub-processes buffers:50 */
	

	char buf[128];
	int nc,obuf;
	int cnt;
	
#ifdef v8
	ioctl(inproc, FIONREAD, &cnt);
	if(cnt <= 0)
		return;
#endif
	nc = read(inproc,buf,127);
	buf[nc]=0;
	if (nc) {
		if (curbf != procbuf) {
			if (procbuf == windbuf()) {
				owind();
				stuffproc(buf);
				disup();
				owind();
				disup();
			} else {
				obuf=curbf;
				chbuf(procbuf);
				stuffproc(buf);
				chbuf(obuf);
			}
		} else {
			stuffproc(buf);
			disup();	/* Update display for user */
		}
	} else {
	}
}

/* stuffproc -- stuff process input into the current buffer */

stuffproc(string)
char *string;
{
/* Keywords: unix-interface reading shell-escape sub-processes buffers:50 */
	
	bot();
	putin(string);
	mark(curbf);
}

/* sendproc -- send current line to the process */

sendproc (ptr,count)
char * ptr;
int count;
{
/* Keywords: unix-interface  shell-escape buffer-representation:10 buffers:50 writing sub-processes */
	if (write(outproc,ptr,count)<0) flushproc();
#ifdef ux3
	ioset(1);			/* Set back timeout appropriately */
	
#endif
}

/* brkproc -- send an interrupt to the current sub-process */

brkproc(arg)

/* Keywords: unix-interface shell-escape break-handling  sub-processes */


{
	if (curbf == procbuf) {
		if (arg== 1) arg = SIGINT;
		kill(procpid,arg);
	}
}

/* flushproc -- rid ourselves of the current sub-process */

flushproc()
{
/* Keywords: unix-interface shell-escape closing sub-processes */
	
	int status;
	
	if (procpid) {
		kill (procpid,9);
		status=wait(&status);
		close(inproc);
		close(outproc);
		procpid=inproc=outproc=0;
		procbuf= -1;
	}
}
#else
/* Define some tombstones for this code.  It's easier than ifdeffing out all the calls */
brkproc()
{
}
#endif
/* Fill tty buffer if we have ndelay I/O */

ttfill()
/* Keywords: the-terminal terminal-modes lookahead reading unix-interface:50 */

{
#ifdef NDLAY
	long i;
	register char *addr;
	
	if (no_io || (ndfrn == 0)) return;
	mflush(stdout);			/* flush tty */

#if (defined(bsd) || defined(v8))	/* CRC */
	i = 0;				/* CRC */
	ioctl(2,FIONREAD,&i);		/* CRC */
	if( i <= 0)			/* CRC */
		/* no input awaits us.	 - CRC */
		 return;		/* CRC */
#endif (bsd || v8)
	if (ttcnt) {
					/* more input, append to it */
		addr = ttptr+ttcnt;
		i = read(2,addr,ttbuf+2+TTLOOK-addr);
	} else {
		i = read(2,ttbuf+2,TTLOOK);
		ttptr = ttbuf+2;
	}
	if (i < 0) i = 0;
	ttcnt += i;			/* more characters */
	if (infrn == 0) {		/* reading from tty */
		incnt = ttcnt;
		inptr = ttptr;
	}
#endif
	return;
}
pushin(fp)
/* Keywords: command-files:50 keyboard-macros:20 opening unix-interface:10 */
register char *fp;
{
	register int frn;
	
	if (fp != NULL) {
		while((frn = open(fp,READM)) <= 0) {
			if ((errno != EINTR) && (errno != 23))break;
		}
	} else frn = 0;
	if (frn >= 0) {
		_incnt[inlev] = incnt;
		_infrn[inlev] = infrn;
		_inptr[inlev++] = inptr;
		infrn = frn;
		if (frn) incnt = 0;
		else incnt = ttcnt;
		return(1);
	} else return(0);
}

pshmac(pos)
/* Keywords: macro-invocation */

short pos;
{
	
	if (inlev>= NINP) error(FATAL,64);
	_incnt[inlev] = incnt;
	_infrn[inlev] = infrn;
	_inptr[inlev++] = inptr;
	infrn = -1;
	incnt = 100;			/* will never decrease */
	inptr = &bbuf[0][pos];		/* macro buffer position */
	return(1);
}



inpop()
/* Keywords: exit-processing:10 macro-invoction:20 command-files:20 */
{
	if (inlev > 0) {
		--inlev;
		if (infrn>0) close(infrn);
		infrn = _infrn[inlev];
		inptr = _inptr[inlev];
		inbuf = _inbuf[inlev];
		if (infrn) incnt = _incnt[inlev];
		else incnt = ttcnt;
		return(1);
	} else 	return(0);
}

#ifdef COMPRESS

#define CRELOAD 0376

translate(buf,count)
/* Keywords: standard-I/O:10 compressed-output conversions writing */
char *buf;
int count;
{
	register char *bp;
	char *be;
	register char *bo;
	register char *mp;
	register char *tp;
	bp=bo=buf;
	be=buf+count;
	
	while (bp<be-1) {
		if (*bp&0200) {
			*bp = 0;	/* PUNT all meta characters to 0 */
			goto stop;
		}
		mp=trtab[*bp];
		while (*mp) {
			tp=bp+1;
			while (*tp++ == *mp++);
			if ((mp[-1]&0200)){
				if (tp > be+1) {
					goto stop;
				}
				 /* Matched a substring */
				*bo++ = mp[-1];
				bp = tp-1;
				goto out;
			}
			while ((*mp++ & 0200) == 0); /* Skip rest of string */
		}
stop:		*bo++ = *bp++;
out:		continue;
	}
	if (bp < be) 	*bo++ = *bp++;
	return(bo-buf);
}

loadtbl()
/* Load compression table here and remote */
/* Keywords: commands the-terminal:90 conversions compressed-output */
{
	extern int macptr;
	char *nullp;
	char c;
	int nc;
	FILE fbuf;
	FILE *fp;
	int i;
	
	if (DOCOMP==0) return;			/* Can't do compression */
	nullp = &bbuf[0][macptr];
	pshchr(0);			/* Store a null string for common cases */
	fp = xopen(&fbuf,expenv(getname("Compression file: ")),"r");
	if (fp == NULL) return;
	DOCOMP=0;
	mflush(stdout);			/* Past the point of no return */
	i = 0;
	while (getc(fp) != EOF) {
		nc = 0;
		while (getc(fp) == '\\') {
			if (nc == 0) trtab[i] = &bbuf[0][macptr];
			nc++;
			c = ((getc(fp)&07)<<6);
			c += ((getc(fp)&07)<<3);
			c += (getc(fp)&07);
			pshchr(c);
		}
		c = getc(fp);		/* Eat comma */
		c = getc(fp);		/* Eat newline */
		if (nc) pshchr(0);	/* Put in null */
		else trtab[i]=nullp; /* Null string */
		i++;
	}
					/* OK, host table loaded, now load the blit */
	mclose(fp);
	fp = xopen(&fbuf,expenv(getname("Decompression file: ")),"r");
	if (fp == NULL) return;
	putchar(CRELOAD);		/* Tell blit it's time to reload */
	while ((c = getc(fp)) != EOF) putchar(c);
	putchar(CRELOAD);		/* Reload done (let's hope!) */
	mflush(stdout);			/* Force output out before we turn on compression */
	DOCOMP=1;			/* Here goes nothing */
	clear();			/* refresh screen */
	mclose(fp);
}
#endif
ungetch(c)
/* Keywords: the-terminal:90 standard-I/O parsing:10 reading:10 */
{
	if (incnt < 0) return;		/* Can't unget an eof */
	++incnt;
	if (infrn==0) {
		++ungc;
		*(--ttptr) = c;
		++ttcnt;
	} else {
		*(--inptr) = c;
	}	
}

mflush(p)

register FILE *p;
/* Keywords: writing the-terminal:10 standard-I/O PC-only:40 unix-interface:20 */
{
	register int bc;
	register int bo;

	if (p->_flags & _OUTPUT) {
		if (p->_flags & _ERROR) {
			p->_cnt = 0;
			return;
		}
		if (p == stdout) {
			if (no_io) {
				p->_cnt = 0;
				return;
			}
			ntwrite++;		/* count for stats */
			noutc += p->_cnt;
#ifdef PC
			return;		/* No output to stdout */
#endif PC
#ifdef COMPRESS
			if (DOCOMP&& (p->_cnt > 10)) p->_cnt = translate(p->_buf,p->_cnt);
			coutc += p->_cnt;
#endif
#ifdef FLIM
			if (FLOWMIN && (p->_cnt > FLOWMIN) && (xon == 0)) {
#ifdef CBRAKE
				quietout();
				ioctl(1,TIOCSETC,&fcon);
#else
				nttyjunk.c_iflag |= IXON+IXANY;
				ioctl(1,TCSETA,&nttyjunk);
#endif
				xon = 1;
			}
#endif
		}
#ifdef PC
		if (p->_cnt) {
			 if ((bc = write(p->_frn, p->_buf, p->_cnt)) < p->_cnt) {
				error (NORM,errno,"writing");
			}
		}
#else
		bo = 0;
		while((bc = write(p->_frn, p->_buf+bo, p->_cnt)) != p->_cnt) {
			if (bc >0) {
				p->_cnt -= bc;
				bo += bc;
			}
			if ((errno != EINTR)|| (++irupt > MAXIRUPT)) {
				if ((p == stdout) || (errno == EPIPE)) break; /* PUNT
					 * errors on standard output */
				p->_flags |= _ERROR;
				SAVEMD = 0; /* Give up on autosaving */
				error (NORM,errno,"writing");
				break;
			}
		}
#endif PC
		p->_cnt = 0;
	}
}

#ifdef CBRAKE
/* Wait for quiet output on berkely systems.  This is very imperfect */
/* but it's all we can do. */

quietout()
{
	int outqs;
	struct timeval outime;
	long timeout;
	
	ioctl(1,TIOCOUTQ,&outqs);
	while (outqs > 0) {

		timeout = outqs*1000 /ttywarp - 3000; /* Microseconds of timeout */
		if (timeout > 0) {

			outime.tv_sec = timeout/1000000;
			outime.tv_usec = timeout%1000000;
			select(0,NULL,NULL,NULL,&outime); /* Wait for something to happen! */
		}
		ioctl(1,TIOCOUTQ,&outqs);
	}
}
#endif
filbuf(p)

FILE *p;
{
/* Keywords: reading standard-I/O PC-only:40 unix-interface:10 */
#ifdef PC
	p->_cnt = read(p->_frn, p->_buf, BUFSIZ);
#else
	mflush(stdout);			/* Make output come out */
	while ((p->_cnt = read(p->_frn, p->_buf, BUFSIZ)) < 0) {
		if ((errno != EINTR)|| (++irupt > MAXIRUPT)) break;
	}
#endif PC
	p->_ptr = &(p->_buf[1]);
	if (p->_cnt > 0) {
		p->_cnt--;
		return(p->_buf[0]&0377);
	}
	else p->_cnt = 0;
	return(EOF);
}

/*VARARGS1*/

eprintf(string,a1)

char *string;
/* Keywords: formatting standard-I/O:50 terminal-parameters:50 */
{
	char pbuf[1024];
	sxprintf(pbuf,string,&a1);
	puts(pbuf,stdout);
}

puts(cp,p)

FILE *p;
register char *cp;
/* Keywords standard-I/O writing */
{
	while (*cp) {
		putc(*cp++,p);
	}
}

mclose(p)

FILE *p;
/* Keywords: standard-I/O closing files unix-interface */
{
	mflush(p);
	close(p->_frn);
	return(p->_flags & _ERROR);
}

FILE *

xopen(p,np,mode)

FILE *p;
char *np;
char *mode;
/* Keywords: files opening standard-I/O unix-interface */
{
	int omode;
	
	omode = 0;
	if (*mode == 'b') {
#ifdef PC
		omode = 4;
#endif
		mode++;
	}
	switch(*mode) {
	case 'r':			/* read file mode */
	
		while ((p->_frn = open(np,omode+READM)) <0) {
			if ((errno != EINTR) && (errno != 23))return(NULL);
		}
		p->_flags = _INPUT;
		break;
		
	case 'a':
		while((p->_frn = open(np,omode+APPEM)) <0) {
			if ((errno != EINTR) && (errno != 23))return(NULL);
		}
		lseek (p->_frn,0L,2); /* at end */
		p->_flags = _OUTPUT;
		break;
	case 'w':

		while((p->_frn = creat(np,omode+WRITEM)) <=0) {
			if ((errno != EINTR)&& (errno != 23)) return(NULL);
		}
		p->_flags = _OUTPUT;
		break;
	}
	p->_cnt = 0;
	p->_ptr = 0;
	return(p);
}

FILE *

fdopen(p,frn,mode)

FILE *p;
int frn;
char *mode;
/* Keywords: standard-I/O opening unix-interface */

{
	p->_frn = frn;
	p->_cnt = 0;
	p->_ptr = &(p->_buf[0]);
	p->_flags = _DEAD;
	switch(*mode) {
		
	case 'r':
		p->_flags = _INPUT;
		break;
	case 'w':
		p->_flags = _OUTPUT;
		break;
	}
	return(p);
}

/* ascii to integer */

aint(string)

register char *string;
/* Keywords: standard-I/O:10 conversions string-handling:10 */
{
	register int result;
	register int base;
	
	base = ((*string == '0') ? 8 : 10);
	result = 0;
	while ((*string >= '0') && (*string <= '9')) {
		result = (result * base) + (*string++ - '0');
	}
	if (*string == 0) return(result);
	else return(-1);
}

xclose(lowfile)

register int lowfile;
/* Keywords: unix-interface files closing */
{
	register int fileno;
#ifndef	PC
	for (fileno = lowfile; fileno < 20; fileno++) {
		close(fileno);
	}
#endif PC
}
#ifdef PC
/* All the nice? C routines you don't really need! */
signal()
{
	return(0);
}
char *
getenv()
{
	return(0);
}
char cbuf[2];				/* Fake character buffer */
#else

/* ctime stuff */



char	cbuf[26];
char	dmsize[12] =
{
	31,
	28,
	31,
	30,
	31,
	30,
	31,
	31,
	30,
	31,
	30,
	31
};

long timez = 0;
int daylite = 1;

#define dysize(xyear) ((xyear&03) ? 365 : 366)


struct tm	*gmtime();
struct tm	*localtime();
char	*ctime();
char	*asctime();

char *
ctime(t)
long *t;
{
/* Keywords: time-processing */
	return(asctime(localtime(t)));
}

#ifdef ux3
void
#endif
tzset()
{
	register char *p;
	register int n;
	int sign;
/* Keywords: time-processing internal-initialization */
	if ((p = getenv ("TZ")) && *p) {

		p += 3;
		if (sign = *p == '-')
			p++;
		n = 0;
		while (*p >= '0' && *p <= '9')
			n = (n * 10) + *p++ - '0';
		if (sign)
			n = -n;
		timez = ((long) (n * 60)) * 60;
		if (*p) daylite = 1;
		else daylite = 0;
	}
}

struct tm *
localtime(tim)
long *tim;
{
/* Keywords: time-processing */
	register int dayno;
	register struct tm *ct;
	register daylbegin, daylend;
	long copyt;

	if (timez == 0) tzset();

	copyt = *tim - timez;
	ct = gmtime(&copyt);
	if (!daylite) return(ct);
	dayno = ct->tm_yday;
	daylbegin  = 119;	/* last Sun in Apr */
	daylend  = 303;		/* Last Sun in Oct */
	daylbegin = sunday(ct, daylbegin);
	daylend = sunday(ct, daylend);
	if ((dayno>daylbegin || (dayno==daylbegin && ct->tm_hour>=2)) &&
	    (dayno<daylend || (dayno==daylend && ct->tm_hour<1))) {
		copyt += 1*60*60;
		ct = gmtime(&copyt);
	}
	return(ct);
}

/*
 * The argument is a 0-origin day number.
 * The value is the day number of the first
 * Sunday on or after the day.
 */
sunday(t, d)
register struct tm *t;
register int d;
{
/* Keywords: time-processing */
	if ((d >= 58) && ((t->tm_year & 03) == 0)) d+=1; /* leap year */
	return(d - (d - t->tm_yday + t->tm_wday + 700) % 7);
}

struct tm *
gmtime(tim)
long *tim;
{
/* Keywords: time-processing */
	register int d0, d1;
	int day;
	long hms;
	static struct tm xtime;

	/*
	 * break initial number into days
	 */
	day = *tim / 86400;
	hms = *tim - (day*86400);

	/*
	 * generate hours:minutes:seconds
	 */
	d1 = hms/60;
	xtime.tm_sec = hms - (((long)d1)*60L);
	xtime.tm_min = d1%60;
	d1 /= 60;
	xtime.tm_hour = d1;

	/*
	 * day is the day number.
	 * generate day of the week.
	 * The addend is 4 mod 7 (1/1/1970 was Thursday)
	 */

	xtime.tm_wday = (day+4)%7;

	/*
	 * year number
	 */
	for(d1=70; day >= dysize(d1); d1++)
		day -= dysize(d1);
	xtime.tm_year = d1;
	xtime.tm_yday = d0 = day;

	/*
	 * generate month
	 */

	if ((d1&03) == 0)dmsize[1] = 29;
	else dmsize[1] = 28;
	for(d1=0; d0 >= dmsize[d1]; d1++)
		d0 -= dmsize[d1];
	xtime.tm_mday= d0+1;
	xtime.tm_mon = d1;
	return(&xtime);
}

char *
asctime(t)
register struct tm *t;
{
/* Keywords: time-processing */

	seprintf(cbuf,"%s %s %d %2d:%2d",
		&"Sun\0Mon\0Tue\0Wed\0Thu\0Fri\0Sat"[4*t->tm_wday],
		&"Jan\0Feb\0Mar\0Apr\0May\0Jun\0Jul\0Aug\0Sep\0Oct\0Nov\0Dec"[(t->tm_mon)*4],
		t->tm_mday,
		t->tm_hour,
		t->tm_min);
	return(cbuf);
}
#endif
