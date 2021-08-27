#include "emacs_gb.h"
#include "emacs_io.h"
#include <time.h>


/* EMACS_MODES: c !fill */

#ifdef ux3
#include <termio.h>
#else
#include <sgtty.h>
#endif
#ifdef NDLAY
#include <sys/types.h>
#include <fcntl.h>

int ndfrn;				/* file for ndelay I/O */
#endif

#define READM 0
#define APPEM 1
#define WRITEM 0644
 
/* TTY buffer */

/* if unix3.0 (or later) system with no wait raw-mode I/O, use a big tty */
/* buffer to allow type ahead and avoid excessive re-display */

#ifdef ux3
#define TTLOOK 16
#else
#define TTLOOK 1
#endif
int ttcnt;
char ttbuf[TTLOOK];
char *ttptr;
extern int ttywarp;

/* controlification stuff */

int ctlify;
#define CONCHAR ''
extern char ctype[];
ioinit()
{

	xclose(3);			/* close all files */
	
	_stdout._frn = 1;
	_stdout._cnt = 0;
	_stdout._flags = _OUTPUT;
	incnt = inlev = infrn = 0;
	infrn = NULL;
	inbuf = _inbuf[0];
#ifdef NDLAY

/* IF we have NDELAY I/O, close standard error and open it with ndelay */

	close(2);			/* close standard error */
	

	if ((ttywarp < 2) || (open("/dev/tty",O_RDWR+O_NDELAY)<0)) {
			dup(1);
			ndfrn = 0;
	} else ndfrn = 2;
	
					/* if we can't get tty, restore frn2  */
#endif	
}

/* into raw mode */


/* Terminal I/O modes, sgttyb for before unix 3.0, termio for later */


#ifdef ux3
struct termio ttyjunk;
#else
struct sgttyb ttyjunk;
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



#ifdef ux3
	struct termio nttyjunk;

	ioctl(1, TCGETA, &ttyjunk);
	nttyjunk=ttyjunk;
	nttyjunk.c_iflag = IGNBRK;	/* ignore break, no crnl
					   mapping, no ^S^Q */ 
	nttyjunk.c_oflag = 0;		/* no delays, no crlf mapping */
	nttyjunk.c_lflag &= ~(ISIG|ECHO|ICANON); /* no echo, signals,
					    or erase/kill processing */
	nttyjunk.c_cc[VMIN] =  1;	/* return after every character read */
	nttyjunk.c_cc[VTIME] = 1;
	ttywarp = charms[ttyjunk.c_cflag&CBAUD]; /* milliseconds for character */

	ioctl(1, TCSETAW, &nttyjunk);
	ioctl(1, TCXONC,1);		/* Force tty back on */
	
#else

	struct sgttyb nttyjunk;
	
	gtty (1,&ttyjunk);
#ifdef	ux6
	nttyjunk.sg_ispeed = ttyjunk.sg_ispeed;
	nttyjunk.sg_ospeed = ttyjunk.sg_ospeed;
	nttyjunk.sg_erase = ttyjunk.sg_erase;
	nttyjunk.sg_kill = ttyjunk.sg_kill;
	nttyjunk.sg_flags = ttyjunk.sg_flags;
#else
	nttyjunk=ttyjunk;
#endif
	nttyjunk.sg_flags &= (~ECHO);	/* it was so SIMPLE in the old days */
	nttyjunk.sg_flags |= (RAW);
	ttywarp = charms[ttyjunk.sg_ospeed]; /* milliseconds for char */
	stty(1,&nttyjunk);
#endif

}

/* out of raw mode */

cook()
{
#ifdef ux3
	ioctl(1,TCSETAW, &ttyjunk);
#else
	stty(1,&ttyjunk);
#endif
}


/*VARARGS2*/

char *
nscan(stptr,ret)

register char *stptr;
register int *ret;
{
	register c;

	*ret = 0;
	while (((c = *stptr)>='0') && (c <= '9')) {
		stptr++;
		*ret = *ret*10+(c-'0');
	}
	return(stptr);
}


seprintf(string,fmt, x1)
register char *string;
register char *fmt;
unsigned x1;
{
	int c;
	int width;
	register unsigned int *adx;
	extern char *strcpy();
	
	adx = &x1;
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
				if (n > 32768L) n = n-65536L; /* sign correction */
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
			while (i > 0) *string++ =(dstack[--i] + '0');	/* print number */
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
		string = strcpy(string,(char *)*adx);
		break;
	case '+':
		*string++ = *adx + *fmt++; /* character + offset */
		break;
	case 'c':
		c = *adx;
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
char * inget()
{
	if (infrn < 0)return(inptr);
	else return(NULL);
}
inset(newptr)
char *newptr;
{
	if (infrn < 0) inptr = newptr;
}


int
getchar()
{
	extern int timemd;
	extern int newmail;
	extern int mailcnt;
	register int c;
	
	if (incnt == 0) {
		while (1) {
			if (timemd) dtime(0);
			if ((mailcnt>=0) && (--mailcnt<=0)) {
				ckmail();
			}
			mflush(stdout);
			errno = 0;
			if (infrn) {
				inptr = inbuf;
				incnt = read(infrn,inbuf,INLOOK);
			} else {

/* The following code expects read from a raw mode tty port to
 * return whenever at least one character has been received.  It
 * will not work if read can return 0 characters, or if read will
 * not return until an entire buffer load has been collected */

				ttptr = ttbuf;
				ttcnt = incnt = read(0,ttbuf,TTLOOK);
			}
			ninch+= incnt;		/* count for stats */
			if (incnt >= 0) break;
			if (errno != 4) break;
		}

	}

	if (infrn < 0) {		/* if in macro */
		incnt++;
	}
	if (infrn) {
		if (incnt-- >0)  return(*inptr++ & 0377);
	} else {
		if ((incnt= --ttcnt) >= 0)  {
			c = (*ttptr++ & 0177);
			if (ctlify && (c == CONCHAR)) {
				ctlify = 0; /* don't controlify again */
				
				c = getchar();
				if ((c>= 'a') && (c <= 'z')) c -= 040;
				if (ctype[c] == 0) c ^= 0100;
				ctlify++;
			}
			return(c);
		}
	}
	return('');
}

/* Fill tty buffer if we have ndelay I/O */

ttfill()
{
#ifdef NDLAY
	register int i;
	register char *addr;
	
	if (ndfrn == 0) return;
	mflush(stdout);			/* flush tty */
	if (ttcnt) {
					/* more input, append to it */
		addr = ttptr+ttcnt;
		i = read(2,addr, TTLOOK-(addr-ttbuf));
	} else {
		i = read(2,ttbuf,TTLOOK);
		ttptr = ttbuf;
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

register char *fp;
{
	register frn;
	
	if (fp != NULL) {
		while((frn = open(fp,READM)) <= 0) {
			if ((errno != 4) && (errno != 23))break;
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

ungetch(c)
{

	

	++incnt;
	if (infrn==0) {
		*(--ttptr) = c;
		++ttcnt;
	} else {
		*(--inptr) = c;
	}	
}

mflush(p)

register FILE *p;

{
	register int bc;
	register int bo;

	if (p->_flags & _OUTPUT) {
		if (p == stdout) {
			ntwrite++;		/* count for stats */
			noutc += p->_cnt;
		}
		bo = 0;
		while((bc = write(p->_frn, p->_buf+bo, p->_cnt)) != p->_cnt) {
			if (bc >0) {
				p->_cnt -= bc;
				bo += bc;
			}
			if (errno != 4) {
				if (p == stdout) break; /* PUNT
					 * errors on standard output */
				if (error (NORM,errno,"writing")) break;
			}
		}
		p->_cnt = 0;
	}
}

filbuf(p)

FILE *p;
{
	
	while ((p->_cnt = read(p->_frn, p->_buf, BUFSIZ)) < 0) {
		if (errno != 4) break;
	}
	p->_ptr = &(p->_buf[1]);
	if (p->_cnt > 0) {
		p->_cnt--;
		return(p->_buf[0]&0377);
	}
	else p->_cnt = 0;
	return(EOF);
}

/*VARARGS1*/

eprintf(string,a1,a2,a3,a4,a5,a6,a7)

char *string;

{
	char pbuf[1024];
	seprintf(pbuf,string,a1,a2,a3,a4,a5,a6,a7);
	puts(pbuf,stdout);
}

puts(cp,p)

FILE *p;
register char *cp;

{
	while (*cp) {
		putc(*cp++,p);
	}
}

mclose(p)

FILE *p;

{
	mflush(p);
	close(p->_frn);
}

FILE *

fopen(p,np,mode)

FILE *p;
char *np;
char *mode;

{

	switch(*mode) {
		
	case 'r':			/* read file mode */
	
		while ((p->_frn = open(np,READM)) <0) {
			if ((errno != 4) && (errno != 23))return(NULL);
		}
		p->_flags = _INPUT;
		break;
		
	case 'a':
		 while((p->_frn = open(np,APPEM)) <0) {
			if ((errno != 4) && (errno != 23))return(NULL);
		}
		lseek (p->_frn,0L,2); /* at end */
		p->_flags = _OUTPUT;
		break;
	case 'w':
		while((p->_frn = creat(np,WRITEM)) <=0) {
			if ((errno != 4)&& (errno != 23)) return(NULL);
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

{
	register result;
	register base;
	
	base = ((*string == '0') ? 8 : 10);
	result = 0;
	while ((*string >= '0') && (*string <= '9')) {
		result = (result * base) + (*string++ - '0');
	}
	if (*string == 0) return(result);
	else return(-1);
}

xclose(lowfile)

register lowfile;

{
	register fileno;
	
	for (fileno = lowfile; fileno < 20; fileno++) {
		close(fileno);
	}
}

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

long timezone = 0;


#define dysize(xyear) ((xyear&03) ? 365 : 366)


struct tm	*gmtime();
struct tm	*localtime();
char	*ctime();
char	*asctime();

char *
ctime(t)
long *t;
{
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
	char *getenv();

	if ((p = getenv ("TZ")) && *p) {

		p += 3;
		if (sign = *p == '-')
			p++;
		n = 0;
		while (*p >= '0' && *p <= '9')
			n = (n * 10) + *p++ - '0';
		if (sign)
			n = -n;
		timezone = ((long) (n * 60)) * 60;
	}
}

struct tm *
localtime(tim)
long *tim;
{
	register int dayno;
	register struct tm *ct;
	register daylbegin, daylend;
	long copyt;

	if (timezone == 0) tzset();

	copyt = *tim - timezone;
	ct = gmtime(&copyt);
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
	if ((d >= 58) && ((t->tm_year & 03) == 0)) d+=1; /* leap year */
	return(d - (d - t->tm_yday + t->tm_wday + 700) % 7);
}

struct tm *
gmtime(tim)
long *tim;
{
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


	seprintf(cbuf,"%s %s %d %2d:%2d",
		&"Sun\0Mon\0Tue\0Wed\0Thu\0Fri\0Sat"[4*t->tm_wday],
		&"Jan\0Feb\0Mar\0Apr\0May\0Jun\0Jul\0Aug\0Sep\0Oct\0Nov\0Dec"[(t->tm_mon)*4],
		t->tm_mday,
		t->tm_hour,
		t->tm_min);
	return(cbuf);
}
