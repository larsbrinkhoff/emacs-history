
/* EMACS_MODES: c !fill */

#define	BUFSIZ	512
extern	struct	iobuf {
	int	_cnt;
	char	*_ptr;
	int	_frn;
	int	_flags;
	char	_buf[BUFSIZ];
};

/* values for _flags */

#define _OUTPUT 01
#define _INPUT 02
#define _DEAD 04



#define	NULL	0
#define	FILE	struct iobuf
#define	EOF	(-1)


struct iobuf _stdout;

/* input buffer */

#define INLOOK 1


char *inbuf;
char *inptr;
int incnt;
int infrn;
int inlev;

/* INPUT BUFFER STACK */

#define NINP 32				/* levels of calls */
char _inbuf[NINP] [INLOOK];
char _incnt[NINP];
char *_inptr[NINP];
char _infrn[NINP];

#define	stdout	(&_stdout)
#define	putc(x,p)	p->_buf[p->_cnt++] = (x);if (p->_cnt>=BUFSIZ) mflush(p)
#define	putchar(x)	putc(x,stdout)
#define	getc(p)		((--((p)->_cnt)>=0)? *((p)->_ptr++)&0377:filbuf(p))
#define MOREIN	(incnt)

/* function definitions */

FILE *fopen();
FILE *fdopen();
int filbuf();
