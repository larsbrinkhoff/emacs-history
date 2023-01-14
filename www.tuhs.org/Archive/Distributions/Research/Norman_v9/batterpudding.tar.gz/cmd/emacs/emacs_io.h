
/* EMACS_MODES: c !fill */

#ifdef OWNER
#define EXTERN
#else
#define EXTERN extern
#endif

#ifdef pdp11
#define BUFSIZ  512
#else
#define BUFSIZ 1024
#endif
extern  struct  iobuf {
        int     _cnt;
        char    *_ptr;
        int     _frn;
        int     _flags;
        char    _buf[BUFSIZ];
};

/* values for _flags */

#define _OUTPUT 01
#define _INPUT 02
#define _DEAD 04
#define _ERROR 010

#ifdef v8
#undef NULL
#endif
#define NULL    0
#define FILE    struct iobuf
#define EOF     (-1)

EXTERN int donttime;
EXTERN struct iobuf _stdout;
EXTERN int no_io;			/* Flag to force off I/O */

EXTERN int outproc;			/* Process to send output to */
EXTERN int inproc;			/* Process to receive input from */
EXTERN int procbuf;			/* Buffer coupled to this. */
EXTERN int procpid;

/* input buffer */

#define INLOOK 1


EXTERN char *inbuf;
EXTERN char *inptr;
EXTERN int incnt;
EXTERN int infrn;
EXTERN int inlev;

/* INPUT BUFFER STACK */

#define NINP 32                         /* levels of calls */
EXTERN char _inbuf[NINP] [INLOOK];
EXTERN int _incnt[NINP];
EXTERN char *_inptr[NINP];
EXTERN int _infrn[NINP];

#define stdout  (&_stdout)
#define putc(x,p)       p->_buf[p->_cnt++] = (unsigned)(x);if (p->_cnt>=BUFSIZ) mflush(p)
#define putchar(x)      putc(x,stdout)
#define getc(p)         ((--((p)->_cnt)>=0)? *((p)->_ptr++)&0377:filbuf(p))
#define MOREIN  (incnt)
#define Mgetchar(x) ((*inptr++) &0377)
/* function definitions */

EXTERN FILE *xopen();
EXTERN FILE *fdopen();
EXTERN int filbuf();
