/* This file contains standard definitions of useful C constants
 * and macros.		J. Leth, IH 6E-318
 */


typedef int BOOLEAN;

#define		HIGHB		0xff00
#define		LOWB		0x00ff

#define		TRUE		1
#define		FALSE		0

#define		NUL		'\0'

#define		makeint(hi,lo)	((hi << 8) | (lo & LOWB))
	/* Makes an int out of two bytes */

#define usage(x)	fprintf(stderr, x); exit(1)
#define errexit(x,arg)	fprintf(stderr, x, arg); perror(""); exit(1)
#define errmsg(x,arg)	fprintf(stderr, x, arg)

#define IDENT(x)x
#define CONCAT(x,y)IDENT(x)y
	/* Concatenates two symbols into one, (compiler or C
	 * preprocessor symbols).
	 */
#define QUOTE(x)"x"

#ifndef NDEBUG
#	define assert(ex) if (!(ex)) {\
	fprintf(stderr,"Assertion '%s' failed: file %s, line %d\n",\
		QUOTE(ex), __FILE__, __LINE__);\
	exit(1);\
	}
#else
#	define assert(ex)
#endif

#define ESC '\033'

#ifdef TRACEFLAG
#define TRACE(x,y) fprintf(stderr, "TRACE: "); fprintf(stderr, x, y)
#define TRACE2(x,y,z) fprintf(stderr, "TRACE: "); fprintf(stderr, x, y, z)
#else
#define TRACE(x,y)
#define TRACE2(x,y,z)
#endif
