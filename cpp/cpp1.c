/*
 * CPP main program.
 *
 * Edit history
 * 21-May-84	MM	"Field test" release
 * 23-May-84	MM	Some minor hacks.
 * 30-May-84	ARF	Didn't get enough memory for __DATE__
 *			Added code to read stdin if no input
 *			files are provided.
 * 29-Jun-84	MM	Added ARF's suggestions, Unixifying cpp.
 * 11-Jul-84	MM	"Official" first release (that's what I thought!)
 * 22-Jul-84	MM/ARF/SCK Fixed line number bugs, added cpp recognition
 *			of #line, fixed problems with #include.
 * 23-Jul-84	MM	More (minor) include hacking, some documentation.
 *			Also, redid cpp's #include files
 * 25-Jul-84	MM	#line filename isn't used for #include searchlist
 *			#line format is <number> <optional name>
 * 25-Jul-84	ARF/MM	Various bugs, mostly serious.  Removed homemade doprint
 * 01-Aug-84	MM	Fixed recursion bug, remove extra newlines and
 *			leading whitespace from cpp output.
 * 02-Aug-84	MM	Hacked (i.e. optimized) out blank lines and unneeded
 *			whitespace in general.  Cleaned up unget()'s.
 * 03-Aug-84	Keie	Several bug fixes from Ed Keizer, Vrije Universitet.
 *			-- corrected arg. count in -D and pre-defined
 *			macros.  Also, allow \n inside macro actual parameter
 *			lists.
 * 06-Aug-84	MM	If debugging, dump the preset vector at startup.
 * 12-Aug-84	MM/SCK	Some small changes from Sam Kendall
 * 15-Aug-84	Keie/MM	cerror, cwarn, etc. take a single string arg.
 *			cierror, etc. take a single int. arg.
 *			changed LINE_PREFIX slightly so it can be
 *			changed in the makefile.
 * 31-Aug-84	MM	USENET net.sources release.
 *  7-Sep-84	SCH/ado Lint complaints
 * 10-Sep-84	Keie	Char's can't be signed in some implementations
 * 11-Sep-84	ado	Added -C flag, pathological line number fix
 * 13-Sep-84	ado	Added -E flag (does nothing) and "-" file for stdin.
 * 14-Sep-84	MM	Allow # 123 as a synonym for #line 123
 * 19-Sep-84	MM	scanid always reads to token, make sure #line is
 *			written to a new line, even if -C switch given.
 *			Also, cpp - - reads stdin, writes stdout.
 * 03-Oct-84	ado/MM	Several changes to line counting and keepcomments
 *			stuff.  Also a rewritten control() hasher -- much
 *			simpler and no less "perfect". Note also changes
 *			in cpp3.c to fix numeric scanning.
 * 04-Oct-84	MM	Added recognition of macro formal parameters if
 *			they are the only thing in a string, per the
 *			draft standard.
 * 08-Oct-84	MM	One more attack on scannumber
 * 15-Oct-84	MM/ado	Added -N to disable predefined symbols.  Fixed
 *			linecount if COMMENT_INVISIBLE enabled.
 * 22-Oct-84	MM	Don't evaluate the #if/#ifdef argument if
 *			compilation is supressed.  This prevents
 *			unnecessary error messages in sequences such as
 *			    #ifdef FOO		-- undefined
 *			    #if FOO == 10	-- shouldn't print warning
 * 25-Oct-84	MM	Fixed bug in false ifdef supression.  On vms,
 *			#include <foo> should open foo.h -- this duplicates
 *			the behavior of Vax-C
 * 31-Oct-84	ado/MM	Parametized $ in indentifiers.  Added a better
 *			token concatenator and took out the trial
 *			concatenation code.  Also improved #ifdef code
 *			and cleaned up the macro recursion tester.
 *  2-Nov-84	MM/ado	Some bug fixes in token concatenation, also
 *			a variety of minor (uninteresting) hacks.
 *  6-Nov-84	MM	Happy Birthday.  Broke into 4 files and added
 *			#if sizeof (basic_types)
 *  9-Nov-84	MM	Added -S* for pointer type sizes
 * 13-Nov-84	MM	Split cpp1.c, added vms defaulting
 * 23-Nov-84	MM/ado	-E supresses error exit, added CPP_INCLUDE,
 *			fixed strncpy bug.
 *  3-Dec-84	ado/MM	Added OLD_PREPROCESSOR
 *  7-Dec-84	MM	Stuff in Nov 12 Draft Standard
 * 17-Dec-84	george	Fixed problems with recursive macros
 * 17-Dec-84	MM	Yet another attack on #if's (f/t)level removed.
 * 07-Jan-85	ado	Init defines before doing command line options
 *			so -Uunix works.
 * 21-Oct-85	RMS	Rename `token' to `tokenbuf'.
 *			Allocate it dynamically, with size in `tokenbsize'.
 * 23-Oct-85	RMS	Do not print message about number of errors.
 */

/*)BUILD
	$(PROGRAM)	= cpp
	$(FILES)	= { cpp1 cpp2 cpp3 cpp4 cpp5 cpp6 }
	$(INCLUDE)	= { cppdef.h cpp.h }
	$(STACK)	= 2000
	$(TKBOPTIONS)	= {
		STACK	= 2000
	}
*/

#ifdef	DOCUMENTATION

title	cpp		C Pre-Processor
index			C pre-processor

synopsis
	.s.nf
	cpp [-options] [infile [outfile]]
	.s.f
description

	CPP reads a C source file, expands macros and include
	files, and writes an input file for the C compiler.
	If no file arguments are given, CPP reads from stdin
	and writes to stdout.  If one file argument is given,
	it will define the input file, while two file arguments
	define both input and output files.  The file name "-"
	is a synonym for stdin or stdout as appropriate.

	The following options are supported.  Options may
	be given in either case.
	.lm +16
	.p -16
	-C		If set, source-file comments are written
	to the output file.  This allows the output of CPP to be
	used as the input to a program, such as lint, that expects
	commands embedded in specially-formatted comments.
	.p -16
	-Dname=value	Define the name as if the programmer wrote

	    #define name value

	at the start of the first file.  If "=value" is not
	given, a value of "1" will be used.

	On non-unix systems, all alphabetic text will be forced
	to upper-case.
	.p -16
	-E		Always return "success" to the operating
	system, even if errors were detected.  Note that some fatal
	errors, such as a missing #include file, will terminate
	CPP, returning "failure" even if the -E option is given.
	.p -16
	-Idirectory	Add this directory to the list of
	directories searched for #include "..." and #include <...>
	commands.  Note that there is no space between the
	"-I" and the directory string.  More than one -I command
	is permitted.  On non-Unix systems "directory" is forced
	to upper-case.
	.p -16
	-N		CPP normally predefines some symbols defining
	the target computer and operating system.  If -N is specified,
	no symbols will be predefined.  If -N -N is specified, the
	"always present" symbols, __LINE__, __FILE__, and __DATE__
	are not defined.
	.p -16
	-Stext		CPP normally assumes that the size of
	the target computer's basic variable types is the same as the size
	of these types of the host computer.  (This can be overridden
	when CPP is compiled, however.)  The -S option allows dynamic
	respecification of these values.  "text" is a string of
	numbers, separated by commas, that specifies correct sizes.
	The sizes must be specified in the exact order:

	    char short int long float double

	If you specify the option as "-S*text", pointers to these
	types will be specified.  -S* takes one additional argument
	for pointer to function (e.g. int (*)())

	For example, to specify sizes appropriate for a PDP-11,
	you would write:

	       c s i l f d func
	     -S1,2,2,2,4,8,
	    -S*2,2,2,2,2,2,2

	Note that all values must be specified.
	.p -16
	-Uname		Undefine the name as if

	    #undef name

	were given.  On non-Unix systems, "name" will be forced to
	upper-case.
	.p -16
	-Xnumber	Enable debugging code.  If no value is
	given, a value of 1 will be used.  (For maintenence of
	CPP only.)
	.s.lm -16

Pre-Defined Variables

	When CPP begins processing, the following variables will
	have been defined (unless the -N option is specified):
	.s
	Target computer (as appropriate):
	.s
	    pdp11, vax, M68000 m68000 m68k
	.s
	Target operating system (as appropriate):
	.s
	    rsx, rt11, vms, unix
	.s
	Target compiler (as appropriate):
	.s
	    decus, vax11c
	.s
	The implementor may add definitions to this list.
	The default definitions match the definition of the
	host computer, operating system, and C compiler.
	.s
	The following are always available unless undefined (or
	-N was specified twice):
	.lm +16
	.p -12
	__FILE__	The input (or #include) file being compiled
	(as a quoted string).
	.p -12
	__LINE__	The line number being compiled.
	.p -12
	__DATE__	The date and time of compilation as
	a Unix ctime quoted string (the trailing newline is removed).
	Thus,
	.s
	    printf("Bug at line %s,", __LINE__);
	    printf(" source file %s", __FILE__);
	    printf(" compiled on %s", __DATE__);
	.s.lm -16

Draft Proposed Ansi Standard Considerations

	The current version of the Draft Proposed Standard
	explicitly states that "readers are requested not to specify
	or claim conformance to this draft."  Readers and users
	of Decus CPP should not assume that Decus CPP conforms
	to the standard, or that it will conform to the actual
	C Language Standard.

	When CPP is itself compiled, many features of the Draft
	Proposed Standard that are incompatible with existing
	preprocessors may be disabled.  See the comments in CPP's
	source for details.

	The latest version of the Draft Proposed Standard (as reflected
	in Decus CPP) is dated November 12, 1984.

	Comments are removed from the input text.  The comment
	is replaced by a single space character.  The -C option
	preserves comments, writing them to the output file.

	The '$' character is considered to be a letter.  This is
	a permitted extension.

	The following new features of C are processed by CPP:
	.s.comment Note: significant spaces, not tabs, .br quotes #if, #elif
	.br;####_#elif expression    (_#else _#if)
	.br;####'_\xNNN'             (Hexadecimal constant)
	.br;####'_\a'                (Ascii BELL)
	.br;####'_\v'                (Ascii Vertical Tab)
	.br;####_#if defined NAME    1 if defined, 0 if not
	.br;####_#if defined (NAME)  1 if defined, 0 if not  
	.br;####_#if sizeof (basic type)
	.br;####unary +
	.br;####123U, 123LU          Unsigned ints and longs.
	.br;####12.3L                Long double numbers
	.br;####token_#token         Token concatenation
	.br;####_#include token      Expands to filename

	The Draft Proposed Standard has extended C, adding a constant
	string concatenation operator, where

	    "foo" "bar"

	is regarded as the single string "foobar".  (This does not
	affect CPP's processing but does permit a limited form of
	macro argument substitution into strings as will be discussed.)

	The Standard Committee plans to add token concatenation
	to #define command lines.  One suggested implementation
	is as follows:  the sequence "Token1#Token2" is treated
	as if the programmer wrote "Token1Token2".  This could
	be used as follows:

	    #line 123
	    #define ATLINE foo#__LINE__

	ATLINE would be defined as foo123.

	Note that "Token2" must either have the format of an
	identifier or be a string of digits.  Thus, the string

	    #define ATLINE foo#1x3

	generates two tokens: "foo1" and "x3".

	If the tokens T1 and T2 are concatenated into T3,
	this implementation operates as follows:

	  1. Expand T1 if it is a macro.
	  2. Expand T2 if it is a macro.
	  3. Join the tokens, forming T3.
	  4. Expand T3 if it is a macro.

	A macro formal parameter will be substituted into a string
	or character constant if it is the only component of that
	constant:

	    #define VECSIZE 123
	    #define vprint(name, size) \
	      printf("name" "[" "size" "] = {\n")
	      ... vprint(vector, VECSIZE);

	expands (effectively) to

	      vprint("vector[123] = {\n");

	Note that this will be useful if your C compiler supports
	the new string concatenation operation noted above.
	As implemented here, if you write

	    #define string(arg) "arg"
	      ... string("foo") ...

	This implementation generates "foo", rather than the strictly
	correct ""foo"" (which will probably generate an error message).
	This is, strictly speaking, an error in CPP and may be removed
	from future releases.

error messages

	Many.  CPP prints warning or error messages if you try to
	use multiple-byte character constants (non-transportable)
	if you #undef a symbol that was not defined, or if your
	program has potentially nested comments.

author

	Martin Minow

bugs

	The #if expression processor uses signed integers only.
	I.e, #if 0xFFFFu < 0 may be TRUE.

#endif

#include	<stdio.h>
#include	<ctype.h>
#include	"cppdef.h"
#include	"cpp.h"

/*
 * Commonly used global variables:
 * line		is the current input line number.
 * wrongline	is set in many places when the actual output
 *		line is out of sync with the numbering, e.g,
 *		when expanding a macro with an embedded newline.
 *
 * tokenbuf	holds the last identifier scanned (which might
 *		be a candidate for macro expansion).
 * errors	is the running cpp error counter.
 * infile	is the head of a linked list of input files (extended by
 *		#include and macros being expanded).  infile always points
 *		to the current file/macro.  infile->parent to the includer,
 *		etc.  infile->fd is NULL if this input stream is a macro.
 */
int		line;			/* Current line number		*/
int		wrongline;		/* Force #line to compiler	*/
char		*tokenbuf;		/* Buffer for current input token */
int		tokenbsize;		/* Allocated size of tokenbuf, */
					/* not counting zero at end.  */
int		errors;			/* cpp error counter		*/
FILEINFO	*infile = NULL;		/* Current input file		*/
#if DEBUG
int		debug;			/* TRUE if debugging now	*/
#endif
/*
 * This counter is incremented when a macro expansion is initiated.
 * If it exceeds a built-in value, the expansion stops -- this tests
 * for a runaway condition:
 *	#define X Y
 *	#define Y X
 *	X
 * This can be disabled by falsifying rec_recover.  (Nothing does this
 * currently: it is a hook for an eventual invocation flag.)
 */
int		recursion;		/* Infinite recursion counter	*/
int		rec_recover = TRUE;	/* Unwind recursive macros	*/

/*
 * instring is set TRUE when a string is scanned.  It modifies the
 * behavior of the "get next character" routine, causing all characters
 * to be passed to the caller (except <DEF_MAGIC>).  Note especially that
 * comments and \<newline> are not removed from the source.  (This
 * prevents cpp output lines from being arbitrarily long).
 *
 * inmacro is set by #define -- it absorbs comments and converts
 * form-feed and vertical-tab to space, but returns \<newline>
 * to the caller.  Strictly speaking, this is a bug as \<newline>
 * shouldn't delimit tokens, but we'll worry about that some other
 * time -- it is more important to prevent infinitly long output lines.
 *
 * instring and inmarcor are parameters to the get() routine which
 * were made global for speed.
 */
int		instring = FALSE;	/* TRUE if scanning string	*/
int		inmacro = FALSE;	/* TRUE if #defining a macro	*/

/*
 * work[] and workp are used to store one piece of text in a temporay
 * buffer.  To initialize storage, set workp = work.  To store one
 * character, call save(c);  (This will fatally exit if there isn't
 * room.)  To terminate the string, call save(EOS).  Note that
 * the work buffer is used by several subroutines -- be sure your
 * data won't be overwritten.  The extra byte in the allocation is
 * needed for string formal replacement.
 */
char		work[NWORK + 1];	/* Work buffer			*/
char		*workp;			/* Work buffer pointer		*/

/*
 * keepcomments is set TRUE by the -C option.  If TRUE, comments
 * are written directly to the output stream.  This is needed if
 * the output from cpp is to be passed to lint (which uses commands
 * embedded in comments).  cflag contains the permanent state of the
 * -C flag.  keepcomments is always falsified when processing #control
 * commands and when compilation is supressed by a false #if
 *
 * If eflag is set, CPP returns "success" even if non-fatal errors
 * were detected.
 *
 * If nflag is non-zero, no symbols are predefined except __LINE__.
 * __FILE__, and __DATE__.  If nflag > 1, absolutely no symbols
 * are predefined.
 */
int		keepcomments = FALSE;	/* Write out comments flag	*/
int		cflag = FALSE;		/* -C option (keep comments)	*/
int		eflag = FALSE;		/* -E option (never fail)	*/
int		nflag = 0;		/* -N option (no predefines)	*/

/*
 * ifstack[] holds information about nested #if's.  It is always
 * accessed via *ifptr.  The information is as follows:
 *	WAS_COMPILING	state of compiling flag at outer level.
 *	ELSE_SEEN	set TRUE when #else seen to prevent 2nd #else.
 *	TRUE_SEEN	set TRUE when #if or #elif succeeds
 * ifstack[0] holds the compiling flag.  It is TRUE if compilation
 * is currently enabled.  Note that this must be initialized TRUE.
 */
char		ifstack[BLK_NEST] = { TRUE };	/* #if information	*/
char		*ifptr = ifstack;		/* -> current ifstack[] */

/*
 * incdir[] stores the -i directories (and the system-specific
 * #include <...> directories.
 */
char	*incdir[NINCLUDE];		/* -i directories		*/
char	**incend = incdir;		/* -> free space in incdir[]	*/

/*
 * This is the table used to predefine target machine and operating
 * system designators.  It may need hacking for specific circumstances.
 * Note: it is not clear that this is part of the Ansi Standard.
 * The -N option supresses preset definitions.
 */
char	*preset[] = {			/* names defined at cpp start	*/
#ifdef	MACHINE
	MACHINE,
#endif
#ifdef	SYSTEM
	SYSTEM,
#endif
#ifdef	COMPILER
	COMPILER,
#endif
#if	DEBUG
	"decus_cpp",			/* Ourselves!			*/
#endif
	NULL				/* Must be last			*/
};

/*
 * The value of these predefined symbols must be recomputed whenever
 * they are evaluated.  The order must not be changed.
 */
char	*magic[] = {			/* Note: order is important	*/
	"__LINE__",
	"__FILE__",
	NULL				/* Must be last			*/
};

main(argc, argv)
int		argc;
char		*argv[];
{
	register int	i;

#if HOST == SYS_VMS
	argc = getredirection(argc, argv);	/* vms >file and <file	*/
#endif
	initdefines();				/* O.S. specific def's	*/
	i = dooptions(argc, argv);		/* Command line -flags	*/
	switch (i) {
	case 3:
	    /*
	     * Get output file, "-" means use stdout.
	     */
	    if (!streq(argv[2], "-")) {
#if HOST == SYS_VMS
		/*
		 * On vms, reopen stdout with "vanilla rms" attributes.
		 */
		if ((i = creat(argv[2], 0, "rat=cr", "rfm=var")) == -1
		 || dup2(i, fileno(stdout)) == -1) {
#else
		if (freopen(argv[2], "w", stdout) == NULL) {
#endif
		    perror(argv[2]);
		    cerror("Can't open output file \"%s\"", argv[2]);
		    exit(IO_ERROR);
		}
	    }				/* Continue by opening input	*/
	case 2:				/* One file -> stdin		*/
	    /*
	     * Open input file, "-" means use stdin.
	     */
	    if (!streq(argv[1], "-")) {
		if (freopen(argv[1], "r", stdin) == NULL) {
		    perror(argv[1]);
		    cerror("Can't open input file \"%s\"", argv[1]);
		    exit(IO_ERROR);
		}
		strcpy(work, argv[1]);	/* Remember input filename	*/
		break;
	    }				/* Else, just get stdin		*/
	case 0:				/* No args?			*/
	case 1:				/* No files, stdin -> stdout	*/
#if HOST == SYS_UNIX
	    work[0] = EOS;		/* Unix can't find stdin name	*/
#else
	    fgetname(stdin, work);	/* Vax-11C, Decus C know name	*/
#endif
	    break;

	default:
	    exit(IO_ERROR);		/* Can't happen			*/
	}
	setincdirs();			/* Setup -I include directories	*/
	addfile(stdin, work);		/* "open" main input file	*/
#if DEBUG
	if (debug > 0)
	    dumpdef("preset #define symbols");
#endif
	cppmain();			/* Process main file		*/
	if ((i = (ifptr - &ifstack[0])) != 0) {
#if OLD_PREPROCESSOR
	    ciwarn("Inside #ifdef block at end of input, depth = %d", i);
#else
	    cierror("Inside #ifdef block at end of input, depth = %d", i);
#endif
	}
	fclose(stdout);
	if (errors > 0 && !eflag)
	  exit(IO_ERROR);
	exit(IO_NORMAL);		/* No errors or -E option set	*/
}

FILE_LOCAL
cppmain()
/*
 * Main process for cpp -- copies tokens from the current input
 * stream (main file, include file, or a macro) to the output
 * file.
 */
{
	register int		c;		/* Current character	*/
	register int		counter;	/* newlines and spaces	*/
	extern int		output();	/* Output one character	*/

	/* Initialize for reading tokens */
	tokenbsize = 50;
	tokenbuf = getmem (tokenbsize + 1);

	/*
	 * Explicitly output a #line at the start of cpp output so
	 * that lint (etc.) knows the name of the original source
	 * file.  If we don't do this explicitly, we may get
	 * the name of the first #include file instead.
	 */
	sharp();
	/*
	 * This loop is started "from the top" at the beginning of each line
	 * wrongline is set TRUE in many places if it is necessary to write
	 * a #line record.  (But we don't write them when expanding macros.)
	 *
	 * The counter variable has two different uses:  at
	 * the start of a line, it counts the number of blank lines that
	 * have been skipped over.  These are then either output via
	 * #line records or by outputting explicit blank lines.
 	 * When expanding tokens within a line, the counter remembers
	 * whether a blank/tab has been output.  These are dropped
	 * at the end of the line, and replaced by a single blank
	 * within lines.
	 */
	for (;;) {
	    counter = 0;			/* Count empty lines	*/
	    for (;;) {				/* For each line, ...	*/
		while (type[(c = get())] == SPA) /* Skip leading blanks	*/
		    ;				/* in this line.	*/
		if (c == '\n')			/* If line's all blank,	*/
		    ++counter;			/* Do nothing now	*/
		else if (c == '#') {		/* Is 1st non-space '#'	*/
		    keepcomments = FALSE;	/* Don't pass comments	*/
		    counter = control(counter);	/* Yes, do a #command	*/
		    keepcomments = (cflag && compiling);
		}
		else if (c == EOF_CHAR)		/* At end of file?	*/
		    break;
		else if (!compiling) {		/* #ifdef false?	*/
		    skipnl();			/* Skip to newline	*/
		    counter++;			/* Count it, too.	*/
		}
		else {
		    break;			/* Actual token		*/
		}
	    }
	    if (c == EOF_CHAR)			/* Exit process at	*/
		break;				/* End of file		*/
	    /*
	     * If the loop didn't terminate because of end of file, we
	     * know there is a token to compile.  First, clean up after
	     * absorbing newlines.  counter has the number we skipped.
	     */
	    if ((wrongline && infile->fp != NULL) || counter > 4)
		sharp();			/* Output # line number	*/
	    else {				/* If just a few, stuff	*/
		while (--counter >= 0)		/* them out ourselves	*/
		    putchar('\n');
	    }
	    /*
	     * Process each token on this line.
	     */
	    unget();				/* Reread the char.	*/
	    for (;;) {				/* For the whole line,	*/
		do {				/* Token concat. loop	*/
		    for (counter = 0; (type[(c = get())] == SPA);) {
#if COMMENT_INVISIBLE
			if (c != COM_SEP)
			    counter++;
#else
			counter++;		/* Skip over blanks	*/
#endif
		    }
		    if (c == EOF_CHAR || c == '\n')
			goto end_line;		/* Exit line loop	*/
		    else if (counter > 0)	/* If we got any spaces	*/
			putchar(' ');		/* Output one space	*/
		    c = macroid(c);		/* Grab the token	*/
		} while (type[c] == LET && catenate());
		if (c == EOF_CHAR || c == '\n')	/* From macro exp error	*/
		    goto end_line;		/* Exit line loop	*/
		switch (type[c]) {
		case LET:
		    fputs(tokenbuf, stdout);	/* Quite ordinary token	*/
		    break;


		case DIG:			/* Output a number	*/
		case DOT:			/* Dot may begin floats	*/
		    scannumber(c, output);
		    break;

		case QUO:			/* char or string const	*/
		    scanstring(c, output);	/* Copy it to output	*/
		    break;

		default:			/* Some other character	*/
		    cput(c);			/* Just output it	*/
		    break;
		}				/* Switch ends		*/
	    }					/* Line for loop	*/
end_line:   if (c == '\n') {			/* Compiling at EOL?	*/
		putchar('\n');			/* Output newline, if	*/
		if (infile->fp == NULL)		/* Expanding a macro,	*/
		    wrongline = TRUE;		/* Output # line later	*/
	    }
	}					/* Continue until EOF	*/
}

output(c)
int		c;
/*
 * Output one character to stdout -- output() is passed as an
 * argument to scanstring()
 */
{
#if COMMENT_INVISIBLE
	if (c != TOK_SEP && c != COM_SEP)
#else
	if (c != TOK_SEP)
#endif
	    putchar(c);
}

static char	*sharpfilename = NULL;

FILE_LOCAL
sharp()
/*
 * Output a line number line.
 */
{
	register char		*name;

	if (keepcomments)			/* Make sure # comes on	*/
	    putchar('\n');			/* a fresh, new line.	*/
	printf("#%s %d", LINE_PREFIX, line);
	if (infile->fp != NULL) {
	    name = (infile->progname != NULL)
		? infile->progname : infile->filename;
	    if (sharpfilename == NULL
	     || sharpfilename != NULL && !streq(name, sharpfilename)) {
		if (sharpfilename != NULL)
		    free(sharpfilename);
		sharpfilename = savestring(name);
		printf(" \"%s\"", name);
	     }
	}
	putchar('\n');
	wrongline = FALSE;
}
