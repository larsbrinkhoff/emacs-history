/* m- file for Iris-4D machines.  Use with s-iris3-6.h
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) ((signed char)(c))

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef mips
#define mips
#endif

#ifndef IRIS_4D
#define IRIS_4D
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long	/* This doesn't quite work on the 4D */

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / 256.0)

/* s-iris3-6.h uses /vmunix */

#undef KERNEL_FILE
#define KERNEL_FILE "/unix"

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
/* #define HAVE_ALLOCA */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#define UNEXEC unexmips.o

/*
 * The TEXT_START here is by experiment; the DATA_START is as per the
 * manual (Assembly Language Programmers guide).  These values can be
 * found at run time from &_ftext and &_fdata, though the C file which
 * reads these must be compile with -G 0 (prevent access to those two
 * 'variables' using the $gp register).  However, I am lazy and don't
 * really want to rewrite unexec.c yet again to use this instead of
 * TEXT_START and DATA_START.  The manual indicates that these
 * locations are fixed for ZMAGIC files, so I'll be lazy and do it
 * this way.  It will probably break as of the next release of the
 * IRIS4D, but I've described up above how to find the new values, so
 * I won't feel too guilty.  Note that the value returned through
 * _ftext is the start of text space (in this case it was 0x400140); I
 * presumed that the headers started at 0x400000 and was right.
 *
 * DATA_SEG_BITS forces that bit to be or'd in with any pointers which
 * are trying to access pure strings (as gnu-emacs only allows 24 bits
 * for the value field of a LISP_OBJECT).
 */


#define TEXT_START 0x400000
#define DATA_START 0x10000000
#define DATA_SEG_BITS	0x10000000

#undef LIBS_MACHINE
#define LIBS_MACHINE -lPW -lmld
#define LIBS_DEBUG

/* Define this if you have a fairly recent system,
   in which crt1.o and crt1.n should be used.  */
#define HAVE_CRTN

#ifdef HAVE_CRTN
/* Must define START-FILES so that the linker can find /usr/lib/crt0.o.  */
#define START_FILES pre-crt0.o /usr/lib/crt1.o
#define LIB_STANDARD -lbsd -lc /usr/lib/crtn.o
#else
#define START_FILES pre-crt0.o /usr/lib/crt0.o
/* The entry-point label (start of text segment) is `start', not `__start'.  */
#define DEFAULT_ENTRY_ADDRESS start
#define LIB_STANDARD -lbsd -lc
#endif

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'

/* Define STACK_DIRECTION for alloca.c */

#define STACK_DIRECTION -1

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << INTBITS-VALBITS) >> INTBITS-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << INTBITS-VALBITS) >> INTBITS-VALBITS))

#define XSETINT(a, b)  XSET(a, XTYPE(a), b)
#define XSETUINT(a, b) XSET(a, XTYPE(a), b)
#define XSETPNTR(a, b) XSET(a, XTYPE(a), b)

#define XMARKBIT(a) ((a) < 0)
#define XSETMARKBIT(a,b) ((a) = ((a) & ~MARKBIT) | ((b) ? MARKBIT : 0))
#define XUNMARK(a) ((a) = (((unsigned)(a) << INTBITS-GCTYPEBITS-VALBITS) >> INTBITS-GCTYPEBITS-VALBITS))
