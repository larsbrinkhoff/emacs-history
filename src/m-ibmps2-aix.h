/* m- file for ibm ps/2 aix386.
   Copyright (C) 1989 Free Software Foundation, Inc.

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

/* i386 is not big-endian: lowest numbered byte is least significant. */

/* #undef BIG_ENDIAN */

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#define INTEL386
#define aix386
#undef  SYSTEM_TYPE
#define SYSTEM_TYPE "ibm-aix-386"

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

#define CRT0_DUMMIES bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* USG systems do not actually support the load average,
so disable it for them.  */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP
#undef static
  /* Since NO_REMAP, problem with statics doesn't exist */

#define TEXT_START 0x00400000
#define TEXT_END 0
#define DATA_START 0x00800000
#define DATA_END 0

/* The data segment in this machine always starts at address 0x00800000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.  */

#define DATA_SEG_BITS 0x00800000

#if 0 /* I refuse to promulgate a recommendation that would make
         users unable to debug - RMS.  */
/* delete the following line to foil optimization, enable debugging */
#define C_DEBUG_SWITCH -O
#endif

#define BSTRING
#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_SELECT
#define HAVE_TIMEVAL
#define HAVE_VFORK

/*
 * 	Define SYSV_SYSTEM_DIR to use the V.3 getdents/readir
 *	library functions.  Almost, but not quite the same as
 *	the 4.2 functions
 */
#define SYSV_SYSTEM_DIR
#define HAVE_CLOSEDIR  /* This system, unlike ordinary SYSV, has closedir.  */

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */
#undef  NONSYSTEM_DIR_LIBRARY

/* But don't use utimes() -- it causes SIGSEGV!  Use utime() instead. */
#define USE_UTIME

/* AIX defines FIONREAD, but it does not work.  */
#define BROKEN_FIONREAD

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#ifdef __GNUC__
#define HAVE_ALLOCA
#define alloca(n) __builtin_alloca(n)
#define LIBS_MACHINE /usr/local/lib/gcc-gnulib -lbsd -lrts
#else
#define C_ALLOCA
#define STACK_DIRECTION -1 /* tell alloca.c which way it grows */
#define LIBS_MACHINE -lbsd -lrts
#endif

#define OBJECTS_MACHINE hftctl.o
#define LD_SWITCH_MACHINE -T0x00400000 -K -e start
