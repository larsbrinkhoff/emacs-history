/* m- file for SEQUENT BALANCE machines
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


/* NOTE: this file works for DYNIX release 2.0 
	  (not tested on 1.3) on NS32000's */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

/* #define BIG_ENDIAN */

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
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

/* BTW: DYNIX defines sequent, ns32000, and ns16000 (GENIX compatibility) */
#ifndef	sequent		/* pre DYNIX 2.1 releases */
# define sequent
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* crt0.c should use the vax-bsd style of entry, with these dummy args.  */

#define CRT0_DUMMIES bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE unsigned long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define	FSCALE	1000.0
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

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

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* #define C_ALLOCA */
#define HAVE_ALLOCA

/* Name of file the to look in
   for the kernel symbol table (for load average) */

#undef KERNEL_FILE
#define KERNEL_FILE "/dynix"

/* Avoids a compiler bug */

#define TAHOE_REGISTER_BUG

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  Furthermore, the value written
   in the a_text in the file must have N_ADDRADJ added to it.  */

#define A_TEXT_OFFSET(HDR) (sizeof (HDR) + N_ADDRADJ (HDR))

/* (short) negative-int doesn't sign-extend correctly */
#define SHORT_CAST_BUG

/* Cause compilations to be done in parallel in ymakefile.  */
#define MAKE_PARALLEL &

/* Define how to search all pty names.
   This is for Dynix 3.0; delete next 5 definitions for older systems.  */

#define PTY_MAJOR "pqrstuvwPQRSTUVW"
#define PTY_MINOR "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define PTY_ITERATION					\
  register int ma, mi;					\
  for (ma = 0; ma < sizeof(PTY_MAJOR) - 1; ma++)	\
    for (mi = 0; mi < sizeof(PTY_MINOR) - 1; mi++)
#define PTY_NAME_SPRINTF \
  sprintf (ptyname, "/dev/pty%c%c", PTY_MAJOR[ma], PTY_MINOR[mi]);
#define PTY_TTY_NAME_SPRINTF \
  sprintf (ptyname, "/dev/tty%c%c", PTY_MAJOR[ma], PTY_MINOR[mi]);
