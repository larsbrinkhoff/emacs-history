/* m- file for AT&T UNIX PC model 7300
   Copyright (C) 1986 Free Software Foundation, Inc.
   Modified for this machine by mtxinu!rtech!gonzo!daveb

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


/* This machine does not have flexnames.  Yuk */

/* # define SHORTNAMES */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

# ifndef mc68k
# define mc68k
# endif
#ifndef m68k
#define m68k
#endif

/* Cause crt0.c to define errno.  */

#define NEED_ERRNO

/* Data type of load average, as read out of kmem.  */
/* These are commented out since it is not supported by this machine.  */
  
/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0) */

#define SWITCH_ENUM_BUG

/* These three lines were new in 18.50.  They were said to permit
   a demand-paged executable, but someone else says they don't work.
   Someone else says they do.  They didn't work because errno was an
   initialized variable in crt0.c, and because of %splimit (also therein),
   both of which have been fixed now. */
#define SECTION_ALIGNMENT 0x03ff
#define SEGMENT_MASK 0xffff
#define LD_SWITCH_MACHINE -z
