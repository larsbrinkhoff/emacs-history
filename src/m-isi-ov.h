/* m- file for ISI 68000's
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


#define ISI68K

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* 68000 has lowest-numbered byte as most significant */

#define BIG_ENDIAN

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Say this machine is a 68000 */

#define m68000

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#ifdef BSD4_3
#define LOAD_AVE_TYPE long
#else
#define LOAD_AVE_TYPE double
#endif BSD4_3

/* Convert that into an integer that is 100 for a load average of 1.0  */

#ifdef BSD4_3
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)
#else
#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))
#endif

/* Mask for address bits within a memory segment */

#define SEGMENT_MASK 0x1ffff

/* use the -20 switch to get the 68020 code */
/* #define C_SWITCH_MACHINE -20 */

/* Use the version of the library for the 68020
   because the standard library requires some special hacks in crt0
   which the GNU crt0 does not have.  */

#define LIB_STANDARD -lmc

/* macros to make unexec work right */

#define A_TEXT_OFFSET(HDR) sizeof(HDR)
#define A_TEXT_SEEK(HDR) sizeof(HDR)

/* A few changes for the newer systems.  */

#ifdef BSD4_3
#define HAVE_ALLOCA
/* The following line affects crt0.c.  */
#undef m68k

#undef LIB_STANDARD
#define LIB_STANDARD -lmc -lc
#define C_DEBUG_SWITCH -20 -O -X23
#endif
