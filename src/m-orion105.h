/* m- file for HLH Orion 1/05 (Clipper).
   Copyright (C) 1985 Free Software Foundation, Inc.
   Lee McLoughlin <lmjm%doc.imperial.ac.uk@nss.cs.ucl.ac.uk>

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

#undef BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#define SIGN_EXTEND_CHAR(c) ((int)(c))

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#ifndef FSCALE
#define FSCALE 1.0
#endif
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* HLH have a SIGWINCH defined (but unimplemented) so we need a sigmask */
#ifndef sigmask
#define sigmask(m) (1 << ((m) - 1))
#endif

#define HAVE_ALLOCA

/* Here is where programs actually start running */
#define TEXT_START 0x8000
#define LD_TEXT_START_ADDR 8000

/* Arguments to ignore before argc in crt0.c.  */
#define DUMMIES dummy1, dummy2,

/* dbx can't cope so what the heck - currently (July 88) eval.c causes
 * the compiler to go into an infinite loop - so compile it by hand
 *    cc -c -Demacs eval.c
 * before running make
 */
#define C_DEBUG_SWITCH -O

/* Since not debugging don't add dbx lib */
#define LIBS_DEBUG 
