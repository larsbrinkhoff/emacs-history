/* m- file for tek4300.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* 68000 has lowest-numbered byte as most significant */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#undef NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#define tek4300

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (x)

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead. */

#define C_ALLOCA

/* setjmp and longjmp can safely replace _setjmp and _longjmp, */

#define _longjmp longjmp
#define _setjmp setjmp

/* The text segment always starts at a fixed address.
   This way we don't need to have a label _start defined.  */

#define TEXT_START 0

/* The Tektronix exec struct for ZMAGIC files is struct zexec */

#define EXEC_HDR_TYPE struct zexec

/* The entry-point label (start of text segment) is `start', not `__start'.  */

#define DEFAULT_ENTRY_ADDRESS start
