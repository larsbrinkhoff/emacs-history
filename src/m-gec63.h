/* m- file for gec63
   Copyright (C) 1986 Free Software Foundation, Inc.

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

/* GEC63 is big-endian: lowest numbered byte is most significant. */

#define BIG_ENDIAN

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (((c)<<24)>>24)

/* Say this machine is a 68000 */

#define gec63

/* Use an int to represent Lisp_Object */

#define NO_UNION_TYPE

/* GEC63 has alloca in the PW/ux63 library.  */
#define LIB_STANDARD -lPW -lc
#define HAVE_ALLOCA

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no /dev/kmem */

#undef ADDR_CORRECT(x)
#define NO_ARG_ARRAY

#undef TERMCAP
#define TERMINFO

/* Define sizes of portions of a Lisp_Object.  */
#define VALBITS 22
#define GCTYPEBITS 5

#define VALAMASK (((1<<VALBITS) - 1)| 0xF0000000L)

#define XTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))
#define XSETTYPE(a, b) ((a)  =  ((a) & VALAMASK)  +  ((int)(b) << VALBITS))

#define XPNTR(a) ((a) & VALAMASK)
#define XSETPNTR(a, b) ((a) = ((a) & ~VALAMASK)  +  ((b) & VALAMASK))

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + ((int) (ptr) & VALAMASK))

/* Move some garbage-collector flag bits to different bit positions.  */
#define ARRAY_MARK_FLAG (1 << 27)
#define DONT_COPY_FLAG (1 << 26)

#define NO_REMAP
