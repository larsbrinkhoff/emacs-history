/* RTPC AIX machine/system dependent defines
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

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) ((((int) (c)) << 24) >> 24)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#define IBMRTAIX

#ifndef AIX
#define AIX
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* No load average information appears in the AIX kernel.  VRM has this
   info, and if anyone desires they should fix fns.c to get it out of VRM */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define addresses, macros, change some setup for dump */

#undef COFF
#define NO_REMAP
#undef static
  /* Since NO_REMAP, problem with statics doesn't exist */

#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.  */

#define DATA_SEG_BITS 0x20000000

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
#define A_TEXT_OFFSET(HDR) sizeof(HDR)
#define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
#define STACK_DIRECTION -1 /* tell alloca.c which way it grows */

/* AIX has PTYs, but they work so differently that they would need
   lots of changes in process.c.  Therefore, they are disabled.  */

/* #define HAVE_PTYS */

/* AIX has IPC. It also has sockets, and either can be used for client/server.
   I would suggest the client/server code be changed to use HAVE_SOCKETS rather
   than BSD as the conditional if sockets provide any advantages. */

#define HAVE_SYSVIPC

/* AIX has sockets */

#define HAVE_SOCKETS
/* #define SKTPAIR */ /* SKTPAIR works, but what is advantage over pipes? */

/* Specify the font for X to use.  */

#define X_DEFAULT_FONT "Rom14.500"

/* Here override various assumptions in ymakefile */

/* On AIX 2.2.1, use these definitions instead
#define C_SWITCH_MACHINE -I/usr/include -Nn2000
#define LIBS_MACHINE -lrts
#define LIBX10_MACHINE -lrts
#define LIBX11_MACHINE -lrts
*/

#define C_SWITCH_MACHINE -I/usr/include -I/usr/include/bsd -Nn2000
/* need to duplicate -lsock -lbsd -lrts so refs in libX can be resolved   */
/* order of lib specs in ymakefile should probably be changed.            */
#define LIBS_MACHINE -lsock -lbsd -lrts
#define LIBX10_MACHINE -lsock -lbsd -lrts
#define LIBX11_MACHINE -lsock -lbsd -lrts

#define OBJECTS_MACHINE hftctl.o
#define START_FILES /lib/crt0.o
#define LD_SWITCH_MACHINE -n -T0x10000000 -K -e start

#if 0 /* I refuse to promulgate a recommendation that would make
         users unable to debug - RMS.  */
/* delete the following line to foil optimization, enable debugging */
#define C_DEBUG_SWITCH -O
#endif


/* Setup to do some things BSD way - these won't work previous to AIX 2.1.2 */

#define bzero(t,s) (memset((t),(0),(s)))
#define bcmp(f,t,s)     (memcmp((t),(f),(s)))
#define vfork fork
#define killpg( pgrp, sig )  (kill( -(pgrp), (sig) ))

#define BSTRING
#define HAVE_DUP2
#define HAVE_GETTIMEOFDAY
#define HAVE_SELECT
#define HAVE_TIMEVAL
#define HAVE_VFORK

/* But don't use utimes() -- it causes SIGSEGV!  Use utime() instead. */
#define USE_UTIME

/* getwd is in same object module as getcwd in AIX 2.2, but doesn't exist */
/* at all in 2.1.2.  So, for compatibility, avoid name collision on 2.2 */
#define getwd AIX_getwd

/* AIX defines FIONREAD, but it does not work.  */
#define BROKEN_FIONREAD
