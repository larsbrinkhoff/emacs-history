/* m- file for Sun's 386-based RoadRunner.  This file borrows heavily from
  "m-sun2.h", but since that file is heavily cpu-specific, it was easier
  not to include it.

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

/* Say this machine is a bird */
#ifndef roadrunner
#define roadrunner
#endif

/* Actual cpu-specific defs */
#include "m-intel386.h"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Underscores are not prepended to C symbols on this machine.  */ 
#undef LDAV_SYMBOL
#define LDAV_SYMBOL "avenrun"

/* Must use the system's termcap.  It does special things.  */

#define LIBS_TERMCAP -ltermcap

/* Arrange to link with sun windows, if requested.  */
/* For details on emacstool and sunfns, see etc/SUN-SUPPORT */
/* These programs require Sun UNIX 4.2 Release 3.2 or greater */

#ifdef HAVE_SUN_WINDOWS
#define OTHER_FILES  ${etcdir}emacstool
#define LIBS_MACHINE -lsuntool -lsunwindow -lpixrect
#define OBJECTS_MACHINE sunfns.o
#define SYMS_MACHINE syms_of_sunfns ()
#define PURESIZE 132000
#endif

/* Roadrunner uses 'COFF' format */
#define COFF

#define C_SWITCH_MACHINE -Bstatic       /* avoid dynamic linking */
#define LD_SWITCH_MACHINE -n -Bstatic
/* Get rid of the -e __start that s-sunos4.h does.  */
#undef LD_SWITCH_SYSTEM
