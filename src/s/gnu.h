/* s/ file for the GNU Hurd.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu"

/* XXX should getloadavg be in libc?  Should we have a libutil?
#define HAVE_GETLOADAVG */

#define HAVE_UNION_WAIT

#define SIGNALS_VIA_CHARACTERS

#define HAVE_TERMIOS
#define NO_TERMIO

/* XXX emacs should not expect TAB3 to be defined.  */
#define TABDLY OXTABS
#define TAB3 OXTABS


#define SYSV_SYSTEM_DIR

/* GNU has POSIX-style pgrp behavior.  */
#undef BSD_PGRPS

/* Reread the time zone on startup.  */
#define LOCALTIME_CACHE

#define HAVE_WAIT_HEADER

/* GNU needs its own crt0, and libc defines data_start.  */
#define ORDINARY_LINK

/* Until mib finishes the terminal driver.  */
#define BROKEN_SIGIO
#undef HAVE_PTYS

/* Until bson finishes the network.  */
#undef HAVE_SOCKETS
