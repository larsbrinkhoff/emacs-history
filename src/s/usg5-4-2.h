/* s/ file for System V release 4.2.  */

#include "s/usg5-4.h"

/* fsf@cygnus.com says these exist.  */
#define HAVE_VFORK
#define HAVE_TCATTR
#define HAVE_SYSV_SIGPAUSE
#define HAVE_GETHOSTNAME
#define HAVE_RANDOM
/* #define HAVE_GETWD  (appears to be buggy on SVR4.2) */

/* Info from fnf@cygnus.com suggests this is appropriate.  */
#define POSIX_SIGNALS

/* We don't need the definition from usg5-3.h with POSIX_SIGNALS.  */
#undef sigsetmask
