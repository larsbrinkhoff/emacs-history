#include "s-bsd4-2.h"

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#define LD_SWITCH_SYSTEM -e __start -Bstatic
