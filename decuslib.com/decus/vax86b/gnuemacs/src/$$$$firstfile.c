/* VMS file for providing start and end of data area.
   Copyright (C) 1986 Mukesh Prasad

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

/* Special VMS filename ensures the static data of this module is the
 * first data in the $DATA segment.  The special variable name 
 * ensures the variable will be allocated at the end of data area.
 * LINKER OPTIONS SHOULD NOT BE USED TO BREAK DATA AREA INTO DIFFERENT
 * CLUSTERS.
 */

static char buf[ 512 ];
int ____end_of_data;	/* Special variable name to ensure it is located
			 * at end of memory.
			 */

char * start_of_data()
{
    return ( (char *) ( ( (int) buf + 511 ) & ~511 ));
}

char * end_of_data()
{
    return ( &____end_of_data );
}
