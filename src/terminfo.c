/* Interface from Emacs to terminfo.
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

/* Define these variables that serve as global parameters to termcap,
   so that we do not need to conditionalize the places in Emacs
   that set them.  */

char *UP, *BC, PC;
short ospeed;

static buffer[512];

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.
*/

char *
tparam (string, outstring, len, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
     char *string;
     char *outstring;
     int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9;
{
  char *temp;
  extern char *tparm();

  temp = tparm (string, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  if (outstring == 0)
    outstring = ((char *) (malloc ((strlen (temp)) + 1)));
  strcpy (outstring, temp);
  return outstring;
}
