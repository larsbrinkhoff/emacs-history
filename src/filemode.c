/* Examine the result of  stat  and make a string describing file modes.
   Copyright (C) 1985 Free Software Foundation, Inc.

		       NO WARRANTY

  BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY
NO WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT
WHEN OTHERWISE STATED IN WRITING, FREE SOFTWARE FOUNDATION, INC,
RICHARD M. STALLMAN AND/OR OTHER PARTIES PROVIDE THIS PROGRAM "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY
AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

 IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL RICHARD M.
STALLMAN, THE FREE SOFTWARE FOUNDATION, INC., AND/OR ANY OTHER PARTY
WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR
DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR
A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS
PROGRAM, EVEN IF YOU HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY OTHER PARTY.

		GENERAL PUBLIC LICENSE TO COPY

  1. You may copy and distribute verbatim copies of this source file
as you receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy a valid copyright notice "Copyright
(C) 1985 Free Software Foundation, Inc."; and include following the
copyright notice a verbatim copy of the above disclaimer of warranty
and of this License.  You may charge a distribution fee for the
physical act of transferring a copy.

  2. You may modify your copy or copies of this source file or
any portion of it, and copy and distribute such modifications under
the terms of Paragraph 1 above, provided that you also do the following:

    a) cause the modified files to carry prominent notices stating
    that you changed the files and the date of any change; and

    b) cause the whole of any work that you distribute or publish,
    that in whole or in part contains or is a derivative of this
    program or any part thereof, to be licensed at no charge to all
    third parties on terms identical to those contained in this
    License Agreement (except that you may choose to grant more extensive
    warranty protection to some or all third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

Mere aggregation of another unrelated program with this program (or its
derivative) on a volume of a storage or distribution medium does not bring
the other program under the scope of these terms.

  3. You may copy and distribute this program (or a portion or derivative
of it, under Paragraph 2) in object code or executable form under the terms
of Paragraphs 1 and 2 above provided that you also do one of the following:

    a) accompany it with the complete corresponding machine-readable
    source code, which must be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    b) accompany it with a written offer, valid for at least three
    years, to give any third party free (except for a nominal
    shipping charge) a complete machine-readable copy of the
    corresponding source code, to be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    c) accompany it with the information you received as to where the
    corresponding source code may be obtained.  (This alternative is
    allowed only for noncommercial distribution and only if you
    received the program in object code or executable form alone.)

For an executable file, complete source code means all the source code for
all modules it contains; but, as a special exception, it need not include
source code for modules which are standard libraries that accompany the
operating system on which the executable file runs.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

  5. If you wish to incorporate parts of this program into other free
programs whose distribution conditions are different, write to the Free
Software Foundation at 675 Mass Ave, Cambridge, MA 02139.  We have not yet
worked out a simple rule that can be stated here, but we will often permit
this.  We will be guided by the two goals of preserving the free status of
all derivatives of our free software and of promoting the sharing and reuse of
software.


In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


#include <sys/types.h>
#include <sys/stat.h>

/* filemodestring - set file attribute data 

*** WARNING!  FILE STRUCTURE DEPENDENT ***

   Filemodestring converts the data in the st_mode field of file status
block `s' to a 10 character attribute string, which it stores in
the block that `a' points to.
This attribute string is modelled after the string produced by the Berkeley ls.

As usual under Unix, the elements of the string are numbered
from 0.  Their meanings are:

   0	File type.  'd' for directory, 'c' for character
	special, 'b' for block special, 'm' for multiplex,
	'l' for symbolic link, 's' for socket, 'p' for fifo,
	'-' for any other file type

   1	'r' if the owner may read, '-' otherwise.

   2	'w' if the owner may write, '-' otherwise.

   3	'x' if the owner may execute, 's' if the file is
	set-user-id, '-' otherwise.
	'S' if the file is set-user-id, but the execute
	bit isn't set.  (sys v `feature' which helps to
	catch screw case.)

   4	'r' if group members may read, '-' otherwise.

   5	'w' if group members may write, '-' otherwise.

   6	'x' if group members may execute, 's' if the file is
	set-group-id, '-' otherwise.
	'S' if it is set-group-id but not executable.

   7	'r' if any user may read, '-' otherwise.

   8	'w' if any user may write, '-' otherwise.

   9	'x' if any user may execute, 't' if the file is "sticky"
	(will be retained in swap space after execution), '-'
	otherwise.

 */

#define VOID void

static char ftypelet ();
static VOID rwx (), setst ();

VOID
filemodestring (s,a)
   struct stat	*s;
   char *a;
{
   a[0] = ftypelet (s);
   /* Aren't there symbolic names for these byte-fields? */
   rwx ((s->st_mode & 0700) << 0, &(a[1]));
   rwx ((s->st_mode & 0070) << 3, &(a[4]));
   rwx ((s->st_mode & 0007) << 6, &(a[7]));
   setst (s->st_mode, a);
}

/* ftypelet - file type letter

*** WARNING!  FILE STRUCTURE DEPENDENT ***

   Ftypelet accepts a file status block and returns a character
code describing the type of the file.  'd' is returned for
directories, 'b' for block special files, 'c' for character
special files, 'm' for multiplexor files, 'l' for symbolic link,
's' for socket, 'p' for fifo, '-' for any other file type
 */

static char
ftypelet(s)
   struct stat *s;
{
  switch (s->st_mode & S_IFMT)
    {
    default:
      return '-';
    case S_IFDIR:
      return 'd';
#ifdef S_IFLNK
    case S_IFLNK:
      return 'l';
#endif
#ifdef S_IFCHR
    case S_IFCHR:
      return 'c';
#endif
#ifdef S_IFBLK
    case S_IFBLK:
      return 'b';
#endif
#ifdef S_IFMPC
/* These do not seem to exist */
    case S_IFMPC:
    case S_IFMPB:
      return 'm';
#endif
#ifdef S_IFSOCK
    case S_IFSOCK:
      return 's';
#endif
#ifdef S_IFIFO
#if S_IFIFO != S_IFSOCK
    case S_IFIFO:
      return 'p';
#endif
#endif
#ifdef S_IFNWK /* hp-ux hack */
    case S_IFNWK:
      return 'n';
#endif
    }
}


/* rwx - look at read, write, and execute bits and set character
flags accordingly

*** WARNING!  FILE STRUCTURE DEPENDENT ***

 */

static VOID
rwx (bits, chars)
   unsigned short bits;
   char chars[];
{
  chars[0] = (bits & S_IREAD)  ? 'r' : '-';
  chars[1] = (bits & S_IWRITE) ? 'w' : '-';
  chars[2] = (bits & S_IEXEC)  ? 'x' : '-';
}


/* setst - set s & t flags in a file attributes string */
/* *** WARNING!  FILE STRUCTURE DEPENDENT *** */
static VOID
setst (bits, chars)
   unsigned short bits;
   char chars[];
{
#ifdef S_ISUID
   if (bits & S_ISUID)
     {
       if (chars[3] != 'x')
	 /* Screw case: set-uid, but not executable. */
	 chars[3] = 'S';
       else
	 chars[3] = 's';
     }
#endif
#ifdef S_ISGID
   if (bits & S_ISGID)
     {
       if (chars[6] != 'x')
	 /* Screw case: set-gid, but not executable. */
	 chars[6] = 'S';
       else
	 chars[6] = 's';
     }
#endif
#ifdef S_ISVTX
   if (bits & S_ISVTX)
      chars[9] = 't';
#endif
}
