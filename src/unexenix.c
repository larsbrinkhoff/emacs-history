/* Unexec for Xenix.
   Note that the GNU project considers support for Xenix operation
   a peripheral activity which should not be allowed to divert effort
   from development of the GNU system.  Changes in this code will be
   installed when Xenix users send them in, but aside from that
   we don't plan to think about it, or about whether other Emacs
   maintenance might break it.

   Copyright (C) 1988 Free Software Foundation, Inc.

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
(C) 1987 Free Software Foundation, Inc."; and include following the
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


/*
  On 80386 Xenix, segmentation screws prevent us from modifying the text
  segment at all.  We basically just plug a new value for "data segment
  size" into the countless headers and copy the other records straight
  through.  The data segment is ORG'ed at the xs_rbase value of the data
  segment's xseg record (always @ 0x1880000, thanks to the "sophisticated
  memory management hardware" of the chip) and extends to sbrk(0), exactly.
  This code is afraid to malloc (should it be?), and alloca has to be the
  wimpy, malloc-based version; consequently, data is usually copied in
  smallish chunks.

  gb@entity.com
*/

#include "config.h"
#include <sys/types.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <varargs.h>
#include <a.out.h>

static void fatal_unexec ();

#define READ(_fd, _buffer, _size, _error_message, _error_arg) \
	errno = EEOF; \
	if (read(_fd, _buffer, _size) != _size) \
	  fatal_unexec(_error_message, _error_arg);

#define WRITE(_fd, _buffer, _size, _error_message, _error_arg) \
	if (write(_fd, _buffer, _size) != _size) \
	  fatal_unexec(_error_message, _error_arg);

#define SEEK(_fd, _position, _error_message, _error_arg) \
	errno = EEOF; \
	if (lseek(_fd, _position, L_SET) != _position) \
	  fatal_unexec(_error_message, _error_arg);

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];
#define EEOF -1

#ifndef L_SET
#define L_SET 0
#endif

/* Should check the magic number of the old executable;
   not yet written.  */
check_exec (x)
     struct xexec *x;
{
}


unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  char *sbrk (), *datalim = sbrk (0), *data_org;
  long segpos, textseen, textpos, textlen, datapos, datadiff, datalen;

  struct xexec u_xexec,       /*  a.out header */
              *u_xexecp = &u_xexec;
  struct xext  u_xext,        /*  extended header */
              *u_xextp  = &u_xext;
  struct xseg  u_xseg,        /*  segment table entry */
              *u_xsegp  = &u_xseg;
  int i, nsegs, isdata = 0, infd, outfd;

  infd = open (a_name, O_RDONLY, 0);
  if (infd < 0) fatal_unexec ("opening %s", a_name);

  outfd = creat (new_name, 0666);
  if (outfd < 0) fatal_unexec ("creating %s", new_name);

  READ (infd, u_xexecp, sizeof (struct xexec),
	"error reading %s", a_name);
  check_exec (u_xexecp);
  READ (infd, u_xextp, sizeof (struct xext),
	"error reading %s", a_name);
  segpos = u_xextp->xe_segpos;
  nsegs = u_xextp->xe_segsize / sizeof (struct xseg);
  SEEK (infd, segpos, "seek error on %s", a_name);
  for (i = 0; i < nsegs; i ++)
    {
      READ (infd, u_xsegp, sizeof (struct xseg),
	    "error reading %s", a_name);
      switch (u_xsegp->xs_type)
	{
	case XS_TTEXT:
	  {
	    if (i == 0)
	      {
		textpos = u_xsegp->xs_filpos;
		textlen = u_xsegp->xs_psize;
		break;
	      }
	    fatal_unexec ("invalid text segment in %s", a_name);
	  }
	case XS_TDATA:
	  {
	    if (i == 1)
	      {
		datapos = u_xsegp->xs_filpos;
		datalen = datalim - (data_org = (char *)(u_xsegp->xs_rbase));
		datadiff = datalen - u_xsegp->xs_psize;
		break;
	      }
	    fatal_unexec ("invalid data segment in %s", a_name);
	  }
	default:
	  {
	    if (i > 1) break;
	    fatal_unexec ("invalid segment record in %s", a_name);
	  }
	}
    }
  u_xexecp->x_data = datalen;
  u_xexecp->x_bss = 0;
  WRITE (outfd, u_xexecp, sizeof (struct xexec),
	 "error writing %s", new_name);
  WRITE (outfd, u_xextp, sizeof (struct xext),
	 "error writing %s", new_name);
  SEEK (infd, segpos, "seek error on %s", a_name);
  SEEK (outfd, segpos, "seek error on %s", new_name);

  /* Copy the text segment record verbatim. */

  copyrec (infd, outfd, sizeof (struct xseg), a_name, new_name);

  /* Read, modify, write the data segment record. */

  READ (infd, u_xsegp, sizeof (struct xseg),
	"error reading %s", a_name);
  u_xsegp->xs_psize = u_xsegp->xs_vsize = datalen;
  u_xsegp->xs_attr &= (~XS_AITER & ~XS_ABSS);
  WRITE (outfd, u_xsegp, sizeof (struct xseg),
	 "error writing %s", new_name);

  /* Now copy any additional segment records, adjusting their
     file position field */

  for (i = 2; i < nsegs; i++)
    {
      READ (infd, u_xsegp, sizeof (struct xseg),
	    "error reading %s", a_name);
      u_xsegp->xs_filpos += datadiff;
      WRITE (outfd, u_xsegp, sizeof (struct xseg),
	     "error writing %s", new_name);
    }

  SEEK (infd, textpos, "seek error on %s", a_name);
  SEEK (outfd, textpos, "seek error on %s", new_name);
  copyrec (infd, outfd, textlen, a_name, new_name);

  SEEK (outfd, datapos, "seek error on %s", new_name);
  WRITE (outfd, data_org, datalen,
	 "write error on %s", new_name);

  for (i = 2, segpos += (2 * sizeof (struct xseg)); 
       i < nsegs;
       i++, segpos += sizeof (struct xseg))
    {
      SEEK (infd, segpos, "seek error on %s", a_name);
      READ (infd, u_xsegp, sizeof (struct xseg),
	    "read error on %s", a_name);
      SEEK (infd, u_xsegp->xs_filpos, "seek error on %s", a_name);
      /* We should be at eof in the output file here, but we must seek
         because the xs_filpos and xs_psize fields in symbol table
	 segments are inconsistent. */
      SEEK (outfd, u_xsegp->xs_filpos + datadiff, "seek error on %s", new_name);
      copyrec (infd, outfd, u_xsegp->xs_psize, a_name, new_name);
    }
  close (infd);
  close (outfd);
  mark_x (new_name);
  return 0;
}

copyrec (infd, outfd, len, in_name, out_name)
     int infd, outfd, len;
     char *in_name, *out_name;
{
  char buf[BUFSIZ];
  int chunk;

  while (len)
    {
      chunk = BUFSIZ;
      if (chunk > len)
	chunk = len;
      READ (infd, buf, chunk, "error reading %s", in_name);
      WRITE (outfd, buf, chunk, "error writing %s", out_name);
      len -= chunk;
    }
}

/*
 * mark_x
 *
 * After succesfully building the new a.out, mark it executable
 */
static
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um = umask (777);
  umask (um);
  if (stat (name, &sbuf) < 0)
    fatal_unexec ("getting protection on %s", name);
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) < 0)
    fatal_unexec ("setting protection on %s", name);
}

static void
fatal_unexec (s, va_alist)
    va_dcl
{
  va_list ap;
  if (errno == EEOF)
    fputs ("unexec: unexpected end of file, ", stderr);
  else if (errno < sys_nerr)
    fprintf (stderr, "unexec: %s, ", sys_errlist[errno]);
  else
    fprintf (stderr, "unexec: error code %d, ", errno);
  va_start (ap);
  _doprnt (s, ap, stderr);
  fputs (".\n", stderr);
  exit (1);
}
