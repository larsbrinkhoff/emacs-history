/* Unexec for HP9000 series 800.
   Note that the GNU project considers support for series 800 operation
   a peripheral activity which should not be allowed to divert effort
   from development of the GNU system.  Changes in this code will be
   installed when users send them in, but aside from that
   we don't plan to think about it, or about whether other Emacs
   maintenance might break it.

   Copyright (C) 1988 Free Software Foundation, Inc.
   Contributed by Eric Decker.

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
 * undump.c - Convert a core file to an a.out.
 *
 * Usage:
 * undump new-a.out [a.out] [core]
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/signal.h>
#include <sys/user.h>
#include <sys/stat.h>
#include <magic.h>

#include "config.h"

#define PSIZE	    10240

struct header hdr,ohdr;
unsigned datacnt;
unsigned datastart;
unsigned dataend;


unexec( new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
    FILE *new, *a_out, *core;
    unsigned pagemask;
    /* avoid fclose hazards... */
    char buf0[BUFSIZ];
    char buf1[BUFSIZ];

    pagemask = NBPG -1;

    datastart = DATA_START;
    dataend = ((sbrk(0) + pagemask) &~pagemask);
    if(dataend < datastart){
      fprintf(stderr, "unexec error: data ends before beginning.\n");
      return(-1);
    }
    datacnt = dataend - datastart;
    
    if ((a_out = fopen(a_name, "r")) == NULL)
    {
	perror(a_name);
	exit(1);
    }
    (void) setbuf(a_out, buf0);
    

    if ((new = fopen(new_name, "w")) == NULL)
    {
	perror(new_name);
	exit(1);
    }
    (void) setbuf(a_out, buf1);

    make_hdr(new, a_out, entry_address);
    copy_text(new, a_out);
    copy_data(new);
    fclose(new);

    fclose(a_out);
    mark_x(new_name);
    return(0);
}

/*
 * Make the header in the new a.out from the header in the old one
 * modified by the new data size.
 */
#ifdef hp9000s800
#undef N_BADMAG
#define N_BADMAG(h) (h.a_magic != SHARE_MAGIC)
struct som_exec_auxhdr auxhdr,oauxhdr;
#endif hp9000s800

make_hdr(new, a_out, entry_addr)
@FILE *new, *a_out;
unsigned entry_addr;
{
  if (fread(&hdr, sizeof hdr, 1, a_out) != 1)
    {
      perror("Couldn't read header from a.out file");
      exit(1);
    }
  ohdr = hdr;
  if (fread(&auxhdr, sizeof auxhdr, 1, a_out) != 1)
    {
      perror("Couldn't read auxiliary header from a.out file");
      exit(1);
    }
  oauxhdr = auxhdr;
  printf("Bss  segment size was %u\n", auxhdr.exec_bsize);
  printf("Data segment size was %u", auxhdr.exec_dsize);
  
    /* this is very hackly */
  /* do just enough to make the exec 
     loader happy */
  
  auxhdr.exec_dsize = datacnt;
  auxhdr.exec_bsize = 0;			/* all data is inited now! */
  auxhdr.exec_tfile = roundup(hdr.aux_header_location + hdr.aux_header_size,NBPG);
  auxhdr.exec_dfile = roundup(auxhdr.exec_tfile+auxhdr.exec_tsize,NBPG);
  printf(" now is %u\n", auxhdr.exec_dsize);
  printf("Data segment loc was %u", auxhdr.exec_dmem);
  auxhdr.exec_dmem = datastart;
  printf(" now is %u\n", auxhdr.exec_dmem);
  
  /* entry point unchanged. */
  if (fwrite(&hdr, sizeof hdr, 1, new) != 1)
    {
      perror("Couldn't write header to new a.out file");
      exit(1);
    }
  if (fwrite(&auxhdr, sizeof auxhdr, 1, new) != 1)
    {
      perror("Couldn't write auxiliary header to new a.out file");
      exit(1);
    }
  
}

/*
 * Copy the text from the a.out to the new a.out
 */
copy_text(new, a_out)
     FILE *new, *a_out;
{
  char page[PSIZE];
  int txtcnt = auxhdr.exec_tsize;
  
  if (hdr.a_magic == EXEC_MAGIC)
    {
      printf("a.out file is not shared text, this wont work.\n");
      exit(1);
    }
  fseek(new, auxhdr.exec_tfile, 0);
  fseek(a_out, oauxhdr.exec_tfile, 0);
  while (txtcnt >= PSIZE)
    {
      if (fread(page, PSIZE, 1, a_out) != 1)
	{
	  perror("Read failure on a.out text");
	  exit(1);
	}
      if (fwrite(page, PSIZE, 1, new) != 1)
	{
	  perror("Write failure in text segment");
	  exit(1);
	}
      txtcnt -= PSIZE;
    }
  if (txtcnt)
    {
      if (fread(page, txtcnt, 1, a_out) != 1)
	{
	  perror("Read failure on a.out text");
	  exit(1);
	}
      if (fwrite(page, txtcnt, 1, new) != 1)
	{
	  perror("Write failure in text segment");
	  exit(1);
	}
    }
}

/*
 * copy the data from the core file to the new a.out
 */
copy_data(new)
@FILE *new;
{
  fseek(new,auxhdr.exec_dfile,0);
  if (fwrite(datastart,datacnt,1,new) != 1) {
    perror("Write failure in data segment");
    exit(1);
  }
}

/*
 * After succesfully building the new a.out, mark it executable
 */
mark_x(name)
char *name;
{
    struct stat sbuf;
    int um;

    um = umask(777);
    umask(um);
    if (stat(name, &sbuf) == -1)
    {
	perror ("Can't stat new a.out");
	fprintf(stderr, "Setting protection to %o\n", 0777 & ~um);
	sbuf.st_mode = 0777;
    }
    sbuf.st_mode |= 0111 & ~um;
    if (chmod(name, sbuf.st_mode) == -1)
	perror("Couldn't change mode of new a.out to executable");

}
