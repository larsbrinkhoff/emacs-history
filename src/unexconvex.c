/* Modified version of unexec for convex machines.
   Note that the GNU project considers support for the peculiarities
   of the Convex operating system a peripheral activity which should
   not be allowed to divert effort from development of the GNU system.
   Changes in this code will be installed when Convex system
   maintainers send them in, but aside from that we don't plan to
   think about it, or about whether other Emacs maintenance might
   break it.

   Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

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
    License Agreement (except that you may choose to grant more
    extensive warranty protection to third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

  3. You may copy and distribute this program or any portion of it in
compiled, executable or object code form under the terms of Paragraphs
1 and 2 above provided that you do the following:

    a) cause each such copy to be accompanied by the
    corresponding machine-readable source code, which must
    be distributed under the terms of Paragraphs 1 and 2 above; or,

    b) cause each such copy to be accompanied by a
    written offer, with no time limit, to give any third party
    free (except for a nominal shipping charge) a machine readable
    copy of the corresponding source code, to be distributed
    under the terms of Paragraphs 1 and 2 above; or,

    c) in the case of a recipient of this program in compiled, executable
    or object code form (without the corresponding source code) you
    shall cause copies you distribute to be accompanied by a copy
    of the written offer of source code which you received along
    with the copy you received.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

  5. If you wish to incorporate parts of this program into other free
programs whose distribution conditions are different, write to the Free
Software Foundation at 1000 Mass Ave, Cambridge, MA 02138.  We have not yet
worked out a simple rule that can be stated here, but we will often permit
this.  We will be guided by the two goals of preserving the free status of
all derivatives of our free software and of promoting the sharing and reuse of
software.


In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/* modifyed for C-1 arch by jthomp@convex 871103 */

/*
 * unexec.c - Convert a running program into an a.out file.
 *
 * Author:	Spencer W. Thomas
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified heavily since then.
 *
 * Synopsis:
 *	unexec (new_name, a_name, data_start, bss_start, entry_address)
 *	char *new_name, *a_name;
 *	unsigned data_start, bss_start, entry_address;
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If a_name is non-NULL, the symbol table will be taken from the given file.
 * On some machines, an existing a_name file is required.
 *
 * The boundaries within the a.out file may be adjusted with the data_start
 * and bss_start arguments.  Either or both may be given as 0 for defaults.
 *
 * Data_start gives the boundary between the text segment and the data
 * segment of the program.  The text segment can contain shared, read-only
 * program code and literal data, while the data segment is always unshared
 * and unprotected.  Data_start gives the lowest unprotected address.
 * The value you specify may be rounded down to a suitable boundary
 * as required by the machine you are using.
 *
 * Specifying zero for data_start means the boundary between text and data
 * should not be the same as when the program was loaded.
 * If NO_REMAP is defined, the argument data_start is ignored and the
 * segment boundaries are never changed.
 *
 * Bss_start indicates how much of the data segment is to be saved in the
 * a.out file and restored when the program is executed.  It gives the lowest
 * unsaved address, and is rounded up to a page boundary.  The default when 0
 * is given assumes that the entire data segment is to be stored, including
 * the previous data and bss as well as any additional storage allocated with
 * break (2).
 *
 * The new file is set up to start at entry_address.
 *
 * If you make improvements I'd like to get them too.
 * harpo!utah-cs!thomas, thomas@Utah-20
 *
 */

/* There are several compilation parameters affecting unexec:

* COFF

Define this if your system uses COFF for executables.
Otherwise we assume you use Berkeley format.

* NO_REMAP

Define this if you do not want to try to save Emacs's pure data areas
as part of the text segment.

Saving them as text is good because it allows users to share more.

However, on machines that locate the text area far from the data area,
the boundary cannot feasibly be moved.  Such machines require
NO_REMAP.

Also, remapping can cause trouble with the built-in startup routine
/lib/crt0.o, which defines `environ' as an initialized variable.
Dumping `environ' as pure does not work!  So, to use remapping,
you must write a startup routine for your machine in Emacs's crt0.c.
If NO_REMAP is defined, Emacs uses the system's crt0.o.

* SECTION_ALIGNMENT

Some machines that use COFF executables require that each section
start on a certain boundary *in the COFF file*.  Such machines should
define SECTION_ALIGNMENT to a mask of the low-order bits that must be
zero on such a boundary.  This mask is used to control padding between
segments in the COFF file.

If SECTION_ALIGNMENT is not defined, the segments are written
consecutively with no attempt at alignment.  This is right for
unmodified system V.

* SEGMENT_MASK

Some machines require that the beginnings and ends of segments
*in core* be on certain boundaries.  For most machines, a page
boundary is sufficient.  That is the default.  When a larger
boundary is needed, define SEGMENT_MASK to a mask of
the bits that must be zero on such a boundary.

* A_TEXT_OFFSET(HDR)

Some machines count the a.out header as part of the size of the text
segment (a_text); they may actually load the header into core as the
first data in the text segment.  Some have additional padding between
the header and the real text of the program that is counted in a_text.

For these machines, define A_TEXT_OFFSET(HDR) to examine the header
structure HDR and return the number of bytes to add to `a_text'
before writing it (above and beyond the number of bytes of actual
program text).  HDR's standard fields are already correct, except that
this adjustment to the `a_text' field has not yet been made;
thus, the amount of offset can depend on the data in the file.
  
* A_TEXT_SEEK(HDR)

If defined, this macro specifies the number of bytes to seek into the
a.out file before starting to write the text segment.a

* EXEC_MAGIC

For machines using COFF, this macro, if defined, is a value stored
into the magic number field of the output file.

* ADJUST_EXEC_HEADER

This macro can be used to generate statements to adjust or
initialize nonstandard fields in the file header

* ADDR_CORRECT(ADDR)

Macro to correct an int which is the bit pattern of a pointer to a byte
into an int which is the number of a byte.

This macro has a default definition which is usually right.
This default definition is a no-op on most machines (where a
pointer looks like an int) but not on all machines.

*/

#ifndef emacs
#define PERROR(arg) perror (arg); return -1
#else
#include "config.h"
#define PERROR(file) report_error (file, new)
#endif

#ifndef CANNOT_DUMP  /* all rest of file!  */

#ifndef CANNOT_UNEXEC /* most of rest of file */

#include <a.out.h>
/* Define getpagesize () if the system does not.
   Note that this may depend on symbols defined in a.out.h
 */
#include "getpagesize.h"

#ifndef makedev			/* Try to detect types.h already loaded */
#include <sys/types.h>
#endif
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>

extern char *start_of_text ();		/* Start of text */
extern char *start_of_data ();		/* Start of initialized data */

#ifdef COFF
#ifndef USG
#ifndef STRIDE
#ifndef UMAX
/* I have a suspicion that these are turned off on all systems
   and can be deleted.  Try it in version 19.  */
#include <machine/filehdr.h>
#include <machine/opthdr.h>
#include <machine/scnhdr.h>
#include <machine/pte.h>
#include <machine/symtab.h>
#endif /* not UMAX */
#endif /* Not STRIDE */
#endif /* not USG */
static long block_copy_start;		/* Old executable start point */
static struct filehdr f_hdr;		/* File header */
static struct opthdr f_ohdr;		/* Optional file header (a.out) */
long bias;			/* Bias to add for growth */
long lnnoptr;			/* Pointer to line-number info within file */
#define SYMS_START block_copy_start

static long text_scnptr;
static long data_scnptr;

#else /* not COFF */

extern char *sbrk ();

#define SYMS_START ((long) N_SYMOFF (ohdr))

#ifdef HPUX
#ifdef HP9000S200_ID
#define MY_ID HP9000S200_ID
#else
#include <model.h>
#define MY_ID MYSYS
#endif /* no HP9000S200_ID */
static MAGIC OLDMAGIC = {MY_ID, SHARE_MAGIC};
static MAGIC NEWMAGIC = {MY_ID, DEMAND_MAGIC};
#define N_TXTOFF(x) TEXT_OFFSET(x)
#define N_SYMOFF(x) LESYM_OFFSET(x)
static struct exec hdr, ohdr;

#else /* not HPUX */

#if defined (USG) && !defined (IRIS)
static struct bhdr hdr, ohdr;
#define a_magic fmagic
#define a_text tsize
#define a_data dsize
#define a_bss bsize
#define a_syms ssize
#define a_trsize rtsize
#define a_drsize rdsize
#define a_entry entry
#define	N_BADMAG(x) \
    (((x).fmagic)!=OMAGIC && ((x).fmagic)!=NMAGIC &&\
     ((x).fmagic)!=FMAGIC && ((x).fmagic)!=IMAGIC)
#define NEWMAGIC FMAGIC
#else /* IRIS or not USG */
static struct exec hdr, ohdr;
#define NEWMAGIC ZMAGIC
#endif /* IRIS or not USG */
#endif /* not HPUX */

static int unexec_text_start;
static int unexec_data_start;

#endif /* not COFF */

static int pagemask;

/* Correct an int which is the bit pattern of a pointer to a byte
   into an int which is the number of a byte.
   This is a no-op on ordinary machines, but not on all.  */

#ifndef ADDR_CORRECT   /* Let m-*.h files override this definition */
#define ADDR_CORRECT(x) ((char *)(x) - (char*)0)
#endif

#ifdef emacs

static
report_error (file, fd)
     char *file;
     int fd;
{
  if (fd)
    close (fd);
  error ("Failure operating on %s", file);
}
#endif /* emacs */

#define ERROR0(msg) report_error_1 (new, msg, 0, 0); return -1
#define ERROR1(msg,x) report_error_1 (new, msg, x, 0); return -1
#define ERROR2(msg,x,y) report_error_1 (new, msg, x, y); return -1

static
report_error_1 (fd, msg, a1, a2)
     int fd;
     char *msg;
     int a1, a2;
{
  close (fd);
#ifdef emacs
  error (msg, a1, a2);
#else
  fprintf (stderr, msg, a1, a2);
  fprintf (stderr, "\n");
#endif
}

/* ****************************************************************
 * unexec
 *
 * driving logic.
 */
unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int new, a_out = -1;

  if (a_name && (a_out = open (a_name, 0)) < 0)
    {
      PERROR (a_name);
    }
  if ((new = creat (new_name, 0666)) < 0)
    {
      PERROR (new_name);
    }

  if (make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name) < 0
      || copy_text_and_data (new) < 0
      || copy_sym (new, a_out, a_name, new_name) < 0
#ifdef COFF
      || adjust_lnnoptrs (new, a_out, new_name) < 0
#endif
      )
    {
      close (new);
      /* unlink (new_name);	    	/* Failed, unlink new a.out */
      return -1;	
    }

  close (new);
  if (a_out >= 0)
    close (a_out);
  mark_x (new_name);
  return 0;
}

/* ****************************************************************
 * make_hdr
 *
 * Make the header in the new a.out from the header in core.
 * Modify the text and data sizes.
 */
#ifdef COFF
 struct scnhdr f_thdr;		/* Text section header */
 struct scnhdr f_dhdr;		/* Data section header */
 struct scnhdr f_bhdr;		/* Bss section header */
 struct scnhdr scntemp;		/* Temporary section header */
#endif /* COFF */
static int
make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name)
     int new, a_out;
     unsigned data_start, bss_start, entry_address;
     char *a_name;
     char *new_name;
{
  int tem;
  register int scns;
  unsigned int bss_end;

  pagemask = getpagesize () - 1;

  /* Adjust text/data boundary. */
#ifdef NO_REMAP
  data_start = (int) start_of_data ();
#else /* not NO_REMAP */
  if (!data_start)
    data_start = (int) start_of_data ();
#endif /* not NO_REMAP */
  data_start = ADDR_CORRECT (data_start);

#ifdef SEGMENT_MASK
  data_start = data_start & ~SEGMENT_MASK; /* (Down) to segment boundary. */
#else
  data_start = data_start & ~pagemask; /* (Down) to page boundary. */
#endif

  bss_end = (ADDR_CORRECT (sbrk (0)) + pagemask) & ~pagemask;

  /* Adjust data/bss boundary. */
  if (bss_start != 0)
    {
      bss_start = (ADDR_CORRECT (bss_start) + pagemask) & ~pagemask;	      /* (Up) to page bdry. */
      if (bss_start > bss_end)
	{
	  ERROR1 ("unexec: Specified bss_start (%u) is past end of program",
		  bss_start);
	}
    }
  else
    bss_start = bss_end;

  if (data_start > bss_start)	/* Can't have negative data size. */
    {
      ERROR2 ("unexec: data_start (%u) can't be greater than bss_start (%u)",
	      data_start, bss_start);
    }

#ifdef COFF
  /* Salvage as much info from the existing file as possible */
  if (a_out >= 0)
    {
      if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
	{
	  PERROR (a_name);
	}
      block_copy_start += sizeof (f_hdr);
      if (f_hdr.h_opthdr > 0)
	{
	  if (read (a_out, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
	    {
	      PERROR (a_name);
	    }
	  block_copy_start += sizeof (f_ohdr);
	}
      /* Loop through section headers, copying them in */
      for (scns = f_hdr.h_nscns; scns > 0; scns--) {
	if (read (a_out, &scntemp, sizeof (scntemp)) != sizeof (scntemp))
	  {
	    PERROR (a_name);
	  }
	if (scntemp.s_scnptr > 0L)
	  {
            if (block_copy_start < scntemp.s_scnptr + scntemp.s_size)
	      block_copy_start = scntemp.s_scnptr + scntemp.s_size;
	  }
	if (scntemp.s_flags & S_TEXT)
	  {
	    f_thdr = scntemp;
	  }
	else if (scntemp.s_flags & S_DATA)
	  {
	    f_dhdr = scntemp;
	  }
	else if (scntemp.s_flags & S_BSS)
	  {
	    f_bhdr = scntemp;
	  }
      }
    }
  else
    {
      ERROR0 ("can't build a COFF file from scratch yet");
    }

  /* Now we alter the contents of all the f_*hdr variables
     to correspond to what we want to dump.  */

  f_hdr.h_flags |= (0);      /* this isn't really correct..*/
#ifdef EXEC_MAGIC
  f_ohdr.magic = EXEC_MAGIC;
#endif
  f_thdr.s_vaddr = (long) start_of_text ();
  f_thdr.s_size = data_start - f_thdr.s_vaddr;
  f_thdr.s_scnptr = 4096;
  f_thdr.s_relptr = 0;
  f_thdr.s_nrel = 0;
  f_thdr.s_prot = 0xA;

  f_dhdr.s_vaddr = data_start;
  f_dhdr.s_size = bss_start - data_start;
  f_dhdr.s_scnptr = 4096 + (data_start - f_thdr.s_vaddr);
  f_dhdr.s_relptr = 0;
  f_dhdr.s_nrel = 0;
  f_dhdr.s_prot = 0xC;
  
  f_bhdr.s_vaddr = bss_start;
  f_bhdr.s_size = bss_end - bss_start + 4096 /* fudge */;
  f_bhdr.s_scnptr = 0;
  f_bhdr.s_relptr = 0;
  f_bhdr.s_nrel = 0;
  f_bhdr.s_prot = 0xC;
#ifdef SECTION_ALIGNMENT
  /* Some systems require special alignment
     of the sections in the file itself.  */
  f_thdr.s_scnptr
    = (f_thdr.s_scnptr + SECTION_ALIGNMENT) & ~SECTION_ALIGNMENT;
#endif /* SECTION_ALIGNMENT */
  text_scnptr = f_thdr.s_scnptr;
  data_scnptr = f_dhdr.s_scnptr;
  bias = f_dhdr.s_scnptr + f_dhdr.s_size - block_copy_start;

  if (f_ohdr.o_symptr > 0L)
    {
      f_ohdr.o_symptr += bias;
    }

  if (f_hdr.h_strptr > 0)
  {
      f_hdr.h_strptr += bias;
  }

#ifdef ADJUST_EXEC_HEADER
  ADJUST_EXEC_HEADER
#endif /* ADJUST_EXEC_HEADER */

  if (write (new, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_thdr, sizeof (f_thdr)) != sizeof (f_thdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_dhdr, sizeof (f_dhdr)) != sizeof (f_dhdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_bhdr, sizeof (f_bhdr)) != sizeof (f_bhdr))
    {
      PERROR (new_name);
    }
  return (0);

#else /* if not COFF */

  /* Get symbol table info from header of a.out file if given one. */
  if (a_out >= 0)
    {
      if (read (a_out, &ohdr, sizeof hdr) != sizeof hdr)
	{
	  PERROR (a_name);
	}

      if N_BADMAG (ohdr)
	{
	  ERROR1 ("invalid magic number in %s", a_name);
	}
      hdr = ohdr;
    }
  else
    {
      bzero (hdr, sizeof hdr);
    }

  unexec_text_start = (long) start_of_text ();
  unexec_data_start = data_start;

  /* Machine-dependent fixup for header, or maybe for unexec_text_start */
#ifdef ADJUST_EXEC_HEADER
  ADJUST_EXEC_HEADER;
#endif /* ADJUST_EXEC_HEADER */

  hdr.a_trsize = 0;
  hdr.a_drsize = 0;
  if (entry_address != 0)
    hdr.a_entry = entry_address;

  hdr.a_bss = bss_end - bss_start;
  hdr.a_data = bss_start - data_start;
#ifdef NO_REMAP
  hdr.a_text = ohdr.a_text;
#else /* not NO_REMAP */
  hdr.a_text = data_start - unexec_text_start;
#endif /* not NO_REMAP */

#ifdef A_TEXT_OFFSET
  hdr.a_text += A_TEXT_OFFSET (ohdr);
#endif

  if (write (new, &hdr, sizeof hdr) != sizeof hdr)
    {
      PERROR (new_name);
    }

#ifdef A_TEXT_OFFSET
  hdr.a_text -= A_TEXT_OFFSET (ohdr);
#endif

  return 0;

#endif /* not COFF */
}

/* ****************************************************************
 * copy_text_and_data
 *
 * Copy the text and data segments from memory to the new a.out
 */
static int
copy_text_and_data (new)
     int new;
{
  register char *end;
  register char *ptr;

#ifdef COFF
  lseek (new, (long) text_scnptr, 0);
  ptr = (char *) f_thdr.s_vaddr;
  end = ptr + f_thdr.s_size;
  write_segment (new, ptr, end);

  lseek (new, (long) data_scnptr, 0);
  ptr = (char *) f_dhdr.s_vaddr;
  end = ptr + f_dhdr.s_size;
  write_segment (new, ptr, end);

#else /* if not COFF */

/* Some machines count the header as part of the text segment.
   That is to say, the header appears in core
   just before the address that start_of_text () returns.
   For them, N_TXTOFF is the place where the header goes.
   We must adjust the seek to the place after the header.
   Note that at this point hdr.a_text does *not* count
   the extra A_TEXT_OFFSET bytes, only the actual bytes of code.  */

#ifdef A_TEXT_SEEK
  lseek (new, (long) A_TEXT_SEEK (hdr), 0);
#else
#ifdef A_TEXT_OFFSET
  /* Note that on the Sequent machine A_TEXT_OFFSET != sizeof (hdr)
     and sizeof (hdr) is the correct amount to add here.  */
  /* In version 19, eliminate this case and use A_TEXT_SEEK whenever
     N_TXTOFF is not right.  */
  lseek (new, (long) N_TXTOFF (hdr) + sizeof (hdr), 0);
#else
  lseek (new, (long) N_TXTOFF (hdr), 0);
#endif /* no A_TEXT_OFFSET */
#endif /* no A_TEXT_SEEK */

  ptr = (char *) unexec_text_start;
  end = ptr + hdr.a_text;
  write_segment (new, ptr, end);

  ptr = (char *) unexec_data_start;
  end = ptr + hdr.a_data;
/*  This lseek is certainly incorrect when A_TEXT_OFFSET
    and I believe it is a no-op otherwise.
    Let's see if its absence ever fails.  */
/*  lseek (new, (long) N_TXTOFF (hdr) + hdr.a_text, 0); */
  write_segment (new, ptr, end);

#endif /* not COFF */

  return 0;
}

write_segment (new, ptr, end)
     int new;
     register char *ptr, *end;
{
  register int i, nwrite, ret;
  char buf[80];
  extern int errno;
  char zeros[128];

  bzero (zeros, sizeof zeros);

  for (i = 0; ptr < end;)
    {
      /* distance to next multiple of 128.  */
      nwrite = (((int) ptr + 128) & -128) - (int) ptr;
      /* But not beyond specified end.  */
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      /* If write gets a page fault, it means we reached
	 a gap between the old text segment and the old data segment.
	 This gap has probably been remapped into part of the text segment.
	 So write zeros for it.  */
      if (ret == -1 && errno == EFAULT)
	write (new, zeros, nwrite);
      else if (nwrite != ret)
	{
	  sprintf (buf,
		   "unexec write failure: addr 0x%x, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		   ptr, new, nwrite, ret, errno);
	  PERROR (buf);
	}
      i += nwrite;
      ptr += nwrite;
    }
}

/* ****************************************************************
 * copy_sym
 *
 * Copy the relocation information and symbol table from the a.out to the new
 */
static int
copy_sym (new, a_out, a_name, new_name)
     int new, a_out;
     char *a_name, *new_name;
{
  char page[1024];
  int n;

  if (a_out < 0)
    return 0;

#ifdef COFF
  if (SYMS_START == 0L)
    return 0;
#endif  /* COFF */

#ifdef COFF
  if (lnnoptr)			/* if there is line number info */
    lseek (a_out, lnnoptr, 0);	/* start copying from there */
  else
#endif /* COFF */
    lseek (a_out, SYMS_START, 0);	/* Position a.out to symtab. */

  while ((n = read (a_out, page, sizeof page)) > 0)
    {
      if (write (new, page, n) != n)
	{
	  PERROR (new_name);
	}
    }
  if (n < 0)
    {
      PERROR (a_name);
    }
  return 0;
}

/* ****************************************************************
 * mark_x
 *
 * After succesfully building the new a.out, mark it executable
 */
static
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um;
  int new = 0;  /* for PERROR */

  um = umask (777);
  umask (um);
  if (stat (name, &sbuf) == -1)
    {
      PERROR (name);
    }
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) == -1)
    PERROR (name);
}

/*
 *	If the COFF file contains a symbol table and a line number section,
 *	then any auxiliary entries that have values for x_lnnoptr must
 *	be adjusted by the amount that the line number section has moved
 *	in the file (bias computed in make_hdr).  The #@$%&* designers of
 *	the auxiliary entry structures used the absolute file offsets for
 *	the line number entry rather than an offset from the start of the
 *	line number section!
 *
 *	When I figure out how to scan through the symbol table and pick out
 *	the auxiliary entries that need adjustment, this routine will
 *	be fixed.  As it is now, all such entries are wrong and sdb
 *	will complain.   Fred Fish, UniSoft Systems Inc.
 */

#ifdef COFF

/* This function is probably very slow.  Instead of reopening the new
   file for input and output it should copy from the old to the new
   using the two descriptors already open (WRITEDESC and READDESC).
   Instead of reading one small structure at a time it should use
   a reasonable size buffer.  But I don't have time to work on such
   things, so I am installing it as submitted to me.  -- RMS.  */

adjust_lnnoptrs (writedesc, readdesc, new_name)
     int writedesc;
     int readdesc;
     char *new_name;
{
#if 0
  register int nsyms;
  register int new;
#ifdef amdahl_uts
  SYMENT symentry;
  AUXENT auxentry;
#else
  struct syment symentry;
  struct auxent auxentry;
#endif

  if (!lnnoptr || !f_hdr.f_symptr)
    return 0;

  if ((new = open (new_name, 2)) < 0)
    {
      PERROR (new_name);
      return -1;
    }

  lseek (new, f_hdr.f_symptr, 0);
  for (nsyms = 0; nsyms < f_hdr.f_nsyms; nsyms++)
    {
      read (new, &symentry, SYMESZ);
      if (symentry.n_numaux)
	{
	  read (new, &auxentry, AUXESZ);
	  nsyms++;
	  if (ISFCN (symentry.n_type)) {
	    auxentry.x_sym.x_fcnary.x_fcn.x_lnnoptr += bias;
	    lseek (new, -AUXESZ, 1);
	    write (new, &auxentry, AUXESZ);
	  }
	}
    }
  close (new);
#endif 0
}

#endif /* COFF */

#endif /* not CANNOT_UNEXEC */
#endif /* not CANNOT_DUMP */
