/* Work with core dump and executable files, for GDB.
   Copyright (C) 1986, 1987 Free Software Foundation, Inc.

GDB is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY.  No author or distributor accepts responsibility to anyone
for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.
Refer to the GDB General Public License for full details.

Everyone is granted permission to copy, modify and redistribute GDB,
but only under the conditions described in the GDB General Public
License.  A copy of this license is supposed to have been given to you
along with GDB so you can know your rights and responsibilities.  It
should be in a file named COPYING.  Among other things, the copyright
notice and this notice must be preserved on all copies.

In other words, go ahead and share GDB, but don't try to stop
anyone else from sharing it farther.  Help stamp out software hoarding!
*/

#include <stdio.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/file.h>
#include <sys/user.h>
#include <sys/stat.h>
#include <a.out.h>
#include "initialize.h"
#include "defs.h"
#include "param.h"

#ifdef NEW_SUN_CORE
#include <sys/core.h>
#endif /* NEW_SUN_CORE */

#ifndef N_TXTADDR
#define N_TXTADDR(hdr) 0
#endif /* no N_TXTADDR */

#ifndef N_DATADDR
#define N_DATADDR(hdr) hdr.a_text
#endif /* no N_DATADDR */

START_FILE

/* File names of core file and executable file.  */

static char *corefile;
static char *execfile;

/* Descriptors on which core file and executable file are open.  */

static int corechan;
static int execchan;

/* Last modification time of executable file.  */

int exec_mtime;

/* Virtual addresses of bounds of the two areas of memory in the core file.  */

static CORE_ADDR data_start;
static CORE_ADDR data_end;
static CORE_ADDR stack_start;
static CORE_ADDR stack_end;

/* Virtual addresses of bounds of two areas of memory in the exec file.
   Note that the data area in the exec file is used only when there is no core file.  */

static CORE_ADDR text_start;
static CORE_ADDR text_end;
static CORE_ADDR exec_data_start;
static CORE_ADDR exec_data_end;

/* Address in executable file of start of text area data.  */

static int text_offset;

/* Address in executable file of start of data area data.  */

static int exec_data_offset;

/* Address in core file of start of data area data.  */

static int data_offset;

/* Address in core file of start of stack area data.  */

static int stack_offset;

/* a.out header saved in core file.  */

struct exec core_aouthdr;

/* a,out header of exec file.  */

struct exec exec_aouthdr;

static void validate_files ();
unsigned int register_addr ();

core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;
  extern char registers[];

  if (corefile)
    free (corefile);
  corefile = 0;
  data_start = 0;
  data_end = 0;
  stack_start = STACK_END_ADDR;
  stack_end = STACK_END_ADDR;
  if (corechan >= 0)
    close (corechan);
  corechan = -1;
  if (filename)
    {
      if (have_inferior_p ())
	error ("To look at a core file, you must kill the inferior with \"kill\".");
      corechan = open (filename, O_RDONLY, 0);
      if (corechan < 0)
	perror_with_name (filename);
#ifdef NEW_SUN_CORE
      {
	struct core corestr;

	val = myread (corechan, &corestr, sizeof corestr);
	if (val < 0)
	  perror_with_name (filename);
	if (corestr.c_magic != CORE_MAGIC)
	  error ("\"%s\" does not appear to be a core dump file (magic 0x%x, expected 0x%x)",
		 filename, corestr.c_magic, (int) CORE_MAGIC);
	else if (sizeof (struct core) != corestr.c_len)
	  error ("\"%s\" has an invalid struct core length (%d, expected %d)",
		 filename, corestr.c_len, (int) sizeof (struct core));

	data_start = exec_data_start;
	data_end = data_start + corestr.c_dsize;
	stack_start = stack_end - corestr.c_ssize;
	data_offset = sizeof corestr;
	stack_offset = sizeof corestr + corestr.c_dsize;

	bcopy (&corestr.c_regs, registers, 16 * 4);
	*(int *)&registers[REGISTER_BYTE (PS_REGNUM)] = corestr.c_regs.r_ps;
	*(int *)&registers[REGISTER_BYTE (PC_REGNUM)] = corestr.c_regs.r_pc;
	bcopy (corestr.c_fpstatus.fps_regs,
	       &registers[REGISTER_BYTE (FP0_REGNUM)],
	       sizeof corestr.c_fpstatus.fps_regs);
	bcopy (&corestr.c_fpstatus.fps_control,
	       &registers[REGISTER_BYTE (FPC_REGNUM)],
	       sizeof corestr.c_fpstatus - sizeof corestr.c_fpstatus.fps_regs);

	bcopy (&corestr.c_aouthdr, &core_aouthdr, sizeof (struct exec));

	printf ("Core file is from \"%s\".\n", corestr.c_cmdname);
      }
#else /* not NEW_SUN_CORE */
      {
	struct user u;
	int reg_offset;

	/* 4.2bsd-style core dump */
	val = myread (corechan, &u, sizeof u);
	if (val < 0)
	  perror_with_name (filename);
	data_start = exec_data_start;
	data_end = data_start + NBPG * u.u_dsize;
	stack_start = stack_end - NBPG * u.u_ssize;
	data_offset = NBPG * UPAGES;
	stack_offset = NBPG * (UPAGES + u.u_dsize);
	reg_offset = (int) u.u_ar0 - KERNEL_U_ADDR;

	/* Read the register values out of the core file and store
	   them where `read_register' will find them.  */

	{
	  register int regno;

	  for (regno = 0; regno < NUM_REGS; regno++)
	    {
	      REGISTER_TYPE buf;

	      val = lseek (corechan, register_addr (regno, reg_offset), 0);
	      if (val < 0)
		perror_with_name (filename);

	      val = myread (corechan, &buf, sizeof buf);
	      if (val < 0)
		perror_with_name (filename);
	      supply_register (regno, buf);
	    }
	}
	/* I don't know where to find this info.
	   So, for now, mark it as not available.  */
	core_aouthdr.a_magic = 0;
      }
#endif /* not NEW_SUN_CORE */
      if (filename[0] == '/')
	corefile = savestring (filename, strlen (filename));
      else
	{
	  char dirname[MAXPATHLEN];

	  getwd (dirname);
	  corefile = concat (dirname, "/", filename);
	}

      set_current_frame (read_register (FP_REGNUM));
      select_frame (get_current_frame (), 0);
      validate_files ();
    }
  else if (from_tty)
    printf ("No core file now.\n");
}

exec_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;

  if (execfile)
    free (execfile);
  execfile = 0;
  data_start = 0;
  data_end -= exec_data_start;
  text_start = 0;
  text_end = 0;
  exec_data_start = 0;
  exec_data_end = 0;
  if (execchan >= 0)
    close (execchan);
  execchan = -1;
  if (filename)
    {
      execchan = openp (getenv ("PATH"), 1, filename, O_RDONLY, 0,
			&execfile);
      if (execchan < 0)
	perror_with_name (filename);

      val = myread (execchan, &exec_aouthdr, sizeof exec_aouthdr);
      if (val < 0)
	perror_with_name (filename);

      text_start = N_TXTADDR (exec_aouthdr);
      text_end = text_start + exec_aouthdr.a_text;
      text_offset = N_TXTOFF (exec_aouthdr);
      exec_data_start = N_DATADDR (exec_aouthdr);
      exec_data_end = exec_data_start + exec_aouthdr.a_data;
      exec_data_offset = N_TXTOFF (exec_aouthdr) + exec_aouthdr.a_text;
      data_start = exec_data_start;
      data_end += exec_data_start;
      validate_files ();
    }
  else if (from_tty)
    printf ("No exec file now.\n");
}

/* If we have both a core file and an exec file,
   print a warning if they don't go together.
   This should really check that the core file came
   from that exec file, but I don't know how to do it.  */

static void
validate_files ()
{
  struct stat st_exec;

  if (execfile != 0)
    {
      fstat (execchan, &st_exec);
      exec_mtime = st_exec.st_mtime;

      if (corefile != 0)
	{
	  struct stat st_core;

	  fstat (corechan, &st_core);
	  if (core_aouthdr.a_magic != 0
	      && bcmp (&core_aouthdr, &exec_aouthdr, sizeof core_aouthdr))
	    printf ("Warning: core file does not match specified executable file.\n");
	  else if (st_exec.st_mtime > st_core.st_mtime)
	    printf ("Warning: exec file is newer than core file.\n");
	}
    }
}

char *
get_exec_file ()
{
  if (execfile == 0)
    error ("No executable file specified.\n\
Use the \"exec-file\" and \"symbol-file\" commands.");
  return execfile;
}

int
have_core_file_p ()
{
  return corefile != 0;
}

static void
files_info ()
{
  char *symfile;
  extern char *get_sym_file ();

  if (execfile)
    printf ("Executable file \"%s\".\n", execfile);
  else
    printf ("No executable file\n");
  if (corefile == 0)
    printf ("No core dump file\n");
  else
    printf ("Core dump file \"%s\".\n", corefile);

  if (have_inferior_p ())
    printf ("Using the running image of the program, rather than these files.\n");

  symfile = get_sym_file ();
  if (symfile != 0)
    printf ("Symbols loaded from \"%s\".\n", symfile);

  if (! have_inferior_p ())
    {
      if (execfile)
	{
	  printf ("Text segment from 0x%x to 0x%x.\n",
		  text_start, text_end);
	}
      if (corefile)
	{
	  printf ("Data segment from 0x%x to 0x%x.\nStack segment from 0x%x to 0x%x.\n",
		  data_start, data_end, stack_start, stack_end);
	}
      else
	{
	  printf ("Data segment in executable from 0x%x to 0x%x.\n",
		  exec_data_start, exec_data_end);
	}
    }
}

/* Read "memory data" from core file and/or executable file */

read_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  if (have_inferior_p ())
    read_inferior_memory (memaddr, myaddr, len);
  else
    xfer_core_file (memaddr, myaddr, len, 0);
}

/* Write LEN bytes of data starting at address MYADDR
   into debugged program memory at address MEMADDR.
   Returns zero if successful, 1 if failed because inferior is shared.  */

int
write_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  if (have_inferior_p ())
    return write_inferior_memory (memaddr, myaddr, len);
  else
    error ("Can write memory only when program being debugged is running.");
}

xfer_core_file (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  register int i;
  register int val;
  int xferchan;
  char **xferfile;
  int fileptr;

  while (len > 0)
    {
      xferfile = 0;
      xferchan = 0;

      /* Determine which file the next bunch of addresses reside in,
	 and where in the file.  Set the file's read/write pointer
	 to point at the proper place for the desired address
	 and set xferfile and xferchan for the correct file.
	 If desired address is nonexistent, leave them zero.
	 i is set to the number of bytes that can be handled
	 along with the next address.  */

      if (memaddr < text_start)
	{
	  i = min (len, text_start - memaddr);
	}
      else if (memaddr >= text_end && memaddr < data_start)
	{
	  i = min (len, data_start - memaddr);
	}
      else if (memaddr >= (corechan >= 0 ? data_end : exec_data_end)
	       && memaddr < stack_start)
	{
	  i = min (len, stack_start - memaddr);
	}
      else if (memaddr >= stack_end && stack_end != 0)
	{
	  i = min (len, - memaddr);
	}
      /* Note that if there is no core file
	 data_start and data_end are equal.  */
      else if (memaddr >= data_start && memaddr < data_end)
	{
	  i = min (len, data_end - memaddr);
	  fileptr = memaddr - data_start + data_offset;
	  xferfile = &corefile;
	  xferchan = corechan;
	}
      /* Note that if there is no core file
	 stack_start and stack_end are equal.  */
      else if (memaddr >= stack_start && memaddr < stack_end)
	{
	  i = min (len, stack_end - memaddr);
	  fileptr = memaddr - stack_start + stack_offset;
	  xferfile = &corefile;
	  xferchan = corechan;
	}
      else if (corechan < 0
	       && memaddr >= exec_data_start && memaddr < exec_data_end)
	{
	  i = min (len, exec_data_end - memaddr);
	  fileptr = memaddr - exec_data_start + exec_data_offset;
	  xferfile = &execfile;
	  xferchan = execchan;
	}
      else if (memaddr >= text_start && memaddr < text_end)
	{
	  i = min (len, text_end - memaddr);
	  fileptr = memaddr - text_start + text_offset;
	  xferfile = &execfile;
	  xferchan = execchan;
	}

      /* Now we know which file to use.
	 Set up its pointer and transfer the data.  */
      if (xferfile)
	{
	  if (*xferfile == 0)
	    if (xferfile == &execfile)
	      error ("No program file to examine.");
	    else
	      error ("No core dump file or running program to examine.");
	  val = lseek (xferchan, fileptr, 0);
	  if (val < 0)
	    perror_with_name (*xferfile);
	  val = myread (xferchan, myaddr, i);
	  if (val < 0)
	    perror_with_name (*xferfile);
	}
      /* If this address is for nonexistent memory,
	 read zeros if reading, or do nothing if writing.  */
      else
	bzero (myaddr, i);

      memaddr += i;
      myaddr += i;
      len -= i;
    }
}

/* My replacement for the read system call.
   Takes same args as read, plus filename to use in error reports.
   Used like read but keeps going if read returns too soon.  */

myread (desc, addr, len, filename)
     int desc;
     char *addr;
     int len;
     char *filename;
{
  register int val;
  int orglen = len;

  while (len > 0)
    {
      val = read (desc, addr, len);
      if (val < 0)
	return val;
      if (val == 0)
	return orglen - len;
      len -= val;
      addr += val;
    }
}

#ifndef NEW_SUN_CORE

/* Return the address in the core dump or inferior of register REGNO.
   BLOCKEND is the address of the end of the user structure.  */

unsigned int
register_addr (regno, blockend)
     int regno;
     int blockend;
{
  int addr;

  if (regno < 0 || regno >= NUM_REGS)
    error ("Invalid register number %d.", regno);

  REGISTER_U_ADDR (addr, blockend, regno);

  return addr;
}

#endif /* not NEW_SUN_CORE */

static
initialize ()
{
  corechan = -1;
  execchan = -1;
  corefile = 0;
  execfile = 0;

  text_start = 0;
  text_end = 0;
  data_start = 0;
  data_end = 0;
  exec_data_start = 0;
  exec_data_end = 0;
  stack_start = STACK_END_ADDR;
  stack_end = STACK_END_ADDR;

  add_com ("core-file", class_files, core_file_command,
	   "Use FILE as core dump for examining memory and registers.\n\
No arg means have no core file.");
  add_com ("exec-file", class_files, exec_file_command,
	   "Use FILE as program for getting contents of pure memory.\n\
If FILE cannot be found as specified, your execution directory path\n\
is searched for a command of that name.\n\
No arg means have no executable file.");
  add_info ("files", files_info, "Names of files being debugged.");
}

END_FILE
