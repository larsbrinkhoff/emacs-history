/* Asynchronous subprocess control for GNU Emacs.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
document "GNU Emacs copying permission notice".   An exact copy
of the document is supposed to have been given to you along with
GNU Emacs so that you can know how you may redistribute it all.
It should be in a file named COPYING.  Among other things, the
copyright notice and this notice must be preserved on all copies.  */

#include "config.h"

#ifdef subprocesses
/* The entire file is within this conditional */

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <sgtty.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#undef NULL
#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "process.h"

extern errno;
extern sys_nerr;
extern char *sys_errlist[];
extern char *sys_siglist[];

#ifdef vipc

#include "vipc.h"
extern int comm_server;
extern int net_listen_address;
#endif vipc


#ifdef SKTPAIR
#include <sys/types.h>
#include <sys/socket.h>
#endif SKTPAIR

int  child_changed;		/* Flag when a child process has ceased
				   to be */

/* Mask of bits indicating the descriptors that we wait for input on */

int input_wait_mask;

int delete_exited_processes;

#define MAXDESC 32

/* Indexed by descriptor, gives the process (if any) for that descriptor */
Lisp_Object chan_process[MAXDESC];

/* Alist of elements (NAME . PROCESS) */
Lisp_Object Vprocess_alist;

Lisp_Object Qprocessp;

Lisp_Object get_process ();

extern int unread_command_char;

/* Open an available pty, putting descriptor in *ptyv,
  and return the file name of the pty.  Return 0 if none available.  */

char *
pty (ptyv)
     int *ptyv;
{
  struct stat stb;
  static char name[24];
  int on = 1;
  register c, i;

  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
      {
	sprintf (name, "/dev/pty%c%x", c, i);
	if (stat( name, &stb ) < 0)
	  return 0;

	*ptyv = open (name, O_RDWR | O_NDELAY, 0);
	if (*ptyv >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
	    int x;
            sprintf (name, "/dev/tty%c%x", c, i);
	    x = open (name, O_RDWR | O_NDELAY, 0);
	    if (x < 0)
	      {
		close (*ptyv);
		continue;
	      }
	    close(x);
	    /*
		* If the following statement is included,
		* then a 0 length record is EOT, but no other
		* control characters can be sent down the pty
		* (e.g., ^S/^Q, ^O, etc.).  If it is not
		* included, then sending ^D down the pty-pipe
		* makes a pretty good EOF.
		*/
/*	      ioctl( *ptyv, TIOCREMOTE, &on );	/* for EOT */
/* this is said to be unecessary, and to be harmful in 4.3.  */
/*	    ioctl (*ptyv, FIONBIO, &on);  */
	    return name;
	  }
	}
}

Lisp_Object
make_process (name)
     Lisp_Object name;
{
  Lisp_Object val, tem, name1;
  register struct Lisp_Process *p;
  char suffix[10];
  register int i;

  val = Fmake_vector (make_number ((sizeof (struct Lisp_Process)
				    - sizeof (int) - sizeof (struct Lisp_Vector *))
				   / sizeof (Lisp_Object)),
		      Qnil);
  XSETTYPE (val, Lisp_Process);

  p = XPROCESS (val);
  XFASTINT (p->infd) = 0;
  XFASTINT (p->outfd) = 0;
  XFASTINT (p->pid) = 0;
  XFASTINT (p->flags) = 0;
  XFASTINT (p->reason) = 0;
  p->mark = Fmake_marker ();

  /* If name is already in use, modify it until it is unused.  */

  name1 = name;
  for (i = 1; ; i++)
    {
      tem = Fget_process (name1);
      if (NULL (tem)) break;
      sprintf (suffix, "<%d>", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;
  Vprocess_alist = Fcons (Fcons (name, val), Vprocess_alist);
  return val;
}

remove_process (proc)
     Lisp_Object proc;
{
  Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);
  Fset_marker (XPROCESS (proc)->mark, Qnil, Qnil);

  deactivate_process (proc);
}

DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
  "Return t if OBJECT is a process.")
  (obj)
     Lisp_Object obj;
{
  return XTYPE (obj) == Lisp_Process ? Qt : Qnil;
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
  "Return the process named NAME, or nil if there is none.")
  (name)
     Lisp_Object name;
{
  if (XTYPE (name) == Lisp_Process)
    return name;
  CHECK_STRING (name, 0);
  return Fcdr (Fassoc (name, Vprocess_alist));
}

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
  "Return the (or, a) process associated with BUFFER.\n\
BUFFER may be a buffer or the name of one.")
  (name)
     Lisp_Object name;
{
  Lisp_Object buf, tail, proc;

  if (NULL (name)) return Qnil;
  buf = Fget_buffer (name);
  if (NULL (buf)) return Qnil;

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      if (XTYPE (proc) == Lisp_Process && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
  return Qnil;
}

/* This is how commands for the user decode process arguments */

Lisp_Object
get_process (name)
     Lisp_Object name;
{
  Lisp_Object proc;
  if (NULL (name))
    proc = Fget_buffer_process (Fcurrent_buffer ());
  else
    {
      proc = Fget_process (name);
      if (NULL (proc))
	proc = Fget_buffer_process (Fget_buffer (name));
    }

  if (!NULL (proc))
    return proc;

  if (NULL (name))
    error ("Current buffer has no process");
  else
    error ("Process %s does not exist", XSTRING (name)->data);
}

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1,
  "sDelete process: ",
  "Delete PROCESS: kill it and forget about it immediately.\n\
PROCESS may be a process or the name of one, or a buffer name.")
  (proc)
     Lisp_Object proc;
{
  proc = get_process (proc);
  if (XFASTINT (XPROCESS (proc)->infd))
    Fkill_process (proc, Qnil);
  remove_process (proc);
  return Qnil;
}

DEFUN ("process-status", Fprocess_status, Sprocess_status, 1, 1, 0,
  "Return the status of PROCESS: a symbol, one of these:\n\
run  -- for a process that is running.\n\
stop -- for a process stopped but continuable.\n\
exit -- for a process that has exited.\n\
signal -- for a process that has got a fatal signal.\n\
command -- for a command channel opened to Emacs by another process.\n\
external -- for an i/o channel opened to Emacs by another process.\n\
nil -- if arg is a process name and no such process exists.")
  (proc)
     Lisp_Object proc;
{
  register struct Lisp_Process *p;
  proc = Fget_process (proc);
  if (NULL (proc))
    return proc;
  p = XPROCESS (proc);

  switch (XFASTINT (p->flags) & PROC_STATUS)
    {
    case RUNNING:
      if (!NULL (p->childp))
	return intern ("run");
      else if (!NULL (p->command_channel_p))
	return intern ("command");
      return intern ("external");

    case EXITED:
      return intern ("exit");

    case SIGNALED:
      return intern ("signal");

    case STOPPED:
      return intern ("stop");
    }
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
  "Return the process id of PROCESS.\n\
This is the pid of the Unix process which PROCESS uses or talks to.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->pid;
}

DEFUN ("process-name", Fprocess_name, Sprocess_name, 1, 1, 0,
  "Return the name of PROCESS, as a string.\n\
This is the name of the program invoked in PROCESS,\n\
possibly modified to make it unique among process names.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->name;
}

DEFUN ("process-command", Fprocess_command, Sprocess_command, 1, 1, 0,
  "Return the command that was executed to start PROCESS.\n\
This is a list of strings, the first string being the program executed\n\
and the rest of the strings being the arguments given to it.\n\
For a non-child channel, this is nil.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->command;
}

DEFUN ("set-process-buffer", Fset_process_buffer, Sset_process_buffer,
  2, 2, 0,
  "Set buffer associated with PROCESS to BUFFER.")
  (proc, buffer)
     Lisp_Object proc, buffer;
{
  CHECK_PROCESS (proc, 0);
  CHECK_BUFFER (buffer, 1);
  XPROCESS (proc)->buffer = buffer;
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, Sprocess_buffer,
  1, 1, 0,
  "Return the buffer PROCESS is associated with.\n\
Output from PROCESS is inserted in this buffer\n\
unless PROCESS has a filter.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, Sprocess_mark,
  1, 1, 0,
  "Return the marker for the end of the last output from PROCESS.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->mark;
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
  2, 2, 0,
  "Give PROCESS the filter function FILTER; nil means no filter.\n\
When a process has a filter, each time it does output\n\
the entire string of output is passed to the filter.\n\
The filter gets two arguments: the process and the string of output.\n\
If the process has a filter, its buffer is not used for output.")
  (proc, filter)
     Lisp_Object proc, filter;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->filter = filter;
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, Sprocess_filter,
  1, 1, 0,
  "Returns the filter function of PROCESS; nil if none.\n\
See set-process-filter for more info on filter functions.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->filter;
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, Sset_process_sentinel,
  2, 2, 0,
  "Give PROCESS the sentinel SENTINEL; nil for none.\n\
The sentinel is called as a function when the process changes state.\n\
It gets two arguments: the process, and a string describing the change.")
  (proc, sentinel)
     Lisp_Object proc, sentinel;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->sentinel = sentinel;
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, Sprocess_sentinel,
  1, 1, 0,
  "Return the sentinel of PROCESS; nil if none.\n\
See set-process-sentinel for more info on sentinels.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->sentinel;
}

DEFUN ("process-kill-without-query", Fprocess_kill_without_query,
  Sprocess_kill_without_query, 1, 1, 0,
  "Say no query needed if this process is running when Emacs is exited.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->kill_without_query = Qt;
  return Qt;
}

Lisp_Object
list_processes_1 ()
{
  Lisp_Object tail, proc, minspace, tem, tem1;
  register struct buffer *old = bf_cur;
  register struct Lisp_Process *p;
  register int state;

  XFASTINT (minspace) = 1;

  SetBfp (XBUFFER (Vstandard_output));
  Fbuffer_flush_undo (Vstandard_output);

  bf_cur->truncate_lines = Qt;

  write_string ("\
Proc         Status   Buffer         Command\n\
----         ------   ------         -------\n", -1);

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);
      if (NULL (p->childp))
	continue;

      Finsert (1, &p->name);
      Findent_to (make_number (13), minspace, Qnil);

      state = XFASTINT (p->flags) & PROC_STATUS;
      switch (state)
	{
	case RUNNING:
	  write_string ("Run", -1);
	  break;

	case STOPPED:
	  write_string ("Stop", -1);
	  break;

	case EXITED:
	  write_string ("Exit", -1);
	  if (XFASTINT (p->reason))
	    {
	      sprintf (tem, " %d", XFASTINT (p->reason));
	      write_string (tem, -1);
	    }
	  remove_process (proc);
	  break;

	case SIGNALED:
	  if (XFASTINT (p->reason) < NSIG)
	    write_string (sys_siglist [XFASTINT (p->reason)], -1);
	  else
	    write_string ("Signal", -1);
	  remove_process (proc);
	}

      Findent_to (make_number (22), minspace, Qnil);
      if (!NULL (p->buffer))
	Finsert (1, &XBUFFER (p->buffer)->name);

      Findent_to (make_number (37), minspace, Qnil);

      tem = p->command;
      while (1)
	{
	  tem1 = Fcar (tem);
	  Finsert (1, &tem1);
	  tem = Fcdr (tem);
	  if (NULL (tem))
	    break;
	  InsStr (" ");
	}

      InsStr ("\n");
    }

  SetBfp (old);
  return Qnil;
}

DEFUN ("list-processes", Flist_processes, Slist_processes, 0, 0, "",
  "Display a list of all processes and comm channels.\n\
First two columns are S for subprocess, C for command channel,\n\
or X for external I/O channel.\n\
\(Any processes listed as Exited or Signaled are actually eliminated\n\
after the listing is made.)")
  ()
{
  internal_with_output_to_temp_buffer ("*Process List*",
				       list_processes_1, Qnil);
  return Qnil;
}

DEFUN ("start-process", Fstart_process, Sstart_process, 3, MANY, 0,
  "Start a program in a subprocess.  Return the process object for it.\n\
First arg is name for process.  It is modified if nec to make it unique.\n\
Second arg is buffer to associate with the process (or buffer name).\n\
 Process output goes at end of that buffer, unless you specify\n\
 an output stream or filter function to handle the output.\n\
Third arg is program file name.  It is searched for as in the shell.\n\
Remaining arguments are strings to give program as arguments.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object buffer, name, program, proc, tem;
  register unsigned char **new_argv;
  register unsigned char *progname;
  register int i;

  name = args[0];
  CHECK_STRING (name, 0);

  buffer = args[1];
  program = args[2];

  CHECK_STRING (program, 2);

  new_argv = (unsigned char **) alloca ((nargs - 1) * sizeof (char *));

  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem, i);
      new_argv[i - 2] = XSTRING (tem)->data;
    }
  new_argv[i - 2] = 0;
  new_argv[0] = XSTRING (program)->data;

  /* If program file name is not absolute, search our path for it */
  if (new_argv[0][0] != '/')
    {
      tem = Qnil;
      openp (Vexec_path, program, "", &tem, 1);
      if (NULL (tem))
	report_file_error ("Searching for program", Fcons (program, Qnil));
      new_argv[0] = XSTRING (tem)->data;
    }

  buffer = Fget_buffer_create (buffer);
  proc = make_process (name);

  XPROCESS (proc)->childp = Qt;
  XPROCESS (proc)->command_channel_p = Qnil;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

  create_process (proc, new_argv);

  return proc;
}

create_process_1 ()
{
}

create_process (process, new_argv)
     Lisp_Object process;
     char **new_argv;
{
  int pid, inchannel, outchannel, forkin, forkout;
  register int i;
  int sv[2];

#ifndef SKTPAIR
  char	*ptyname;

  ptyname = pty (&inchannel);
  outchannel = inchannel;
  if (ptyname)
    {
      forkout = forkin = open (ptyname, O_RDWR, 0);
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
    }
  else
    {
      pipe (sv);
      inchannel = sv[0];
      forkout = sv[1];
      pipe (sv);
      outchannel = sv[1];
      forkin = sv[0];
    }
#else
  if (socketpair (AF_UNIX, SOCK_STREAM, 0, sv) < 0)
    error ("Can't create socketpair");
  ptyname = 0;
  outchannel = inchannel = sv[0];
  forkout = forkin = sv[1];
#endif SKTPAIR

  ioctl (inchannel, FIOCLEX, 0);
  ioctl (outchannel, FIOCLEX, 0);

  chan_process[inchannel] = process;
  XFASTINT (XPROCESS (process)->infd) = inchannel;
  XFASTINT (XPROCESS (process)->outfd) = outchannel;
  XFASTINT (XPROCESS (process)->flags) = RUNNING;

  input_wait_mask |= ChannelMask (inchannel);

  /* Delay interrupts until we have a chancel to store
     the new fork's pid in its process structure */
  sigsetmask (1 << (SIGCHLD - 1));

  pid = vfork ();
  if (pid == 0)
    {
      int xforkin = forkin;
      int xforkout = forkout;
      /* In 4.3BSD, the TIOCSPGRP bug has been fixed, and now you
	 can do TIOCSPGRP only to the process's controlling tty.
	 We must make the pty terminal the controlling tty of the child.  */
      if (ptyname)
	{
	  /* I wonder: would just ioctl (0, TIOCNOTTY, 0) work here? 
	     I can't test it since I don't have 4.3.  */
	  int j = open ("/dev/tty", O_RDWR, 0);
	  ioctl (j, TIOCNOTTY, 0);
          close (j);

	  /* I wonder if close (open (ptyname, ...)) would work?  */
	  close (xforkin);
          xforkout = xforkin = open (ptyname, O_RDWR, 0);

          if (xforkin < 0)
	    abort ();
        }
      child_setup (xforkin, xforkout, xforkout, new_argv);
    }

  /* If the subfork execv fails, and it exits,
     this close hangs.  I don't know why.
     So have an interrupt jar it loose.  */
  signal (SIGALRM, create_process_1);
  alarm (1);
  close (forkin);
  alarm (0);
  if (forkin != forkout)
    close (forkout);

  if (pid < 0)
    {
      remove_process (process);
      report_file_error ("Doing vfork", Qnil);
    }

  XFASTINT (XPROCESS (process)->pid) = pid;

  sigsetmask (0);
}

deactivate_process (proc)
     Lisp_Object proc;
{
  register int inchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

  inchannel = XFASTINT (p->infd);

  if (inchannel)
    {
      ioctl (inchannel, TIOCFLUSH, 0);	/* flush any pending output */
      close (inchannel);
      if (XFASTINT (p->outfd) != inchannel)
	close (XFASTINT (p->outfd));

      XFASTINT (p->infd) = 0;
      XFASTINT (p->outfd) = 0;
      chan_process[inchannel] = Qnil;
      input_wait_mask &= ~ChannelMask (inchannel);
    }
}

/* Read and dispose of subprocess output
 while waiting for timeout to elapse and/or keyboard input to be available.

 time_limit is the timeout in seconds, or zero for no limit.

 read_kbd is nonzero to return when input is available.
 Negative means caller will actually read the input.

 do_display means redisplay should be done to show
 subprocess output that arrives.  */

wait_reading_process_input (time_limit, read_kbd, do_display)
     int time_limit, read_kbd, do_display;
{
  register int channel, nfds, m;
  int Available = 0;
  int Exception;
  char buf[1024];
  int count;
  Lisp_Object proc;
  struct timeval timeout, end_time, garbage;
  int Atemp;
  int lose;
  int fcntl_flags;
  extern kbd_count;

  /* Since we may need to wait several times,
     compute the absolute time to return at.  */
  if (time_limit)
    {
      gettimeofday (&end_time, &garbage);
      end_time.tv_sec += time_limit;
    }

  while (1)
    {
      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	QUIT;

      /* If status of something has changed, and no input is available,
	 notify the user of the change right away */
      if (child_changed && do_display)
	{
	  Atemp = input_wait_mask;
	  timeout.tv_sec=0; timeout.tv_usec=0;
	  if (select (MAXDESC, &Atemp, 0, 0, &timeout) <= 0)
	    change_msgs();
	}

      /* Compute time from now till when time limit is up */
      /* Exit if already run out */
      if (time_limit)
	{
	  gettimeofday (&timeout, &garbage);
	  timeout.tv_sec = end_time.tv_sec - timeout.tv_sec;
	  timeout.tv_usec = end_time.tv_usec - timeout.tv_usec;
	  if (timeout.tv_usec < 0)
	    timeout.tv_usec += 1000000,
	    timeout.tv_sec--;
	  if (timeout.tv_sec < 0)
	    break;
	}
      else
	{
	  /* If no real timeout, loop sleeping with a bug timeout
	     so that input interrupt can wake us up by zeroing it  */
	  timeout.tv_sec = 100;
	  timeout.tv_usec = 0;
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* Wait till there is something to do */

      Available = Exception = input_wait_mask;
      if (!read_kbd)
	Available &= ~1;

      if (read_kbd && kbd_count)
	nfds = 0;
      else
	nfds = select (MAXDESC, &Available, 0, &Exception, &timeout);

      /* Make C-g and alarm signals set flags again */
      clear_waiting_for_input ();

      if (time_limit && nfds == 0)	/* timeout elapsed */
	break;
      if (nfds < 0)
	{
	  if (errno == EINTR)
	    Available = 0;
	  else
	    error("select error: %s", sys_errlist[errno]);
	}

      /* Check for keyboard input */
      /* If there is any, return immediately
	 to give it higher priority than subprocesses */

      if (read_kbd && (kbd_count || !NULL (Vquit_flag)))
	break;

      if (read_kbd && (Available & ChannelMask (0)))
	break;

#ifdef vipc
      /* Check for connection from other process */

      if (Available & ChannelMask (comm_server))
	{
	  Available &= ~(ChannelMask (comm_server));
	  create_commchan ();
	}
#endif vipc

      /* Check for data from a process or a command channel */

      for (channel = 3; Available && channel < MAXDESC; channel++)
	{
	  m = ChannelMask (channel);
	  if (m & Available)
	    {
	      Available &= ~m;
	      proc = chan_process[channel];
	      if (NULL (proc))
		continue;

#ifdef vipc
	      /* It's a command channel */
	      if (!NULL (XPROCESS (proc)->command_channel_p))
		{
		  ProcessCommChan (channel, proc);
		  if (NULL (XPROCESS (proc)->command_channel_p))
		    {
		      /* It has ceased to be a command channel! */
		      int bytes_available;
		      ioctl (channel, FIONREAD, &bytes_available);
		      if (bytes_available)
			Available |= m;
		    }
		  continue;
		}
#endif vipc

	      count = read (channel, buf, sizeof buf);
	      if (count > 0)
		{
		  handle_process_output (proc, buf, count);
		  if (do_display)
		    DoDsp (1);
		}
	      else
		{
/*
 * With pty:s, when the parent process of a pty exits we are notified,
 * just as we would be with any of our other children.  After the process
 * exits, select() will indicate that we can read the channel.  When we
 * do this, read() returns 0.  Upon receiving this, we close the channel.
 *
 * For external channels, when the peer closes the connection, select()
 * will indicate that we can read the channel.  When we do this, read()
 * returns -1 with errno = ECONNRESET.  Since we never get notified of
 * this via wait3(), we must explictly mark the process as having exited.
 */

		  XFASTINT (XPROCESS (proc)->flags) = EXITED | CHANGED;
		  XFASTINT (XPROCESS (proc)->reason) = 0;
		  child_changed++;
		  deactivate_process (proc);
		}
	    }
	} /* end for */
    } /* end while */
}

/*
 * Output has been recieved from a process on "chan" and should be stuffed in
 * the correct buffer.
 */
handle_process_output (proc, chars, howmany)
     Lisp_Object proc;
     char *chars;
     int howmany;
{
  Lisp_Object outstream;
  register struct buffer *old = bf_cur;
  register struct Lisp_Process *p = XPROCESS (proc);
  register int odot;

  outstream = p->filter;
  if (!NULL (outstream))
    {
      call2 (outstream, proc, make_string (chars, howmany));
      return 1;
    }

  if (!NULL (p->buffer))
    {
      Fset_buffer (p->buffer);
      odot = dot;
      SetDot (NumCharacters + 1);
      if (odot == dot) odot = -1;
      InsCStr (chars, howmany);
      Fset_marker (p->mark, make_number (dot), p->buffer);
      RedoModes++;

      if (odot >= 0)
	SetDot (odot);
      SetBfp (old);
    }
  else return 0;

  /* Old feature was, delete early chars in chunks if
    buffer gets bigger that ProcessBufferSize.
    This feature is flushed */

  return 1;
}

/* Sending data to subprocess */

jmp_buf send_process_frame;

send_process_trap ()
{
  longjmp (send_process_frame, 1);
}

send_process_1 (proc, buf, len)
     Lisp_Object proc;
     char *buf;
     int len;
{
  /* Don't use register vars; longjmp can lose them.  */
  int rv;
  unsigned char *procname = XSTRING (XPROCESS (proc)->name)->data;

  if ((XFASTINT (XPROCESS (proc)->flags) & PROC_STATUS) != RUNNING)
    error ("Process %s not running", procname);

  signal (SIGPIPE, send_process_trap);

  if (!setjmp (send_process_frame))
    rv = write (XFASTINT (XPROCESS (proc)->outfd), buf, len);
  else
    {
      signal (SIGPIPE, 0);
      XFASTINT (XPROCESS (proc)->flags) =  EXITED | CHANGED;
      deactivate_process (proc);
      error ("SIGPIPE raised on socket %s; closing",
	     procname);
    }

  signal (SIGPIPE, 0);

  if (rv != len)
    error ("Error writing to process %s", procname);
}

/*** Is it really safe for this to get an error ?  */

send_process (proc, buf, count)
     Lisp_Object proc;
     char *buf;
     int count;
{
  struct { int checkword, type, datalen; } header;

#ifdef vipc
  if (!NULL (XPROCESS (proc)->command_channel_p))
    {
      checkword = UNIQUE_FROB;
      type = VIPC_MESG;
      datalen = count;
      send_process_1 (proc, &header, sizeof header);
    }
#endif vipc
  send_process_1 (proc, buf, count);
}

DEFUN ("send-region", Fsend_region, Ssend_region, 3, 3,
  "sSend region (to process): \nr",
  "Send current contents of region as input to PROCESS.\n\
PROCESS may be a process name.\n\
Called from program, takes three arguments, PROCESS, START and END.")
  (process, start, end)
     Lisp_Object process, start, end;
{
  Lisp_Object proc;
  proc = get_process (process);
  validate_region (&start, &end);

  if (XINT (start) < bf_s1 && XINT (end) >= bf_s1)
    GapTo (start);

  send_process (proc, &CharAt (XINT (start)), XINT (end) - XINT (start));

  return Qnil;
}

DEFUN ("send-string", Fsend_string, Ssend_string, 2, 2,
  "sSend string to process: \nSend string: ",
  "Send PROCESS the contents of STRING as input.\n\
PROCESS may be a process name.")
  (process, string)
     Lisp_Object process, string;
{
  Lisp_Object proc;
  CHECK_STRING (string, 1);
  proc = get_process (process);
  send_process (proc, XSTRING (string)->data, XSTRING (string)->size);
  return Qnil;
}

/* send a signal to `process' */

sig_process (process, signal, current_group, nomsg)
     Lisp_Object process;
     int signal;
     int current_group;
     int nomsg;
{
  Lisp_Object proc;
  register struct Lisp_Process *p;
  int gid;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (NULL (p->childp))
    error ("Process %s is not a subprocess",
	   XSTRING (p->name)->data);
  if (!XFASTINT (p->infd))
    error ("Process %s is not active",
	   XSTRING (p->name)->data);

  if (current_group)
    ioctl (XFASTINT (p->infd), TIOCGPGRP, &gid);
  else
    gid = XFASTINT (p->pid);

  switch (signal)
    {
    case SIGCONT:
      XFASTINT (p->flags) = RUNNING | CHANGED;
      child_changed++;
      break;

    case SIGINT:
    case SIGQUIT:
    case SIGKILL:
      ioctl (XFASTINT (p->infd), TIOCFLUSH, 0 );
      break;
    }
  killpg (gid, signal);

  /* Put notices in buffers now, since it is safe now.
     Because of this, we know that a process we have just killed
     will never need to use its buffer again.  */
  if (!nomsg)
    change_msgs ();
}

DEFUN ("interrupt-process", Finterrupt_process, Sinterrupt_process, 0, 2,
  "sInterrupt process: ",
  "Interrupt process PROCESS.  May be process or name of one.\n\
Nil or no arg means current buffer's process.\n\
Second arg CURRENT-GROUP non-nil means send signal to\n\
the current process-group of the process's controlling terminal\n\
rather than to the process's own process group.\n\
If the process is a shell, this means interrupt current subjob\n\
rather than the shell.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGINT, current_group, 0);
  return process;
}

DEFUN ("kill-process", Fkill_process, Skill_process, 0, 2,
  "sKill process: ",
  "Kill process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGKILL, current_group, 0);
  return process;
}

DEFUN ("quit-process", Fquit_process, Squit_process, 0, 2,
  "sQuit process: ",
  "Send QUIT signal to process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGQUIT, current_group, 0);
  return process;
}

DEFUN ("stop-process", Fstop_process, Sstop_process, 0, 2,
  "sStop process: ",
  "Stop process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGTSTP, current_group, 0);
  return process;
}

DEFUN ("continue-process", Fcontinue_process, Scontinue_process, 0, 2,
  "sContinue process: ",
  "Continue process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGCONT, current_group, 0);
  return process;
}

DEFUN ("process-send-eof", Fprocess_send_eof, Sprocess_send_eof, 0, 1,
  "sProcess (to send eof to): ",
  "Make PROCESS see end-of-file in its input.\n\
Eof comes after any text already sent to it.\n\
Nil or no arg means current buffer's process.")
  (process)
     Lisp_Object process;
{
  Lisp_Object proc;
  register struct Lisp_Process *p;
  proc = get_process (process);
  send_process (proc, "\004", 1);
  return process;
}

/* Kill all processes associated with `buffer'.
 If `buffer' is nil, kill all processes  */

kill_buffer_processes (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tail, proc;

  for (tail = Vprocess_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      proc = XCONS (XCONS (tail)->car)->cdr;
      if (XGCTYPE (proc) == Lisp_Process
	  && (NULL (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	sig_process (proc, SIGKILL, 0, 1);
    }
}

count_active_processes (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tail, proc;
  register int count = 0;

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));

      if ((1 << (XFASTINT (XPROCESS (proc)->flags) & PROC_STATUS))
	  && ((1 << RUNNING) | (1 << STOPPED))
	  && NULL (XPROCESS (proc)->kill_without_query))
	count++;
    }

  return count;
}

/* On receipt of a signal that a child status has changed,
 loop asking about children with changed statuses until
 the system says there are no more.
   All we do is change the flags components;
 we do not run sentinels or print notifications.
 That is saved for the next time keyboard input is done,
 in order to avoid timing errors.  */

/** WARNING: this can be called during garbage collection.
 Therefore, it must not be fooled by the presence of mark bits in
 Lisp objects.  */

#ifdef SIGCHLD
child_sig ()
{
  register int pid;
  union wait w;
  Lisp_Object tail, proc;
  register struct Lisp_Process *p;

loop: 

  pid = wait3 (&w, WUNTRACED | WNOHANG, 0);
  if (pid <= 0)
    {
      if (errno == EINTR)
	{
	  errno = 0;
	  goto loop;
	}
      return;
    }

  for (tail = Vprocess_alist; XSYMBOL (tail) != XSYMBOL (Qnil); tail = XCONS (tail)->cdr)
    {
      proc = XCONS (XCONS (tail)->car)->cdr;
      p = XPROCESS (proc);
      if (!NULL (p->childp) && XFASTINT (p->pid) == pid)
	break;
    }

  if (XSYMBOL (tail) == XSYMBOL (Qnil))
    goto loop;		/* We don't know who this is */

  if (WIFSTOPPED (w))
    {
      XFASTINT (p->flags) = STOPPED | CHANGED;
      XFASTINT (p->reason) = w.w_stopsig;
      child_changed++;
    }
  else if (WIFEXITED (w))
    {
      XFASTINT (p->flags) = EXITED | CHANGED;
      if (w.w_coredump)
	XFASTINT (p->flags) |= COREDUMPED;
      XFASTINT (p->reason) = w.w_retcode;
      deactivate_process (proc);
      child_changed++;
    }
  else if (WIFSIGNALED (w))
    {
      XFASTINT (p->flags) = SIGNALED | CHANGED;
      if (w.w_coredump)
	XFASTINT (p->flags) |= COREDUMPED;
      XFASTINT (p->reason) = w.w_termsig;
      deactivate_process (proc);
      child_changed++;
    }
  goto loop;
}
#endif SIGCHLD

/* Find all process marked as "changed"
  and notify the user in a suitable fashion
  (either run the sentinel or output a message).
  This is done while Emacs is waiting for keyboard input */

change_msgs()
{
  Lisp_Object tail, proc, buffer;
  register struct Lisp_Process *p;
  register struct buffer *old = bf_cur;
  char line[50];
  int odot;

  child_changed = 0;

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);

      if (!(XFASTINT (p->flags) & CHANGED))
	continue;

      XFASTINT (p->flags) &= ~CHANGED;

      line[0] = 0;
      buffer = p->buffer;

      if ((XFASTINT (p->flags) & PROC_STATUS) == SIGNALED
	  || (XFASTINT (p->flags) & PROC_STATUS) == STOPPED)
	{
	  sprintf (line, "%s%s\n",
		   XFASTINT (p->reason) < NSIG
		     ? sys_siglist[XFASTINT (p->reason)] : "unknown",
		   XFASTINT (p->flags) & COREDUMPED ? " (core dumped)" : "");
	  if (line[0] >= 'A' && line[0] <= 'Z')
	    line[0] += 040;

	  if ((XFASTINT (p->flags) & PROC_STATUS) == SIGNALED
	      && delete_exited_processes)
	    remove_process (proc);
	}
      else if ((XFASTINT (p->flags) & PROC_STATUS) == EXITED)
	{
	  if (XFASTINT (p->reason))
	    sprintf (line, "exited abnormally with code %d\n",
		     XFASTINT (p->reason));
	  else
	    sprintf (line, "finished\n");

	  if (delete_exited_processes)
	    remove_process (proc);
	}

      if (!NULL (p->sentinel))
	exec_sentinel (proc, build_string (line));
      else if (line[0] && !NULL (buffer))
	{
	  /* Avoid error if buffer is deleted
	     (probably that's why the process is dead, too) */
	  if (NULL (XBUFFER (buffer)->name))
	    continue;
	  Fset_buffer (buffer);
	  odot = dot;
	  SetDot (NumCharacters + 1);
	  if (dot == odot)
	    odot = -1;
	  InsStr ("\nProcess ");
	  Finsert (1, &p->name);
	  InsStr (" ");
	  InsStr (line);
	  if (odot > 0)
	    SetDot (odot);
	}
    } /* end for */

  SetBfp (old);

  RedoModes++;  /* in case buffers use %s in mode-line-format */
  DoDsp (1);
}

exec_sentinel (proc, reason)
     Lisp_Object proc, reason;
{
  Lisp_Object sentinel;
  register struct Lisp_Process *p = XPROCESS (proc);

  sentinel = p->sentinel;
  if (NULL (sentinel))
    return;

  p->sentinel = Qnil;
  call2 (sentinel, proc, reason);
  p->sentinel = sentinel;
}

init_process ()
{
  register int i;
  register char *sh;

#ifdef SIGCHLD
  signal (SIGCHLD, child_sig);
#endif

  input_wait_mask = ChannelMask(0);
  Vprocess_alist = Qnil;
  for (i = 0; i < MAXDESC; i++)
    chan_process[i] = Qnil;
}

syms_of_process ()
{
  Qprocessp = intern ("processp");
  staticpro (&Qprocessp);

  staticpro (&Vprocess_alist);

  DefBoolVar ("delete-exited-processes", &delete_exited_processes,
    "*Non-nil means delete processes immediately when they exit.\n\
nil means don't delete them until list-processes is done.");

  delete_exited_processes = 1;

  defsubr (&Sprocessp);
  defsubr (&Sget_process);
  defsubr (&Sget_buffer_process);
  defsubr (&Sdelete_process);
  defsubr (&Sprocess_status);
  defsubr (&Sprocess_id);
  defsubr (&Sprocess_name);
  defsubr (&Sprocess_command);
  defsubr (&Sset_process_buffer);
  defsubr (&Sprocess_buffer);
  defsubr (&Sprocess_mark);
  defsubr (&Sset_process_filter);
  defsubr (&Sprocess_filter);
  defsubr (&Sset_process_sentinel);
  defsubr (&Sprocess_sentinel);
  defsubr (&Sprocess_kill_without_query);
  defsubr (&Slist_processes);
  defsubr (&Sstart_process);
  defsubr (&Ssend_region);
  defsubr (&Ssend_string);
  defsubr (&Sinterrupt_process);
  defsubr (&Skill_process);
  defsubr (&Squit_process);
  defsubr (&Sstop_process);
  defsubr (&Scontinue_process);
  defsubr (&Sprocess_send_eof);
}

#endif subprocesses
