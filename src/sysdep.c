/* Interfaces to system-dependent kernel and library entries.
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

#include <stdio.h>
#include <sys/ioctl.h>
#include <fcntl.h>

#ifndef USG
#include <sys/wait.h>
#include <sgtty.h>
#define TERMINAL struct sgttyb
#define OSPEED(str) str.sg_ospeed
#endif

#ifdef USG
#include <termio.h>
#include <sys/utsname.h>
#include <memory.h>
#include <string.h>
#define TIOCGETP TCGETA
#define TIOCSETN TCSETAW
#define TIOCSETP TCSETAF
#define TERMINAL struct termio
#define OSPEED(str) (str.c_cflag & CBAUD)
#endif

#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "dispextern.h"

static int baud_convert[] =
  {
    0, 50, 75, 110, 135, 150, 200, 300, 600, 1200,
    1800, 2400, 4800, 9600, 19200, 38400
  };

extern int baud_rate;
extern short ospeed;

get_input_pending (addr)
     int *addr;
{
  ioctl (0, FIONREAD, addr);
}

discard_tty_input ()
{
  TERMINAL buf;

  ioctl (0, TIOCGETP, &buf);
  ioctl (0, TIOCSETP, &buf);
}

stuff_char (c)
     int c;
{
#ifndef USG
  ioctl (0, TIOCSTI, &c);
#else
  croak ("stuff_char");
#endif
}

init_baud_rate ()
{
  TERMINAL sg;

  ioctl (0, TIOCGETP, &sg);
  ospeed = OSPEED (sg);
  baud_rate = ospeed == 0 ? 1200
    : ospeed < sizeof baud_convert / sizeof baud_convert[0]
      ? baud_convert[ospeed] : 9600;
}

set_exclusive_use (fd)
     int fd;
{
#ifdef FIOCLEX
  ioctl (fd, FIOCLEX, 0);
#endif
  /* Ok to do nothing if this feature does not exist */
}

wait_without_blocking ()
{
#ifndef USG
  wait3 (0, WNOHANG | WUNTRACED, 0);
#else
  croak ("wait_without_blocking");
#endif
}

child_setup_tty (out)
     int out;
{
  TERMINAL s;

#ifndef USG  
  ioctl (out, TIOCGETP, &s);
  s.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
  ioctl (out, TIOCSETN, &s);
#else
  croak ("child_setup_tty");
#endif
}

setpgrp_of_tty (pid)
     int pid;
{
#ifdef TIOCSPGRP
  ioctl (0, TIOCSPGRP, &pid);
#else
  croak ("setpgrp_of_tty");
#endif
}

#ifdef F_SETFL
int old_fcntl_flags;

/* This may also be defined in stdio,
 but if so, this does no harm,
 and using the same name avoids wasting the other one's space.  */
char _sobuf[BUFSIZ];

request_sigio ()
{
  old_fcntl_flags = fcntl (0, F_GETFL, 0);
  fcntl (0, F_SETFL, old_fcntl_flags | FASYNC);
}

unrequest_sigio ()
{
  fcntl (0, F_SETFL, old_fcntl_flags);
}

#endif /* F_SETFL */

/* Nonzero means don't do interactive redisplay.  */

extern int noninteractive;

TERMINAL old_gtty;		/* The initial tty mode bits */

int term_initted;		/* 1 if outer tty status has been recorded */
/* This may also be defined in stdio,
 but if so, this does no harm,
 and using the same name avoids wasting the other one's space.  */
char _sobuf[BUFSIZ];

#ifdef F_SETFL
int old_fcntl_owner;
#endif

#ifdef TIOCGLTC
struct tchars old_tchars;
struct ltchars old_ltchars;
int old_lmode;
#endif

InitDsp ()
{
  TERMINAL sg;
#ifdef TIOCGLTC
  struct tchars tchars;
  int lmode;
  static struct tchars new_tchars = {-1};
  static struct ltchars new_ltchars = {-1};
#endif

  if (noninteractive)
    return;

  ioctl (0, TIOCGETP, &old_gtty);
  if (!read_socket_hook)
    {
      sg = old_gtty;

#ifdef USG
      sg.c_iflag |= (IGNBRK);	/* Ignore break condition */
      if (!flow_control)
	sg.c_iflag &= ~IXON;	/* Disable start/stop output control */
      sg.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
      sg.c_lflag &= ~ECHO;	/* Disable echo */
      sg.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
      sg.c_lflag |= ISIG;	/* Enable signals */
      sg.c_oflag &= ~ONLCR;	/* Disable map of CR to CR-NL on output */
      sg.c_oflag &= ~TAB3;	/* Disable tab expansion */
      sg.c_cc[VINTR] = '\007';		/* ^G gives SIGINT */
      sg.c_cc[VQUIT] = CDEL;		/* Turn off SIGQUIT */
      sg.c_cc[VMIN] = 1;
      sg.c_cc[VTIME] = 1;
#else /* if not USG */
      sg.sg_flags &= ~(ECHO | CRMOD | XTABS | ANYP);
      sg.sg_flags |= interrupt_input ? RAW : CBREAK;
#endif /* not USG (BSD, that is) */

      ioctl (0, TIOCSETN, &sg);

#ifdef F_SETFL
      if (interrupt_input)
	{
	  old_fcntl_owner = fcntl (0, F_GETOWN, 0);
	  fcntl (0, F_SETOWN, getpid ());
	  request_sigio ();
	}
#endif /* F_SETFL */

      /* If going to use CBREAK mode, we must request C-g to interrupt
	   and turn off start and stop chars, etc.
	   If not going to use CBREAK mode, do this anyway
	   so as to turn off local flow control for user coming over
	   network on 4.2; in this case, only t_stopc and t_startc really matter.  */
#ifdef TIOCGLTC
      ioctl (0, TIOCGETC, &old_tchars);
      ioctl (0, TIOCGLTC, &old_ltchars);
      ioctl (0, TIOCLGET, &old_lmode);

      /* Note: if not using CBREAK mode, it makes no difference how we set this */
      tchars = new_tchars;
      tchars.t_intrc = 07;
      if (flow_control)
	{
	  tchars.t_startc = '\021';
	  tchars.t_stopc = '\023';
	}
      lmode = LLITOUT | old_lmode;

      ioctl (0, TIOCSETC, &tchars);
      ioctl (0, TIOCSLTC, &new_ltchars);
      ioctl (0, TIOCLSET, &lmode);
#endif TIOCGLTC
    }
  screen_garbaged = 1;
  setbuf (stdout, _sobuf);
  term_initted = 1;
  set_terminal_modes ();
}

/* Return nonzero if safe to use tabs in output.
   At the time this is called, InitDsp has not been done yet.  */
   
tabs_safe_p ()
{
  TERMINAL sg;
  gtty (&sg);
  return (sg.sg_flags & XTABS) != XTABS;
}

RstDsp ()
{
  if (noninteractive)
    {
      fflush (stdout);
      return;
    }
  if (!term_initted)
    return;
  topos (screen_height - 1, 0);
  clear_end_of_line (screen_width);
  reset_terminal_modes ();
  fflush (stdout);
  if (read_socket_hook)
    return;
#ifdef F_SETFL
  if (interrupt_input)
    {
      unrequest_sigio ();
      fcntl (0, F_SETOWN, old_fcntl_owner);
    }
#endif
#ifdef TIOCGLTC
  ioctl (0, TIOCSETC, &old_tchars);
  ioctl (0, TIOCSLTC, &old_ltchars);
  ioctl (0, TIOCLSET, &old_lmode);
#endif TIOCGLTC
  ioctl (0, TIOCSETN, &old_gtty);
}

#ifdef USG

static struct utsname name;

char *
get_system_name ()
{
  uname (&name);
  return (name.nodename);
}

void
bzero (b, length)
     char *b;
     int length;
{
  (void) memset (b, length, 0);
}

void 
bcopy (b1, b2, length)
     char *b1;
     char *b2;
     int length;
{
  (void) memcpy (b2, b1, length);
}

int
bcmp (b1, b2, length)	/* This could be a macro! */
     char *b1;
     char *b2;
     int length;
{
  return (memcmp (b1, b2, length));
}

char *
index (s, c)		/* This could be a macro! */
     char *s, c;
{
  return (strchr (s, c));
}

/*
 *	Warning, this function may not duplicate 4.2 action properly
 *	under error conditions.
 */

char *
getwd (pathname)
     char *pathname;
{
  extern char *getcwd ();

  return (getcwd (pathname, MAXPATHLEN));
}

Fget_buffer_process() {croak ("Fget_buffer_process");}
Fprocess_status() {croak ("Fprocess_status");}
unexec() {croak ("unexec");}
random() {croak ("random");}
vfork() {croak ("vfork");}
setpriority() {croak ("setpriority");}
dup2() {croak ("dup2");}
fchmod() {croak ("fchmod");}
rename() {croak ("rename");}
symlink() {croak ("symlink");}
readlink() {croak ("readlink");}
Ffile_name_completion() {croak ("Ffile_name_completion");}
Ffile_name_all_completions() {croak ("Ffile_name_all_completions");}


/*
 *	This function will go away as soon as all the stubs fixed.  (fnf)
 */

croak (badfunc)
     char *badfunc;
{
  printf ("%s not yet implemented\r\n", badfunc);
  RstDsp ();
  exit (1);
}

/* A broken definition of alloca was removed from here.
   Valid definitions for many cpus are in alloca.s.
   It is totally useless to attempt to deal with the
   problem of alloca in any manner except to add a valid
   definition for your cpu type to alloca.s.  */

#endif /* USG */
