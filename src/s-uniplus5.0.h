/* Template for s- header files.
   This file describes the parameters that s- files should define or not.
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

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "unisoft uniplus"
#define USG				/* System III, System V, etc */

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Define SYSTEM_NAME to have as its value
 a string for the Lisp function system-name to return.
 It is good to use a C expression that asks the operating system for
 the name, but if that is hard, omit it here and let config.h define it.  */

extern char *get_system_name();
#define SYSTEM_NAME get_system_name ()

/* Define NOMULTIPLEJOBS since shell lacks job control.  */

#define NOMULTIPLEJOBS

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'a'

/* Maximum length of a fully qualified pathname */

#define MAXPATHLEN	256

/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

#define sigsetmask(mask)	/* Null expansion */

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

#define _setjmp setjmp
#define _longjmp longjmp

/* Include cpp file to remap longnames to shortnames.  Note that not
   all cpp's support longnames.  In particular, most pre 5.2 UniSoft
   ports only support 16 unique characters.  To bootstrap GNU emacs
   to a 5.0 UniPlus+ system I had to port the 5.2 cpp (with FLEXNAMES)
   to the development system.  This was about a 1 hour job with sources.
   Alternatively, m4 can probably be used.  There are too many changes
   for sed to handle in a single pass.  Initially, this file is largely
   mechanically generated in the shortnames directory.
   Fred Fish, UniSoft Systems. */

#include "../shortnames/short.h"
