/* Definitions file for GNU Emacs running on bsd 4.2
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

#define SYSTEM_TYPE "berkeley-unix"

/* Default is to set interrupt_input to 1: do input buffering within Emacs */

#define INTERRUPT_INPUT

/* Define SYSTEM_NAME to have as its value
 a string for the Lisp function system-name to return.
 The text here asks the operating system for the name
 and saves it  */

#ifndef ASSEMBLY
char system_name_saved[32];
#endif ASSEMBLY

#define SYSTEM_NAME \
  (gethostname (system_name_saved, sizeof system_name_saved), \
   system_name_saved)

/* First pty name is /dev/ptyp0.  */

#define FIRST_PTY_LETTER 'p'
