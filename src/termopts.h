/* Flags and paramaters describing user options for handling the terminal.
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


extern int visible_bell;	/* If true and the terminal will support it
				   then the screen will flash instead of
				   feeping when an error occurs */

extern int inverse_video;	/* If true and the terminal will support it
				   then we will use inverse video */

/* Nonzero means use ^S/^Q as cretinous flow control.  */

extern int flow_control;

/* Nonzero means use interrupt-driven input.  */

extern int interrupt_input;

extern int MetaFlag;		/* Terminal has meta key */

/* Nonzero means truncate lines in all windows less wide than the screen */

extern int truncate_partial_width_windows;
