/* Flags and parameters describing terminal's characteristics.
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


extern int baud_rate;		/* Output speed in baud */
extern int screen_width;	/* Number of usable columns */
extern int screen_height;	/* Number of lines */
extern int must_write_spaces;	/* Nonzero means spaces in the text
				   must actually be output; can't just skip
				   over some columns to leave them blank.  */
extern int min_padding_speed;	/* Speed below which no padding necessary */
extern int fast_clear_end_of_line; /* Nonzero means terminal has command for this */

extern int line_ins_del_ok;	/* Terminal can insert and delete lines */
extern int char_ins_del_ok;	/* Terminal can insert and delete chars */
extern int scroll_region_ok;	/* Terminal supports setting the scroll window */
extern int memory_below_screen;	/* Terminal remembers lines scrolled off bottom */

/* DCICcost[n] is cost of inserting N characters.
   DCICcost[-n] is cost of deleting N characters. */

#define DCICcost (&DC_ICcost[MScreenWidth])
int DC_ICcost[1 + 2 * MScreenWidth];
