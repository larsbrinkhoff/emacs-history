/* Display generation from window structure and buffer text.
   Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include <stdio.h>
/*#include <ctype.h>*/
#undef NULL
#include "lisp.h"
#include "window.h"
#include "termchar.h"
#include "dispextern.h"
#include "buffer.h"
#include "indent.h"
#include "commands.h"
#include "macros.h"

extern int interrupt_input;

/* Nonzero means print newline before next minibuffer message.  */

int noninteractive_need_newline;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* The buffer position of the first character appearing
 entirely or partially on the current screen line.
 Or zero, which disables the optimization for the current screen line. */
static int this_line_bufpos;

/* Number of characters past the end of this line,
   including the terminating newline */
static int this_line_endpos;

/* The vertical position of this screen line. */
static int this_line_vpos;

/* Hpos value for start of display on this screen line.
   Usually zero, but negative if first character really began
   on previous line */
static int this_line_start_hpos;

/* Buffer that this_line variables are describing. */
static struct buffer *this_line_buffer;

/* Value of minibuf_message when it was last acted on.
  If this is nonzero, there is a message on the screen
  in the minibuffer and it should be erased as soon
  as it is no longer requested to appear. */
char *prev_minibuf_message;

/* Nonzero means truncate lines in all windows less wide than the screen */
int truncate_partial_width_windows;

Lisp_Object Vglobal_mode_string;

/* Marker for where to display an arrow on top of the buffer text.  */
Lisp_Object Voverlay_arrow_position;

/* String to display for the arrow.  */
Lisp_Object Voverlay_arrow_string;

/* Values of those variables at last redisplay.  */
Lisp_Object last_arrow_position, last_arrow_string;

/* The number of lines to try scrolling a
  window by when point leaves the window; if
  it is <=0 then point is centered in the window */
int scroll_step;

/* Nonzero means send various TERMCAP strings when screen is cleared.  */
int reset_terminal_on_clear;

/* Nonzero if try_window_id has made blank lines at window bottom
 since the last redisplay that paused */
static int blank_end_of_window;

/* Number of windows showing the buffer of the selected window.
   keyboard.c refers to this.  */
int buffer_shared;

/* display_text_line sets these to the screen position (origin 0) of point,
  whether the window is selected or not.
 Set one to -1 first to determine whether point was found afterwards.  */

static int point_vpos;
static int point_hpos;

int debug_end_pos;

/* Nonzero means display mode line highlighted */
int mode_line_inverse_video;

struct position *display_text_line ();

/* Prompt to display in front of the minibuffer contents */
char *minibuf_prompt;

/* Width in columns of current minibuffer prompt.  */
int minibuf_prompt_width;

/* Message to display instead of minibuffer contents
   This is what the functions error and message make,
   and command echoing uses it as well.
   It overrides the minibuf_prompt as well as the buffer.  */
char *minibuf_message;

/* Depth in recursive edits */
int RecurseDepth;

/* Depth in minibuffer invocations */
int MinibufDepth;

/* true iff we should redraw the mode lines on the next redisplay */
int RedoModes;

/* Minimum value of bf_s1 since last redisplay that finished.
 Valid for current buffer unless Cant1WinOpt is nonzero. */
int beg_unchanged;

/* Minimum value of bf_s2 since last redisplay that finished.
 Valid for current buffer unless Cant1WinOpt is nonzero. */
int end_unchanged;

/* bf_modified as of last redisplay that finished;
 if it matches bf_modified, beg_unchanged and end_unchanged
 contain no useful information */
int unchanged_modified;

/* Nonzero if head_clip or tail_clip of current buffer has changed
 since last redisplay that finished */
int clip_changed;

/* Nonzero if window sizes or contents have changed
 since last redisplay that finished */
int windows_or_buffers_changed;

char *decode_mode_spec ();

DEFUN ("redraw-display", Fredraw_display, Sredraw_display, 0, 0, "",
  "Clear the screen and output again what is supposed to appear on it.")
  ()
{
  if (screen_height == 0) abort (); /* Some bug zeros some core */
  if (reset_terminal_on_clear)
    set_terminal_modes ();
  clear_screen ();
  fflush (stdout);
  clear_screen_records ();
  if (screen_height == 0) abort (); /* Some bug zeros some core */
  windows_or_buffers_changed++;
  /* Mark all windows as INaccurate,
     so that every window will have its redisplay done.  */
  mark_window_display_accurate (XWINDOW (minibuf_window)->prev, 0);
  if (screen_height == 0) abort (); /* Some bug zeros some core */
  return Qnil;
}

static char message_buf[MScreenWidth + 1];

/* dump an informative message to the minibuf */
/* VARARGS 1 */
message (m, a1, a2, a3)
     char *m;
{
  if (noninteractive)
    {
      if (noninteractive_need_newline)
	putchar ('\n');
      printf (m, a1, a2, a3);
      printf ("\n");
      fflush (stdout);
    }
  else if (INTERACTIVE)
    {
#ifdef NO_ARG_ARRAY
      int a[3];
      a[0] = a1;
      a[1] = a2;
      a[2] = a3;

      doprnt (message_buf, sizeof message_buf - 1, m, 3, a);
#else
      doprnt (message_buf, sizeof message_buf - 1, m, 3, &a1);
#endif /* NO_ARG_ARRAY */
      minibuf_message = message_buf;
      do {
	hold_window_change ();
	display_minibuf_message ();
	update_screen (1, 1);
	unhold_window_change ();
      } while (screen_garbaged);
    }
}

/* Specify m, a string, as a message in the minibuf.  */
message1 (m)
     char *m;
{
  if (noninteractive)
    {
      if (noninteractive_need_newline)
	putchar ('\n');
      printf ("%s\n", m);
      fflush (stdout);
    }
  else if (INTERACTIVE)
    {
      minibuf_message = m;
      do {
	hold_window_change ();
	display_minibuf_message ();
	update_screen (1, 1);
	unhold_window_change ();
      } while (screen_garbaged);
    }
}

display_minibuf_message ()
{
  register int vpos;
  register struct display_line *line;

  if (screen_garbaged)
    {
      Fredraw_display ();
      screen_garbaged = 0;
    }

  if (minibuf_message || !MinibufDepth)
    {
      vpos = XFASTINT (XWINDOW (minibuf_window)->top);
      line = get_display_line (vpos, 0);
      display_string (XWINDOW (minibuf_window), line,
		      minibuf_message ? minibuf_message : "",
		      0, 0, 0, screen_width);

      /* If desired cursor location is on this line, put it at end of text */
      if (cursY == vpos)
	cursX = line->length;
    }
  else if (!EQ (minibuf_window, selected_window))
    windows_or_buffers_changed++;

  if (EQ (minibuf_window, selected_window))
    this_line_bufpos = 0;

  prev_minibuf_message = minibuf_message;
}

/* Do a screen update, taking possible shortcuts into account.
   This is the main external entry point for redisplay.

   If the last redisplay displayed a minibuffer message and that
   message is no longer requested, we usually redraw the contents
   of the minibuffer, or clear the minibuffer if it is not in use.
   If the argument SaveMiniBuf is nonzero, this is not done.

   Everyone would like to have a hook here to call eval,
   but that cannot be done safely without a lot of changes elsewhere.
   This can be called from signal handlers; with alarms set up;
   or with synchronous processes running.
   See the function EchoThem in keyboard.c.
   See Fcall_process; if you called it from here, it could be
   entered recursively.  */

DoDsp (SaveMiniBuf)
{
  register struct window *w = XWINDOW (selected_window);
  register int pause;
  int inhibit_hairy_id = 0;
  int must_finish = 0;
  int all_windows;
  register int tlbufpos, tlendpos;
  struct position pos;
  extern int input_pending;

  if (noninteractive)
    return;

  hold_window_change ();

  if (screen_garbaged)
    {
      Fredraw_display ();
      screen_garbaged = 0;
    }

  if (minibuf_message ||
      (prev_minibuf_message && !SaveMiniBuf))
    {
      display_minibuf_message ();
      must_finish = 1;
    }

  if (clip_changed || windows_or_buffers_changed)
    RedoModes++;

  /* Detect case that we need to write a star in the mode line.  */
  if (XFASTINT (w->last_modified) < bf_modified
      && XFASTINT (w->last_modified) <= bf_cur->save_modified)
    {
      w->redo_mode_line = Qt;
      if (buffer_shared > 1)
	RedoModes++;
    }

  all_windows = RedoModes || buffer_shared > 1;

  /* If specs for an arrow have changed, do thorough redisplay
     to ensure we remove any arrow that should no longer exist.  */
  if (Voverlay_arrow_position != last_arrow_position
      || Voverlay_arrow_string != last_arrow_string)
    all_windows = 1, clip_changed = 1;

  tlbufpos = this_line_bufpos;
  tlendpos = this_line_endpos;
  if (!all_windows && tlbufpos > 0 && NULL (w->redo_mode_line)
      /* Make sure recorded data applies to current buffer, etc */
      && this_line_buffer == bf_cur
      && bf_cur == XBUFFER (w->buffer)
      && NULL (w->force_start)
      /* Point must be on the line that we have info recorded about */
      && point >= tlbufpos
      && point <= bf_s1 + bf_s2 + 1 - tlendpos
      /* All text outside that line, including its final newline,
	 must be unchanged */
      && (XFASTINT (w->last_modified) >= bf_modified
	  || (beg_unchanged >= tlbufpos - 1
	      && bf_s1 >= tlbufpos - 1
	      && end_unchanged >= tlendpos
	      && bf_s2 >= tlendpos)))
    {
      if (tlbufpos > FirstCharacter && CharAt (tlbufpos - 1) != '\n'
	  && (tlbufpos == NumCharacters + 1
	      || CharAt (tlbufpos) == '\n'))
	/* Former continuation line has disappeared by becoming empty */
	goto cancel;
      else if (XFASTINT (w->last_modified) < bf_modified
	       || EQ (selected_window, minibuf_window))
	{
	  point_vpos = -1;
	  display_text_line (w, tlbufpos, this_line_vpos, this_line_start_hpos,
			     pos_tab_offset (w, tlbufpos));
	  /* If line contains point, is not continued,
		 and ends at same distance from eob as before, we win */
	  if (point_vpos >= 0 && this_line_bufpos
	      && this_line_endpos == tlendpos)
	    {
	      /* Done by display_text_line
		 cursX = point_hpos;
	         cursY = this_line_vpos;
	       */
	      if (XFASTINT (w->width) != screen_width)
		preserve_other_columns (w);
	      goto update;
	    }
	  else
	    goto cancel;
	}
      else if (point == XFASTINT (w->last_point))
	{
	  if (!must_finish)
	    return;
	  goto update;
	}
      else
	{
	  pos = *compute_motion (tlbufpos, 0,
				XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0,
				point, 2, - (1 << (SHORTBITS - 1)),
				XFASTINT (w->width) - 1
				- (XFASTINT (w->width) + XFASTINT (w->left) != screen_width),
				XINT (w->hscroll), 0);
	  if (pos.vpos < 1)
	    {
	      cursX = max (XFASTINT (w->left), pos.hpos);
	      cursY = this_line_vpos;
	      goto update;
	    }
	  else
	    goto cancel;
	}
    cancel:
      /* Text changed drastically or point moved off of line */
      cancel_line (this_line_vpos);
    }

  this_line_bufpos = 0;

  if (all_windows)
    redisplay_all_windows ();
  else
    {
      inhibit_hairy_id = redisplay_window (selected_window, 1);
      if (XFASTINT (w->width) != screen_width)
	preserve_other_columns (w);
    }

update: 
  if (interrupt_input)
    unrequest_sigio ();

  pause = update_screen (0, inhibit_hairy_id);

  /* If screen does not match, prevent doing single-line-update next time.
     Also, don't forget to check every line to update the arrow.  */
  if (pause)
    {
      this_line_bufpos = 0;
      if (!NULL (last_arrow_position))
	{
	  last_arrow_position = Qt;
	  last_arrow_string = Qt;
	}
      /* If we pause after scrolling, some lines in PhysScreen may be null
	 and then preserve_other_columns won't be able to preserve all
	 the vertical-bar separators.  So avoid using it in that case.  */
      if (XFASTINT (w->width) != screen_width)
	RedoModes = 1;
    }

  /* Now text on screen agrees with windows, so
     put info into the windows for partial redisplay to follow */

  if (!pause)
    {
      register struct buffer_text *t
	= XBUFFER (w->buffer) == bf_cur
	  ? &bf_text : &XBUFFER (w->buffer)->text;

      blank_end_of_window = 0;
      clip_changed = 0;
      unchanged_modified = t->modified;
      beg_unchanged = t->size1, end_unchanged = t->size2;

      XFASTINT (w->last_point) = t->pointloc;
      XFASTINT (w->last_point_x) = cursX;
      XFASTINT (w->last_point_y) = cursY;

      if (all_windows)
	mark_window_display_accurate (XWINDOW (minibuf_window)->prev, 1);
      else
	{
	  w->redo_mode_line = Qnil;
	  XFASTINT (w->last_modified) = t->modified;
	  w->window_end_valid = Qt;
	  last_arrow_position = Voverlay_arrow_position;
	  last_arrow_string = Voverlay_arrow_string;
	}
      RedoModes = 0;
      windows_or_buffers_changed = 0;
    }

  /* Start SIGIO interrupts coming again.
     Having them off during the code above
     makes it less likely one will discard output,
     but not impossible, since there might be stuff
     in the system buffer here.
     But it is much hairier to try to do anything about that.  */

  if (interrupt_input)
    request_sigio ();

  unhold_window_change ();

  if (screen_garbaged)
    DoDsp (SaveMiniBuf);
}

mark_window_display_accurate (window, flag)
     Lisp_Object window;
     int flag;
{
  register struct window *w;

  for (;!NULL (window); window = w->next)
    {
      w = XWINDOW (window);

      if (!NULL (w->buffer))
	XFASTINT (w->last_modified)
	  = !flag ? 0
	    : XBUFFER (w->buffer) == bf_cur
	      ? bf_modified : XBUFFER (w->buffer)->text.modified;
      w->window_end_valid = Qt;
      w->redo_mode_line = Qnil;

      if (!NULL (w->vchild))
	mark_window_display_accurate (w->vchild, flag);
      if (!NULL (w->hchild))
	mark_window_display_accurate (w->hchild, flag);
    }

  if (flag)
    {
      last_arrow_position = Voverlay_arrow_position;
      last_arrow_string = Voverlay_arrow_string;
    }
  else
    {
      /* t is unequal to any useful value of Voverlay_arrow_... */
      last_arrow_position = Qt;
      last_arrow_string = Qt;
    }
}

int do_id = 1;

/* Do full redisplay of one or all windows.
  This does not include updating the screen;
  just generating lines to pass to update_screen.  */

/* Entry point to redisplay all windows */

redisplay_all_windows ()
{
  buffer_shared = 0;

  redisplay_windows (XWINDOW (minibuf_window)->prev);
}

redisplay_windows (window)
     Lisp_Object window;
{
  for (; !NULL (window); window = XWINDOW (window)->next)
    redisplay_window (window, 0);
}

redisplay_window (window, just_this_one)
     Lisp_Object window;
     int just_this_one;
{
  register struct window *w = XWINDOW (window);
  int height;
  register int lpoint = point;
  struct buffer *old = bf_cur;
  register int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left) != screen_width);
  register int startp;
  register int hscroll = XINT (w->hscroll);
  struct position pos;
  int inhibit_hairy_id = 0;
  int opoint;
  int tem;

  if (screen_height == 0) abort (); /* Some bug zeros some core */

  /* If this is a combination window, do its children; that's all.  */

  if (!NULL (w->vchild))
    {
      redisplay_windows (w->vchild);
      return 0;
    }
  if (!NULL (w->hchild))
    {
      redisplay_windows (w->hchild);
      return 0;
    }
  if (NULL (w->buffer))
    abort ();

  if (RedoModes)
    w->redo_mode_line = Qt;

  /* Otherwise set up data on this window; select its buffer and point value */

  height = XFASTINT (w->height);
  if (w != XWINDOW (minibuf_window))
    height--;
  else if (minibuf_message)
    return 0;

  SetBfx (XBUFFER (w->buffer));
  opoint = point;

  if (!just_this_one
      && bf_cur == XBUFFER (XWINDOW (selected_window)->buffer))
    buffer_shared++;

  if (!EQ (window, selected_window))
    {
      SetPoint (marker_position (w->pointm));
      if (point < FirstCharacter)
	point = FirstCharacter;
      else if (point > NumCharacters)
	point = NumCharacters + 1;
    }

  /* If window-start is screwed up, choose a new one.  */

  if (XMARKER (w->start)->buffer != bf_cur)
    goto recenter;

  startp = marker_position (w->start);

  /* Handle case where place to start displaying has been specified */

  if (!NULL (w->force_start))
    {
      w->redo_mode_line = Qt;
      w->force_start = Qnil;
      XFASTINT (w->last_modified) = 0;
      if (!try_window (window, startp))
	{
	  /* If point does not appear, move point so it does appear */
	  pos = *compute_motion (startp, 0,
				((EQ (window, minibuf_window) && startp == 1)
				 ? minibuf_prompt_width : 0)
				+
				(hscroll ? 1 - hscroll : 0),
				NumCharacters + 1, height / 2,
				- (1 << (SHORTBITS - 1)),
				width, hscroll, pos_tab_offset (w, startp));
	  SetPoint (pos.bufpos);
	  if (w != XWINDOW (selected_window))
	    Fset_marker (w->pointm, make_number (point), Qnil);
	  else
	    lpoint = point;

	  if (EQ (window, selected_window))
	    {
	      cursX = max (0, pos.hpos) + XFASTINT (w->left);
	      cursY = pos.vpos + XFASTINT (w->top);
	    }
	}
      goto done;
    }

  /* Handle case where text has not changed, only point,
     and it has not moved off the screen */

  /* This code is not used for minibuffer for the sake of
     the case of redisplaying to replace an echo area message;
     since in that case the minibuffer contents per se are usually unchanged.
     This code is of no real use in the minibuffer since
     the handling of tlbufpos, etc., in DoDsp handles the same cases.  */

  if (XFASTINT (w->last_modified) >= bf_modified
      && point >= startp && !clip_changed
      && (just_this_one || XFASTINT (w->width) == screen_width)
      && !EQ (window, minibuf_window))
    {
      pos = *compute_motion (startp, 0, (hscroll ? 1 - hscroll : 0),
			    point, height + 1, 10000, width, hscroll,
			    pos_tab_offset (w, startp));

      if (pos.vpos < height)
	{
	  /* Ok, point is still on screen */
	  if (w == XWINDOW (selected_window))
	    {
	      /* These variables are supposed to be origin 1 */
	      cursX = max (0, pos.hpos) + XFASTINT (w->left);
	      cursY = pos.vpos + XFASTINT (w->top);
	    }
/* This doesn't do the trick, because if a window to the right of
 this one must be redisplayed, this does nothing because there
 is nothing in DesiredScreen yet, and then the other window is
 redisplayed, making likes that are empty in this window's columns.
	  if (XFASTINT (w->width) != screen_width)
	    preserve_my_columns (w);
*/
	  goto done;
	}
      /* Don't bother trying redisplay with same start;
	we already know it will lose */
    }
  /* If current starting point was originally the beginning of a line
     but no longer is, find a new starting point.  */
  else if (!NULL (w->start_at_line_beg)
	   && !(startp == FirstCharacter
		|| CharAt (startp - 1) == '\n'))
    {
      goto recenter;
    }
  else if (just_this_one && !EQ (window, minibuf_window)
	   && point >= startp
	   && XFASTINT (w->last_modified)
	   && ! EQ (w->window_end_valid, Qnil)
	   && do_id && !clip_changed
	   && !blank_end_of_window
	   && XFASTINT (w->width) == screen_width
	   && EQ (last_arrow_position, Voverlay_arrow_position)
	   && EQ (last_arrow_string, Voverlay_arrow_string)
	   && (tem = try_window_id (selected_window))
	   && tem != -2)
    {
      /* tem > 0 means success.  tem == -1 means choose new start.
	 tem == -2 means try again with same start,
	  and nothing but whitespace follows the changed stuff.
	 tem == 0 means try again with same start.  */
      if (tem > 0)
	{
/*       inhibit_hairy_id = 1;   */
	  goto done;
	}
    }
  else if (startp >= FirstCharacter && startp <= NumCharacters + 1
	   /* Avoid starting display at end of buffer! */
	   && (startp <= NumCharacters || startp == FirstCharacter
	       || (XFASTINT (w->last_modified) >= bf_modified)))
    {
      /* Try to redisplay starting at same place as before */
      /* If point has not moved off screen, accept the results */
      if (try_window (window, startp))
	goto done;
      else
	cancel_my_columns (w);
    }

  XFASTINT (w->last_modified) = 0;
  w->redo_mode_line = Qt;

  /* Try to scroll by specified few lines */

  if (scroll_step && !clip_changed)
    {
      if (point > startp)
	{
	  pos = *vmotion (bf_s1 + bf_s2 + 1 - XFASTINT (w->window_end_pos),
			  scroll_step, width, hscroll, window);
	  if (pos.vpos >= height)
	    goto scroll_fail;
	}

      pos = *vmotion (startp, point < startp ? - scroll_step : scroll_step,
		      width, hscroll, window);

      if (point >= pos.bufpos)
	{
	  if (try_window (window, pos.bufpos))
	    goto done;
	  else
	    cancel_my_columns (w);
	}
    scroll_fail: ;
    }

  /* Finally, just choose place to start which centers point */

recenter:
  pos = *vmotion (point, - height / 2, width, hscroll, window);
  try_window (window, pos.bufpos);

  startp = marker_position (w->start);
  w->start_at_line_beg = 
    (startp == FirstCharacter || CharAt (startp - 1) == '\n') ? Qt : Qnil;

done:
  /* If window not full width, must redo its mode line
     if the window to its side is being redone */
  if ((!NULL (w->redo_mode_line)
       || (!just_this_one && width < screen_width - 1))
      && !EQ (window, minibuf_window))
    display_mode_line (w);

  SetPoint (opoint);
  SetBfx (old);
  SetPoint (lpoint);

  return inhibit_hairy_id;
}

/* Do full redisplay on one window,
  starting at position `pos',
  and return nonzero if point appears in the displayed text */

try_window (window, pos)
     Lisp_Object window;
     register int pos;
{
  register struct window *w = XWINDOW (window);
  register int height = XFASTINT (w->height) - !EQ (window, minibuf_window);
  register int vpos = XFASTINT (w->top);
  register int last_text_vpos = vpos;
  int tab_offset = pos_tab_offset (w, pos);

  struct position val;

  Fset_marker (w->start, make_number (pos), Qnil);

  point_vpos = -1;
  val.hpos = XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0;

  while (--height >= 0)
    {
      val = *display_text_line (w, pos, vpos, val.hpos, tab_offset);
      tab_offset += XFASTINT (w->width) - 1;
      if (val.vpos) tab_offset = 0;
      vpos++;
      if (pos != val.bufpos)
	last_text_vpos
	  /* Next line, unless prev line ended in end of buffer with no cr */
	  = vpos - (val.vpos && CharAt (val.bufpos - 1) != '\n');
      pos = val.bufpos;
    }

  /* If last line is continued in middle of character,
     include the split character in the text considered on the screen */
  if (val.hpos < (XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0))
    pos++;

  /* Say where last char on screen will be, once redisplay is finished.  */
  XFASTINT (w->window_end_pos) = bf_s1 + bf_s2 + 1 - pos;
  XFASTINT (w->window_end_vpos) = last_text_vpos - XFASTINT (w->top);
  /* But that is not valid info until redisplay finishes.  */
  w->window_end_valid = Qnil;
  return point_vpos >= 0;
}

/* Try to redisplay when buffer is modified locally,
 computing insert/delete line to preserve text outside
 the bounds of the changes.
 Return 1 if successful, 0 if if cannot tell what to do,
 or -1 to tell caller to find a new window start,
 or -2 to tell caller to do normal redisplay with same window start.  */

try_window_id (window)
     Lisp_Object window;
{
  int pos;
  register struct window *w = XWINDOW (window);
  register int height = XFASTINT (w->height) - !EQ (window, minibuf_window);
  int top = XFASTINT (w->top);
  int start = marker_position (w->start);
  int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left) != screen_width);
  int hscroll = XINT (w->hscroll);
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  register int vpos;
  register int i, tem;
  int last_text_vpos = 0;
  int stop_vpos;

  struct position val, bp, ep, xp, pp;
  int scroll_amount = 0;
  int delta;
  int tab_offset, epto;

  if (bf_s1 < beg_unchanged)
    beg_unchanged = bf_s1;
  if (bf_s2 < end_unchanged)
    end_unchanged = bf_s2;

  if (beg_unchanged + 1 < start)
    return 0;			/* Give up if changes go above top of window */

  /* Find position before which nothing is changed.  */
  bp = *compute_motion (start, 0, lmargin,
			beg_unchanged + 1, 10000, 10000, width, hscroll,
			pos_tab_offset (w, start));
  if (bp.vpos >= height)
    return point < bp.bufpos && !bp.contin;

  vpos = bp.vpos;

  /* Find beginning of that screen line.  Must display from there.  */
  bp = *vmotion (bp.bufpos, 0, width, hscroll, window);

  pos = bp.bufpos;
  val.hpos = lmargin;
  if (pos < start)
    return -1;

  /* If about to start displaying at the beginning of a continuation line,
     really start with previous screen line, in case it was not
     continued when last redisplayed */
  if (bp.contin && bp.bufpos - 1 == beg_unchanged && vpos > 0)
    {
      bp = *vmotion (bp.bufpos, -1, width, hscroll, window);
      --vpos;
      pos = bp.bufpos;
    }

  if (bp.contin && bp.hpos != lmargin)
    {
      val.hpos = bp.prevhpos - width + lmargin;
      pos--;
    }

  bp.vpos = vpos;

  /* Find first visible newline after which no more is changed.  */
  tem = find_next_newline (bf_s1 + bf_s2 + 1
			   - max (end_unchanged, bf_tail_clip),
			   1);
  if (XTYPE (bf_cur->selective_display) == Lisp_Int
      && XINT (bf_cur->selective_display) > 0)
    while (tem < NumCharacters
	   && (position_indentation (tem)
	       >= XINT (bf_cur->selective_display)))
      tem = find_next_newline (tem, 1);

  /* Compute the cursor position after that newline.  */
  ep = *compute_motion (pos, vpos, val.hpos, tem,
			height, - (1 << (SHORTBITS - 1)),
			width, hscroll, pos_tab_offset (w, bp.bufpos));

  /* If changes reach past the text available on the screen,
     just display rest of screen.  */
  if (ep.bufpos > bf_s1 + bf_s2 + 1 - XFASTINT (w->window_end_pos))
    stop_vpos = height;
  else
    stop_vpos = ep.vpos;

  /* If no newline before ep, the line ep is on includes some changes
     that must be displayed.  Make sure we don't stop before it.  */
  /* Also, if changes reach all the way until ep.bufpos,
     it is possible that something was deleted after the
     newline before it, so the following line must be redrawn. */
  if (stop_vpos == ep.vpos
      && (ep.bufpos == FirstCharacter
	  || CharAt (ep.bufpos - 1) != '\n'
	  || ep.bufpos == bf_s1 + bf_s2 + 1 - end_unchanged))
    stop_vpos = ep.vpos + 1;

  point_vpos = -1;

  /* If changes do not reach to bottom of window,
     figure out how much to scroll the rest of the window */
  if (stop_vpos < height)
    {
      /* Now determine how far up or down the rest of the window has moved */
      epto = pos_tab_offset (w, ep.bufpos);
      xp = *compute_motion (ep.bufpos, ep.vpos, ep.hpos,
			    bf_s1 + bf_s2 + 1 - XFASTINT (w->window_end_pos),
			    10000, 0, width, hscroll, epto);
      scroll_amount = xp.vpos - XFASTINT (w->window_end_vpos);

      /* Is everything on screen below the changes whitespace?
	 If so, no scrolling is really necessary.  */
      for (i = ep.bufpos; i < xp.bufpos; i++)
	{
	  tem = CharAt (i);
	  if (tem != ' ' && tem != '\n' && tem != '\t')
	    break;
	}
      if (i == xp.bufpos)
	return -2;

      XFASTINT (w->window_end_vpos) += scroll_amount;

      /* Before doing any scrolling, verify that point will be on screen. */
      if (point > ep.bufpos && !(point <= xp.bufpos && xp.bufpos < height))
	{
	  if (point <= xp.bufpos)
	    {
	      pp = *compute_motion (ep.bufpos, ep.vpos, ep.hpos,
				    point, height, - (1 << (SHORTBITS - 1)),
				    width, hscroll, epto);
	    }
	  else
	    {
	      pp = *compute_motion (xp.bufpos, xp.vpos, xp.hpos,
				    point, height, - (1 << (SHORTBITS - 1)),
				    width, hscroll, pos_tab_offset (w, xp.bufpos));
	    }
	  if (pp.bufpos < point || pp.vpos == height)
	    return 0;
	  point_vpos = pp.vpos + top;
	  point_hpos = pp.hpos + XFASTINT (w->left);
	}

      if (stop_vpos - scroll_amount >= height
	  || ep.bufpos == xp.bufpos)
	{
	  if (scroll_amount < 0)
	    stop_vpos -= scroll_amount;
	  scroll_amount = 0;
	  /* In this path, we have altered window_end_vpos
	     and not left it negative.
	     We must make sure that, in case display is preempted
	     before the screen changes to reflect what we do here,
	     further updates will not come to try_window_id
	     and assume the screen and window_end_vpos match.  */
	  blank_end_of_window = 1;
	}
      else if (!scroll_amount)
	{}
      else if (bp.bufpos == bf_s1 + bf_s2 + 1 - end_unchanged)
	{
	  /* If reprinting everything is nearly as fast as scrolling,
	     don't bother scrolling.  Can happen if lines are short.  */
	  if (scroll_cost (bp.vpos + top - scroll_amount,
			   top + height - max (0, scroll_amount),
			   scroll_amount)
	      > xp.bufpos - bp.bufpos - 20)
	    /* Return "try normal display with same window-start."
	       Too bad we can't prevent further scroll-thinking.  */
	    return -2;
	  /* If pure deletion, scroll up as many lines as possible.
	     In common case of killing a line, this can save the
	     following line from being overwritten by scrolling
	     and therefore having to be redrawn.  */
	  tem = scroll_screen_lines (bp.vpos + top - scroll_amount,
				     top + height - max (0, scroll_amount),
				     scroll_amount);
	  if (!tem) stop_vpos = height;
	}
      else if (scroll_amount)
	{
	  /* If reprinting everything is nearly as fast as scrolling,
	     don't bother scrolling.  Can happen if lines are short.  */
	  /* Note that if scroll_amount > 0, xp.bufpos - bp.bufpos is an
	     overestimate of cost of reprinting, since xp.bufpos
	     would end up below the bottom of the window.  */
	  if (scroll_cost (ep.vpos + top - scroll_amount,
			   top + height - max (0, scroll_amount),
			   scroll_amount)
	      > xp.bufpos - ep.bufpos - 20)
	    /* Return "try normal display with same window-start."
	       Too bad we can't prevent further scroll-thinking.  */
	    return -2;
	  tem = scroll_screen_lines (ep.vpos + top - scroll_amount,
				     top + height - max (0, scroll_amount),
				     scroll_amount);
	  if (!tem) stop_vpos = height;
	}
    }

  /* In any case, do not display past bottom of window */
  if (stop_vpos >= height)
    {
      stop_vpos = height;
      scroll_amount = 0;
    }

  /* Handle case where pos is before w->start --
     can happen if part of line had been clipped and is not clipped now */
  if (vpos == 0 && pos < marker_position (w->start))
    Fset_marker (w->start, make_number (pos), Qnil);

  /* Redisplay the lines where the text was changed */
  last_text_vpos = vpos;
  tab_offset = pos_tab_offset (w, pos);
  if (val.hpos < 0)
    tab_offset += XFASTINT (w->width) - 1;
  while (vpos < stop_vpos)
    {
      val = *display_text_line (w, pos, top + vpos++, val.hpos, tab_offset);
      tab_offset += XFASTINT (w->width) - 1;
      if (val.vpos) tab_offset = 0;
      if (pos != val.bufpos)
	last_text_vpos
	  /* Next line, unless prev line ended in end of buffer with no cr */
	    = vpos - (val.vpos && CharAt (val.bufpos - 1) != '\n');
      pos = val.bufpos;
    }

  /* There are two cases:
     1) we have displayed down to the bottom of the window
     2) we have scrolled lines below stop_vpos by scroll_amount  */

  if (vpos == height)
    {
      /* If last line is continued in middle of character,
	 include the split character in the text considered on the screen */
      if (val.hpos < lmargin)
	val.bufpos++;
      XFASTINT (w->window_end_vpos) = last_text_vpos;
      XFASTINT (w->window_end_pos) = bf_s1 + bf_s2 + 1 - val.bufpos;
    }

  /* If scrolling made blank lines at window bottom,
     redisplay to fill those lines */
  if (scroll_amount < 0)
    {
      vpos = xp.vpos;
      pos = xp.bufpos;
      val.hpos = lmargin;
      if (pos == NumCharacters + 1)
	vpos = height + scroll_amount;
      else if (xp.contin && xp.hpos != lmargin)
	{
	  val.hpos = xp.prevhpos - width + lmargin;
	  pos--;
	}

      blank_end_of_window = 1;
      tab_offset = pos_tab_offset (w, pos);
      if (val.hpos < 0)
	tab_offset += XFASTINT (w->width) - 1;

      while (vpos < height)
	{
	  val = *display_text_line (w, pos, top + vpos++, val.hpos, tab_offset);
	  tab_offset += XFASTINT (w->width) - 1;
	  if (val.vpos) tab_offset = 0;
	  pos = val.bufpos;
	}

      /* Here is a case where display_line_text sets point_vpos wrong.
	 Make it be fixed up, below.  */
      if (xp.bufpos == NumCharacters + 1
	  && xp.bufpos == point)
	point_vpos = -1;
    }

  /* Attempt to adjust end-of-text positions to new bottom line */
  if (scroll_amount)
    {
      delta = height - xp.vpos;
      if (delta < 0
	  || (delta > 0 && xp.bufpos <= NumCharacters)
	  || (delta == 0 && xp.hpos))
	{
	  val = *vmotion (bf_s1 + bf_s2 + 1 - XFASTINT (w->window_end_pos),
			  delta, width, hscroll, window);
	  XFASTINT (w->window_end_pos) = bf_s1 + bf_s2 + 1 - val.bufpos;
	  XFASTINT (w->window_end_vpos) += val.vpos;
	}
    }

  w->window_end_valid = Qnil;

  /* If point was not in a line that was displayed, find it */
  if (point_vpos < 0)
    {
      val = *compute_motion (start, 0, lmargin, point, 10000, 10000,
			     width, hscroll, pos_tab_offset (w, start));
      /* Admit failure if point is off screen now */
      if (val.vpos >= height)
	{
	  for (vpos = 0; vpos < height; vpos++)
	    cancel_line (vpos + top);
	  return 0;
	}
      point_vpos = val.vpos + top;
      point_hpos = val.hpos + XFASTINT (w->left);
    }

  cursX = max (0, point_hpos);
  cursY = point_vpos;

  if (debug_end_pos)
    {
      val = *compute_motion (start, 0, lmargin, NumCharacters + 1,
			     height, - (1 << (SHORTBITS - 1)),
			     width, hscroll, pos_tab_offset (w, start));
      if (val.vpos != XFASTINT (w->window_end_vpos))
	abort ();
      if (XFASTINT (w->window_end_pos)
	  != bf_s1 + bf_s2 + 1 - val.bufpos)
	abort ();
    }

  return 1;
}

/* Display one line of window w, starting at position `start' in w's buffer.
 Display starting at horizontal position `hpos',
  which is normally zero or negative.
  A negative value causes output up to hpos = 0 to be discarded.
  This is done for negative hscroll, or when this is a continuation line
  and the continuation occurred in the middle of a multi-column character.

 `taboffset' is an offset for ostensible hpos, used in tab stop calculations.

 Display on position `vpos' on the screen.  (origin 0).

 Returns a `struct position' giving character to start next line with
 and where to display it, including a zero or negative hpos.
 The vpos field is not really a vpos; it is 1 unless the line is continued */

struct position val_display_text_line;

struct position *
display_text_line (w, start, vpos, hpos, taboffset)
     struct window *w;
     int start;
     int vpos;
     int hpos;
     int taboffset;
{
  register int pos = start;
  register int c;
  register char *p1;
  int end;
  register int pause;
  register unsigned char *p;
  char *endp;
  register char *startp;
  register char *p1prev;
  register struct display_line *line;
  int tab_width = XINT (bf_cur->tab_width);
  int ctl_arrow = !NULL (bf_cur->ctl_arrow);
  int width = XFASTINT (w->width) - 1
    - (XFASTINT (w->width) + XFASTINT (w->left) != screen_width);
  struct position val;
  int lastpos;
  int invis;
  int hscroll = XINT (w->hscroll);
  int truncate = hscroll
    || (truncate_partial_width_windows
	&& XFASTINT (w->width) < screen_width)
    || !NULL (bf_cur->truncate_lines);
  int selective
    = XTYPE (bf_cur->selective_display) == Lisp_Int
      ? XINT (bf_cur->selective_display)
	: !NULL (bf_cur->selective_display) ? -1 : 0;
  int selective_e = selective && !NULL (bf_cur->selective_display_ellipses);

  hpos += XFASTINT (w->left);
  line = get_display_line (vpos, XFASTINT (w->left));
  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  if (w == XWINDOW (minibuf_window) && start == 1
      && vpos == XFASTINT (w->top))
    {
      if (minibuf_prompt)
	hpos = display_string (w, line, minibuf_prompt, hpos,
			       !truncate ? '\\' : '$', -1, -1);
      minibuf_prompt_width = hpos;
    }

  p1 = line->body + hpos;

  end = NumCharacters + 1;

  startp = line->body + XFASTINT (w->left);
  endp = startp + width;

  /* Loop generating characters.
   Stop at end of buffer, before newline,
   or if reach or pass continuation column.  */

  pause = pos;
  while (p1 < endp)
    {
      p1prev = p1;
      if (pos == pause)
	{
	  if (pos == end)
	    break;
	  if (pos == point && point_vpos < 0)
	    {
	      point_vpos = vpos;
	      point_hpos = p1 - startp;
	    }

	  pause = end;
	  if (pos < point && point < pause)
	    pause = point;
	  if (pos <= bf_s1 && bf_s1 + 1 < pause)
	    pause = bf_s1 + 1;

	  p = &CharAt (pos);
	}
      c = *p++;
      if (c >= 040 && c < 0177)
	{
	  if (p1 >= startp)
	    *p1 = c;
	  p1++;
	}
      else if (c == '\n')
	{
	  invis = 0;
	  while (pos < end
		 && selective > 0
		 && position_indentation (pos + 1) >= selective)
	    {
	      invis = 1;
	      pos = find_next_newline (pos + 1, 1);
	      if (CharAt (pos - 1) == '\n')
		pos--;
	    }
	  if (invis && selective_e)
	    {
	      p1 += 4;
	      if (p1 - startp > width)
		p1 = endp;
	      strncpy (p1prev, " ...", p1 - p1prev);
	    }
	  break;
	}
      else if (c == '\t')
	{
	  do
	    {
	      if (p1 >= startp)
		*p1 = ' ';
	      p1++;
	    }
	  while ((p1 - startp + taboffset + hscroll - (hscroll > 0))
		 % tab_width);
	}
      else if (c == Ctl('M') && selective == -1)
	{
	  pos = find_next_newline (pos, 1);
	  if (CharAt (pos - 1) == '\n')
	    pos--;
	  if (selective_e)
	    {
	      p1 += 4;
	      if (p1 - startp > width)
		p1 = endp;
	      strncpy (p1prev, " ...", p1 - p1prev);
	    }
	  break;
	}
      else if (c < 0200 && ctl_arrow)
	{
	  if (p1 >= startp)
	    *p1 = '^';
	  p1++;
	  if (p1 >= startp)
	    *p1 = c ^ 0100;
	  p1++;
	}
      else
	{
	  if (p1 >= startp)
	    *p1 = '\\';
	  p1++;
	  if (p1 >= startp)
	    *p1 = (c >> 6) + '0';
	  p1++;
	  if (p1 >= startp)
	    *p1 = (7 & (c >> 3)) + '0';
	  p1++;
	  if (p1 >= startp)
	    *p1 = (7 & c) + '0';
	  p1++;
	}
      pos++;
    }

  val.hpos = - XINT (w->hscroll);
  if (val.hpos)
    val.hpos++;

  val.vpos = 1;

  lastpos = pos;

  /* Handle continuation in middle of a character */
  /* by backing up over it */
  if (p1 > endp)
    {
      /* Start the next line with that same character */
      pos--;
      /* but at a negative hpos, to skip the columns output on this line.  */
      val.hpos += p1prev - endp;
      /* Keep in this line everything up to the continuation column.  */
      p1 = endp;
    }

  /* Finish deciding which character to start the next line on,
     and what hpos to start it at.
     Also set `lastpos' to the last position which counts as "on this line"
     for cursor-positioning.  */

  if (pos < NumCharacters + 1)
    {
      if (CharAt (pos) == '\n')
	/* If stopped due to a newline, start next line after it */
	pos++;
      else
	/* Stopped due to right margin of window */
	{
	  if (truncate)
	    {
	      *p1++ = '$';
	      /* Truncating => start next line after next newline,
		 and point is on this line if it is before the newline,
		 and skip none of first char of next line */
	      pos = find_next_newline (pos, 1);
	      val.hpos = XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0;

	      lastpos = pos - (CharAt (pos - 1) == '\n');
	    }
	  else
	    {
	      *p1++ = '\\';
	      val.vpos = 0;
	      lastpos--;
	    }
	}
    }

  /* If point is at eol or in invisible text at eol,
     record its screen location now.  */

  if (start <= point && point <= lastpos && point_vpos < 0)
    {
      point_vpos = vpos;
      point_hpos = p1 - startp;
    }

  if (point_vpos == vpos)
    {
      if (point_hpos < 0) point_hpos = 0;
      if (point_hpos > width) point_hpos = width;
      point_hpos += XFASTINT (w->left);
      if (w == XWINDOW (selected_window))
	{
	  cursY = point_vpos;
	  cursX = point_hpos;

	  /* Line is not continued and did not start in middle of character */
	  if (hpos == (XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0)
	      && val.vpos)
	    {
	      this_line_bufpos = start;
	      this_line_buffer = bf_cur;
	      this_line_vpos = point_vpos;
	      this_line_start_hpos = hpos;
	      this_line_endpos = bf_s1 + bf_s2 + 1 - lastpos;
	    }
	  else
	    this_line_bufpos = 0;
	}
    }

  /* If hscroll and line not empty, insert truncation-at-left marker */
  if (hscroll && lastpos != start)
    {
      *startp = '$';
      if (p1 <= startp)
	p1 = startp + 1;
    }

  if (XFASTINT (w->width) + XFASTINT (w->left) != screen_width)
    {
      endp++;
      if (p1 < startp) p1 = startp;
      while (p1 < endp) *p1++ = ' ';
      *p1++ = '|';
    }
  line->length = max (line->length, p1 - line->body);
  line->body[line->length] = 0;

  /* If the start of this line is the overlay arrow-position,
     then put the arrow string into the display-line.  */

  if (XTYPE (Voverlay_arrow_position) == Lisp_Marker
      && bf_cur == XMARKER (Voverlay_arrow_position)->buffer
      && start == marker_position (Voverlay_arrow_position)
      && XTYPE (Voverlay_arrow_string) == Lisp_String)
    {
      unsigned char *p = XSTRING (Voverlay_arrow_string)->data;
      int len = XSTRING (Voverlay_arrow_string)->size;
      if (len > XFASTINT (w->width) - 1)
	len = XFASTINT (w->width) - 1;
      bcopy (p, startp, len);
      if (line->length < len + startp - line->body)
	line->length = len + startp - line->body;
    }

  val.bufpos = pos;
  val_display_text_line = val;
  return &val_display_text_line;
}

/* Display the mode line for window w */

display_mode_line (w)
     struct window *w;
{
  register int vpos = XFASTINT (w->height) + XFASTINT (w->top) - 1;
  register int left = XFASTINT (w->left);
  register int right = XFASTINT (w->width) + left;
  register struct display_line *line = get_display_line (vpos, left);

  display_mode_element (w, line, left, 0, right, right,
			bf_cur->mode_line_format);

  /* Make the mode line inverse video if the entire line
     is made of mode lines.
     I.e. if this window is full width,
     or if it is the child of a full width window
     (which implies that that window is split side-by-side
     and the rest of this line is mode lines of the sibling windows).  */
  if (XFASTINT (w->width) == screen_width ||
      XFASTINT (XWINDOW (w->parent)->width) == screen_width)
    line->highlighted = mode_line_inverse_video;

}

/* Contribute ELT to the mode line for window W.
   How it translates into text depends on its data type.

   LINE is the display-line that the mode line is being displayed in.

   HPOS is the position (absolute on screen) where this element's text
   should start.  The output is truncated automatically at the right
   edge of window W.

   DEPTH is the depth in recursion.  It is used to prevent
   infinite recursion here.

   MINENDCOL is the hpos before which the element may not end.
   The element is padded at the right with spaces if nec
   to reach this column.

   MAXENDCOL is the hpos past which this element may not extend.
   If MINENDCOL is > MAXENDCOL, MINENDCOL takes priority.
   (This is necessary to make nested padding and truncation work.)

   Returns the hpos of the end of the text generated by ELT.
   The next element will receive that value as its HPOS arg,
   so as to concatenate the elements.  */

int
display_mode_element (w, line, hpos, depth, minendcol, maxendcol, elt)
     struct window *w;
     struct display_line *line;
     register int hpos;
     int depth;
     int minendcol;
     register int maxendcol;
     register Lisp_Object elt;
{
 tail_recurse:
  if (depth > 10)
    goto invalid;

  depth++;

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (elt))
#else
  switch (XTYPE (elt))
#endif
    {
    case Lisp_String:
      {
	/* A string: output it and check for %-constructs within it.  */
	register unsigned char c;
	register unsigned char *this = XSTRING (elt)->data;

	while (hpos < maxendcol && *this)
	  {
	    unsigned char *last = this;
	    while ((c = *this++) != '\0' && c != '%')
	      ;
	    if (this - 1 != last)
	      {
		register int lim = --this - last + hpos;
		hpos = display_string (w, line, last, hpos, 0, hpos,
				       min (lim, maxendcol));
	      }
	    else /* c == '%' */
	      {
		register int spec_width = 0;

		/* We can't allow -ve args due to the "%-" construct */
		/* Argument specifies minwidth but not maxwidth
		   (maxwidth can be specified by
		     (<negative-number> . <stuff>) mode-line elements) */

		while ((c = *this++) >= '0' && c <= '9')
		  {
		    spec_width = spec_width * 10 + (c - '0');
		  }

		spec_width += hpos;
		if (spec_width > maxendcol)
		  spec_width = maxendcol;

		if (c == 'M')
		  hpos = display_mode_element (w, line, hpos, depth,
					       spec_width, maxendcol,
					       Vglobal_mode_string);
		else if (c != 0)
		  hpos = display_string (w, line,
					 decode_mode_spec (w, c,
							   maxendcol - hpos),
					 hpos, 0, spec_width, maxendcol);
	      }
	  }
      }
      break;

    case Lisp_Symbol:
      /* A symbol: process the value of the symbol recursively
	 as if it appeared here directly.  Avoid error if symbol void.
	 Special case: if value of symbol is a string, output the string
	 literally.  */
      {
	register Lisp_Object tem;
	tem = Fboundp (elt);
	if (!NULL (tem))
	  {
	    tem = Fsymbol_value (elt);
	    /* If value is a string, output that string literally:
	       don't check for % within it.  */
	    if (XTYPE (tem) == Lisp_String)
	      hpos = display_string (w, line, XSTRING (tem)->data,
				     hpos, 0, minendcol, maxendcol);
	    /* Give up right away for nil or t.  */
	    else if (!EQ (tem, elt))
	      { elt = tem; goto tail_recurse; }
	  }
      }
      break;

    case Lisp_Cons:
      {
	register Lisp_Object car, tem;

	/* A cons cell: three distinct cases.
	   If first element is a string or a cons, process all the elements
	   and effectively concatenate them.
	   If first element is a negative number, truncate displaying cdr to
	   at most that many characters.  If positive, pad (with spaces)
	   to at least that many characters.
	   If first element is a symbol, process the cadr or caddr recursively
	   according to whether the symbol's value is non-nil or nil.  */
	car = XCONS (elt)->car;
	if (XTYPE (car) == Lisp_Symbol)
	  {
	    tem = Fboundp (car);
	    elt = XCONS (elt)->cdr;
	    if (XTYPE (elt) != Lisp_Cons)
	      goto invalid;
	    /* elt is now the cdr, and we know it is a cons cell.
	       Use its car if CAR has a non-nil value.  */
	    if (!NULL (tem))
	      {
		tem = Fsymbol_value (car);
		if (!NULL (tem))
		  { elt = XCONS (elt)->car; goto tail_recurse; }
	      }
	    /* Symbol's value is nil (or symbol is unbound)
	       Get the cddr of the original list
	       and if possible find the caddr and use that.  */
	    elt = XCONS (elt)->cdr;
	    if (NULL (elt))
	      break;
	    else if (XTYPE (elt) != Lisp_Cons)
	      goto invalid;
	    elt = XCONS (elt)->car;
	    goto tail_recurse;
	  }
	else if (XTYPE (car) == Lisp_Int)
	  {
	    register int lim = XINT (car);
	    elt = XCONS (elt)->cdr;
	    if (lim < 0)
	      /* Negative int means reduce maximum width.
		 DO NOT change MINENDCOL here!
		 (20 -10 . foo) should truncate foo to 10 col
		 and then pad to 20.  */
	      maxendcol = min (maxendcol, hpos - lim);
	    else if (lim > 0)
	      {
		/* Padding specified.  Don't let it be more than
		   current maximum.  */
		lim += hpos;
		if (lim > maxendcol)
		  lim = maxendcol;
		/* If that's more padding than already wanted, queue it.
		   But don't reduce padding already specified even if
		   that is beyond the current truncation point.  */
		if (lim > minendcol)
		  minendcol = lim;
	      }
	    goto tail_recurse;
	  }
	else if (XTYPE (car) == Lisp_String || XTYPE (car) == Lisp_Cons)
	  {
	    register int limit = 50;
	    /* LIMIT is to protect against circular lists.  */
	    while (XTYPE (elt) == Lisp_Cons && --limit > 0
		   && hpos < maxendcol)
	      {
		hpos = display_mode_element (w, line, hpos, depth,
					     hpos, maxendcol,
					     XCONS (elt)->car);
		elt = XCONS (elt)->cdr;
	      }
	  }
      }
      break;

    default:
    invalid:
      return (display_string (w, line, "*invalid*", hpos, 0,
			      minendcol, maxendcol));
    }

 end:
  if (minendcol > hpos)
    hpos = display_string (w, line, "", hpos, 0, minendcol, -1);
  return hpos;
}


char decode_mode_spec_buf[MScreenWidth + 1];

static char *
fmodetrunc (str, width, buf)
     char *str, *buf;
     long width;
{
  register char *bp = buf;
  register long len;
    
  len = strlen (str);
  if (width && width < len)
    {
      strcpy (buf, str + len - width);
      if (buf[0] != '/')
	while (*bp)
	  if (*bp++ == '/')
	    {
	      bp--;
	      *--bp = '$';
	      return bp;
	    }
      buf[0] = '$';
      return buf;
    }
  return str;
}

char *
decode_mode_spec (w, c, maxwidth)
     struct window *w;
     register char c;
     register int maxwidth;
{
  Lisp_Object obj = Qnil;
  char *tbuf = decode_mode_spec_buf;

  switch (c)
    {
    case 'b': 
      obj = bf_cur->name;
      if (maxwidth >= 3 && XSTRING (obj)->size > maxwidth)
	{
	  bcopy (XSTRING (obj)->data, tbuf, maxwidth - 1);
	  tbuf[maxwidth - 1] = '\\';
	  tbuf[maxwidth] = '\0';
	  return tbuf;
	}
      break;

    case 'f': 
      obj = bf_cur->filename;
      if (NULL (obj))
	return "[none]";
      else if (XTYPE (obj) == Lisp_String)
	return fmodetrunc (XSTRING (obj)->data, maxwidth, tbuf);
      break;

    case 'm': 
      obj = bf_cur->mode_name;
      break;

    case 'n':
      if (bf_head_clip > 1 || bf_tail_clip > 0)
	return " Narrow";
      break;

/*  Handled specially in display_mode_element
    case 'M': 
      obj = Vglobal_mode_string;
      break;
 */

    case '*':
      return (!NULL (bf_cur->read_only)
	      ? "%"
	      : ((bf_modified > bf_cur->save_modified)
		 ? "*"
		 : "-"));

    case 's':
      /* status of process */
#ifdef subprocesses
      obj = Fget_buffer_process (Fcurrent_buffer ());
      if (NULL (obj))
	return "no process";
      obj = Fsymbol_name (Fprocess_status (obj));
      break;
#else
      return "no processes";
#endif /* subprocesses */

    case 'p':
      {
	int pos = marker_position (w->start);
	int total = NumCharacters + 1 - FirstCharacter;

	if (XFASTINT (w->window_end_pos) <= bf_tail_clip)
	  {
	    if (pos <= FirstCharacter)
	      return "All";
	    else
	      return "Bottom";
	  }
	else if (pos <= FirstCharacter)
	  return "Top";
	else
	  {
	    total = ((pos - FirstCharacter) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    sprintf (tbuf, "%2d%%", total);
	    return tbuf;
	  }
      }

    case '[': 
      if (RecurseDepth - MinibufDepth > 10)
	return "[[[... ";
      else
	return ("[[[[[[[[[[" + 10 - (RecurseDepth - MinibufDepth));

    case '%':
      return "%";

    case ']': 
      if (RecurseDepth - MinibufDepth > 10)
	return " ...]]]";
      else
	return ("]]]]]]]]]]" + 10 - (RecurseDepth - MinibufDepth));

    case '-':
      return "--------------------------------------------------------------------------------------------------------------------------------------------";
    }

  if (XTYPE (obj) == Lisp_String)
    return (char *) XSTRING (obj)->data;
  else
    return "";
}

/* Display STRING on one line of window W, starting at HPOS.
   Display on the display_line LINE, which should have
   been obtained by get_display_line (vpos, hpos)
   or in some suitable manner.

  TRUNCATE is character to display at end if truncated.  Zero for none.

  MINCOL is the first column ok to end at.  (Pad with spaces to this col.)
  MAXCOL is the last column ok to end at.  Truncate here.
    -1 for MINCOL or MAXCOL means no explicit minimum or maximum.
  Both count from the left edge of the screen, as does HPOS.
  The right edge of W is an implicit maximum.
  If TRUNCATE is nonzero, the implicit maximum is one column before the edge.

  Returns ending hpos */

display_string (w, line, string, hpos, truncate, mincol, maxcol)
     struct window *w;
     register struct display_line *line;
     unsigned char *string;
     int hpos;
     char truncate;
     int mincol, maxcol;
{
  register int c;
  register unsigned char *p1;
  int hscroll = XINT (w->hscroll);
  int tab_width = XINT (bf_cur->tab_width);
  register unsigned char *start;
  register unsigned char *end;
  unsigned char *p1start = (unsigned char *) line->body + hpos;
  int window_width = XFASTINT (w->width);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  p1 = p1start;
  start = (unsigned char *) line->body + XFASTINT (w->left);
  end = start + window_width - (truncate != 0);

  if ((window_width + XFASTINT (w->left)) != screen_width)
    *end-- = '|';

  if (maxcol >= 0 && end - (unsigned char *) line->body > maxcol)
    end = (unsigned char *) line->body + maxcol;
  if (maxcol >= 0 && mincol > maxcol)
    mincol = maxcol;

  while (p1 < end)
    {
      c = *string++;
      if (!c) break;
      if (c >= 040 && c < 0177)
	{
	  if (p1 >= start)
	    *p1 = c;
	  p1++;
	}
      else if (c == '\t')
	{
	  do
	    {
	      if (p1 >= start)
		*p1 = ' ';
	      p1++;
	    }
	  while ((p1 - start + hscroll - (hscroll > 0)) % tab_width);
	}
      else if (c < 0200 && buffer_defaults.ctl_arrow)
	{
	  if (p1 >= start)
	    *p1 = '^';
	  p1++;
	  if (p1 >= start)
	    *p1 = c ^ 0100;
	  p1++;
	}
      else
	{
	  if (p1 >= start)
	    *p1 = '\\';
	  p1++;
	  if (p1 >= start)
	    *p1 = (c >> 6) + '0';
	  p1++;
	  if (p1 >= start)
	    *p1 = (7 & (c >> 3)) + '0';
	  p1++;
	  if (p1 >= start)
	    *p1 = (7 & c) + '0';
	  p1++;
	}
    }

  if (c)
    {
      p1 = end;
      if (truncate) *p1++ = truncate;
    }
  else if (mincol >= 0)
    {
      end = (unsigned char *) line->body + mincol;
      while (p1 < end)
	*p1++ = ' ';
    }

  {
    register int len = p1 - (unsigned char *) line->body;
    if (len > line->length)
      line->length = len;
    line->body[line->length] = 0;
    return len;
  }
}

syms_of_xdisp ()
{
  staticpro (&last_arrow_position);
  staticpro (&last_arrow_string);
  last_arrow_position = Qnil;
  last_arrow_string = Qnil;

  DEFVAR_LISP ("global-mode-string", &Vglobal_mode_string,
    "String displayed by mode-line-format's \"%m\" specifiation.");
  Vglobal_mode_string = Qnil;

  DEFVAR_LISP ("overlay-arrow-position", &Voverlay_arrow_position,
    "Marker for where to display an arrow on top of the buffer text.\n\
This must be the beginning of a line in order to work.\n\
See also overlay-arrow-string.");
  Voverlay_arrow_position = Qnil;

  DEFVAR_LISP ("overlay-arrow-string", &Voverlay_arrow_string,
    "String to display as an arrow.  See also overlay-arrow-position.");
  Voverlay_arrow_string = Qnil;

  DEFVAR_INT ("scroll-step", &scroll_step,
    "*The number of lines to try scrolling a window by when point moves out.\n\
If that fails to bring point back on screen, point is centered instead.\n\
If this is zero, point is always centered after it moves off screen.");

  DEFVAR_BOOL ("reset-terminal-on-clear", &reset_terminal_on_clear,
    "Non-nil means re-init terminal modes for clear screen as on entry to Emacs.");
  reset_terminal_on_clear = 1;

  DEFVAR_INT ("debug-end-pos", &debug_end_pos, "Don't ask");

  DEFVAR_BOOL ("truncate-partial-width-windows",
	       &truncate_partial_width_windows,
    "*Non-nil means truncate lines in all windows less than full screen wide.");
  truncate_partial_width_windows = 1;

  DEFVAR_BOOL ("mode-line-inverse-video", &mode_line_inverse_video,
    "*Non-nil means use inverse video, or other suitable display mode, for the mode line.");
  mode_line_inverse_video = 1;

  defsubr (&Sredraw_display);
}

/* initialize the window system */
init_xdisp ()
{
  Lisp_Object root_window;
#ifndef COMPILER_REGISTER_BUG
  register
#endif COMPILER_REGISTER_BUG
    struct window *mini_w;

  this_line_bufpos = 0;

  mini_w = XWINDOW (minibuf_window);
  root_window = mini_w->prev;

  minibuf_message = 0;
  prev_minibuf_message = 0;

  if (!noninteractive)
    {
      XFASTINT (XWINDOW (root_window)->top) = 0;
      set_window_height (root_window, screen_height - 1, 0);
      XFASTINT (mini_w->top) = screen_height - 1;
      set_window_height (minibuf_window, 1, 0);

      XFASTINT (XWINDOW (root_window)->width) = screen_width;
      XFASTINT (mini_w->width) = screen_width;
    }
}
