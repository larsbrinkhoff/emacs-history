/* Buffer insertion/deletion and gap motion for GNU Emacs.
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
#include "lisp.h"
#include "buffer.h"
#include "window.h"

/* Move gap to position `pos'. */

GapTo (pos)
     int pos;
{
  if (pos <= bf_s1)
    gap_left (pos);
  else if (pos > bf_s1 + 1)
    gap_right (pos);
}

gap_left (pos)
     register int pos;
{
  register unsigned char *to, *from;
  register int i;

  pos--;

  if (unchanged_modified == bf_modified)
    {
      beg_unchanged = pos;
      end_unchanged = bf_s1 + bf_s2 - pos;
    }
  else
    {
      if (bf_s2 < end_unchanged)
	end_unchanged = bf_s2;
      if (pos < beg_unchanged)
	beg_unchanged = pos;
    }

  adjust_markers (pos + 1, bf_s1 + 1, bf_gap);

  to = bf_p2;
  from = bf_p1;

  i = bf_s1 + 1;
  while (--i > pos)
    to[i] = from[i];

  bf_s2 += bf_s1 - pos;
  bf_s1 = pos;
}

gap_right (pos)
     register int pos;
{
  register unsigned char *to, *from;
  register int i;

  pos--;

  if (unchanged_modified == bf_modified)
    {
      beg_unchanged = pos;
      end_unchanged = bf_s1 + bf_s2 - pos;
    }
  else
    {
      if (bf_s1 + bf_s2 - pos < end_unchanged)
	end_unchanged = bf_s1 + bf_s2 - pos;
      if (bf_s1 < beg_unchanged)
	beg_unchanged = bf_s1;
    }

  adjust_markers (bf_s1 + bf_gap + 1, pos + bf_gap + 1, - bf_gap);

  from = bf_p2;
  to = bf_p1;

  i = bf_s1;
  while (++i <= pos)
    to[i] = from[i];

  bf_s2 += bf_s1 - pos;
  bf_s1 = pos;
}

adjust_markers (from, to, amount)
     register int from, to, amount;
{
  Lisp_Object marker;
  register struct Lisp_Marker *m;
  register int mpos;

  marker = bf_cur->markers;

  while (!NULL (marker))
    {
      m = XMARKER (marker);
      mpos = m->bufpos;
      if (amount > 0)
	{
	  if (mpos > to && mpos < to + amount)
	    mpos = to + amount;
	}
      else
	{
	  if (mpos > from + amount && mpos <= from)
	    mpos = from + amount;
	}
      if (mpos > from && mpos <= to)
	mpos += amount;
      if (m->bufpos != mpos)
	m->bufpos = mpos, m->modified++;
      marker = m->chain;
    }
}

/* make sure that the gap in the current buffer is at least k
   characters wide */

make_gap (k)
     int k;
{
  register unsigned char *p1, *p2, *lim;

  if (bf_gap >= k)
    return;

  k += 2000;			/* Get more than just enough */

  p1 = (unsigned char *) realloc (bf_p1 + 1, bf_s1 + bf_s2 + bf_gap + k);
  if (p1 == 0)
    memory_full ();

  /* Record new location of text */
  bf_p1 = p1 - 1;

  /* Transfer the new free space from the end to the gap
     by shifting the second segment upward */
  p2 = bf_p1 + 1 + bf_s1 + bf_s2 + bf_gap;
  p1 = p2 + k;
  lim = p2 - bf_s2;
  while (lim < p2)
      *--p1 = *--p2;

  /* Finish updating text location data */
  bf_gap += k;
  bf_p2 = bf_p1 + bf_gap;

  /* Don't wait for next SetBfp; make it permanent now. */
  bf_cur->text = bf_text;

  /* adjust markers */
  adjust_markers (bf_s1 + 1, bf_s1 + bf_s2 + bf_gap + 1, k);
}

/* Insert the character c before dot */

insert_char (c)
     unsigned char c;
{
  InsCStr (&c, 1);
}

/* Insert the null-terminated string s before dot */

InsStr (s)
     char *s;
{
  InsCStr (s, strlen (s));
}

/* Insert a string of specified length before dot */

InsCStr (string, length)
     register unsigned char *string;
     register length;
{
  register unsigned char *end = string + length;

  if (length<1)
    return;
  if (!NULL (bf_cur->read_only))
    Fbarf_if_buffer_read_only();
  if (dot != bf_s1 + 1)
    GapTo (dot);
  if (bf_gap < length)
    make_gap (length);
  RecordInsert (dot, length);
  bcopy (string, bf_p1 + dot, length);

  bf_gap -= length;
  bf_p2 -= length;
  bf_s1 += length;
  dot += length;
  bf_modified++;
}

/* Delete characters in current buffer
  from `from' up to (but not incl) `to' */

del_range (from, to)
     register int from, to;
{
  register int numdel;

  /* Make args be valid */
  if (from < FirstCharacter)
    from = FirstCharacter;
  if (to > NumCharacters)
    to = NumCharacters + 1;

  if ((numdel = to - from) <= 0)
    return;

  if (!NULL (bf_cur->read_only))
    Fbarf_if_buffer_read_only();

  if (from < dot)
    {
      if (dot < to)
	dot = from;
      else
	dot -= numdel;
    }

  /* Make sure the gap is somewhere in or next to what we are deleting */
  if (from - 1 > bf_s1)
    gap_right (from);
  if (to - 1 < bf_s1)
    gap_left (to);
  
  RecordDelete (from, numdel);
  bf_modified++;

  bf_gap += numdel;
  bf_p2 += numdel;
  bf_s2 -= to - 1 - bf_s1;
  bf_s1 = from - 1;

  if (bf_s1 < beg_unchanged)
    beg_unchanged = bf_s1;
  if (bf_s2 < end_unchanged)
    end_unchanged = bf_s2;
}

modify_region (start, end)
     int start, end;
{
  if (!NULL (bf_cur->read_only))
    Fbarf_if_buffer_read_only();
  bf_modified++;
  if (start - 1 < beg_unchanged || unchanged_modified == bf_modified)
    beg_unchanged = start - 1;
  if (bf_s1 + bf_s2 + 1 - end < end_unchanged
      || unchanged_modified == bf_modified)
    end_unchanged = bf_s1 + bf_s2 + 1 - end;
}
