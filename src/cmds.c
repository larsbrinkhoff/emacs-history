/* Simple built-in editing commands.
   Copyright (C) 1985 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "syntax.h"

Lisp_Object Qkill_forward_chars, Qkill_backward_chars, Vblink_paren_hook;


DEFUN ("forward-char", Fforward_char, Sforward_char, 0, 1, "p",
  "Move point right ARG characters (left if ARG negative).\n\
On reaching end of buffer, stop and signal error.")
  (n)
     Lisp_Object n;
{
  if (NULL (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  SetPoint (point + XINT (n));
  if (point < FirstCharacter)
    {
      SetPoint (FirstCharacter);
      Fsignal (Qbeginning_of_buffer, Qnil);
    }
  if (point > NumCharacters + 1)
    {
      SetPoint (NumCharacters + 1);
      Fsignal (Qend_of_buffer, Qnil);
    }
  return Qnil;
}

DEFUN ("backward-char", Fbackward_char, Sbackward_char, 0, 1, "p",
  "Move point left ARG characters (right if ARG negative).\n\
On attempt to pass beginning or end of buffer, stop and signal error.")
  (n)
     Lisp_Object n;
{
  if (NULL (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  XSETINT (n, - XINT (n));
  return Fforward_char (n);
}

DEFUN ("forward-line", Fforward_line, Sforward_line, 0, 1, "p",
  "If point is on line i, move to the start of line i + ARG.\n\
If there isn't room, go as far as possible (no error).\n\
Returns the count of lines left to move.\n\
With positive ARG, a non-empty line at the end counts as one line\n\
  successfully moved (for the return value).")
  (n)
     Lisp_Object n;
{
  int pos2 = point;
  int pos;
  int count, shortage, negp;

  if (NULL (n))
    count = 1;
  else
    {
      CHECK_NUMBER (n, 0);
      count = XINT (n);
    }

  negp = count <= 0;
  pos = scan_buffer ('\n', pos2, count - negp, &shortage);
  if (shortage > 0
      && (negp
	  || (NumCharacters >= FirstCharacter
	      && CharAt (pos - 1) != '\n')))
    shortage--;
  SetPoint (pos);
  return make_number (negp ? - shortage : shortage);
}

DEFUN ("beginning-of-line", Fbeginning_of_line, Sbeginning_of_line,
  0, 1, "p",
  "Move point to beginning of current line.\n\
With argument ARG not nil or 1, move forward ARG - 1 lines first.\n\
If scan reaches end of buffer, stop there without error.")
  (n)
     Lisp_Object n;
{
  if (NULL (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  Fforward_line (make_number (XINT (n) - 1));
  return Qnil;
}

DEFUN ("end-of-line", Fend_of_line, Send_of_line,
  0, 1, "p",
  "Move point to end of current line.\n\
With argument ARG not nil or 1, move forward ARG - 1 lines first.\n\
If scan reaches end of buffer, stop there without error.")
  (n)
     Lisp_Object n;
{
  register int pos;
  register int stop;

  if (NULL (n))
    XFASTINT (n) = 1;
  else
    CHECK_NUMBER (n, 0);

  if (XINT (n) != 1)
    Fforward_line (make_number (XINT (n) - 1));

  pos = point;
  stop = NumCharacters + 1;
  while (pos < stop && CharAt (pos) != '\n') pos++;
  SetPoint (pos);

  return Qnil;
}

DEFUN ("delete-char", Fdelete_char, Sdelete_char, 1, 2, "p\nP",
  "Delete the following ARG characters (previous, with negative arg).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, ARG is the prefix arg, and KILLFLAG is set if\n\
ARG was explicitly specified.")
  (n, killflag)
     Lisp_Object n, killflag;
{
  CHECK_NUMBER (n, 0);

  if (NULL (killflag))
    {
      if (XINT (n) < 0)
	{
	  if (point + XINT (n) < FirstCharacter)
	    Fsignal (Qbeginning_of_buffer, Qnil);
	  else
	    del_range (point + XINT (n), point);
	}
      else
	{
	  if (point + XINT (n) > NumCharacters + 1)
	    Fsignal (Qend_of_buffer, Qnil);
	  else
	    del_range (point, point + XINT (n));
	}
    }
  else
    {
      call1 (Qkill_forward_chars, n);
    }
  return Qnil;
}

DEFUN ("delete-backward-char", Fdelete_backward_char, Sdelete_backward_char,
  1, 2, "p\nP",
  "Delete the previous ARG characters (following, with negative ARG).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, ARG is the prefix arg, and KILLFLAG is set if\n\
ARG was explicitly specified.")
  (n, killflag)
     Lisp_Object n, killflag;
{
  CHECK_NUMBER (n, 0);
  return Fdelete_char (make_number (-XINT (n)), killflag);
}

DEFUN ("self-insert-command", Fself_insert_command, Sself_insert_command, 1, 1, "p",
  "Insert this character.  Prefix arg is repeat-count.")
  (arg)
     Lisp_Object arg;
{
  CHECK_NUMBER (arg, 0);

  while (XINT (arg) > 0)
    {
      XFASTINT (arg)--;		/* Ok since old and new vals both nonneg */
      SelfInsert (last_command_char, XFASTINT (arg) != 0);
    }
  return Qnil;
}

DEFUN ("newline", Fnewline, Snewline, 0, 1, "P",
  "Insert a newline.  With arg, insert that many newlines.\n\
In Auto Fill mode, can break the preceding line if no numeric arg.")
  (arg1)
     Lisp_Object arg1;
{
  int flag;
  Lisp_Object arg;
  char c1 = '\n';

  arg = Fprefix_numeric_value (arg1);

  if (!NULL (bf_cur->read_only))
    Fsignal (Qbuffer_read_only, Qnil);

  /* Inserting a newline at the end of a line
     produces better redisplay in try_window_id
     than inserting at the ebginning fo a line,
     And the textual result is the same.
     So if at beginning, pretend to be at the end.
     Must avoid SelfInsert in that case since point is wrong.
     Luckily SelfInsert's special features all do nothing in that case.  */

  flag = point > FirstCharacter && CharAt (point - 1) == '\n';
  if (flag) PointLeft (1);

  while (XINT (arg) > 0)
    {
      if (flag)
	InsCStr (&c1, 1);
      else
	SelfInsert ('\n', !NULL (arg1));
      XFASTINT (arg)--;		/* Ok since old and new vals both nonneg */
    }

  if (flag) PointRight (1);

  return Qnil;
}

SelfInsert (c1, noautofill)
     char c1;
     int noautofill;
{
  extern Lisp_Object Fexpand_abbrev ();
  int hairy = 0;
  Lisp_Object tem;
  register enum syntaxcode synt;
  register int c = c1;

  if (!NULL (bf_cur->overwrite_mode)
      && point <= NumCharacters
      && c != '\n' && CharAt (point) != '\n'
      && (CharAt (point) != '\t'
	  || XINT (bf_cur->tab_width) <= 0
	  || !((current_column () + 1) % XFASTINT (bf_cur->tab_width))))
    {
      del_range (point, point + 1);
      hairy = 1;
    }
  if (!NULL (bf_cur->abbrev_mode)
      && SYNTAX (c) != Sword
      && NULL (bf_cur->read_only)
      && point > FirstCharacter && SYNTAX (CharAt (point - 1)) == Sword)
    {
      tem = Fexpand_abbrev ();
      if (!NULL (tem))
	hairy = 1;
    }
  if ((c == ' ' || c == '\n')
      && !noautofill
      && !NULL (bf_cur->auto_fill_hook)
      && current_column () > XFASTINT (bf_cur->fill_column))
    {
      if (c1 != '\n')
	InsCStr (&c1, 1);
      call0 (bf_cur->auto_fill_hook);
      if (c1 == '\n')
	InsCStr (&c1, 1);
      hairy = 1;
    }
  else
    InsCStr (&c1, 1);
  synt = SYNTAX (c);
  if ((synt == Sclose || synt == Smath)
      && !NULL (Vblink_paren_hook) && INTERACTIVE)
    {
      call0 (Vblink_paren_hook);
      hairy = 1;
    }
  return hairy;
}

/* module initialization */

syms_of_cmds ()
{
  Qkill_backward_chars = intern ("kill-backward-chars");
  staticpro (&Qkill_backward_chars);

  Qkill_forward_chars = intern ("kill-forward-chars");
  staticpro (&Qkill_forward_chars);

  DEFVAR_LISP ("blink-paren-hook", &Vblink_paren_hook,
    "Function called, if non-nil, whenever a char with closeparen syntax is self-inserted.");
  Vblink_paren_hook = Qnil;

  defsubr (&Sforward_char);
  defsubr (&Sbackward_char);
  defsubr (&Sforward_line);
  defsubr (&Sbeginning_of_line);
  defsubr (&Send_of_line);

  defsubr (&Sdelete_char);
  defsubr (&Sdelete_backward_char);

  defsubr (&Sself_insert_command);
  defsubr (&Snewline);
}

keys_of_cmds ()
{
  int n;

  defkey (GlobalMap, Ctl('M'), "newline");
  defkey (GlobalMap, Ctl('I'), "self-insert-command");
  for (n = 040; n < 0177; n++)
    defkey (GlobalMap, n, "self-insert-command");

  defkey (GlobalMap, Ctl ('A'), "beginning-of-line");
  defkey (GlobalMap, Ctl ('B'), "backward-char");
  defkey (GlobalMap, Ctl ('D'), "delete-char");
  defkey (GlobalMap, Ctl ('E'), "end-of-line");
  defkey (GlobalMap, Ctl ('F'), "forward-char");
  defkey (GlobalMap, 0177, "delete-backward-char");
}
