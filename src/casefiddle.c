/* GNU Emacs case conversion functions.
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
#include "buffer.h"
#include "commands.h"
#include "syntax.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

Lisp_Object
casify_object (flag, obj)
     enum case_action flag;
     Lisp_Object obj;
{
  register int i, c, len;
  register int inword = flag == CASE_DOWN;

  while (1)
    {
      if (XTYPE (obj) == Lisp_Int)
	{
	  c = XINT (obj);
	  if (c >= 0 && c <= 0400)
	    {
	      if (inword)
		XFASTINT (obj) = DOWNCASE (c);
	      else if (!UPPERCASEP (c))
		XFASTINT (obj) = UPCASE1 (c);
	    }
	  return obj;
	}
      if (XTYPE (obj) == Lisp_String)
	{
	  obj = Fcopy_sequence (obj);
	  len = XSTRING (obj)->size;
	  for (i = 0; i < len; i++)
	    {
	      c = XSTRING (obj)->data[i];
	      if (inword)
		c = DOWNCASE (c);
	      else if (!UPPERCASEP (c))
		c = UPCASE1 (c);
	      XSTRING (obj)->data[i] = c;
	      if (flag == CASE_CAPITALIZE)
		inword = SYNTAX (c) == Sword;
	    }
	  return obj;
	}
      obj = wrong_type_argument (Qchar_or_string_p, obj, 0);
    }
}

DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0,
  "One arg, a character or string.  Convert it to upper case and return that.")
  (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_UP, obj);
}

DEFUN ("downcase", Fdowncase, Sdowncase, 1, 1, 0,
  "One arg, a character or string.  Convert it to lower case and return that.")
  (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_DOWN, obj);
}

DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 1, 0,
  "One arg, a character or string.  Convert it to capitalized form and return that.\n\
This means that each word's first character is upper case and the rest is lower case.")
  (obj)
     Lisp_Object obj;
{
  return casify_object (CASE_CAPITALIZE, obj);
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   b and e specify range of buffer to operate on. */

casify_region (flag, b, e)
     enum case_action flag;
     Lisp_Object b, e;
{
  register int i;
  register int c;
  register int inword = flag == CASE_DOWN;

  validate_region (&b, &e);
  prepare_to_modify_buffer ();
  record_change (XFASTINT (b), XFASTINT (e) - XFASTINT (b));

  for (i = XFASTINT (b); i < XFASTINT (e); i++)
    {
      c = CharAt (i);
      if (inword && flag != CASE_CAPITALIZE_UP)
	c = DOWNCASE (c);
      else if (!UPPERCASEP (c)
	       && (!inword || flag != CASE_CAPITALIZE_UP))
	c = UPCASE1 (c);
      CharAt (i) = c;
      if ((int) flag >= (int) CASE_CAPITALIZE)
	inword = SYNTAX (c) == Sword;
    }

  modify_region (XFASTINT (b), XFASTINT (e));
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 2, "r",
  "Convert the region to upper case.  In programs, wants two arguments.\n\
These arguments specify the starting and ending character numbers of\n\
the region to operate on.  When used as a command, the text between\n\
point and the mark is operated on.")
  (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_UP, b, e);
  return Qnil;
}

DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 2, "r",
  "Convert the region to lower case.  In programs, wants two arguments.\n\
These arguments specify the starting and ending character numbers of\n\
the region to operate on.  When used as a command, the text between\n\
point and the mark is operated on.")
  (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_DOWN, b, e);
  return Qnil;
}

DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 2, "r",
  "Convert the region to upper case.  In programs, wants two arguments.\n\
These arguments specify the starting and ending character numbers of\n\
the region to operate on.  When used as a command, the text between\n\
point and the mark is operated on.\n\
Capitalized form means each word's first character is upper case\n\
and the rest of it is lower case.")
  (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_CAPITALIZE, b, e);
  return Qnil;
}

/* Like Fcapitalize but change only the initials.  */

Lisp_Object
upcase_initials_region (b, e)
     Lisp_Object b, e;
{
  casify_region (CASE_CAPITALIZE_UP, b, e);
  return Qnil;
}

void
operate_on_word (flag, arg)
     int flag;
     Lisp_Object arg;
{
  Lisp_Object beg, end;
  int farend;
  CHECK_NUMBER (arg, 0);
  farend = scan_words (point, XINT (arg));
  if (!farend)
    farend = XINT (arg) > 0 ? NumCharacters + 1 : FirstCharacter;
  XFASTINT (beg) = point < farend ? point : farend;
  XFASTINT (end) = point > farend ? point : farend;
  casify_region (flag, beg, end);
  SetPoint (XFASTINT (end));
}

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 1, "p",
  "Convert following word (or ARG words) to upper case, moving over.\n\
With negative argument, convert previous words but do not move.")
  (arg)
     Lisp_Object arg;
{
  operate_on_word (CASE_UP, arg);
  return Qnil;
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p",
  "Convert following word (or ARG words) to lower case, moving over.\n\
With negative argument, convert previous words but do not move.")
  (arg)
     Lisp_Object arg;
{
  operate_on_word (CASE_DOWN, arg);
  return Qnil;
}

DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 1, "p",
  "Capitalize the following word (or ARG words), moving over.\n\
This gives the word(s) a first character in upper case\n\
and the rest lower case.\n\
With negative argument, capitalize previous words but do not move.")
  (arg)
     Lisp_Object arg;
{
  operate_on_word (CASE_CAPITALIZE, arg);
  return Qnil;
}

syms_of_casefiddle ()
{
  defsubr (&Supcase);
  defsubr (&Sdowncase);
  defsubr (&Scapitalize);
  defsubr (&Supcase_region);
  defsubr (&Sdowncase_region);
  defsubr (&Scapitalize_region);
  defsubr (&Supcase_word);
  defsubr (&Sdowncase_word);
  defsubr (&Scapitalize_word);
}

keys_of_casefiddle ()
{
  defkey (CtlXmap, Ctl('U'), "upcase-region");
  defkey (CtlXmap, Ctl('L'), "downcase-region");
  defkey (ESCmap, 'u', "upcase-word");
  defkey (ESCmap, 'l', "downcase-word");
  defkey (ESCmap, 'c', "capitalize-word");
}
