/* Record indices of function doc strings stored in a file.
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
#include <sys/file.h>

#ifdef USG
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#define index strchr
#else
#include <strings.h>
#endif

Lisp_Object Vdoc_file_name;

Lisp_Object
get_doc_string (filepos)
     long filepos;
{
  char buf[512 * 32 + 1];
  register int fd;
  register char *name;
  register char *p, *p1;
  register int count;

  if (XTYPE (Vexec_directory) != Lisp_String
      || XTYPE (Vdoc_file_name) != Lisp_String)
    return Qnil;

  name = (char *) alloca (XSTRING (Vexec_directory)->size
			  + XSTRING (Vdoc_file_name)->size + 2);
  strcpy (name, XSTRING (Vexec_directory)->data);
  strcat (name, XSTRING (Vdoc_file_name)->data);

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    error ("Cannot open doc string file");
  if (0 > lseek (fd, filepos, 0))
    {
      close (fd);
      error ("Position out of range in doc string file");
    }
  p = buf;
  while (p != buf + sizeof buf - 1)
    {
      count = read (fd, p, 512);
      p[count] = 0;
      if (!count)
	break;
      p1 = index (p, '\037');
      if (p1)
	{
	  *p1 = 0;
	  p = p1;
	  break;
	}
      p += count;
    }
  close (fd);
  return make_string (buf, p - buf);
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 1,
  "fDocumentation of function: ",
  "Return the documentation string of FUNCTION.")
  (fun1)
     Lisp_Object fun1;
{
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object tem;
  Lisp_Object val;

  fun = fun1;
  while (XTYPE (fun) == Lisp_Symbol)
    fun = Fsymbol_function (fun);
  if (XTYPE (fun) == Lisp_Subr)
    {
      if (XSUBR (fun)->doc == 0) return Qnil;
      if ((int) XSUBR (fun)->doc >= 0)
	return Fsubstitute_command_keys (build_string (XSUBR (fun)->doc));
      return Fsubstitute_command_keys (get_doc_string (- (int) XSUBR (fun)->doc));
    }
  if (XTYPE (fun) == Lisp_Vector)
    return build_string ("Prefix-command definition (a Lisp vector of subcommands).");
  if (XTYPE (fun) == Lisp_String)
    return build_string ("Keyboard macro.");
  if (!LISTP(fun))
    Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  funcar = Fcar (fun);
  if (XTYPE (funcar) != Lisp_Symbol)
    Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  if (XSYMBOL (funcar) == XSYMBOL (Qkeymap))
    return build_string ("Prefix-command definition (A list whose cdr is an alist of subcommands.");
  if (XSYMBOL (funcar) == XSYMBOL (Qlambda)
      || XSYMBOL (funcar) == XSYMBOL (Qautoload))
    {
      tem = Fcar (Fcdr (Fcdr (fun)));
      if (XTYPE (tem) == Lisp_String)
	return Fsubstitute_command_keys (tem);
      if (XTYPE (tem) == Lisp_Int && XINT (tem) >= 0)
	return Fsubstitute_command_keys (get_doc_string (XFASTINT (tem)));
      return Qnil;
    }
  if (XSYMBOL (funcar) == XSYMBOL (Qmocklisp))
    return Qnil;
  if (XSYMBOL (funcar) == XSYMBOL (Qmacro))
    return Fdocumentation (Fcdr (fun));
  else
    Fsignal (Qinvalid_function, Fcons (fun, Qnil));
}

DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
  1, 1, 0,
  "Used during Emacs initialization, before dumping runnable Emacs,\n\
to find pointers to doc strings stored in etc/DOCSTR... and\n\
record them in function definitions.\n\
One arg, FILENAME, a string which does not include a directory.\n\
The file is found in ../etc now; found in the exec-directory\n\
when doc strings are referred to later in the dumped Emacs.")
  (filename)
     Lisp_Object filename;
{
  int fd;
  char buf[1024 + 1];
  register int filled;
  register int pos;
  register char *p, *end;
  Lisp_Object sym, fun, tem;
  char *name;

  CHECK_STRING (filename, 0);

  name = (char *) alloca (XSTRING (filename)->size + 8);
  strcpy (name, "../etc/");
  strcat (name, XSTRING (filename)->data);

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    report_file_error ("Opening doc string file", Fcons (filename, Qnil));
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      if (filled < 512)
	filled += read (fd, &buf[filled], sizeof buf - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      p = buf;
      end = buf + (filled < 512 ? filled : filled - 128);
      while (p != end && *p != '\037') p++;
      if (p != end)
	{
	  end = index (p, '\n');
	  sym = oblookup (Vobarray, p + 1, end - p - 1);
	  if (XTYPE (sym) == Lisp_Symbol)
	    {
	      fun = XSYMBOL (sym)->function;
	      if (XTYPE (fun) == Lisp_Subr)
		XSUBR (fun)->doc = (char *) - (pos + end + 1 - buf);
	      else if (LISTP (fun))
		{
		  tem = XCONS (fun)->car;
		  if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
		    {
		      tem = Fcdr (Fcdr (fun));
		      if (LISTP (tem) && XTYPE (XCONS (tem)->car) == Lisp_Int)
			XFASTINT (XCONS (tem)->car) = (pos + end + 1 - buf);
		    }
		}
	    }
	}
      pos += end - buf;
      filled -= end - buf;
      bcopy (end, buf, filled);
    }
  close (fd);
  return Qnil;
}

DEFUN ("substitute-command-keys", Fsubstitute_command_keys,
  Ssubstitute_command_keys, 1, 1, 0,
  "Return the STRING with substrings of the form \\=\\[COMMAND]\n\
replaced by either:  a keystroke sequence that will invoke COMMAND,\n\
or \"M-x COMMAND\" if COMMAND is not on any keys.\n\
\\=\\= quotes the following character and is discarded;\n\
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.")
  (str)
     Lisp_Object str;
{
  unsigned char *buf;
  int didone = 0;
  register unsigned char *strp;
  register unsigned char *bufp;
  register unsigned char *send, *bend;
  Lisp_Object key;
  unsigned char *funp;
  int func;
  Lisp_Object usual;

  if (NULL (str))
    return Qnil;

  CHECK_STRING (str, 0);
  strp = (unsigned char *) XSTRING (str)->data;
  send = strp + XSTRING (str)->size;

  bufp = buf = (unsigned char *) alloca (4 * XSTRING (str)->size);
  bend = buf + 4 * XSTRING (str)->size;

  while (bufp < bend && strp < send)
    {
      if (strp[0] == '\\' && strp[1] == '=')
	{
	  /* \= quotes the next character;
	     thus, to put in \[ without its special meaning, use \=\[.  */
	  didone = 1;
	  *bufp++ = strp[2];
	  strp += 3;
	}
      else if (strp[0] == '\\' && strp[1] == '[')
	{
	  didone = 1;
	  strp += 2;		/* skip \[ */
	  funp = strp;

	  while (strp < send && *strp != ']')
	    strp++;
	  func = strp - funp;

	  key = Fintern (make_string (funp, func), Qnil);
	  key = Fcar (Fwhere_is_internal (key));
	  strp++;			/* skip ] */

	win:
	  if (NULL (key))	/* but not on any keys */
	    {
	      strncpy (bufp, "M-x ", bend - bufp);
	      if (bend - bufp > 4)
		bufp += 4;
	      else
		bufp = bend;
	    }
	  else
	    {		/* function is on a key */
	      key = Fkey_description (key);
	      funp = XSTRING (key)->data;
	      func = XSTRING (key)->size;
	    }

	  if (func > bend - bufp)
	    func = bend - bufp;
	  bcopy (funp, bufp, func);
	  bufp += func;
	}
      else			/* just copy other chars */
	*bufp++ = *strp++;
    }

  if (didone)			/* don't bother if nothing substituted */
    return make_string (buf, bufp - buf);
  else
    return str;
}

syms_of_doc ()
{
  staticpro (&Vdoc_file_name);
  Vdoc_file_name = Qnil;

  defsubr (&Sdocumentation);
  defsubr (&Ssnarf_documentation);
  defsubr (&Ssubstitute_command_keys);
}
