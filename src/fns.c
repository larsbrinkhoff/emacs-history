/* Random utility Lisp functions.
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

#ifndef eunice
#ifdef USG
#include <a.out.h> 
#else
#include <nlist.h>
#endif
#include <sys/param.h>
#endif

#undef NULL
#include "lisp.h"
#include "commands.h"

Lisp_Object Qstring_lessp;

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
  "Return the argument unchanged.")
  (arg)
     Lisp_Object arg;
{
  return arg;
}

DEFUN ("random", Frandom, Srandom, 0, 0, 0,
  "Return a pseudo-random number.")
  ()
{
  extern long random ();
  return make_number ((int) random ());
}

/* Random data-structure functions */

DEFUN ("length", Flength, Slength, 1, 1, 0,
  "Return the length of vector, list or string SEQUENCE.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object tail, val;
  register int i;

  if (XTYPE (obj) == Lisp_Vector || XTYPE (obj) == Lisp_String)
    return Farray_length (obj);
  else if (LISTP(obj))
    {
      for (i = 0, tail = obj; !NULL(tail); i++)
	{
	  QUIT;
	  tail = Fcdr (tail);
	}
      /* The people hacking the pyramid added a semicolon here.
	 Perhaps that was to compensate for a funny compiler bug
	 or perhaps it was a mistake.
	 Let's see if they really need it, by leaving it out.  */

      XFASTINT (val) = i;
      return val;
    }
  else if (NULL(obj))
    {
      XFASTINT (val) = 0;
      return val;
    }
  else
    wrong_type_argument (Qsequencep, obj, 0);
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
  "T if two strings have identical contents.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  if (XTYPE (s1) == Lisp_Symbol)
    XSETSTRING (s1, XSYMBOL (s1)->name), XSETTYPE (s1, Lisp_String);
  if (XTYPE (s2) == Lisp_Symbol)
    XSETSTRING (s2, XSYMBOL (s2)->name), XSETTYPE (s2, Lisp_String);
  if (XTYPE (s1) != Lisp_String)
    wrong_type_argument (Qstringp, s1, 0);
  if (XTYPE (s2) != Lisp_String)
    wrong_type_argument (Qstringp, s2, 1);

  if (XSTRING (s1)->size != XSTRING (s2)->size ||
      bcmp (XSTRING (s1)->data, XSTRING (s2)->data, XSTRING (s1)->size))
    return Qnil;
  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
  "T if first arg string is less than second in lexicographic order.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  register int i;
  register unsigned char *p1, *p2;
  register int end;

  if (XTYPE (s1) == Lisp_Symbol)
    XSETSTRING (s1, XSYMBOL (s1)->name), XSETTYPE (s1, Lisp_String);
  if (XTYPE (s2) == Lisp_Symbol)
    XSETSTRING (s2, XSYMBOL (s2)->name), XSETTYPE (s2, Lisp_String);
  if (XTYPE (s1) != Lisp_String)
    wrong_type_argument (Qstringp, s1, 0);
  if (XTYPE (s2) != Lisp_String)
    wrong_type_argument (Qstringp, s2, 1);

  p1 = XSTRING (s1)->data;
  p2 = XSTRING (s2)->data;
  end = XSTRING (s1)->size;
  if (end > XSTRING (s2)->size)
    end = XSTRING (s2)->size;

  for (i = 0; i < end; i++)
    {
      if (p1[i] != p2[i])
	return p1[i] < p2[i] ? Qt : Qnil;
    }
  return i < XSTRING (s2)->size ? Qt : Qnil;
}

static Lisp_Object concat ();

Lisp_Object
concat2 (s1, s2)
     Lisp_Object s1, s2;
{
  return concat (2, &s1, Lisp_String);
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
  "Concatenate arguments and make the result a list.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Cons);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
  "Concatenate arguments and make the result a string.\n\
The result is a string whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string; but all elements\n\
of a list or vector must be numbers, or an error is signaled.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_String);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
  "Concatenate arguments and make the result a vector.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Vector);
}

DEFUN ("copy-sequence", Fcopy_sequence, Scopy_sequence, 1, 1, 0,
  "Return a copy of a list, vector or string.")
  (arg)
     Lisp_Object arg;
{
  if (NULL (arg)) return arg;
  if (!LISTP (arg) && XTYPE (arg) != Lisp_Vector && XTYPE (arg) != Lisp_String)
    wrong_type_argument (Qsequencep, arg, 0);
  return concat (1, &arg, LISTP (arg) ? Lisp_Cons : XTYPE (arg));
}

static Lisp_Object
concat (nargs, args, target_type)
     int nargs;
     Lisp_Object *args;
     enum Lisp_Type target_type;
{
  Lisp_Object val;
  Lisp_Object len;
  Lisp_Object tail;
  Lisp_Object this;
  int toindex;
  int leni;
  int argnum;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      if (!(LISTP (this) || NULL (this)
          || XTYPE (this) == Lisp_Vector || XTYPE (this) == Lisp_String))
	{
	  if (XTYPE (this) == Lisp_Int)
            args[argnum] = Fint_to_string (this);
	  else
	    wrong_type_argument (Qsequencep, this, argnum);
	}
    }

  for (argnum = 0, leni = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      len = Flength (this);
      leni += XINT (len);
    }

  XSETINT (len, leni);

  if (target_type == Lisp_Cons)
    val = Fmake_list (len, Qnil);
  else if (target_type == Lisp_Vector)
    val = Fmake_vector (len, Qnil);
  else
    val = Fmake_string (len, len);

  if (LISTP (val))
    tail = val, toindex = -1;		/* -1 in toindex is flag we are making a list */
  else
    toindex = 0;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      register Lisp_Object this;
      Lisp_Object thislen;
      int thisleni;
      register int thisindex = 0;

      this = args[argnum];
      if (!LISTP (this))
	thislen = Flength (this), thisleni = XINT (thislen);

      while (1)
	{
	  register Lisp_Object elt;

	  /* Fetch next element of `this' arg into `elt', or break if `this' is exhausted. */
	  if (NULL (this)) break;
	  if (LISTP (this))
	    elt = Fcar (this), this = Fcdr (this);
	  else
	    {
	      if (thisindex >= thisleni) break;
	      if (XTYPE (this) == Lisp_String)
		XFASTINT (elt) = XSTRING (this)->data[thisindex++];
	      else
		elt = XVECTOR (this)->contents[thisindex++];
	    }

	  /* Store into result */
	  if (toindex < 0)
	    {
	      XCONS (tail)->car = elt;
	      tail = XCONS (tail)->cdr;
	    }
	  else if (XTYPE (val) == Lisp_Vector)
	    XVECTOR (val)->contents[toindex++] = elt;
	  else
	    {
	      if (XTYPE (elt) != Lisp_Int)
		wrong_type_argument (Qintegerp, elt, -1);
	      else
	        XSTRING (val)->data[toindex++] = XINT (elt);
	    }
	}
    }
  return val;  
}

DEFUN ("substring", Fsubstring, Ssubstring, 2, 3, 0,
  "Return a substring of STRING, starting at index FROM and reaching until TO.\n\
TO may be nil or omitted; then the substring runs to the end of STRING.\n\
If FROM or TO is negative, it counts from the end.")
  (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  register Lisp_Object val, len;

  CHECK_STRING (string, 0);
  CHECK_NUMBER (from, 1);
  if (NULL (to))
    to = Flength (string);
  else
    CHECK_NUMBER (to, 2);

  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + XSTRING (string)->size);
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + XSTRING (string)->size);
  if (!(0 <= XINT (from) && XINT (from) <= XINT (to)
        && XINT (to) <= XSTRING (string)->size))
    Fsignal (Qargs_out_of_range, Fcons (string, Fcons (from, Fcons (to, Qnil))));

  XFASTINT (len) = XINT (to) - XINT (from);
  val = Fmake_string (len, len);

  bcopy (XSTRING (string)->data + XINT (from), XSTRING (val)->data, XINT (len));

  return val;
}

DEFUN ("nthcdr", Fnthcdr, Snthcdr, 2, 2, 0,
  "Takes cdr N times on LIST, returns the result.")
  (n, list)
     Lisp_Object n;
     register Lisp_Object list;
{
  register int i, num;
  CHECK_NUMBER (n, 0);
  num = XINT (n);
  for (i = 0; i < num; i++)
    {
      QUIT;
      list = Fcdr (list);
    }
  return list;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
  "Returns the Nth element of LIST.")
  (n, list)
     Lisp_Object n, list;
{
  CHECK_NUMBER (n, 0);
  if (!(XTYPE (list) == Lisp_Cons || NULL (list)))
    wrong_type_argument (Qlistp, list, 1);
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
  "Returns element of SEQUENCE at index N.")
  (seq, n)
     register Lisp_Object seq, n;
{
  CHECK_NUMBER (n, 0);
  if (XTYPE (seq) == Lisp_Cons || NULL (seq))
    return Fcar (Fnthcdr (n, seq));
  else if (XTYPE (seq) == Lisp_String ||
	   XTYPE (seq) == Lisp_Vector)
    return Faref (seq, n);
  else
    wrong_type_argument (Qsequencep, seq, 1);
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
  "Returns non-nil if ELT is an element of LIST.  Comparison done with EQ.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (elt, tem)) return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
  "Returns non-nil if ELT is the car of an element of LIST.  Comparison done with eq.\n\
The value is actually the element of LIST whose car is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!LISTP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 2, 0,
  "Returns non-nil if ELT is the car of an element of LIST.  Comparison done with  equal.\n\
The value is actually the element of LIST whose car is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!LISTP (elt)) continue;
      tem = Fequal (Fcar (elt), key);
      if (!NULL (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
  "Returns non-nil if ELT is the cdr of an element of LIST.  Comparison done with EQ.\n\
The value is actually the element of LIST whose cdr is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!LISTP (elt)) continue;
      tem = Fcdr (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
  "Deletes by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.\n\
If the first member of LIST is ELT, there is no way to remove it by side effect;\n\
therefore, write  (setq foo (delq element foo))  to be sure of changing  foo.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NULL (tail))
    {
      tem = Fcar (tail);
      if (EQ (elt, tem))
	{
	  if (NULL (prev))
	    list = Fcdr (tail);
	  else
	    Fsetcdr (prev, Fcdr (tail));
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }
  return list;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
  "Reverses LIST by modifying cdr pointers.  Returns the beginning of the reversed list.")
  (list)
     Lisp_Object list;
{
  register Lisp_Object prev, tail, next;

  if (NULL (list)) return list;
  prev = Qnil;
  tail = list;
  while (!NULL (tail))
    {
      QUIT;
      next = Fcdr (tail);
      Fsetcdr (tail, prev);
      prev = tail;
      tail = next;
    }
  return prev;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
  "Reverses LIST, copying.  Returns the beginning of the reversed list.")
  (list)
     Lisp_Object list;
{
  Lisp_Object length;
  register Lisp_Object *vector;
  register Lisp_Object tail;
  register int i;

  length = Flength (list);
  vector = (Lisp_Object *) alloca (XINT (length) * sizeof (Lisp_Object));
  for (i = XINT (length) - 1, tail = list; i >= 0; i--, tail = Fcdr (tail))
    vector[i] = Fcar (tail);

  return Flist (XINT (length), vector);
}

Lisp_Object merge ();

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
  "Sort LIST, comparing elements using PREDICATE.\n\
Returns the sorted list.  LIST is modified by side effects.\n\
PREDICATE is called with two elements of LIST, and should return T\n\
if the first element is \"less\" than the second.")
  (list, pred)
     Lisp_Object list, pred;
{
  Lisp_Object front, back;
  register Lisp_Object len, tem;
  struct gcpro gcpro1, gcpro2;
  register int length;

  front = list;
  len = Flength (list);
  length = XINT (len);
  if (length < 2)
    return list;

  XSETINT (len, (length / 2) - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO2 (front, back);
  front = Fsort (front, pred);
  back = Fsort (back, pred);
  UNGCPRO;
  return merge (front, back, pred);
}

Lisp_Object
merge (org_l1, org_l2, pred)
     Lisp_Object org_l1, org_l2;
     Lisp_Object pred;
{
  Lisp_Object value;
  register Lisp_Object tail;
  Lisp_Object tem;
  register Lisp_Object l1, l2;
  struct gcpro gcpro1, gcpro2;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into those.  */
  GCPRO2 (org_l1, value);
  gcpro1.nvars = 3;

  while (1)
    {
      if (NULL (l1))
	{
	  UNGCPRO;
	  if (NULL (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NULL (l2))
	{
	  UNGCPRO;
	  if (NULL (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}
      tem = call2 (pred, Fcar (l1), Fcar (l2));
      if (!NULL (tem))
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;
	}
      if (NULL (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);
      tail = tem;
    }
}

DEFUN ("get", Fget, Sget, 2, 2, 0,
  "Return the value of SYMBOL's PROPNAME property.\n\
This is the last VALUE stored with  (put SYMBOL PROPNAME VALUE).")
  (sym, prop)
     Lisp_Object sym;
     register Lisp_Object prop;
{
  register Lisp_Object tail;
  for (tail = Fsymbol_plist (sym); !NULL (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fcar (Fcdr (tail));
    }
  return Qnil;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
  "Store SYMBOL's PROPNAME property with value VALUE.\n\
It can be retrieved with  (get SYMBOL PROPNAME).")
  (sym, prop, val)
     Lisp_Object sym;
     register Lisp_Object prop;
     Lisp_Object val;
{
  register Lisp_Object tail, prev;
  Lisp_Object newcell;
  prev = Qnil;
  for (tail = Fsymbol_plist (sym); !NULL (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fsetcar (Fcdr (tail), val);
      prev = tail;
    }
  newcell = Fcons (prop, Fcons (val, Qnil));
  if (NULL (prev))
    Fsetplist (sym, newcell);
  else
    Fsetcdr (Fcdr (prev), newcell);
  return val;
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
  "T if two Lisp objects have similar structure and contents.\n\
They must have the same data type.\n\
Conses are compared by comparing the cars and the cdrs.\n\
Vectors and strings are compared element by element.\n\
Numbers are compared by value.  Symbols must match exactly.")
  (o1, o2)
     register Lisp_Object o1, o2;
{
do_cdr:
  QUIT;
  if (XTYPE (o1) != XTYPE (o2)) return Qnil;
  if (XINT (o1) == XINT (o2)) return Qt;
  if (XTYPE (o1) == Lisp_Cons)
    {
      Lisp_Object v1;
      v1 = Fequal (Fcar (o1), Fcar (o2));
      if (NULL (v1))
	return v1;
      o1 = Fcdr (o1), o2 = Fcdr (o2);
      goto do_cdr;
    }
  if (XTYPE (o1) == Lisp_Vector)
    {
      register int index;
      if (XVECTOR (o1)->size != XVECTOR (o2)->size)
	return Qnil;
      for (index = 0; index < XVECTOR (o1)->size; index++)
	{
	  Lisp_Object v, v1, v2;
	  v1 = XVECTOR (o1)->contents [index];
	  v2 = XVECTOR (o2)->contents [index];
	  v = Fequal (v1, v2);
	  if (NULL (v)) return v;
	}
      return Qt;
    }
  if (XTYPE (o1) == Lisp_String)
    {
      register int index;
      if (XSTRING (o1)->size != XSTRING (o2)->size)
	return Qnil;
      if (bcmp (XSTRING (o1)->data, XSTRING (o2)->data, XSTRING (o1)->size))
	return Qnil;
      return Qt;
    }
  return Qnil;
}

DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
  "Store each element of ARRAY with ITEM.  ARRAY is a vector or string.")
  (array, item)
     Lisp_Object array, item;
{
  register int size, index, charval;
  if (XTYPE (array) == Lisp_Vector)
    {
      register Lisp_Object *p = XVECTOR (array)->contents;
      size = XVECTOR (array)->size;
      for (index = 0; index < size; index++)
	p[index] = item;
    }
  else if (XTYPE (array) == Lisp_String)
    {
      register unsigned char *p = XSTRING (array)->data;
      CHECK_NUMBER (item, 1);
      charval = XINT (item);
      size = XSTRING (array)->size;
      for (index = 0; index < size; index++)
	p[index] = charval;
    }
  else
    wrong_type_argument (Farrayp, array, 0);
  return array;
}

Lisp_Object
nconc2 (s1, s2)
     Lisp_Object s1, s2;
{
  return Fnconc (2, &s1);
}

DEFUN ("nconc", Fnconc, Snconc, 0, MANY, 0,
  "Concatenate any number of lists by altering them.\n\
Only the last argument is not altered, and need not be a list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tail, tem, val;

  val = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
      if (NULL (tem)) continue;

      if (!LISTP (tem))
	wrong_type_argument (Flistp, tem, argnum);

      if (NULL (val))
	val = tem;

      if (argnum + 1 == nargs) break;

      while (LISTP (tem))
	{
	  tail = tem;
	  tem = Fcdr (tail);
	  QUIT;
	}

      tem = args[argnum + 1];
      Fsetcdr (tail, tem);
      if (NULL (tem))
	args[argnum + 1] = tail;
    }

  return val;
}

/* This is the guts of all mapping functions.
 Apply fn to each element of seq, one by one,
 storing the results into elements of vals, a C vector of Lisp_Objects.
 leni is the length of vals, which should also be the length of seq. */

static void
mapcar1 (leni, vals, fn, seq)
     int leni;
     Lisp_Object *vals;
     Lisp_Object fn, seq;
{
  register Lisp_Object tail;
  Lisp_Object dummy;
  register int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (dummy, fn, seq);
  gcpro1.var = vals;
  gcpro1.nvars = leni;
  /* We need not explicitly protect `tail' because it is used only on lists, and
    1) lists are not relocated and 2) the list is marked via `seq' so will not be freed */

  if (XTYPE (seq) == Lisp_Vector)
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = XVECTOR (seq)->contents[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (XTYPE (seq) == Lisp_String)
    {
      for (i = 0; i < leni; i++)
	{
	  XFASTINT (dummy) = XSTRING (seq)->data[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else   /* Must be a list, since Flength did not get an error */
    {
      tail = seq;
      for (i = 0; i < leni; i++)
	{
	  vals[i] = call1 (fn, Fcar (tail));
	  tail = Fcdr (tail);
	}
    }

  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
  "Apply FN to each element of SEQ, and concat the results as strings.\n\
In between each pair of results, stick in SEP.\n\
Thus, \" \" as SEP results in spaces between the values return by FN.")
  (fn, seq, sep)
     Lisp_Object fn, seq, sep;
{
  Lisp_Object len;
  register int leni;
  int nargs;
  register Lisp_Object *args;
  register int i;

  len = Flength (seq);
  leni = XINT (len);
  nargs = leni + leni - 1;
  if (nargs < 0) return build_string ("");

  args = (Lisp_Object *) alloca (nargs * sizeof (Lisp_Object));

  mapcar1 (leni, args, fn, seq);

  for (i = leni - 1; i >= 0; i--)
    args[i + i] = args[i];
      
  for (i = 1; i < nargs; i += 2)
    args[i] = sep;

  return Fconcat (nargs, args);
}

DEFUN ("mapcar", Fmapcar, Smapcar, 2, 2, 0,
  "Apply FUNCTION to each element of LIST, and make a list of the results.\n\
THe result is a list just as long as LIST.")
  (fn, seq)
     Lisp_Object fn, seq;
{
  register Lisp_Object len;
  register int leni;
  register Lisp_Object *args;

  len = Flength (seq);
  leni = XFASTINT (len);
  args = (Lisp_Object *) alloca (leni * sizeof (Lisp_Object));

  mapcar1 (leni, args, fn, seq);

  return Flist (leni, args);
}

DEFUN ("y-or-n-p", Fy_or_n_p, Sy_or_n_p, 1, 1, 0,
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".\n\
No confirmation of the answer is requested; a single character is enough.\n\
Also accepts Space to mean yes, or Delete to mean no.")
  (prompt)
     Lisp_Object prompt;
{
  register int ans;
  register Lisp_Object xprompt;
  Lisp_Object args[2];

  CHECK_STRING (prompt, 0);
  xprompt = prompt;
  while (1)
    {
      message ("%s(y or n) ", XSTRING (xprompt)->data);
      ans = get_char (0);
      message ("%s(y or n) %c", XSTRING (xprompt)->data, ans);
      QUIT;
      if (ans >= 'A' && ans <= 'Z') ans += 'a' - 'A';
      if (ans == 'y' || ans == ' ')
	return Qt;
      if (ans == 'n' || ans == 127)
	return Qnil;
      if (EQ (xprompt, prompt))
	{
	  args[0] = build_string ("Please answer y or n.  ");
	  args[1] = prompt;
	  xprompt = Fconcat (2, args);
	}
    }
}

DEFUN ("yes-or-no-p", Fyes_or_no_p, Syes_or_no_p, 1, 1, 0,
  "Ask user a yes or no question.  Return t if answer is yes.\n\
The user must confirm the answer with a newline, and can rub it out if not confirmed.")
  (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object ans;
  Lisp_Object args[2];
  CHECK_STRING (prompt, 0);

  args[0] = prompt;
  args[1] = build_string ("(yes or no) ");
  prompt = Fconcat (2, args);
  while (1)
    {
      ans = Fdowncase (read_minibuf_string (Vminibuffer_local_map,
					    Qnil,
					    prompt));
      if (XSTRING (ans)->size == 3 && !strcmp (XSTRING (ans)->data, "yes"))
	return Qt;
      if (XSTRING (ans)->size == 2 && !strcmp (XSTRING (ans)->data, "no"))
	return Qnil;

      message ("Please answer yes or no.");
      Fsleep_for (make_number (2));
    }
}

DEFUN ("load-average", Fload_average, Sload_average, 0, 0, 0,
  "Return the current 1 minute, 5 minute and 15 minute load averages\n\
in a list (all floating point load average values are multiplied by 100\n\
and then turned into integers).")
  ()
{
  /*************************************************************/
  /* WARNING:  We can't have initialized static data that will */
  /* -------   be modified later as xemacs makes such things   */
  /*	     read/only!!				     */
  /*************************************************************/
  static short int initialized;
  static int channel;


#ifdef	eunice
#include <vms/iodef.h>
  /*
   *	VMS/Eunice specific code -- read from the Load Ave driver
   */
  float load_ave[3];
  struct {int size; char *ptr;} descriptor;

  /* If this fails for any reason, we can return (0 0 0) */
  load_ave[0] = 0.0; load_ave[1] = 0.0; load_ave[2] = 0.0;

  /*
   *	Ensure that there is a channel open to the load ave device
   */
  if (initialized == 0)
    {
      /* Attempt to open the channel */
      descriptor.size = 18;
      descriptor.ptr  = "$$VMS_LOAD_AVERAGE";
      if (sys$assign (&descriptor, &channel, 0, 0) & 1)
	initialized = 1;
    }
  /*
   *	Read the load average vector
   */
  if (initialized)
    {
      if (!(sys$qiow (0, channel, IO$_READVBLK, 0, 0, 0,
		     load_ave, 12, 0, 0, 0, 0)
	    & 1))
	{
	  sys$dassgn (channel);
	  initialized = 0;
	}
    }
#else  /* not eunice */

  /*
   *	4.2BSD UNIX-specific code -- read _avenrun from /dev/kmem
   */

  LOAD_AVE_TYPE load_ave[3];

#ifdef USG
  error ("load-average not implemented for AT&T unix versions");
#else /* not USG */
  static struct nlist nl[2];

  /* If this fails for any reason, we can return (0 0 0) */
  load_ave[0] = 0.0; load_ave[1] = 0.0; load_ave[2] = 0.0;

  /*
   *	Make sure we have the address of _avenrun
   */
  if (nl[0].n_value == 0)
    {
      /*
       *	Get the address of _avenrun
       */
      nl[0].n_name = "_avenrun";
      nl[1].n_name = "";
      nlist ("/vmunix", nl);
    }
  /*
   *	Make sure we have /dev/kmem open
   */
  if (initialized == 0)
    {
      /*
       *	Open /dev/kmem
       */
      channel = open ("/dev/kmem", 0);
      if (channel >= 0) initialized = 1;
    }
  /*
   *	If we can, get the load ave values
   */
  if ((nl[0].n_value != 0) && (initialized != 0))
    {
      /*
       *	Seek to the correct address
       */
      lseek (channel, (long) nl[0].n_value, 0);
      if (read (channel, load_ave, sizeof load_ave)
	  != sizeof(load_ave))
	{
	  close(channel);
	  initialized = 0;
	}
    }
#endif /* not USG */
#endif /* not eunice */

  /*
   *	Return the list of load average values
   */
  return Fcons (make_number (LOAD_AVE_CVT (load_ave[0])),
		Fcons (make_number (LOAD_AVE_CVT (load_ave[1])),
		       Fcons (make_number (LOAD_AVE_CVT (load_ave[2])),
			      Qnil)));
}

Lisp_Object Vfeatures;

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 1, 0,
  "Returns t if FEATURE is present in this Emacs.\n\
Use this to conditionalize execution of lisp code based on the presence or\n\
absence of emacs or environment extensions.\n\
Use  provide  to declare that a feature is available.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  return (NULL (tem)) ? Qnil : Qt;
}

DEFUN ("provide", Fprovide, Sprovide, 1, 1, 0,
  "Announce that FEATURE is a feature of the current Emacs.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  if (NULL (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  return feature;
}

DEFUN ("require", Frequire, Srequire, 1, 2, 0,
       "Same as (if (not featurep FEATURE) (load (or FILE-NAME FEATURE)))")
     (feature, file_name)
     Lisp_Object feature, file_name;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  if (NULL (tem))
    { Fload (NULL (file_name) ? Fsymbol_name (feature) : file_name);
      tem = Fmemq (feature, Vfeatures);
      if (NULL (tem))
	error ("Required feature %s was not provided",
	       XSYMBOL (feature)->name->data );
    }
  return feature;
}

syms_of_fns ()
{
  Qstring_lessp = intern ("string-lessp");
  staticpro (&Qstring_lessp);

  DefLispVar ("features", &Vfeatures,
    "A list of symbols which are the features of the executing emacs.\n\
Used by  featurep  and  require, and altered by  provide.");
  Vfeatures = Qnil;

  defsubr (&Sidentity);
  defsubr (&Srandom);
  defsubr (&Slength);
  defsubr (&Sstring_equal);
  defsubr (&Sstring_lessp);
  defalias (&Sstring_equal, "string=");
  defalias (&Sstring_lessp, "string<");
  defsubr (&Sappend);
  defsubr (&Sconcat);
  defsubr (&Svconcat);
  defsubr (&Scopy_sequence);
  defsubr (&Ssubstring);
  defsubr (&Snthcdr);
  defsubr (&Snth);
  defsubr (&Selt);
  defsubr (&Smemq);
  defsubr (&Sassq);
  defsubr (&Sassoc);
  defsubr (&Srassq);
  defsubr (&Sdelq);
  defsubr (&Snreverse);
  defsubr (&Sreverse);
  defsubr (&Ssort);
  defsubr (&Sget);
  defsubr (&Sput);
  defsubr (&Sequal);
  defsubr (&Sfillarray);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapconcat);
  defsubr (&Sy_or_n_p);
  defsubr (&Syes_or_no_p);
  defsubr (&Sload_average);
  defsubr (&Sfeaturep);
  defsubr (&Srequire);
  defsubr (&Sprovide);
}
