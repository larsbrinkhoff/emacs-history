/* Execution of byte code produced by bytecomp.el.
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

Lisp_Object Qbytecode;

/*  Byte codes: */

#define Bvarref 010
#define Bvarset 020
#define Bvarbind 030
#define Bcall 040
#define Bunbind 050

#define Bnth 070
#define Bsymbolp 071
#define Bconsp 072
#define Bstringp 073
#define Blistp 074
#define Beq 075
#define Bmemq 076
#define Bnot 077
#define Bcar 0100
#define Bcdr 0101
#define Bcons 0102
#define Blist1 0103
#define Blist2 0104
#define Blist3 0105
#define Blist4 0106
#define Blength 0107
#define Baref 0110
#define Baset 0111
#define Bsymbol_value 0112
#define Bsymbol_function 0113
#define Bset 0114
#define Bfset 0115
#define Bget 0116
#define Bsubstring 0117
#define Bconcat2 0120
#define Bconcat3 0121
#define Bconcat4 0122
#define Bsub1 0123
#define Badd1 0124
#define Beqlsign 0125
#define Bgtr 0126
#define Blss 0127
#define Bleq 0130
#define Bgeq 0131
#define Bdiff 0132
#define Bnegate 0133
#define Bplus 0134
#define Bmax 0135
#define Bmin 0136

#define Bdot 0140
#define Bmark 0141
#define Bgoto_char 0142
#define Binsert 0143
#define Bdot_max 0144
#define Bdot_min 0145
#define Bchar_after 0146
#define Bfollowing_char 0147
#define Bpreceding_char 0150
#define Bcurrent_column 0151
#define Bindent_to 0152
#define Bscan_buffer 0153
#define Beolp 0154
#define Beobp 0155
#define Bbolp 0156
#define Bbobp 0157
#define Bcurrent_buffer 0160
#define Bset_buffer 0161
#define Bread_char 0162
#define Bset_mark 0163
#define Binteractive_p 0164   /* Needed since interactive-p takes unevalled args */

#define Bconstant2 0201
#define Bgoto 0202
#define Bgotoifnil 0203
#define Bgotoifnonnil 0204
#define Bgotoifnilelsepop 0205
#define Bgotoifnonnilelsepop 0206
#define Breturn 0207
#define Bdiscard 0210
#define Bdup 0211

#define Bsave_excursion 0212
#define Bsave_window_excursion 0213
#define Bsave_restriction 0214
#define Bcatch 0215

#define Bunwind_protect 0216
#define Bcondition_case 0217
#define Btemp_output_buffer_setup 0220
#define Btemp_output_buffer_show 0221

#define Bconstant 0300
#define CONSTANTLIM 0100

/* Fetch the next byte from the bytecode stream */

#define FETCH ((unsigned char *)XSTRING (bytestr)->data)[pc++]

/* Fetch two bytes from the bytecode stream
 and make a 16-bit number out of them */

#define FETCH2 (tem1 = FETCH, tem1 + (FETCH << 8))

/* Push x onto the execution stack. */

#define PUSH(x) (*stackp++ = (x))

/* Pop a value off the execution stack.  */

#define POP (*--stackp)

/* Get the value which is at the top of the execution stack, but don't pop it. */

#define TOP (*(stackp - 1))


DEFUN ("byte-code", Fbyte_code, Sbyte_code, 3, 3, 0,
  "")
  (bytestr, vector, maxdepth)
     Lisp_Object bytestr, vector, maxdepth;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  int count = specpdl_ptr - specpdl;
  register int pc = 0;
  register int op;
  int tem, tem1;
  Lisp_Object *stack;
  register Lisp_Object *stackp;
  Lisp_Object *stacke;
  register Lisp_Object val, v1, v2, v3;
  Lisp_Object *vectorp = XVECTOR (vector)->contents;

  CHECK_STRING (bytestr, 0);
  if (XTYPE (vector) != Lisp_Vector)
    vector = wrong_type_argument (Qvectorp, vector, 1);
  CHECK_NUMBER (maxdepth, 2);

  stack = (Lisp_Object *) alloca (XINT (maxdepth) * sizeof (Lisp_Object));
  stackp = stack;
  stacke = stack + XINT (maxdepth);
  bzero (stack, XINT (maxdepth) * sizeof (Lisp_Object));

  GCPRO3 (bytestr, vector, *stack);
  gcpro3.nvars = XINT (maxdepth);

  while (1)
    {
      if (stackp > stacke)
	error ("Stack overflow in byte code interpretation");
      if (stackp < stack)
	error ("Stack underflow in byte code interpretation");
      switch (op = FETCH)
	{
	case Bvarref: case Bvarref+1: case Bvarref+2: case Bvarref+3:
	case Bvarref+4: case Bvarref+5:
	  PUSH (Fsymbol_value (vectorp[op - Bvarref]));
	  break;

	case Bvarref+6:
	  PUSH (Fsymbol_value (vectorp[FETCH]));
	  break;

	case Bvarref+7:
	  PUSH (Fsymbol_value (vectorp[FETCH2]));
	  break;

	case Bvarset: case Bvarset+1: case Bvarset+2: case Bvarset+3:
	case Bvarset+4: case Bvarset+5:
	  Fset (vectorp[op - Bvarset], POP);
	  break;

	case Bvarset+6:
	  Fset (vectorp[FETCH], POP);
	  break;

	case Bvarset+7:
	  Fset (vectorp[FETCH2], POP);
	  break;

	case Bvarbind: case Bvarbind+1: case Bvarbind+2: case Bvarbind+3:
	case Bvarbind+4: case Bvarbind+5:
	  specbind (vectorp[op - Bvarbind], POP);
	  break;

	case Bvarbind+6:
	  specbind (vectorp[FETCH], POP);
	  break;

	case Bvarbind+7:
	  specbind (vectorp[FETCH2], POP);
	  break;

	case Bcall: case Bcall+1: case Bcall+2: case Bcall+3:
	case Bcall+4: case Bcall+5:
	  tem = 1 + op - Bcall;
	docall:
	  stackp -= tem;
	  gcpro3.nvars = stackp - stack;
	  PUSH (Ffuncall (tem, stackp));
	  gcpro3.nvars = XFASTINT (maxdepth);
	  break;

	case Bcall+6:
	  tem = FETCH + 1;
	  goto docall;

	case Bcall+7:
	  tem = FETCH2 + 1;
	  goto docall;

	case Bunbind: case Bunbind+1: case Bunbind+2: case Bunbind+3:
	case Bunbind+4: case Bunbind+5:
	  unbind_to (specpdl_ptr - specpdl - (op - Bunbind));
	  break;

	case Bunbind+6:
	  unbind_to (specpdl_ptr - specpdl - FETCH);
	  break;

	case Bunbind+7:
	  unbind_to (specpdl_ptr - specpdl - FETCH2);
	  break;

	case Bgoto:
	  QUIT;
	  tem = FETCH2;    /* pc = FETCH2 loses since FETCH2 contains pc++ */
	  pc = tem;
	  break;

	case Bgotoifnil:
	  QUIT;
	  val = POP;
	  tem = FETCH2;
	  if (NULL (val))
	    pc = tem;
	  break;

	case Bgotoifnonnil:
	  QUIT;
	  val = POP;
	  tem = FETCH2;
	  if (!NULL (val))
	    pc = tem;
	  break;

	case Bgotoifnilelsepop:
	  QUIT;
	  val = TOP;
	  tem = FETCH2;
	  if (NULL (val))
	    pc = tem;
	  else POP;
	  break;

	case Bgotoifnonnilelsepop:
	  QUIT;
	  val = TOP;
	  tem = FETCH2;
	  if (!NULL (val))
	    pc = tem;
	  else POP;
	  break;

	case Breturn:
	  val = POP;
	  goto exit;

	case Bdiscard:
	  POP;
	  break;

	case Bdup:
	  val = TOP;
	  PUSH (val);
	  break;

	case Bconstant2:
	  PUSH (vectorp[FETCH2]);
	  break;

	case Bsave_excursion:
	  record_unwind_protect (save_excursion_restore, save_excursion_save ());
	  break;

	case Bsave_window_excursion:
	  val = Fsave_window_excursion (POP);
	  PUSH (val);
	  break;

	case Bsave_restriction:
	  record_unwind_protect (save_restriction_restore, save_restriction_save ());
	  break;

	case Bcatch:
	  val = POP;
	  val = internal_catch (POP, Feval, val);
	  PUSH (val);
	  break;

	case Bunwind_protect:
	  record_unwind_protect (0, POP);
	  (specpdl_ptr - 1)->symbol = Qnil;
	  break;

	case Bcondition_case:
	  val = POP;
	  val = Fcons (POP, val);
	  val = Fcons (POP, val);
	  val = Fcondition_case (val);
	  PUSH (val);
	  break;

	case Btemp_output_buffer_setup:
	  val = POP;
	  temp_output_buffer_setup (XSTRING (val)->data);
	  PUSH (Vstandard_output);
	  break;

	case Btemp_output_buffer_show:
	  val = POP;
	  temp_output_buffer_show (POP);
	  PUSH (val);
	  break;

	case Bnth:
	  v1 = POP;
	  val = POP;
	  CHECK_NUMBER (val, 0);
	  while (XINT (val) > 0)
	    {
	      if (LISTP (v1))
		v1 = XCONS (v1)->cdr;
	      else if (!NULL (v1))
		wrong_type_argument (Qlistp, v1, 0);
	      XSETINT (val, XINT (val) - 1);
	    }
	  goto docar;

	case Bsymbolp:
	  v1 = POP;
	  PUSH (XTYPE (v1) == Lisp_Symbol ? Qt : Qnil);
	  break;

	case Bconsp:
	  v1 = POP;
	  PUSH (LISTP (v1) ? Qt : Qnil);
	  break;

	case Bstringp:
	  v1 = POP;
	  PUSH (XTYPE (v1) == Lisp_String ? Qt : Qnil);
	  break;

	case Blistp:
	  v1 = POP;
	  PUSH (LISTP (v1) || NULL (v1) ? Qt : Qnil);
	  break;

	case Beq:
	  v1 = POP; v2 = POP;
	  PUSH (EQ (v1, v2) ? Qt : Qnil);
	  break;

	case Bmemq:
	  v2 = POP; v1 = POP;
	  val = Fmemq (v1, v2);
	  PUSH (val);
	  break;

	case Bnot:
	  v1 = POP;
	  PUSH (NULL (v1) ? Qt : Qnil);
	  break;

	case Bcar:
	  v1 = POP;
	docar:
	  if (LISTP (v1)) PUSH (XCONS (v1)->car);
	  else if (NULL (v1)) PUSH (Qnil);
	  else wrong_type_argument (Qlistp, v1, 0);
	  break;

	case Bcdr:
	  v1 = POP;
	  if (LISTP (v1)) PUSH (XCONS (v1)->cdr);
	  else if (NULL (v1)) PUSH (Qnil);
	  else wrong_type_argument (Qlistp, v1, 0);
	  break;

	case Bcons:
	  v2 = POP; v1 = POP;
	  PUSH (Fcons (v1, v2));
	  break;

	case Blist1:
	  PUSH (Fcons (POP, Qnil));
	  break;

	case Blist2:
	  v2 = POP; v1 = POP;
	  PUSH (Fcons (v1, Fcons (v2, Qnil)));
	  break;

	case Blist3:
	  PUSH (Flist (3, stackp -= 3));
	  break;

	case Blist4:
	  PUSH (Flist (4, stackp -= 4));
	  break;

	case Blength:
	  PUSH (Flength (POP));
	  break;

	case Baref:
	  v2 = POP; v1 = POP;
	  PUSH (Faref (v1, v2));
	  break;

	case Baset:
	  v3 = POP; v2 = POP; v1 = POP;
	  PUSH (Faset (v1, v2, v3));
	  break;

	case Bsymbol_value:
	  PUSH (Fsymbol_value (POP));
	  break;

	case Bsymbol_function:
	  PUSH (Fsymbol_function (POP));
	  break;

	case Bset:
	  v2 = POP; v1 = POP;
	  PUSH (Fset (v1, v2));
	  break;

	case Bfset:
	  v2 = POP; v1 = POP;
	  PUSH (Ffset (v1, v2));
	  break;

	case Bget:
	  v2 = POP; v1 = POP;
	  PUSH (Fget (v1, v2));
	  break;

	case Bsubstring:
	  v3 = POP; v2 = POP; v1 = POP;
	  PUSH (Fsubstring (v1, v2, v3));
	  break;

	case Bconcat2:
	  PUSH (Fconcat (2, stackp -= 2));
	  break;

	case Bconcat3:
	  PUSH (Fconcat (3, stackp -= 3));
	  break;

	case Bconcat4:
	  PUSH (Fconcat (4, stackp -= 4));
	  break;

	case Bsub1:
	  v1 = POP;
	  if (XTYPE (v1) == Lisp_Int)
	    {
	      XSETINT (v1, XINT (v1) - 1);
	      PUSH (v1);
	    }
	  else
	    PUSH (Fsub1 (v1));
	  break;

	case Badd1:
	  v1 = POP;
	  if (XTYPE (v1) == Lisp_Int)
	    {
	      XSETINT (v1, XINT (v1) + 1);
	      PUSH (v1);
	    }
	  else
	    PUSH (Fadd1 (v1));
	  break;

	case Beqlsign:
	  v2 = POP; v1 = POP;
	  CHECK_NUMBER_COERCE_MARKER (v1, 0);
	  CHECK_NUMBER_COERCE_MARKER (v2, 0);
	  PUSH (XINT (v1) == XINT (v2) ? Qt : Qnil);
	  break;

	case Bgtr:
	  v2 = POP; v1 = POP;
	  PUSH (Fgtr (v1, v2));
	  break;

	case Blss:
	  v2 = POP; v1 = POP;
	  PUSH (Flss (v1, v2));
	  break;

	case Bleq:
	  v2 = POP; v1 = POP;
	  PUSH (Fleq (v1, v2));
	  break;

	case Bgeq:
	  v2 = POP; v1 = POP;
	  PUSH (Fgeq (v1, v2));
	  break;

	case Bdiff:
	  PUSH (Fminus (2, stackp -= 2));
	  break;

	case Bnegate:
	  v1 = POP;
	  if (XTYPE (v1) == Lisp_Int)
	    {
	      XSETINT (v1, - XINT (v1));
	      PUSH (v1);
	    }
	  else
	    PUSH (Fminus (1, stackp));
	  break;

	case Bplus:
	  PUSH (Fplus (2, stackp -= 2));
	  break;

	case Bmax:
	  PUSH (Fmax (2, stackp -= 2));
	  break;

	case Bmin:
	  PUSH (Fmin (2, stackp -= 2));
	  break;

	case Bdot:
	  XFASTINT (v1) = dot;
	  PUSH (v1);
	  break;

	case Bmark:
	  PUSH (Fmark ());
	  break;

	case Bgoto_char:
	  PUSH (Fgoto_char (POP));
	  break;

	case Binsert:
	  PUSH (Finsert (1, stackp -= 1));
	  break;

	case Bdot_max:
	  XFASTINT (v1) = NumCharacters+1;
	  PUSH (v1);
	  break;

	case Bdot_min:
	  XFASTINT (v1) = FirstCharacter;
	  PUSH (v1);
	  break;

	case Bchar_after:
	  PUSH (Fchar_after (POP));
	  break;

	case Bfollowing_char:
	  XFASTINT (v1) = dot>NumCharacters ? 0 : CharAt(dot);
	  PUSH (v1);
	  break;

	case Bpreceding_char:
	  XFASTINT (v1) = dot<=FirstCharacter ? 0 : CharAt(dot-1);
	  PUSH (v1);
	  break;

	case Bcurrent_column:
	  XFASTINT (v1) = current_column ();
	  PUSH (v1);
	  break;

	case Bindent_to:
	  v1 = POP;
	  PUSH (Findent_to (v1, Qnil, Qnil));
	  break;

	case Bscan_buffer:
	  v3 = POP; v2 = POP; v1 = POP;
	  PUSH (Fscan_buffer (v1, v2, v3));
	  break;

	case Beolp:
	  PUSH (Feolp ());
	  break;

	case Beobp:
	  PUSH (Feobp ());
	  break;

	case Bbolp:
	  PUSH (Fbolp ());
	  break;

	case Bbobp:
	  PUSH (Fbobp ());
	  break;

	case Bcurrent_buffer:
	  PUSH (Fcurrent_buffer ());
	  break;

	case Bset_buffer:
	  PUSH (Fset_buffer (POP));
	  break;

	case Bread_char:
	  PUSH (Fread_char ());
	  QUIT;
	  break;

	case Bset_mark:
	  PUSH (Fset_mark (POP));
	  break;

	case Binteractive_p:
	  PUSH (Finteractive_p ());
	  break;

	default:
	  if (op >= Bconstant && op < Bconstant + CONSTANTLIM)
	    PUSH (vectorp[op - Bconstant]);
	}
    }

 exit:
  UNGCPRO;
  unbind_to (count);
  return val;
}

syms_of_bytecode ()
{
  Qbytecode = intern ("bytecode");
  staticpro (&Qbytecode);

  defsubr (&Sbyte_code);
}
