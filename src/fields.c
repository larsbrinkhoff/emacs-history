/* Fields in the buffer.
   Copyright (C) 1993 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#define FIELD_START(F) marker_position (XCONS (XCONS ((F))->car)->car)
#define FIELD_END(F) marker_position (XCONS (XCONS ((F))->car)->cdr)
#define FIELD_DATA(F) (XCONS (XCONS ((F))->cdr)->cdr)

/* Return a list of all buffer fields that contain position POS.  */

Lisp_Object
fields_at (pos)
     int pos;
{
  Lisp_Object tail, field, result;

  result = Qnil;

  for (tail = current_buffer->fields_before;
       CONSP (tail);
       tail = XCONS (tail)->cdr)
    {
      field = XCONS (tail)->car;
      if (FIELD_END (field) <= pos)
	break;
      if (FIELD_START (field) <= pos)
	result = Fcons (field, result);
    }

  for (tail = current_buffer->fields_after;
       CONSP (tail);
       tail = XCONS (tail)->cdr)
    {
      field = XCONS (tail)->car;
      if (FIELD_START (field) > pos)
	break;
      if (FIELD_END (field) > pos)
	result = Fcons (field, result);
    }

  return result;
}

/* Shift fields in the field lists, to center them at POS.  */

void
relocate_field_lists (pos)
     int pos;
{
  Lisp_Object tail, next, other, otherfield;

  /* First, see if anything in fields_before should move to fields_after.  */

  for (tail = current_buffer->fields_before;
       CONSP (tail);
       tail = next)
    {
      next = XCONS (tail)->cdr;
      field = XCONS (tail)->car;

      if (FIELD_END (field) > pos)
	{
	  /* FIELD needs to be moved.  */
	  int where = FIELD_START (field);

	  /* Search thru fields_after for where to put it.  */
	  for (other = current_buffer->fields_after;
	       CONSP (other); other = XCONS (other)->cdr)
	    {
	      follower = XCONS (other)->cdr;
	      otherfield = XCONS (follower)->car;
	      if (FIELD_START (otherfield) >= where)
		{
		  XCONS (tail)->cdr = follower;
		  XCONS (other)->cdr = tail;
		}
	    }
	}
      else
	/* We've reached the things that should stay in fields_before.
	   All the rest of fields_before must end even earlier,
	   so stop now.  */
	break;
    }

  /* See if anything in fields_after should be in fields_before.  */
  for (tail = current_buffer->fields_after;
       CONSP (tail);
       tail = next)
    {
      next = XCONS (tail)->cdr;
      field = XCONS (tail)->car;

      /* Stop looking, when we know that nothing further
	 can possibly end before POS.  */
      if (FIELD_START (field) > pos)
	break;

      if (FIELD_END (field) <= pos)
	{
	  /* FIELD needs to be moved.  */
	  int where = FIELD_END (field);

	  /* Search thru fields_before for where to put it.  */
	  for (other = current_buffer->fields_before;
	       CONSP (other); other = XCONS (other)->cdr)
	    {
	      follower = XCONS (other)->cdr;
	      otherfield = XCONS (follower)->car;
	      if (FIELD_END (otherfield) <= where)
		{
		  XCONS (tail)->cdr = follower;
		  XCONS (other)->cdr = tail;
		}
	    }
	}
    }

  current_buffer->field_center = pos;
}

DEFUN ("make-field", Fmake_field, Smake_field, 2, 2, 0,
  "Create a new field in the current buffer, with range START to END.")
  (beg, end)
     Lisp_Object beg, end;
{
  Lisp_Object field;

  field = Fcons (Fcons (Fcopy_marker (beg), Fcopy_marker (end)), Qnil);

  /* Put the new field on fields_after regardless of where it belongs.  */ 
  current_buffer->fields_after = Fcons (field, current_buffer->fields_after);

  /* This puts it where it belongs.  */
  relocate_field_lists (current_buffer->field_center);
}

DEFUN ("field-get", Ffield_get, Sfield_get, 2, 2, 0,
  "Get the property of field FIELD with property name NAME.")
  (field, prop)
     Lisp_Object field, prop;
{
  Lisp_Object plist;
  for (plist = Fcdr_safe (Fcdr_safe (field));
       CONSP (plist) && CONSP (XCONS (plist)->cdr);
       plist = XCONS (XCONS (plist)->cdr)->cdr)
    {
      if (EQ (XCONS (plist)->car, prop))
	return XCONS (XCONS (plist)->cdr)->car;
    }
}

DEFUN ("field-put", Ffield_put, Sfield_put, 3, 3, 0,
  "Set one property of field FIELD: give property PROP value VALUE.")
  (field, prop, value)
     Lisp_Object field, prop, value;
{
  Lisp_Object plist, tail;

  plist = Fcdr_safe (Fcdr_safe (field));

  for (tail = plist;
       CONSP (tail) && CONSP (XCONS (tail)->cdr);
       tail = XCONS (XCONS (tail)->cdr)->cdr)
    {
      if (EQ (XCONS (tail)->car, prop))
	return XCONS (XCONS (tail)->cdr)->car = value;
    }

  XCONS (XCONS (field)->cdr)->cdr
    = Fcons (prop, Fcons (value, plist));

  return value;
}

DEFUN ("delete-field", Fdelete_field, Sdelete_field, 1, 1, 0,
  "Delete the field FIELD from the current buffer.")
  (field)
{
  current_buffer->fields_before = Fdelq (field, current_buffer->fields_before);
  current_buffer->fields_after = Fdelq (field, current_buffer->fields_after);
  return Qnil;
}
