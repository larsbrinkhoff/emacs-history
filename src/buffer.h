/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


#ifdef lint
#include "undo.h"
#endif /* lint */


#define SetPoint  point =

#define PointRight  point +=
#define  PointLeft  point -=

struct buffer_text
  {
    unsigned char *p1;		/* Address of first data char, minus 1 */
    unsigned char *p2;		/* p1 plus gap size */
    int size1;			/* # characters before gap */
    int size2;			/* # characters after gap */
    int gap;			/* gap size in chars */
    int modified;		/* tick at which contents last modified */
    int head_clip;		/* # of first char that's visible (origin 1) */
    int tail_clip;		/* # chars not visible at end of buffer */
    int pointloc;		/* # of char point is at (origin 1) */
  };

/* structure that defines a buffer */
struct buffer
  {
    /* Everything before the `name' slot must be of a non-Lisp_Object type,
       and every slot after `name' must be a Lisp_Object
       This is known about by both mark_buffer (alloc.c) and
       Flist_buffer_local_variables (buffer.c)
     */

    /* This describes the buffer's text */
    struct buffer_text text;
    /* Next buffer, in chain of all buffers that exist.  */
    struct buffer *next;
    /* Flags saying which DefBufferLispVar variables
       are local to this buffer.  */
    int local_var_flags;
    /* Value of text.modified when buffer last saved */
    int save_modified;
    /* Set to the modtime of the visited file when read or written.
       -1 means visited file was nonexistent.
       0 means visited file modtime unknown; in no case complain
       about any mismatch on next save attempt.  */
    int modtime;
    /* the value of text.modified at the last auto-save. */
    int auto_save_modified;
    /* Position in buffer at which display started
       the last time this buffer was displayed */
    int last_window_start;
    /* Undo records for changes in this buffer. */
    struct UndoData *undodata;
    /* the syntax table in use */
    struct Lisp_Vector *syntax_table_v;

    /* This is a special exception -- as this slot should not be
       marked by gc_sweep, and as it is not lisp-accessible as
       a local variable -- so re regard it as not really being of type
       Lisp_Object */
    /* the markers that refer to this buffer.
       This is actually a single marker ---
       successive elements in its marker `chain'
       are the other markers referring to this
       buffer */
    Lisp_Object markers;


    /* Everything from here down must be a Lisp_Object */


    /* the name of this buffer */
    Lisp_Object name;
    /* Nuked: buffer number, assigned when buffer made Lisp_Object number;*/
    /* the name of the file associated with this buffer */
    Lisp_Object filename;
    /* Dir for expanding relative pathnames */
    Lisp_Object directory;
    /* true iff this buffer has been been backed
       up (if you write to its associated file
       and it hasn't been backed up, then a
       backup will be made) */
    Lisp_Object backed_up;
    /* Length of file when last read or saved. */
    Lisp_Object save_length;
    /* file name used for auto-saving this buffer */
    Lisp_Object auto_save_file_name;
    /* Non-nil if buffer read-only */
    Lisp_Object read_only;
    /* "The mark"; no longer allowed to be nil */
    Lisp_Object mark;

    /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER)
       for all per-buffer variables of this buffer.  */
    Lisp_Object local_var_alist;


    /* Symbol naming major mode (eg lisp-mode) */
    Lisp_Object major_mode;
    /* Pretty name of major mode (eg "Lisp") */
    Lisp_Object mode_name;
    /* Format string for mode line */
    Lisp_Object mode_line_format;

    /* Keys that are bound local to this buffer */
    Lisp_Object keymap;
    /* This buffer's local abbrev table */
    Lisp_Object abbrev_table;

    /* Values of several buffer-local variables */
    /* tab-width is buffer-local so that redisplay can find it
       in buffers that are not current */
    Lisp_Object case_fold_search;
    Lisp_Object tab_width;
    Lisp_Object fill_column;
    Lisp_Object left_margin;
    /* Function to call when insert space past fiull column */
    Lisp_Object auto_fill_hook;

    /* Non-nil means do not display continuation lines */
    Lisp_Object truncate_lines;
    /* Non-nil means display ctl chars with uparrow */
    Lisp_Object ctl_arrow;
    /* Non-nil means do selective display;
       See doc string in syms_of_buffer (buffer.c) for details.  */
    Lisp_Object selective_display;
    /* Non-nil means show ... at end of line followed by invisible lines.  */
    Lisp_Object selective_display_ellipses;
    /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer. */
    Lisp_Object minor_modes;
    /* t if "self-insertion" should overwrite */
    Lisp_Object overwrite_mode;
    /* non-nil means abbrev mode is on.  Expand abbrevs automatically. */
    Lisp_Object abbrev_mode;
};

extern struct buffer *bf_cur;		/* points to the current buffer */

/* This structure contains data describing the text of the current buffer.
 Switching buffers swaps their text data in and out of here */

extern struct buffer_text bf_text;

/* This structure holds the default values of the buffer-local variables
   defined with DefBufferLispVar, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

extern struct buffer buffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is zero, then even though there may
   be a DefBufferLispVar for the slot, there is no default valuefeor it;
   and the corresponding slot in buffer_defaults is not used.  */

extern struct buffer buffer_local_flags;

/* For each buffer slot, this points to the Lisp symbol name
   for that slot in the current buffer.  It is 0 for slots
   that don't have such names.  */

extern struct buffer buffer_local_symbols;

/* Some aliases for info about the current buffer.  */

#define bf_p1 bf_text.p1
#define bf_p2 bf_text.p2
#define bf_s1 bf_text.size1
#define bf_s2 bf_text.size2
#define bf_gap bf_text.gap
#define bf_modified bf_text.modified
#define bf_head_clip bf_text.head_clip
#define bf_tail_clip bf_text.tail_clip
#define point bf_text.pointloc

/* Lowest legal value of point for current buffer */
#define FirstCharacter bf_text.head_clip

/* Number of last visible character in current buffer */
/* The highest legal value for point is one greater than this */
#define NumCharacters (bf_text.size1+bf_text.size2-bf_text.tail_clip)

/* Return character at position n.  No range checking */
#define CharAt(n) *(((n)>bf_s1 ? bf_p2 : bf_p1) + (n))
  
/* BufferSafeCeiling (resp. BufferSafeFloor), when applied to n, return */
/* the max (resp. min) p such that &CharAt (p) - &CharAt (n) == p - n   */
#define BufferSafeCeiling(n) (bf_s1 + (((n) <= bf_s1) ? 0 : bf_s2))

#define BufferSafeFloor(n) (((n) <= bf_s1) ? 1 : bf_s1 + 1)

extern void reset_buffer ();
