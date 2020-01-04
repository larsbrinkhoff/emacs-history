#define BIGDEBUG
/* X Communication module for terminals which understand the X protocol.
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

/* Written by Yakim Martillo, mods and things by Robert Krawitz  */
/* Redone for X11 by Robert French */

/*
 *	$Source: /@/orpheus/u1/X11/clients/emacs/RCS/xterm.c,v $
 *	$Author: newman $
 *	$Locker:  $
 *	$Header: xterm.c,v 1.2 87/09/12 16:34:06 newman Exp $
 */

#ifndef lint
static char *rcsid_xterm_c = "$Header: xterm.c,v 1.2 87/09/12 16:34:06 newman Exp $";
#endif	lint

#include "config.h"

#ifdef HAVE_X_WINDOWS

#include "lisp.h"
#undef NULL

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>

/* This may include sys/types.h, and that somehow loses
 * if this is not done before the other system files.  */

/*#include "xdebug.h"*/

#include "xterm.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#include <sys/time.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#ifdef BSD
#include <strings.h>
#endif
#include <sys/stat.h>

#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "sink.h"
#include "sinkmask.h"

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define sigunblockx(sig) sigblock (0)
#define sigblockx(sig) sigblock (1 << ((sig) - 1))

#define METABIT 0200
#define MINWIDTH 12
#define MINHEIGHT 5

int pixelwidth;
int pixelheight;
char *progname;

XEvent *XXm_queue[XMOUSEBUFSIZE];
int XXm_queue_num;
char *XXcurrentfont;
XFontStruct *fontinfo;
Font XXfid;
int XXfontw,XXfonth,XXbase,XXisColor;
Colormap XXColorMap;
char *default_window;
int configure_pending;
extern struct display_line *DesiredScreen[], *PhysScreen[];
extern int initialized;

extern char *alternate_display;

int XXdebug;
int XXpid;
extern int screen_garbaged;
int XXxoffset, XXyoffset;

int WindowMapped;

static int flexlines;		/* last line affected by dellines or */
				/* inslines functions */
extern int errno;
int VisibleX, VisibleY;		/* genuine location of cursor on screen */
				/* if it is there */
static int SavedX, SavedY;	/* Where the cursor was before update */
				/* started */

int CursorExists;		/* during updates cursor is turned off */
static int InUpdate;		/* many of functions here may be invoked */
				/* even if no update in progress, when */
				/* no update is in progress the action */
				/* can be slightly different */

Display *XXdisplay;
Window XXwindow;
GC XXgc_norm,XXgc_rev,XXgc_curs,XXgc_temp;
XGCValues XXgcv;
Cursor EmacsCursor;
Pixmap SinkPixmap, SinkMaskPixmap;

char *fore_color;	/* Variables to store colors */
char *back_color;
char *brdr_color;
char *curs_color;
char *mous_color;

unsigned long fore;
unsigned long back;
unsigned long brdr;
unsigned long curs;
unsigned long mous;

char *desiredwindow;

int CurHL;			/* Current Highlighting actually being */
				/* being used for bold font right now*/

int XXborder;
int XXInternalBorder;

int (*handler)();

char *rindex();

extern Display *XOpenDisplay ();
extern Window XCreateSimpleWindow();
extern XFontStruct *XLoadQueryFont();

#ifdef BIGDEBUG
char debug_string[500000];
#endif BIGDEBUG

/* HLmode -- Changes the GX function for output strings.  Could be used to
 * change font.  Check an XText library function call. 
 */

HLmode (new)
     int new;
{
	CurHL =  new;
}


/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

XTreassert_line_highlight (highlight, vpos)
     int highlight, vpos;
{
	HLmode (highlight);
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

XTchange_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
	HLmode (new_highlight);
	XTtopos (vpos, 0);
	XTclear_end_of_line (0);
}


/* Used for starting or restarting (after suspension) the X window.  Puts the
 * cursor in a known place, update does not begin with this routine but only
 * with a call to DoDsp.
 */

XTset_terminal_modes ()
{
	int stuffpending;
#ifdef XDEBUG
	fprintf (stderr, "XTset_terminal_modes\n");
#endif

	InUpdate = 0;
	stuffpending = 0;
	if (!initialized) {
		CursorExists = 0;
		VisibleX = 0;
		VisibleY = 0;
	}
	XTclear_screen ();

#ifdef FIONREAD
	ioctl (0, FIONREAD, &stuffpending);
	if (stuffpending)
		read_events_block ();
#endif

}

/* XTtopos moves the cursor to the correct location and checks whether an
 * update is in progress in order to toggle it on.
 */

XTtopos (row, col)
     register int row, col;
{
	int mask = sigblock (sigmask (SIGIO));
#ifdef XDEBUG
	fprintf (stderr, "XTtopos (X %d, Y %d)\n",col,row);
#endif

	cursX = col;
	cursY = row;

	if (InUpdate) {
		if (CursorExists)
			CursorToggle ();
		sigsetmask (mask);
		return;
		/* Generally, XTtopos will be invoked */
		/* when InUpdate with !CursorExists */
		/* so that wasteful XFlush is not called */
	}
	if ((row == VisibleY) && (col == VisibleX)) {
		if (!CursorExists)
			CursorToggle ();
		XFlush (XXdisplay);
		sigsetmask (mask);
		return;
	}
	if (CursorExists)
		CursorToggle ();
	VisibleX = col;
	VisibleY = row;
	if (!CursorExists)
		CursorToggle ();
	XFlush (XXdisplay);
	sigsetmask (mask);
}

/* Used to get the terminal back to a known state after resets.  Usually
 * used when restarting suspended or waiting emacs
 */

cleanup ()
{
	inverse_video = 0;
	HLmode (0);
}

/* wipes out numcols columns starting a current column on the current line */
  
XTclear_end_of_line (first_blank)
     register int first_blank;
{
	register int numcols;
	int mask;

#ifdef XDEBUG
	fprintf (stderr, "XTclear_end_of_line (blank %d)\n",first_blank);
#endif

	if (cursY < 0 || cursY >= screen_height)
		return;

#ifdef notdef
	if (first_blank >= screen_width)
		return;

	if (first_blank < 0)
		first_blank = 0;
#endif notdef
	
	mask = sigblock (sigmask (SIGIO));
	numcols = screen_width - cursX;
	if (cursY == VisibleY && VisibleX >= first_blank && CursorExists)
		CursorToggle ();

	if (CurHL)
		XFillRectangle (XXdisplay, XXwindow, XXgc_norm,
				cursX*XXfontw+XXInternalBorder,
				cursY*XXfonth+XXInternalBorder,
				XXfontw*numcols,
				XXfonth);
	else
		XClearArea (XXdisplay, XXwindow, 
			    cursX*XXfontw+XXInternalBorder, 
			    cursY*XXfonth+XXInternalBorder, 
			    XXfontw*numcols,
			    XXfonth,
			    0);
	XTtopos (cursY, first_blank);
	sigsetmask (mask);
}

XTreset_terminal_modes ()
{
#ifdef XDEBUG
	fprintf (stderr, "XTreset_terminal_modes\n");
#endif

	XTclear_screen ();
}

XTclear_screen ()
{
	int mask = sigblock (sigmask (SIGIO));

#ifdef XDEBUG
	fprintf (stderr, "XTclear_screen\n");
#endif

	HLmode (0);
	CursorExists = 0;
  
	cursX = 0;
	cursY = 0;
	SavedX = 0;
	SavedY = 0;
	VisibleX = 0;
	VisibleY = 0;
	XClearWindow(XXdisplay, XXwindow);
	CursorToggle ();
	if (!InUpdate)
		XFlush (XXdisplay);
	sigsetmask (mask);
}

/* used by dumprectangle which is usually invoked upon Expose
 * events which come from bit blt's or moving an obscuring opaque window
 */

dumpchars (ActiveScreen, numcols, tempX, tempY, tempHL)
     register struct display_line **ActiveScreen;
     register int numcols;
     register int tempX, tempY, tempHL;
{
	if (numcols <= 0)
		return;

	if (numcols-1+tempX > screen_width)
		numcols = screen_width-tempX+1;

	if (tempX < 0 || tempX >= screen_width ||
	    tempY < 0 || tempY >= screen_height)
		return;

	XDrawImageString(XXdisplay, XXwindow, tempHL ? XXgc_rev : XXgc_norm,
			 tempX*XXfontw+XXInternalBorder,
			 tempY*XXfonth+XXInternalBorder+XXbase,
			 &ActiveScreen[tempY+1]->body[tempX],
			 numcols);
}

/* When a line has been changed this function is called.  X is so fast
 * that the actual sequence is ignore.  Rather, the new version of the
 * line is simply output if this function is invoked while in UpDate.
 * Sometimes writechars can be invoked when not in update if text is to
 * be output at the end of the line.  In this case the whole line is not
 * output.  Simply the new text at the current cursor position given
 * by VisibleX,Y.  The cursor is moved to the end of the new text.
 */

writechars (start, end)
     register char *start, *end;
{
	int mask = sigblock (sigmask (SIGIO));

	if (cursY < 0 || cursY >= screen_height) {
		sigsetmask (mask);
		return;
	}

	if (CursorExists)
		CursorToggle ();

	if (cursY < 0 || cursY >= screen_height ||
	    cursX < 0 || cursX >= screen_width) {
		sigsetmask (mask);
		return;
	}
	
	if (end-start+cursX >= screen_width)
		end = start+screen_width-cursX -1;

	if (end >= start) {
		if (start)
			XDrawImageString(XXdisplay, XXwindow,
					 CurHL ? XXgc_rev : XXgc_norm,
					 cursX * XXfontw+XXInternalBorder,
					 cursY * XXfonth+XXInternalBorder+XXbase,
					 start,
					 end-start+1);
		else
			if (CurHL)
				XFillRectangle (XXdisplay, XXwindow, XXgc_norm,
						cursX*XXfontw+XXInternalBorder,
						cursY*XXfonth+XXInternalBorder,
						(end-start+1)*XXfontw,
						XXfonth);
			else
				XClearArea (XXdisplay, XXwindow, 
					    cursX*XXfontw+XXInternalBorder, 
					    cursY*XXfonth+XXInternalBorder, 
					    (end-start+1)*XXfontw,
					    XXfonth,0);
		VisibleX = cursX+end-start+1;
		if (InUpdate)
			cursX = VisibleX;
	} 
	if (!InUpdate && !CursorExists)
		CursorToggle();
	sigsetmask (mask);
}

static 
XTwrite_chars (start, len)
     register char *start;
     register int len;
{
#ifdef XDEBUG
	fprintf (stderr, "XTwrite_chars (len %d)\n",len);
#endif

	writechars (start, start+len-1);
}

/* The following routine is for the deaf or for the pervert who prefers 
 * that his terminal flashes at him rather than beep at him.
 */

static int flashedback;

XTflash ()
{
	XGCValues gcv_temp;
	struct itimerval itimer;
	int mask = sigblock (sigmask (SIGIO));

	extern int flashback ();

#ifdef XDEBUG
	fprintf (stderr, "XTflash\n");
#endif

	XXgc_temp = XCreateGC(XXdisplay, XXwindow, 0, &gcv_temp);
	XSetState(XXdisplay, XXgc_temp, WhitePixel(XXdisplay,0),
		  BlackPixel(XXdisplay,0), GXinvert,
		  AllPlanes);
	
	signal (SIGALRM, flashback);
	getitimer (ITIMER_REAL, &itimer);
	itimer.it_value.tv_usec += 250000;
	itimer.it_interval.tv_sec = 0;
	itimer.it_interval.tv_usec = 0;
	flashedback = 0;

	setitimer (ITIMER_REAL, &itimer, 0);

	XFillRectangle (XXdisplay, XXwindow, XXgc_temp, 0, 0,
			screen_width*XXfontw+2*XXInternalBorder,
	  	 	screen_height*XXfonth+2*XXInternalBorder);
	XFlush (XXdisplay);

	sigsetmask (mask);

	while (!flashedback)
		pause ();

	XFreeGC(XXdisplay, XXgc_temp);
}

flashback ()
{
	int mask = sigblock (sigmask (SIGIO) | sigmask (SIGALRM));

	XFillRectangle (XXdisplay, XXwindow, XXgc_temp, 0, 0,
			screen_width*XXfontw+2*XXInternalBorder,
	  	 	screen_height*XXfonth+2*XXInternalBorder);
	XFlush (XXdisplay);

	flashedback = 1;
	sigsetmask (mask);
}	

XTfeep ()
{
	int mask = sigblock (sigmask (SIGIO));
#ifdef XDEBUG
	fprintf (stderr, "XTfeep\n");
#endif
	XBell (XXdisplay,50);
	sigsetmask (mask);
}

/* Artificially creating a cursor is hard, the actual position on the
 * screen (either where it is or last was) is tracked with VisibleX,Y.
 * Gnu Emacs code tends to assume a cursor exists in hardward at cursX,Y
 * and that output text will appear there.  During updates, the cursor is
 * supposed to be blinked out and will only reappear after the update 
 * finishes.
 */

CursorToggle ()
{
	register struct display_line **ActiveScreen;

	if (!WindowMapped) {
		CursorExists = 0;
		return 0;
		/* Currently the return values are not */
		/* used, but I could anticipate using */
		/* them in the future. */
	}
	
	if (VisibleX < 0 || VisibleX >= screen_width ||
	    VisibleY < 0 || VisibleY >= screen_height) {
		/* Not much can be done */
		XFlush (XXdisplay);
		CursorExists = 0;
		return 0;
	}

	ActiveScreen = PhysScreen;

	if (ActiveScreen && ActiveScreen[VisibleY+1] &&
	    VisibleX < ActiveScreen[VisibleY+1]->length)
		if (CursorExists)
			XDrawImageString(XXdisplay, XXwindow, XXgc_norm,
				    VisibleX*XXfontw+XXInternalBorder,
				    VisibleY*XXfonth+XXInternalBorder+XXbase,
				    &ActiveScreen[VisibleY+1]->body[VisibleX],
				    1);
		else
			XDrawImageString(XXdisplay, XXwindow, XXgc_curs,
				    VisibleX*XXfontw+XXInternalBorder,
				    VisibleY*XXfonth+XXInternalBorder+XXbase,
				    &ActiveScreen[VisibleY+1]->body[VisibleX],
				    1);
	else
		if (CursorExists)
			XClearArea (XXdisplay, XXwindow,
				    VisibleX*XXfontw+XXInternalBorder,
				    VisibleY*XXfonth+XXInternalBorder,
				    XXfontw, XXfonth, 0);
		else
			XFillRectangle (XXdisplay, XXwindow, XXgc_norm,
					VisibleX*XXfontw+XXInternalBorder,
					VisibleY*XXfonth+XXInternalBorder,
					XXfontw, XXfonth);

	CursorExists = !CursorExists;

	if (!InUpdate)
		XFlush (XXdisplay);

	return 1;
}

/* This routine is used by routines which are called to paint regions */
/* designated by Expose events.  If the cursor may be in the exposed */
/* region, this routine makes sure it is gone so that dumprectangle can */
/* toggle it back into existance if dumprectangle is invoked when not in */
/* the midst of a screen update. */

static 
ClearCursor ()
{
	int mask = sigblock (sigmask (SIGIO));

	if (!WindowMapped) {
		CursorExists = 0;
		sigsetmask (mask);
		return;
	}

	if (VisibleX < 0 || VisibleX >= screen_width ||
	    VisibleY < 0 || VisibleY >= screen_height) {
		/* Not much can be done */
		CursorExists = 0;
		sigsetmask (mask);
		return;
	}

	XClearArea (XXdisplay, XXwindow,
		    VisibleX*XXfontw+XXInternalBorder,
		    VisibleY*XXfonth+XXInternalBorder,
		    XXfontw, XXfonth, 0);

	CursorExists = 0;
	sigsetmask (mask);
}

XTupdate_begin ()
{
	int mask = sigblock (sigmask (SIGIO));

#ifdef XDEBUG
	fprintf (stderr, "XTupdate_begin\n");
#endif

	InUpdate = 1;
	if (CursorExists) 
		CursorToggle ();

	SavedX = cursX;
	SavedY = cursY;
	/* Thw initial "hardware" cursor position is */
	/* saved because that is where gnu emacs */
	/* expects the cursor to be at the end of */
	/* the update */

	sigsetmask (mask);
}

XTupdate_end ()
{	
	int mask = sigblock (sigmask (SIGIO));

#ifdef XDEBUG
	fprintf (stderr, "XTupdate_end\n");
#endif

	if (CursorExists)
		CursorToggle ();

	InUpdate = 0;
	XTtopos (SavedY, SavedX); /* XTtopos invokes cursor toggle */
	XFlush (XXdisplay);
	sigsetmask (mask);
}

/* Used for Expose events.  Have to get the text
 * back into the newly blank areas.
 */

dumprectangle (top, left, rows, cols)
     register int top, left, rows, cols;
{
	register struct display_line **ActiveScreen;
	register int ourindex;
	int localX, localY, localHL;

	rows += top;
	cols += left;
	top /= XXfonth;
	/* Get row and col containing up and */
	/* left borders of exposed region -- */
	/* round down here*/
	left /= XXfontw;
	rows += XXfonth-1;
	cols += XXfontw-1;
	rows /= XXfonth;
	/* Get row and col containing bottom and */
	/* right borders -- round up here */
	rows -= top;
	cols /= XXfontw;
	cols -= left;

	if (rows < 0)
		return;
	if (cols < 0)
		return;
	if (top > screen_height - 1)
		return;
	if (left > screen_width - 1)
		return;
	if (VisibleX >= left && VisibleX < left + cols &&
	    VisibleY >= top && VisibleY < top + rows)
		ClearCursor ();

	if (InUpdate && DesiredScreen)
		ActiveScreen = DesiredScreen;
	else
		if (PhysScreen)
			/* When queue is dumped in update this */
			ActiveScreen = PhysScreen;
	else
		return;

	/* should perhaps be DesiredScreen */
	/* but PhysScreen is guaranteed to contain */
	/* data which was good for every line on */
	/* screen. For desired screen only for */
	/* lines which are changing.  Emacs does */
	/* not consider a line within a newly */
	/* exposed region necessarily to have */
	/* been changed.  Emacs knows nothing */
	/* about Expose events. */
	
	for (localY = top, ourindex = 0;
	     ourindex < rows && localY < screen_height;
	     ++ourindex, ++localY) {
		if (localY < 0 || localY >= screen_height ||
		    !ActiveScreen[localY+1] ||
		    left+1 > ActiveScreen[localY+1]->length)
			continue;
		localX = left;
		localHL = ActiveScreen[localY+1]->highlighted;
		dumpchars (ActiveScreen,
			   min (cols,
				ActiveScreen[localY+1]->length-localX),
			   localX, localY, localHL);
	}
	if (!InUpdate && !CursorExists)
		CursorToggle ();
	/* Routine usually called */
	/* when not in update */
}

/* What sections of the window will be modified from the UpdateDisplay
 * routine is totally under software control.  Any line with Y coordinate
 * greater than flexlines will not change during an update.  This is really
 * used only during dellines and inslines routines (scraplines and stufflines)
 */

XTset_terminal_window (n)
     register int n;
{
#ifdef XDEBUG
	fprintf (stderr, "XTset_terminal_window\n");
#endif

	if (n <= 0 || n > screen_height)
		flexlines = screen_height;
	else
		flexlines = n;
}

XTins_del_lines (vpos, n)
     int vpos, n;
{
#ifdef XDEBUG
	fprintf (stderr, "XTins_del_lines\n");
#endif

	XTtopos (vpos, 0);
	if (n >= 0)
		stufflines (n);
	else
		scraplines (-n);
}

static 
XTinsert_chars (start, len)
     register char *start;
     register int len;
{
	int mask = sigblock (sigmask (SIGIO));
	
#ifdef XDEBUG
	fprintf (stderr, "XTinsert_chars\n");
#endif

	if (len > 0) {
		XCopyArea (XXdisplay, XXwindow, XXwindow, XXgc_norm,
			   cursX*XXfontw+XXInternalBorder,
			   cursY*XXfonth+XXInternalBorder,
			   (screen_width-cursX-len)*XXfontw,
			   XXfonth,
			   (cursX+len)*XXfontw+XXInternalBorder,
			   cursY*XXfonth+XXInternalBorder);
		writechars (start, start+len-1);
	}

	sigsetmask(mask);
}

static 
XTdelete_chars (n)
     register int n;
{
	int mask = sigblock (sigmask (SIGIO));
	
#ifdef XDEBUG
	fprintf (stderr, "XTdelete_chars (num %d)\n",n);
#endif

	/* XXX WARNING!  This is dangerous...We aren't waiting for the
	 * exposures from the copyarea because it slows things down too
	 * much.  This could cause updating problems under some
	 * circumstances. */
	
	if (n > 0) {
		XCopyArea (XXdisplay, XXwindow, XXwindow, XXgc_norm,
			   (cursX+n)*XXfontw+XXInternalBorder,
			   cursY*XXfonth+XXInternalBorder,
			   (screen_width-cursX-n)*XXfontw,
			   XXfonth,
			   cursX*XXfontw+XXInternalBorder,
			   cursY*XXfonth+XXInternalBorder);
	}
	
	if (n >= 0) {
		XClearArea (XXdisplay, XXwindow,
			    (screen_width-n)*XXfontw+XXInternalBorder,
			    cursY*XXfonth+XXInternalBorder,
			    n*XXfontw+XXInternalBorder,
			    XXfonth,0);
	}
	cursX += n;
	
	sigsetmask (mask);
}

stufflines (n)
     register int n;
{
	register int topregion, bottomregion;
	register int length, newtop, mask;

	if (cursY >= flexlines)
		return;
  
	mask = sigblock (sigmask (SIGIO));

	if (CursorExists)
		CursorToggle ();

	sigsetmask (mask);
	topregion = cursY;
	bottomregion = flexlines-(n+1);
	newtop = cursY+n;
	length = bottomregion-topregion+1;

	if (length > 0 && newtop <= flexlines) {
		mask = sigblock (sigmask (SIGIO));
		XCopyArea (XXdisplay, XXwindow, XXwindow, XXgc_norm,
			   XXInternalBorder,
			   topregion*XXfonth+XXInternalBorder,
			   screen_width*XXfontw,
			   length*XXfonth,
			   XXInternalBorder, newtop*XXfonth+XXInternalBorder);
	}

	newtop = min (newtop, flexlines-1);
	length = newtop-topregion;
	if (length > 0)
		XClearArea (XXdisplay, XXwindow,
			    XXInternalBorder,
			    topregion*XXfonth+XXInternalBorder,
			    screen_width*XXfontw,
			    n*XXfonth, 0);
}

scraplines (n)
     register int n;
{
	int mask;

	if (cursY >= flexlines)
		return;

	mask = sigblock (sigmask (SIGIO));

	if (CursorExists)
		CursorToggle ();

	if (cursY+n >= flexlines) {
		if (flexlines >= (cursY + 1))
			XClearArea (XXdisplay, XXwindow,
				    XXInternalBorder,
				    cursY*XXfonth+XXInternalBorder,
				    screen_width*XXfontw,
				    (flexlines-cursY) * XXfonth,
				    0);
		sigsetmask (mask);
	}
	else {
		XCopyArea (XXdisplay, XXwindow, XXwindow, XXgc_norm,
			   XXInternalBorder,
			   (cursY+n)*XXfonth+XXInternalBorder,
			   screen_width*XXfontw,
			   (flexlines-cursY-n)*XXfonth,
			   XXInternalBorder, cursY*XXfonth+XXInternalBorder);

		XClearArea (XXdisplay, XXwindow, XXInternalBorder,
			    (flexlines-n)*XXfonth+XXInternalBorder,
			    screen_width*XXfontw,
			    n*XXfonth, 0);
		sigsetmask (mask);
	}
}
	
/* Substitutes for standard read routine.  Under X not interested in individual
 * bytes but rather individual packets.
 */

XTread_socket (sd, bufp, numchars)
     register int sd;
     register char *bufp;
     register int numchars;
{
#ifdef XDEBUG
	fprintf(stderr,"XTread_socket\n");
#endif

	return (internal_socket_read (bufp, numchars));
}

read_events_block ()
{
	unsigned char buf[64];
	int i, nread;

#ifdef XDEBUG
	fprintf(stderr,"read_events_block\n");
#endif
	
	nread = internal_socket_read (buf, sizeof buf);
	if (!nread)
		return;
	
	for (i=0; i < nread; i++) {
		kbd_buffer_store_char (buf[i]);
		/* Don't look at input that follows a C-g too closely.
		 * This reduces lossage due to autorepeat on C-g.  */
		if (buf[i] == ('G' & 037))
			break;
	}
}

internal_socket_read(bufp, numchars)
	register unsigned char *bufp;
	register int numchars;
{
	int count,nbytes,rows,cols;
	char mapping_buf[20];
	int mask = sigblock (sigmask (SIGIO));
	XEvent event;
	XComposeStatus status;

	extern int input_pending;

	count = 0;

#ifdef BIGDEBUG
	my_log("Entering internal_socket_read input_pending = %d\n",input_pending);
#endif BIGDEBUG
	
	while (XPending (XXdisplay)) {
		XNextEvent (XXdisplay,&event);

#ifdef BIGDEBUG
		my_log("Got event %d\n",event.type);
#endif BIGDEBUG

		switch (event.type) {

		default:
			break;

		case ConfigureNotify:
			XXxoffset = event.xconfigure.x;
			XXyoffset = event.xconfigure.y;
			if (abs(pixelheight-event.xconfigure.height) < 
			    XXfonth &&
			    abs(pixelwidth-event.xconfigure.width) < 
			    XXfontw)
				break;

			configure_pending = 1;

			rows = event.xconfigure.height/XXfonth;
			cols = event.xconfigure.width/XXfontw;
			pixelwidth = cols*XXfontw+2*XXInternalBorder;
			pixelheight = rows*XXfonth+2*XXInternalBorder;

			break;

		case Expose:
			if (configure_pending) {
				if (event.xexpose.count)
					break;
				change_screen_size ((pixelheight-2*XXInternalBorder)/XXfonth,
						    (pixelwidth-2*XXInternalBorder)/XXfontw);
				configure_pending = 0;
				break;
			} 
			dumprectangle (event.xexpose.y-XXInternalBorder,
				       event.xexpose.x-XXInternalBorder,
				       event.xexpose.height,
				       event.xexpose.width);
			break;

		case GraphicsExpose:
			dumprectangle (event.xgraphicsexpose.y-XXInternalBorder,
				       event.xgraphicsexpose.x-XXInternalBorder,
				       event.xgraphicsexpose.height,
				       event.xgraphicsexpose.width);
			break;

		case NoExpose:
			break;
			
		case KeyPress:
			nbytes = 0;
			switch (event.xkey.keycode) {
			case 167:
				strcpy(mapping_buf,"\002");
				nbytes = 1;
				break;
			case 168:
				strcpy(mapping_buf,"\006");
				nbytes = 1;
				break;
			case 170:
				strcpy(mapping_buf,"\020");
				nbytes = 1;
				break;
			case 169:
				strcpy(mapping_buf,"\016");
				nbytes = 1;
				break;
			}
			if (!nbytes) {
				nbytes = XLookupString (&event,
							mapping_buf, 20, 0,
							&status);
				if (nbytes) {
					if (event.xkey.state & Mod1Mask)
						*mapping_buf |= METABIT;
				}
			}
			if (numchars-nbytes > 0) {
				bcopy (mapping_buf, bufp, nbytes);
				bufp += nbytes;
				count += nbytes;
				numchars -= nbytes;
			}
			break;

		case ButtonPress:
		case ButtonRelease:
			*bufp++ = (char) 'X' & 037;
			++count;
			--numchars;
			*bufp++ = (char) '@' & 037;
			++count;
			--numchars;
			if (XXm_queue_num == XMOUSEBUFSIZE)
				break;
			XXm_queue[XXm_queue_num] = (XEvent *)
				malloc(sizeof(XEvent));
			*XXm_queue[XXm_queue_num++] = event;
			break;
		}
	} 

	if (CursorExists)
		xfixscreen ();

	sigsetmask (mask);
	return count;
}

#ifdef notdef
XBitmapIcon () 
{
	XWMHints wmhints;
	int mask = sigblock (sigmask (SIGIO));

	wmhints.flags = IconPixmapHint|IconMaskHint;
	wmhints.icon_pixmap = SinkPixmap;
	wmhints.icon_mask = SinkMaskPixmap;
	XSetWMHints (XXdisplay, XXwindow, &wmhints);

	sigsetmask (mask);
}

XTextIcon () 
{
	XWMHints wmhints;
	int mask = sigblock (sigmask (SIGIO));

	wmhints.flags = 0;
	XSetWMHints (XXdisplay, XXwindow, &wmhints);

	sigsetmask (mask);
}
#endif notdef

/* Interpreting incoming keycodes. Should have table modifiable as needed
 * from elisp.
 */

/* Exit gracefully from gnuemacs, doing an autosave and giving a status.
 */

XExitGracefully ()
{
	XCleanUp();
	exit (70);
}

XIgnoreError ()
{
	return 0;
}

xfixscreen ()
{
	int mask = sigblock (sigmask (SIGIO));

	/* Yes, this is really what I mean -- Check to see if we've
	 * lost our connection */
	
	XSetErrorHandler(0);
	XSetIOErrorHandler(0);
	XNoOp (XXdisplay);
	XFlush (XXdisplay);
	XSetErrorHandler(handler);
	XSetIOErrorHandler(handler);
	if (!InUpdate && !CursorExists)
		CursorToggle ();

	sigsetmask (mask);
}

x_term_init ()
{
	register char *vardisplay;
	register int xxargc;
	register char **xxargv;
	extern char *getenv ();
	char *temp_font;
	char *option;
	extern XTinterrupt_signal ();
	extern char *malloc ();
	int reversevideo;
	XColor cdef;
	XColor cursor_fore, cursor_back;
	extern Lisp_Object Vxterm, Vxterm1, Qt;
	extern int xargc;
	extern char **xargv;
	extern void init_sigio (), request_sigio (), unrequest_sigio ();
	extern int XIgnoreError();

	vardisplay = (alternate_display ? alternate_display : "");
	if (!vardisplay) {
		fprintf (stderr, "DISPLAY environment variable must be set\n");
		exit (-200);
	}

	XXdisplay = XOpenDisplay (vardisplay);
	if (XXdisplay == (Display *) 0) {
		fprintf (stderr, "X server not responding.  Check your DISPLAY environment variable.\n");
		exit (-99);	
	}

	XXisColor = DisplayCells(XXdisplay,0)>2;
	XXColorMap = DefaultColormap(XXdisplay,0);
	
	x_init_1 (1);
	WindowMapped = 0;
	baud_rate = 9600;
	min_padding_speed = 10000;
	must_write_spaces = 1;
	MetaFlag = 1;
	visible_bell = 1;
	interrupt_input = 1;
	inverse_video = 1;
	configure_pending = 0;
	
	fix_screen_hook = xfixscreen;
	clear_screen_hook = XTclear_screen;
	clear_end_of_line_hook = XTclear_end_of_line;
	ins_del_lines_hook = XTins_del_lines;
	change_line_highlight_hook = XTchange_line_highlight;
	insert_chars_hook = XTinsert_chars;
	write_chars_hook = XTwrite_chars;
	delete_chars_hook = XTdelete_chars;
	ring_bell_hook = XTfeep;
	reset_terminal_modes_hook = XTreset_terminal_modes;
	set_terminal_modes_hook = XTset_terminal_modes;
	update_begin_hook = XTupdate_begin;
	update_end_hook = XTupdate_end;
	set_terminal_window_hook = XTset_terminal_window;
	read_socket_hook = XTread_socket;
	topos_hook = XTtopos;
	reassert_line_highlight_hook = XTreassert_line_highlight;
	scroll_region_ok = 1;	/* we'll scroll partial screens */
	char_ins_del_ok = 1;	/* Don't bother */
	line_ins_del_ok = 1;	/* we'll just blt 'em */
	fast_clear_end_of_line = 1; /* X does this well */
	memory_below_screen = 0; /* we don't remember what scrolls 
				  * 		off the bottom */
	dont_calculate_costs = 1;

	/* New options section */
	XXborder = 1;
	XXInternalBorder = 1;
	screen_width = 80;
	screen_height = 66;
	reversevideo = 0;
	XXxoffset = 0;
	XXyoffset = 0;
	XXdebug = 0;
	XXm_queue_num = 0;

	handler = XIgnoreError;
	XSetErrorHandler (handler);
	XSetIOErrorHandler (handler);
	
	progname = xargv[0];
	if (rindex(progname,'/'))
		progname = rindex(progname,'/')+1;
	
	if (option = XGetDefault (XXdisplay, progname, "ReverseVideo"))
		if (!strcmp (option,"on"))
			reversevideo = 1;
	if (option = XGetDefault (XXdisplay, progname, "BorderWidth"))
		XXborder = atoi (option);
	if (option = XGetDefault (XXdisplay, progname, "InternalBorder"))
		XXInternalBorder = atoi (option);

	if (!(brdr_color = XGetDefault (XXdisplay, progname, "Border")))
		brdr_color = XGetDefault (XXdisplay, progname, "BorderColor");
	back_color = XGetDefault (XXdisplay, progname, "Background");
	fore_color = XGetDefault (XXdisplay, progname, "Foreground");
	mous_color = XGetDefault (XXdisplay, progname, "Mouse");
	curs_color = XGetDefault (XXdisplay, progname, "Cursor");

	temp_font  = XGetDefault (XXdisplay, progname, "BodyFont");
	desiredwindow = "";
	if (option = XGetDefault (XXdisplay, progname, "Geometry"))
		desiredwindow = option;
	if (!temp_font)
		temp_font = "fixed";
	XXcurrentfont = (char *) xmalloc (strlen (temp_font) + 1);
	strcpy (XXcurrentfont, temp_font);

	if (XXisColor) {
		if (fore_color &&
		    XParseColor (XXdisplay, XXColorMap, fore_color, &cdef) &&
		    XAllocColor (XXdisplay, XXColorMap, &cdef))
			fore = cdef.pixel;
		else {
			fore_color = "black";
			fore = BlackPixel(XXdisplay,0);
		}

		if (back_color &&
		    XParseColor (XXdisplay, XXColorMap, back_color, &cdef) &&
		    XAllocColor (XXdisplay, XXColorMap, &cdef))
			back = cdef.pixel;
		else {
			back_color = "white";
			back = WhitePixel(XXdisplay,0);
		}

		if (curs_color &&
		    XParseColor (XXdisplay, XXColorMap, curs_color, &cdef) &&
		    XAllocColor (XXdisplay, XXColorMap, &cdef))
			curs = cdef.pixel;
		else {
			curs_color = "black";
			curs = BlackPixel(XXdisplay,0);
		}

		if (mous_color &&
		    XParseColor (XXdisplay, XXColorMap, mous_color, &cdef) &&
		    XAllocColor (XXdisplay, XXColorMap, &cdef))
			mous = cdef.pixel;
		else {
			mous_color = "black";
			mous = BlackPixel(XXdisplay,0);
		}

		if (brdr_color &&
		    XParseColor (XXdisplay, XXColorMap, brdr_color, &cdef) &&
		    XAllocColor (XXdisplay, XXColorMap, &cdef))
			brdr = cdef.pixel;
		else {
			brdr_color = "black";
			brdr = BlackPixel(XXdisplay,0);
		}
	}
	else {
		fore_color  = curs_color = mous_color = brdr_color = "black";
		fore = curs = mous = brdr = BlackPixel(XXdisplay,0);
		back_color = "white";
		back = WhitePixel(XXdisplay,0);
	}

	XXpid = getpid ();
	default_window = "=80x24+0+0";

	/* Process X command line args...*/
	xxargc = xargc;
	xxargv = xargv;
	xxargc++;
	xxargc--;
	while (xxargc) {
		int sargc;
		sargc = xxargc;
		if (xxargc && !strcmp (*xxargv, "-r")) {
			reversevideo = !reversevideo;
			xxargc--;
			xxargv++;
		}
		if ((xxargc > 1) && !strcmp (*xxargv, "-font")) {
			xxargc--;
			xxargv++;
			free(XXcurrentfont);
			XXcurrentfont = (char *) xmalloc (strlen (*xxargv)+1);
			strcpy (XXcurrentfont, *xxargv);
			xxargc--;
			xxargv++;
		}
		if ((xxargc > 1) && !strcmp (*xxargv, "-b")) {
			xxargc--;
			xxargv++;
			XXborder = atoi (*xxargv);
			xxargc--;
			xxargv++;
		}
		if ((xxargc > 1) && !strcmp (*xxargv, "-ib")) {
			xxargc--;
			xxargv++;
			XXInternalBorder = atoi (*xxargv);
			xxargc--;
			xxargv++;
		}
		if ((xxargc > 1) && !strcmp (*xxargv, "-w")) {
			xxargc--;
			xxargv++;
			desiredwindow = (char *) xmalloc (strlen (*xxargv)+1);
			strcpy (desiredwindow, *xxargv);
			xxargc--;
			xxargv++;
		}
		if (XXisColor) {
			if ((xxargc > 1 && !strcmp (*xxargv, "-fg"))) {
				xxargc--;
				xxargv++;
				if (XParseColor (XXdisplay, XXColorMap,
						 *xxargv, &cdef) &&
				    XAllocColor (XXdisplay, XXColorMap,
						 &cdef)) {
					fore_color = (char *)
						xmalloc (sizeof (*xxargv)+1);
					strcpy (fore_color, *xxargv);
					fore = cdef.pixel;
				}
				xxargc--;
				xxargv++;
			}
			if ((xxargc > 1 && !strcmp (*xxargv, "-bg"))) {
				xxargc--;
				xxargv++;
				if (XParseColor (XXdisplay, XXColorMap,
						 *xxargv, &cdef) &&
				    XAllocColor (XXdisplay, XXColorMap,
						 &cdef)) {
					back_color = (char *)
						xmalloc (sizeof (*xxargv)+1);
					strcpy (back_color, *xxargv);
					back = cdef.pixel;
				}
				xxargc--;
				xxargv++;
			}
			if ((xxargc > 1 && !strcmp (*xxargv, "-bd"))) {
				xxargc--;
				xxargv++;
				if (XParseColor (XXdisplay, XXColorMap,
						 *xxargv, &cdef) &&
				    XAllocColor (XXdisplay, XXColorMap,
						 &cdef)) {
					brdr_color = (char *)
						xmalloc (sizeof (*xxargv)+1);
					strcpy (brdr_color, *xxargv);
					brdr = cdef.pixel;
				}
				xxargc--;
				xxargv++;
			}
			if ((xxargc > 1 && !strcmp (*xxargv, "-cr"))) {
				xxargc--;
				xxargv++;
				if (XParseColor (XXdisplay, XXColorMap,
						 *xxargv, &cdef) &&
				    XAllocColor (XXdisplay, XXColorMap,
						 &cdef)) {
					curs_color = (char *)
						xmalloc (sizeof (*xxargv)+1);
					strcpy (curs_color, *xxargv);
					curs = cdef.pixel;
				}
				xxargc--;
				xxargv++;
			}
			if ((xxargc > 1 && !strcmp (*xxargv, "-ms"))) {
				xxargc--;
				xxargv++;
				if (XParseColor (XXdisplay, XXColorMap,
						 *xxargv, &cdef) &&
				    XAllocColor (XXdisplay, XXColorMap,
						 &cdef)) {
					mous_color = (char *)
						xmalloc (sizeof (*xxargv)+1);
					strcpy (mous_color, *xxargv);
					mous = cdef.pixel;
				}
				xxargc--;
				xxargv++;
			}
		}
		if (sargc == xxargc) {
			xxargc--;
			xxargv++;
		}
	}

	if (reversevideo) {
		int tempcolor;
		char *tempname;
		brdr = back;
		brdr_color = back_color;
		tempcolor = fore;
		fore = back;
		back = tempcolor;
		tempname = fore_color;
		fore_color = back_color;
		back_color = tempname;
		if (curs == WhitePixel(XXdisplay,0)) {
			curs = BlackPixel(XXdisplay,0);
			curs_color = "black";
		}
		else if (curs == BlackPixel(XXdisplay,0)) {
			curs = WhitePixel(XXdisplay,0);
			curs_color = "white";
		}
		if (mous == WhitePixel(XXdisplay,0)) {
			mous = BlackPixel(XXdisplay,0);
			mous_color = "black";
		}
		else if (mous == BlackPixel(XXdisplay,0)) {
			mous = WhitePixel(XXdisplay,0);
			mous_color = "white";
		}
	}

	if (!XXcurrentfont) {
		fprintf (stderr, "Memory allocation failure.\n");
		exit (-150);
	}

	signal (SIGPIPE, XExitGracefully);
	fontinfo = XLoadQueryFont(XXdisplay,XXcurrentfont);
	if (fontinfo == (XFontStruct *) 0) {
		fprintf (stderr, "No font\n");
		exit (-98);
	}
	
	XXfid = fontinfo->fid;
	XXfonth = fontinfo->ascent + fontinfo->descent;
	XXfontw = fontinfo->max_bounds.width;
	XXbase = fontinfo->ascent;
	pixelwidth = screen_width * XXfontw + 2 * XXInternalBorder;
	pixelheight = screen_height * XXfonth + 2 * XXInternalBorder;
	flexlines = screen_height;
#ifndef CANNOT_DUMP
	if (initialized)
#endif				/* CANNOT_DUMP */
		Vxterm = Qt, Vxterm1 = Qt;

	XInitWindow ();

	cursor_fore.pixel = fore;
	cursor_fore.flags = 0;
	cursor_back.pixel = back;
	cursor_back.flags = 0;
	
	XSelectInput (XXdisplay, XXwindow, KeyPressMask | 
		      ExposureMask | ButtonPressMask | ButtonReleaseMask |
		      StructureNotifyMask);

	Fset_input_mode (Qt, Qnil);
	request_sigio();

#ifdef BIGDEBUG
	*debug_string = '\0';
	if (1) {
		int dump_log();
		signal(SIGUSR1,dump_log);
	} 
#endif BIGDEBUG	
}

x_init_1 (unrequest)
	int unrequest;
{
#ifdef F_SETOWN
	extern int old_fcntl_owner;
#endif
	extern void init_sigio (), request_sigio (), unrequest_sigio ();

	dup2 (ConnectionNumber(XXdisplay), 0);
	close (ConnectionNumber(XXdisplay));
	ConnectionNumber(XXdisplay) = 0;	/* Looks a little strange?
						 * check the def of the macro;
						 * it is a genuine lvalue */
	init_sigio ();
	request_sigio ();
	setpgrp (0,getpid());
	
#ifdef F_SETOWN
	old_fcntl_owner = fcntl (0, F_GETOWN, 0);
	fcntl (0, F_SETOWN, getpid ());
#endif
	if (unrequest)
		unrequest_sigio ();
}

XSetFlash ()
{
	ring_bell_hook = XTflash;
}

XSetFeep ()
{
	ring_bell_hook = XTfeep;
}

XNewFont (newname)
     register char *newname;
{
	XFontStruct *temp;
	XSizeHints sizes;

	int mask = sigblock (sigmask (SIGIO));

	XFlush (XXdisplay);

	temp = XLoadQueryFont(XXdisplay, newname);
	if (temp == (XFontStruct *) 0) {
		sigsetmask (mask);
		if (QLength (XXdisplay) > 0)
			read_events_block ();
		return -1;
	}
	XXfid = temp->fid;
	XXfonth = temp->ascent + temp->descent;
	XXfontw = temp->max_bounds.width;
	XXbase = temp->ascent;
	XSetFont (XXdisplay, XXgc_norm, XXfid);
	XSetFont (XXdisplay, XXgc_rev, XXfid);
	XSetFont (XXdisplay, XXgc_curs, XXfid);
	XFreeFont (XXdisplay, fontinfo);
	fontinfo = temp;
	sizes.flags = PResizeInc|PMinSize;
	sizes.width_inc = XXfontw;
	sizes.height_inc = XXfonth;
	sizes.min_width = XXfontw*MINWIDTH+2*XXInternalBorder;
	sizes.min_height = XXfonth*MINHEIGHT+2*XXInternalBorder;
	XSetNormalHints (XXdisplay, XXwindow, &sizes);

	XSetWindowSize (screen_height, screen_width);

	sigsetmask (mask);
	if (QLength (XXdisplay) > 0)
		read_events_block ();
	
	return 0;
}

/* Flip foreground/background colors */

XFlipColor ()
{
	int tempcolor;
	char *tempname;
	int mask = sigblock (sigmask (SIGIO));

	CursorToggle ();
	XSetWindowBackground(XXdisplay, XXwindow, fore);
	if (XXborder)
		XSetWindowBorder(XXdisplay, XXwindow, back);
	brdr = back;
	brdr_color = back_color;
	tempcolor = fore;
	fore = back;
	back = tempcolor;
	tempname = fore_color ;
	fore_color = back_color;
	back_color = tempname;
	XClearArea (XXdisplay, XXwindow, 0, 0,
		    screen_width*XXfontw+2*XXInternalBorder,
		    screen_height*XXfonth+2*XXInternalBorder, 0);
	XXgc_temp = XXgc_norm;
	XXgc_norm = XXgc_rev;
	XXgc_rev = XXgc_temp;

	XRedrawDisplay ();
	if (curs == WhitePixel(XXdisplay,0)) {
		curs = BlackPixel(XXdisplay,0);
		curs_color = "black";
	}
	else
		if (curs == BlackPixel(XXdisplay,0)) {
			curs = WhitePixel(XXdisplay,0);
			curs_color = "white";
		}
	XSetState (XXdisplay, XXgc_curs, back, curs, GXinvert, AllPlanes);

	if (mous == WhitePixel(XXdisplay,0)) {
		mous = BlackPixel(XXdisplay,0);
		mous_color = "black";
	}
	else
		if (mous == BlackPixel(XXdisplay,0)) {
			mous = WhitePixel(XXdisplay,0);
			mous_color = "white";
		}
	CursorToggle ();
	XFlush (XXdisplay);
	sigsetmask (mask);
}

/* Change just the size of the window */

XSetWindowSize (rows, cols)
     register int rows, cols;
{
	pixelwidth = cols*XXfontw+2*XXInternalBorder;
	pixelheight = rows*XXfonth+2*XXInternalBorder;

	XResizeWindow(XXdisplay, XXwindow, pixelwidth, pixelheight);
	XFlush (XXdisplay);

	change_screen_size (rows, cols);
}

XInitWindow ()
{
	int x, y, width, height;
	XSizeHints sizes;
	XWMHints wmhints;
	char iconname[50];
	extern int xargc;
	extern char **xargv;
	
	strcpy(iconname,"emacs@");
	gethostname(iconname+strlen(iconname),30);
	
	XGeometry (XXdisplay, 0, (desiredwindow && *desiredwindow) ?
		   desiredwindow : default_window,
		   default_window, XXborder, XXfontw, XXfonth,
		   XXInternalBorder*2, XXInternalBorder*2,
		   &x, &y, &width, &height);

	XXwindow = XCreateSimpleWindow(XXdisplay, RootWindow(XXdisplay,0),
				  x, y, width*XXfontw+2*XXInternalBorder,
				  height*XXfonth+2*XXInternalBorder,
				  XXborder, brdr, back);

	if (!XXwindow) {
		fprintf (stderr, "Could not create window");
		fflush (stderr);
		exit (-97);
	}

	XXgcv.font = XXfid;
	XXgcv.foreground = fore;
	XXgcv.background = back; 
	XXgc_norm = XCreateGC(XXdisplay, XXwindow,
			      GCFont|GCForeground|GCBackground,
			      &XXgcv);
	XXgcv.foreground = back;
	XXgcv.background = fore;
	XXgc_rev = XCreateGC(XXdisplay, XXwindow,
			     GCFont|GCForeground|GCBackground,
			     &XXgcv);
	XXgcv.foreground = back;
	XXgcv.background = curs;
	XXgc_curs = XCreateGC(XXdisplay, XXwindow,
			      GCFont|GCForeground|GCBackground,
			      &XXgcv);

	XSelectInput (XXdisplay, XXwindow, NoEventMask);

	EmacsCursor = XCreateFontCursor(XXdisplay, XC_left_ptr);
	XDefineCursor (XXdisplay, XXwindow, EmacsCursor);

	SinkPixmap = XCreateBitmapFromData (XXdisplay, XXwindow,
					    sink_bits, sink_width,
					    sink_height);
	SinkMaskPixmap = XCreateBitmapFromData (XXdisplay, XXwindow,
						sink_mask_bits,
						sink_mask_width,
						sink_mask_height);
	wmhints.flags = IconPixmapHint|IconMaskHint;
	wmhints.icon_pixmap = SinkPixmap;
	wmhints.icon_mask = SinkMaskPixmap;
	XSetWMHints (XXdisplay, XXwindow, &wmhints);

	sizes.flags = PResizeInc|PMinSize;
	sizes.width_inc = XXfontw;
	sizes.height_inc = XXfonth;
	sizes.min_width = XXfontw*MINWIDTH+2*XXInternalBorder;
	sizes.min_height = XXfonth*MINHEIGHT+2*XXInternalBorder;

	if (!desiredwindow || !*desiredwindow)
		sizes.flags |= PPosition|PSize;
	else
		sizes.flags |= USPosition|USSize;
	sizes.x = x;
	sizes.y = y;
	sizes.width = width*XXfontw+2*XXInternalBorder;
	sizes.height = height*XXfonth+2*XXInternalBorder;

	change_screen_size(height,width);
	
	XXxoffset = x;
	XXyoffset = y;
	pixelwidth = sizes.width;
	pixelheight = sizes.height;
	
	XSetStandardProperties (XXdisplay, XXwindow, progname, iconname,
				SinkPixmap, xargv, xargc, &sizes);

	CursorExists = 0;
	VisibleX = 0;
	VisibleY = 0;
	XMapWindow (XXdisplay, XXwindow);
	XFlush (XXdisplay);
	WindowMapped = 1;
}

#ifdef BIGDEBUG

my_log(s,a,b,c,d,e,f,g,h)
	char *s;
{
	char bfr[256];
	
	sprintf(bfr,s,a,b,c,d,e,f,g,h);

	strcat(debug_string,bfr);
}

dump_log()
{
	fputs(debug_string,stderr);
	fflush(stderr);
	*debug_string = '\0';
}

#endif BIGDEBUG
	
#endif /* HAVE_X_WINDOWS */

/*#include "xundebug.h"*/
