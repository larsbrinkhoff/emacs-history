/* X Communication module for terminals which understand the X protocol */

/* Copyright (c) 1985  Joachim Martillo */
/* Anyone who wishes to use this module in order to enhance Gnu Emacs */
/* or any program is free to do so.  On 750's Gnu emacs is observed   */
/* to run twice as fast when talking directly to the X server rather  */
/* going through the terminal emulator.  If you can modify this code  */
/* to do better than that I would be interested in the modifications. */

#include "config.h"
#include "dispextern.h"

#ifdef XVSWINDOW  /* Entire file is contained in this conditional */

sorry, you lose
/* This code was ok for version 15, but needs changes to work in version 16,
and I did not get them by the distribution deadline.  -- rms */

#include <sys/time.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <ctype.h>
#include <vs/codes.h>
#include <errno.h>
#include <signal.h>
#include <vs/Xenv.h>
#include <vs/Xlib.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>

#define min(a,b) ((a)<(b) ? (a) : (b))

extern struct display_line *DesiredScreen[], *PhysScreen[];


static int flexlines;		/* last line affect by dellines or */
				/* inslines functions */
extern int VisibleBell;		/* Since to ring bell requires calling */
				/* XFeep, I use VisibleBell to ring the */
				/* bell */
extern int errno;
static int VisibleX, VisibleY;	/* genuine location of cursor on screen */
				/* if it is there */
static int SavedX, SavedY;	/* Where the cursor was before update */
				/* started */

static int bitblt;		/* Used to track bit blt events */
static int CursorExists;	/* during updates cursor is turned off */
static int InUpdate;		/* many of functions here may be invoked */
				/* even if no update in progress, when */
				/* no update is in progress the action */
				/* can be slightly different */

static char MouseCursor[33] ="\000\000\002\000\006\000\016\000\036\000\076\000\
\176\000\376\000\376\001\076\000\066\000\142\000\140\000\300\000\300\000\000\
\000";

static char MouseMask[33] = "\003\000\007\000\017\000\037\000\077\000\177\000\
\377\000\377\001\377\003\377\003\177\000\367\000\363\000\340\001\340\001\
\300\000";

static DISPLAY XXdisplay;	/* stores data about the connection to the */
				/* X server */
static FONT *normalfont, *boldfont;
static int fontwidth, fontheight;
static FontInfo fontinfo;
static WindowInfo windowinfo;
static Window *XXwindow;	/* stores info about the current window */
static RASTER *RasterMouseCursor;
static RASTER *RasterMouseMask;	/* masks are used to make sure the cursor */
				/* stands out against any background */

static XRep XXreply;		/* as X messages are read in they are */
				/* stored here */
static int CurHL;		/* Current Highlighting actually being */
				/* being used for bold font right now*/
extern int InverseVideo;
static int XXborder;
static int XXwidth;
static int XXheight;
static int Vsnum;
static char hostname[128];
static char *namenormalfont = "vtsingle";
static char *nameboldfont = "vtbold";

/* HLmode -- Changes the GX function for output strings.  Could be used to
 * change font.  Check an XText library function call. 
 */

static
HLmode(new)
{
	CurHL = new;
}

static int pixelwidth;
static int pixelheight;

/* Used for starting or restarting (after suspension) the X window.  Puts the
 * cursor in a known place, update does not begin with this routine but only
 * with a call to DoDsp.  The mouse cursor is warped into the window and then
 * the cursor is turned on.
 */



static init()
{

	CursorExists = 0;
	InUpdate = 0;
	VisibleX = 1;
	VisibleY = 1;
	XWarpMouse(XXwindow, pixelwidth >> 1, pixelheight >> 1);
	CursorToggle();
}

/* topos moves the cursor to the correct location and checks whether an update
 * is in progress in order to toggle it on.
 */

static
topos(col, row)
register int row, col;
{
	cursX = row;
	cursY = col;
	if(InUpdate)
	{
		if(CursorExists) 
		{
			CursorToggle();
			XFlush(&XXdisplay);/* Cursor does not blank out */
					   /* until the XFlush is */
					   /* transmitted */
		}
		return;		/* Generally, topos will be invoked */
				/* when InUpdate with !CursorExists */
				/* so that wasteful XFlush is not called */
	}
	if((col == VisibleY) && (row == VisibleX))
	{
		if(!CursorExists)
		{
			CursorToggle();
		}
	}
	else if(CursorExists)
	{
		CursorToggle();
		VisibleX = row;
		VisibleY = col;
		CursorToggle();
	}
	else
	{
		VisibleX = row;
		VisibleY = col;
		CursorToggle();
	}
	XFlush(&XXdisplay);
}

/* Used to get the terminal back to a known state after resets.  Usually
 * used when restarting suspended or waiting emacs
 */

static
cleanup()
{
	InverseVideo = 0;
	HLmode(0);
}

/* wipes out numcols columns starting a current column on the current line */

static
wipeline(hlmod, numcols)
register int hlmod;
register int numcols;
{
	if((cursX < 1) || (cursX > tt.t_width) ||
	   (cursY < 1) || (cursY > tt.t_length))
	{
		return;
	}
	if(numcols) {
		XRasterFill(XXwindow, 
			    GXset, /* GXset is white */
			    ((cursX - 1) * fontinfo.width), 
			    ((cursY - 1) * fontinfo.height), 
			    (fontinfo.width * numcols), 
			    fontinfo.height);
 	}
	if((cursY == VisibleY) && (VisibleX >= cursX) &&
	   (VisibleX < (cursX + numcols)))
	{
		CursorExists = 0;
	}
	topos(cursY, cursX);
}

static
reset()
{
	wipescreen();
}

static
wipescreen()
{
	HLmode(0);
	XClear(XXwindow);
	CursorExists = 0;
	if(!InUpdate)
	{
		CursorToggle();
	}
	XFlush(&XXdisplay);
}

/* used by dumprectangle which is usually invoked upon ExposeRegion
 * events which come from bit blt's or moving an obscuring opaque window
 */

static
dumpchars(start, end)
register char *start, *end;
{
	if(end < start) {
		return;
	}
	if((cursX < 1) || (cursX > tt.t_width) ||
	   (cursY < 1) || (cursY > tt.t_length)) {
		return;
	}
	if(CursorExists)
	{
		CursorToggle();
	}
	if(InUpdate){
		XText(XXwindow,
		      (CurHL ? GXcopy : GXcopyInverted),
		      ((cursX - 1) * fontinfo.width),
		      ((cursY - 1) * fontinfo.height),
		      start,
		      ((end - start) + 1),
		      (InverseVideo ? boldfont : normalfont));
		topos(cursY, cursX + (end - start) + 1);
	}
	else			/* Currently this code is not used */
				/* because upon deciding to expose a */
				/* a region, InUpdate is set to 1 */
				/* This code is for later flexibility */
	{
		XText(XXwindow,
		      (CurHL ? GXcopy : GXcopyInverted),
		      ((VisibleX - 1) * fontinfo.width),
		      ((VisibleY - 1) * fontinfo.height),
		      start,
		      ((end - start) + 1),
		      (InverseVideo ? boldfont : normalfont));
		VisibleX = VisibleX + (end - start) + 1;
		CursorToggle();
	}
}

/* When a line has been changed this function is called.  X is so fast
 * that the actual sequence is ignore.  Rather, the new version of the
 * line is simply output if this function is invoked while in UpDate.
 * Sometimes writechars can be invoked when not in update if text is to
 * be output at the end of the line.  In this case the whole line is not
 * output.  Simply the new text at the current cursor position given
 * by VisibleX,Y.  The cursor is moved to the end of the new text.
 */
static
writechars(start, end)
register char *start, *end;
{
	register int temp_length;
	if(end < start) {
		return;
	}
	if((cursX < 1) || (cursX > tt.t_width) ||
	   (cursY < 1) || (cursY > tt.t_length)) {
		return;
	}
	if(CursorExists)
	{
		CursorToggle();
	}
	if(InUpdate){
		temp_length = DesiredScreen[cursY]->length;
		if(temp_length > 0) {
			XText(XXwindow,
			      (CurHL ? GXcopy : GXcopyInverted),
			      0,
			      ((cursY - 1) * fontinfo.height),
			      &DesiredScreen[cursY]->body[0],
			      temp_length,
			      (InverseVideo ? boldfont : normalfont));
			topos(cursY, ++temp_length);
			if(temp_length < tt.t_width) {
				wipeline(CurHL,
					 tt.t_width - temp_length);
			}

		}
		else XRasterFill(XXwindow, GXset, 0,
				 ((cursY - 1) * fontinfo.height), 
				 (fontinfo.width * tt.t_width), 
				 fontinfo.height);
		topos(cursY, cursX + (end - start) + 1);
	}
	else {
		if((VisibleX < 1) || (VisibleX > tt.t_width)) {
			return;
		}
		if((VisibleY < 1) || (VisibleY > tt.t_length)) {
			return;
		}
		XText(XXwindow,
		      (CurHL ? GXcopy : GXcopyInverted),
		      ((VisibleX - 1) * fontinfo.width),
		      ((VisibleY - 1) * fontinfo.height),
		      start,
		      ((end - start) + 1),
		      (InverseVideo ? boldfont : normalfont));
		VisibleX = VisibleX + (end - start) + 1;
		CursorToggle();
	}
}

/* The following routine is for the deaf or for the pervert who prefers 
 * that his terminal flashes at him rather than beep at him.
 */

/*
static
flash()
{
	XRasterFill(XXwindow, GXinvert, 0, 0, tt.t_width * fontinfo.width,
		    tt.t_length * fontinfo.height);
	XFlush(&XXdisplay);
	XRasterFill(XXwindow, GXinvert, 0, 0, tt.t_width * fontinfo.width,
		    tt.t_length * fontinfo.height);
	XFlush(&XXdisplay);
}
*/

/* A kludge to get a bell */

static flash()
{
	XFeep(XXwindow, 2);
}

/* Artificially creating a cursor is hard, the actual position on the
 * screen (either where it is or last was) is tracked with VisibleX,Y.
 * Gnu Emacs code tends to assume a cursor exists in hardward at cursX,Y
 * and that output text will appear there.  During updates, the cursor is
 * supposed to be blinked out and will only reappear after the update 
 * finishes.
 */

static
CursorToggle()
{
	if((VisibleX < 1) || (VisibleX > tt.t_width) ||
	   (VisibleY < 1) || (VisibleY > tt.t_length))
	{			/* Current Cursor position trash */
				/* Not much can be done */
		XFlush(&XXdisplay);
		CursorExists = 0;
		return(0);	/* Currently the return values are not */
				/* used, but I could anticipate using */
				/* them in the future. */
	}
	XRasterFill(XXwindow, GXinvert,
		    (VisibleX - 1) * fontinfo.width,
		    (VisibleY - 1) * fontinfo.height,
		    fontinfo.width, fontinfo.height);
	CursorExists = !CursorExists;/* Cursor has either been blinked in */
				     /* or out */
	if(!InUpdate) XFlush(&XXdisplay);/* If InUpdate a timely XFlush */
					 /* will be performed when the */
					 /* the UpDate finishes */
	return(1);
}

static
UpdateBegin()
{	
	InUpdate = 1;
	SavedX = cursX;		/* The initial"hardware" cursor position is */
				/*  saved because that is where gnu emacs */
				/*  expects the cursor to be at the end of*/
				/* the update */
	SavedY = cursY;
	if(CursorExists) {
		CursorToggle();
	}
}


static
UpdateEnd()
{	
	InUpdate = 0;		/* InUpdate should probably come after */
				/* the next instruction */
	if(CursorExists) {
		CursorToggle();
	}
	topos(SavedY, SavedX);	/* topos invokes cursor toggle */
}

/* Used for expose region and expose copy events.  Have to get the text
 * back into the newly blank areas.
 */

static
dumprectangle(top, left, rows, cols)
register int top, left, rows, cols;
{
	int StoredHL;
	int StoredX;
	int StoredY;
	int UpdateStatus;
	register struct display_line **ActiveScreen;
	register int index;

	if(rows < 0) return;
	if(cols < 0) return;
	if(top > (tt.t_length - 1)) return;
	if(left > (tt.t_width - 1)) return;
	rows += 2;		/* Maybe +1? */
	cols += 2;

	if(CursorExists) CursorToggle();/* This is a bit sloppy, really */
					/* only have to worry about the */
					/* cursor if it was supposed to */
					/* be in the just exposed area */

	StoredHL = CurHL;
	StoredX = cursX;
	StoredY = cursY;
	UpdateStatus = InUpdate;
	InUpdate = 1;
	ActiveScreen = PhysScreen;
/*	XRasterFill(XXwindow, GXset, left * fontinfo.width,
		    top * fontinfo.height, 
		    cols * fontinfo.width,
		    rows * fontinfo.height);
 */
	for(cursY = top + 1, index = 0;
	    (index < rows) && (cursY <= tt.t_length);
	    ++index, ++cursY)
	{
		if((left + 1) > ActiveScreen[cursY]->length) continue;
		cursX = left + 1;
		CurHL = ActiveScreen[cursY]->highlighted;/* Recreating */
							 /* the missing text */
		dumpchars(&ActiveScreen[cursY]->body[left],
			   &ActiveScreen[cursY]->body[left]
			   + min(cols - 1,
				 ActiveScreen[cursY]->length - cursX));
	}
	cursX = StoredX;
	cursY = StoredY;
	CurHL = StoredHL;
	InUpdate = UpdateStatus;
	if(!InUpdate) {
		CursorToggle();
	}
	else
	{
		XFlush(&XXdisplay);
	}
}

/* What sections of the window will be modified from the UpdateDisplay
 * routine is totally under software control.  Any line with Y coordinate
 * greater than flexlines will not change during an update.  This is really
 * used only during dellines and inslines routines (scraplines and stufflines)
 */
static
setflexlines(n)
register int n;
{
	if((n <= 0) || (n > tt.t_length)) flexlines = tt.t_length;
	else flexlines = n;
}

static
stufflines(n)
register int n;
{
	register int topregion, bottomregion;
	register int length, newtop;

	if(cursY > flexlines) return;
	if(CursorExists) CursorToggle();
	topregion = cursY;
	bottomregion = flexlines - n;
	newtop = cursY + n;
	length = (bottomregion - topregion) + 1;
	if((length > 0) && (newtop <= flexlines)) {
		XRasterCopy(XXwindow, GXcopy, 0,
			    (topregion - 1) * fontinfo.height,
			    0, (newtop - 1) * fontinfo.height,
			    tt.t_width * fontinfo.width,
			    length * fontinfo.height);
		while(bitblt > 0) {
			;	/* pausing for bitblt sync see */
				/* readsocket*/
		}
		++bitblt;
		XFlush(&XXdisplay);
		while(bitblt > 0) {
			;	/* pausing for bitblt sync see */
				/* readsocket*/
		}
	}
	newtop = min(newtop, flexlines);
	length = newtop - topregion;
	if(length > 0) {
		XRasterFill(XXwindow, GXset, 0,
			   (topregion - 1) * fontinfo.height,
			    tt.t_width * fontinfo.width,
			    n * fontinfo.height);
	}
	if(!InUpdate) CursorToggle();
}

static
scraplines(n)
register int n;
{
	if(cursY > flexlines) return;
	if(CursorExists) CursorToggle();
	if((cursY + n) > flexlines) {
		if(flexlines > cursY) {
			XRasterFill(XXwindow, GXset, 0,
				    (cursY - 1) * fontinfo.height,
				    tt.t_width * fontinfo.width,
				    (flexlines - cursY) * fontinfo.height);

		}
	}
	else {

		XRasterCopy(XXwindow, GXcopy, 0,
			    (cursY + n - 1) * fontinfo.height,
			    0, (cursY - 1) * fontinfo.height,
			    tt.t_width * fontinfo.width,
			    (flexlines + 1 - (cursY + n)) * fontinfo.height);
		while(bitblt > 0) {
			;	/* pausing for bitblt sync see */
				/* readsocket */
		}
		++bitblt;
		XFlush(&XXdisplay);
		while(bitblt > 0) {
			;	/* pausing for bitblt sync see */
				/* readsocket */
		}
		XRasterFill(XXwindow, GXset, 0,
			    (flexlines - n) * fontinfo.height,
			    tt.t_width * fontinfo.width,
			    n * fontinfo.height);
	}
	if(!InUpdate) CursorToggle();
}
	
/* Substitutes for standard read routine.  Under X not interested in individual
 * bytes but rather individual packets.
 */

static
readsocket(sd, bufp, numchars)
register int sd;
register char *bufp;
register int numchars;
{
	register int PendingExposure;
	int count;
	int stuffpending;
	int i;
	register int StoredX;
	register int StoredY;
	register int StoredHL;
	int temp_width, temp_height;

	count = 0;
	PendingExposure = 0;
	do {
		stuffpending = 0;
		ioctl(sd, FIONREAD, &stuffpending);/* Checking for complete */
						   /* packet*/
		if(stuffpending <= 0) {
			if(count < 0) count = 0;
			return(count);
		}
		stuffpending /= sizeof(XRep);
	}
	while(!stuffpending);	/* You will get stuck if for some reason */
				/* full packet never arrives, I should */
				/* probably put in an alarm here */
	if(numchars <= 0) {	/* To keep from overflowing read buffer */
		numchars = 1;
		--bufp;
	}
	while(min(numchars, stuffpending) > 0) {
		if(read(sd, &XXreply, sizeof(XRep)) != sizeof(XRep)) {
			fprintf(stderr, "Wrong Size Read\n");
			_XError(&XXdisplay);
		}
		--stuffpending;
		switch(XXreply.code) {
		case X_Reply:
			break;
		case X_Error:
			break;
		default:
			break;
		case X_Event:
			switch(XXreply.param.s[2]) {
			case ExposeWindow:
				PendingExposure = 1;/* No reason to repeat */
						    /* this if several */
						    /* ExposeWindow events */
						    /* come in quick succes- */
						    /* ion */
				break;
			case ExposeRegion:
				if(PendingExposure) {/* Don't bother with */
						     /* region events when */
						     /* full window event */
						     /* is pending */
					break;
				}
				dumprectangle(XXreply.param.s[8]
					      / fontinfo.height,
					      XXreply.param.s[9]
					      / fontinfo.width,
					      (XXreply.param.s[5]
					       / fontinfo.height),
					      (XXreply.param.s[4]
					       / fontinfo.width));
				break;
			case ExposeCopy:/* For ExposeCopy sync */
					/* will block all outgoing */
					/* requests until this is */
					/* decremented */
				--bitblt;
				break;
			default:
				break;
			case KeyPressed:
			case KeyReleased:
				if(Input(XXreply.param.s[3], bufp))
				{
					++bufp;
					++count;
					--numchars;
				}
				break;
			}
		}
	}
	if(PendingExposure) {
		XQueryWindow(XXwindow, &windowinfo);
		temp_width = (windowinfo.width / fontinfo.width);
		temp_height = (windowinfo.height / fontinfo.height);
		if((temp_width != tt.t_width) || (temp_height != tt.t_length))
		{
			ChangeScreenSize(temp_height, temp_width);
		}
		else dumprectangle(0, 0, tt.t_length, tt.t_width);
	}
	if(count < 0) count = 0; 
	return(count);
}

/* Interpreting incoming keycodes. Should have table modifiable as needed
 * from elisp.
 */

static
Input(keycode, buffer)
register int keycode;
register char *buffer;
{
	register short c;
	register int offset;
	register int size;
	extern KeyMapEntry StdMap[];

	offset = XKeyState (keycode);	/* set SHIFT, CONTROL, META */

	c = StdMap [keycode & XValueMask] [offset];
	if( (keycode & XShiftLockMask) && (c >= 'a') && (c <= 'z') )
	{
		c += 'A' - 'a';
	}

	keycode &= XValueMask;	/* no longer need shift bits for anything */

	if (! (c & ~377))
	{
		*buffer = c;
		return(1);
	}
	switch (c)
	{
	case KEYPAD:	
	case CURSOR:
	case PFX:
	case (short) -1:
	case SHFT:
	case CNTL:
	case SYMBOL:
	case LOCK:
	case FUNC1:
	case FUNC2:
	case FUNC3:
	case FUNC4:
	case FUNC5:
	case FUNC6:
	case FUNC7:
	case FUNC8:
	case FUNC9:
	case FUNC10:
	case FUNC11:
	case FUNC12:
	case FUNC13:
	case FUNC14:
	case FUNC15:
	case FUNC16:
	case FUNC17:
	case FUNC18:
	case FUNC19:
	case FUNC20:
	case E1:
	case E2:
	case E3:
	case E4:
	case E5:
	case E6:
		return(0);	
	default:
		*buffer = c;
		return(1);
	}
}


TrmXTERM()
{
	register char *vardisplay;
	register char *colonpointer;
	extern char *getenv();
	register int scratchindex;


	XXborder = 1;
	vardisplay = getenv("DISPLAY");
	if(!vardisplay)
	{
		fprintf(stderr, "DISPLAY environment variable must be set\n");
		exit(-200);
	}
	else 
	{
		colonpointer = rindex(vardisplay, ':');
		if(!colonpointer)
		{
			fprintf(stderr,
				"DISPLAY environment variable has been\
trashed\n");
			exit(-9);
		}
		scratchindex = colonpointer - vardisplay;
		if(scratchindex > 126) {
			fprintf(stderr, "Hostname is too large\nCheck \
DISPLAY environment variable\n");
			exit(-10);
		}
		(void) strncpy(hostname, vardisplay, scratchindex);
		hostname[scratchindex] = '\0';
		Vsnum = atoi(++colonpointer);
	}

	if(XOpenDisplay(&XXdisplay, hostname, Vsnum) == -1)
	{
		fprintf(stderr, "No X. Try = %d\n", scratchindex);
		exit(-99);	
	}
	dup2(XXdisplay.fd, 0);
	close(XXdisplay.fd);
	XXdisplay.fd = 0;
	XGetFont(&XXdisplay, namenormalfont, &normalfont);
	XGetFont(&XXdisplay, nameboldfont, &boldfont);/* Currently not using */
						      /* bold font perhaps */
						      /* later for under- */
						      /* lining */
	XXwidth = 80;
	XXheight = 66;
	MetaFlag = 1;
	VisibleBell = 1;
	tt.t_length = XXheight;
	tt.t_width = XXwidth;
	flexlines = tt.t_length - 2;
	bitblt = 0;
	XQueryFont(normalfont, &fontinfo);
	pixelwidth = (XXwidth * fontinfo.width) + (XXborder * 2);
	pixelheight = (XXheight * fontinfo.height) + (XXborder * 2);
	XOpenWindow(0 /* Absolute horizontal offset */,
		    0 /* Absolute Vertical offset */, pixelwidth, pixelheight,
		    2 * XXborder, XXdisplay.black, XXdisplay.white,
		    XXdisplay.root, &XXwindow);
	XStoreName(XXwindow, "emacs");
	XSetResizeHint(XXwindow, 2 * XXborder, fontinfo.height, 2 * XXborder,
		       fontinfo.width);
	XMapWindow(XXwindow);
	XSelectInput(XXwindow, KeyReleased |KeyPressed | ExposeWindow |
		     ButtonPressed | ButtonReleased | ExposeRegion |
		     ExposeCopy);
	XStoreRaster(&XXdisplay, MouseCursor, 16, 16, &RasterMouseCursor);
	XStoreRaster(&XXdisplay, MouseMask, 16, 16, &RasterMouseMask);
	XRegisterCursor(XXwindow, RasterMouseCursor, RasterMouseMask, 11, 8,
			GXcopyInverted, 4, 1);
	tt.t_socketinput = 1;
	tt.t_topos = topos;
	tt.t_reset = reset;
	tt.t_INSmode = NoOperation;
	tt.t_HLmode = HLmode;
	tt.t_inslines = stufflines;
	tt.t_dellines = scraplines;
	tt.t_blanks = NoOperation;
	tt.t_init = init;
	tt.t_cleanup = cleanup;
	tt.t_wipeline = wipeline;
	tt.t_wipescreen = wipescreen;
	tt.t_delchars = NoOperation;
	tt.t_writechars = writechars;
	tt.t_window = setflexlines;
	tt.t_flash = flash;
	tt.t_UpdateBegin = UpdateBegin;
	tt.t_UpdateEnd = UpdateEnd;
	tt.t_ReSize = NoOperation;
	tt.t_socketinput = 1;
	tt.t_readsocket = readsocket;
	tt.t_ILnov = 0;
	tt.t_ILov = 0;
	tt.t_DLnov = 0;
	tt.t_DLov = 0;
	tt.t_ICov = MissingFeature;
	tt.t_DCov = MissingFeature;
}

XError()
{
	;
}

XIOError()
{
	fprintf(stderr, "XIOError\n");
	fprintf(stderr, "errno = %d\n", errno);
	exit(-10);
}

#endif /* XVSWINDOW */  /* Entire file is contained in this conditional */
