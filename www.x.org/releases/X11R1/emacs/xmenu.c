/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1986 Free Software Foundation, Inc.

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
and this notice must be preserved on all copies.

 * X pop-up deck-of-cards menu facility for gnuemacs.
 *
 * Written by Jon Arnold and Roman Budzianowski
 * Mods and rewrite by Robert Krawitz
 *
 */

/* $Source: /@/orpheus/u1/X11/clients/emacs/RCS/xmenu.c,v $
 * $Author: newman $
 * $Locker:  $
 * $Header: xmenu.c,v 1.1 87/09/12 16:33:40 newman Exp $
 *
 */

#ifndef lint
static char *rcsid_GXMenu_c = "$Header: xmenu.c,v 1.1 87/09/12 16:33:40 newman Exp $";
#endif	lint

#ifdef HAVE_X_MENU

#include "config.h"
#include <signal.h>
#include "xterm.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#include <stdio.h>
#undef NULL
#include "dispextern.h"
#include <sys/time.h>
#include <fcntl.h>
#include "lisp.h"
#include "window.h"
#include <X11/XMenu.h>

#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

#define NUL 0

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif TRUE
extern Display *XXdisplay;
extern int XXxoffset,XXyoffset;
extern int XXfontw,XXfonth;
extern int (*handler)();
extern int request_sigio(), unrequest_sigio();

/*************************************************************/

XMenuQuit ()
{
  error("Unknown XMenu error");
}

DEFUN ("x-popup-menu",Fx_popup_menu, Sx_popup_menu, 1, 2, 0,
       "Interface to XMenu library; permits creation and use of\n\
deck-of-cards pop-up menus.\n\
\n\
ARG is a position specification; it is a list of (Xoffset, Yoffset)\n\
from the top-left corner of the editor window.  Note that this offset\n\
is relative to the center of the first line in the first pane of the\n\
menu, not the top left of the menu as a whole.\n\
\n\
MENU is a specifier for a menu.  It is a list of the form\n\
(title pane1 pane2...), and each pane is a list of form\n\
(title (line item)...).  Each line should be a string, and item should\n\
be the return value for that line (i. e. if it is selected.")
       (arg,menu)
     Lisp_Object arg,menu;
{
  int number_of_panes;
  Lisp_Object XMenu_return;
  int XMenu_xpos,XMenu_ypos;
  char **menus;
  char ***names;
  Lisp_Object **obj_list;
  int *items;
  char *title;
  char *error_name;
  Lisp_Object ltitle, selection;
  extern Lisp_Object Vx_mouse_abs_pos;
  Lisp_Object XEmacsMenu();
  int i;
  int mask;

  check_xterm();
  XMenu_xpos = XINT(Fcar(Vx_mouse_abs_pos));
  XMenu_ypos = XINT(Fcar(Fcdr (Vx_mouse_abs_pos)));
  ltitle = Fcar(menu);
  CHECK_STRING(ltitle,1);
  title = (char *) XSTRING(ltitle)->data;
  /* title = (char *) xmalloc ((XSTRING(ltitle)->size + 1) * sizeof(char));
     bcopy (XSTRING(ltitle)->data, title, XSTRING(ltitle)->size); */
  number_of_panes=list_of_panes(&obj_list, &menus, &names, &items, Fcdr(menu));
#ifdef XDEBUG
  fprintf(stderr,"Panes= %d\n", number_of_panes);
  for(i=0; i < number_of_panes; i++)
    {
      fprintf (stderr,"Pane %d lines %d title %s\n", i, items[i], menus[i]);
      for (j=0; j < items[i]; j++)
	{
	  fprintf (stderr,"    Item %d %s\n", j, names[i][j]);
	}
    }
#endif
  mask = sigblock (sigmask (SIGIO));
  XSetErrorHandler(XMenuQuit);
  selection = XEmacsMenu(RootWindow(XXdisplay,0), XMenu_xpos, XMenu_ypos, names, menus,
			 items, number_of_panes, obj_list ,title, &error_name);
  XSetErrorHandler(handler);
  sigsetmask (mask);
  /** fprintf(stderr,"selection = %x\n",selection);  **/
  if (selection != NUL)
    {				/* selected something */
      XMenu_return = selection;
    }
  else
    {				/* nothing selected */
      XMenu_return = Qnil;
    }
  /* now free up the strings */
  for (i=0; i < number_of_panes; i++)
    {
      free(names[i]);
      free(obj_list[i]);
    }
  free(menus);
  free(obj_list);
  free(names);
  free(items);
  /*   free(title); */
  if (error_name) error(error_name);
  return XMenu_return;
}

struct indices {
  int pane;
  int line;
};

Lisp_Object
XEmacsMenu(parent, startx, starty, line_list, pane_list, line_cnt,
		      pane_cnt, item_list, title, errorstr)
     Window parent;		
     int startx, starty;	/* upper left corner position BROKEN */
     char **line_list[];   	/* list of strings for items */
     char *pane_list[];		/* list of pane titles */
     char *title;
     int pane_cnt;		/* total number of panes */
     Lisp_Object *item_list[];	/* All items */
     int line_cnt[];		/* Lines in each pane */
     char **errorstr;		/* Error returned */
{
  XMenu *GXMenu;
  int panes, sel, lpane, status;
  Lisp_Object entry;
  /* struct indices *datap, *datap_save; */
  char *datap;
  
  *errorstr = (char *) 0;		/* Initialize errorstr pointer to null */
  if ((GXMenu = XMenuCreate(XXdisplay, parent, "emacs" )) == NUL )
    {
      *errorstr = "Can't create menu";
      /* error("Can't create menu"); */
      return(0);
    }
  
  for (panes=0; panes < pane_cnt; panes++ )
    ;
  /* datap = (struct indices *) xmalloc (lines * sizeof(struct indices)); */
  /*datap = (char *) xmalloc(lines * sizeof(char));
    datap_save = datap;*/
  
  for (panes = 0;panes < pane_cnt; panes++ )
    {
      /* create all the necessary panes */
      if ((lpane= XMenuAddPane(XXdisplay, GXMenu, pane_list[panes] , TRUE ))
	  == XM_FAILURE)
	{
	  XMenuDestroy(XXdisplay, GXMenu);
	  *errorstr = "Can't create pane";
	  /* error("Can't create pane"); */
	  /* free(datap); */
	  return(0);
	}
      for ( sel = 0; sel < line_cnt[panes] ; sel++ )
	{
	  /* add the selection stuff to the menus */
	  /* datap[sel+sofar].pane = panes;
	     datap[sel+sofar].line = sel; */
	  if (XMenuAddSelection(XXdisplay, GXMenu, lpane, 0, line_list[panes][sel], TRUE)
	      == XM_FAILURE)
	    {
	      XMenuDestroy(XXdisplay, GXMenu);
	      /* free(datap); */
	      *errorstr = "Can't add selection to menu";
	      /* error ("Can't add selection to menu"); */
	      return(0);
	    }
	}
    }
  /* all set and ready to fly */
  XMenuSetFreeze(GXMenu, TRUE);
  XMenuRecompute(XXdisplay,GXMenu);
  panes = sel = 0;
  
  status = XMenuActivate(XXdisplay, GXMenu, &panes, &sel, startx, starty,
			 ButtonReleaseMask, &datap);
  switch (status ) {
  case XM_SUCCESS:
#ifdef XDEBUG
    fprintf (stderr, "pane= %d line = %d\n", panes, sel);
#endif
    entry = item_list[panes][sel];
    break;
  case XM_FAILURE:
    /*free (datap_save); */
    /*XMenuDestroy(XXdisplay, GXMenu);*/
    *errorstr = "Can't activate menu";
    /* error("Can't activate menu"); */
  case XM_IA_SELECT:
  case XM_NO_SELECT:
    entry = Qnil;
    break;
  }
  XMenuDestroy(XXdisplay, GXMenu);
  XFlush(XXdisplay);
  if (XPending(XXdisplay))
	  read_events_block();
  /*free(datap_save);*/
  return(entry);
}

syms_of_xmenu ()
{
  defsubr (&Sx_popup_menu);
}

list_of_panes (vector, panes, names, items, menu)
     Lisp_Object ***vector;	/* RETURN all menu objects */
     char ***panes;		/* RETURN pane names */
     char ****names;		/* RETURN all line names */
     int **items;		/* RETURN number of items per pane */
     Lisp_Object menu;
{
  Lisp_Object tail, item, item1;
  int i;
  
  if (XTYPE (menu) != Lisp_Cons) menu = wrong_type_argument(Qlistp, item);
  
  i= XFASTINT(Flength(menu,1));

  *vector = (Lisp_Object **) xmalloc(i * sizeof (Lisp_Object *));
  *panes = (char **) xmalloc(i * sizeof (char *));
  *items = (int *) xmalloc(i * sizeof (int));
  *names = (char ***) xmalloc(i * sizeof (char **));

  for (i=0,tail = menu; !NULL (tail); tail = Fcdr (tail), i++)
    {
       item = Fcdr(Fcar(tail));
       if (XTYPE (item) != Lisp_Cons) (void) wrong_type_argument(Qlistp, item);
#ifdef XDEBUG
       fprintf (stderr, "list_of_panes check tail, i=%d\n", i);
#endif
       item1 = Fcar(Fcar(tail));
       CHECK_STRING (item1, 1);
#ifdef XDEBUG
       fprintf (stderr, "list_of_panes check pane, i=%d%s\n", i,
		XSTRING(item1)->data);
#endif
       (*panes)[i] = (char *) XSTRING(item1)->data;
       (*items)[i] = list_of_items ((*vector)+i, (*names)+i, item);
       /* (*panes)[i] = (char *) xmalloc((XSTRING(item1)->size)+1);
	  bcopy (XSTRING (item1)->data, (*panes)[i], XSTRING(item1)->size + 1)
	  ; */
    }
  return i;
}
     

list_of_items (vector,names,pane)  /* get list from emacs and put to vector */
     Lisp_Object **vector;	/* RETURN menu "objects" */
     char ***names;		/* RETURN line names */
     Lisp_Object pane;
{
  Lisp_Object tail, item, item1;
  int i;

  if (XTYPE (pane) != Lisp_Cons) pane = wrong_type_argument(Qlistp, item);
  
  i= XFASTINT(Flength(pane,1));

  *vector = (Lisp_Object *) xmalloc(i * sizeof (Lisp_Object));
  *names = (char **) xmalloc(i * sizeof (char *));

  for (i=0,tail = pane; !NULL (tail); tail = Fcdr (tail), i++)
    {
       item = Fcar(tail);
       if (XTYPE (item) != Lisp_Cons) (void) wrong_type_argument(Qlistp, item);
#ifdef XDEBUG
       fprintf (stderr, "list_of_items check tail, i=%d\n", i);
#endif
       (*vector)[i] =  Fcdr (item);
       item1 = Fcar(item);
       CHECK_STRING (item1, 1);
#ifdef XDEBUG
       fprintf (stderr, "list_of_items check item, i=%d%s\n", i,
		XSTRING(item1)->data);
#endif
       (*names)[i] = (char *) XSTRING(item1)->data;
       /* (*names)[i] = (char *) xmalloc((XSTRING(item1)->size)+1);
       bcopy (XSTRING (item1)->data, (*names)[i], XSTRING(item1)->size + 1); */
    }
  return i;
}

#endif HAVE_X_MENU
