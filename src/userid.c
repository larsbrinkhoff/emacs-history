/* Look up user ids in /etc/passwd
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


#include <pwd.h>

#define MAXUNAME 9	/* The number of characters used to
			   store a user name. */

#define COUNT int
#define VOID void
#define TEXT char
#define EOS 0

VOID loadusers ();

/* User id routines

The user id routines find the user name associated with a given
user id number.  They derive this information from the system
routines getpwent(), setpwent(), and endpwent(), then store it
in the usernames linked list where it is retained for the
duration of the ls run.  The usernames list is then sequentially
searched for user names.  The last user id found and the
associated list entry are kept in lastuid and lastuser.

The usernames list is searched in the order in which getpwent()
returned the names.  ANY OTHER SEARCH ORDER IS WRONG!  By
long-standing Unix convention, the first user name associated
with a user id is the one which "owns" that id.

 */

/* Uname - the structure of the usernames list */

typedef struct uname {
   struct uname *u_nextu;
   COUNT	u_uid;
   TEXT	u_uname[MAXUNAME];
   } UNAME;

/* usernames - the head of the user names list
   lastuser  - a pointer to the last UNAME entry found.
   lastuid   - the last user id found
 */
UNAME *usernames;
UNAME *lastuser;
COUNT   lastuid;

/* getuser - get user name, if possible

Getuser returns a pointer to a static string containing the user
name associated with user id uid or, if the user name cannot be
found, a numeric string representing the user id.  If the user
id number will not fit within the available space (unlikely) a
pointer to "***" will be returned.

Getuser checks the usernames list, invoking loadusers() if the
list has not yet been initialized.

 */

TEXT *
getuser (uid)
   COUNT uid; {
   static TEXT cuid[MAXUNAME];
   UNAME *u;
   TEXT *strcpy();
   COUNT ndigits();
   
   if (usernames==0)
      loadusers();

   if (lastuser!=0 && lastuid==uid)
      return lastuser->u_uname;

   for (u=usernames; u!=0; u = u->u_nextu)
      if (u->u_uid==uid) {
         lastuid = uid;
	 lastuser = u;
	 return u->u_uname;
	 }

   if (ndigits(uid) < MAXUNAME)
      sprintf (cuid, "%1d", uid);
   else
      strcpy (cuid, "***");

   return cuid;
   }

/* ndigits - return the number of digits in a uid */
COUNT
ndigits (uid)
   COUNT uid; {
   COUNT n=0;
   
   for (; uid!=0; uid /= 10)
      n++;

   return (n);
   }

/* loadusers - load user names list

Loadusers reads the password file using getpwent, storing user
ids and user names in a linked list.  THE IDS AND NAMES MUST BE
STORED IN THE ORDER GETPWENT() RETURNS THEM.  Otherwise,
multiple users of the same uid will be wrongly shown.

 */

VOID 
loadusers() {
   struct passwd *getpwent();
   VOID setpwent(), endpwent();
   struct passwd *pwe;
   UNAME *une, *lastune;
   TEXT *strncpy();

   lastuser = 0;
   setpwent();
   while ((pwe=getpwent())!=0) {
      une = (UNAME *) calloc(1,sizeof(UNAME));
      if (une==(UNAME *)0)
         error ("loadusers: not enough memory to load all the user names");
      une->u_uid = pwe->pw_uid;
      strncpy (une->u_uname, pwe->pw_name, MAXUNAME);
      (une->u_uname)[MAXUNAME-1] = EOS;
      une->u_nextu = 0;
      if (usernames==0)
         usernames = une;
      else
         lastune->u_nextu = une;
      lastune = une;
      }
   endpwent();
   }
