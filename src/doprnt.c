/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
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


#include <stdio.h>
#include <ctype.h>

doprnt (buffer, bufsize, format, args)
     char *buffer;
     register int bufsize;
     char *format;
     char **args;
{
  int cnt = 0;			/* Number of arg to gobble next */
  register char *fmt = format;	/* Pointer into format string */
  register char *bufptr = buffer; /* Pointer into output buffer.. */
  char tembuf[80];
  register int tem;
  char *string;
  char fmtcpy[20];

  bufsize--;
  while (*fmt && bufsize > 0)	/* Loop until end of format string or buffer full */
    {
      if (*fmt == '%')	/* Check for a '%' character */
	switch (*++fmt)
	  {
	  default:
	    /* Copy this one %-spec into fmtcopy.  */
	    string = fmtcpy;
	    *string++ = '%';
	    while (1)
	      {
		*string++ = *fmt;
		if (! (*fmt >= '0' && *fmt <= '9') && *fmt != '-' && *fmt != ' ')
		  break;
		fmt++;
	      }
	    *string = 0;
	    /* If this fmt spec is valid for us, format it into tembuf.
	       Otherwise, error.  */
	    if (*fmt == 'b' || *fmt == 'o' || *fmt == 'x' || *fmt == 'd')
	      sprintf (tembuf, fmtcpy, args[cnt++]);
	    else
	      error ("Invalid format operation %%%c", *fmt);
	    /* Now copy tembuf into final outupt, truncating as nec.  */
	    string = tembuf;
	    goto doit;

	  case 's':
	    string = args[cnt++];
	    /* Copy string into final output, truncating if no room.  */
	  doit:
	    fmt++;
	    tem = strlen (string);
	    if (tem > bufsize)
	      tem = bufsize;
	    strncpy (bufptr, string, tem);
	    bufptr += tem;
	    bufsize -= tem;
	    continue;

	  case 'c':
	    fmt++;
	    *bufptr++ = (int) args[cnt++];
	    bufsize--;
	    continue;

	  case '%': ;
	  }
      *bufptr++ = *fmt++;	/* Just some characters; Copy 'em */
      bufsize--;
    };

  *bufptr = 0;		/* Make sure our string end with a '\0' */
  return bufptr - buffer;
}
