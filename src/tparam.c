/* Merge parameters into a termcap entry string.
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

/* Assuming `string' is the value of a termcap string entry
   containing `%' constructs to expand parameters,
   merge in parameter values and store result in block `outstring' points to.
   No check is made for overflowing `outstring';
   the caller is wise to allocate space for it based on the size of
   `string', knowing that the size can increase by at most a couple
   of characters per parameter.
   The third and following args to tparam serve as the parameter values.  */

tparam (string, outstring, arg)
     char *string;
     register char *outstring;
     int arg;
{
  register int c;
  register char *p = string;
  register int *argp = &arg;

  int tem;
  char *format;
  char buf[10];

  while (c = *p++)
    {
      if (c == '%')
	{
	  c = *p++;
	  format = 0;
	  switch (c)
	    {
	    case 'd':		/* %d means output in decimal */
	      format = "%d";
	      break;
	    case '2':		/* %2 means output in decimal, 2 digits. */
	      format = "%2d";
	      break;
	    case '3':		/* %3 means output in decimal, 3 digits. */
	      format = "%3d";
	      break;
	    case '.':		/* %. means output as character */
	      format = "%c";
	      break;
	    case 'C':
	      /* For c-100: print quotient of value by 96, if nonzero,
		 then do like %+ */
	      if (argp[0] >= 96)
		{
		  *outstring++ = argp[0] / 96;
		  argp[0] %= 96;
		}
	    case '+':		/* %+x means add character code of char x */
	      format = "%c";	/* Then output sum as a character. */
	      argp[0] += *p++;
	      break;
	    case '>':		/* %>xy means if arg is > char code of x, */
	      if (argp[0] > *p++) /* then add char code of y to the arg, */
		argp[0] += *p;	/* and in any case don't output. */
	      p++;		/* Leave the arg to be output later. */
	      break;
	    case 'r':		/* %r means interchange following two args */
	      tem = argp[0];
	      argp[0] = argp[1];
	      argp[1] = tem;
	      break;
	    case 'i':		/* %i means add one to arg, */
	      argp[0] ++;	/* and leave it to be output later. */
	      break;
	    case '%':		/* %% means output %; no arg. */
	      format = "%%";
	      argp--;
	      break;
	    case 'n':		/* %n means xor each of next two args with 140 */
	      argp[0] ^= 0140;
	      argp[1] ^= 0140;
	      break;
	    case 'B':		/* %B means express arg as BCD char code. */
	      argp[0] = 16 * (argp[0] / 10) + argp[0] % 10;
	    case 'D':		/* %D means weird Delta Data transformation */
	      argp[0] -= 2 * (argp[0] % 16);
	      break;
	    }
	  /* If an output format was specified, output one arg with it,
	     and swallow that arg.  */
	  if (format)
	    {
	      sprintf (outstring, format, argp[0]);
	      outstring += strlen (outstring);
	      argp++;
	    }
	}
      else
	/* Ordinary character in the argument string.
	   Just output it as itself, but flush high bit.  */
	*outstring++ = c;
    }
  *outstring = 0;
}
