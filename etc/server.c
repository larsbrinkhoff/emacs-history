/* Communication subprocess for GNU Emacs acting as server.
   Copyright (C) 1986, 1987 Free Software Foundation, Inc.

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


/* The GNU Emacs edit server process is run as a subprocess of Emacs
   under control of the file lisp/server.el.
   This program accepts communication from client (program emacsclient.c)
   and passes their commands (consisting of keyboard characters)
   up to the Emacs which then executes them.  */

#define NO_SHORTNAMES
#include "../src/config.h"
#undef read
#undef write
#undef open

#ifndef BSD
#include <stdio.h>

main ()
{
  fprintf (stderr, "Sorry, the Emacs server is supported only on Berkeley Unix.\n");
  exit (1);
}

#else /* BSD */

#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/un.h>
#include <stdio.h>
#include <errno.h>

extern int errno;

main ()
{
  int s, infd, fromlen;
  struct sockaddr_un server, fromunix;
  char *homedir;
  char *str, string[BUFSIZ], code[BUFSIZ];
  FILE *infile;
  FILE **openfiles;
  int openfiles_size;

  char *getenv ();

  openfiles_size = 20;
  openfiles = (FILE **) malloc (openfiles_size * sizeof (FILE *));
  if (openfiles == 0)
    abort ();

  /* 
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      perror ("socket");
      exit (1);
    }
  server.sun_family = AF_UNIX;
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr,"No home directory\n");
      exit (1);
    }
  strcpy (server.sun_path, homedir);
  strcat (server.sun_path, "/.emacs_server");
  if (bind (s, &server, strlen (server.sun_path) + 2) < 0)
    {
      perror ("bind");
      exit (1);
    }
  /*
   * Now, just wait for everything to come in..
   */
  if (listen (s, 5) < 0)
    {
      perror ("listen");
      exit (1);
    }

  /* Disable sigpipes in case luser kills client... */
  signal (SIGPIPE, SIG_IGN);
  for (;;)
    {
      int rmask = (1 << s) + 1;
      if (select (s + 1, &rmask, 0, 0, 0) < 0)
	perror ("select");
      if (rmask & (1 << s))	/* client sends list of filenames */
	{
	  fromlen = sizeof (fromunix);
	  fromunix.sun_family = AF_UNIX;
	  infd = accept (s, &fromunix, &fromlen); /* open socket fd */
	  if (infd < 0)
	    {
	      if (errno == EMFILE || errno == ENFILE)
		printf ("Too many clients.\n");
	      else
		perror ("accept");
	      continue;
	    }

	  if (infd >= openfiles_size)
	    {
	      openfiles_size *= 2;
	      openfiles = (FILE **) realloc (openfiles,
					     openfiles_size * sizeof (FILE *));
	      if (openfiles == 0)
		abort ();
	    }

	  infile = fdopen (infd, "r+"); /* open stream */
	  if (infile == NULL)
	    {
	      printf ("Too many clients.\n");
	      write (infd, "Too many clients.\n", 18);
	      close (infd);		/* Prevent descriptor leak.. */
	      continue;
	    }
	  str = fgets (string, BUFSIZ, infile);
	  if (str == NULL)
	    {
	      perror ("fgets");
	      close (infd);		/* Prevent descriptor leak.. */
	      continue;
	    }
	  openfiles[infd] = infile;
	  printf ("Client: %d %s", infd, string);
	  /* If what we read did not end in a newline,
	     it means there is more.  Keep reading from the socket
	     and outputting to Emacs, until we get the newline.  */
	  while (string[strlen (string) - 1] != '\n')
	    {
	      if (fgets (string, BUFSIZ, infile) == 0)
		break;
	      printf ("%s", string);
	    }
	  fflush (stdout);
	  fflush (infile);
	  continue;
	}
      else if (rmask & 1) /* emacs sends codeword, fd, and string message */
	{
	  /* Read command codeword and fd */
	  scanf ("%s %d%*c", code, &infd);

	  /* Transfer text from Emacs to the client, up to a newline.  */
	  infile = openfiles[infd];
	  while (1)
	    {
	      if (fgets (string, BUFSIZ, stdin) == 0)
		break;
	      fprintf (infile, "%s", string);
	      if (string[strlen (string) - 1] == '\n')
		break;
	    }
	  fflush (infile);

	  /* If command is close, close connection to client.  */
	  if (strncmp (code, "Close:", 6) == 0) 
	    if (infd > 2) 
	      {
		fclose (infile);
		close (infd);
	      }
	  continue;
	} 
    }
}
#endif /* BSD */


