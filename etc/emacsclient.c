/* Client process that communicates with GNU Emacs acting as server.
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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  int s, n, i;
  FILE *out;
  struct sockaddr_un server;
  char *homedir, *cwd, *str;
  char string[BUFSIZ];

  char *getenv (), *getwd ();

  if (argc < 2)
    {
      printf ("Usage: %s [filename]\n", argv[0]);
      exit (1);
    }

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
      fprintf (stderr, "No home directory\n");
      exit (1);
    }
  strcpy (server.sun_path, homedir);
  strcat (server.sun_path, "/.emacs_server");
  if (connect (s, &server, strlen (server.sun_path) + 2) < 0)
    {
      perror ("connect");
      exit (1);
    }
  if ((out = fdopen (s, "r+")) == NULL)
    {
      perror ("fdopen");
      exit (1);
    }

  cwd = getwd (string);
  if (cwd == 0)
    abort ();

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] != '/')
	fprintf (out, "%s/", cwd);
      fprintf (out, "%s ", argv[i]);
    }
  fprintf (out, "\n");
  fflush (out);

  printf ("Waiting for Emacs...");
  fflush (stdout);

  rewind (out); /* re-read the output */
  str = fgets (string, BUFSIZ, out); 

  /* Now, wait for an answer and print any messages.  */
  
  while (str = fgets (string, BUFSIZ, out))
    printf ("%s", str);
  
  exit (0);
}

#endif /* BSD */

