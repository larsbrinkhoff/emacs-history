#include <stdio.h>
#include "stdefs.h"
#include <pwd.h>

/* @(#)homedir.c	1.1	11/25/80 07:41:35
 * Get home directory of designated user.
 * If this program is compiled with the option "-DMAIN", then it will
 * produce a main program that prints the result on standard output.
 * otherwise it will produce a C-callable subroutine that returns a pointer
 * to a static area containing the result.
 * In either case, the user name is the argument to the procedure.
 * J. Leth, IH 6E-318, x6133.
 */

/* Functions: */
extern struct passwd *	getpwnam();

#ifndef MAIN
char *
homedir(user)
	char *user;	/* user login name */
{

#else
main(argc, argv)
	int argc;
	char *argv[];
{
	char *user;
#endif

	struct passwd *pwentry;

#ifdef MAIN
	if (argc != 2) {
		usage("Usage:\thomedir user\n\
	Prints home directory (login dir.) of the designated user.\n");
	}
	user = argv[1];
#endif

	if((pwentry = getpwnam(user)) == NULL) {
		errexit("homedir: user '%s' unknown.\n", user);
	}

#ifdef MAIN
	puts(pwentry->pw_dir);
#else
	return(pwentry->pw_dir);
#endif
}
