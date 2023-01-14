#include <stdio.h>
#include "stddefs.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <utmp.h>

/* Returns a pointer to a character string containing the tty name of a
 * logged-on user.  If the user is logged on more than once, the tty
 * returned is the one that has been used most recently.  If the user is
 * not presently logged on, the null string is returned.  Each call to
 * findtty returns a pointer to the same data area, so the string must
 * be copied if it is to be used again.
 *
 * NOTE: if this code is compiled with the "-DMAIN" option, then a main
 * program will be produced, instead of a function.  The main program
 * takes one user name as argument and outputs the ttyname on standard
 * output.
 *
 * J. Leth, IH 6E-318, x6133.
 */

#define NAMELEN	8
	/* Max. number of characters in a login name */
#define TTYLEN	8
	/* Max. number of characters in a tty name */

#ifndef MAIN
char *
findtty(user)
	char *user;	/* user login name */
{

#else
main(argc, argv)
	int argc;
	char *argv[];
{
	char *user;
#endif

	extern long ftime();
	extern int strcmp();

	FILE *f;
	struct utmp buf;
	long ttytime = 0L;
		/* Holds latest access time found for requested user */
	static char ttyname[TTYLEN+1];
		/* Holds name of tty with that access time */
	long acc_time = 0L;
		/* Access time of tty currently being considered */
	char *utmp = "/etc/utmp";

	if((f=fopen(utmp,"r"))==NULL) {
		errexit("findtty: can't open %s file.\n", utmp);
	}

	ttyname[0] = NUL;

#ifdef MAIN
	if (argc != 2) {
		usage("Usage:\tfindtty <login-id>\n\
	Outputs the name of the most recently-used terminal on which\n\
	user <login-id> is currently logged in.\n");
	}

	user = argv[1];
#endif

	while(fread(&buf,sizeof(buf),1,f) == 1) {
		/* Search utmp file for logins of the designated user.
		 * For each one found, see if the access time is later
		 * than the latest one so far; if it is, remember the
		 * ttyname and ttytime.
		 */

		if(strncmp(buf.ut_name, user, NAMELEN) != 0)	continue;

		if((acc_time=ftime(buf.ut_line)) > ttytime) {
			ttytime = acc_time;
			strncpy(ttyname, buf.ut_line, TTYLEN);
			ttyname[TTYLEN] = '\0';
				/* just to be sure string is terminated */
		}
	}
#ifndef MAIN
	return(ttyname);
#else
	printf("%s\n", ttyname);
#endif
}

long
ftime(s)
char *s;
{
	/* Returns the time of last access for the named device.
	 * The name given as taken as the last component of the device
	 * pathname.  The first component, "/dev/" is supplied by this
	 * routine.  If the device doesn't exist, -1L is returned.
	 */

	char dev[20];
	struct stat statb;

	strcpy(dev,"/dev/");
	strcat(dev,s);
	if( stat(dev,&statb)<0 )
		return(-1L);
	return( statb.st_atime );
}
