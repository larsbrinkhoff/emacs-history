/* $Header:   RCS/loadst.v  Revision 1.1  83/02/09  17:16:53  fen  Rel$ */
/*
 * loadst -- print current time and load statistics.
 *				-- James Gosling @ CMU, May 1981
 *  loadst [ -n ] [ interval ]
 *	07/29/81 jag -- also print info on presence of mail.
 *	05/05/82 jag -- add disk drive utilization statistics.
 */

#include <nlist.h>
#include <stdio.h>
#include <sys/time.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/dk.h>
#include <sys/stat.h>

struct tm *localtime ();

struct nlist    nl[] = {
    { "_avenrun" },
#define	X_CPTIME	1
	{ "_cp_time" },
#define	X_DKXFER	2
	{ "_dk_xfer" },
    { 0 },
};

struct {
    long	time[CPUSTATES];
    long	xfer[DK_NDRIVE];
} s, s1;

double	etime;

int nflag;			/* -n flag -- no newline */
int uflag;			/* -u flag -- user current user ID rather
				   than login user ID */
int repetition;			/* repetition interval */


main (argc, argv)
char  **argv;
{
    register    kmem, i;
    char mail[300];
    struct stat st;
    if ((kmem = open ("/dev/kmem", 0)) < 0) {
	fprintf (stderr, "Can't access /dev/kmem\n");
	exit (1);
    }
    nlist ("/vmunix", nl);
    while (--argc > 0) {
	argv++;
	if (strcmp (*argv, "-n") == 0)
	    nflag++;
	else if (strcmp (*argv, "-u") == 0)
	    uflag++;
	else
	    if ((repetition = atoi (*argv)) <= 0) {
		fprintf (stderr, "Bogus agument: %s\n", *argv);
		exit (1);
	    }
    }
    sprintf (mail, "/usr/spool/mail/%s",
	  uflag ? ((struct passwd *) getpwuid(getuid())) -> pw_name
		: (char *) getenv("USER"));

    while (1) {
	register struct tm *nowt;
	long    now;
#ifdef sun
	long	avenrun[3];
#else
	float   avenrun[3];
#endif
	time (&now);
	nowt = localtime (&now);
	lseek (kmem, (long) nl[0].n_value, 0);
	read (kmem, avenrun, sizeof (avenrun));
	printf ("%d:%02d%s %.2f%s",
	    nowt -> tm_hour == 0 ? 12
	    : nowt ->tm_hour>12 ? nowt->tm_hour-12 : nowt->tm_hour,
	    nowt -> tm_min,
	    nowt -> tm_hour>=12 ? "pm" : "am",
#ifdef sun
	    (double)avenrun[0]/FSCALE,
#else
	    avenrun[0],
#endif
	    stat (mail, &st)>=0 && st.st_size ? " Mail" : "");
	lseek(kmem, (long)nl[X_CPTIME].n_value, 0);
 	read(kmem, s.time, sizeof s.time);
	lseek(kmem, (long)nl[X_DKXFER].n_value, 0);
	read(kmem, s.xfer, sizeof s.xfer);
	etime = 0;
	for (i=0; i < DK_NDRIVE; i++) {
		register t = s.xfer[i];
		s.xfer[i] -= s1.xfer[i];
		s1.xfer[i] = t;
	}
	for (i=0; i < CPUSTATES; i++) {
		register t = s.time[i];
		s.time[i] -= s1.time[i];
		s1.time[i] = t;
		etime += s.time[i];
	}
	if(etime == 0.)
		etime = 1.;
	etime /= 60.;
	{   register max = s.xfer[0];
	    for(i=1; i<DK_NDRIVE; i++)
		if (s.xfer[i]>max) max = s.xfer[i];
	    printf ("[%d]", (int) (max/etime + 0.5));
	}
	if (!nflag)
	    putchar ('\n');
	fflush (stdout);
	if (repetition <= 0)
	    break;
	sleep (repetition);
    }
}

