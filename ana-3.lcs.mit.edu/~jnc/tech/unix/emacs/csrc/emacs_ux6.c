#include <tty.h>
#include <rqfile.h>
#include <init_rec.h>
#include "emacs_io.h"
#include "emacs_gb.h"

extern int curbf;


char	*gethome();
char	*getmail();
char	*getterm();

char	mailbox[128] = "";
char	home[128] = "";

	
int	ttystat[] = {
	T_GET | T_CANO,
	-1 };

char	*ttys[] = {
	0,
	"vt52",
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	"h19"
};

	
/* simulate the UNIX v.7 system call "getenv" for the special cases: */
/* 	"TERM" - get terminal type */
/* 	"HOME" - get user's home directory */
/* 	"MAIL" - get user's mailbox file name */

char	*getenv (env)

char	*env;
{
	if (streq (env, "TERM")) return (getterm ());
	else if (streq (env, "HOME")) return (gethome ());
	else if (streq (env, "MAIL")) return (getmail ());
	else return (0);
}


char	*getterm ()
{
	register int cano;

	ttymod (0, ttystat);
	cano = ttystat[0] & 0377;
	return (ttys[cano]);
	}



char	*gethome ()
{
	struct init_rec inrec;
	int infd;
	
	if (*home != 0) return (home);
	gtpw();
	seprintf(mailbox,"%s/%s",home, INITFILE);
	if ((infd = open (mailbox, 2)) >= 0) {
		read (infd, &inrec, sizeof (inrec));
		seprintf (mailbox,"%s/%s",home,inrec.in_mail);
		close (infd);
	} else {
		seprintf (mailbox,"%s/%s",home,".mail");
	}
	return (home);
}


char *getmail ()
{
	if (*mailbox != 0) return (mailbox);
	gethome ();
	return (mailbox);
}


/* gtpw -- get password file entry */

/* **** NOTE, we can't use getpwnam, because it uses standard I/O */

gtpw()


{
	int oldbf;
#ifdef	DIRED
	extern int diron;
#endif
	char xbuf[100];
	register char *cp;
	register char *ap;
	register int uid;
	int newbf;
	oldbf = curbf;
	
	uid = getuid() & 0377;
	chgbuf (".passwd");
	newbf = curbf;
	if (nlines < 10) {
#ifdef	DIRED
		diron = 0;
#endif
		readin ("/etc/passwd",1);
#ifdef	DIRED
		diron = 1;
#endif
	}
	
	seprintf (xbuf,"^[^:]*:[^:]*:%d:",uid);	

	if (rgsrch(1,0,xbuf,0,1)) {
		srch(kline,kcol,":",1);
		srch(kline,kcol+1,":",1);
		srch(kline,kcol+1,":",1);
		srch(kline,kcol+1,":",1);
		srch(kline,kcol+1,":",1);
		cp = mkline(kline)+kcol+1;
		ap = home;
		while (*cp != ':') {
			*ap++ = *cp++;
		}
	} 
	chbuf(oldbf);
	klbfr (newbf);
}


/* Suspend the current EMACS - Display "Suspended..." message
 * and ptrace back to your parent.
 */

suspend ()
{
	prompt1 ("...");
	mflush (stdout);
	cook();
	ptrace (0, getpid (), 0, 1);	/* suspend for later resumption */
	uncook();
	unprompt();
	clear();
	disup();
}


#ifdef	DIRED
#define	INTR	2
#define	QUIT	3

system(s)
char *s;
{
	int status, pid, wpid;
	register int *istat, *qstat;

	if ((pid = fork()) == 0) {
		execl("/bin/sh", "sh", "-c", s, 0);
		_exit(127);
	}
	istat = signal(INTR, 1);
	qstat = signal(QUIT, 1);
	while ((wpid = wait(&status)) != pid && wpid != -1);
	if (wpid == -1)
		status = -1;
	signal(INTR, istat);
	signal(QUIT, qstat);
	return (status);
}
#endif
