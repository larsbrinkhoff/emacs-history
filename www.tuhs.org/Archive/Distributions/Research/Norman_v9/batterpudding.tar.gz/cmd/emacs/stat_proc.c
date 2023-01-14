#include <stdio.h>
#include <pwd.h>

/* EMACS_MODES: c !fill */

/* emacs statistics file processor -- this program analyzes the
 * emacs statistics file to print useful summaries of usage */


float tscale = 60.0;

main(argc,argv)

int  argc;
char **argv;

{
	char inbuf[256];	
	float usrtime,systime,ninch,noutc,nmkline,nbread,nbwrite,nbseek;
	float tustime,tsytime,tinch,toutc,tmkline,tbread,tbwrite,tbseek = 0.0;
	float tsessions = 0.0;
#define MAXPEOPLE 100
	

	int myusers[MAXPEOPLE];
	int nusers = 0;
	int people[MAXPEOPLE];
	int sessions[MAXPEOPLE];
	int starflag = 0;

	char xbuf[BUFSIZ];
	char sysbuf[100];
	
	
	register i;
	int uid;
	struct passwd *pwd;
	struct passwd mpwd;
	int pipes[2];
	long pplid;
	char mpname[20];
	
	if (argv[0][0]=='3') tscale = 100.0; /* 3b20 statistics */

	if (argc > 1) {
		close(0);
		open(argv[1],0);
		setbuf(stdin,xbuf);
		if (*argv[2] == '-') {
			starflag++;
			argc = 0;
			pipe(pipes);
			if(fork()) {
					/* parent */
				close(0);
				close(pipes[1]);
				dup(pipes[0]);
				close(pipes[0]);
			} else {
				close(1);
				dup(pipes[1]);
				close(pipes[0]);
				close(pipes[1]);
				execl("/bin/sort","sort",argv[1],0);
			}
		}
		while (nusers+2 < argc) {

			pwd = getpwnam(argv[nusers+2]);
			if (pwd == NULL) {
				pwd = &mpwd;
				sscanf(argv[nusers+2],"%d",&(pwd->pw_uid));
			}
			myusers[nusers++] = pwd->pw_uid;
		}
	}
	myusers[nusers] = -2;
	for (i = 0; i < MAXPEOPLE; i++) {
		people[i] = -1;
		sessions[i] = 0;
	}
	printf("\n\n EMACS version %s User Statistics:\n\n",argv[1]+1);
	while (1) {
next_entry:	if (gets(inbuf) == NULL) {
			starflag = 0;
nextuid:		if (tinch == 0.0) tinch = 1.0;
			if (toutc == 0.0) toutc = 1.0;
			if (tmkline == 0.0) tmkline = 1.0;
			for (i = 0; people[i] != -1; i++) {
				pwd = getpwuid(people[i]);
				if (pwd == NULL) {
					pwd = &mpwd;
					pwd->pw_name = mpname;
					pplid = (unsigned) people[i];
					sprintf(mpname,"%ld",pplid);
				}
				printf("%20s: %d\n",pwd->pw_name,sessions[i]);
			}
			if (i > 1) printf("%20s: %f\n","all",tsessions);
			
			
			printf ("\nusertime: %.3f systime: %.3f input: %.0f output: %.0f\n",
				tustime,tsytime,tinch,toutc);
			printf ("makeline: %.0f read: %.0f write: %.0f, seek:  %.0f\n\n",
				tmkline,tbread,tbwrite,tbseek);
			printf ("seconds/inchar: %.6f  seconds/outchar: %.6f  outchar/inchar: %.3f \n",
			tustime/tinch,tustime/toutc,toutc/tinch);
			printf ("input chars/session: %.2f	time/session: %.3f\n\n",tinch/tsessions,tustime/tsessions);
			
			printf ("r-w-s/inchar:	%.6f	%.6f	%.6f\n",tbread/tinch,tbwrite/tinch,tbseek/tinch);
			printf ("r-w-s/outchar:	%.6f	%.6f	%.6f\n",tbread/toutc,tbwrite/toutc,tbseek/toutc);
			printf ("r-w-s/mkline:	%.6f	%.6f	%.6f\n\n",tbread/tmkline,tbwrite/tmkline,tbseek/tmkline);
			printf("\n");
			if (starflag) {
				tbread = tbwrite = tbseek = tmkline = toutc = tinch = tustime = tsytime = 0.0;
				people[0] = -1;
				sessions[0] = 0;
				tsessions = 0.0;
				goto adduid;
			}
			printf("");
			exit(0);
		}

		sscanf(inbuf,"%d %f %f %f %f %f %f %f %f",
			&uid,&usrtime,&systime,&ninch,&noutc,&nmkline,
			&nbread,&nbwrite,&nbseek);
		if (uid < 0) uid += 65536;
		if (starflag && (people[0] != -1) && (people[0] != uid)) goto nextuid;

adduid:		if (nusers > 0) {
			for (i = 0; i < nusers; i++) {
				if (myusers[i] == uid) break;
			}
			if (myusers[i] != uid) goto next_entry;
		}
		tbread += nbread;
		tbwrite += nbwrite;
		tbseek +=  nbseek;
		tmkline += nmkline;
		if (nmkline < 0) tmkline += 65536.0;
		toutc += noutc;
		tinch += ninch;
		if (ninch < 0) tinch += 65336.0;
		tsessions += 1;
		for (i = 0; i < MAXPEOPLE; i++) {
			if (people[i] == uid) {
				sessions[i] += 1;
				i = MAXPEOPLE; /* break loop */
			}
			if (people[i]!= -1) continue;
			people[i] = uid;
			i--;		/* next iteration counts */
		}
		tsytime += (systime/tscale);
		tustime += (usrtime/tscale);
	}
}
