#include <termio.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>

/* EMACS_MODES: c,!fill,tabstop=4 */

#define MWIND 6

int wproc[MWIND];
int wx[MWIND];
int wy[MWIND];
int wox[MWIND];
int woy[MWIND];
int wofrn[MWIND];
int wifrn[MWIND];
int wbase[MWIND];
int wmode[MWIND];
int wstate[MWIND];

char wrbuf[MWIND] [BUFSIZ];
int wcnt[MWIND];
int wptr[MWIND];
int wmore[MWIND];

#define MSNORM 0
#define MSESC 1
#define MSXWAIT 2
#define MSYWAIT 3

#define MRAW 1
#define MMORE 2
#define MMSCROLL 4
#define MECHO 8
#define MVSEND 16
#define WSCROLL 32
#define MOPENW 64
#define MWAIT 128

char *mdname[16] = {"RAW",
					"MORE",
					"ROLL",
					"ECHO",
					"VIRTUAL",
					"SCROLLING",
					"OPEN-WAIT",
					"MWAIT",
				};
int wlen[MWIND];

int talkf = 0;						/* 1 if we are xtalk */

int cwind,nwind;

int quote,squig;

int ic;
int rx,ry;

char ibuf[BUFSIZ];
char tibuf[BUFSIZ];

extern int SCRNLIN;
extern int SCRWID;
extern int mcol;
extern int mline;
extern int ttykill;
extern int ttyerase;
extern int ttyintr;
extern int errno;
extern char *getname();
extern char *homedir();
extern char *findtty();
extern char *getenv();

tdelay(num)

int num;
{
/* 	VMIN = 0  does not seem to work	
	

	struct termio td;
	
	ioctl(0,TCGETA, &td);
	td.c_cc[VMIN] = num;
	td.c_cc[VTIME] = 1;
	ioctl(0,TCSETA, &td);
*/
/*		THIS KLUDGE ALSO WORKS POORLY, so now do it with a user level timer 


	if (num == 0) fcntl(0,F_SETFL,O_RDWR+O_NDELAY);		
	else fcntl(0,F_SETFL,O_RDWR);
*/
}


alarming()
{
	return;
}

xclose(x)
int x;
{
	int i;
	
	for (i = x; i < 20; i++) close(i);
}

wput(i,cp,nc)
int i;
int nc;
char *cp;
{
	register int x;
	int maxln;
	
	maxln = wbase[i]+wlen[i];
	xgo(wx[i]+wbase[i],wy[i]);
	for (x = 0; x < nc; x++) {
		if (cp[x] == '\n') {
			clrl();
			mline++;
			mcol=0;
		} else xputc(cp[x]);
		if (mline>=maxln) {
			wroll(i);
		}
	}
}

/* wroll -- roll over window i */


wroll(i)
register int i;
{
	if ((wmode[i] & WSCROLL) || (vadjust(wbase[i],wbase[i]-1+wlen[i],-1)== 0)) {
		xgo(wbase[i],0);
		clrl();
	} else {
		mline--;
	}
}

wmfix(c)
register c;
{
	xgo(wx[c]+wbase[c],wy[c]);
	wroll(c);
	if (mline == wbase[c]) {
		wmore[c]  = wlen[c]-1;
	} else {
		wmore[c] = 0;
		wmode[c] |= MMSCROLL;
	}
	wmode[c] |= MWAIT;
	wmode[c] ^= MWAIT;
	wx[c]=mline-wbase[c];
	wy[c]=mcol;
}

main(argc,argv)

int argc;
char **argv;
{

	int c;
	char *cp,*tp;
	int status;
	int pipe1[2];
	int pipe2[2];
	int i;
	int x;
	ttystart();
	signal(SIGCLD,SIG_IGN);
	signal(SIGPIPE,SIG_IGN);
	tdelay(0);
	nwind = 1;
	wifrn[0]=0;
	wofrn[0]=0;
	wmode[0]=0;
	wx[0]=0;
	wy[0]=0;
	wbase[0]=0;
	wlen[0]=SCRNLIN-2;
	wmore[0] = wlen[0]-1;	
	
	if (**argv != 'w') {
		talkf = 1;					/* We are talk */
		talk(argv[1]);				/* start talking */
	}

	while (1) {		
		for (i = 0; i < nwind; i++) {
			
			if (wmode[i] & MOPENW) {

				wofrn[i] = open(wrbuf[i],1+O_NDELAY);
				if (wofrn[i] >= 0) {
					wmode[i] ^= MOPENW;
					prompt1("TALKING");
				} else {
					prompt1("WAITING");
				}
			} else if (wifrn[i]) {
				if (wcnt[i] == 0) {
					wcnt[i] = read(wifrn[i],wrbuf[i],BUFSIZ);
					wptr[i] = 0;
				}
				if (wcnt[i]) {
					x = wprot(i,wrbuf[i]+wptr[i],wcnt[i]);
					wptr[i] += wcnt[i]-x;
					wcnt[i] = x;
					if (i == cwind) {
						rx = wx[i]+wbase[i];
						ry = wy[i];
					}
				}
			}
		}
		xgo (SCRNLIN-2,0);
		for (i = 0; i < nwind; i++) {
			if (wmode[i]&MWAIT) {
				sputs("  MORE-");
				xputc(i+'0');
			}
		}
		clrl();
		if (squig) prompt1("~: ");
		else if (wmode[cwind] & MRAW) {
			mgo(wx[cwind]+wbase[cwind],wy[cwind]);
		} else {
			mgo (rx,ry);
		}
		fflush(stdout);		
		signal(SIGALRM,alarming);
		alarm(1);
		x = read(0,&c,1);
		alarm(0);
		if (x > 0) {
			if (squig) {
				squig = 0;
				unprompt();
				switch(c) {
				
				case '':
					abort();
				case '':
					rfrsh();
					break;
				case 'w':
					cwind++;
					if (cwind >= nwind) cwind = 0;
					if ((wmode[cwind] &MRAW) == 0) {
						rx = wx[cwind]+wbase[cwind];
						ry = wy[cwind];
						ic = 0;
					}
					break;
				
					
					
					
				case 't':
					cp = getname ("Talk to? ");
					if (cp) talk(cp);
					break;
				case 's':
					wsplit(cwind);
					break;

				case '?':
					wmode[cwind] |= MMORE;
					newin("/n1/warren/emacs/windows.help");
					break;
				case '<':
					cp = getname("Input from? ");
					newin(cp);
					break;
				case '>':
					if (wofrn[cwind]) close(wofrn[cwind]);
					tdelay(1);
					cp = getname("Output to? ");
					tdelay(0);
					wofrn[cwind] = open(cp,1+O_NDELAY);
					if (wofrn[cwind] < 0) prompt1("error code %d",errno);
					break;
				case '!':
					if (wifrn[cwind]) close(wifrn[cwind]);
					if (wofrn[cwind]) close(wofrn[cwind]);
					if (wproc[cwind]) {
						kill(wproc[cwind],9);
					};
					tdelay(1);
					cp = getname ("Command? ");
					tdelay(0);
					
					pipe(pipe1);
					pipe(pipe2);
					if ((wproc[cwind] = fork()) == 0) {
						close(0);
						dup(pipe1[0]);
						close(1);
						close(2);
						dup(pipe2[1]);
						dup(pipe2[1]);
						xclose(3);
						signal(SIGCLD,SIG_DFL);
						tp = getenv("TERM");
						*tp='v';
						tp[1]=0;
						execl("/bin/sh", "sh", "-c", cp , 0);
						exit(0);
					} else {
						close(pipe1[0]);
						close(pipe2[1]);
						wifrn[cwind] = pipe2[0];
						fcntl(pipe2[0],F_SETFL,O_RDONLY+O_NDELAY);
						wofrn[cwind] = pipe1[1];
					}
					break;
					
				case 'k':
					if (wproc[cwind]) kill(wproc[cwind],9);
					if (wofrn[cwind] ){
						close(wofrn[cwind]);
						wofrn[cwind] = 0;
					}
					if (wifrn[cwind]) {
						close(wifrn[cwind]);
						wifrn[cwind] = 0;
					}
					break;
				case 'v':
					wmode[cwind] ^= MVSEND;
					break;
				case 'd':
					xgo(SCRNLIN-1,0);
					xprintf("#%d,ifrn:%d,ofrn:%d,base:%d,len:%d,",
					cwind,wifrn[cwind],wofrn[cwind],wbase[cwind],wlen[cwind]);
					xprintf("pid:%o,mode:%o (",wproc[cwind],wmode[cwind]);
					for (x = 0; x < 16; x++) {
						if (wmode[cwind] & (1<<x)) {
							xprintf ("%s,",mdname[x]);
						}
					}
					xputc(')');
					clrl();
					break;
				case '0':
				case '1':
				case '2':
				case '3':
					c-='0';
					if (wmode[c]&MWAIT) {
						wmfix(c);
					}
					break;
				case 'm':
					wmode[cwind] ^= MMORE;
				case 'S':
					wmode[cwind] ^= WSCROLL;
					break;
				case 'r':
					wmode[cwind] ^= MRAW;
					break;
				case '~':
					goto defchar;
				case '.':
					for (i = 0; i < nwind; i++) {
						if (wproc[i]) kill (wproc[i],9); /* murder */
					}
					tdelay(1);
					die(0);			/* go away */
				}
			} else {
				
				if (c == '~') {
					squig++;
				} else {
defchar:					 /* implement modes here */

					if (wmode[cwind]&MWAIT) {
						wmfix(cwind);
					} else {
						if (wmode[cwind] & MMSCROLL) {
							wmore[cwind] = 0; /* input to this window */
						} else {
							wmore[cwind] = wlen[cwind]-1;
						}
					}
					if (wmode[cwind]&MRAW) {
						if (wofrn[cwind]) {
							write(wofrn[cwind],&c,1);
						}
					} else {
						if (quote)  goto deflt;
						if (c == ttyerase) {
							if (ic) ic--;
							if (wmode[cwind] & MVSEND) {
								wput(cwind,tibuf,ic);
								x = ry-mcol;
								if (x < 0) x=1;
								if (wofrn[cwind]) {
									write(wofrn[cwind],"",x);
									write(wofrn[cwind],"T",2);
								}
							}
						} else if (c == ttykill) {
							ic = 0;
							if (wofrn[cwind] && (wmode[cwind] & MVSEND)) {
								write(wofrn[cwind],"T",3);
							}
						} else if ((c == 015) ||(c == '\n') || (c == 4)) {
							if (c == '\r') c = '\n';
							tibuf[ic++] = c;
							wput(cwind,tibuf,ic);
							rx = mline;
							wx[cwind] = mline-wbase[cwind];
							ry = wy[cwind]=mcol;
							if (wofrn[cwind]) {
								if (wmode[cwind] & MVSEND) {
									write(wofrn[cwind],&c,1);
								} else {
									write(wofrn[cwind],tibuf,ic);
								}
							}
							ic = 0;
							if ((c == 4) && talkf) die(0); /* exit */
						} else if (c == '\\') {
							quote++;
						} else if (c == ttyintr) {
							if (wproc[cwind]) signal(wproc[cwind],SIGINT);
						} else {
deflt:						tibuf[ic++] = c;
							quote = 0;
							if ((wmode[cwind] & MVSEND) && wofrn[cwind]) {
								write(wofrn[cwind],&c,1);
							}
						}
						wput(cwind,tibuf,ic);
						rx = mline;
						ry = mcol;
						clrl();
					}
				}
			}
		} else {
/*			sleep(1);*/
		}
	}
}
wsplit(w)

int w;
{	
	int x;
	
	wbase[nwind] = wbase[w]+1+(wlen[w]/2);
	wlen[nwind] = wlen[w]-wbase[nwind]+wbase[w];
	wlen[w]=wbase[nwind]-wbase[w]-1;
	xgo(wbase[w]+wlen[w],0);
	for (x = 0; x < SCRWID; x++) {
		xputc('-');
	}
	wx[nwind]=wy[nwind]=wifrn[nwind]=wofrn[nwind]=0;

	if (wx[w] >= wlen[w]) wx[w] = 0;

	wmore[w] = wlen[w]-1;
	wmore[nwind]=wlen[nwind]-1;
	nwind++;
}


wprot(w,cp,n)
int w;
char *cp;
int n;
{
	char wbuf[10];
	
	int i;

	if (wmode[w] & MWAIT) return(n);
	
	for (i = 0; (i < n); i++) {
		switch(wstate[w]) {
			
		case MSNORM:
			switch(cp[i]) {
				
			case '':
				if (wy[w]) wy[w]--;
				break;
			case '':
				if (wx[w]) wx[w]--;
				break;
			case '':
				wy[w]++;
				break;
			case '':
				wy[w]=0;
				break;
			case '':
				xgo(wx[w]+wbase[w],wy[w]);
				sputs("EOF");
				if (talkf) die(0);
				break;
			case '':
			case '\n':
				xgo(wx[w]+wbase[w],wy[w]);
				if (cp[i] == '\n'){
					clrl();
					mcol=0;
				}
				mline++;
				if ((cp[i] == '\n') && (mline <wlen[w]+wbase[w])) clrl();
				goto wscroll;
			case '':
				beep();
				break;
			case '':
				wx[w]=wy[w]=0;
				break;
			case '':
				wstate[w]=MSESC;
				break;
			case '':
				break;
			default:
				xgo(wx[w]+wbase[w],wy[w]);
				xputc(cp[i]);
wscroll:		if (mline>=wbase[w]+wlen[w]) {
					if((wmode[w]&MMORE)&& (++wmore[w] >= (wlen[w]))) {
						wmode[w] |= MWAIT;
						wx[w]=mline-wbase[w];
						wy[w]=mcol;
						return(n-i);
					}
					wroll(w);
				}
				wx[w]=mline-wbase[w];
				wy[w]=mcol;
			}
			break;
		case MSESC:
			switch(cp[i]) {
			
			case 'Y':
				while (wx[w] < wlen[w]) {
					xgo(wx[w]+wbase[w],0);
					clrl();
					wx[w]++;
				}
				wx[w]=wy[w]=0;
				break;
			case 'T':
				xgo(wx[w]+wbase[w],wy[w]);
				clrl();
				break;
			case '?':
				if (wofrn[w]) {
					sprintf(wbuf,">%c%c",wlen[w]+' ',SCRWID+' ');
					write(wofrn[w],wbuf,4);
				}
				break;
			case '=':
				wstate[w] = MSXWAIT;
				break;
			}
			if (wstate[w] == MSESC) wstate[w] = MSNORM;
			break;
		case MSXWAIT:
			wx[w] = cp[i]-040;
			if (wx[w] >= wlen[w]) wx[w]=0;
			wstate[w]= MSYWAIT;
			break;
		case MSYWAIT:
			wy[w] = cp[i]-040;
			wstate[w] = MSNORM;
		}
	}
	return(0);
}

/* TALK stuff */

char	desttty[20] = "/dev/";

alert(tty)
	char *tty;
{
	
	/* Alerts the destination tty of an incoming "talk" */
	FILE *ttyfile;

	strcat(desttty, tty);
	if ((ttyfile = fopen(desttty, "w")) == NULL) {
		return(0);
	}

	fprintf(ttyfile, "%cTALK2 FROM %s (%s)...\n%c",
			7, getenv("LOGNAME"), getenv("LOGTTY"), 7);
	fclose(ttyfile);
	return(1);
}


talk(cp)
char *cp;
{
	char infifo[100];
	char outfifo[100];
	char *ttyp;
	int owind;

	
	owind = nwind;
	wsplit(cwind);			/* two windows */
	wmode[cwind] = MVSEND;
	wmode[owind] = MVSEND+MOPENW;
	
/* Find other user's terminal */

	ttyp = findtty(cp);
	if (*ttyp == NULL) {
		prompt1("talk: user %s not logged on.\n", cp);
		return;
	}

	/* Check for existence of .talk fifos */

	sprintf(infifo, "%s/.talk", getenv("HOME"));
	sprintf(wrbuf[owind], "%s/.talk", homedir(cp));

	if (access(infifo, 04) != 0) {
		prompt1("Type mknod $HOME/.talk p and try again");
		return;
	}
	wifrn[cwind] = open(infifo,0+O_NDELAY);

	/* alert the destination terminal */

	if (!alert(ttyp)) {
		prompt1("talk: cant alert tty: /dev/%s.\n", ttyp);
	}
	cwind = owind;
	rx = wbase[cwind];
	ry = 0;
}
newin(cp)

register char *cp;
{
	if (cp) {
		if (wifrn[cwind]) close(wifrn[cwind]);
					
		wifrn[cwind] = open(cp,0+O_NDELAY);
		if (wifrn[cwind] < 0) prompt1("error code %d",errno);
	}
}
