#include <stdio.h>

/* EMACS_MODES: c, !fill */

/* terminal type tester.  exercises terminal capabilities to dest
 * their definition and use. */

extern char *UP;					/* cursor up line */
extern char *DOWN;
extern char *BACK;
extern char *FORWARD;
extern char *HOME;
extern char *CLEAR;
extern char *CLREST;
extern char *CLINE;
extern char *BELL;
extern char *CURAD;
extern char *TMAP;
extern char *NOP;
extern char *LOPEN;
extern char *LDEL;
extern char *INSERTC;
extern char *INSERTM;
extern char *OSERTC;
extern char *INSERTP;
extern char *DELC;
extern char *SSCROLL;
extern char *RSCROLL;
extern char *CR;
extern char *SCREG;
extern char *ULINE;
extern int EOVER;
extern char *SCINIT;
extern char *VEXIT;
extern int XBASE;
extern int YBASE;
extern int SCRWID;
extern int SCRNLIN;
extern int SCRWRAP;
extern int VCOST;
extern int SRCADD;
extern int MI;
extern int IN;
extern int DELMODE;
extern char *RELUP;
extern char *RELDOWN;
extern char *RELFORW;
extern char *RELBACK;

extern int scrlin,scrcol;

pause()
{
	mgetchar();
}

main()
{
	int i;
	
	ttystart();			/* start terminal processing */
	
/* Clear screen test */
	
	xprintf ("\n\n       Clear Screen Test");
	pause();
	clear();
	xprintf (
		"*\n Screen was cleared, astrick @ upper left of screen");
	pause();
	
/* Cursor addressing: */

	clear();

	if (CURAD) {
		for (i = 0; i <= SCRWID;  i++) {
			scrlin = -100;		/* make sure we do cursor movement */
			mgo(10,i);
			putchar((i&07)+'0');
		}
		mgo(12,10);
		xprintf ("Above line should count accross the screen 0-7");
		pause();
		clear();
		for (i = 0; i < SCRNLIN; i++) {
			scrlin = -100;
			mgo(i,SCRWID/2);
			xprintf ("%d",i);
		}
		mgo(0,0);
		xprintf ("Screen should");
		mgo(1,0);
		xprintf ("have a line");
		mgo(2,0);
		xprintf ("counting down");
		mgo(3,0);
		xprintf ("the middle");
		pause();
		clear();
	}
	if (!CURAD) {
		switch ((RELUP!=0)+
			(RELDOWN!=0)+
			(RELFORW!=0)+
			(RELBACK!=0)) {
				
		default:
			xprintf("ru, rd, rr and rl must all be defined");
			pause();
			clear();
			die(0);

		case 4 : ;
		/* Need some relative addressing tests here */

		case 0: ;
		}
	}
	if ((!CURAD)&&(!RELUP)&&((!UP)||(!DOWN)||((!CR)&&(!BACK)))) {
		xprintf(
		"Must have cursor addressing, or up, do & (cr or bc)");
		pause();
		clear();
		die(0);
	}
/* RELATIVE SCREEN MOTION */
	
	if (BACK) {
		mgo(5,0);
		xprintf ("Drawing backwards from 10,20 to 10,0");
		mgo(10,20);
		for (i = 0; i < 20; i++) {
			putchar('x');
			eprintf(BACK);
			eprintf(BACK);
		}
		putchar('x');
		eprintf(BACK);
		pause();
		clear();
	}
	if (FORWARD) {
		mgo(5,0);
		xprintf ("Drawing Forwards from 10,0 to 10,20");
		if (BACK==0) {
			mgo(6,0);
			xprintf("Skipping everyother character");
		}
		mgo(10,0);
		for (i = 0; i < 20; i++) {
			putchar('x');
			if (BACK) eprintf(BACK);
			else i++;
			eprintf(FORWARD);
		}
		putchar('x');
		eprintf(BACK);
		pause();
		clear();
	}
	if (DOWN) {
		mgo(0,5);
		if (BACK) xprintf ("Drawing down from 1,20 to 21,20");
		else xprintf("Drawing down diagonal from 1,20 to 21,40");
		mgo(1,20);
		for (i = 0; i < 20; i++) {
			putchar('x');
			eprintf(BACK);
			eprintf(DOWN);
		}
		putchar('x');
		eprintf(BACK);
		pause();
		clear();
	}
	if (UP) {
		mgo(0,5);
		if (BACK) xprintf ("Drawing UP from 21,20 to 1,20");
		else xprintf("Drawing up diagonal from 21,20 to 1,40");
		mgo(21,20);
		for (i = 0; i < 20; i++) {
			putchar('x');
			eprintf(BACK);
			eprintf(UP);
		}
		putchar('x');
		eprintf(BACK);
		pause();
		clear();
	}
/* Carriage Return  */
	
	if (CR) {
		mgo(10,25);
		xprintf ("Carriage");
		mgo(10,0);
		xprintf ("Return");
		pause();
		clear();
	}
	
/* HOME */
	
	if (HOME) {
		mgo(10,25);
		xprintf ("Going");
		eprintf (HOME);
		printf("Home");
		pause();
		clear();
	}
	
/* Wrapping */
	
	if (SCRWRAP) {
		mgo(5,SCRWID-6);
		printf("cursor wraps");
		if (CURAD) {
			scrlin = -100;
			mgo(7,0);
		} else {
			printf("\r\n"); /* PUNT for terminal with no CM */
		}
		printf("wraps under wraps");
		pause();
		clear();
	}
	
	if (CLINE) {
		mgo(10,0);
		xprintf ("clearing everything after clear");
		mgo(11,0);
		xprintf ("clearing everything after clear");
		mgo(10,5);
		clrl();
		pause();
		clear();
	}
/* BELL */
	
	if (BELL) {
		xprintf ("beep beep beep");
		beep();
		sdelay(1000);
		beep();
		sdelay(1000);
		beep();
		clear();
	}
	if (SSCROLL){
		mgo(SCRNLIN-1,0);
		printf ("scrolling");
		eprintf(SSCROLL);
		mgo(SCRNLIN-1,0);
		printf("up");
		eprintf(SSCROLL);
		mgo(SCRNLIN-1,0);
		printf("the");
		eprintf(SSCROLL);
		mgo(SCRNLIN-1,0);
		printf("screen");
		pause();
		clear();
	}
	if (INSERTC||INSERTM) { 
		
		mgo(10,0);
		xprintf("Testing insert character.");
		if (IN) {
			xprintf ("		These won't move unless pushed\n");
			xprintf("These move when you push them\n");
		}
		else xprintf ("		These move too\n");
		xprintf("  all characters except ^G insert after test");
		mgo(10,4);
		if (INSERTM) {
			eprintf(INSERTM);
		}
		while ((i = mgetchar()) != '') {
			if (INSERTC) eprintf(INSERTC);
			putchar(i);
			if (INSERTP) eprintf(INSERTP);
		}
		if (OSERTC) eprintf(OSERTC);
		clear();
	}
	if (DELC) { 
		mgo(10,0);
		xprintf("Testing delete character.");
		if (IN) {
			xprintf(" All of these characters, including the wrap to the new line, should move");
			xprintf ("		These won't move\n");
		}
		else xprintf ("		These move too\n");
		xprintf("  all characters except ^G delete after test");
		mgo(10,4);
		if (DELMODE) {
			eprintf(INSERTM);
		}
		while ((i = mgetchar()) != '') {
			eprintf(DELC);
		}
		if (DELMODE) eprintf(OSERTC);
		clear();
	}
	if ((SCREG)&&((!SSCROLL)||(!RSCROLL)||(!CURAD))) {
		mgo(1,1);
		xprintf("Region scrolling requires sf, sr and cm");
		pause();
		clear();
		goto skipscroll;
	}
	if (LOPEN) {
		for (i = 0; i < SCRNLIN; i++) {
			mgo(i,0);
			xprintf ("Line %d",i);
		}
		mgo(0,10);
		xprintf("u to scroll lines %d through %d up",2,SCRNLIN-2);
		mgo(1,10);
		xprintf("d to scroll down");
again:		i = mgetchar();
		if (i == 'u') {
			vadjust(2,SCRNLIN-2,-1);
			goto again;
		}
		if (i == 'd') {
			vadjust(2,SCRNLIN-2,1);
			goto again;
		}
		clear();
	}
skipscroll:
	if (ULINE) {
		
		if (EOVER) {
			mgo(4,0);
			uprint("Underlined_Characters");
			mgo(6,0);
			uprint("These won't wind up underlined");
			mgo(6,0);
			xprintf("These won't wind up underlined");
		} else {
			mgo(6,0);
			EOVER = 1;
			uprint("...............................");
			EOVER = 0;
			mgo(6,0);
			xprintf("These characters are underlined");
		}
		pause();
		clear();
	}
	die(0);
		
}

uprint(sp)
register char *sp;
{
	int lie = 0;
	if (!EOVER) {
		/* terminal does not overwrite underscores! */
		/* What a mess, tell handler it will */
		lie = 1;
		EOVER = 0;
	}
	while (*sp) {
			xputc('_');
			xputc('\b');
			xputc(*sp++);
	}
	if (lie) EOVER=1;
}
