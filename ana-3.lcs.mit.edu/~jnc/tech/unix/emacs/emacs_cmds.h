/* word and sentence stuff */

/* EMACS_MODES: c !fill */

#define WRDSEP 01			/* separates words */
#define WRDCHR 02			/* makes words */
#define WHITE 04			/* white space */
#define SENTE 010			/* ends sentences */

char bits[128] = {
	05,	01,	01,	01,	01,	01,	01,	01,
	01,	05,	05,	01,	01,	05,	01,	01,
	01,	01,	01,	01,	01,	01,	01,	01,
	01,	01,	01,	01,	01,	01,	01,	01,
	05,	011,	01,	01,	01,	01,	01,	03,
	01,	01,	01,	01,	01,	01,	011,	01,
	02,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	011,	01,	01,	01,	01,	011,
	03,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	02,	01,	01,	01,	01,	01,
	02,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	02,	02,	02,	02,	02,	02,
	02,	02,	02,	01,	01,	01,	02,	01,
};

char casem[128];


/* mark stuff */

#define MARK struct markst

struct markst {
	int markl;
	int markc;
};

#define NMARKS 16
struct markst marks[NMARKS];

/* modes */

int FILLMD = 1;
int FILLCOL = 72;
int SAVEMD = 0;
int SAVECHAR = 256;
int NSCHAR = 0;
int NLINS = 0;
int NOCASE = 0;				/* caseless search */
int CKMAIL = 100;			/* check mail interval */
int NLRUN = 0;
int USILENT = 0;			/* echo unix commands */

/* C mode stuff */

int comcol = 40;
int comln = 0;
int TABMD = 0;
int BACKP = 0;

/* mode edit data */

struct mdata {
	char *modename;
	int *modeloc;
	char modetype;
	char moderset;
};

#define NMODES 26
#define ONOFF 1
#define INT 2

/* parameters that describe what should be reset following mode display */

#define DISPLAY 1			/* re-display */
#define DSIZE 2				/* re-format display */
#define CSE 4				/* re-compute case map */

extern int LNOWID;
extern int LNOMOD;
extern int TABSTOP;
extern int NOBEL;
extern int keepg;
extern int kbapp;
extern int ttywarp;
extern int NOTABS;
extern int ctlify;
extern char mailbox[128];
extern char home[128];
int timemd = 0;
int VERBOSE = 1;
int OVERW = 0;
int READONLY = 0;

struct mdata mdata[NMODES] = {
	"save",&SAVEMD,ONOFF,0,
	"savetype",&SAVECHAR,INT,0,
	"fill",&FILLMD,ONOFF,0,
	"lnumb",&LNOMOD,ONOFF,DISPLAY,
	"c",&TABMD,ONOFF,0,
	"tabstop",&TABSTOP,INT,DISPLAY,
	"comcol",&comcol,INT,0,
	"fillcol",&FILLCOL,INT,0,
	"height",&SCRLINES,INT,DISPLAY+DSIZE,
	"width", &SCRWID,INT,DISPLAY+DSIZE,
	"backspace",&BACKP,ONOFF,DISPLAY,
	"time",&timemd,ONOFF,0,
	"verbose",&VERBOSE,ONOFF,0,
	"overwrite",&OVERW,ONOFF,0,
	"nobell", &NOBEL, ONOFF,0,
	"lnowid", &LNOWID, INT,DISPLAY,
	"keepscroll", &keepg, INT,DISPLAY,
	"rigid_newline", &NLINS, ONOFF, 0,
	"caseless", &NOCASE, ONOFF, CSE,
	"mailtype",&CKMAIL, INT, 0,
	"end_newline",&NLRUN, ONOFF, 0,
	"tspeed",&ttywarp, INT, 0,
	"usilent",&USILENT, ONOFF, 0,
	"readonly",&READONLY, ONOFF, 0,
	"notabs",&NOTABS, ONOFF, DISPLAY,
	"controlify",&ctlify,ONOFF,0,
};

/* macro string pointer table */

int nmac = 0;				/* number of defined macros */

#define NMAC 100			/* number of macros allowed */

int macname[NMAC];

/* saved search string */

char presst[128];

/* function defns */

MARK *markptr();
extern int junked;
extern int numarg;
char *strcpy();
char *expenv();
char *getenv();
extern FILE *fdopen();
extern int loc2;
extern char *endput;
extern int macptr;
extern int fbkno;
extern int *do_it[];
extern char helpc[];
extern char xhelp[];
extern char *inget();
extern char *nscan();
extern char fnbuf[];
extern int numarg;
extern int curbf;
extern int nln;
extern int minln;
extern int maxln;
