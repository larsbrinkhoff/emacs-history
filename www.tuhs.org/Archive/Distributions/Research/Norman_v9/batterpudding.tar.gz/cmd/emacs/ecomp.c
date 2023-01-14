#include <stdio.h>
#ifdef PC
#include "../pcompile/adapt.h"
#define setbin(file) _isbin[file->_file] = 1;
#undef getchar
#undef putchar
#define getchar()	fgetc(stdin)
#define putchar(x)	fputc((x), stdout)
#endif
extern char *getenv();

#define CTRLX 030
#define CMENT 034
#define META(x) (x+0200)
#define CTRL(x) (x^0100)

#define BAD 0
#define SIMPLE 1
#define SDCL 2
#define CASE 3
#define COND 4
#define BEGIN 5
#define WHILE 6
#define PSTRING 7
#define NUMBER 8
#define BINARY 9
#define UNARY 10
#define QUOTE 11
#define INSERT 12
#define XSTRING 13
#define DCL 14
#define LOCAL 15
#define CHAR 16
#define MAP 17
#define GLOBAL 18
#define SGLOBAL 19
#define SVAL 256 			/* Returns string value */
#define DOUBLE 512					/* Command is really 2 base commands */

#ifndef PC
char *defdir = SDIR;
#define DEFILE	"emacs_defs"
#endif

int line = 1;
int DEBUG = 0;
char *typetable[] = {
	"BAD","SIMPLE","SDCL","CASE","COND","BEGIN","WHILE",
	"SSTRING","NUMBER","BINARY","UNARY","QUOTE","INSERT",
	"STRING","DCL","LOCAL","CHAR","MAP","GLOBAL","SGLOBAL",NULL,
};

#define NHOOKS 10
char *hooks[NHOOKS] = {
	"No_Hook",
	"Pre_Read_Hook",
	"Post_Read_Hook",
	"Pre_Write_Hook",
	"Load_Macro_Hook",
	"Read_Line_Hook",
	"Mode_Line_Hook",
	"Exit_Emacs_Hook",
	"Leave_Buffer_Hook",
	"Enter_Buffer_Hook",
};

extern char *malloc();
struct defblk {
	struct defblk *next;
	char *name;
	int type;
	char *body;
};

/* Definitions for expression contexts */

#define CARG 1 				/* function argument */
#define CSINGLE 2 			/* Must produce single command */
#define CSTRING 4			/* Argument to string type function */

#define CCONT 8				/* Must generate pass-last-result after command */
#define CCLOSE 16			/* Must generate a closing brace */

#define NLOCAL 10
char *locals[NLOCAL];
int nlocal=NLOCAL;

#define NHASH 256
struct defblk *hashtable[NHASH];

char symbuf[128];

char *
mstrcpy(cp,cp1)

/* Keywords: assignment string-handling */
register char *cp;
register char *cp1;
{
	while (*cp++ = *cp1++);
	return(cp-1);
}

wrdchr(c)
int c;
{
	if ((c>='a') && (c <= 'z')) return(1);
	if ((c>='A') && (c <= 'Z')) return(1);
	if ((c>='0') && (c <= '9')) return(1);
	return(0);
}

char *
expenv(str)
register char *str;
/* Keywords: environment-variables unix-interface user-interface:20 shell-escape:10 */
{
	char strtemp[128];
	char vartemp [64];
	register char *cp1;
	char *cp2;
	register int c;
	int oc;
	
	if (str == NULL) return(NULL);
		
	cp1 = strtemp;
	cp2 = str;
	while (*cp1++ = *str) {
		if ((*str== '`')||(*str=='*')||(*str=='{')||(*str=='[')||((*str++)=='?')) {
			return("Error");
		}
	}
	cp1 = strtemp;
	str = symbuf;			/* always copy back into file name */
	while (c = *cp1++) {
		if ((c == '$')|| (c == '~')) {

/* Environment variable */
			
			oc = c;
			cp2 = vartemp;
			while (wrdchr(c=((*cp1++)&0377))) {
				*cp2++ = c;
			}
			cp1--;		/* backspace pointer */
			*cp2 = 0;
			if (oc == '$') {
				cp2 = getenv(vartemp); /* environment variable */
			} else {
/* Home Directory */
			
				if (*vartemp == 0) {
					cp2 = getenv("HOME"); /* Bare ~ means home */
				} else if ((strcmp(vartemp,"exptools")==0) &&
					(cp2 = getenv("TOOLS")) && *cp2) {
					;
				} else {
					return("Error"); /* Can't do it */
					
				}
			}
			if (cp2 != NULL) {	
				str = mstrcpy(str,cp2);
			} else {
				*str++ = oc;
				str = mstrcpy(str,vartemp);
			}
		} else {
			*str++ = c;
		}
	}
	*str++ = 0;
	return(symbuf);
}
char macb[128];
char *
macbody(name)
char *name;
{
	char *bp;
	bp = macb;
	*bp++ = META('x');
	while (*name) *bp++ = *name++;
	*bp++ = '\n';
	*bp++ = 0;
	return(macb);
}
struct defblk *
getname(name)

char *name;
{
	int hash;
	char *np;
	struct defblk *defp;

	np = name;
	hash = 0;
	while (*np) hash += *np++;
	hash = hash %NHASH;
	defp = hashtable[hash];
	while (defp && strcmp(name,defp->name)) defp = defp->next;
	if (defp == NULL) {
		defp = ((struct defblk *) malloc(sizeof(*defp)));
		defp->next = hashtable[hash];
		hashtable[hash] = defp;
		defp->name = malloc(strlen(name)+1);
		strcpy(defp->name,name);
		if ((*name == '-') || ((*name>='0') && (*name <= '9'))) {
			defp->type = NUMBER;
			defp->body = malloc(strlen(name)+1);
			defp->body[0] = defp->name[0]+0200;
			strcpy(defp->body+1,defp->name+1);
		} else if ((*name == '\'')  && (name[2] == '\'') && (name[3] == 0)) {
			defp->type = SIMPLE;
			defp->body = malloc(3);
			defp->body[0] = META(CTRL('Q'));
			defp->body[1] = name[1];
			defp->body[2] = 0;
		} else {
			fprintf(stderr,"Undefined command name %s at line %d, assumed external\n",name,line);
			defp->type = SIMPLE;
			name = macbody(name);
			defp->body = malloc(strlen(name));
			strcpy(defp->body,name);
		}
	}
	return(defp);
}

lookhook(name)
char *name;
{
	register int i;
	for (i = 1; i < NHOOKS; i++) if(strcmp(name,hooks[i]) == 0) return(i);
	return(0);
}
undefine(name)

char *name;
{
	int hash;
	char *np;
	struct defblk *defp;
	struct defblk  *odefp;
	np = name;
	hash = 0;
	while (*np) hash += *np++;
	hash = hash %NHASH;
	defp = hashtable[hash];
	odefp = ((struct defblk *) &hashtable[hash]);
	while (defp && strcmp(name,defp->name)) {
		odefp = defp;
		defp = defp->next;
	}
	if (defp) odefp->next = defp->next;
	else {
		fprintf(stderr,"Internal error undefining symbol %s\n",name);
	}
}
define(name,type,body)

char *name;
char *body;
int type;
{
	int hash;
	char *np;
	struct defblk *defp;

	np = name;
	hash = 0;
	while (*np) hash += *np++;
	hash = hash %NHASH;
	defp = hashtable[hash];
	while (defp && strcmp(name,defp->name)) defp = defp->next;
	if (defp == NULL) {
		defp = ((struct defblk *) malloc(sizeof(*defp)));
		defp->next = hashtable[hash];
		hashtable[hash] = defp;
		defp->name = malloc(strlen(name)+1);
		strcpy(defp->name,name);
	}
	defp->type = type;
	defp->body = malloc(strlen(body)+1);
	strcpy(defp->body,body);
}

definit()
{
	int i;
	for (i = 0; i < NHASH; i++) {
		hashtable[i] = NULL;
	}
}


char *
symbol()
{
	char *sp;
	int c;

	sp = symbuf;
	c = nonblank(1);
	ungetc(c,stdin);
	while (1) {
		c = gochar();
		if ((c == EOF) || (c == ' ')|| (c == '	')|| (c == ')') ||
			(c == '(') || (c == '\n')) break;
		*sp++ = c;
	}
	ungetc(c,stdin);
	if (c == '\n') line--;		/* Uncount newline */
	*sp = 0;
	return(symbuf);
}

read_defs()
{
	FILE *fp;
	char name[128];
	int type;
	char body[128];
	char *cp;
	int c;
	
#ifdef PC
	fp = fopen ("edefs.dat","r");
	if (fp == NULL) fp = fopen ("a:edefs.dat","r");
	if (fp == NULL) fp = fopen ("b:edefs.dat","r");
	if (fp == NULL) fp = fopen ("c:edefs.dat","r");	
	if (fp == NULL) {
		printf ("Can't find definitions file edefs.dat\n");
		exit(0);
	}
	setbin(fp);
#else	
	cp = expenv(defdir);
	sprintf(name,"%s/%s",cp,DEFILE);
	fp = fopen (name,"r");
	if (fp == NULL) {
		fprintf(stderr,"Can't open definitions file: %s\n",name);
		fprintf(stderr,"Please contact your local emacs maintainer\n");
		exit(-1);
	}
#endif
	while ((c = fgetc(fp)) != EOF) {
		if (c != '(') fprintf(stderr,"Internal error, bad def file format %c\n",c);
		symbin(fp,name);
		symbin(fp,body);
		type = gtype(body);
		if ((type&0377) == BAD) fprintf(stderr,"Internal error, Bad type %s for symbol %s in defs file\n",body,name);
		symbin(fp,body);
		while ((c = fgetc(fp)) != '\n');
		define(name,type,body);
	}
	fclose(fp);
}

gtype(name)

/* Returns type of name is a type definition, 0 otherwise */

char *name;
{
	int c;
	int type;
	
	type = BAD;
	if (*name == '$') {
		type |= SVAL;
		name++;
	}
	c = 0;
	while (typetable[c]) if (strcmp(typetable[c],name) == 0) {
		type |= c;
		break;
	} else c++;
	return(type);
}

symbin(fp,xp)
FILE *fp;
char *xp;
{
	int c;
	do {
		c = fgetc(fp);
	} while ((c == ' ') || (c == '\n'));
	ungetc(c,fp);
	while (1) {
		c = fgetc(fp);
		if ((c == EOF) || (c == ' ') || (c == ')') ||
			(c == '(') || (c == '\n')) break;
		if (c == '\\') {
			c = fgetc(fp)-'0';
			c = c * 8 + (fgetc(fp)-'0');
			c = c * 8 + (fgetc(fp)-'0');
		}
		*xp++ = c;
	}
	ungetc(c,fp);
	*xp = 0;
}

		
main(argc, argv)

int argc;
char *argv [];

{
	int c;
	
	if (argc>1) {
		char buf[256];
		int x;
		strcpy(buf,argv[1]);
		x = strlen(buf);
		if ((buf[x-2] != '.') || (buf[x-1] != 'e')) {
			buf[x++]= '.';
			buf[x++] = 'e';
			buf[x]=0;
		}
		if (freopen(buf,"r",stdin) == NULL) {
			fprintf(stderr,"Can't open input file %s\n",buf);
			exit(-1);
		}
		buf[x-2]=0;
		if (freopen(buf,"w",stdout) == NULL) {
			fprintf(stderr,"Can't open output file %s\n",buf);
			exit(-1);
		}
#ifdef PC
		setbin(stdout);
#endif
	}
	definit();
	read_defs();
	c = getchar();
	if (c == '#') {
		DEBUG=1;
	} else {
		ungetc(c,stdin);
	}
	while ((c = nonblank(0)) != EOF) {
		if (c == '(' ) function();
	}
}


char *
glob(name,body,arg)
char *name;
char *body;
int arg;
{
	char *bp;
	bp = macb;
	*bp++ = CTRL('X');
	*bp++ = '<';
	while (*name) *bp++ = *name++;
	*bp++ = '\n';
	*bp++ = arg;
	while (*body) *bp++ = *body++;
	*bp=0;
	return(macb);
}


function()
{
	char *name;
	
	int c;
	int type;
	int nobind;
	
	c = nonblank(0);
	if (c == '(') {
		c = gochar();
		while (c != ')') {
			if (c == EOF) {
				fprintf(stderr,"Error, macro binding sequence does not terminate\n");
				return;
			}
			putchar(c);
			c = gochar();
			nobind = 0;
		}
	} else {
		ungetc(c,stdin);
		nobind=1;
	}

	name = symbol();
	if (type=gtype(name)) {		/* Name is a symbol declaration */
		name = symbol();	/* Now get real symbol */
	} else type = SIMPLE;		/* Defaults to simple macro */
	if (nobind) {
		nobind = lookhook(name);
		putchar(CTRL('Z'));
		putchar(nobind);
	}
	putchar (CMENT);			/* ^/ */
	PUTS(name);
	define(name,type,macbody(name));
	putchar (' ');
	c = nonblank(0);
	if (c != '(') fprintf(stderr,"Bad syntax for macro definition at line %d\n",line);
	while ((c = getchar()) != ')') {
		if (c == EOF) break;
		putchar(c);
		if (c == '\n') {
			putchar(CMENT);
			line++;
		}
	}
	putchar('\n');
	parseform(0);
	putchar (CTRL('Z'));
	putchar('\n');
	
	while (nlocal < NLOCAL) {
		undefine(locals[nlocal]);
		locals[nlocal] [strlen(locals[nlocal])-1] = 0;
		undefine(locals[nlocal]);
		nlocal++;
	}
}
parseform(flags)
int flags;
{
	int c;
	
	if (DEBUG) fprintf(stderr,"parseform\n");
	/* Now parse the form */
	while ((c = nonblank(1)) != ')') {
		if (c == EOF) {
																		/* ARGH!! unterminated form */
			fprintf(stderr,"Unterminated form at line %d\n",line);
			return;
		}
		if (parsememb(c,flags)&CCLOSE) fprintf(stderr,"Internal error in parsememb at line %d\n",line); 
		flags = 0;
	}
}
parsememb(c,context)
int c;
int context;
{
	char *oname;
	char *name;
	struct defblk *defp;	
	int retflags,retval;

	retflags = 0;	
	if (c == ')') {
		ungetc(c,stdin); /* handle users typing '(foo)'  */
		return(0);
	}
	if (c == '(') {
		name = symbol();
		defp = getname(name);
		if (DEBUG) fprintf(stderr,"parsememb complex %s type %s context %d\n",name,typetable[(defp->type&0377)],context);
		if (defp->type & SVAL) retflags |= CSTRING;
		if ((defp->type & DOUBLE) && (context & CSINGLE)) {
			putchar(META('{'));
			retflags |= CCLOSE;
			context^= CSINGLE;
		}
		if (context & CARG) {
			if ((defp->type & SVAL) == 0) retflags |= CCONT;
			if (context&CSINGLE)   {
				putchar(META('{'));
				retflags |= CCLOSE;
				context ^= CSINGLE;
			}
		}
		switch(defp->type&0377) {

		case GLOBAL:
			name = symbol();
			define (name,SIMPLE+DOUBLE,glob(name,defp->body,META('1')));
			c = strlen(name);
			oname = glob(name,defp->body,META('2'));
			name[c]='=';
			name[c+1] = 0;
			define (name,UNARY+DOUBLE,oname);
			closep(defp->name);
			break;
		case SGLOBAL:
		{
			char sbuf[128];
			
			name = symbol();
			sprintf(sbuf,"%c<%s\n%s",CTRL('X'),name,defp->body);
			define (name,SIMPLE+DOUBLE+SVAL,sbuf);
			
			sprintf(sbuf,"%s=",defp->name);
			defp = getname(sbuf); /* Look up giberish for def */
			sprintf(sbuf,"%c<%s\n%s",CTRL('X'),name,defp->body);
			c = strlen(name);
			name[c]='=';
			name[c+1] = 0;
			define (name,XSTRING+DOUBLE,sbuf);
			closep(defp->name);
			}
			break;

		case DCL:
			name = symbol();
			if (c = gtype(name)) {
				name = symbol();
			} else c = SIMPLE;
			define(name,c,macbody(name));
			closep(name);
			break;
		case LOCAL:
			name = symbol();
			if (--nlocal <= 1) {
				fprintf (stderr,"Too many local declarations, symbol %s ignored at line %d\n",name,line);
				++nlocal;
			} else {
				char bod[4];
				int x;
				
				bod[0] = META('0')+nlocal;
				bod[1] = CTRL(']');
				bod[2] = 0;
				
				define(name,NUMBER,bod);
				x = strlen(name);
				name[x]='=';
				name[x+1] = 0;
				bod[1] = META(CTRL(']'));
				define(name,UNARY,bod);
				defp = getname(name);
				locals[nlocal] = defp->name;  
			}
			closep(name);
			break;
		case SDCL:
			name = symbol();
			if (--nlocal <= 1) {
				fprintf (stderr,"Too many local declarations, symbol %s ignored at line %d\n",name,line);
				++nlocal;
			} else {
				char bod[10];
				int x;
				
				bod[0] = META('1');
				bod[1] = '2';
				bod[2] = CTRL('X');
				bod[3] = '&';
				bod[4] = META('0')+nlocal;
				bod[5] = CTRL(']');
				bod[6] = CTRL('Z');
				bod[7] = 0;
				define(name,SIMPLE+SVAL,bod);
				x = strlen(name);
				name[x]='=';
				name[x+1] = 0;
				bod[0] = META('0')+nlocal;
				bod[1] = META(CTRL(']'));
				bod[2] = META('1');
				bod[3] = '1';
				bod[4] = CTRL('X');
				bod[5] = '&';
				bod[6] = 0;
				define(name,XSTRING,bod);
				defp = getname(name);
				locals[nlocal] = defp->name;  
			}
			closep(name);
			break;
		case SIMPLE:
			{
				char nbuf[128];
				
				strcpy(nbuf,name);
				c = nonblank(1);
				retflags |= parsememb(c,CARG|(context&CSINGLE));
				PUTS(defp->body);
				closep(nbuf);
			}
			break;
		case NUMBER:
			PUTS (defp->body);
			putchar(CTRL('Z'));
			closep(name);
			break;
		case QUOTE:
			PUTS(defp->body);
			putchar(nonblank(1));
			closep(name);
			break;
		case BINARY:
			PUTS(defp->body);
			c = nonblank(1);
			parsememb(c,CSINGLE);
			c = nonblank(1);
			parsememb(c,CSINGLE);
			closep(name);
			break;
		case UNARY:
			PUTS(defp->body);
			c = nonblank(1);
			parsememb(c,CSINGLE);
			closep(name);
			break;
		case BEGIN:
			putchar (META('{'));
			parseform(0);
			putchar (META('}'));
			break;
		case WHILE:
			putchar (CTRLX);
			putchar ('^');
			putchar (META('{'));
			parseform(CSINGLE);
			putchar (META('}'));
			break;
		case CASE:
			putchar (CTRLX);
			putchar ('!');
			putchar (META('{'));
			c = nonblank(1);
			parsememb(c,CSINGLE);
			while (1) {
				c = nonblank(1);
				if (c == ')') break;
				if (c != '(') {
					fprintf(stderr,"Syntax error in case at line %d, character %c\n",line,c);
					if (c == EOF) break;			/* Best we can do */
					continue;
				}
				putchar (META('{'));
				c = gochar();
				if (c == 'e') {
					int c1;
					c1 = gochar();
					if (c1 == 'l') {
						c1 = gochar();
						c1 = gochar();
						c = 0377; /* Default case */
					} else ungetc(c1,stdin);
				}
				putchar(c);
				parseform(0);
				putchar (META('}'));
			}
			putchar (META('}'));
			break;
		case COND:
			putchar(CTRLX);
			putchar ('|');
			putchar (META('{'));
			while (1) {
				c = nonblank(1);
				if (c == ')') {
					putchar (META('}'));
					break;
				}
				if (c != '(') {
					fprintf(stderr,"Syntax error in conditional at line %d, character %c\n",line,c);
					continue;
				}
				putchar(META('{'));
				parseform(CSINGLE);
				putchar(META('}'));
			}
			break;
		case INSERT:
			c = nonblank(1);
			if (c == '"') {
				while ((c=gochar())  != '"') {
					if ((c <= 040) || ((c&0377) >= 0177)) {
						if ((c&0377) >= 0200) putchar(META('q'));
						else putchar(CTRL('Q'));
					}
					putchar(c&0177);
				}
			} else {
				fprintf(stderr,"Argument to %s at line %d must be enclosed in quotes\n",name,line);
			}
			closep(name);
			break;
		case MAP:
			{
				char buf[256];
				char *cp;
				c = nonblank(1);
				if (c == '"') {
					pstring(buf);
				} else {
					fprintf(stderr,"Argument to %s at line %d must be enclosed in quotes\n",name,line);
				}
				
				while ((c = nonblank(1)) != ')') {
					retval = parsememb(c,CARG|CSTRING|(context&CSINGLE));
					if (retval & CCLOSE) {
						context &= ~CSINGLE;
						retflags |= CCLOSE;
					}
				}
				PUTS(defp->body);
				cp = buf;
				while (*cp) {
					putchar(*cp);
					cp++;
				}
			}
			break;
		case CHAR:
			{
				char buf[10];
				
				buf[0] = nonblank(1);
				if (buf[0] == CTRL('X')) {
					buf[1] = nonblank(1);
					buf[2]= 0;
				} else buf[1] = 0;
				c = nonblank(1);
				if (c != ')') {
					retflags |= parsememb(c,CARG|(context&CSINGLE));
					closep(defp->name);
				}
				PUTS(buf);
			}
			break;
		case PSTRING:
			{
				char buf[256];
				char *cp;
				c = nonblank(1);
				if (c == '"') {
					pstring(buf);
				} else {
/* Argument is not a literal, must use the long form */
					ungetc(c,stdin);
					sprintf(buf,"L%s",defp->name);
					defp = getname(buf);
					goto xstring;	/* Process long form */
				}
				
				while ((c = nonblank(1)) != ')') {
					retval = parsememb(c,CARG|CSTRING|(context&CSINGLE));
					if (retval & CCLOSE) {
						context &= ~CSINGLE;
						retflags |= CCLOSE;
					}
				}
				PUTS(defp->body);
				cp = buf;
				while (*cp) {
					if (*cp == '\n') putchar (CTRL('Q'));
					if (*cp == CTRL('Z')) putchar (CTRL('Q'));
					putchar(*cp);
					cp++;
				}
				putchar('\n');
			}
			break;
		case XSTRING:
xstring:		while ((c = nonblank(1)) != ')') {
				retval = parsememb(c,CARG|CSTRING|(context&CSINGLE));
				if (retval & CCLOSE) {
					context &= ~CSINGLE;
					retflags |= CCLOSE;
				}
			}
			PUTS(defp->body);
			break;
		default:
			fprintf(stderr,"Error in parser at line %d, name %s\n",line,name);
		}
	} else {
		if (c == '"') { 	/* String argument, if appropriate, push it */
			if ((context & CSTRING) == 0) {
				fprintf(stderr,"Misplaced character string at line %d\n",line);
			}
			if (context & CSINGLE) {
				retflags |= CCLOSE;
				putchar(META('{'));
			}
			putchar (CTRL('X'));
			putchar ('<');
			pstring (NULL);
			retflags |= CSTRING;
		} else {
			ungetc(c,stdin);
			name = symbol();
			defp = getname(name);
			if (defp->type & SVAL) retflags |= CSTRING;
			if ((defp->type & DOUBLE) && (context & CSINGLE)) {
				putchar(META('{'));
				retflags |= CCLOSE;
				context^= CSINGLE;
			}
			if (DEBUG) fprintf(stderr,"parsememb simple %s type %s, context: %d\n",name,typetable[defp->type&0377],context);
			switch(defp->type&0377) {
			case SIMPLE:
			case XSTRING:
				if (context & CARG) {
					if ((defp->type & SVAL) == 0) retflags |= CCONT;
					if (context&CSINGLE)   {
						putchar(META('{'));
						retflags |= CCLOSE;
					}
				}
				PUTS(defp->body);
				break;
			case NUMBER:
				PUTS (defp->body);
				if ((context &CARG) == 0) putchar(CTRL('Z'));
				break;
			default:
				fprintf(stderr,"function %s at line %d requires arguments\n",name,line);
			}
		}
	}
	if (DEBUG) {
		c = getchar();
		fprintf(stderr,"exiting parsememb before %c\n",c);
		ungetc(c,stdin);
	}

	if (((context & CARG) == 0) && (retflags & CCLOSE)) {
		putchar(CTRL('^'));
		putchar(META('}'));
		retflags &= ~(CCLOSE|CARG);
	}
	if (retflags & CCONT) putchar(CTRL('^'));
	return(retflags & (CCLOSE^CSTRING));
}
closep(name)
char *name;
{
	int c;
	
	c = nonblank(1);
	if (c != ')') {
		fprintf(stderr,"Syntax error at line %d, extraneous characters in form after %s\n	Ignoring characters:",line,name);
		while ((c = getchar()) != ')') {
			if (c == EOF) break;
			fputc(c,stderr);
		}
		fputc('\n',stderr);
	}
}
gochar()
{
	int c;
	
	c = getchar();
	if (c == '\n') line++;
	if (c != '\\') return(c);
	else {
		c = getchar();
		if (c == 'n') return('\n'+01000);
		if ((c >= '0') && (c <= '7')) {
			c -= '0';
			c = c*8 + getchar() - '0';
			c = c*8 + getchar() - '0';
		}
		return(c+01000);	/* Make sure it doesn't match anything */
	}
}
nonblank(cment)
int cment;
{
	int c;
	while (1) {
		c = gochar();
		if (c == EOF) return(c);
		if ((c == ' ') || (c == '	')) continue;
		if (c == '\n') {
			continue;
		}
		if (c == '/') {
			if (cment) putchar(CMENT);
			while ((c = getchar()) != '/') {
				if (c == EOF) {
					fprintf(stderr,"unterminated comment");
					return(c);
				}
				if (cment) putchar(c);
				if (c == '\n') {
					if (cment) putchar(CMENT);
					line++;
				}
			}
			if (cment) putchar('\n');
			continue;
		}
		return(c);
	}
}

PUTS(string)
char *string;
{
	while (*string){
		putchar(*string);
		string++;
	}
}
pstring(ptr)
char *ptr;
{
	int c;	
	int oline;
	oline = line;
	while ((c = gochar()) != '"') {
		if (c == EOF) {
			fprintf(stderr,"Unterminated string starting at line %d\n",oline);
			break;
		}
		if (ptr) {
			*ptr++ = c;
		} else {
			if ((c&0377) == '\n')  putchar(CTRL('Q'));
			if ((c&0377) == CTRL('Z'))  putchar(CTRL('Q'));
			putchar(c);
		}
	}
	if (ptr) {
		*ptr++ = 0;
	} else {
		putchar('\n');
	}
}

xgetc(fp)
FILE *fp;
{
	int c;
	
	c= fgetc(fp);
	fprintf(stderr,"got '%c' %o\n",c);
	return(c);
}
