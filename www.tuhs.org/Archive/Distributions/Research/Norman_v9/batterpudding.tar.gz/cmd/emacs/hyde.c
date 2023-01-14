#include <stdio.h>
#define trace if (traceon) printf

extern FILE *popen();
extern char *getenv();
extern char *malloc();

/* HYpothesis Driven Expert */

struct ref {
	struct ref *next;
	struct defblk *this;
	char *text;
};
struct defblk {
	struct defblk *next;
	char *name;
	int type;
	struct ref *definition;
};
struct conc {
	int weight;
	struct hype *hypo;
	int vector;
	int num;
	struct fact *confirms[];
};
struct fact {
	struct fact *next;
	struct defblk *name;
	int truth;
	struct concr *setlist;
};

struct concr {
	struct concr *next;
	struct conc *this;
};
struct hype {
	struct hype *next;
	struct defblk *name;
	int confid;
	short asked;
	short action;
	struct ref *queries;
	char *text;
	char *explain;
};

#define NHASH 255
struct defblk *hashtable[NHASH];


#define FACT 1
#define HYPO 2
#define STRING 3

#define ASK 1
#define SCAN 2
#define RUN 3

#define FUNKNOWN 0
#define FTRUE 1
#define FFALSE -1

#define HUNCERTAIN 10
#define HTRUE 100
#define HFALSE 0

struct hype *hypes;
struct fact *facts;

int tot_facts;
int tot_hypes;
int traceon = 0;
FILE *kfile;

#define SYMCHAR 1
#define WHITE 2
char ctype[128] = {
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	WHITE,	WHITE,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	WHITE,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,0,	0,	0,	0,	0,	0,
	0,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,SYMCHAR,0,	0,	0,	0,	SYMCHAR,
	0	,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,SYMCHAR,
	SYMCHAR,SYMCHAR,SYMCHAR,0,	0,	0,	0,	0
};
#define streq(x,y) (strcmp(x,y) == 0)

struct defblk *getref(name,type,create)

/* Keywords: symbol-table:50 internal-database:50 */

char *name;
int type;
int create;
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
		struct fact *factp;
		struct hype *hypep;

		if (create == 0) return(NULL);
		defp = ((struct defblk *) malloc(sizeof(*defp)));
		defp->next = hashtable[hash];
		hashtable[hash] = defp;
		defp->name = (char *) malloc(strlen(name)+1);
		strcpy(defp->name,name);
		defp->type = type;
		switch(type) {

		case STRING:
			defp->definition = NULL;
			break;
		case FACT:
		
			tot_facts++;
			defp->definition = (struct ref *) malloc(sizeof(*factp));
			factp = (struct fact *) defp->definition;
			factp->name = defp;
			factp->next = facts;
			facts = factp;
			factp->truth = FUNKNOWN;
			factp->setlist = NULL;
			trace ("Defining fact %s\n",name);
			break;
		case HYPO:
			tot_hypes++;
			defp->definition = (struct ref *) malloc(sizeof(*hypep));
			hypep = (struct hype *) defp->definition;
			hypep -> name = defp;
			hypep -> next = hypes;
			hypes = hypep;
			hypep -> asked = 0;
			hypep -> confid = HUNCERTAIN;
			hypep -> text = NULL;
			hypep -> explain = NULL;
			hypep -> queries = NULL;
			trace ("Defining hypothesis %s\n",name);
			break;
		}
	} else {
		if (type && (type != defp->type)) {
			fprintf(stderr,"Multiply defined symbol: %s\n",name);
		}
	}
	return(defp);
}

char expbuf[512];

char *expcom(oldcom)
register char *oldcom;
{
	register char *newcom;
	char symbuf[128];
	char *symp;
	struct defblk *defp;
	struct fact *factp;
	int infalse;
	newcom = expbuf;

	while (*oldcom) {
		switch (*oldcom) {
			
		case '\\':
			*newcom++ = * ++oldcom;
			break;
		case '%':
			oldcom++;
			symp = symbuf;
			while ((ctype[*oldcom] & SYMCHAR)==0) oldcom++;
			while ((ctype[*oldcom] & SYMCHAR)) *symp++ = *oldcom++;
			*symp = 0;
			defp = getref(symbuf,0,0);
			if (defp ) switch(defp->type) {
			case STRING:
				symp = (char *) defp->definition;
				while (*symp) *newcom++ = *symp++;
				oldcom--;
				break;
			case FACT:
				factp = (struct fact *) defp->definition;
				while (*oldcom != '(' ) oldcom++;
				infalse = 0;
				oldcom++;
				while (1) {
					switch (*oldcom) {
					case 0:
					case ')':
						goto done;
					case ':':
						infalse++;
						break;
					case '\\':
						++oldcom;
					default:
						if (((factp->truth == FTRUE) && (infalse == 0)) ||
							((factp->truth == FFALSE) && (infalse))) {
								 
							*newcom++ = *oldcom;
						}
						break;
					}
					oldcom++;
				}
			} else {
				fprintf(stderr,"%s is undefined on expansion",symbuf);
			}
done:			break;
		default:
			*newcom++ = *oldcom;
		}
		oldcom++;
	}
	*newcom= 0;
	trace ("Expanded into %s\n",expbuf);
	return(expbuf);
}


struct hype * curhype()
{
	struct hype *hype,*rehype;
	int maxhype;
	
	maxhype = HUNCERTAIN;		/* Cut off any below this level */
	rehype = NULL;
	for (hype = hypes; hype; hype = hype-> next) if ((hype->asked==0) && (hype->confid > maxhype)) {
		maxhype = hype->confid;
		rehype = hype;
	}
	trace ("Best hypothesis is %s\n",rehype->name->name);
	return(rehype);
}
reset()
{
	struct fact *factp;
	struct hype *hypep;
	struct defblk *defp;
	int i;
	
	for (i = 0; i < NHASH; i++) {
		for (defp = hashtable[i]; defp; defp = defp->next) {
			if (defp->type == STRING) defp->definition = NULL;
		}
	}
	for (factp = facts; factp; factp = factp->next) factp->truth=FUNKNOWN;
	for (hypep = hypes; hypep; hypep = hypep->next) {
		hypep->confid = HUNCERTAIN;
		hypep->asked = 0;
	}
	defp = getref("start",FACT,1);
	setfact(defp->definition,FTRUE);		/* Start up the inference engine */
}

askhype(hype)
struct hype *hype;
{
	char buf[20];
	struct defblk *defp;
	struct ref *refp;
	struct fact *factp;
	struct fact *factors[20];
	int truth[20];
	int nfact = 0;
	int i;
	char *bufp;
	FILE *fp;
	
	trace ("Asking about hypothesis %s\n",hype->name->name);
	refp = hype->queries;
	while (refp) {
		defp = refp->this;
		if (defp->type == STRING) {
			if (defp->definition) return(0);
			goto setup;
		}
		factp = (struct fact *) defp->definition;
		trace ("Sub-fact %s, state %d\n",factp->name->name,factp->truth);
		if (factp->truth == FUNKNOWN) {
			if (nfact == 0) {
setup:				switch (hype->action) {
				case ASK: printf ("%s\n\n",expcom(hype->text));
					if (defp->type == STRING) {
						gets(buf);
						defp->definition = (struct ref *) malloc(strlen(buf)+1);
						strcpy(defp->definition,buf);
						return(1);
					}
					break;
				case SCAN:
					fp = fopen(hype->text,"r");
					trace ("Scanning %s from hypothesis %s\n", hype->text,hype->name->name);
					fmatch(fp,hype->queries);
					fclose(fp);
					return(1);
				case RUN:
					fp = popen(expcom(hype->text),"r");
					trace ("Running %s from hypothesis %s\n", hype->text,hype->name->name);
					fmatch(fp,hype->queries);
					pclose(fp);
					return(1);
				}
			}
			factors[nfact++] = factp;
			printf ("	%d) %s\n",nfact,refp->text);
		}
		refp = refp->next;
	}
	if (nfact == 0) return(0);
	
	printf ("\n? ");
again: gets(buf);
	for (i = 0; i < nfact; i++) truth[i] = FFALSE;
	for (bufp = buf; *bufp; bufp++) {
		if (*bufp == 't') {
			traceon = !traceon;
			continue;
		}
		if (*bufp == '!') {
			bufp++;
			if (*bufp == 0) {
				bufp = getenv("SHELL");
				if ((bufp == NULL) || (*bufp == 0)) bufp= "/bin/sh";
			}
			system (bufp);
			return(askhype(hype));
		}
		if (*bufp == 'q') return(-1);
		if ((*bufp < '1') || (*bufp > '0'+nfact)) {
			if ((*bufp == ' ') || (*bufp == ',')) continue;
			printf ("Please type all of the numbers that apply\n");
			goto again;
		}
		truth[*bufp-'1'] = FTRUE;
	}
	for (i = 0; i < nfact; i++) {
		setfact(factors[i],truth[i]);
	}
	return(1);
}

setfact(factp,truth)
struct fact *factp;
int truth;
{
	struct hype *hype;
	struct conc *concp;
	struct concr *refp;
	int trigger;
	int i;
	
	factp->truth = truth;
	
	trace ("Setting fact %s to %d\n",factp->name->name,truth);
	
	for (refp = factp->setlist; refp; refp = refp ->next) {
		concp = refp->this;
		trigger = 1;
		for (i = 0;(trigger && (i < concp->num)); i++) {
			if (concp->confirms[i]->truth == FUNKNOWN) trigger = 0;
			else {
				if ((concp->vector>>i) & 1) {
					if (concp->confirms[i]->truth == FFALSE) trigger = 0;
				} else {
					if (concp->confirms[i]->truth == FTRUE) trigger = 0;
				}
			}
		}
		if (trigger) {
			sethype(concp->hypo,concp->weight);
		}

	}
}

sethype(hype,weight)

struct hype *hype;
int weight;
{

	if (hype->name->type == FACT) {
		struct fact *factp;
		factp = (struct fact *) hype;
		if (factp-> truth != FUNKNOWN) return;
		if (weight ==  0) {
			setfact(factp,FFALSE);
		} else {
			setfact(factp,FTRUE);
		}
		return;
	}
	if (weight < 0) hype->confid = HTRUE;
	else {
		hype->confid *= weight;
		hype->confid /= 10;
	}
	if (hype->confid > HTRUE) hype->confid = HTRUE;
	trace ("Adjusting hypothesis %s by %d gives %d\n",hype->name->name,weight,hype->confid);
}

dumplike()
{
	struct hype *hypo;
	
	for (hypo = hypes; hypo; hypo = hypo->next) {
		hypo->asked = 0;
	}
	while (hypo = curhype()) {
		hypo->asked = 1;
		if (hypo->explain) {
			printf ("The following is a possible cause of your problem:\n\n%s\n",hypo->explain);
			printf ("\nThis explaination has a confidence factor of %d on a scale of %d to %d\n",hypo->confid,HFALSE,HTRUE);
			printf ("_________________________________________________________________________\n");
		}
	}
}

cycle()
{
	struct hype *hypo;
	int ret;

#ifdef DIAGNOSE
	printf ("You will be asked to clarify your problem\n");
#endif
	printf ("For each question, please answer with all of the numbers\n");
	printf ("of the statements that apply.  For example 136\n");
	printf ("if answers 1, 3, and 6 apply.  If you would like to quit, type 'q'\n");
	printf ("If you would like to escape to run a unix commmand, type '!'\n");
	printf ("or '!command' in response to a question.\n\n");
	
	while (hypo = curhype()) {
		if (ret= askhype(hypo)) {
			if (ret < 0) return; /* User quit */
			if (hypo->explain == NULL) hypo->asked = 1;
		} else {
			hypo->asked = 1;
			if (hypo->explain) {
#ifdef DIAGNOSE
				printf ("The following is a possible cause of your problem:\n\n%s\n",hypo->explain);
				printf ("\nThis explaination has a confidence factor of %d on a scale of %d to %d\n",hypo->confid,HFALSE,HTRUE);
				if (gyn("Would you like to continue to identify other possible causes") == 0) return;
#else
				printf("%s\n",hypo->explain);
				return(0);
#endif
			}
		}
	}
}

gyn(string)
char *string;
{
	char buf[100];
	
	while (1) {
		printf ("%s\n",string);
		if (gets(buf) == NULL) return(0);
		if (buf[0] == 'y') return(1);
		if (buf[0] == 'n') return(0);
		printf ("Please answer with 'yes' or 'no'\n");
	}
}

char *
gstring()
{
	char buf[512];
	char *bufp;
	int c;
	
	c = nonblank();
	if (c != '"') {
		fprintf("Missing character string argument\n");
		ungetc(c,kfile);
		return;
	}
	bufp = buf;
	while (((c =getc(kfile)) != EOF) && (c != '"')) {
		if (c == '\\') c = getc(kfile);
		*bufp++ = c;
	}
	*bufp++ = 0;
	bufp = malloc(bufp-buf+1);
	strcpy(bufp,buf);
	trace ("Storing character string %s\n",bufp);
	return(bufp);
}

nonblank()
{
	int c;

	while ((c = getc(kfile)) != EOF) {
		if ((ctype[c] & WHITE) == 0) return(c);
	}
	return(EOF);
}

int number(sp)
/* Keywords: string-processing file-scanning:50 user-interface:10 */

char *sp;
{
	int n;
	n = 0;
	while (*sp) n = n*10 + (*sp++) -'0';
	return(n);
}

char symbuf[64];

char * symbol()
{
	int c;
	char *symp;
	
	symp = symbuf;
	
	c = nonblank();
	while ((c != EOF) && (ctype[c]&SYMCHAR)) {
		*symp++ = c;
		c = getc(kfile);
	}
	if (c == EOF) return(NULL);
	ungetc(c,kfile);
	*symp++ = 0;
	trace ("Reading symbol: %s\n",symbuf);
	return(symbuf);
}


/* Knowledge file formats: */


/* Hypothesis name: {  */
/* 	Ask: "text for asking" */
/* 	Scan: "file(s) to scan"  */
/* 	Run: "command to run" */
/* 	Replies: { */
/* 		factname: "description" */
/* 	} */
/* 	Read: string */

/* 	Explain: "explaination if terminal diagnosis" */

/* Infer hypothesis <with certaintity number> from { */
/* Conclude fact from { */
/* 	fact, or !fact */
/* } */

main(argc, argv)

/* Keywords: user-interface command-line file-scanning:10 file-opening:10 */

int argc;
char *argv [];

{
	int i;
	int ktest = 0;
	for (i = 1; i < argc; i++) {
		if (streq(argv[i],"-t")) {
			traceon++;
			i++;
		}
		if (streq(argv[i],"-k")) {
			ktest++;
			i++;
		}
		if (argc>2) printf ("Loading file %s\n",argv[i]);
		parse(argv[i]);
	}
	if (ktest) {
		dumpknow();
	}
	while (1) {
		reset();
		cycle();
#ifdef DIAGNOSE
		printf ("I can offer no further help with this problem\n");
		printf ("If you are still having trouble, contact ihnss!warren\n\n");

		if (gyn("Would you like a summary of all likely causes?")) dumplike();
		if (gyn("Would you like to try another diagnosis?")==0) break;
#else
		if (gyn("Would you like to do something else?")==0) break;
#endif
	}
}
dumpknow()
{
	struct fact *factp;
	struct hype *hypep;
	struct ref *refp;
	struct conc *consp;
	struct concr *concrp;
	int header;
	
	reset();
	header = 0;
	for (factp = facts; factp; factp= factp->next) {
		for (concrp = factp->setlist; concrp; concrp = concrp->next) {
			concrp->this->hypo->asked = 1;
		}
	}
	header = 0;
	for (hypep = hypes; hypep; hypep = hypep->next) {
		if ((hypep->text == NULL)&& (hypep->explain == NULL)) {
			if (header == 0) {
				header = 1;
				printf ("The following hypotheses are never defined:\n");
			}
			printf ("	%s\n",hypep->name->name);
		}
		for (refp = hypep->queries; refp; refp = refp->next) {
			((struct fact *) refp->this->definition)->truth = FTRUE;
		}
	}
	header = 0;
	for (hypep = hypes; hypep; hypep = hypep->next) {
		if (hypep->asked == 0) {
			if (header == 0) {
				header = 1;
				printf ("The following hypotheses are not inferred by any facts:\n");
			}
			printf ("	%s\n",hypep->name->name);
		}
	}
	header = 0;
	for (factp = facts; factp; factp= factp->next) {
		if (factp->truth == FUNKNOWN) {
			if (header == 0) {
				header = 1;
				printf ("The following facts are never set:\n");
			}
			printf ("	%s\n",factp->name->name);
		}
	}
	header = 0;
	for (factp = facts; factp; factp= factp->next) {
		if (factp->setlist == NULL) {
			if (header == 0) {
				header = 1;
				printf ("The following facts are never used to infer anything:\n");
			}
			printf ("	%s\n",factp->name->name);
		}
	}
	reset();
}
parse(np)

/* Keywords: file-scanning file-opening:10 preprocessor:10 database-input:10 */

char *np;
{
	register char *symp;
	char c;
	kfile = fopen(np,"r");

	if (kfile == NULL) {
		fprintf(stderr,"Can't open %s\n",np);
		return;
	}
	
	while (symp = symbol()) {
		if (streq(symp,"Hypothesis")) addhype();
		else if (streq(symp,"Infer")) addinfer(HYPO);
		else if (streq(symp,"Conclude")) addinfer(FACT);
		else {
			fprintf (stderr,"Bad keyword %s\n",symp);
			c = nonblank();
			if (ctype[c] & SYMCHAR) ungetc(c,kfile);
		}
	}
}

addhype()
{
	register char *sp;
	register struct hype *hypep;
	register struct defblk *defp;
	register struct fact *factp;
	struct ref *refp;
	char c;
	
	sp = symbol();
	if (sp == NULL) {
eof:	fprintf(stderr,"Unexpected EOF in hypothesis\n");
		return;
	}
	defp = getref(sp,HYPO,1);
	hypep = (struct hype *) defp->definition;
	c = nonblank();
	if (c != ':') {
		fprintf(stderr,"Missing ':' after hypothesis name %s\n",sp);
	}
	c = nonblank();
	if (c != '{') {
		fprintf (stderr,"Empty hypothesis body %s\n",sp);
	}
	while (1) {
		c = nonblank();
		if ((c == EOF) || (c == '}')) return;
		ungetc(c,kfile);
		sp = symbol();
		c = nonblank();
		if (c != ':') {
			fprintf(stderr,"Missing ':' after hypothesis name %s\n",sp);
		}
		if (streq(sp,"Ask")) {
			hypep->text = gstring();
			hypep->action = ASK;
		}
		else if (streq(sp,"Scan")) {
			hypep->text = gstring();
			hypep->action = SCAN;
		}
		else if (streq(sp,"Run")) {
			hypep->text = gstring();
			hypep->action = RUN;			
		} else if (streq(sp,"Explain")) {
			hypep->explain = gstring();
		} else if (streq(sp,"Read")) {
			sp = symbol();
			defp = getref(sp,STRING,1);
			refp = (struct ref *) malloc(sizeof *refp);
			refp->next = hypep->queries;
			refp->this = defp;
			refp->text = NULL;
			hypep->queries = refp;
		} else if (streq(sp,"Replies")) {
			c = nonblank();
			if (c != '{') {
				fprintf (stderr,"Empty replies body %s\n",sp);
			}
			while (1) {
				c = nonblank();
				if (c == '}') break;
				ungetc(c,kfile);
				sp = symbol();
				c = nonblank();
				if (c != ':') {
					fprintf(stderr,"Missing ':' after reply name %s\n",sp);
				}
				if ((*sp >= 'A') && (*sp <= 'Z')) {

/* Define a hypothesis and fact of the same name, and make an inference */

					struct hype *infhype;
					struct conc *concp;
					struct concr *concrp;
					
					defp = getref(sp,HYPO,1);
					infhype = (struct hype *) defp->definition;
					*sp += 32;
					defp = getref(sp,FACT,1);
					factp = (struct fact *) defp->definition;
					if (factp->setlist) {

						/* Already exists, assume the infers part does to */
						goto makeref; /* Join other branch to make this reference to it. */
					}
					concrp = (struct concr *) malloc(sizeof *concrp);
					factp->setlist = concrp;
					concp = (struct conc *) malloc((sizeof *concp) + (sizeof factp));
					concrp->this = concp;
					concrp->next = NULL;
					concp -> weight = -1;
					concp ->hypo = infhype;
					concp -> vector = 1;
					concp -> num = 1;
					concp ->confirms[0] = factp;
				} else {
					defp = getref(sp,FACT,1);
					factp = (struct fact *) defp->definition;
				}
makeref:
				c = nonblank();
				ungetc(c,kfile);
				refp = (struct ref *) malloc(sizeof *refp);
				refp -> next = hypep -> queries;
				hypep->queries = refp;
				refp->this = defp;
				if (c == '"') {
					refp->text = gstring();
				}
			}
		} else {
			fprintf (stderr,"Bad Keyword %s\n",sp);
		}
	}
}
addinfer(type)
int type;
{
	struct hype *hypep;
	struct fact *factp;
	struct conc *concp;
	struct fact *facts[20];
	struct concr *concrp;
	struct ref *refp;
	struct defblk *defp;
	register char *sp;
	int factor;
	int vector;
	int c;
	int numinf;
	int num;
	
	sp = symbol();
	factor = -1;
	if (*sp == '!') {
		factor = 0;
		sp++;
	}
	defp = getref(sp,type,1);
	hypep = (struct hype *) defp->definition;

	sp = symbol();
	if (streq(sp,"certainty")) {
		if (type != HYPO) fprintf(stderr,"Conclude clause can't have a certainty factor\n");
		sp = symbol();
		factor = number(sp);
		sp = symbol();
	}
	if (streq(sp,"from")) {
		c = nonblank();
		if (c == '{') {
			num=1000;
		} else {
			ungetc(c,kfile);
			num = 1;
		} 
		vector = 0;
		numinf = 0;
		while (num--) {
			c = nonblank();
			if (c == '}') break;
			vector = vector << 1;
			if (c != '!' ) {
				vector += 1;
				ungetc(c,kfile);
		}
			sp = symbol();
			defp = getref(sp,FACT,1);
			facts[numinf++] = (struct fact *) defp->definition;
		}
		if (numinf) {
			int x;
			
			concp = (struct conc *) malloc((sizeof *concp) + numinf * (sizeof factp));
			concp ->hypo = hypep;
			concp -> weight = factor;
			concp -> vector = vector;
			concp -> num = numinf;
			x = 0;
			while (--numinf >= 0) {
				factp = facts[numinf];
				concrp = (struct concr *) malloc(sizeof *concrp);
				concrp ->next = factp->setlist;
				factp->setlist = concrp;
				concrp->this = concp;
				concp->confirms[x++]  = factp;
			}
		}
	}
}


match(pat,targ)
char *pat,*targ;
{
	int first;
	register char *p, *t;
	
	if (*pat== '^') {
		first=1;
		pat++;
	} else first = 0;
	
	while ( *targ) {
		if (*pat == *targ) {
			p = pat;
			t = targ;
			while (*p) if (*p++ != *t++)  goto next;
			return(1);
		}
next:		if (first) return(0);
		targ++;
	}
	return(0);
}

fmatch(fp,refp)
FILE *fp;
struct ref *refp;
{
	struct fact *factp;
	struct ref *rp;
	char buf[512];
	
	if (fp == NULL) return;
	while (fgets(buf,512,fp)) {
		for (rp = refp; rp != NULL; rp = rp->next) {
			if (rp->this->type == STRING) {
				buf[strlen(buf)-1] = 0;
				rp->this->definition = (struct ref *) malloc(strlen(buf)+1);
				strcpy(rp->this->definition,buf);
			} else {
				factp = (struct fact *) rp->this->definition;
				if ((factp->truth == FUNKNOWN) && match(rp->text,buf)) setfact(factp,FTRUE);
			}
		}
	}
	for (rp = refp; rp != NULL; rp = rp->next) {
		if (rp->this->type == FACT) {
			factp = (struct fact *) rp->this->definition;
			if (factp->truth == FUNKNOWN) setfact(factp,FFALSE);
		}
	}
}
