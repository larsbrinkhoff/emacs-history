#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/* EMACS_MODES: c, !fill */

/* xusend -- Warren Montgomery  12/15/80 */

/* xusend  calls usend, segmenting the list files sent so that each
 * invocation of usend sends no more than 150,000 bytes.  This is a
 * temporary kludge, that really should be part of the standard
 * usend command. */

/* Usage is: 
 * 
 * xusend "usend args" <files>
 *
 */

char *usend = "usend ";			/* program to call */
#define FLIMIT 100000
#define ALIMIT 160000

char combuf[2048];

char *
strcat(cp,cp1)

register char *cp;
register char *cp1;
{
	while (*cp++ = *cp1++);
	return(cp-1);
}

main(argc,argv)

int argc;
char **argv;
{
	long fbytes = 0;
	int filecount = 0;
	int status;
	struct stat statb;
	char *bufptr;
	char *savbuf;
	
	if (argc < 3) {
		printf ("Usage is:\n\n");
		printf ("xusend \"usend args\" <files>\n\n");
		exit(0);
	}
	
	savbuf = strcat(combuf,usend);
	savbuf=strcat(savbuf,argv[1]);
	argc-= 2;
	argv+= 2;
	
	while (argc>0) {
		
		
					/* process next block of files */
		
		bufptr = savbuf;
		fbytes = 0;
		filecount = 0;
		
		while (argc && (fbytes < FLIMIT)) {
			status = stat(*argv,&statb);
			if (status != 0) {
				printf ("Can't stat %s, ignoring it\n", *argv);;
				argv++;
				argc--;
			} else {
				fbytes += statb.st_size;
				if ((fbytes < FLIMIT) || (filecount == 0)){
					*bufptr++ = ' ';
					bufptr = strcat(bufptr,*argv);
					argv++;
					argc--;
					filecount++;
					if (fbytes >FLIMIT) {
						printf ("File %s may be to big for usend\n", *argv);
					}
				}
			}
		}
		if (filecount) {
			printf("\n%s\n",combuf);
			system (combuf);
		} else {
			printf ("File %s is too big for usend, ignoring it\n", *argv);
			argv++;
			argc--;
		}
	}
}

