/*
**	Concatenate files.
*/

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>

/* character type table */

#define PLAIN 0
#define CONTRL 1
#define TAB 2
#define BACKSP 3

char ctype[128] = {
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL, TAB,	PLAIN,	CONTRL,	TAB,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,	CONTRL,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,
	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	PLAIN,	CONTRL,
};
char	stdbuf[BUFSIZ];
main(argc, argv)
char **argv;
{
	register FILE *fi;
	register c;
	int	fflg = 0;
	int	silent = 0;
	int	status = 0;
	int	tabs = 0;
	int	dev, ino = -1;
	struct	stat	statb;

	setbuf(stdout, stdbuf);
	for( ; argc>1 && argv[1][0]=='-'; argc--,argv++) {
		switch(argv[1][1]) {
		case 0:
			break;
		case 'u':
			setbuf(stdout, (char *)NULL);
			continue;
		case 's':
			silent++;
			continue;
		case 't':
			tabs++;
			continue;
		}
		break;
	}
	if(fstat(fileno(stdout), &statb) < 0) {
		if(!silent)
			fprintf(stderr, "cat: Cannot stat stdout\n");
		exit(2);
	}
	statb.st_mode &= S_IFMT;
	if (statb.st_mode!=S_IFCHR && statb.st_mode!=S_IFBLK) {
		dev = statb.st_dev;
		ino = statb.st_ino;
	}
	if (argc < 2) {
		argc = 2;
		fflg++;
	}
	while (--argc > 0) {
		if ((*++argv)[0]=='-' && (*argv)[1]=='\0' || fflg)
			fi = stdin;
		else {
			if ((fi = fopen(*argv, "r")) == NULL) {
				if (!silent)
					fprintf(stderr, "cat: cannot open %s\n", *argv);
				status = 2;
				continue;
			}
		}
		if(fstat(fileno(fi), &statb) < 0) {
			if(!silent)
				fprintf(stderr, "cat: cannot stat %s\n", *argv);
			status = 2;
			continue;
		}
		if (statb.st_dev==dev && statb.st_ino==ino) {
			if(!silent)
				fprintf(stderr, "cat: input %s is output\n",
				   fflg?"-": *argv);
			fclose(fi);
			status = 2;
			continue;
		}
		while ((c = getc(fi)) != EOF) {
			if (c & 0200) {
				putchar('M');
				putchar('-');
				c-= 0200;
			}

			switch(ctype[c]) {
			case PLAIN:
				putchar(c);
				break;
			case TAB:
				if (tabs) {
					putchar(c);
				} else {
					putchar('^');
					putchar (c^0100);
				}
				break;
			case CONTRL:
				putchar('^');
				putchar(c^0100);
				break;
			}
		}
		if (fi!=stdin)
			fclose(fi);
	}
	exit(status);
}
