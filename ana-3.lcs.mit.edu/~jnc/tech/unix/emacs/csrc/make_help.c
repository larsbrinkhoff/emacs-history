#include <stdio.h>

/* make help file for emacs.   This program converts from UNIX file
 * format to fixed length lines needed for the EMACS help file */

#define HELSIZE 128

main()
{
	register  i;
	char buf[HELSIZE];
	
	while (1) {
		for (i = 0; i < HELSIZE; i++) buf[i] = 0;
		if (gets(buf) == NULL) exit(0);
		write(1,buf,HELSIZE);
	}
}
