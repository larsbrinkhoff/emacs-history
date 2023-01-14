#include <stdio.h>

/* make help file for emacs.   This program converts from UNIX file
 * format to fixed length lines needed for the EMACS help file */

#define HELSIZE 128

main()
{
	register  i;
	char buf[HELSIZE+1];
	
	while (1) {
		for (i = 1; i <= HELSIZE; i++) buf[i] = 0;
		if (gets(buf) == NULL) exit(0);
		i = strlen(buf);
		buf[i-1] = buf[i-2] = 0;
		write(1,buf+1,HELSIZE);
	}
}
