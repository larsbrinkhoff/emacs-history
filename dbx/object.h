#ifndef object_h
#define object_h

struct {
    unsigned int stringsize;	/* size of the dumped string table */
    unsigned int nsyms;		/* number of symbols */
    unsigned int nfiles;	/* number of files */
    unsigned int nlines;	/* number of lines */
} nlhdr;

String objname ;
Integer objsize;
char *stringtab;
readobj(/* file */);
objfree(/*  */);
#endif
