#ifndef VMS
/* the default search path for Lisp function "load" */
#define PATH_LOADSEARCH ":/usr/athena/lib/gnuemacs/lisp"

/* the extra search path for programs to invoke.
 This is appended to whatever the PATH environment variable says. */
#define PATH_EXEC "/usr/athena/lib/gnuemacs/etc"

/* the name of the directory that contains lock files
 with which we record what files are being modified in Emacs.
 This directory should be writable by everyone.
 THE STRING MUST END WITH A SLASH!!!  */
#define PATH_LOCK "/usr/athena/lib/gnuemacs/lock/"

/* the name of the file !!!SuperLock!!! in the directory
 specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "/usr/athena/lib/gnuemacs/lock/!!!SuperLock!!!"
#else
#define	PATH_LOADSEARCH	"EMACS_LISPDIR"
#define	PATH_EXEC	"EMACS_DIR"
#define	PATH_LOCK	"EMACS_LOCK"
#define	PATH_SUPERLOCK	"EMACS_SUPERLOCK"
#endif
