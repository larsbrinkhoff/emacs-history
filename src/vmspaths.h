/* the default search path for Lisp function "load" */
#define PATH_LOADSEARCH ",/emacs_library/lisp"

/* the extra search path for programs to invoke.
  This is appended to whatever the PATH environment variable says. */
#define PATH_EXEC "/emacs_library/etc"

/* the name of the directory that contains lock files
  with which we record what files are being modified in Emacs.
  This directory should be writable by everyone.
  THE STRING MUST END WITH A SLASH!!!  */
#define PATH_LOCK "/emacs_library/lock/"

/* the name of the file !!!SuperLock!!! in the directory
  specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "/emacs_library/lock/$$$SuperLock$$$"
