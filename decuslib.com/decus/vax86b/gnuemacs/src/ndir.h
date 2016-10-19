/*
	<dir.h> -- definitions for 4.2BSD-compatible directory access

	last edit:	09-Jul-1983	D A Gwyn
*/

#ifdef VMS
#ifndef FAB$C_BID
#include <fab.h>
#endif
#ifndef NAM$C_BID
#include <nam.h>
#endif
#ifndef RMS$_NMF
#include <rmsdef.h>
#endif
#endif

#define DIRBLKSIZ	512		/* size of directory block */
#ifdef VMS
#define	MAXNAMLEN	63
#define	MAXFULLSPEC	127		/* Maximum full specification length */
#else
#define MAXNAMLEN	15		/* maximum filename length */
#endif
	/* NOTE:  MAXNAMLEN must be one less than a multiple of 4 */

struct direct				/* data from readdir() */
	{
#ifndef VMS
	long		d_ino;		/* inode number of entry */
	unsigned short	d_reclen;	/* length of this record */
#endif /* not VMS */
	unsigned short	d_namlen;	/* length of string in d_name */
	char		d_name[MAXNAMLEN+1];	/* name of file */
	};

typedef struct
	{
#ifdef VMS
	int	dd_released;		/* Context released flag */
	char	dd_name[MAXFULLSPEC+1];	/* Directory specifications */
	struct FAB
		dd_fab;			/* File Access Block */
	struct NAM
		dd_nam;			/* Name block */
	char	dd_esa[MAXFULLSPEC+1];	/* Full file specifications */
	char	dd_rsa[MAXFULLSPEC+1];	/* Retrieved file specifications */
#else
	int	dd_fd;			/* file descriptor */
	int	dd_loc;			/* offset in block */
	int	dd_size;		/* amount of valid data */
	char	dd_buf[DIRBLKSIZ];	/* directory block */
#endif VMS
	}	DIR;			/* stream data from opendir() */

extern DIR		*opendir();
extern struct direct	*readdir();
#ifndef VMS
extern long		telldir();
extern void		seekdir();
#endif
extern void		closedir();

#ifndef VMS
#define rewinddir( dirp )	seekdir( dirp, 0L )
#endif
