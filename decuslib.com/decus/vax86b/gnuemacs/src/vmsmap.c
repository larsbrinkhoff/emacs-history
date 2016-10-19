/* VMS mapping of data and alloc arena for GNU Emacs.
   Copyright (C) 1986 Mukesh Prasad

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#ifdef VMS

#include "config.h"
#include "lisp.h"
#include <rab.h>
#include <fab.h>
#include <rmsdef.h>
#include <secdef.h>

/* RMS block size */
#define	BLOCKSIZE	512

/* Maximum number of bytes to be written in one RMS write.
 * Must be a multiple of BLOCKSIZE.
 */
#define	MAXWRITE	( BLOCKSIZE * 30 )

extern char * malloc_base, * start_of_data(), * end_of_data();

/* Structure to write into first block of map file.
 */

struct map_data {
    char * sdata;	/* Start of data area */
    char * edata;	/* End of data area */
    char * smalloc;	/* Start of malloc area */
    char * emalloc;	/* End of malloc area */
    int  datablk;	/* Block in file to map data area from/to */
    int  mblk;		/* Block in file to map malloc area from/to */
    };

static void fill_fab(), fill_rab();
static int write_data();

/* Maps in the data and alloc area from the map file.
 */

int mapin_data()
{
    struct FAB fab;
    struct RAB rab;
    int status, size;
    int inadr[ 2 ];
    struct map_data map_data;

    /* Open map file.
     */
    fill_fab( &fab, 1, 0 );
    status = sys$open( &fab, 0, 0 );
    if ( status != RMS$_NORMAL ) {
	printf( "Map file not available, running bare Emacs....\n" );
	return ( 0 );		/* Map file not available */
	}
    /* Connect the RAB block */
    fill_rab( &rab, &fab );
    status = sys$connect( &rab, 0, 0 );
    if ( status != RMS$_NORMAL )
	lib$stop( status );
    /* Read the header data */
    rab.rab$l_ubf = &map_data;
    rab.rab$w_usz = sizeof( map_data );
    rab.rab$l_bkt = 0;
    status = sys$read( &rab, 0, 0 );
    if ( status != RMS$_NORMAL )
	lib$stop( status );
    status = sys$close( &fab, 0, 0 );
    if ( status != RMS$_NORMAL )
	lib$stop( status );
    if ( map_data.sdata != start_of_data()) {
	printf( "Start of data area has moved: cannot map in data.\n" );
	return ( 0 );
	}
    if ( map_data.edata != end_of_data()) {
	printf( "End of data area has moved: cannot map in data.\n" );
	return ( 0 );
	}
    /* Extend virtual address space to end of previous malloc area.
     */
    brk( map_data.emalloc );
    /* Open the file for mapping now.
     */
    fill_fab( &fab, 1, 1 );
    status = sys$open( &fab, 0, 0 );
    if ( status != RMS$_NORMAL )
	lib$stop( status );
    /* Map data area.
     */
    inadr[ 0 ] = map_data.sdata;
    inadr[ 1 ] = map_data.edata;
    status = sys$crmpsc( inadr, 0, 0, SEC$M_CRF | SEC$M_WRT, 0, 0, 0,
			 fab.fab$l_stv, 0, map_data.datablk, 0, 0 );
    if ( ! ( status & 1))
	lib$stop( status );
    /* Check mapping.
     */
    if ( malloc_base != map_data.smalloc ) {
	printf( "Data area mapping invalid.\n" );
	exit ( 1 );
	}
    /* Map malloc area.
     */
    inadr[ 0 ] = map_data.smalloc;
    inadr[ 1 ] = map_data.emalloc;
    status = sys$crmpsc( inadr, 0, 0, SEC$M_CRF | SEC$M_WRT, 0, 0, 0,
			 fab.fab$l_stv, 0, map_data.mblk, 0, 0 );
    if ( ! ( status & 1))
	lib$stop( status );
    return ( 1 );
}

/* Writes the data and alloc area to the map file.
 */

mapout_data()
{
    struct FAB fab;
    struct RAB rab;
    int status;
    struct map_data map_data;
    int datasize, msize;
    char * sbrk();

    map_data.sdata = start_of_data();
    map_data.edata = end_of_data();
    map_data.smalloc = malloc_base;
    map_data.emalloc = sbrk(0) - 1;
    datasize = map_data.edata - map_data.sdata + 1;
    msize = map_data.emalloc - map_data.smalloc + 1;
    map_data.datablk = 2 + ( sizeof( map_data ) + BLOCKSIZE - 1 ) / BLOCKSIZE;
    map_data.mblk = 1 + map_data.datablk +
		( ( datasize + BLOCKSIZE - 1 ) / BLOCKSIZE );
    /* Create map file.
     */
    fill_fab( &fab, 0, 0 );
    fab.fab$l_alq = 1 + map_data.mblk +
		(( msize + BLOCKSIZE - 1 ) / BLOCKSIZE );
    status = sys$create( &fab, 0, 0 );
    if ( status != RMS$_NORMAL ) {
	error( "Could not create map file" );
	return ( 0 );
	}
    /* Connect the RAB block */
    fill_rab( &rab, &fab );
    status = sys$connect( &rab, 0, 0 );
    if ( status != RMS$_NORMAL ) {
	error( "RMS connect to map file failed" );
	return ( 0 );
	}
    /* Write the header */
    rab.rab$l_rbf = &map_data;
    rab.rab$w_rsz = sizeof( map_data );
    status = sys$write( &rab, 0, 0 );
    if ( status != RMS$_NORMAL ) {
	error( "RMS write (header) to map file failed" );
	return ( 0 );
	}
    if ( ! write_data( &rab, map_data.datablk, map_data.sdata, datasize ))
	return ( 0 );
    if ( ! write_data( &rab, map_data.mblk, map_data.smalloc, msize ))
	return ( 0 );
    status = sys$close( &fab, 0, 0 );
    if ( status != RMS$_NORMAL ) {
	error( "RMS close on map file failed" );
	return ( 0 );
	}
    return ( 1 );
}

static int write_data( rab, firstblock, data, length )
struct RAB
    * rab;
int
    firstblock;
char
    * data;
int
    length;
{
    int status;

    rab->rab$l_bkt = firstblock;
    while ( length > 0 ) {
	rab->rab$l_rbf = data;
	rab->rab$w_rsz = length > MAXWRITE ? MAXWRITE : length;
	status = sys$write( rab, 0, 0 );
	if ( status != RMS$_NORMAL ) {
	    error( "RMS write to map file failed" );
	    return ( 0 );
	    }
	data = &data[ MAXWRITE ];
	length -= MAXWRITE;
	rab->rab$l_bkt = 0;
	}
    return ( 1 );
} /* write_data */


static void fill_fab( fab, input, ufo )
struct FAB
    * fab;
int
    input,
    ufo;
{
    bzero( fab, sizeof( struct FAB ));
    fab->fab$b_bid = FAB$C_BID;
    fab->fab$b_bln = FAB$C_BLN;
    fab->fab$b_fac = FAB$M_BIO;
    if ( input )
	fab->fab$b_fac |= FAB$M_GET;
    else
	fab->fab$b_fac |= FAB$M_PUT;
    fab->fab$l_fna = MAPFILE;
    fab->fab$b_fns = sizeof( MAPFILE ) - 1;
    fab->fab$l_fop = FAB$M_CBT;
    if ( ufo )
	fab->fab$l_fop |= FAB$M_UFO;
    fab->fab$b_fsz = 0;
    fab->fab$w_mrs = 0;
    fab->fab$l_nam = 0;
    fab->fab$b_org = FAB$C_SEQ;
    fab->fab$b_rat = FAB$M_CR;
    fab->fab$b_rfm = FAB$C_VAR;
    fab->fab$b_rtv = 0;
    fab->fab$l_xab = 0;
}

static void fill_rab( rab, fab )
struct RAB
    * rab;
struct FAB
    * fab;
{
    bzero( rab, sizeof( struct RAB ));
    rab->rab$b_bln = RAB$C_BLN;
    rab->rab$b_bid = RAB$C_BID;
    rab->rab$l_fab = fab;
    rab->rab$w_isi = 0;
    rab->rab$b_krf = 0;
    rab->rab$b_mbc = 0;
    rab->rab$b_rac = RAB$C_SEQ;
    rab->rab$b_mbf = 0;
    rab->rab$l_rop = RAB$M_BIO;
    rab->rab$l_xab = 0;
}

#endif
