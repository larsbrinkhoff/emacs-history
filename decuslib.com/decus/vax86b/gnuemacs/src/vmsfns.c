/* VMS subprocess and command interface.
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


/*
 * INTERFACE PROVIDED BY EMACS FOR VMS SUBPROCESSES:
 *
 *	Emacs provides the following functions:
 *
 *      "spawn-subprocess", which takes as arguments:
 *
 *	  (i)   an integer to identify the spawned subprocess in future
 *              operations,
 *	  (ii)  A function to process input from the subprocess, and
 *	  (iii) A function to be called upon subprocess termination.
 *
 *      First argument is required.  If second argument is missing or nil,
 *      the default action is to insert all received messages at the current
 *      location in the current buffer.  If third argument is missing or nil,
 *      no action is taken upon subprocess termination.
 *	The input-handler is called as
 *		(input-handler num string)
 *	where num is the identifying integer for the subprocess and string
 *	is a string received from the subprocess.  exit-handler is called
 *	with the identifying integer as the argument.
 *
 *      "send-command-to-subprocess" takes two arguments:
 *
 *	  (i)   Subprocess identifying integer.
 *        (ii)  String to send as a message to the subprocess.
 *
 *      "stop-subprocess" takes the subprocess identifying integer as
 *      argument.
 *
 *  Implementation is done by spawning an asynchronous subprocess, and
 *  communicating to it via mailboxes.
 */

#ifdef VMS

#include "config.h"
#include "lisp.h"
#include <descrip.h>
#include <dvidef.h>
#include <prvdef.h>
/* #include <clidef.h> */
#include <iodef.h>
#include <errno.h>

#define	CLI$M_NOWAIT	1	/* clidef.h is missing from C library */

#define	MSGSIZE	160		/* Maximum size for mailbox operations */

/* IO status block for mailbox operations.
 */
struct mbx_iosb {
    short status;
    short size;
    int   pid;
    };

/* Structure for maintaining linked list of subprocesses.
 */
struct process_list {
    int name;			/* Numeric identifier for subprocess */
    int process_id;		/* VMS process address */
    int process_active;		/* 1 iff process has not exited yet */
    int mbx_chan;		/* Mailbox channel to write to process */
    struct mbx_iosb iosb;	/* IO status block for write operations */
    Lisp_Object input_handler;	/* Input handler for subprocess */
    Lisp_Object exit_handler;	/* Exit handler for subprocess */
    struct process_list * next;	/* Linked list chain */
    };

/* Structure for privilege list.
 */
struct privilege_list {
    char * name;
    int  mask;
    };

static int exit_ast();		/* Called upon subprocess exit */
static int create_mbx();	/* Creates mailbox */
static void mbx_msg();		/* Writes null terminated string to mbx */
static void write_to_mbx();	/* Writes message to string */
static void start_mbx_input();	/* Queues I/O request to mailbox */

static int input_mbx_chan = 0;	/* Channel to read subprocess input on */
static char input_mbx_name[ 20 ];
				/* Storage for mailbox device name */
static struct dsc$descriptor_s input_mbx_dsc;
				/* Descriptor for mailbox device name */
static struct process_list * process_list = 0;
				/* Linked list of subprocesses */
static char mbx_buffer[ MSGSIZE ];
				/* Buffer to read from subprocesses */
static struct mbx_iosb input_iosb;
				/* IO status block for mailbox reads */

int have_process_input,		/* Non-zero iff subprocess input pending */
    process_exited;		/* Non-zero iff suprocess exit pending */

/* List of privilege names and mask offsets */
static struct privilege_list priv_list[] = {

    { "ACNT",		PRV$V_ACNT },
    { "ALLSPOOL",	PRV$V_ALLSPOOL },
    { "ALTPRI",		PRV$V_ALTPRI },
    { "BUGCHK",		PRV$V_BUGCHK },
    { "BYPASS",		PRV$V_BYPASS },
    { "CMEXEC",		PRV$V_CMEXEC },
    { "CMKRNL",		PRV$V_CMKRNL },
    { "DETACH",		PRV$V_DETACH },
    { "DIAGNOSE",	PRV$V_DIAGNOSE },
    { "EXQUOTA",	PRV$V_EXQUOTA },
    { "GROUP",		PRV$V_GROUP },
    { "GRPNAM",		PRV$V_GRPNAM },
    { "LOG_IO",		PRV$V_LOG_IO },
    { "MOUNT",		PRV$V_MOUNT },
    { "NETMBX",		PRV$V_NETMBX },
    { "NOACNT",		PRV$V_NOACNT },
    { "OPER",		PRV$V_OPER },
    { "PFNMAP",		PRV$V_PFNMAP },
    { "PHY_IO",		PRV$V_PHY_IO },
    { "PRMCEB",		PRV$V_PRMCEB },
    { "PRMGBL",		PRV$V_PRMGBL },
    { "PRMMBX",		PRV$V_PRMMBX },
    { "PSWAPM",		PRV$V_PSWAPM },
    { "SETPRI",		PRV$V_SETPRI },
    { "SETPRV",		PRV$V_SETPRV },
    { "SHMEM",		PRV$V_SHMEM },
    { "SYSGBL",		PRV$V_SYSGBL },
    { "SYSLCK",		PRV$V_SYSLCK },
    { "SYSNAM",		PRV$V_SYSNAM },
    { "SYSPRV",		PRV$V_SYSPRV },
    { "TMPMBX",		PRV$V_TMPMBX },
    { "VOLPRO",		PRV$V_VOLPRO },
    { "WORLD",		PRV$V_WORLD },

    };

Lisp_Object Qdefault_subproc_input_handler;

extern int process_ef;		/* Event flag for subprocess operations */

DEFUN( "default-subprocess-input-handler",
           Fdefault_subproc_input_handler, Sdefault_subproc_input_handler,
        2, 2, 0,
        "Default input handler for input from spawned subprocesses." )
  (name, input)
     Lisp_Object name, input;
{
  /* Just insert in current buffer */
  InsCstr( XSTRING (input)->data, XSTRING (input)->size );
  InsCstr( "\n", 1 );
}

DEFUN ("spawn-subprocess", Fspawn_subprocess, Sspawn_subprocess, 1, 3, 0,
  "Spawns an asynchronous VMS suprocess for command processing.")
  (name, input_handler, exit_handler)
     Lisp_Object name, input_handler, exit_handler;
{
  int status;
  char output_mbx_name[ 20 ];
  struct dsc$descriptor_s output_mbx_dsc;
  struct process_list * ptr, * p;

  CHECK_NUMBER( name, 0 );
  if ( ! input_mbx_chan ) {
    if ( ! create_mbx( &input_mbx_dsc, input_mbx_name, &input_mbx_chan, 1 ))
      return ( Qnil );
    start_mbx_input();
    }
  ptr = 0;
  for ( p = process_list; p; p = p->next )
    if ( p->name == XFASTINT( name )) {
	ptr = p;
	if ( p->process_active )
	    return ( Qt );
	else
	    sys$dassgn( p->mbx_chan );
	break;
	}
  if ( ! ptr )
    ptr = xmalloc( sizeof( struct process_list ));
  if ( ! create_mbx( &output_mbx_dsc, output_mbx_name, &ptr->mbx_chan, 2 )) {
    free ( ptr );
    return ( Qnil );
    }
  if ( NULL( input_handler ))
    input_handler = Qdefault_subproc_input_handler;
  ptr->input_handler = input_handler;
  ptr->exit_handler = exit_handler;
  message( "Creating subprocess..." );
  status = lib$spawn( 0, &output_mbx_dsc, &input_mbx_dsc, &CLI$M_NOWAIT, 0,
                      &ptr->process_id, 0, 0, exit_ast, &ptr->process_active );
  if ( ! ( status & 1 )) {
    sys$dassgn( ptr->mbx_chan );
    free( ptr );
    error( "Unable to spawn subprocess" );
    return ( Qnil );
    }
  ptr->name = XFASTINT (name);
  ptr->next = process_list;
  ptr->process_active = 1;
  process_list = ptr;
  message( "Creating subprocess...done" );
  return ( Qt );
}

static void mbx_msg( ptr, msg )
struct process_list
    * ptr;
char
    * msg;
{
    write_to_mbx( ptr, msg, strlen( msg ));
}

DEFUN ("send-command-to-subprocess",
  Fsend_command_to_subprocess, Ssend_command_to_subprocess, 2, 2, 0,
  "Sends a command to a VMS subprocess.")
  (name, command)
     Lisp_Object name, command;
{
  struct process_list * ptr;

  CHECK_NUMBER( name, 0 );
  CHECK_STRING( command, 1 );
  for ( ptr = process_list; ptr; ptr = ptr->next )
    if ( XFASTINT (name) == ptr->name ) {
       write_to_mbx( ptr, XSTRING (command)->data,
                       XSTRING (command)->size );
       return ( Qt );
       }
  return ( Qnil );
}

DEFUN ("stop-subprocess", Fstop_subprocess, Sstop_subprocess, 1, 1, 0,
  "Stops a VMS subprocess.")
  (name)
     Lisp_Object name;
{
  struct process_list * ptr;

  CHECK_NUMBER( name, 0 );
  for ( ptr = process_list; ptr; ptr = ptr->next )
    if ( XFASTINT (name) == ptr->name ) {
       ptr->exit_handler = Qnil;
       if ( sys$delprc( &ptr->process_id, 0 ) & 1 )
         ptr->process_active = 0;
       return ( Qt );
       }
  return ( Qnil );
}

static int exit_ast( active )
int
    * active;
{
    process_exited = 1;
    *active = 0;
    sys$setef( process_ef );
}

/* Process to handle input on the input mailbox.
 * Searches through the list of processes until the matching PID is found,
 * then calls its input handler.
 */

process_command_input()
{
  struct process_list * ptr;
  char * msg;
  int msglen;
  Lisp_Object expr;

  msg = mbx_buffer;
  msglen = input_iosb.size;
  /* Hack around VMS oddity of sending extraneous CR/LF characters for
   * some of the commands (but not most).
   */
  if ( msglen > 0 && *msg == '\r' ) {
    msg++;
    msglen--;
    }
  if ( msglen > 0 && msg[ msglen - 1 ] == '\n' )
    msglen--;
  if ( msglen > 0 && msg[ msglen - 1 ] == '\r' )
    msglen--;
  /* Search for the subprocess in the linked list.
   */
  expr = Qnil;
  for ( ptr = process_list; ptr; ptr = ptr->next )
    if ( ptr->process_id == input_iosb.pid ) {
      expr = Fcons( ptr->input_handler,
                    Fcons( make_number( ptr->name ),
                           Fcons( make_string( msg, msglen ),
                                  Qnil )));
      break;
      }
  have_process_input = 0;
  start_mbx_input();
  if ( ! NULL( expr ))
    Feval( expr );
}

/* Searches process list for any processes which have exited.  Calls their
 * exit handlers and removes them from the process list.
 */

process_exit()
{
  struct process_list * ptr, * prev, * next;

  process_exited = 0;
  prev = 0;
  ptr = process_list;
  while ( ptr ) {
    if ( ! ptr->process_active ) {
      next = ptr->next;
      if ( prev )
	prev->next = next;
      else
	process_list = next;
      if ( ! NULL( ptr->exit_handler ))
        Feval( Fcons( ptr->exit_handler, Fcons( make_number( ptr->name ),
					        Qnil )));
      sys$dassgn( ptr->mbx_chan );
      free( ptr );
      }
    prev = ptr;
    ptr = next;
    }
}

/* Called at emacs exit.
 */

kill_processes()
{
  struct process_list * ptr;

  for ( ptr = process_list; ptr; ptr = ptr->next )
    if ( ptr->process_active ) {
      sys$dassgn( ptr->mbx_chan );
      sys$delprc( &ptr->process_id, 0 );
      }
  sys$dassgn( input_mbx_chan );
  process_list = 0;
  input_mbx_chan = 0;
}

/* Creates a temporary mailbox and retrieves its device name in 'buf'.
 * Makes the descriptor pointed to by 'dsc' refer to this device.
 * 'buffer_factor' is used to allow sending messages asynchronously
 * till some point.
 */

static int create_mbx( dsc, buf, chan, buffer_factor )
struct dsc$descriptor_s
    * dsc;
char
    * buf;
int
    * chan;
int
    buffer_factor;
{
    int strval[ 2 ];
    int status;

    status = sys$crembx( 0, chan, MSGSIZE, MSGSIZE * buffer_factor, 0, 0, 0 );
    if ( ! ( status & 1 )) {
	message( "Unable to create mailbox.  Need TMPMBX privilege." );
	return ( 0 );
	}
    strval[ 0 ] = 16;
    strval[ 1 ] = buf;
    status = lib$getdvi( &DVI$_DEVNAM, chan, 0, 0, strval,
				&dsc->dsc$w_length );
    if ( ! ( status & 1 ))
	return ( 0 );
    dsc->dsc$b_dtype = DSC$K_DTYPE_T;
    dsc->dsc$b_class = DSC$K_CLASS_S;
    dsc->dsc$a_pointer = buf;
    return ( 1 );
} /* create_mbx */

/* AST routine to be called upon receiving mailbox input.
 * Sets flag telling keyboard routines that input is available.
 */

static int mbx_input_ast()
{
  have_process_input = 1;
}

/* Issue a QIO request on the input mailbox.
 */
static void start_mbx_input()
{
  sys$qio( process_ef, input_mbx_chan, IO$_READVBLK, &input_iosb,
           mbx_input_ast, 0, mbx_buffer, sizeof( mbx_buffer),
	   0, 0, 0, 0 );
}

/* Send a message to the subprocess input mailbox, without blocking if
 * possible.
 */
static void write_to_mbx( ptr, buf, len )
struct process_list
    * ptr;
char
    * buf;
int
    len;
{
    sys$qiow( 0, ptr->mbx_chan, IO$_WRITEVBLK | IO$M_NOW, &ptr->iosb,
		       0, 0, buf, len, 0, 0, 0, 0 );
}

DEFUN ("setprv", Fsetprv, Ssetprv, 1, 3, 0,
  "Set or reset a VMS privilege.  First arg is privilege name.\n\
Second arg is t or nil, indicating whether the privilege is to be\n\
set or reset.  Default is nil.  Returns t if success, nil if not.\n\
If third arg is non-nil, does not change privilege, but returns t\n\
or nil depending upon whether the privilege is already enabled.")
  (priv, value, getprv)
     Lisp_Object priv, value, getprv;
{
  int prvmask[ 2 ], prvlen, newmask[ 2 ];
  char * prvname;
  int found, i;
  struct privilege_list * ptr;

  CHECK_STRING( priv, 0 );
  priv = Fupcase( priv );
  prvname = XSTRING( priv )->data;
  prvlen = XSTRING( priv )->size;
  found = 0;
  prvmask[ 0 ] = 0;
  prvmask[ 1 ] = 0;
  for ( i = 0; i < sizeof( priv_list ) / sizeof( priv_list[ 0 ] ); i++ ) {
    ptr = &priv_list[ i ];
    if ( prvlen == strlen( ptr->name ) &&
         bcmp( prvname, ptr->name, prvlen ) == 0 ) {
	if ( ptr->mask >= 32 )
	    prvmask[ 1 ] = 1 << ( ptr->mask % 32 );
	else
	    prvmask[ 0 ] = 1 << ptr->mask;
	found = 1;
	break;
	}
    }
  if ( ! found )
    error( "Unknown privilege name %s", XSTRING( priv )->data );
  if ( NULL( getprv )) {
    if ( sys$setprv( NULL( value ) ? 0 : 1, prvmask, 0, 0 ) & 1 )
      return ( Qt );
    return ( Qnil );
    }
  /* Get old priv value */
  if ( ! sys$setprv( 0, 0, 0, newmask ) & 1 )
    return ( Qnil );
  if ( (( newmask[ 0 ] ) & ( prvmask[ 0 ] )) ||
       (( newmask[ 1 ] ) & ( prvmask[ 1 ] )) )
    return ( Qt );
  return ( Qnil );
}

DEFUN ("set-xon", Fset_xon, Sset_xon, 0, 1, 0,
  "Set or reset XON/XOFF protocols.  If argument is non-nil, allows\n\
the XON/XOFF mode as it was at emacs initialization.  If argument is\n\
nil, XON/XOFF is disabled, to allow using ^S and ^Q as valid keys.\n\
By default, XON/XOFF is disabled.")
  (xon)
     Lisp_Object xon;
{
    set_vms_term_modes( NULL( xon ), 1 );
    return ( Qnil );
}

/* Generally useful functions for retrieving error strings.
 */

/* vmserrstr overwrites previous error strings. */

char * vmserrstr( status )
int
    status;		/* VMS status code */
{
    int bufadr[ 2 ];
    short len;
    static char buf[ 257 ];

    bufadr[ 0 ] = sizeof( buf ) - 1;
    bufadr[ 1 ] = buf;
    if ( ! ( sys$getmsg( status, &len, bufadr, 0xf, 0 ) & 1 ))
	return ( "untranslatable VMS error status" );
    buf[ len ] = '\0';
    return ( buf );
}

char * errstr()
{
    switch( errno ) {
	case 0:		return ( "error code unavailable" );
	case EPERM:	return ( "not owner" );
	case ENOENT:	return ( "no such file or directory" );
	case ESRCH:	return ( "no such process" );
	case EINTR:	return ( "interrupted system call" );
	case EIO:	return ( "i/o error" );
	case ENXIO:	return ( "no such device or address" );
	case E2BIG:	return ( "arg list too long" );
	case ENOEXEC:	return ( "exec format error" );
	case EBADF:	return ( "bad file number" );
	case ECHILD:	return ( "no children" );
	case EAGAIN:	return ( "no more processes" );
	case ENOMEM:	return ( "not enough core" );
	case EACCES:	return ( "permission denied" );
	case EFAULT:	return ( "bad address" );
	case ENOTBLK:	return ( "block device required" );
	case EBUSY:	return ( "mount device busy" );
	case EEXIST:	return ( "file exists" );
	case EXDEV:	return ( "cross-device link" );
	case ENODEV:	return ( "no such device" );
	case ENOTDIR:	return ( "not a directory" );
	case EISDIR:	return ( "is a directory" );
	case EINVAL:	return ( "invalid argument" );
	case ENFILE:	return ( "file table overflow" );
	case EMFILE:	return ( "too many open files" );
	case ENOTTY:	return ( "not a typewriter" );
	case ETXTBSY:	return ( "text file busy" );
	case EFBIG:	return ( "file too large" );
	case ENOSPC:	return ( "no space left on device" );
	case ESPIPE:	return ( "illegal seek" );
	case EROFS:	return ( "read-only file system" );
	case EMLINK:	return ( "too many links" );
	case EPIPE:	return ( "broken pipe" );
	case EDOM:	return ( "math argument" );
	case ERANGE:	return ( "result too large" );
	case EWOULDBLOCK:	return ( "I/O operation would block channel" );
	case EVMSERR:	return ( vmserrstr( vaxc$errno ));
	}
    return ( "Unknown Error Code" );
}

init_vmsfns()
{
  process_list = 0;
  input_mbx_chan = 0;
}

syms_of_vmsfns()
{
  defsubr( &Sdefault_subproc_input_handler );
  defsubr( &Sspawn_subprocess );
  defsubr( &Ssend_command_to_subprocess );
  defsubr( &Sstop_subprocess );
  defsubr( &Ssetprv );
  defsubr( &Sset_xon );
  Qdefault_subproc_input_handler = intern( "default-subprocess-input-handler" );
  staticpro( &Qdefault_subproc_input_handler );
}
#endif
