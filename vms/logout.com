$! Roland Blaine Roberts (roberts@curie.nsrl.rochester.edu)
$! Nuclear Structure Research Laboratory
$! University of Rochester
$! Monday, 11 July 1988
$!
$! Command procedure to logout "safely".  The logout is aborted unless the
$! current process has no subprocesses.
$!
$! Arguments:
$!   p1 "KILL" means logout regardless of subprocesses.  If this is specified,
$!      the subprocesses are stopped before attempting to logout.  (Under one
$!      release of VMS v5, the subprocesses would not automatically be stopped
$!      and UIS windows would hang.)
$!
$!
$ set noon
$ on control_y then continue
$ verify = f$verify(0)
$ privileges = f$setprv("NOWORLD,NOGROUP")
$ ding[0,8] = 7
$ err := write sys$error
$ on control_y then goto abort
$!
$! Check for the existence of subprocecess before allowing logout.
$! Loop through the process list to find out if the emacs process exists.
$!
$ ctx  = 0
$ me   = f$process ()
$ mid  = ""
$ whoami:
$   pid  = f$pid(ctx)
$   proc = f$getjpi(pid,"PRCNAM")
$   if proc .eqs. me 
$   then mid = pid
$   else goto whoami
$   endif
$ me   = mid
$ ctx  = 0
$ oops = 0
$ loop:
$   pid   = f$pid(ctx)
$   proc  = f$getjpi(pid,"PRCNAM")
$   mode  = f$getjpi(pid,"MODE")
$   owner = f$getjpi(pid,"OWNER")
$   if pid .eqs. me
$   then 
$     if ctx .ne. 0
$     then goto loop
$     else goto oops
$     endif
$   endif
$!   if proc .eqs. "" then goto oops
$   if (p1 .eqs. "KILL")
$   then 
$     if ((pid.nes.me) .and. (mode.eqs."INTERACTIVE") .and. (owner.eqs.me))
$     then	
$       err "%LOGOUT-I-STOPPRC, Stopping subprocess ''pid' ""''proc'"""
$       stop "''proc'"
$     endif
$   else
$     if (pid.nes.me) .and. (mode.eqs."INTERACTIVE") .and. (owner.eqs.me)
$     then
$       if oops
$       then err "-LOGOUT-W-PRCRUN, Subprocess ''pid' ""''proc'"" running!"
$       else err "''ding'%LOGOUT-W-PRCRUN, Subprocess ''pid' ""''proc'"" running!"
$       endif
$       oops = 1
$     endif
$   endif
$   if ctx .ne. 0 then goto loop
$ oops:
$   if oops
$   then
$     err "''ding'%LOGOUT-F-ABORT, Subprocesses are running"
$     err "''ding'                 type LOGOUT KILL to logout anyway."
$     set proc/privileges=( 'privileges' )
$     if verify then set verify
$     exit
$   endif
$!
$! The command 'logout' is likely to be defined to this command procedure.
$! Redefine it.
$!
$ logout:
$   set symbol/scope=(noglobal,nolocal)
$   logout/full/hangup
$ abort:
$   set process/privileges=( 'privileges' )
$   if verify then set verify
$   err "''ding'%LOGOUT-E-USRABORT, Logout aborted by user."
$   exit
