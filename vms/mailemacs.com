$! Roland Blaine Roberts (roberts@nsrl31.nsrl.rochester.edu)
$! Nuclear Structure Research Laboratory
$! University of Rochester
$! Friday, 17 April 1992 
$!
$! Command procedure to invoke Emacs for VMS MAIL editor.  The logical
$! name MAIL$EDIT should translate to this file.
$!
$! Arguments:
$!   p1 = Input file name.
$!   p2 = Output file name.
$!
$! Logical Names:
$!   MAIL$TRAILER file to be appended to the message
$!
$ args = "-f vms-pmail-setup"
$ if (p1 .nes. "") .and. (f$search(p1) .nes. "") then -
     args = "-i ''f$search(p1)' ''args' -f indicate-mail-reply-text"
$ trailer = f$trnlnm("MAIL$TRAILER")
$ if trailer .nes. "" then -
     args = args + " -f insert-signature"
$ emacs "-q" 'p2' "''args'"
$ exit

