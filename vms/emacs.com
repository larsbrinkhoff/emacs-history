$! VMS command file to make the definitions needed to run the installed Emacs.
$! You must execute this in each session in order to run Emacs or else it must
$! be executed by the system at each boot.
$!
$! If you execute at boot time, specify "/SYSTEM" as the first parameter.
$!
$! This file must reside in the [.vms] subdirectory of the Emacs tree when it
$! is executed, because it uses its own directory to initialize a logical name.
$!
$ home = f$environment("DEFAULT")
$ path = f$environment("PROCEDURE")
$ path = f$extract(0,f$locate("]",path)+1,path)
$ set default 'path'
$ set default [-]
$ fdev = f$parse(f$environment("DEFAULT"),,,"DEVICE")-":"
$ fdir = f$parse(f$environment("DEFAULT"),,,"DIRECTORY")-"["
$ ftrn = f$trnlnm(fdev)
$ if ftrn .eqs. "" then ftrn = fdev + ":[]"
$ ndef = ftrn - "]" + fdir - ".000000"
$ base = ndef - "]" + ".]"
$ set default 'home'
$!
$ define 'p1' /translation=concealed emacs_library 'base'
$!
$! This should really not be here, but is defined to allow testing to work
$! without interfering with system logical.
$!
$ define 'p1' emacsloadpath emacs_library:[lisp]
$!
$! The following logical name is needed for M-x shell to work.
$! process.c\create_process keys on the string *dcl*.
$!
$ define 'p1' eshell "*dcl*"
$!
$ runemacs :== $emacs_library:[000000]emacs -map emacs_library:[000000]emacs.dump
$ emacs :== @emacs_library:[vms]kepteditor emacs
