$! Command file to build Emacs on VMS and create a backup saveset.
$!
$! Argument list
$!   P1 (opt) name of the tape output device.
$!
$! History
$!   rms, 16 May 1988
$!     original version
$!   rbr, 30 Oct 1992, roberts@nsrl31.nsrl.rochester.edu
$!     modified to make this location independent.
$!   
$!
$! We assume this file is in disk:[dir.emacs19.vms], where `disk' may be
$! a concealed device name.  The default directory is moved to the location
$! of this file, and the backup saveset will be contain disk:[dir...]*.*
$!
$! We also assume that the tar-tape reader has changed all `-' to `_'
$! in filenames.  If this is not so, delete the first `@allrename' command.
$!
$! You must also edit the mag tape device name
$! And the Emacs version that appears in the saveset name.
$!
$!
$! Get a valid tape device name
$!
$ tape = p1
$ prompt:
$ if tape .eqs. ""
$ then inquire tape "Tape device destination for distribution files" 
$ endif
$ if f$getdvi(tape,"EXISTS") .eqs. "FALSE" then goto prompt
$ if f$getdvi(tape,"DEVCLASS") .ne. 2 then goto prompt
$ if f$locate(":",tape) .eq. f$length(tape) then tape = tape + ":"
$!
$! Set the default directory to the root of the emacs distribution
$!
$ home = f$environment("DEFAULT")
$ path = f$environment("PROCEDURE")
$ path = f$extract(0,f$locate("]",path)+1,path)
$ set default 'path'
$!
$! Change to VMS 4.4 filenames.
$!
$ @[.vms]allrename [...] "_" "-"
$!
$! Edit the configuration files.
$!
$ set default [-.src]
$ copy vmspaths.h paths.h
$ copy config.h-dist config.h
$ edit/edt/nocommand config.h
s/"@opsystemi@"/"[.s]vms4-4.h"/w
s/"@machine@"/"[.m]vms.h"/w
exit
$!
$! Compile, link and dump Emacs.
$!
$ @compile
$ @link
$ @[-.vms]complink
$ @[-.vms]makedoc
$ @build
$!
$! Move the executable and image to the appropriate place.
$!
$ rename temacs.exe  [-]emacs.exe
$ rename temacs.dump [-]emacs.dump
$ copy sys$library:vaxcrtl.olb vaxcrtl.olb
$!
$! Recompile a few files for vms version 4.2.
$! Call these object files for 4.2 `.jbo'.
$!
$ edit/edt/nocommand config.h
s/vms4-4/vms4-2/w
exit
$ rename doc.obj doc.obx
$ rename fileio.obj fileio.obx
$ rename sysdep.obj sysdep.obx
$ rename vmsfns.obj vmsfns.obx
$ 'ccom' doc
$ 'ccom' fileio
$ 'ccom' sysdep
$ 'ccom' vmsfns
$ rename doc.obj doc.jbo
$ rename fileio.obj fileio.jbo
$ rename sysdep.obj sysdep.jbo
$ rename vmsfns.obj vmsfns.jbo
$ rename *.obx *.obj
$ delete config.h;-1
$!
$! Change all files back to the names that VMS 4.2 can read in.
$!
$ set default [-]
$ @allrename [...] "-" "_"
$!
$! Set up the device name for the backup, assume we are located in [.vms]
$! of the Emacs directory tree.
$!
$ olddef = f$environment("DEFAULT")
$ path = f$environment("PROCEDURE")
$ path = f$extract(0,f$locate("]",path)+1,path)
$ set default 'path'
$ set default [-.-]
$ path = f$environment("DEFAULT")
$ set default 'olddef'
$ if f$parse(path,,,"DEVICE") .nes. f$parse(path,,,"DEVICE","NO_CONCEAL")
$ then
$   disk = f$parse(path,,,"DEVICE","NO_CONCEAL")
$   disk = disk + f$parse(path,,,"DIRECTORY") - "]" + ".]"
$ else
$   disk = path - "]" + ".]"
$ endif
$!
$! Dump onto tape.
$!
$ mount /foreign 'tape'
$ define /translation=concealed dumping 'disk'
$ set def dumping:[emacs]
$ backup /interchange /verify /list [...] 'tape'emacs19.0
$ dismount 'tape'
$!
$ set default 'home'
$ exit