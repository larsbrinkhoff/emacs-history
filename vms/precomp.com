$!
$! VMS command file to define the `ccom' command.
$! This version allows the current directory to be part of a search list.
$!
$ dirname = f$environment("PROCEDURE")
$ gosub parse_dir
$ dirname2 = dirname - "]" + ".]"
$ define /nolog sys vaxc$include
$ define /nolog x11 decw$include
$ dir = f$environment("DEFAULT")
$ dir = f$extract(0,f$locate(":",dir),dir)
$ if f$trnlnm("vaxc$include",,,,,"MAX_INDEX") .eq. 0
$  then
$   define /nolog vaxc$include sys$library,sys$disk:[]
$  else
$   x = 1
$   dirlist = f$trnlnm("vaxc$include")
$   loop:
$   next = f$trnlnm("vaxc$include",,x)
$   if next .nes. ""
$    then
$     if next .nes. dir then dirlist = dirlist + ",''next'"
$     x = x + 1
$     goto loop
$    endif
$   dirlist = dirlist + ",''dir'"
$   define /nolog vaxc$include 'dirlist'
$  endif
$ ccom :== cc /debug /define=("""emacs""",HAVE_CONFIG_H) /nolist
$ sh log sys
$ sh log x11
$ exit
$
$
$parse_dir:
$   dirname=f$parse("A.;0",dirname,,,"NO_CONCEAL") - "A.;0"
$   dirname=dirname - ".][000000" - ".><000000" - ".>[000000" - ".]<000000"
$   dirname=dirname - "][" - "><" - ">[" - "]<"
$   if f$element(1,"<",dirname) .nes. "<" then -
	dirname = f$element(0,"<",dirname) + "[" + f$element(1,"<",dirname)
$   if f$element(1,">",dirname) .nes. ">" then -
	dirname = f$element(0,">",dirname) + "]" + f$element(1,">",dirname)
$ ! write sys$output "''dirname'
$   return
