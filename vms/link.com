$!
$! VMS command file to link the .OBJ files of Emacs, producing `temacs.exe'.
$!
$ set default [-.src]
$ set verify
$ link/exe=[-.vms]temacs.exe/map=[-.vms]temacs.map sys$input/opt
CLUSTER=EMACS,,,-
dispnew.obj,-
frame.obj,-
scroll.obj,-
xdisp.obj,-
window.obj,-
term.obj,-
cm.obj,-
! Only include the following ones if you compiled for X
xterm.obj,-
xfns.obj,-
xfaces.obj,-
xmenu.obj,-
xselect.obj,-
xrdb.obj,-
! end of X files
emacs.obj,-
keyboard.obj,-
macros.obj,-
keymap.obj,-
sysdep.obj,-
buffer.obj,-
filelock.obj,-
insdel.obj,-
marker.obj,-
intervals.obj,-
textprop.obj,-
minibuf.obj,-
fileio.obj,-
dired.obj,-
filemode.obj,-
cmds.obj,-
casetab.obj,-
casefiddle.obj,-
indent.obj,-
search.obj,-
regex.obj,-
undo.obj,-
alloc.obj,-
data.obj,-
doc.obj,-
editfns.obj,-
callint.obj,-
eval.obj,-
floatfns.obj,-
fns.obj,-
print.obj,-
lread.obj,-
abbrev.obj,-
syntax.obj,-
mocklisp.obj,-
bytecode.obj,-
process.obj,-
callproc.obj,-
vmsfns.obj,-
vmsproc.obj,-
doprnt.obj,-
getloadavg.obj,-
vmsmap.obj,-
termcap.obj,-
tparam.obj,-
lastfile.obj,-
vmstime.obj,-
vmsgmalloc.obj,-
vm-limit.obj,-
alloca.obj
COLLECT=NON_SAVED_DATA,-
stdin,-
stdout,-
stderr,-
errno,-
vaxc$errno,-
sys_errlist,-
sys_nerr,-
environ
[-.oldxmenu]libxmenu11/lib
! If you don't have MultiNet, comment the following line.
!multinet:multinet_socket_library/share
! If you don't have UCX, comment the following line.
sys$library:ucx$ipc/library ! You cannot link in the shareable.
! If you don't link for X, comment the following line.
sys$share:decw$xlibshr/share
SYS$SHARE:VAXCRTL/SHARE ! Always link with this library.
$! 'f$verify(0)
$ set default [-.vms]
$ exit
