# VFLAGS = -Dux3 -DNDLAY
VFLAGS=
UFLAGS = -Dux6

FILES =  README emacs.tm macro.mm \
	 emacs_main.c emacs_io.c emacs_buf.c emacs_disp.c emacs_cmds.c \
	 emacs_re.c make_help.c helpstrings emacs_errors stat_proc.c\
	 emacs_gb.h emacs_main.h emacs_help.h emacs_buf.h emacs_cmds.h \
	 emacs_disp.h emacs_io.h makefile emacs_vt.c xusend.c emacs_vt.mm \
	 MACROS recent_changes see_it.c TERMINALS emacs.man dired.man \
	 emacs_ux6.c
SOURCES = emacs_main.c emacs_io.c emacs_buf.c emacs_disp.c \
	emacs_cmds.c emacs_re.c emacs_ux6.c

CPROGS = emacs_main.c emacs_io.c emacs_buf.c emacs_disp.c emacs_cmds.c \
	 emacs_re.c make_help.c helpstrings emacs_errors stat_proc.c\
	 emacs_gb.h emacs_main.h emacs_help.h emacs_buf.h emacs_cmds.h \
	 emacs_disp.h emacs_io.h makefile emacs_vt.c emacs_ux6.c
 
P = opr -f xerox

xemacs: emacs_main.o emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o emacs_ux6.o helpfile errfile terminals.x
	cc -i emacs_main.o emacs_re.o emacs_cmds.o emacs_buf.o emacs_disp.o\
emacs_io.o emacs_ux6.o -o a.out
	mv a.out xemacs
	size xemacs

semacs: emacs_main.o emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o emacs_ux6.o helpfile errfile terminals.x
	cc  emacs_main.o emacs_re.o emacs_cmds.o emacs_buf.o emacs_disp.o\
 emacs_io.o emacs_ux6.o
	mv a.out semacs

emacs: emacs.xopr macro.xopr
	ln /usr/obin/emacs /usr/obin/oemacs
	rm /usr/obin/emacs
	ln /usr/obin/nemacs /usr/obin/emacs

nemacs: emacs_main.o emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o emacs_ux6.o helpfile errfile terminals.x
	cc -i emacs_main.o emacs_re.o emacs_cmds.o emacs_buf.o emacs_disp.o\
 emacs_io.o emacs_ux6.o
	mv a.out /usr/obin/nemacs
emacs_main.o: emacs_gb.h emacs_main.h emacs_help.h emacs_main.c emacs_io.h
	cc -c -O $(UFLAGS) -DSDIR=\"/lib emacs_main.c
emacs_main.p: emacs_gb.h emacs_main.h emacs_help.h emacs_main.c emacs_io.h
	cc -c -O $(UFLAGS) -DSDIR=\"/lib -DMONITOR emacs_main.c
	mv emacs_main.o emacs_main.p

emacs_vt.o: emacs_vt.c terminals.x
	cc -c -O -DSDIR=\"/lib $(VFLAGS) emacs_vt.c

emacs_vm.o: emacs_vm.c terminals.x
	cc -c -O -DSDIR=\"/lib $(VFLAGS) emacs_vm.c

emacs_cmds.o:	emacs_gb.h emacs_cmds.h emacs_cmds.c emacs_io.h
	cc -c -O emacs_cmds.c

emacs_disp.o: emacs_gb.h emacs_disp.h emacs_disp.c emacs_io.h
	cc -c -O emacs_disp.c

emacs_buf.o: emacs_buf.h emacs_gb.h emacs_buf.c emacs_io.h
	cc -c -O $(UFLAGS) emacs_buf.c

emacs_io.o: emacs_io.h emacs_gb.h
	cc -c -O $(UFLAGS) $(VFLAGS) emacs_io.c

emacs_re.o: emacs_io.h emacs_gb.h
	cc -c -O emacs_re.c

emacs_ux6.o: emacs_gb.h emacs_io.h
	cc -c -O emacs_ux6.c

emacs.nm: xemacs
	nm -ts xemacs > emacs.nm
	nm -ds xemacs >> emacs.nm
	tpr emacs.nm | lpr

dired: dired_main.o emacs_cmds.o dired_buf.o emacs_disp.o emacs_io.o \
	emacs_re.o dired_ux6.o helpfile errfile terminals.x
	cc -i dired_main.o emacs_re.o dired_buf.o emacs_cmds.o emacs_disp.o\
 emacs_io.o dired_ux6.o
	mv a.out /usr/obin/dired

ndired: dired_main.o emacs_cmds.o dired_buf.o emacs_disp.o emacs_io.o \
	emacs_re.o dired_ux6.o helpfile errfile terminals.x
	cc -i dired_main.o emacs_re.o dired_buf.o emacs_cmds.o emacs_disp.o\
 emacs_io.o dired_ux6.o
	mv a.out ndired

dired_main.o: emacs_gb.h emacs_help.h emacs_main.h emacs_main.c
	cp emacs_main.c dired_main.c
	cc -c -O $(UFLAGS) -DDIRED -DSDIR=\"/lib dired_main.c
	rm dired_main.c

dired_buf.o: emacs_gb.h emacs_buf.h emacs_buf.c
	cp emacs_buf.c dired_buf.c
	cc -c -O $(UFLAGS) -DDIRED dired_buf.c
	rm dired_buf.c

dired_ux6.o: emacs_gb.h emacs_io.h
	cp emacs_ux6.c dired_ux6.c
	cc -c -O -DDIRED dired_ux6.c
	rm dired_ux6.c

profile: emacs_main.p emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o helpfile errfile
	cc -i -p emacs_main.p emacs_cmds.o emacs_buf.o emacs_disp.o emacs_io.o\
 emacs_re.o emacs_ux6.o
	mv a.out xemacs

print: $(CPROGS)
	tpr $? | $P
	echo "" >print

/lib/helpfile: helpstrings make_help
	make_help <helpstrings >/lib/helpfile

/lib/errfile: emacs_errors make_help
	make_help <emacs_errors >/lib/errfile

make_help: make_help.c
	cc -o make_help make_help.c -lS

see_it: see_it.c
	cc -o see_it see_it.c

MACROS: macros/*
	ar r MACROS macros/*

crib: crib.nr
	nroff -TX crib.nr | npf -i0 -TX -l137 -w135 -s | opr -o0 -r -f x2on1

emacs.xopr: emacs.tm
	xnroff -rB3 emacs.tm > emacs.xopr
	xopr emacs.xopr
macro.xopr: macro.mm
	xnroff macro.mm > macro.xopr
	xopr macro.xopr
emacs_vt.xopr: emacs_vt.mm
	xnroff emacs_vt.mm > emacs_vt.xopr
	xopr emacs_vt.xopr
lint:
	lint  -xn -DSDIR=\" $(VFLAGS) $(SOURCES)

tape16: MACROS
	ar r TERMINALS /lib/terminals/*
	ls $(FILES) | cpio -oB > /dev/rmt8
tape:  MACROS 
	ar r TERMINALS /lib/terminals/*
	ls $(FILES) | cpio -o > /dev/mt0
usend: MACROS
	ar r TERMINALS /lib/terminals/*
	xusend "-D $(DEST) -u $(USER)" $(FILES)
nusend: MACROS
	ar r TERMINALS /lib/terminals/*
	nusend -D $(DEST) -u $(USER) $(CPROGS)
	nusend -D $(DEST) -u $(USER) README emacs.tm macro.mm MACROS  \
	 recent_changes TERMINALS emacs.man dired.man
tartape: MACROS
	ar r TERMINALS /lib/terminals/*
	tar -c $(FILES)
tptape: MACROS
	ar r TERMINALS /lib/terminals/*
	tp rmc $(FILES)

terminals.x: TERMINALS
	-if { ! -d /lib/terminals } ; then mkdir /lib/terminals; chmod 755 /lib/terminals; fi
	echo "" >terminals.x
	cd /lib/terminals;ar x /src/src/emacs/TERMINALS
graveyard:
	mkdir $(VERSION)
	cp $(FILES) $(VERSION)

playpen:
	mkdir playpen
	cpall $(FILES) playpen

promote:
	ar r TERMINALS /lib/terminals/*
	cpall $(FILES) ..
windows: emacs_vt.o windows.c
	cc -o windows emacs_vt.o windows.c -lNSS
