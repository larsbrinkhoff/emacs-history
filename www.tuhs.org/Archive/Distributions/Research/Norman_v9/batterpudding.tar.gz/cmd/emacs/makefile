CC = newcc
VFLAGS = -Dv8 -DTERMCAP -DPORTEXT
BINDIR = /usr/lbin
LIBDIR = /usr/lib/emacs
MANDIR = /lusr/man

FILES =  README term_support emacs.tm macro.mm \
	 emacs_main.c emacs_io.c emacs_buf.c emacs_disp.c emacs_cmds.c \
	 emacs_re.c make_help.c helpstrings emacs_errors stat_proc.c\
	 emacs_gb.h emacs_main.h emacs_buf.h emacs_cmds.h \
	 emacs_disp.h emacs_io.h makefile emacs_vt.c xusend.c emacs_vt.mm \
	 MACROS MACRO_SOURCE recent_changes see_it.c TERMINALS \
	 emacs.man dired.man ecomp.c ecomp.man emacs_defs\
	 ttest.c windows.c windows.man homedir.c\
	 findtty.c stddefs.h buildmacs hyde.c kbase
SOURCES = emacs_main.c emacs_io.c emacs_buf.c emacs_disp.c \
	emacs_cmds.c emacs_re.c 

CPROGS = emacs_main.c emacs_io.c emacs_buf.c emacs_disp.c emacs_cmds.c \
	 emacs_re.c make_help.c helpstrings emacs_errors stat_proc.c\
	 emacs_gb.h emacs_main.h emacs_buf.h emacs_cmds.h \
	 emacs_disp.h emacs_io.h makefile emacs_vt.c ttest.c
 
P = xpr

xemacs: emacs_main.o emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o version.o helpfile errfile ecomp
	$(CC) emacs_main.o emacs_re.o emacs_cmds.o emacs_buf.o \
	emacs_disp.o emacs_io.o version.o -ltermcap
	mv a.out xemacs
	size xemacs

ecomp: ecomp.c terminals.x
	$(CC) -O -DSDIR=\"`cat terminals.x`\" -o ecomp ecomp.c 
xecomp: xecomp.c terminals.x
	$(CC) -O -DSDIR=\"`cat terminals.x`\" -o xecomp xecomp.c 
semacs: emacs_main.o emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o version.o helpfile errfile
	$(CC)  emacs_main.o emacs_re.o emacs_cmds.o emacs_buf.o emacs_disp.o emacs_io.o version.o 
	mv a.out semacs

install:
	echo $(LIBDIR) > terminals.x
	make xemacs
	make ndired
	make ecomp
	-mkdir $(LIBDIR)
	-mkdir $(LIBDIR)/macros
	-mkdir $(LIBDIR)/macro_source
	-mkdir $(LIBDIR)/terminals
	cp MACROS $(LIBDIR)/macros
	cp MACRO_SOURCE $(LIBDIR)/macro_source
	cp TERMINALS $(LIBDIR)/terminals
	cd $(LIBDIR)/macros;cpio -ic < MACROS;rm MACROS
	cd $(LIBDIR)/macro_source;cpio -ic < MACRO_SOURCE;rm MACRO_SOURCE
	cd $(LIBDIR)/terminals;cpio -ic < TERMINALS;rm TERMINALS
	cp helpfile $(LIBDIR)/helpfile
	cp errfile $(LIBDIR)/errfile
	cp emacs_defs $(LIBDIR)/emacs_defs
	chmod +x buildmacs
	buildmacs $(LIBDIR)/macro_source
	cp emacs.man $(MANDIR)/man1/emacs.1
	cp dired.man $(MANDIR)/man1/dired.1
	cp ecomp.man $(MANDIR)/man1/ecomp.1
	cp ndired $(BINDIR)/dired
	cp ecomp $(BINDIR)/ecomp
	cp xemacs $(BINDIR)/emacs

emacs_main.o: emacs_gb.h emacs_main.h emacs_main.c emacs_io.h terminals.x
	$(CC) -c -DSDIR=\"`cat terminals.x`\" $(VFLAGS) emacs_main.c
emacs_main.p: emacs_gb.h emacs_main.h emacs_main.c emacs_io.h terminals.x
	$(CC) -c -DSDIR=\"`cat terminals.x`\" $(VFLAGS) -DMONITOR emacs_main.c
	mv emacs_main.o emacs_main.p

emacs_vt.o: emacs_vt.c terminals.x 
	$(CC) -c -DSDIR=\"`cat terminals.x` $(VFLAGS) emacs_vt.c

emacs_vm.o: emacs_vm.c terminals.x
	$(CC) -c -DSDIR=\"`cat terminals.x` $(VFLAGS) emacs_vm.c

emacs_cmds.o:	emacs_gb.h emacs_cmds.h emacs_cmds.c emacs_io.h 
	$(CC) -c $(VFLAGS) emacs_cmds.c 

emacs_disp.o: emacs_gb.h emacs_disp.h emacs_disp.c emacs_io.h
	$(CC) -c $(VFLAGS) emacs_disp.c

emacs_buf.o: emacs_buf.h emacs_gb.h emacs_buf.c emacs_io.h
	$(CC) -c $(VFLAGS) emacs_buf.c

emacs_io.o: emacs_io.h emacs_gb.h
	$(CC) -c $(VFLAGS) emacs_io.c

emacs_re.o: emacs_io.h emacs_gb.h
	$(CC) -c $(VFLAGS) emacs_re.c

emacs.nm: xemacs
	nm -ts xemacs > emacs.nm
	nm -ds xemacs >> emacs.nm
	tpr emacs.nm | lpr

dired: dired_main.o emacs_cmds.o dired_buf.o emacs_disp.o emacs_io.o \
	emacs_re.o version.o helpfile errfile
	$(CC) -i dired_main.o emacs_re.o dired_buf.o emacs_cmds.o emacs_disp.o emacs_io.o version.o
	mv a.out /usr/obin/dired

ndired: dired_main.o emacs_cmds.o dired_buf.o emacs_disp.o emacs_io.o \
	emacs_re.o version.o helpfile errfile
	$(CC) -i dired_main.o emacs_re.o dired_buf.o emacs_cmds.o \
	emacs_disp.o emacs_io.o version.o -ltermcap
	mv a.out ndired

dired_main.o: emacs_gb.h emacs_main.h emacs_main.c terminals.x
	cp emacs_main.c dired_main.c
	$(CC) -c -O -DDIRED $(VFLAGS) -DSDIR=\"`cat terminals.x`\" dired_main.c
	rm dired_main.c

dired_buf.o: emacs_gb.h emacs_buf.h emacs_buf.c
	cp emacs_buf.c dired_buf.c
	$(CC) -c -O -DDIRED $(VFLAGS) dired_buf.c 
	rm dired_buf.c

profile: emacs_main.p emacs_cmds.o emacs_buf.o emacs_disp.o \
	emacs_io.o emacs_re.o version.o helpfile errfile
	$(CC) -i emacs_main.p emacs_cmds.o emacs_buf.o emacs_disp.o emacs_io.o emacs_re.o version.o
	mv a.out xemacs

print: $(CPROGS)
	tpr $? | $P
	touch print

helpfile: helpstrings make_help
	make_help <helpstrings >helpfile

errfile: emacs_errors make_help
	make_help <emacs_errors >errfile

make_help: make_help.c
	$(CC) -o make_help make_help.c

see_it: see_it.c
	$(CC) -o see_it see_it.c

MAKE_TERM:
	cd macros;ls | cpio -oc > ../MACROS
	cd macro_source;ls *.e | cpio -oc > ../MACRO_SOURCE
	cd terminals;ls | cpio -oc > ../TERMINALS

crib: crib.nr
	nroff -TX crib.nr | npf -i0 -TX -l137 -w135 -s | opr -o0 -r -p -txr -p2on1

emacs.tm.x: emacs.tm
	xroff -cs -prt emacs.tm
macro.mm.x: macro.mm
	xroff -cs -prt -tbl macro.mm
emacs_vt.x: emacs_vt.mm
	xroff -cs -prt emacs_vt.mm
lint:
	lint  -xn -DSDIR=\" \" $(VFLAGS) $(SOURCES)

tape16: MAKE_TERM
	ls $(FILES) | cpio -oB > /dev/rmt8

tape:  MAKE_TERM
	ls $(FILES) | cpio -oB > /dev/rmt0

tape3b:  MAKE_TERM
	ls $(FILES) | cpio -oc > /dev/mt8
tapex:  MAKE_TERM
	ls $(FILES) | cpio -ocB > /dev/rmt8

usend: MAKE_TERM
	xusend "-d $(DEST) -u $(USER)"  $(FILES)
	echo $(DEST) $(USER) `date` >> /n1/warren/emacs/send_list
uusend: MAKE_TERM
	uusend -d $(DEST) -u $(USER)  $(FILES)
	echo $(DEST) $(USER) `date` >> /n1/warren/emacs/send_list
qsend:
	xusend "-d $(DEST) -u $(USER) -a $(ACCT)" $(FILES)
	echo $(DEST) $(USER) `date` >> /n1/warren/emacs/send_list
up:
	find $(FILES) -newer $(DEST).emx -print > flist
	echo "-D $DEST) -u $(USER)" `cat flist`
	touch $(DEST).emx

uucp: MAKE_TERM
	uuto $(FILES) $(DEST)!$(USER)

tartape: MAKE_TERM
	tar -c $(FILES)

tar16:	
	tar -cbf 10 /dev/rmt8 $(FILES)
tptape: MAKE_TERM
	tp rmc $(FILES)

terminals.x: TERMINALS
	pwd > terminals.x
	-if [ ! -d terminals ]; then mkdir terminals; chmod 755 terminals; fi
	-if [ ! -d macros ]; then mkdir macros; chmod 755 macros; fi
	-if [ ! -d macro_source ]; then mkdir macro_source; chmod 755 macros; fi 
	cd macros;cpio -ic < ../MACROS
	cd macro_source;cpio -ic < ../MACRO_SOURCE
	cd terminals;cpio -ic < ../TERMINALS
version.o: makefile
	-rm -f version.c version.o
	echo 'char version[] = "4.9" ;' > version.c
	echo char hdate[] = \"`date`\"\; >> version.c
	$(CC) -c version.c

findtty.o: findtty.c
	$(CC) -c findtty.c
homedir.o: homedir.c
	$(CC) -c homedir.c
windows: emacs_vt.o windows.c findtty.o homedir.o
	$(CC) -o windows emacs_vt.o findtty.o homedir.o windows.c 
ttest: emacs_vt.o ttest.c
	$(CC) -o ttest emacs_vt.o ttest.c

hyde: hyde.c
	$(CC) -O -DDIAGNOSE -o hyde hyde.c
