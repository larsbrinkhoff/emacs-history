#### Makefile for the Emacs User's Manual.
#### Just a few targets required by the coding standards.

MAKEINFO = makeinfo
TEXINDEX_OBJS = texindex.o getopt.o
INFO_TARGETS = ../info/emacs ../info/cl ../info/forms ../info/vip \
	       ../info/gnus ../info/sc ../info/info ../info/dired-x
DVI_TARGETS = emacs.dvi cl.dvi forms.dvi vip.dvi gnus.dvi sc.dvi dired-x.dvi
INFOSOURCES = info.texi info-stnd.texi

# The following rule does not work with all versions of `make'.
.SUFFIXES: .texi .dvi
.texi.dvi:
	texi2dvi $<

TEXI2DVI = texi2dvi

EMACSSOURCES= \
	emacs.texi \
	screen.texi \
	commands.texi \
	entering.texi \
	basic.texi \
	mini.texi \
	m-x.texi \
	help.texi \
	mark.texi \
	killing.texi \
	regs.texi \
	display.texi \
	search.texi \
	fixit.texi \
	files.texi \
	buffers.texi \
	windows.texi \
	frames.texi \
	major.texi \
	indent.texi \
	text.texi \
	programs.texi \
	building.texi \
	abbrevs.texi \
	picture.texi \
	sending.texi \
	rmail.texi \
	dired.texi \
	calendar.texi \
	misc.texi \
	custom.texi \
	trouble.texi \
	cmdargs.texi \
	anti.texi \
	gnu.texi \
	gnu1.texi \
	glossary.texi

info: $(INFO_TARGETS)

dvi: $(DVI_TARGETS)

texindex: ${TEXINDEX_OBJS}
	$(CC) -o texindex ${TEXINDEX_OBJS}

texindex.o: texindex.c
	$(CC) -Demacs -I../src -I../lib-src -c texindex.c

getopt.o: getopt.c
	$(CC) -Demacs -I../src -I../lib-src -c getopt.c

../info/info: ${INFOSOURCES}
	$(MAKEINFO) --no-split info.texi

info.dvi: texindex ${INFOSOURCES}
	$(TEXI2DVI) info.texi

../info/emacs: ${EMACSSOURCES}
	$(MAKEINFO) emacs.texi

emacs.dvi: texindex ${EMACSSOURCES}
	$(TEXI2DVI) emacs.texi

../info/cl: cl.texi
	$(MAKEINFO) cl.texi
cl.dvi: texindex cl.texi
	$(TEXI2DVI) cl.texi

../info/dired-x: dired-x.texi
	$(MAKEINFO) dired-x.texi
dired-x.dvi: texindex dired-x.texi
	$(TEXI2DVI) dired-x.texi

../info/forms: forms.texi
	$(MAKEINFO) forms.texi
forms.dvi: texindex forms.texi
	$(TEXI2DVI) forms.texi

../info/sc: sc.texi
	$(MAKEINFO) sc.texi
sc.dvi: texindex sc.texi
	$(TEXI2DVI) sc.texi

../info/vip: vip.texi
	$(MAKEINFO) vip.texi
vip.dvi: texindex vip.texi
	$(TEXI2DVI) vip.texi

../info/gnus: gnus.texi
	$(MAKEINFO) gnus.texi
gnus.dvi: texindex gnus.texi
	$(TEXI2DVI) gnus.texi

../etc/GNU: gnu1.texi gnu.texi
	makeinfo --no-headers -o ../etc/GNU gnu1.texi

mostlyclean:
	rm -f *.log *.cp *.fn *.ky *.pg *.vr *.o core

clean: mostlyclean
	rm -f *.dvi

distclean: clean
	rm -f texindex

realclean: distclean
	rm -f *.aux *.cps *.fns *.kys *.pgs *.vrs
# Don't delete these, because they are outside the current directory.
#	for file in $(INFO_TARGETS); do rm -f $${file}*; done
