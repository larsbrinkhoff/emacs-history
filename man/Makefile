#### Makefile for the Emacs User's Manual.
#### Just a few targets required by the coding standards.

info: ../info/emacs ../info/cl.info ../info/forms ../info/vip ../info/gnus ../info/sc

dvi: emacs.dvi cl.dvi forms.dvi gnus.dvi sc.dvi vip.dvi
.texi.dvi:
	tex $<

EMACSSOURCES= \
	emacs.texi \
	screen.texi \
	commands.texi \
	entering.texi \
	basic.texi \
	undo.texi \
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
	glossary.texi

../info/emacs: ${EMACSSOURCES}
	makeinfo emacs.texi
emacs.dvi: ${EMACSSOURCES}

../info/cl: cl.texinfo
	makeinfo cl.texinfo
cl.dvi: cl.texinfo

../info/forms: forms.texinfo
	makeinfo forms.texinfo
forms.dvi: forms.texinfo

../info/sc: sc.texinfo
	makeinfo sc.texinfo
sc.dvi: forms.texinfo

../info/vip: vip.texinfo
	makeinfo vip.texinfo
vip.dvi: vip.texinfo

../info/gnus: gnus.texi
	makeinfo gnus.texi
gnus.dvi: gnus.texi

mostlyclean:
	rm -f *.log *.cp *.fn *.ky *.pg *.vr *.o core
clean: mostlyclean
	rm -f *.dvi
distclean: clean
	rm -f texindex
realclean: distclean
	rm -f *.aux *.cps *.fns *.kys *.pgs *.vrs
	rm -f ../info/emacs* ../info/cl* ../info/forms* ../info/vip*
