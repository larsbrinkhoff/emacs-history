CFLAGS= -g

EXECUTABLES = test-distrib etags ctags loadst make-docfile \
   digest-doc sorted-doc movemail cvtmail fakemail yow env \
   server emacsclient

all: ${EXECUTABLES}

clean:
	-rm -f ${EXECUTABLES} *~ \#* DOC* core
	
test-distrib: test-distrib.c
	cc -o test-distrib test-distrib.c
	./test-distrib

etags: etags.c
	cc -o etags ${CFLAGS} -DETAGS etags.c

ctags: etags.c
	cc -o ctags ${CFLAGS} -DCTAGS etags.c

loadst: loadst.c ../src/config.h
	cc -o loadst ${CFLAGS} loadst.c

make-docfile: make-docfile.c
	cc -o make-docfile ${CFLAGS} make-docfile.c

digest-doc: digest-doc.c
	cc -o digest-doc ${CFLAGS} digest-doc.c

sorted-doc: sorted-doc.c
	cc -o sorted-doc ${CFLAGS} sorted-doc.c

#
movemail: movemail.c ../src/config.h
	cc -o movemail ${CFLAGS} movemail.c

cvtmail: cvtmail.c
	cc -o cvtmail ${CFLAGS} cvtmail.c

fakemail: fakemail.c ../src/config.h
	cc -o fakemail ${CFLAGS} fakemail.c

yow: yow.c
	cc -o yow ${CFLAGS} yow.c

# this is silly -- just use emacs to edit this file!
# (in any case this program doesn't preserve alphabetical ordering,
#  which is why I'm removing it)
#addyow: addyow.c
#	cc -o addyow ${CFLAGS} addyow.c

env: env.c ../src/config.h
	cc -o env -DEMACS ${CFLAGS} env.c

server: server.c ../src/config.h
	cc -o server ${CFLAGS} server.c

emacsclient: emacsclient.c ../src/config.h
	cc -o emacsclient ${CFLAGS} emacsclient.c

# This one is NOT included in EXECUTABLES.
# See ../src/ymakefile.
emacstool: emacstool.c
	cc emacstool.c -o emacstool -g -lsuntool -lsunwindow -lpixrect
