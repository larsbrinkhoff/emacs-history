# Unix makefile for cpp
#
# On certain systems, such as Unix System III, you may need to define
# $(LINTFLAGS) in the make command line to set system-specific lint flags.
#
# This Makefile assumes cpp will replace the "standard" preprocessor.
# Delete the reference to -DLINE_PREFIX=\"\" if cpp is used stand-alone.
# LINEFIX is a sed script filter that reinserts #line -- used for testing
# if LINE_PREFIX is set to "".   Note that we must stand on our heads to
# match the # and a line had better not begin with $.  By the way, what
# we really want is
#	LINEFIX = | sed "s/^#/#line/"
#
CPPDEFINE = -DLINE_PREFIX=\"\"
LINEFIX = | sed "s/^[^ !\"%-~]/&line/"
#
# Define OLD_PREPROCESSOR non-zero to make a preprocessor which is
# "as compatible as possible" with the standard Unix V7 or Ultrix
# preprocessors.  This is needed to rebuild 4.2bsd, for example, as
# the preprocessor is used to modify assembler code, rather than C.
# This is not recommended for current development.  OLD_PREPROCESSOR
# forces the following definitions:
#   OK_DOLLAR		FALSE	$ is not allowed in variables
#   OK_CONCAT		FALSE	# cannot concatenate tokens
#   COMMENT_INVISIBLE	TRUE	old-style comment concatenation
#   STRING_FORMAL	TRUE	old-style string expansion
#
OLDDEFINE = -DOLD_PREPROCESSOR=1
#
# DEFINES collects all -D arguments for cc and lint:
# Change DEFINES = $(CPPDEFINE) $(OLDDEFINE)
# for an old-style preprocessor.
# If being used for GNU Emacs, the macro EMACS will be defined as -DEMACS.
#
DEFINES = $(CPPDEFINE) $(OLDDEFINE) $(EMACS)

CFLAGS = -g $(DEFINES)

#
# ** compile cpp
#
SRCS = cpp1.c cpp2.c cpp3.c cpp4.c cpp5.c cpp6.c
OBJS = cpp1.o cpp2.o cpp3.o cpp4.o cpp5.o cpp6.o
cpp: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o cpp

#
# ** Test cpp by preprocessing itself, compiling the result,
# ** repeating the process and diff'ing the result.  Note: this
# ** is not a good test of cpp, but a simple verification.
# ** The diff's should not report any changes.
# ** Note that a sed script may be executed for each compile
#
test:
	cpp cpp1.c $(LINEFIX) >old.tmp1.c
	cpp cpp2.c $(LINEFIX) >old.tmp2.c
	cpp cpp3.c $(LINEFIX) >old.tmp3.c
	cpp cpp4.c $(LINEFIX) >old.tmp4.c
	cpp cpp5.c $(LINEFIX) >old.tmp5.c
	cpp cpp6.c $(LINEFIX) >old.tmp6.c
	$(CC) $(CFLAGS) old.tmp[123456].c
	a.out cpp1.c >new.tmp1.c
	a.out cpp2.c >new.tmp2.c
	a.out cpp3.c >new.tmp3.c
	a.out cpp4.c >new.tmp4.c
	a.out cpp5.c >new.tmp5.c
	a.out cpp6.c >new.tmp6.c
	diff old.tmp1.c new.tmp1.c
	diff old.tmp2.c new.tmp2.c
	diff old.tmp3.c new.tmp3.c
	diff old.tmp4.c new.tmp4.c
	diff old.tmp5.c new.tmp5.c
	diff old.tmp6.c new.tmp6.c
	rm a.out old.tmp[123456].* new.tmp[123456].*

#
# A somewhat more extensive test is provided by the "clock"
# program (which is not distributed).  Substitute your favorite
# macro-rich program here.
#
clock:	clock.c cpp
	cpp clock.c $(LINEFIX) >temp.cpp.c
	cc temp.cpp.c -lcurses -ltermcap -o clock
	rm temp.cpp.c

#
# ** Lint the code
#

lint:	$(SRCS)
	lint $(LINTFLAGS) $(DEFINES) $(SRCS)

#
# ** Remove unneeded files
#
clean:
	rm -f $(OBJS) cpp

#
# ** Rebuild the archive files needed to distribute cpp
# ** Uses the Decus C archive utility.
#

archc:	archc.c
	$(CC) $(CFLAGS) archc.c -o archc

archx:	archx.c
	$(CC) $(CFLAGS) archx.c -o archx

archive: archc
	archc readme.txt cpp.mem archx.c archc.c cpp.rno makefile.txt \
		cpp*.h >cpp1.arc
	archc cpp1.c cpp2.c cpp3.c >cpp2.arc
	archc cpp4.c cpp5.c cpp6.c >cpp3.arc

#
# Object module dependencies
#

cpp1.o	:	cpp1.c cpp.h cppdef.h

cpp2.o	:	cpp2.c cpp.h cppdef.h

cpp3.o	:	cpp3.c cpp.h cppdef.h

cpp4.o	:	cpp4.c cpp.h cppdef.h

cpp5.o	:	cpp5.c cpp.h cppdef.h

cpp6.o	:	cpp6.c cpp.h cppdef.h


