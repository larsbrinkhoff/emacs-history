
Decus cpp is a public-domain implementation of the C preprocessor.
It runs on VMS native (Vax C), VMS compatibilty mode (Decus C),
RSX-11M, RSTS/E, P/OS, and RT11, as well as on several varieties
of Unix, including Ultrix.  Decus cpp attempts to implement features
in the Draft ANSI Standard for the C language.  It should be noted,
however, that this standard is under active development:  the current
draft of the standard explicitly states that "readers are requested
not to specify or claim conformance to this draft."  Thus readers
and users of Decus cpp should not assume that it conforms to the
draft standard, or that it will conform to the actual C language
standard.

These notes describe how to extract the cpp source files, configure it
for your needs, and mention a few design decisions that may be of interest
to maintainers.

			Installation

Because the primary development of cpp was not on Unix, it
is distributed using the Decus C archive program (quite similar
to the archiver published in Kernighan and Plauger's Software
Tools).  To extract the files from the net.sources distribution,
save this message as cpp1.arc and the other two distribution
files as cpp2.arc and cpp3.arc.  Then, using your favorite editor,
locate the archx.c program, just following the line beginning with
"-h- archx.c" -- the format of the distribution is just:

    -h- readme.txt
      ... this file
    -h- cpp.mem
      ... description of cpp
    -h- archx.c
      ... archx.c program -- extracts archives
    -h- archc.c
      ... archc.c program -- creates archives

Compile archx.c -- it shouldn't require any special editing.
Then run it as follows:

    archx *.arc

You do not need to remove mail headers from the saved messages.

You should then read through cppdef.h to make sure the HOST and
TARGET (and other implementation-specific) definitions are set
correctly for your machine, editing them as needed.

You may then copy makefile.txt to Makefile, editing it as needed
for your particular system.  On Unix, cpp should be compiled
by make without further difficulty.  On other operating systems,
you should compile the six source modules, linking them together.
Note that, on Decus C based systems, you must extend the default
stack allocation.  The Decus C build utility will create the
appropriate command file.

			Support Notes

The USENET distribution kit was designed to keep all submissions around
50,000 bytes:

cpp1.arc:
	readme.txt	This file
	cpp.mem		Documentation page (see below)
	archx.c		Archive extraction program
	archc.c		Archive construction program
	cpp.rno		Source for cpp.mem (see below)
	makefile.txt	Unix makefile -- copy to Makefile
	cpp.h		Main header file (structure def's and globals)
	cppdef.h	Configuration file (host and target definitions)

cpp2.arc:
	cpp1.c		Mainline code, documentation master sources
	cpp2.c		most #control processing
	cpp3.c		filename stuff and command line parsing
cpp3.arc:
	cpp4.c		#define processor
	cpp5.c		#if <expr> processor
	cpp6.c		Support code (symbol table and I/O routines)
	
Cpp intentionally does not rely on the presence of a full-scale
macro preprocessor, it does require the simple parameter substitution
preprocessor capabilities of Unix V6 and Decus C.  If your C
language lacks full preprocessing, you should make sure "nomacargs"
is #define'd in cpp.h.  (This is done automatically by the Decus C
compiler.)

The documentation (manual page) for cpp is included as cpp.mem
and cpp.rno.  Cpp.rno is in Dec Runoff format, built by a Decus C
utility (getrno) from original source which is embedded in cpp1.c.
To my knowledge, there is no equivalent program that creates
the nroff source appropriate for Unix.

I would be happy to receive fixes to any problems you encounter.
As I do not maintain distribution kit base-levels, bare-bones
diff listings without sufficient context are not very useful.
It is unlikely that I can find time to help you with other
difficulties.

			Acknowledgements

I received a great deal of help from many people in debugging cpp.
Alan Feuer and Sam Kendall used "state of the art" run-time code
checkers to locate several errors.  Ed Keiser found problems when
cpp was used on machines with different int and pointer sizes.
Dave Conroy helped with the initial debugging, while Arthur Olsen
and George Rosenberg found (and solved) several problems in the
first USENET release.

Martin Minow
decvax!minow

