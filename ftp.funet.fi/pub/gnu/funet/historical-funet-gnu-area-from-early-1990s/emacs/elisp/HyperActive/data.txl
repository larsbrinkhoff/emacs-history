Release 5.3 of TXL, a rapid prototyping system for programming languages
       and program transformations, is now available via anonymous ftp from
       qusuna.qucis.queensu.ca (130.15.1.100) in the directory 'txl'.  Release
       5.3 fixes a number of bugs in release 5.2, in particular a bug
       in patterns targeted at left-recursive productions, and adds
       support for arbitrary comment conventions, in particular for C
       and C++ commenting and the %operators.
TXL 5.3 is distributed in portable ANSI C source form only, and you must
compile it for your particular Unix system.  It has been tested on all of
the VAX, Sun/3, Sun/4, NeXT, and DECstation MIPS, and meets 'gcc -ansi
-pedantic' so should compile on almost anything.
Full information on the details of fetching TXL can be obtained by
fetching the 00README file, like so:

    myunix% ftp 130.15.1.100
    Connected to 130.15.1.100.
    220 qusuna FTP server (Version 5.56 Thu Apr 18 13:08:27 EDT 1991) ready.
    Name (130.15.1.100:cordy): anonymous
    331 Guest login ok, send ident as password.
    Password: cordy@myunix
    230 Guest login ok, access restrictions apply.
    ftp> cd txl
    250 CWD command successful.
    ftp> get 00README
    200 PORT command successful.
    150 Opening ASCII mode data connection for 00README (1688 bytes).
    226 Transfer complete.
    local: 00README remote: 00README
    1731 bytes received in 0.04 seconds (42 Kbytes/s)
    ftp> quit
    221 Goodbye.
    myunix% 

I will attempt to service any email requests from those who do not have
FTP access as well, but such requests will be serviced very slowly over
the next couple of months and I don't have time to try to fix any email
addresses that don't work from my site directly as sent to me.

For those of you who have forgotten what TXL is good for, I have
reproduced the TXL 5.3 ABSTRACT file below.

Jim Cordy
---
Prof. James R. Cordy				cordy@qucis.queensu.ca
Dept. of Computing and Information Science	James.R.Cordy@QueensU.CA
Queen's University at Kingston			cordy@qucis.bitnet
Kingston, Canada K7L 3N6			utcsri!qucis!cordy


----- TXL ABSTRACT -----

Subject: TXL 5.3, a Rapid Prototyping Tool for Computer Languages

Release 5.3 of TXL: Tree Transformation Language is now available via
anonymous FTP from qusuna.qucis.queensu.ca (130.15.1.100).

TXL 5.3, (c) 1988-1991 Queen's University at Kingston
-----------------------------------------------------
Here's the language prototyping tool you've been waiting for!  TXL is a
generalized source-to-source translation system suitable for rapidly
prototyping computer languages and langauge processors of any kind.  It
has been used to prototype several new programming languages as well as
specification languages, command languages, and more traditional program
transformation tasks such as constant folding, type inference and source
optimization.  

TXL is NOT a compiler technology tool, rather it is a tool for use by
average programmers in quickly prototyping languages and linguistic tasks.
TXL takes as input an arbitrary context-free grammar in extended BNF-like
notation, and a set of show-by-example transformation rules to be applied
to inputs parsed using the grammar.  TXL will automatically parse inputs
in the language described by the grammar, no matter if ambiguous or
recursive, and then successively apply the transformation rules to the
parsed input until they fail, producing as output a formatted transformed
source.  

TXL is particularly well suited to the rapid prototyping of parsers (e.g.,
producing a Modula 2 parser took only the half hour to type in the Modula
2 reference grammar directly from the back of Wirth's book), pretty
printers (e.g., a Modula 2 paragrapher took another ten minutes to insert
output formatting clues in the grammar), and custom or experimental
dialects of existing programming languages (e.g., Objective Turing was
prototyped by transforming to pure Turing and using the standard Turing
compiler to compile the result).

TXL 5.3 comes with fully portable ANSI C source automatically translated
>from the Turing Plus original, self-instruction scripts and a pile of
examples of its use in various applications.  
-- 
Send compilers articles to compilers@iecc.cambridge.ma.us or
{ima | spdcc | world}!iecc!compilers.  Meta-mail to compilers-request.