.tr ~
.\" EMACS_MODES: fill !c
.TL 126010-2200 40158-100
An Interactive Screen Editor for Unix (Issue 2)
.AU "Warren Montgomery" WAM IH 55235 2494 6C-501 85-55235-?
.TM ??
.SA 1
.NR Pt 0
.AS 2
High speed data communication and display screen terminals make
possible a fundamentally different mode of entering and editing text
to a computer system.  An interactive screen editor allows a user to
enter and edit files, and to see the effects of the editing
immediately.
.P
A good screen editor can improve productivity in several ways.  The
need for paper listings, and thus the expense and delay in dealing
with them, is greatly reduced with the availability of a screen
editor.  The editor can provide a customized environment for
particular tasks, such as editing program source or word processing,
which can relieve the user of the mechanical parts of the task (such
as maintaining proper indentation).  The immediate feedback provided
reduces mistakes, and speeds up their detection.  A simple set of editing
commands can be used 
effectively by relatively unskilled users, because of the feedback
obtained by seeing the effects of editing.
.P
This memorandum describes an interactive screen editor for UNIX\*F
.FS
Unix is a Trademark of AT&T Bell Laboratories
.FE
known as EMACS.  The editor is patterned after a very popular screen
editor originally developed at the Artificial Intelligence
Laboratory M.I.T.  It was developed by the
author as a tool for his own work and is currently used by a large
number of users in Bell Laboratories.  Emacs is available through
the experimental tools facility within Bell Labs, and through the
Toolchest package to outside customers.  The editor provides a
friendly editing environment with the advantages outlined above,
while running in the small address space provided to the UNIX user
on the PDP\*F-11/70.  This memorandum describes the commands and
editing environment of EMACS, and some experience with its use.
It obsoletes the first users manual published in 1980.
.FS
PDP is a Trademark of Digital Equipment Corporation
.FE
.AE
.MT "TECHNICAL MEMORANDUM"
.H 1 "INTRODUCTION"
Text editing is the most common task of many computer users.  The
creation and modification of programs, data bases, and memoranda
occupies much of the time that a user spends with a computer system.
The ability to edit programs and recompile them was one of the
primary reasons for the success of early time-sharing systems over
batch processing systems.
.P
Many of the text editing tools now in use are based on the editors
for the early timesharing systems.  These editors were developed for
an environment that included mostly low-speed (110 baud) printing
terminals, and expensive computer systems that were not prepared to
interact with the user on a character at a time basis.  In such a
environment, it was appropriate to minimize the amount of output
produced by the editor, and to allow the user to specify a lot of
changes to be made by a single editor "command".  Editors such as
the standard UNIX\*F editor (ed) are ideal for this environment.
.FS
Unix is a Trademark of AT&T Bell Laboratories
.FE
.P
In recent years, printing terminals have been repaced by display
terminals capable of handling high data rates in many applications.
The cost of
computing has steadily dropped.  The text editing tools made
available to users must evolve to take advantage of these changes. 
With a high-speed display terminal, minimizing output is no longer
appropriate.  Instead, the display can be used to provide feedback
for the user on the results of editing.  The lower cost of
computing, and better hardware support for terminals, make character
at a time interaction with the computing system feasible.  The EMACS
editor described in this report is one attempt to take advantage of
these effects in order to provide the user with a simple and
powerful editing environment.
.P
EMACS is a screen editor that can be used to build or to edit files 
using a display terminal, such as the hp2621, vt-100, or teletype
5420.  The user
interface to this editor is quite simple.  The user is presented
with a display of the contents of a portion of the buffer being
edited.  This display indicates \fIexactly\fP what is
in the area being displayed, including any non-printing characters.
The contents of the buffer being edited can be read from or written
to a UNIX file.
Characters typed by the user will be inserted into the buffer
(and reflected in the display) at the point indicated by the
terminal's cursor.  This is the primary mechanism for entering and
modifying text.
.P
Control characters and escape sequences can be used to perform other
editing functions, such as moving the cursor to a different position
in the buffer, deleting text, replacing text, or searching.  Thus
there is only one mode of interpretation of characters typed to
EMACS, in which either text to be entered or commands can be
entered.  This simple interface relieves the user of the need to
remember what mode he is in, and prevents the disastrous
mistakes that can occur when text to be inserted is evaluated as an
editor command.  A simple mechanism is provided to allow a user to
insert control and escape characters when needed.
.P
Although there is a rich vocabulary of commands available,
including commands that perform functions tailored to a particular
application (such as indenting a C program), the most common way in
which EMACS is used to edit is simply to position the cursor
over the area to be changed, and enter the changes.  The immediate
feedback provided by the visual display appears to be very important
to the user.
.P
This editor was written by the author as an aide to his other work,
and patterned after the EMACS editor written for the PDP\*F-10 systems
.FS
PDP is a Trademark of Digital Equipment Corporation
.FE
at the M.I.T. Artificial Intelligence Laboratory.  The interface to the
user closely follows that provided by the M.I.T. version, because
the author was familiar with that version.  The implementation of
EMACS for UNIX described in this report was done by the author, and
is independent of any other implementations of Emacs.
.P
The author and his organization are not supporting EMACS.  The
author is, however, willing to distribute copies of the software for
use within Bell Laboratories, and is interested in comments
regarding features or problems with EMACS.  The author will repair
problems as time allows, but makes no guarantees to fix problems
promptly.
.P
The remainder of this report contains a user's manual for the EMACS
editor, and a discussion of the experience that we have had with
EMACS in our department.  EMACS continues to evolve to provide more
commands and remove implementation restrictions.  The users manual
here describes EMACS version 4.9, which was in use in March, 1985.
.P
This document is intended to serve the needs of a number of
different kinds of readers.  Briefly, here is a guide on how to read
it for various kinds of readers.
.VL 40
.LI "Getting Started as a New User:"
Read Chapters 2, 3, and 6.  As you need to, refer to the information
in Chapters 4, 5, and 7.
.LI "Getting Started as an Experienced User:"
Users of other Emacs like editors will probably find the material in
chapters 6 to be most useful to start.
.LI "Referring to Specific Commands:"
All users will find the command and mode descriptions in chapters 4,
5, and 6 to be useful to refer back to specific command
descriptions.
.LI "Learning about Editors in General:"
People interested in the general flavor of emacs and in our
experiences with it should read chapters 2 and 8 to get an
introduction and to see a discussion of how the features are used.
.LE
.H 1 "Basic concepts"
Before going into the editing commands of EMACS, some basic concepts
should be learned.  EMACS operates rather differently from 
line oriented editors, and even from other screen oriented editors
in the way that it treats the screen and the keyboard.  Some of it's
conventions for displaying and inputing characters are not like
other Unix tools, primarily because they were originally developed
for another environment.
.H 2 "The Character Set"
EMACS operates on characters from an alphabet of 256 different
characters.  These include the 128 ASCII characters that can be entered
from a terminal, and 128 "Meta" characters.  A Meta character is
entered by preceding it with an escape (ESC key).  
.P
In this document and in the displays produced by emacs, control
characters are indicated by the character '^' followed by the
equivalent printable character (usually capitalized).  Thus '^X'
represents a control-x, which is typed by hitting the control
and 'x' keys simultaneously.  For some
unusual non-printing characters, the display is not obvious:
.VL 10
.LI "^?"
Rubout or delete (ASCII 0177)
.LI "^@"
Null (ASCII 0)
.LI "^["
Escape (ASCII 033)
.LI "^\e"
The "fs" character (ASCII 034)
.LI "^]"
The "gs" character (ASCII 035)
.LI "^^"
The "rs" character (ASCII 036)
.LI "^_"
The "us" character (ASCII 037)
.LE
.P
Meta characters are typed to Emacs by hitting the escape character,
and then any second character (including a control character.)  They
are displayed by emacs as "M-" followed by the equivalent ASCII
character.  Thus "M-a" (Meta - a) is the character obtained by typing escape
followed by a, and "M-^B" (Meta - control-b) is the character
obtained by typing escape followed by control-b.
.H 2 "The display"
The display screen contains a window showing a view of the buffer
being edited, which contains about 20 lines on a typical display
terminal.  The terminal cursor is positioned at the
point where the editor cursor (the current position where editing is
taking place) is in the buffer.  Each line of the buffer
(delimited by a newline character) begins at the beginning of a display
line.  A line that exceeds the screen width is normally continued on
the next screen line.  Whenever a line must be continued on the next
screen line an exclamation mark (!) is displayed in the last column
of the first screen line.  If the editor is in line number (lnumb)
mode, then a line number is printed at the beginning of each line in the buffer.
.P
Printable characters are displayed normally, while tabs are displayed as
white space that fills up the space on the screen until the next
position at a multiple of eight.
Non printing control characters and meta characters are printed with
the conventions outlined above.
.SP
If you edit a file which contains characters that have the high
order (parity) bit set, they will display as "M-" followed by the
display of the character.  You will only run into this when trying
to edit files containing binary information.
.P
In addition to the display buffer, several lines of the screen are used
for status information and for displaying parameters entered into EMACS,
such a file name.  One of these lines known as the status line contains the
editor name, editor version, buffer number and name, and file name.  Some
of the more recently introduced commands described in this document
indicate the version in which they were introduced, so that you can
determine whether or not a particular command is in the version that
you are running.
If the buffer has not been modified since the file was read or written,
an '=' will be displayed between the buffer and file names.  Otherwise,
a '>' will appear.
.P
The lines below the status line are used for the time of day display
(if time mode is on), and for emacs to prompt for parameters for
commands.  Some commands cause the buffer display to be erased in
order to display other information in place of the buffer.  The word
"Continue?" will be displayed at the bottom of the screen when this
happens.  Typing 'y', ' ', or return will bring back the buffer
display.  Typing 'n' may allow you to re-execute the command
producing the display.
.P
Figure 1 shows a typical screen during a EMACS session.  The buffer
"Main", number 0, is being used to edit a program test.c.  The
buffer has been modified since the last write to the file test.c.
.DF
.ce
Figure 1 EMACS screen Display
.SP 2
1   #include <stdio.h>
2   /* EMACS_MODES: c, !fill, comcol=43 */
3
4
5   /* This is a c program */
6
7   main()
8   {
9           int i;
10          char c;
11
12          for (i = 0;i > 0; i++) {
13                  printf("i = %d\en",i); /* print i */
14          }
15  }
16

EMACS 4.8  (0) Main > test.c


.DE
.H 2 "The Text in The Buffer"
Each buffer that you edit holds a sequence of characters.  Any
characters can be present in an emacs buffer, including control and
meta characters.  The only limitation is on the number of characters
that can be on one line in the buffer.  Normally, emacs treats the
buffer just as a sequence of characters.
.P
One difference between emacs and many editors is that emacs does not
treat "newline" characters specially.  Between each pair of adjacent
lines of text in the buffer is an invisible "newline" character.  If
the cursor is at the end of one line, it is in front of the newline
character, and deleting a single character will delete the newline,
causing the text in the following line to be joined to the current
line.  Newlines can be inserted, deleted, and searched for like any
other characters.
.P
Some emacs commands operate on units of text in the buffer, like
words, lines, sentences, pages, etc.  These work on top of the base
level which still treats the buffer as a string of
characters.
.H 2 "Command structure"
Unlike most other editors, emacs does not have distinct "modes" for
inserting text into the buffer and for entering commands.  Thus
there are no commands for inserting text, and no special convention
to end a text insertion.  Instead, at any point ordinary characters
can be inserted into the buffer simply by typing them, while control
and meta characters are used for editing commands.
.P
Each character that is typed into EMACS is in fact interpreted as a
command.  All of the ordinary printing characters insert themselves into the 
buffer being edited at the point defined by the cursor.  Thus
the command invoked when you type the character 'x' inserts an x
into the buffer at the point shown by the cursor.  The control and
meta characters are used for various editing functions.
.H 2 "Arguments and Parameters"
All commands, including the printing characters, take a numeric argument
that has some effect on their interpretation.  The default argument
given to a command for which no argument is specified is 1.  To
specify some other argument to a command, you can enter escape,
followed by a sequence of digits, and then the command.
You can specify a negative value for an argument by entering escape
followed by '-', followed by a sequence of digits.
Numbers starting with a 0 are interpreted as
octal, while numbers starting with any other digit are decimal.
A second
way of specifying the argument is to precede the command by one or more 
^U (control-u) characters.  Each ^U multiplies the value of the argument 
by 4.  
.P
For most commands, the effect of the argument is to multiply the
number of times that the command is applied.  Thus the sequence ^U^Ux 
inserts 16 x's into the buffer at the current location.  The sequence 
ESC13^N moves forward 13 lines in the buffer.
.P
In addition to the numeric argument given to all commands, some
commands will prompt the user for additional character string
parameters.  The commands that take parameters, and the method of
entering parameters are described in the section on file and buffer
commands.
.H 1 "Basic Emacs Commands"
As noted above, every character you type to emacs is interpreted as
a command.  This section describes a simple set of commands that
will be sufficient for most editing that you do.  Subsequent
sections describe more advanced commands that are very useful in
certain situations, and other aspects of emacs.
.P
Emacs has a large number of commands.  Most of them have a mnemonic
significance that should be obvious (like ^B for backwards or ^D for
delete).  Some, unfortunately, don't have any obvious meaning.  As a
general rule, control character commands operate on characters and
lines, while the corresponding meta character commands operate on
words or sentences.
.P
The user interface of emacs was designed for touch typists.  A
deliberate choice was made to use control and meta characters for
commands rather than special keys, such as the function or "arrow"
keys on many terminals, since these keys are different on every
terminal and generally cannot be reached without taking your fingers
off of the home position of the keyboard.  Typically the only
difficulty faced by an emacs user in adapting to a new terminal is
locating the escape key, which is unfortunately located differently
on every terminal.  With a little practice, you will find that your
fingers become adept at locating the keys for all of the basic
commands with little thought and without having to look at the
keyboard.
.H 2 "Getting Help or getting out of trouble"
Emacs has many self-help features.  The commands listed in this
section are useful to know about because they can provide help or
remedy mistakes.
.VL 10
.LI "M-?"
Explain.   This command prompts for a character and prints a brief 
explanation of what that character does.
.LI "M-w"
Wall Chart.  This command puts a listing of all commands (including
user defined commands), and their help explanations into the
current buffer.  This command is a
convenient way of producing a "wall chart" of the commands.  The
list is inserted into the buffer at the current position, so that
normally one would want to execute it in an empty buffer.
The appendix to this report contains a current copy of the wall chart.
.LI "^L"
Refresh.  refresh the display.  Occasionally, some error may cause the
display to become garbled.  ^L re-creates it from scratch.  If you
give an argument to ^L, it is used to specify how many lines will
appear on the screen before the current line.  When invoked with an
argument, this command does not re-create the display from scratch.
.LI "^G"
Abort.  Typing ^G at any point that emacs is asking for input
will abort the current command.  This applies at
any step (specifying arguments, typing escape, entering parameters
that emacs asks for, etc.).  (There are a couple of exceptions
related to advanced editing commands, but even with these, typing ^G
several times will always get you out with minimal damage).
This is a convenient way of aborting anything that you are not sure
that you want to complete, and may not know how you started.
.LI "M-u"
Undo.  This command undoes the effect of the last significant text
modifying command that can be undone.  Undo is it's own inverse, so
invoking undo twice in a row undoes the result of the undo command. 
Significant text modifying commands include all except insertion of
individual characters (including newlines), and the open-line (^O)
command.
.P
Almost all commands can be undone, but a few effects of commands
can't be undone.  M-u will not undo the
effects of reading or writing files, nor can it undo anything done
by executing unix commands from emacs.  Undo cannot be used to bring
back buffers that may have been inadvertently killed with the ^X^K
command (see below).  Undo can undo all other significant text
modifications, with the exception that when the last significant text
modification was a replace command, only a limited number of
replacements can be undone.  If more replacements were done with one
command, undo prints a warning error message and if the user
specifies then undoes what it can.
.LI "^X^C"
Quit Emacs.
If any buffers have been modified since the last write, EMACS will
ask whether or not to write out each such buffer before exiting.
EMACS will not ask whether or not to save an empty buffer.
.LI "BREAK"
Emacs will not respond to your normal interrupt character, but it
will respond if you hit the BREAK key on your terminal.  Break
causes emacs to stop anything that it was doing at the next
convenient point, and displays a message to you listing some
options.  When you hit break you can either continue what you were
doing, abort what you were doing, exit from emacs, or suspend what
you were doing such that you can return to it later if you wish.
.LE
.H 2 "Simple cursor movement commands"
There are many ways to move the cursor around in the buffer without modifying
the text in the buffer.  Most of these use their argument to specify
how many times the movement is to be repeated.  
.VL 10
.LI "^F,^B"
Move forward or backward one character.  Recall that the end of each
line counts as one character, so that ^F at the end of one
line will put you at the beginning of the next line.
.LI "^N,^P"
Move to next or previous line.  Emacs moves to the same character
position in the line below (^N) or above (^P) the current line.
Note that if the buffer contains tab or control characters, the same
character position in the lines in the buffer may display at
different points in the screen. 
.LI "^A,^E"
Move to the beginning (^A) or end (^E) of the current line of the
buffer.  Note that these work on one line of the buffer, not one
line of the screen. If the current line is longer than will fit on
one line of the screen display, these commands will move up or down
on the screen to the real beginning or end of the line in the buffer.
.LI "M-<,M->"
Move the cursor to the beginning or end of the buffer.
.LI "M-f,M-b"
Move the cursor forward or backward by one word.  In emacs, words
are delimited by non-alphabetic or non-numeric characters.  If your
terminal is capable of displaying underscored characters, emacs will
not consider these characters as separating words.
.LI "M- a,M-e"
Move the cursor to the beginning or end of the current sentence. 
The end of a sentence is defined as a punctuation mark followed by two
or more whitespace characters (blanks or newlines).  With an
argument, these commands can be used to move forwards or backwards
by a specified number of sentences.
.LI "^V,M-v"
Move to next or previous page.  The cursor is moved forward or
backward so that the display will show the text just before or just
after the text now in the window on the screen.
.LI "M-g"
Move the cursor to the line number specified by the argument given
to the command.
.LE
.H 2 "Simple Text Deleting and Moving Commands"
Several commands are available to delete text from the buffer.  All of
these commands operate on text near the current cursor position.  The 
deletion commands are:
.VL 10
.LI "^D,^H"
Delete forwards or backwards from the cursor.  ^H, or backspace
deletes the character immediately before the cursor.  ^? or rubout,
is a synonym for ^H.  ^D deletes the character on top of the cursor.
If given arguments, these commands delete blocks of text forward or
backward of the current cursor position.
.LI "M-d,M-^?"
Delete words forwards or backwards from the cursor.  These two
commands delete words (as defined for M-f and M-b).  If the current
cursor position is in the middle of a word, M-d will delete from the
cursor to the end of the word while M-^? will delete everything
before the cursor.
.LI "^K"
Delete to the end of this line.  If invoked without an argument, ^K 
deletes the remaining text on this line (if any).  If no text follows the
cursor on the current line, ^K deletes the newline.  With an
argument of 0, ^K deletes the text before the cursor on the current
line.  With an argument of n greater than zero, it
deletes n lines forward from the cursor position.  The text from the
cursor up to and including the nth newline  is deleted.  With an
argument less than zero, the deletion is backwards from the cursor
position.
.LI "M-~"
(Meta space)
The command Meta-space places an invisible mark on the 
current cursor position.  This mark can be used in subsequent editing.
Each mark is simply a position in the buffer (line number and
character within the line.)  Thus if you add or delete text in front
of a position where a mark was placed, the mark may not remain on
the same character, but stays on the same position.
.P
Emacs actually maintains 16 different marks, normally allocated as
one per buffer.  (Thus if you set marks in different buffers, they
are normally independent.)  You can, however, alter this by
specifying a mark number as an argument to Meta-space and other
commands that work with marks.  This allows you to mark up to 16
different positions in one buffer.  The command ^@ (Control-@) is a
synonym for Meta-space, but cannot be typed on all keyboards.
.LI "^W"
The command '^W' deletes the text between the current cursor position and 
the mark.  This is a convenient way to delete a well defined block of text.
If an argument is specified, it is used to select the mark number
The mark can be either before or after the cursor position and
achieve the same effect.
.LI "^Y"
Insert last killed text.  All text that is deleted is saved in a "kill 
stack".  The kill stack holds the last 16 deletions.  There is
also a limit on the total amount of text that can be held in the
kill stack, but you are unlikely to encounter it.
^Y retrieves the most recently deleted text.  The most
frequent use of this command is in moving text around.  The procedure 
is: kill the text to be moved, move the cursor to where you want it, and 
enter ^Y.  Another use of ^Y is to undo an unwanted deletion.  ^Y 
leaves the mark at the beginning of the inserted text, and puts the 
cursor at the end.  ^Y treats it's argument (if any) as a count for
the number of copies of the deleted text to bring back, and not a
mark number.  ^Y operates only with the default mark.
.LI "M-y"
Replace last retrieved text.  This command kills the text between
the cursor and the mark and replaces it with the next to last item on
the kill stack.  This command can only be used immediately after ^Y,
where it changes text that has just been retrieved.  By entering ^Y
followed by some number of M-y's, any text in the kill stack can be
retrieved.
.LI "M-p"
Pickup the region of text.  This command picks up the text between
the current position and the mark and puts it in the kill stack,
without changing the buffer.  This is useful for duplicating blocks
of text in the buffer.  An argument can be used to specify which mark
to use
.LI "^X^X"
(Type control-X twice) Exchange the cursor position and the mark. 
An argument can be specified to indicate the mark to exchange with.
.LE
.H 2 "Simple File and buffer Commands"
Commands that access files and buffers must ask for the
name of the appropriate file or buffer.  All of these commands (and
some of the others) ask for the appropriate information at the
bottom of the screen.  You can use some of the simple editing
commands described here, plus a couple of special commands, to edit
a file or buffer name that you enter this way.  The commands that
you can use for editing are:
.VL 10
.LI "^F,^B"
Move forward or backward one character
.LI "^A,^E
Go to beginning or end of line.
.LI "^D,^H"
Delete forward or backward
.LI "^U"
Multiply the effect of the next command by 4.
.LI "^K"
Kill (erase) the whole line
.LI "^G"
Abort the command asking for information
.LI "^X"
Enter the current line from the file at the cursor
.LI "^Y"
Enter the current file name at the cursor
.LI "^L"
Redisplay the prompt and the string being entered
.LI "^T"
Transpose the characters before and after the cursor
.LI "^Q"
Quote the following character (eliminates the special significance
of the next character and just sticks it literally in the string
being typed.)
.LI "return"
End the string being typed, and continue editing.  (If the cursor is
in the middle of the line, the whole line is given to whatever
command asked for it, not just the text before the cursor).
.LE
.P
Most of the file accessing commands are invoked through the ^X command.
^X is a prefix for several useful commands, most of which involve file
or buffer access.
These commands are invoked by a ^X followed by a second character.
.P
In the commands that ask for filenames, the normal shell conventions
for partially specified names can be used.  Any of the following
sequences can be used in a filename and will be substituted
properly.
.tr ~~
.VL 20
.LI "$VARIABLE"
Substitutes the value of the environment variable $VARIABLE or
nothing if VARIABLE is not defined.  Thus you can use path names like
$HOME/.profile.
.LI "* and ?"
These characters can be used to specify incomplete filenames and
will be expanded.  If more than one file matches the name given,
then emacs will pick only the first one.
.LI "~USER"
This translates into the home directory of the user USER.  In
addition, EMACS always translates the special name ~EMACS into an
emacs library directory.  This is a directory where special
files needed by emacs are stored and where standard macros are
stored (in ~EMACS/macros).
.LI "`COMMAND`"
This causes COMMAND to be run and substitutes it's standard output.
.LE
.tr ~ 
.P
With these preliminaries out of the way, here are some of the
commands that work with files and buffers
.VL 10
.LI "^X^R"
Read file.  EMACS will prompt for a file name, which you enter as
described above. When the file name for ^X^R has been entered, EMACS
will read the specified file into the buffer.
.P
Many of the commands in this section, including ^X^R, use their
argument to specify minor variations on the basic action of the
command, rather than specifying a count.  In the normal case, (with
the default argument of one), ^X^R clears the buffer before reading.
If ^X^R is invoked with an argument that is not 1 (i.e. ^U^X^R) it
inserts the file into the buffer at the current cursor position. 
If ^X^R is invoked with a negative argument, no error message is
produced if the specified file cannot be read.
.LI "^X^W"
Write file.
Normally, if the specified file exists and has two or more links to
it, EMACS will ask whether to overwrite the existing copy of the
file or to unlink the specified file name and create a new file in
its place, leaving the contents of the old file (which may be
obtained through the other names it was linked to) unchanged.
If emacs fails or the unix system crashes during an attempted write
(either ^X^W or ^X^S), the previous contents of the file are saved
in a file .EMACS in your current working directory.
.P
Passing an argument to ^X^W (i.e. ^U^X^W) causes the contents of the
current buffer to be appended to the specified file rather than
replacing it.
.LI "^X^S"
Save buffer.  This writes out the buffer to the last file read or written if
the file has been modified.  If the buffer was not read from a file,
and has never been written to one, such that there is no file name
associated with the buffer, emacs will ask for a filename to save
the buffer in.
.LI "^X^B"
Change buffer.  EMACS allows up to 12 named buffers to be edited
concurrently.  Each buffer can hold a different file, and has it's
own current cursor position.  In emacs you work with one buffer at a
time, although you can display two buffers on the screen at the same
time.   The ^X^B command asks for the name of a buffer and makes
that buffer the current buffer.
.P
All of the commands that ask for buffer names accept either the text
name of the buffer, or the buffer number (shown in parentheses after
the editor name on the status line) for a buffer name.  The number
is convenient if you
don't like typing long names.  Emacs treats two buffer names
specially.  For any of the commands that ask for buffer name, if you
enter an empty buffer name (by just hitting return in response to
the prompt, emacs shows you a display of all of your currently
defined buffers, indicating which one is current and which ones have
been modified since they were last written.  If you type space
or 'y' in response to the prompt that appears after the display,
emacs will abort whatever command asked for the buffer name and
continue editing.  If you type 'n' in response to the prompt,
emacs will ask again for the buffer name, and then complete whatever
command asked for the buffer name.  If the buffer
name "..." is entered, a new, empty buffer with a unique name is created.
.LI "^X^F"
Find file.  This command prompts for a file name and switches to a
buffer that holds the specified file.  If the specified file has
been read into a buffer, the effect of find file is to change to
that buffer.  If no buffer holds the specified file, the effect of
find file is to create a new buffer and read the specified file into
it.  Find file is a convenient way to switch between editing several files.
If ^X^F is invoked with a negative argument, no error message is
produced if the specified file cannot be found.
.LI "^X^K"
Kill Buffer.  This command prompts for a buffer name and destroys
the specified buffer.  You cannot kill the current buffer this way. 
Text in the buffer that is killed is lost and cannot be recovered.
.LE
.H 2 "Simple Search, Replace, and other commands"
Emacs provides several commands that search for text in the buffer,
and also commands which allow you to specify global replacements,
like change every instance of "football" to "baseball".  The
simplest forms of these commands are described here, along with a
couple of miscellaneous commands that are useful.  More complex
versions of search and replace are described in the subsequent
section on advanced editing.
.VL 10
.LI "^S,^R"
Forward and Reverse Search.  These commands allow you to look for
text in your buffer.  Emacs will prompt at the bottom of the screen
with "Search" or "Reverse Search".  In response to the prompt, you
can type in characters, and emacs will begin to look for the next
match for what you type, going either forwards (^S) or backwards
(^R) from the current position in the buffer.  Emacs will
show you the text that matches what you have typed as you type it,
by moving the cursor to the text, possibly moving the display window
in the buffer if the text you are looking for was not visible.  As
you type in the string to look for, you get immediate feedback about
what emacs has found.  In addition to typing normal printing
characters that become part of the string you are looking for, you
can type some special characters to either edit the string you are
looking for, or control the search in some other way:
.VL 15
.LI "^H"
This deletes the last character of the search string, and will cause
the cursor to go back to whatever matches what is left.
.LI "escape"
Hitting escape stops the search, leaving the cursor on whatever you
last found.
.LI "^G: "
This quits from the search and goes back to the point in the buffer
where you started the search from.
.LI "return or newline: "
These both cause a newline to become part of the search string.  the
newline is displayed as "^J" (which is the control character
actually used by unix to indicate "newline") in the search string to
allow you to see it.  Thus if you type "^Sthe~end<return>of", emacs
will look for a spot where "the~end" appears at the end of one line
and "of" appears at the start of the next line
.LI "^S or ^R: "
These characters control the search.  In general, if you are going
forward, and type ^R, or going backwards and type ^S, the search
changes direction, starting from the last thing you found.  If you
are going forward and type ^S or backwards and type ^R, the search
proceeds to the next occurrence (in whatever direction you were
going) of the search string.  If you type ^S or ^R as the very first
thing after starting a search, emacs takes the \fIlast\fP string
that you successfully found with a search and makes it the current
search string.  These characters provide a convenient way to
navigate when looking for something that occurs many times in the
buffer.
.LI "^Q: "
Typing ^Q "quotes" the next character, making it part of the search
string.  This is a way to look for strings that contain control
characters.
.LI "other control characters: "
Typing any other control character causes the search to stop at
whatever you found, and then executes the command corresponding to
that control character.
.LE
.P
This kind of search is called an incremental search in emacs,
because it shows you what you have matched incrementally as you type
it.  It is very easy to learn to use.  For incremental search, the
search string must \fIexactly\fP match whatever you are looking for. 
(There is a more complicated search available that allows some
pattern matching, and is described in the advanced commands
section.)  Incremental search stops, indicating that it fails if you
reach the beginning or end of the buffer.  Normally, search
considers upper and lower case letters to be different, however you
can override this with "caseless" mode.
.LI "M-r"
Query replace.  You will be prompted for a From string and a To string.  
Each can be edited using the conventions described in the previous
section for editing filenames.  In general, Query
replace will allow you to replace all of the strings in your buffer
from the current cursor position to the end of the buffer that match
the From string with the To string.  In the To string, the '&'
character can be used to designate replacement with the From string.
To get a real '&', prefix it with a '\e'.  To get a real '\e', prefix
it with another '\e'.
EMACS then searches for the from string, positions the cursor in
front of it, and prompts you. You can control the replacement of the
item in question by what you type:
.VL 15
.LI "<space> or y"
Replace this occurrence and move on to the next one.
.LI "n or ^?"
Skip this occurrence and move on to the next.
.LI "."
Replace this occurrence and exit query replace.
.LI "^G"
Quit.  (Exit query replace without replacing the current match.)
.LI "b"
Go back to the previous occurrence of the "To" string.  It won't find
one that you have already replaced!
.LI "r"
Replace the rest without stopping to ask after each, and show the
result of the replacement after each.
.LI "R"
Replace the rest silently.  (i.e. don't show the result after each
replacement.)
.LI "<ESCAPE>"
(Version 4.6) Causes emacs to ask for a new string to replace the "To" string with
with.  The current occurrence will be replaced with what you type,
and emacs will go on to the next occurrence.
.LE 
.P
Normally, query replace will show all occurrences of the search
string.  With an argument (i.e. ^UM-r), it behaves like the
substitute command of the ed editor, looking at only the first match
of the From string on each line.  Query replace exits when the from
string is no longer matched.  '?' prints a summary of the options.
As with incremental search, the From string must match exactly to
something in the buffer.  There is a more advanced form of query
replace that allows pattern matching and is described in the section
on advanced commands.
.LI "^O"
Open up a line.  This command creates one or more empty lines at the 
current cursor position.  This is useful for inserting text in the 
middle of the buffer, while minimizing the amount of screen refresh 
needed.
.LI "^T"
Transpose the next two characters.  The cursor moves forward one
character for each transposition, such giving ^T a count as an
argument causes the character in front of the cursor to be dragged
forward through the text.
.LE
.H 1 "Advanced Editing Commands"
The commands described in the previous section are sufficient to
allow a user to perform most editing tasks efficiently.  The
commands in this section for the most part cover special situations,
like inserting control characters into files, or provide more
efficient ways to to things in certain situations.
.H 2 "Inserting 'odd' characters"
Because EMACS uses control and escape characters for commands, you
cannot directly insert them into the buffer by typing them.  The
following three commands are useful for the occasional need to get
such characters into a buffer.
.VL 10
.LI "^Q"
Quote the next character(s).  ^Q accepts one or more characters (the
number of characters specified by its argument) from the terminal
and inserts them "blindly" into the buffer without interpretation.  
Only the newline (line feed) character is interpreted.  EMACS strips
the parity bit from all characters read from the terminal, so all
characters inserted this way have zero parity.
.LI "M-q"
Quote characters and turn on parity bit.  This acts just like ^Q,
however it turns on the parity bit in the character before
inserting.  Characters inserted this way will be displayed as meta
characters by EMACS.
.LI "M-\e"
Convert the argument to a character and insert into the buffer. 
This command takes its argument and converts it to a character and
inserts it.  This provides an easy way to convert from octal or
decimal to ASCII, and is occasionally useful for inserting odd
characters for which the ASCII code is known.
.LE
.H 2 "Commands related to Windows"
Emacs provides a way to display two buffers on the screen at the
same time.  When this is done, the screen is split vertically, and
one buffer is displayed in the top half and one in the bottom half. 
The status line will show status of the current buffer.
.P
When emacs displays two buffers like this, only the one that is the
current buffer is actively updated.  The display for the other just
sits on the screen undisturbed until you return to that buffer.  You
can have the same buffer displayed in both windows, however note
that the current position is associated with a buffer, not a window,
thus if you move the current position in the lower window, when you
return to the upper window, the cursor will immediately move to
wherever you were in the lower window.  If you have different
buffers in the two windows, the current positions in both buffers
are independent, as they are with any two buffers.
.VL 10
.LI "^X2"
Enter two window mode.  Emacs will ask for a buffer to show in the
second window.  The current buffer becomes the top window, while the
buffer that you type in response to the prompt goes in the lower
window and becomes the current buffer. 
.LI "^X^^"
Grow window.  Makes the current window grow by the number of lines
specified by the argument to ^X^^.  This command works only when you
are in two window mode.  You can use a negative argument to cause
the current window to shrink.
.P
You can also use this command to grow or shrink the buffer display
with only one window on the screen.  This can be useful in avoiding
long delays when working from a low speed terminal port.
.LI "^X1"
Return to one window.  The current window grows to fill the screen.
.LI "^X^O"
Switch windows.  Make the dormant window current and the current
window dormant.
.LE
.H 2 "Advanced Search and Replace Commands"
The simple incremental versions of search and replace described
above require that you match what you are looking for exactly.  The
commands described here allow pattern matching of regular
expressions, like those used by the ed editor.  
.P
The description of regular expressions is to complex to reproduce
here.  Refer to the manual for ed for a brief description.  Emacs
provides two additional special sequences for regular expressions. 
The character sequences "\e<" and "\e>" can be used
to match the beginning and end of words.  Thus the string \e<the\e>
will match any occurrence of the word "the", but not any word
containing the sequence of letters "the", such as "other".
.P
In constructing regular expressions, it is important to remember
that in order to avoid the special significance of a character
like '.' or '*', you must prefix it with a backslash '\e'.  If you
must have control characters in regular expressions, you can quote
them for emacs by typing ^Q before the control character.
.P
Note also that regular expression searches are constrainted to
matching only those things that fit all one one line.
.P
.VL 10
.LI "M-^S"
Regular Expression Search.  This will prompt for a
regular expression to search for.  You can edit the expression like
editing filenames.  You can search forward or backward, either
ending at the beginning or end of buffer, or wrapping around (like
ed) depending on the argument given to the search command:
.VL 10
.LI "1"
(default) Search forward, wrapping from the end of the buffer to the
beginning, and failing only if the buffer contains no match for the
given string. 
.LI "-1"
Search backwards, wrapping around from the beginning to end of the
buffer.
.LI "> 1"
Search forwards, stopping at the end of the buffer.
.LI "< -1"
Search backwards, stopping at the beginning of the buffer.
.LE
.P
In all cases, you can have emacs repeat the search, looking for the
next (or previous) occurrence of the search string by typing ^S or ^R
immediately after the regular expression search.  
.LI "M-^R"
Regular expression query replace.  This is just like
query replace, except that a regular expression is allowed in the
search string.  You may also use the special character sequence
\e<digit>, to specify that the characters matched by the
nth subexpression (delimited by \e( and \e)) are to be used in the
replacement string.
.LE
.H 2 "Macros, Keyboard Macros, and Input Files"
Emacs provides a number of ways for a user to construct editor
programs from sequences of commands.  The macro programming facility
is the best way to construct substantial programs, and is described
in a companion document.  The commands listed here deal with two
other ways of saving a sequence of emacs commands for later use,
Input files and Keyboard Macros, and with the commands that load
"full" macros into Emacs for your use.
.VL 10
.LI "^X^I"
Re-direct input.  This command directs EMACS to take input from a
file.  The file is assumed to contain EMACS commands, and can be
created by editing with EMACS, using ^Q to enter control and escape
characters.  You can also create an input file by using the commands to
create keyboard macros described below, and then saving the
resulting keyboard macro file.
.P
This command can be used to perform a series of
commands on the current  buffer, or to set up a standard set of
initializations.  Thus the file should contain \fIexactly\fP what
you would type from the keyboard to perform whatever task you wish
to perform.   Note that if the file contains only printable ASCII text,
tabs, and newlines, ^X^I will effectively read the file into the
buffer at the  current location.  Note, however, that this is very
slow, and much better done with ^X^R.
.P
A file suitable for executing with ^X^I is known as a keyboard
macro, because it is interpreted just as if it had been typed from
the keyboard.  The following commands provide a sensible way to
create and execute keyboard macros.
.LI "^X(
Begin Keyboard Macro.
This command starts remembering the keystrokes you enter so that
they can later be executed as a keyboard macro.
.LI "^X)
End Keyboard Macro.
This command stops remembering keystrokes for a keyboard macro.
.LI "^XE
Execute Keyboard Macro.
This command retrieves and executes the keystrokes typed between
^X( and ^X).  Emacs executes them just like they came from your
terminal.  Keyboard macros are saved in the file .emacs_kbd in your
home directory.  These are saved between sessions, so that ^XE is
in fact the same as invoking ^X^I (Input file) and giving
$HOME/.emacs_kbd as the file name to execute.  Note again that you
can use ^X( and ^X) to create a keyboard macro, save it for
later use by moving the file $HOME/.emacs_kbd to another file, and
then invoke it by invoking ^X^I with the name of the file you saved.
.LI "^Xd"
Define macros.
This command treats the current buffer as definitions of new macro
commands.  The commands are defined and become available for use. 
For a complete description, consult the macro programming manual. 
Note that you should not use ^Xd with the file created from a
keyboard macro.  The format of "full" macros is different.  Macros
are explained in a separate manual.
.LI "^X^L"
.tr ~~
Load macros.
This command allows you to load "full" macro definitions from a
file.  It is described in the macro programming manual, however even if
you do not program your own macros, you may be interested in using
those defined by others and will use ^X^L to load the resulting
files.  On most systems on which emacs is installed, there is a library
directory of macros available for general use in ~EMACS/macros, and
the file ~EMACS/macros/CATALOG gives a catalog to the available
macros.
.tr ~ 
.LI "^Z"
Exit level.
In an emacs editing session, you may wind up in a nested level of
emacs.  This can happen either by typing "break", and responding "y"
to suspend whatever you were doing and invoke a new command
interpreter, or by invoking a macro that uses the recursive edit
command to allow you to edit something from inside of a macro.  ^Z
exits your current level of emacs, returning to whatever called it.
.P
The ^Z command performs a function similar to ^X^C, and in fact if
you have not invoked a macro that uses the "recursive edit" command,
and have not hit break, both will cause you to exit from emacs.
.LE
.H 2 "Commands that escape to Unix"
There are several commands that interact with unix, allowing you to
run unix commands or send mail from inside of emacs.
.VL 10
.LI "M-!,M-$"
Unix escape.  These two commands implement five different ways to run
unix commands from emacs.  In all cases, the commands prompt for the
name of a unix command to be run and run it.  The command is run
through your normal shell as indicated by $SHELL.  If you enter the
special command name "sh", it runs your normal shell instead of
"sh".  To facilitate writing programs that interact with emacs, the
environment variable "filename" is set to the name of the current
file in emacs when the command is run. The following summarizes the
various flavors:
.VL 8
.LI "M-!"
Run the command, suspending emacs while it runs.
.LI "^UM-!"
Run the command and feed it the contents of your current buffer as
standard input.  When the buffer is exhausted, the command will see
an end of file.
.LI "M-$"
Run the command with standard input from your terminal but standard
output and standard error are captured in the buffer ".exec", which
is created if it doesn't already exist.  (If it does exist, the
command output replaces its current contents).  Normally, output
from the command is also displayed on the terminal as it is
produced, but this can be overridden via the usilent or noecho modes
described in the section on modes. 
This is useful for saving a copy of the error messages produced by a
C compilation of a file being edited, for example.  The file name of
the .exec buffer is set to the command line that produced it.  This
can be useful if you want to re-execute the same command, as you can
make .exec your current buffer, enter M-$, and enter ^Y followed by
newline as the command line.  ^Y gets the old command line back, and
newline will execute it.
.LI "^UM-$"
Run the command and append the output to the buffer ".exec".  This
is just like the above except that the .exec buffer is not cleared
and the output from the command is appended to it.  It also inserts
the command line into the .exec buffer when the command is run.
.LI "M-0M-$"
Start the command as a sub-process in the current buffer.  The
command is run with input and output connected to pipes maintained
by emacs.  Control returns to emacs immediately while the command
runs asynchronously.  Output produced by the command is
asynchronously appended to the buffer as it is produced, and the
mark is set to the end of the output produced by the command.  Input
can be sent to the command in one of two ways:  First, whenever you
enter a newline in this buffer and the mark is on the current line,
emacs will send the text between the mark and your newline to the
sub-process.  This means that if you run a shell in a buffer, you
can interact with it by typing in the buffer just as if it were
running independent of emacs.  When it prints its prompt, the mark
is set to the end of the prompt, and when you hit return, the
current line after the prompt is sent to the shell.  The second way
to send input to the buffer is to use the ^X^T command described
in the next section.  This sends the current region to a buffer.  If that buffer
has a sub-process, the region is sent to the end of the buffer and
also to the sub-process as well.  One use of this feature is to
provide a rapid way to send new function and data declarations to an
interpreted language like LISP.
.P
This feature is new as of emacs 4.9 and has some limitations you
should be aware of.  First, you can have only one sub-process
running at a time.  Running a second one will kill the first one. 
Killing the buffer will also kill the sub-process.  (Note that it
may not kill all of the descendants of the sub-process.)  On
Berkeley unix systems (4.2BSD), this feature is reasonably
inexpensive to use and the response from the command will be quick. 
On System V Unix, there may be some delay between the time at which
output is produced and the time at which it is seen.  Emacs will
always look for output from the process before looking for
characters typed by you at the terminal, however if you are not
typing, it may take some time for emacs to notice command output. 
(On some unix/370 and amdahl unix systems command output is not
noticed until you type at emacs due to a bug in the I/O software on
these systems).  Some commands may not run properly as
sub-processes, depending on what version of the standard I/O
software they were compiled with.  Most interactive programs will,
however, function properly in this mode.  Because of the different
nature of the implementation, this feature may also cause emacs to
consume more CPU time while a sub-process is running.
.LE
.LI "^X^G"
Interrupt sub-process.  If the current buffer has a sub-process running,
this command sends an interrupt signal (SIGINT) to that process,
just as if you had typed your normal interrupt character from the
terminal.  If you give this command an argument, it sends the signal
number corresponding to that argument.  (For example, M-9^X^G would
send a kill signal to the sub-process.)
.LI "M-^M"
Mail.  This command takes the current buffer as unix mail, and sends
it.  The buffer must contain at least one line starting To: , which
specifies the recipients of the mail.  Each recipient is delimited by a
space.  Any number of recipients may be listed in a single line,
however to improve readability, additional To: or Cc: lines may be
used in specifying lists of recipients.
Any errors encountered by mail are printed.  If the environment
variable $MAILER is set, then it is taken as the name of the command
to run to send the mail.  Otherwise, emacs runs "mail".
.LE
.H 2 "Miscellaneous commands"
The remaining commands handle special situations that occur once in
a while.
.VL 10
.LI "^X^T"
Send text to another buffer.
This command sends the text between the mark and the current cursor
position in the current buffer to another buffer.  EMACS prompts for
the name of the other buffer, and the text is inserted into that
buffer at the current cursor position for that buffer.  The current
buffer remains unchanged.  If an argument is given, it selects the
mark to use.  If the target buffer has a sub-process running under
it, then the region is also sent to that process  and is always put
at the end of the buffer. (See the description of M-$ for more
information.)
.LI "M-s"
Statistics
This command displays some statistics about your editing session,
such as how may characters emacs has sent to you and how many
characters you have typed.  The information is normally not of much
interest.
.LI "^X="
Status.  Displays status information (current line, number of lines
in buffer, current character position, number of characters in
buffer, etc.).                                                      
.LI M-/
Begin comment.  This command begins a C program comment by moving to
the appropriate column and putting a /* in the buffer.  The next
newline will close the comment and automatically append a */
.LI "M-_"
Underline word.  This command underlines the following word of text.
Useful for generating underlined text for mm.
.LI "^C"
Capitalize.
This command capitalizes the letter under the cursor and moves the
cursor forward one position.  Lower case alphabetic characters are
converted to upper case, while other letters are unchanged.
.LI "M-c"
Capitalize word.  The letter under the cursor is capitalized, and
the cursor is moved to the beginning of the next word.
.LI "M-l (4.6)
Lower case letter.  The letter under the cursor is capitalized and
the cursor is moved to the right.
.tr ~~
.LI "M-~"
Unmodify Buffer.  This command causes a buffer to be marked as
unmodified even if it has been modified since the last write.  Doing
this will avoid having EMACS ask whether or not to write the buffer
when you exit if you know that you do not want to rewrite the
buffer.  With an argument greater than 1 (i.e. ^UM-~) this command
marks the buffer as modified.  With an argument of zero or a
negative argument, it does not change the state of the buffer but
returns the buffer modified flag as its result.
.tr ~ 
.LI "M-t"
Set terminal type.  Prompts for terminal type and sets the character 
sequences used to display text to be appropriate for that terminal.  
Most common terminal types are supported.
Emacs uses terminal commands for relative and absolute
cursor position, clear screen, clear from the cursor to the end of
line, insert and delete lines, and insert and delete characters. 
The number of characters transmitted for re-display will depend on
which of these functions are available.  Thus EMACS transmits fewer
characters on an adm31, which has all of these functions, than with
an adm3a, which has only cursor positioning and clear screen.  Most
terminals fall between these extremes.
.LI "M-^L"
Redisplay top.  Redisplay the window with the current line at the top. 
This is useful for viewing the lines that follow the current line. 
Note that this does not re-create the entire display, as does ^L, so
it will not correct garbling.
.LI M-"
Auto Fill Buffer.  This command re-adjusts the lines in the buffer
so that each line contains 72 or fewer characters.  The adjustment
is done like nroff (mm) by moving words from one line to another. 
nroff or mm command lines and blank lines are preserved as is.  This
command can be used to improve the way in which an nroff or mm
source file displays, by getting rid of long lines, without
affecting the output.
.LI "M-:"
Remap character (version 4.9). The command M-: allows you to re-map
character commands. 
It prompts for a character (or a meta or ^X sequence) and a command 
(also a character sequence) to put on that character.  This allows you to
re-configure EMACS to your liking.  You can re-map any character you
like, including characters like ^X, escape, and ^U.  Note that the
command string is always interpreted with the default bindings (as
documented in this memo), and not with the bindings set up with
earlier M-: commands.  Thus M-:^A^B followed by M-:^B^A will swap
the ^A and ^B commands, since the command ^A in the second M-:
command refers to the default meaning of ^A, not that established by
the first M-: command.  M-: also changes the behavior of the
control characters used to edit filenames and other string
parameters, but does not change the behavior of some of the
characters that have special meaning in response to prompts issued
by various commands, such as the responses to query replace.
.P
With an argument of 2 or 4 (^UM-:), this command asks for a
character and a macro name to assign to that character.  This allows
you to bind macro commands to characters by their names instead of
their current character binding.
.P
With an argument of 0 (M-0M-:), this command resets all of the
keyboard character bindings to their default values.  This may be
useful in recovering from trouble.
.P
In all cases, the bindings established with this command and through
defining macros apply to both character commands typed from the
keyboard and to characters in keyboard macros and initialization
files.  They do not apply to the character commands executed in the
body of "full" macros.  See the section on macros and the
macro command manual for further information on bindings.
.LI "^X^M"
Set Mode.  This command can be used to set parameters to customize
the behavior of emacs.  For more information on modes, see the next
section.
.LE
.H 1 "Modes"
EMACS has a variety of parameters that can be
changed from commands entered in the terminal.
These are referred to as "modes" in this document and in the
messages printed by emacs.  
There are two types of modes:  on/off
modes and  integer modes.  The '^X^M' command can
be used to display or set these parameters.  '^X^M' will prompt for
the name of the mode to set.  If you enter a return in response to
the prompt, the current mode settings are displayed.  Normally,
emacs will display the value of each integer mode, and the name of
each on/off mode that is currently on.  If you enter return in
response to '^U^X^M', emacs will also display the names of the
on/off that are currently off, indicating for each mode whether it
is on or off.
.P
Modes are set by giving the name of the mode to set in response to '^X^M'.  
If an on/off mode is given, it is turned on if no argument
is given to '^X^M', and turns it off if an argument is specified. 
(Thus '^X^M' turns on, '^U^X^M' turns off).  For an integer mode, the
mode is set to the value of the argument.
.P
The modes and their types are listed in the following sections,  along
with their default values.  For ON/OFF modes, the default is \fBhighlighted\fP.
The modes are grouped into 4 broad categories:
.BL
.LI
Display modes change the information in the buffer is displayed, but
have no effect on it's content.
.LI
Interface modes change the command interface to emacs in minor ways,
but don't really change any of the behavior of the commands.
.LI
Command modes change the way in which some of the commands work.
.LI
Terminal modes change the way that emacs uses the terminal, and
exist mainly to get around special problems caused by certain kinds
of terminals.
.LE
.H 2 "Display modes"
.VL 15
.LI "lnumb"
Line Number Mode (\fBON\fP/OFF)
.br
This mode causes the current line number
to display at the left of each line.
.LI "lnowid"
line number width (INTEGER=4)
.br
This parameter specifies how many character positions are reserved
for the line number when in line number mode
.LI "height"
Display height (INTEGER=<screen_size-4>)
.br
This parameter dictates how many lines from the buffer will be
displayed on the screen.  It is automatically set based on the
terminal type whenever the terminal type is set, and is changed by
the one and two window commands.  This mode and width mode can be
set explicitly to restrict the display to a subset of the entire
terminal screen, or can be used to allow you to use a terminal with
a settable screen size for which emacs does not have the right size
built in.
.LI "width"
Screen Width (INTEGER = <set based on terminal type>)
.br
This mode specifies the width of the display screen.
.LI "tabstop"
Tabstop (INTEGER=8)
.br
This mode is the number of characters per tab that are displayed. 
Displaying a deeply indented c program may look much better if
tabstop is set to something smaller than the eight default.
.LI "backspace"
Backspace Mode (ON/\fBOFF\fP).
.br
Turning on backspace mode causes backspace
characters (^H) to display as moving back one column rather than as
a ^H.  This is very useful for viewing nroff output or manual pages,
but editing the resulting text can be a bit tricky, because it is
impossible to tell whether the character under the cursor is the one
being displayed, one that has been overprinted, or a backspace.
.LI "time"
Time mode (ON/\fBOFF\fP)
.br
When time mode is  on, EMACS will display the
time of day below the mode line (the one that says EMACS and the
buffer name).  The time is updated every time a character is read. 
Using time mode when entering lots of text is expensive.
.LI "display percent (4.5)"
Display cursor as percent of buffer (ON/\fBOFF\fP)
.br
If set, emacs will display the percentage of the current buffer
beyond the current cursor position on th mode line
.LI "7bit_ascii (4.8)"
Display only 7 bit characters (ON/\fBOFF\fP)
This mode controls how characters with the high order bit set are
displayed.  With this mode off, they are displayed as "M-" followed
by the character.  With this mode ON, they are displayed as
highlighted (underlined) characters.  This mode is most useful for
editing files used with Personal Computer word processing systems
which use the high order bit for formatting control.
.LI "leftmargin" (4.9)"
Left most displayed Column (INT=0).
In picture mode (See Section 5.3), emacs automatically scrolls the
window left or right to keep the cursor on the screen.  This mode
allows you to examine or alter this behavior.  The value of
leftmargin is the left
most displayed column.  Setting it will cause the screen to scroll
left or right.  In all cases, if the cursor wanders out of the
window, emacs will pick it's own leftmargin to keep it on the
screen.
.LE
.H 2 "Interface modes"
.VL 15
.LI "save"
Auto Save Mode (ON/\fBOFF\fP)
.br
If auto save mode is on, EMACS will
automatically write the current buffer after savetype characters
have been entered since the last save.  This mode reduces the chance
of disaster in the event of a crash, but can be annoying by causing
a lot of writing.  
.LI "savetype"
Save type ahead (INTEGER=256)
.br
If auto save mode is on, this is the
number of keystrokes between saves.
.LI "verbose"
Verbose mode (\fBON\fP/OFF)
.br
When verbose mode is on, EMACS will prompt
for more input when ^X, ^Q, escape, or ^U are entered.  This makes
it easier to keep track of where you are.
.LI "keepscroll"
scroll keep lines (INTEGER=0)
.br
This parameter specifies how many lines are to be preserved on the
screen when forward page or backward page is invoked.
.LI "caseless"
ignore case in searches 
.br
This mode causes case lower case characters in the search string to
match either case in the buffer on all searches and query replace.
.LI "mailtype"
check mail interval (INT=100)
.br
This parameter determines the number of input characters between
checks of your mailbox ($MAIL).  When emacs discovers something in
your mailbox, a warning is displayed at the bottom of the screen.
.LI "end_newline"
Behavior of ^N at end of buffer (ON/\fBOFF\fP)
.br
This parameter determines whether executing the ^N command in the
last line of the buffer adds a new line to the buffer (mode ON) or
whether it signals an error (mode OFF).
.LI "usilent"
Silent UNIX commands (ON/\fBOFF\fP)
.br
If set, this parameter causes emacs not to display the command name
or output for M-$.  This is useful for invoking a unix command in a
macro and capturing the output without disturbing the display.
.LI "noecho"
Don't echo M-$ output (ON/\fBOFF\fP)
.br
If set, emacs will not echo the output of commands run with M-$ to
the terminal.  The difference between noecho and usilent is that
usilent isolates commands run from emacs from the terminal
completely, so that emacs does not clear the screen and does not
have to redraw it when the command exits.  noecho has no effect at
all on commands run via M-!, but does eliminate the display of
output from M-$.  It is most useful when you wish to run something
that will produce lots of output, capturing the output in .exec.
.LI "eofnl (4.6)"
Write newline at end of file. (\fBON\fP/OFF)
.br
This mode when on causes emacs to append a newline character to any
file written by emacs from a buffer that does not end in a newline. 
Emacs allows you to create files that do not end in newline. 
Unfortunately, many unix tools get confused when reading such a
file.  This mode is on by default, and prevents you from writing a
file that will cause troubles.  For editing files which you do not
want to end in a newline, turn this mode off.
.LI "savelink (4.6)"
Preserve links on write (ON/\fBOFF\fP)
.br
Turning this mode on will cause emacs to automatically
write into the existing file when writing to a file with multiple
links, instead of asking the user what to do in this situation.
.LI "search_newline (4.6)"
Newline ends search (ON/\fBOFF\fP)
.br
Turning this mode on causes incremental search to stop if a newline
or carriage return is typed from the terminal.
.LI "autoload (4.7)"
Automatic macro loading (\fBON\fP/OFF)
.br
This mode controls the automatic loading of macro packages when an
undefined macro is called.  If autoload mode is on and an undefined
macro by the name of <name> is called, emacs will first
attempt to load
.tr ~~
$EMACS_LIB/<name> (resolving the environment variable $EMACS_LIB),
and then try to load ~EMACS/macros/<name>.  If either of these
files exists and defines a macro called <name>, that macro will be
called and execution will continue.  Otherwise, an error message is
produced.  This allows macros to be
loaded incrementally, only when needed.  With the mode off, any
attempt to call an undefined macro results in an error.
.tr ~ 
.LE
.H 2 "Command modes"
.VL 15
.LI "fill"
Auto Fill Mode (\fBON\fP/OFF)
.br
If this mode is on, emacs will
automatically move to the next line whenever the cursor moves to the
end of the line, breaking the line at a word boundary.  This is very
useful for entering text, as no newlines need be entered in the
middle of the text.
.LI "fillcol"
Auto Fill Column (INTEGER=72)
.br
This is the character position beyond which auto fill mode will
cause the line to be broken.
.LI "c"
C Mode (ON/\fBOFF\fP)
.br
This mode automatically indents for a C source
file.  Each line is indented with the number of tabs in the last 
non-comment line plus the net excess { characters over } characters
in the last line.  This mode is particularly useful for entering
c program text.
.P
This mode also changes the behavior of the '}' character, so that
when you type a '}' as the first non-blank character on a line, it
will adjust the indentation automatically to one less than it was. 
Thus you can type the following kind of statement and get it to
indent properly.
.DS

if(test) {
        statement;
}

.DE
.LI "comcol"
Comment Column (INTEGER=40)
.br
This is the column in which comments
entered via M-/ begin.
.LI rigid_newline
rigidly insert newlines (ON/\fBOFF\fP)
.br
This mode causes any newline to insert a newline into the file. 
With this mode off (the default).  a newline will not insert
anything if the following line is empty, but will simply move to the
next line.
.LI "readonly"
Read only buffer (ON/\fBOFF\fP)
.br
If this mode is turned on, saving the current buffer is disabled. 
^X^S will complain with an error message, autosaving will not take
place, and emacs will not complain if you try to exit without
writing buffers.
.LI "picture (4.5)"
Tailor editing for two dimensional displays (ON/\fBOFF\fP)
.br
If set, emacs treats the buffer as an "electronic blackboard",
rather than the exact contents of a unix file.  The display shows a
rectangular region of the blackboard through a window.  Characters
beyond the right margin are not displayed, but are indicated by
a '!' in the right margin.  The window is moved left or right to
keep the cursor in the window.  "leftmargin" mode gives you some
explicit control over the window, but in general it is self
adjusting.  If the window is not at the left
edge of the blackboard, then the character offset of the left edge
of the display is given on the mode line, in front of the editor
name.
.P
Picture mode changes the behavior of several basic commands to be
more suitable for two dimensional editing.
.VL 10
.LI "^N/^P"
These move to the same column in the target line, extending the
target line with spaces if necessary.
.LI "^F/^B"
These will not move off the current line.  ^B will stick at the left
margin, while ^F will continue to extend the line to move to the
request column.
.LI "^W/^Y"
These treat the region to be deleted as a rectangle, bounded by the
mark and the cursor position at the corners.  All text in the
rectangle defined by these positions is killed by ^W.  ^Y brings
text back in the same way.  All text deleting commands behave like
^W, in that the start and end positions of the text region to be
deleted are taken as corners of a rectangle.  When the text region
being deleted is all on one line, behavior is identical to normal
emacs, however deletion across line boundaries via commands like
M-d may not do anything sensible.
.LE
.P
Picture mode is probably most useful with nodelete mode, notabs
mode, and overwrite mode all set.  You can use picture mode without
the others set, but the presence of tabs or control characters in
the buffer may give unexpected results when you navigate in the
file.  This mode is particularly useful for editing fixed format
tables, picture images of "typewriter art" for use with the "gc"
graphics output package, and other two dimensional information.
.LI "overwrite"
Overwrite mode (ON/\fBOFF\fP)
.br
In overwrite mode, text entered will
overwrite text already there.  Text entered when the cursor is at
the end of a line will be inserted as before.  Overwrite mode may be
more natural for some people when making corrections.
.LI "nodelete"
No deletion (ON/\fBOFF\fP)
.br
With this mode set, text deleted in the file is not closed up but
instead is overwritten with spaces.  After any deletion, the cursor
reverts to the first character of the region deleted.  "nodelete"
mode should probably be part of overwrite mode, however overwrite
mode considerably pre-dated it and was left alone for upward
compatibility.
.LI "notabs"
Eliminate tabs (ON/\fBOFF\fP)
.br
With this mode set, emacs does not display tab characters in the
buffer as whitespace, but instead shows them as ^I (control-i),
which is the ASCII code for a tab.  When you enter a tab by typing
^I or tab, emacs converts it to the proper number of spaces to come
up to the next tabstop column.  Emacs does not convert tabs already
in the file, though there are macros that do this.
.P
The main use of this mode is in creating and editing fixed format
information in picture mode, though it can also be useful in
showing you where tabs are in your buffer.
.LE
.H 2 "Terminal modes"
.VL 15
.LI "nobell"
No Bell (ON/\fBOFF\fP)
.br
Ordinarily, unexpected conditions, such as errors or quitting out
of commands, cause the terminal bell to ring.  On some adm3a
terminals, This causes some kind of disaster to occur.  Turning on
nobell mode prevents EMACS from ringing the terminal bell.
.LI "tspeed"
Terminal Speed (INT=<terminal dependent>)
.br
This parameter is the speed of your terminal in milliseconds per
character.  This parameter is set whenever you enter emacs and is
used in determining how to update the display most efficiently. 
.LI "controlify"
Controlify mode (ON/\fBOFF\fP)
.br
When on, this mode causes a particular character (normally ^^, but
settable to another character with the ctl_char described below), to
act as a prefix specifying that the next character is to be
interpreted as a control character.  This mapping takes place at any
time, not just when entering commands.  Thus the sequence ^X^^s will
be mapped into ^X^S, and cause the save command to be invoked.  The
sequence ^^q^^m typed in response to a request for a file name will
be mapped into ^Q^M, which will be interpreted as asking for a file
name of ^M.  Controlify mode is primarily intended to allow you to
enter characters which cannot be sent transparently from your
terminal to emacs.  The most frequent examples are ^S and ^Q, which
cannot be sent by some terminals and some local area networks. 
By setting controlify mode, you can send these characters with ^^s
and ^^q.
.LI "ctl_char (4.6)"
Controlify Character (INT=30)
.br
This parameter sets the value of the character used when in controlify
mode to indicate that the next keystroke should be interpreted as a
control character.  It is the ascii value of the character to be
used.
.LI "flow_lim (4.6)"
Flow Control Limit (INT=0)
.br
This parameter enables the use of xon/xoff flow control during
output to control the rate of output to the terminal.  Emacs
normally turns xon/xoff flow control off to allow ^S and ^Q to be
passed to emacs to be used in specifying commands.  If flow_lim is
non-zero, then whenever more than flow_lim characters are sent to
the terminal at once, xon/xoff flow control will be temporarily
enabled to allow the terminal to send ^S to request that unix stop
output.  xon/xoff flow control is disabled when output is complete. 
The only effect you will see is that you cannot type ^S or ^Q ahead
while emacs is updating the screen.  Normally, emacs supplies
sufficient padding to allow it to run without xon/xoff flow control,
however for some terminals at high speeds xon/xoff flow control is
required.  In this case only, set flow_lim to a number somewhat
larger than the terminal's character buffer (typically 32, 64, or
128).  In some cases it may be desirable to set up the terminal to
always use ^S/^Q for flow control.  This can be done by setting
flow_lim to  -1  (escape - ^X^M flowlim).  If you do this, you will
not be able to type ^S or ^Q from the keyboard and must use
controlify mode to do so.
.LE
.H 2 "Specifying Default modes for a File"
You can specify the modes to be used while editing a particular file
by putting the string "EMACS_MODES: " somewhere in the first 10
lines of the file.  The text on the same line following EMACS_MODES:
will be taken as names of modes to set on or off.  A mode name
preceded by '!' will be set off, while mode names just listed will
be set on.  If '=' immediately follows a mode name, then the
characters immediately following the '=' will be taken as the value
for the mode name.  Any text on the line that does not correspond to a mode
name will be ignored.  Thus the line:
.SP
/* EMACS_MODES: !fill c, comcol=43 */
.SP
in a c source file will set c mode on, fill mode off, and set the
column for starting comments to 43.  These modes are
set whenever the file is read, and
whenever you switch buffers.
.H 1 "Getting started"
When EMACS is invoked, it does a number of things to set up for
your editing session,  These include finding out the type of your
terminal, processing an initialization file that allows you to
customize EMACS for your needs, and processing command line
arguments.
.H 2 "Setting your Terminal Type"
Emacs must know what type of terminal
terminal that you are using in order to know the appropriate
control commands to display text on your screen.
The terminal type is taken from the shell variable $TERM.  To make it
work, you must set the value of TERM in your .profile, and export it.
.P
On some systems, there is a utility program called "ttype" which
figures out your terminal type for you.  It works by sending an
escape sequence to your terminal and testing for a response.
Thus the lines:
.SP
export HOME PATH MAIL LOGNAME LOGTTY TERM
.br
TERM=`ttype`
.SP
will set the terminal type correctly if ttype is available, and if
your terminal responds correctly.  If TERM is not set or
is set to an unrecognizable type, EMACS will prompt for terminal
type, however the prompting message may be formated oddly, since
at this point emacs will not know how to control your terminal.
You can also set your terminal type by putting a M-t (Set terminal
type) command in your initialization file, as described in the
following section.
.P
Most terminals commonly used at Bell Labs are acceptable for use
with emacs.  Support for other terminals could easily be added if needed.
If you need support for an unsupported terminal, either consult the
document ~EMACS/term_support for a description of how to do it
yourself, or contact ihnss!warren for details.
.P
There are some special problems with certain terminals or
particular access arrangements connecting your terminal to emacs
that you may run into:
.VL 25 
.LI "adm3a:"
There are two "features" of the adm3a terminals that sometimes cause
trouble in EMACS.  One of the internal switches on that terminal is
labeled space/advance.  That switch must be in the space position for
EMACS to work properly.  A second problem is that many of these
terminals are set up to use one of the "undefined" pins of the
rs-232 interface for some internal diagnostic.  This causes the
terminal to go wild every time it receives a bell character when
connected to a 1200 baud modem (which uses the same pin for
something else).  If this happens to you in EMACS, set nobell mode,
which will prevent EMACS from sending bell characters to your
terminal.  A better and more permanent solution is to modify the
terminal to disconnect
this lead, use a cable that does not carry the undefined signals
between the modem and the terminal.
.LI "TTY5620 or Blit:"
The Blit or Teletype 5620 terminals can be used in a number of
different ways.  The terminal types "blit" and "5620" will work well
on a stand-alone blit or 5620.  The only difficulty is that the
terminal will not transmit ^S and ^Q, requiring you to use
controlify mode to send these characters (as described below). 
Emacs will also run in a layer defined on the terminal.  
Emacs should discover the size of the window automatically, but
will not discover it if you reshape the window while emacs is
running.  The terminal type "layer" will work with the layer terminal
driver on the blit, but is very crude and buggy.  The terminal type
5620 will work from a layer inside of the TTY 5620, with the above
limitation.  The hp and vitty emulators will work with emacs on both
the blit and 5620.  
.P
There are several improved terminal emulators that work better with
emacs than the standard hp emulator or the straight 5620 layers
terminal.  There is an improved hp emulator that will transmit ^S
and ^Q.  The "netty" terminal emulator is widely available for both
the blit and the 5620.  It works well with emacs, and works with a
macro package that can be loaded into emacs to allow you to
position the cursor with the mouse and to do a certain amount of
cut and paste style editing.  The "emacsterm" emulator is available
for the 5620 in experimental tools, and also works with an emacs
macro package to provide mouse oriented editing.
.LI "VT100 or hp2621B:"
Both of these terminals work fine with emacs when run at low
speeds, but at 9600 baud, these terminals do not keep up with the
flow of characters.  To use either of these terminals at 9600 baud,
you should set  "flow_lim" mode to 32 in emacs, and avoid typing while the
display is being recreated.  In addition, some of the VT100
compatible terminals will not send ^S or ^Q to emacs and must be
used with controlify mode.
.LI "Local Area Networks:"
Some local area networks use ^S and ^Q for flow control, and thus
you will not be able to send these characters directly to emacs. 
You can get around this by using controlify mode as described in
the previous sections.  You can make this less awkward on terminals
with user programmable function keys by setting some of the
function keys to send the character sequences necessary to get ^S
and ^Q.
.LE
.H 2 "Initialization file.
When you start emacs, it consults
initialization file that allows you to initialize the various modes
the way that you want them.  This file is called ".emacs_init" and
should be put in your home directory.  The file is read as a
keyboard macro, and thus should contain \fIexactly\fP what you
would type from the terminal in order to set it up.  Thus the file:
.tr ~~
.SP
.DS
^U^X^Mfill
^X^Mc
^X^L~EMACS/macros/loader
.DE
.SP
sets 'c' mode, unsets fill mode, and loads the loader macro
package.  If you do not have a .emacs_init file, emacs will run the
standard initialization file for your system, which you can examine
by looking at the file ~EMACS/.emacs_init.  It will not run both by
default, but you can call for the default initialization file to be
run from your private file by placing ^X^I~EMACS/.emacs_init in
your initialization file at the point at which you want the system
initialization to be done.
You can also cause initializations to take place by command line
arguments (see below).
.tr ~ 
.H 3 "Command line arguments"
Emacs accepts a number of arguments on the command line that effect
processing.  These are:
.VL 10
.LI "-i"
Emacs interprets the next argument as the name of an initialization file to
run in addition to the standard initialization file (Yours or the system's). 
The specified file is run \fIafter\fP .emacs_init.
.LI ".i"
Emacs interprets the next argument as the name of an initialization file to
run \fIinstead\fP of the standard file.
.LI "+n"
Emacs moves to line n of the specified file after it is read in.
.LI "<filename>
The last argument on the command line should be the name of the
file to be edited.  This file (if present) is read into the buffer
Main.  If no filename is specified, emacs puts you in the buffer Main
with no associated file.
.H 2 "Helpful hints"
This editor is very easy to use, once you know a few of the basic 
commands.  Here are some tips for making the best use of emacs.
.AL
.LI
Learn a few of the basic commands at a time.  You can accomplish a
lot with just the basic commands.
.LI
EMACS tries to be reasonably efficient about the refreshing of the
screen.  Some  sequences, however, will cause lots of text to be 
redisplayed.  While you can insert anything into the middle of a buffer by 
typing it, it is much better to open up some lines by using ^O
where you want to  insert, insert, and then kill unneeded lines.
.LI
Use M-? when in doubt about a command.  The explanations are brief, but 
should be sufficient to tell you what you want to know.
.LI
Like most editors, EMACS maintains a local buffer, so that changes made do
not go into the file until the next write.  Type ^X^S reasonably frequently
so as to avoid being wiped out by machine crashes, editor bugs, or other 
unpredictable events.
.LI
Because EMACS tries to avoid unnecessary refreshing of the screen,
it will get confused if characters are sent to your terminal from
some other program while running EMACS, such as a "background"
program, or by a write command from another user.  If you suspect
that the display does not correspond to the buffer that you are
editing, type ^L to refresh the screen.  After typing ^L, the screen
will match the buffer being edited.
.LE
.H 2 "Limitations of the editor"
There are some limits that you may encounter:
.BL
.LI
Each line can hold at most 512 characters.
.LI
Each buffer can hold a maximum of approximately 15,000 lines.
.LI
You can have at most 12 buffers.
.LI
The kill stack contains 
the 8 most recent deletions, or a total of 256K characters.
.LI
Filenames are limited to 128 characters.
.LE
.H 2 "Recovering from various problems."
Because EMACS puts your terminal in "raw" mode, it does not respond
to interrupt and quit characters the way that most unix programs
do.  If somethings goes wrong with emacs, you can usually stop it
with BREAK (typing ^Z to exit from emacs in response to the
message).  You may have to kill it from another terminal or log
out.
.P
If emacs runs into internal trouble, or if you kill emacs or log
out while running emacs, it will try to save your buffers before
terminating.  The buffers are saved in the files emacs0-emacs11 in
your home directory. You will get mail describing what files are
there from emacs after this happens, though if you do not clean up
these files, they will continue to appear in the message each time
you hang up on or kill an emacs process.
.H 1 "DIRED"
The program DIRED (DIRectory EDitor) is a version of emacs for
editing directories.
DIRED takes an argument which is the name of a directory, and
displays the result of ls -al executed on that directory.  Dired
allows you to edit the resulting buffer using most of the normal
commands.   You may edit it by overwriting the protection mode,
owner, or group fields, or by deleting entries, as discussed below. In
order to facilitate your editing of the fixed format data in the
directory listing, Dired puts you in "overwrite" mode by default, so
that characters that you type overwrite what is there rather than
inserting into it.  
.P
The changes that you indicate this way are not made immediately, but
are indicated by the letters 'D' (file deleted) 'M' (modes modified)
or 'O' (owner or group changed) at the left end of the file entries
modified.  When you try to save the buffer or exit, Dired asks you
if you want to make your changes permanent and then does them.
.P
Most of the usual emacs commands work in Dired just like in Emacs
with a few exceptions:
.VL 15
.LI "^D, ^?, and ^K"
These commands do not delete any text, but simply mark the file
designated on the current line for deletion.
.LI "d or D"
These characters mark the current file for deletion when typed at
the beginning of a line, and otherwise just overwrite what is there.
.LI "u or U"
These characters remove the Deleted mark from files when typed at
the beginning of a line, and otherwise just overwrite what is there.
.LI "e"
When typed at the beginning of a line, this causes dired to examine
the directory entry corresponding to that line.  If the entry is a
file, dired creates a new buffer and reads it in, allowing you to
examine it just like in emacs.  When you enter ^Z, Dired returns you
to the directory.  If the entry is a directory, Dired creates a
buffer and puts a directory listing of that directory in it,
allowing you to edit this just directory.  This feature provides a
very convenient way to scan through a directory structure.
.LE
.P
Ordinarily, the buffer listing for dired is created with a "ls -al"
command.  You can specify additional options for the display by
giving dired an argument beginning with a "-" to specify options. 
Thus "dired~-t~/usr/bin" will invoke dired on /usr/bin with a
listing sorted in time order.
.P
Dired provides a very convenient way to scan through and make
changes to directories.  It is particularly useful for cleaning up
directories that may have files that you have not used for some time
in them.
.P
Dired can be confused if you edit the filename of the entries, or
succeed in inserting or deleting characters in the entries.  It is
not very robust to such failures, and you are advised not to modify
the format of entries.  If you have somehow garbled a buffer with a
directory listing in it, you should probably kill the buffer so that
Dired will not try to update that directory from the garbled listing
when you try to exit.
.H 1 EXPERIENCE
The author originally wrote EMACS in 1979 as an aide to his own work of
programming and memorandum composition.  EMACS has been widely
available inside of Bell Laboratories through the Experimental Tools
facility, and through the author's own distribution efforts.  EMACS
now part of the AT&T Toolchest package and is available commercially
to anyone paying the nominal fee for it.  The following section
describes observations of the author and others on 6 years of
experience with Emacs.
.H 2 "Feature Use"
The most commonly used "feature" of EMACS is simply the ability to
enter text and see the result.  The simple cursor movement commands
appear to be much more frequently used than the more complicated
commands, such as those moving forward or backward by sentences.
.P
The following is a summary of the user reaction to some of the
unique features of EMACS.
.AL
.LI "Multiple Buffers"
.br
The ability to maintain several editing buffers at the same time
is widely used for many purposes.  These include examining several
source files while making modifications, holding source, compiler
output, and program output while debugging, and holding the output
of the spell utility while scanning a document for misspelled words.
The availability of multiple buffers allows the user to manage
several different tasks, such as writing a program, editing a
memorandum, and helping a friend locate a system problem, concurrently.
.LI "Two Window Mode"
.br
Two window mode is less heavily used than some of the other
features, but appears to be quite useful for a variety of tasks. 
These include viewing a buffer containing declarations at the same
time that a program is being entered in the second window, and
viewing the output of the spell program at the same time that the
document is being edited.  The fact that only one of the two
displays is actively maintained does not seem to be a problem, nor
does the limitation of only two display windows.  Few users have
objected to these limitations, though this may only indicate that
users are grateful enough for what Emacs does provide that they have
not run into them.
.LI "Incremental Search"
.br
With a high speed terminal, incremental search seems to be very
effective.  The ability to quickly move from one occurrence of a
string to the next seems to meet most user's needs.  With
significantly less than 9600 baud communication, the amount of
re-display created by incremental search becomes bothersome, and
the regular expression search is preferred.  The fact that
incremental search does not "wrap around", seems to be a significant
problem to many users.
.LI "C mode and Fill mode"
.br
These two modes provide customized facilities for editing certain
kinds of files.  The user reaction to these is mixed.  Some users
make extensive use of these, while others have their own styles for
entering memorandum or program source and do not use these modes. 
They are a very small part of EMACS, and as such are worthwhile even
if they are not extensively used.
.LI "Line Numbers"
.br
The display of the line number (lnumb mode) is one aspect of EMACS
that was not taken from the M.I.T. version.  This appears to be
extremely useful for use with UNIX, primarily because of the
widespread use of line numbers to indicate position within a file. 
Line numbers also serve to provide the user with some feedback as to
what part of the buffer is currently being displayed.
.LI "Macros"
The macro facility was built up over time, and only recently has
become a programming environment capable of being used by a wide
variety of users.  The initial macro language was powerful but
extremely cryptic.  Nevertheless, hundreds of macros were built, ranging from
simple commands that merely combine several basic commands to
self-contained editing systems making use of complex control flow
features.  Some of the more significant ones include a package for
correcting spelling errors, Several packages that compile C code and
step through the error messages, Several mail handling packages, and
Several packages implementing interfaces to mouse-equipped
terminals.
.P
In retrospect, our experience supports the conclusions of Richard
Stallman (The author of the original Emacs), that the "right" way to
build a powerful editor is to build a powerful programming
environment and build the user interface of the editor on top of it.
.LI "Picture Mode"
Picture mode is another feature that was originally unique to this
implementation.  It was inspired by the Two-Dimensional editor
2ded, and by an electronic debate on whether screen editors should
present the file as a sequence of characters or as a two dimensional
blackboard.  Both are useful abstractions, and it was not difficult
to provide both views given the structure of Emacs.  Picture mode
has been widely used for editing tables and other fixed format
information.  It is also sometimes convenient for mouse based
interfaces.
.LE
.H 2 "Human Interface Observations"
There are several controversial aspects of the interface that emacs
presents to users.
.VL 15
.LI "Modeless Editing:"
The characteristics which most clearly distinguishes emacs style
editors from others is that lack of distinct modes for text
insertion and entering of commands.  This is also the most
controversial feature.  There have been many studies on moded and
modeless editors in the 10 years since the first Emacs editors
got significant use.  Most conclude that in spite of strong
preferences among users for one style or the other, neither is
clearly superior.  Also, in spite of feelings of users of both
modeless and moded editors that doing something in the wrong mode
would be a significant source of errors, the data do not show this as
a large problem.
.P
Our feeling on this issue is that there are different editing styles
for modeless or moded editing.  With a moded editor, you think of
editing as a series of commands, each of which changes the file in
some way.  With modeless editing, you tend to work more in the mode
of marking up a document with a red pencil, by moving to each
problem and re-typing whatever is wrong.  Modeless editing seems
natural in conjunction with a pointing device and menu interface,
and has been used effectively on many personal computer and word
processor editors this way.
.LI "Control Characters"
The use of control characters for editing commands in emacs is an
unavoidable consequence of modeless editing.  A conscious decision
was made in emacs to avoid the use of special keys on the keyboard,
such as function and arrow keys.  The rational for this was to avoid
requiring users to locate such keys, which appear in different
places on every terminal.  The result is that touch typists can use
emacs without ever having to look at the keyboard, which is
difficult if you must locate the special keys.
.P
The interface seems to work quite well for frequent users, however
casual users have trouble remembering which characters do what. 
Many users with programmable function keys on their terminals have
programmed and labeled them to send useful emacs commands.  The
conclusion that can be drawn is that it is nice to be able to touch
type all of the commands, and for many users it is desirable to be
able to use the special function keys on many terminals for editing.
.LE
.H 2 "Performance"
While computer resources are not as expensive as they once were,
they are far from free.  An interactive screen editor such as EMACS
consumes more in computer resources than a conventional editor. 
EMACS was designed to be as efficient as possible, some price must
be paid for the benefits gained.
.P
The Processor time consumption of EMACS is comparable to the UNIX
editor for most "commands", including reading and writing files,
searching, and substitution.  The major discrepancy in performance
comes in entering text.  In entering text, EMACS consumes
about 3 times as much processor time as the UNIX editor. 
.P
If we examine how each editor uses its time, we see that almost all
of the time spent by the UNIX editor is spent in the UNIX operating
system reading characters.  About two thirds of the time spent in
EMACS is spent reading and writing, while the remaining third is
spent re-creating the display.  EMACS uses almost twice as much UNIX
operating system processor time, because it is re-writing the
display after each character is typed, in addition to reading the
characters.  In addition, EMACS spends a significant amount of user
process time figuring out how to most efficiently re-display the
screen.
.P
While EMACS does consume more time editing than does the UNIX
editor, the processor time spent editing does not appear to be a
significant problem.  Measurements of CPU consumption of emacs made
on VAX 11/780, 3B20S, and PDP 11/70 systems each indicate that an
emacs user consumes on average about .01 second of cpu time per
character typed, and about 1% of the processor over the period of an
editing session.  These rates suggest that a substantial number of
users can be accommodated on most systems.
.H 1 CONCLUSIONS
The EMACS editor provides an effective means of using a high-speed
display terminal for text editing.  EMACS provides a variety of
unique features that seem to be very useful in editing.  User
reaction indicates that using EMACS has improved their productivity,
however there are no quantitative measurements of this effect.
.P
EMACS uses more computing resources than the standard UNIX editor,
however the resources utilized do not appear to be a serious
problem.  Emacs has been used effectively on many different machines
without serious difficulties in performance.
.P
EMACS was written by the author as a tool for his own work, but is
available to anyone desiring a copy.  There are no currently known
bugs, however there may be undiscovered problems.  The author is
interested in user reaction to EMACS and in reports of problems,
however, the author's job
does not include supporting EMACS, and thus the author does not
promise any prompt response to suggestions or trouble reports.
.SG UNIX
.SK
.tr ~~
.nr Hu 1
.nr Hc 1
.HU "APPENDIX -- EMACS Command Summary"
.SP 2
.nf
EMACS version 4.9 Date: Thu Mar 28 14:17:56 CST 1985

^@:	sets the mark at the cursor position
^A:	moves to the beginning of the line
^B:	moves back one character
^C:	capitalizes the current character
^D:	deletes forward one character
^E:	moves to the end of the line
^F:	moves forward one character
^G:	quits from any command in progress
^H:	deletes backward one character
^I:	inserts a tab
^J:	opens a new line and moves to the beginning of it if the
	next line is non-empty, otherwise down one line
^K:	kills to end of line (with argument, kills multiple lines)
^L:	refreshes the screen
^M:	opens a new line and moves to the beginning of it if the
	next line is non-empty, otherwise down one line
^N:	moves down one line
^O:	opens up a new line
^P:	moves up one line
^Q:	quotes the next character
^R:	starts a reverse search
^S:	starts a search
^T:	transposes the next two characters
^U:	multiplies the argument by 4
^V:	moves to the next page
^W:	kills the current region (between cursor and mark)
^X:	is a prefix for more single character commands,
	type character or '*' for all
^Y:	restores last killed text
	(leaves cursor and mark around it)
^Z:	exits one level
^[:	makes the next character a meta character
^]:	makes a local variable of a macro invocation the argument to the
	next command
^^:	causes the last returned result to become the argument
 :	is self inserting and check for automatic word wrapping
-:	is self inserting unless part of a numeric argument
.:	is self inserting and check for automatic word wrapping
0:	is self inserting unless part of a numeric argument
1:	is self inserting unless part of a numeric argument
2:	is self inserting unless part of a numeric argument
3:	is self inserting unless part of a numeric argument
4:	is self inserting unless part of a numeric argument
5:	is self inserting unless part of a numeric argument
6:	is self inserting unless part of a numeric argument
7:	is self inserting unless part of a numeric argument
8:	is self inserting unless part of a numeric argument
9:	is self inserting unless part of a numeric argument
}:	is self inserting, and re-adjusts indentation in C mode
^?:	deletes backward one character
M-^H:	deletes the last word
M-^L:	re-displays with current line at top of page
M-^M:	mails the current buffer
M-^Q:	returns the next input character (in a macro)
M-^R:	regular expression query replace
M-^S:	regular expression search
M-^X:	executes argument 0 as a character command.
M-^Z:	suspends emacs
M-^]:	assigns the result of the next command to a macro local variable
M- :	sets the mark at the cursor position
M-!:	gets and executes a shell command
M-":	auto Fills the whole buffer
M-$:	executes a command, saving the output in buffer .exec
M--:	is self inserting unless part of a numeric argument
M-/:	starts a comment
M-0:	is self inserting unless part of a numeric argument
M-1:	is self inserting unless part of a numeric argument
M-2:	is self inserting unless part of a numeric argument
M-3:	is self inserting unless part of a numeric argument
M-4:	is self inserting unless part of a numeric argument
M-5:	is self inserting unless part of a numeric argument
M-6:	is self inserting unless part of a numeric argument
M-7:	is self inserting unless part of a numeric argument
M-8:	is self inserting unless part of a numeric argument
M-9:	is self inserting unless part of a numeric argument
M-::	maps a character to a command
M-<:	moves to top of file
M->:	moves to bottom of file
M-?:	explains the next character
M-E:	expands an environment variable and returns the result on the kill stack.
M-\:	converts its argument to a character and inserts it
M-_:	underlines the next word
M-a:	moves to beginning of sentence
M-b:	moves back one word
M-c:	capitalizes the next word
M-d:	deletes the next word
M-e:	moves to End of Sentence
M-f:	moves forward one word
M-g:	moves to a specific line (its argument)
M-l:	converts the next letter to lower case
M-m:	displays active modes
M-p:	puts the current region in the kill buffer without killing it
M-q:	quotes the next character and adds the 0200 bit
M-r:	starts query replace
M-s:	gives EMACS statistics
M-t:	prompts for terminal type
M-u:	undoes the last significant text modification
M-v:	moves back one page
M-w:	puts a wall chart of explanations in the buffer
M-x:	calls a macro by name
M-y:	replaces the last restore() with the next text in
	the kill stack.
M-z:	kills emacs with a core dump (for debugging)
M-{:	enters a command sequence (in a macro)
M-}:	exits one level
M-~:	marks a buffer as being unmodified (up to date)
M-^?:	deletes the last word

Control-X commands:

^X^A:	accesses the argument list to emacs
^X^B:	changes Buffers (Change to * lists active buffers)
^X^C:	exits gracefully
	(after asking whether or not to save the buffer)
^X^D:	changes the working directory
^X^E:	calls emacs recursively taking input from the terminal
^X^F:	edits a file in its own buffer
	(if file has been read into a buffer, moves to it)
^X^G:	sends an interrupt signal to a sub-process in the current buffer
^X^I:	re-directs input from a file
^X^K:	kills a buffer
^X^L:	loads a file full of macro definitions
^X^M:	sets mode from argument (prompts for mode name)
	and string if necessary
^X^N:	changes the buffer or file name
^X^O:	switches between windows
^X^Q:	returns the character under the cursor (in a macro)
^X^R:	reads a new file
^X^S:	saves the buffer in the current file (if modified)
^X^T:	prompts for a buffer name and inserts the text between the
	cursor and the mark into the named buffer.
^X^U:	updates the display and delays for a specified time
^X^V:	puts the current version on the kill stack.
^X^W:	writes a new or old file
^X^X:	exchanges the mark and the cursor
^X^^:	causes the current window to grow one by line
^X!:	begins a case statement (in a macro)
^X#:	reads or writes global variables
^X%:	exchanges the top of the kill stack with another item
^X&:	compares two strings
^X(:	starts a keyboard macro
^X):	ends a keyboard macro
^X+:	causes the next entry to the kill stack to append to the previous entry
^X-:	pops the kill stack
^X1:	exits two window mode
^X2:	enters two window mode
^X<:	pushes a string from the tty or macro text into the kill stack
^X=:	gives statistics about the buffer
^X>:	duplicates an item on the kill stack
^X@:	prompts the user with a string from the kill stack and returns the result.
^XB:	puts the buffer name into the kill stack
^XE:	executes the keyboard macro
^XF:	puts the file name into the stack
^XL:	makes the next character a meta character
^XT:	traces the next command.
^X^:	enters a "while" loop (in a macro)
^Xd:	defines macros from the current buffer
^Xg:	moves to a screen position (arg=128*y+x);
^Xk:	sets the encryption key for file reading and writing
^Xm:	sets mode from argument (prompts for mode name)
	and string if necessary
^X|:	begins a conditional execution sequence (in a macro)
^X~:	performs arithmetic or logical operations (in a macro)
.fi
.SK
.NS ""
All Supervision Lab 5523
.NE
.TC
