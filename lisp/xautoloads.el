;; Debugging autoload definitions for Emacs.
;; Copyright (C) 1985 Richard M. Stallman.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; GNU Emacs so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.


;Autoload definitions for testing Emacs.
;Load this file for testing a newly linked Emacs
;when you don't want to take the time to load
;all the files that these commands live in.

;When Emacs is built and dumped for installation,
;these commands are all loaded, so this file is not used.

(autoload 'how-many "replace"
  "Print number of matches for REGEXP following dot."
  t)

(autoload 'occur "replace"
  "Show all lines containing of REGEXP following dot.
Display each line with NLINES lines before and after.
NLINES defaults to 0.  Interactively it is the prefix arg."
  t)

(autoload 'keep-lines "replace"
  "Delete lines not containing matches for REGEXP.
Applies to lines after dot."
  t)

(autoload 'flush-lines "replace"
  "Delete lines containing matches for REGEXP.
Applies to lines after dot."
  t)

(autoload 'c-mode "c-mode"
  "Set major mode for editing C code." t)

(autoload 'text-mode "text-mode"
  "Set major mode for editing text intended for humans to read."
  t)
(autoload 'lisp-mode "lisp-mode"
  "Set major mode for editing Lisp code." t)

(autoload 'forward-list "lisp"
  "Move forward across one balanced group of parentheses.
With argument, do this that many times." t)
(autoload 'backward-list "lisp"
  "Move backward across one balanced group of parentheses.
With argument, do this that many times." t)

(autoload 'backward-up-list "lisp"
  "Move backward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot." t)

(autoload 'up-list "lisp"
  "Move forward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot." t)

(autoload 'mark-sexp "lisp"
  "Set mark ARG sexps from dot." t)

(autoload 'down-list "lisp"
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level." t)

(autoload 'forward-sexp "lisp"
  "Move forward across one balanced expression.
With argument, do this that many times." t)

(autoload 'backward-sexp "lisp"
  "Move backward across one balanced expression.
With argument, do this that many times." t)

(autoload 'kill-sexp "lisp"
  "Kill the syntactic expression following the cursor.
With argument, kill that many expressions after (or before) the cursor." t)

(autoload 'backward-kill-sexp "lisp"
  "Kill the syntactic expression preceding the cursor.
With argument, kill that many expressions before (or after) the cursor." t)

(autoload 'eval-print-last-sexp "lisp"
  "Evaluate sexp before point and print results into buffer at point.
With argument, print the results into another buffer named Output.
Text printed during evaluation goes in front of the value." t)

(autoload 'beginning-of-defun "lisp"
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer." t)

(autoload 'end-of-defun "lisp"
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one." t)

(autoload 'mark-defun "lisp"
  "Put mark at end of defun, dot at beginning." t)

(autoload 'insert-parentheses "lisp"
  "Put parentheses around next ARG sexps.  Leave dot after open-paren.
No argument is equivalent to zero: just insert () and leave dot between." t)

(autoload 'move-past-close-and-reindent "lisp"
  "Move past next ), delete indentation before it, then indent after it." t)

(autoload 'dot-to-register "register"
  "Store current location of dot in a register.
Argument is a character, naming the register." t)

(autoload 'register-to-dot "register"
  "Move dot to location stored in a register.
Argument is a character, naming the register." t)

(autoload 'number-to-register "register"
  "Store a number in a register.
Two args, NUMBER and REGISTER (a character, naming the register).
If NUMBER is nil, digits in the buffer following dot are read
to get the number to store." t)

(autoload 'increment-register "register"
  "Add NUMBER to the contents of register REGISTER.
Interactively, NUMBER is the prefix arg (none means nil)." t)

(autoload 'view-register "register"
  "Display what is contained in register named REGISTER.
REGISTER is a character." t)

(autoload 'insert-register "register"
  "Insert contents of register REG.  REG is a character.
Normally puts dot before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and dot after." t)

(autoload 'copy-to-register "register"
  "Copy region into register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to copy." t)

(autoload 'append-to-register "register"
  "Append region to text in register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append." t)

(autoload 'prepend-to-register "register"
  "Prepend region to text in register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to prepend." t)

(autoload 'copy-rectangle-to-register "register"
  "Copy rectangular region into register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle." t)

(autoload 'backward-paragraph "paragraphs"
  "Move backward to start of paragraph.  With arg, do it arg times.
A paragraph start is the beginning of a line which is a first-line-of-paragraph
or which is ordinary text and follows a paragraph-separating line; except:
if the first real line of a paragraph is preceded by a blank line,
the paragraph starts at that blank line.
See forward-paragraph for more information." t)

(autoload 'forward-paragraph "paragraphs"
  "Move forward to end of paragraph.  With arg, do it arg times.
A line which  paragraph-start  matches either separates paragraphs
\(if  paragraph-separate  matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer." t)

(autoload 'mark-paragraph "paragraphs"
  "Put point at beginning of this paragraph, mark at end." t)

(autoload 'kill-paragraph "paragraph"
  "Kill to end of paragraph." t)

(autoload 'backward-kill-paragraph "paragraph"
  "Kill back to start of paragraph." t)

(autoload 'transpose-paragraphs "paragraph"
  "Interchange this (or next) paragraph with previous one." t)

(autoload 'forward-sentence "paragraphs"
  "Move forward to next sentence-end.  With argument, repeat.
With negative argument, move backward repeatedly to sentence-beginning.
Sentence ends are identified by the value of sentence-end
treated as a regular expression.  Also, every paragraph boundary
terminates sentences as well." t)

(autoload 'backward-sentence "paragraphs"
  "Move backward to start of sentence.  With arg, do it arg times.
See forward-sentence for more information." t)

(autoload 'kill-sentence "paragraphs"
  "Kill from dot to end of sentence.
With arg, repeat, or backward if negative arg." t)

(autoload 'backward-kill-sentence "paragraphs"
  "Kill back from dot to start of sentence.
With arg, repeat, or forward if negative arg." t)

(autoload 'mark-end-of-sentence "paragraph"
  "Put mark at end of sentence.  Arg works as in forward-sentence." t)

(autoload 'transpose-sentences "paragraph"
  "Interchange this (or next) sentence with previous one." t)

(autoload 'forward-page "page"
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp  page-delimiter." t)

(autoload 'backward-page "page"
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any line whose beginning matches the regexp  page-delimiter." t)

(autoload 'mark-page "page"
  "Put mark at end of page, dot at beginning." t)

(autoload 'count-lines-page "page"
  "Report number of lines on current page, and how many are before or after dot."
  t)

(autoload 'what-page
  "Print page and line number of dot." t)

(autoload 'abbrev-mode "abbrev"
  "Toggle abbrev mode.
With arg, turn abbrev mode on iff arg is positive.
In abbrev mode, inserting an abbreviation causes it to expand
and be replaced by its expansion."
  t)

(autoload 'kill-all-abbrevs "abbrev"
  "Undefine all defined abbrevs." t)

(autoload 'insert-abbrevs "abbrev"
  "Insert after dot a description of all defined abbrevs.
Mark is set after the inserted text." t)

(autoload 'list-abbrevs "abbrev"
  "Display a list of all defined abbrevs." t)

(autoload 'edit-abbrevs "abbrev"
  "Alter abbrev definitions by editing a list of them.
Selects a buffer containing a list of abbrev definitions.
You can edit them and type ^X^S to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted)." t)

(autoload 'define-abbrevs "abbrev"
  "Define abbrevs according to current visible buffer contents.
See documentation of edit-abbrevs for info on the format of the
text you must have in the buffer.
With argument, eliminate all abbrev definitions except
the ones defined from the buffer now." t)

(autoload 'read-abbrev-file "abbrev"
  "Read abbrev definitions from file written with write-abbrev-file."
  t)

(autoload 'write-abbrev-file "abbrev"
  "Write all abbrev definitions to file of Lisp code.
The file can be loaded to define the same abbrevs." t)

(autoload 'add-mode-abbrev "abbrev"
  "Define mode-specific abbrev for last word(s) before dot.
Argument is how many words before dot form the expansion;
or zero means the region is the expansion.
Reads the abbreviation in the minibuffer." t)

(autoload 'add-global-abbrev "abbrev"
  "Define global (all modes) abbrev for last word(s) before dot.
Argument is how many words before dot form the expansion;
or zero means the region is the expansion.
Reads the abbreviation in the minibuffer." t)

(autoload 'inverse-add-global-abbrev "abbrev"
  "Define last word before dot as a global (mode-independent) abbrev.
With argument N, defines the Nth word before dot.
Reads the expansion in the minibuffer.
Expands the abbreviation after defining it." t)

(autoload 'inverse-add-mode-abbrev "abbrev"
  "Define last word before dot as a mode-specific abbrev.
With argument N, defines the Nth word before dot.
Reads the expansion in the minibuffer.
Expands the abbreviation after defining it." t)

(autoload 'expand-region-abbrevs "abbrev"
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type y or n for each occurrence." t)
