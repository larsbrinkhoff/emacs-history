;;; cc-mode.el --- major mode for editing C, C++, and Objective-C code

;; Copyright (C) 1985, 87, 92, 93, 94, 95, 96 Free Software Foundation, Inc.

;; Authors: 1992-1996 Barry A. Warsaw
;;          1987 Dave Detlefs and Stewart Clamen
;;          1985 Richard M. Stallman
;; Created: a long, long, time ago. adapted from the original c-mode.el
;; Barry Warsaw Version:    4.282
;; Keywords: c languages oop

;; NOTE: Read the commentary below for the right way to submit bug reports!
;; NOTE: See the accompanying texinfo manual for details on using this mode!

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;;    This file may contain modifications made by the FSF for inclusion
;;;    in the Emacs release.  It may not be identical to the version
;;;    written by Barry Warsaw, so bugs are not necessarily his fault.
;;;    However, these changes are usually small, so we still do recommend
;;;    reporting bugs to him with C-c C-b.  Even when they are not his fault,
;;;    he may fix them for the FSF's sake.

;; This package provides modes in GNU Emacs for editing C, C++, 
;; Objective-C, and Java code. It is intended to be a replacement for
;; c-mode.el (a.k.a. BOCM -- Boring Old C-Mode), and c++-mode.el
;; (a.k.a cplus-md.el and cplus-md1.el), both of which are ancestors
;; of this file.  A number of important improvements have been made,
;; briefly: complete K&R C, ANSI C, `ARM' C++, Objective-C, and Java
;; support with consistent indentation across all modes, more
;; intuitive indentation controlling variables, compatibility across
;; all known Emacsen, nice new features, and tons of bug fixes.  This
;; package is called "cc-mode" to distinguish it from its ancestors,
;; but there really is no top-level cc-mode.  Usage and programming
;; details are contained in an accompanying texinfo manual.

;; To submit bug reports, type "C-c C-b".  These will be sent to
;; bug-gnu-emacs@prep.ai.mit.edu and I'll read about them there (this
;; is mirrored as the Usenet newsgroup gnu.emacs.bug).  Questions can
;; sent to help-gnu-emacs@prep.ai.mit.edu (mirrored as
;; gnu.emacs.help).  Please do not send bugs or questions to my
;; personal account.

;; YOU CAN IGNORE ALL BYTE-COMPILER WARNINGS. They are the result of
;; the multi-Emacsen support.  Emacs 19 (from the FSF), XEmacs 19
;; (formerly Lucid Emacs), and GNU Emacs 18 all do things differently
;; and there's no way to shut the byte-compiler up at the necessary
;; granularity.  Let me say this again: YOU CAN IGNORE ALL
;; BYTE-COMPILER WARNINGS (you'd be surprised at how many people don't
;; follow this advice :-).

;; If your Emacs is dumped with c-mode.el and/or c++-mode.el, you will
;; need to add the following to your .emacs file before any other
;; reference to c-mode or c++-mode:
;;
;; (fmakunbound 'c-mode)
;; (makunbound 'c-mode-map)
;; (fmakunbound 'c++-mode)
;; (makunbound 'c++-mode-map)
;; (makunbound 'c-style-alist)

;; If your Emacs comes with cc-mode already (and as of 18-Jan-1996,
;; XEmacs 19.13 and Emacs 19.30 both do), you only need to add the
;; following to use the latest version of cc-mode:
;;
;; (load "cc-mode")
;;
;; Make sure the new version is earlier on your load-path.

;; There are four major mode entry points provided by this package,
;; one for editing C++ code, one for editing C code (both K&R and
;; ANSI), one for editing Objective-C code, and one for editing Java
;; code.  The commands are M-x c-mode, M-x c++-mode, M-x objc-mode,
;; and M-x java-mode.

;; If you are using an old version of Emacs which does not come
;; with cc-mode.el, you will need to do these things
;; to use it:
;;
;; (autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
;; (autoload 'c-mode    "cc-mode" "C Editing Mode" t)
;; (autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)
;; (autoload 'java-mode "cc-mode" "Java Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.C$"    . c++-mode)
;;             ("\\.cc$"   . c++-mode)
;;             ("\\.c$"    . c-mode)
;;             ("\\.h$"    . c-mode)
;;             ("\\.m$"    . objc-mode)
;;             ("\\.java$" . java-mode)
;;            ) auto-mode-alist))
;;
;; You do not need these changes in Emacs versions that come with cc-mode.

;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, code contributions, and
;; encouragement cc-mode.el would be a far inferior package.

;; Anonymous ftp URL:
;;
;;    ftp://ftp.python.org/pub/emacs/cc-mode.tar.gz

;;; Code:


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar c-inhibit-startup-warnings-p nil
  "*If non-nil, inhibits start up compatibility warnings.")
(defvar c-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `c-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the syntactic symbol is ignored.")
(defvar c-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented.")
(defvar c-basic-offset 4
  "*Amount of basic offset used by + and - symbols in `c-offsets-alist'.")

(defvar c-offsets-alist
  '((string                . -1000)
    (c                     . c-lineup-C-comments)
    (defun-open            . 0)
    (defun-close           . 0)
    (defun-block-intro     . +)
    (class-open            . 0)
    (class-close           . 0)
    (inline-open           . +)
    (inline-close          . 0)
    (ansi-funcdecl-cont    . +)
    (knr-argdecl-intro     . +)
    (knr-argdecl           . 0)
    (topmost-intro         . 0)
    (topmost-intro-cont    . 0)
    (member-init-intro     . +)
    (member-init-cont      . 0)
    (inher-intro           . +)
    (inher-cont            . c-lineup-multi-inher)
    (block-open            . 0)
    (block-close           . 0)
    (brace-list-open       . 0)
    (brace-list-close      . 0)
    (brace-list-intro      . +)
    (brace-list-entry      . 0)
    (statement             . 0)
    ;; some people might prefer
    ;;(statement             . c-lineup-runin-statements)
    (statement-cont        . +)
    ;; some people might prefer
    ;;(statement-cont        . c-lineup-math)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (statement-case-open   . 0)
    (substatement          . +)
    (substatement-open     . +)
    (case-label            . 0)
    (access-label          . -)
    (label                 . 2)
    (do-while-closure      . 0)
    (else-clause           . 0)
    (comment-intro         . c-lineup-comment)
    (arglist-intro         . +)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-close         . +)
    (stream-op             . c-lineup-streamop)
    (inclass               . +)
    (cpp-macro             . -1000)
    (friend                . 0)
    (objc-method-intro     . -1000)
    (objc-method-args-cont . c-lineup-ObjC-method-args)
    (objc-method-call-cont . c-lineup-ObjC-method-call)
    )
  "*Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, cc-mode first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `c-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is called the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, cc-mode
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-SYMBOL's in `c-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `c-offsets-alist',
an error is generated if `c-strict-syntax-p' is non-nil, otherwise
the element is ignored.

Actually, OFFSET can be an integer, a function, a variable, or one of
the following symbols: `+', `-', `++', `--', `*', or `/'.  These
latter designate positive or negative multiples of `c-basic-offset',
respectively: *1, *-1, *2, *-2, *0.5, and *-0.5. If OFFSET is a
function, it is called with a single argument containing the cons of
the syntactic element symbol and the relative indent point.  The
function should return an integer offset.

Here is the current list of valid syntactic element symbols:

 string                 -- inside multi-line string
 c                      -- inside a multi-line C style block comment
 defun-open             -- brace that opens a function definition
 defun-close            -- brace that closes a function definition
 defun-block-intro      -- the first line in a top-level defun
 class-open             -- brace that opens a class definition
 class-close            -- brace that closes a class definition
 inline-open            -- brace that opens an in-class inline method
 inline-close           -- brace that closes an in-class inline method
 ansi-funcdecl-cont     -- the nether region between an ANSI function
                           declaration and the defun opening brace
 knr-argdecl-intro      -- first line of a K&R C argument declaration
 knr-argdecl            -- subsequent lines in a K&R C argument declaration
 topmost-intro          -- the first line in a topmost construct definition
 topmost-intro-cont     -- topmost definition continuation lines
 member-init-intro      -- first line in a member initialization list
 member-init-cont       -- subsequent member initialization list lines
 inher-intro            -- first line of a multiple inheritance list
 inher-cont             -- subsequent multiple inheritance lines
 block-open             -- statement block open brace
 block-close            -- statement block close brace
 brace-list-open        -- open brace of an enum or static array list
 brace-list-close       -- close brace of an enum or static array list
 brace-list-intro       -- first line in an enum or static array list
 brace-list-entry       -- subsequent lines in an enum or static array list
 statement              -- a C/C++/ObjC statement
 statement-cont         -- a continuation of a C/C++/ObjC statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case `block'
 statement-case-open    -- the first line in a case block starting with brace
 substatement           -- the first line after an if/while/for/do/else
 substatement-open      -- the brace that opens a substatement block
 case-label             -- a case or default label
 access-label           -- C++ private/protected/public access label
 label                  -- any non-special C/C++/ObjC label
 do-while-closure       -- the `while' that ends a do/while construct
 else-clause            -- the `else' of an if/else construct
 comment-intro          -- a line containing only a comment introduction
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as the
                           the arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 stream-op              -- lines continuing a stream operator construct
 inclass                -- the construct is nested inside a class definition
 cpp-macro              -- the start of a cpp macro
 friend                 -- a C++ friend declaration
 objc-method-intro      -- the first line of an Objective-C method definition
 objc-method-args-cont  -- lines continuing an Objective-C method definition
 objc-method-call-cont  -- lines continuing an Objective-C method call
")

(defvar c-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil,
hitting TAB indents the current line if point is at the left margin or
in the line's indentation, otherwise it insert a real tab character.
If other than nil or t, then tab is inserted only within literals
-- defined as comments and strings -- and inside preprocessor
directives, but line is always reindented.

Note that indentation of lines containing only comments is also
controlled by the `c-comment-only-line-offset' variable.")

(defvar c-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . -1000).")

(defvar c-indent-comments-syntactically-p nil
  "*Specifies how comment-only lines should be indented.
When this variable is non-nil, comment-only lines are indented
according to syntactic analysis via `c-offsets-alist', even when
\\[indent-for-comment] is used.")

(defvar c-block-comments-indent-p nil
  "*Specifies how to re-indent C style block comments.

Examples of the supported styles of C block comment indentation are
shown below.  When this variable is nil, block comments are indented
as shown in styles 1 through 4.  If this variable is non-nil, block
comments are indented as shown in style 5.

Note that cc-mode does not automatically insert any stars or block
comment delimiters.  You must type these in manually.  This variable
only controls how the lines within the block comment are indented when
you hit ``\\[c-indent-command]''.

 style 1:    style 2 (GNU):    style 3:     style 4:     style 5:
 /*          /* Blah           /*           /*           /*
    blah        blah.  */       * blah      ** blah      blah
    blah                        * blah      ** blah      blah
    */                          */          */           */")

(defvar c-cleanup-list '(scope-operator)
  "*List of various C/C++/ObjC constructs to \"clean up\".
These clean ups only take place when the auto-newline feature is turned
on, as evidenced by the `/a' or `/ah' appearing next to the mode name.
Valid symbols are:

 brace-else-brace    -- cleans up `} else {' constructs by placing entire
                        construct on a single line.  This clean up only
                        takes place when there is nothing but white
                        space between the braces and the `else'.  Clean
			up occurs when the open-brace after the `else'
			is typed.
 empty-defun-braces  -- cleans up empty defun braces by placing the
                        braces on the same line.  Clean up occurs when
			the defun closing brace is typed.
 defun-close-semi    -- cleans up the terminating semi-colon on defuns
			by placing the semi-colon on the same line as
			the closing brace.  Clean up occurs when the
			semi-colon is typed.
 list-close-comma    -- cleans up commas following braces in array
                        and aggregate initializers.  Clean up occurs
			when the comma is typed.
 scope-operator      -- cleans up double colons which may designate
			a C++ scope operator split across multiple
			lines. Note that certain C++ constructs can
			generate ambiguous situations.  This clean up
			only takes place when there is nothing but
			whitespace between colons. Clean up occurs
			when the second colon is typed.")

(defvar c-hanging-braces-alist '((brace-list-open)
				 (substatement-open after)
				 (block-close . c-snug-do-while))
  "*Controls the insertion of newlines before and after braces.
This variable contains an association list with elements of the
following form: (SYNTACTIC-SYMBOL . ACTION).

When a brace (either opening or closing) is inserted, the syntactic
context it defines is looked up in this list, and if found, the
associated ACTION is used to determine where newlines are inserted.
If the context is not found, the default is to insert a newline both
before and after the brace.

SYNTACTIC-SYMBOL can be any of: defun-open, defun-close, class-open,
class-close, inline-open, inline-close, block-open, block-close,
substatement-open, statement-case-open, brace-list-open,
brace-list-close, brace-list-intro, or brace-list-entry. See
`c-offsets-alist' for details.

ACTION can be either a function symbol or a list containing any
combination of the symbols `before' or `after'.  If the list is empty,
no newlines are inserted either before or after the brace.

When ACTION is a function symbol, the function is called with a two
arguments: the syntactic symbol for the brace and the buffer position
at which the brace was inserted.  The function must return a list as
described in the preceding paragraph.  Note that during the call to
the function, the variable `c-syntactic-context' is set to the entire
syntactic context for the brace line.")

(defvar c-hanging-colons-alist nil
  "*Controls the insertion of newlines before and after certain colons.
This variable contains an association list with elements of the
following form: (SYNTACTIC-SYMBOL . ACTION).

See the variable `c-hanging-braces-alist' for the semantics of this
variable.  Note however that making ACTION a function symbol is
currently not supported for this variable.")

(defvar c-hanging-semi&comma-criteria '(c-semi&comma-inside-parenlist)
  "*List of functions that decide whether to insert a newline or not.
The functions in this list are called, in order, whenever the
auto-newline minor mode is activated (as evidenced by a `/a' or `/ah'
string in the mode line), and a semicolon or comma is typed (see
`c-electric-semi&comma').  Each function in this list is called with
no arguments, and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not insert a newline, and stop checking
  (anything else) -- insert a newline, and stop checking

If every function in the list is called with no determination made,
then no newline is inserted.")

(defvar c-hanging-comment-ender-p t
  "*If nil, `c-fill-paragraph' leaves C block comment enders on their own line.
Default value is t, which inhibits leaving block comment ending string
`*/' on a line by itself.  This is BOCM's sole behavior.")

(defvar c-backslash-column 48
  "*Column to insert backslashes when macroizing a region.")
(defvar c-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode.")
(defvar c-delete-function 'backward-delete-char-untabify
  "*Function called by `c-electric-delete' when deleting characters.")
(defvar c-electric-pound-behavior nil
  "*List of behaviors for electric pound insertion.
Only currently supported behavior is `alignleft'.")

(defvar c-recognize-knr-p t
  "*If non-nil, `c-mode' and `objc-mode' will recognize K&R constructs.
This variable is needed because of ambiguities in C syntax that make
fast recognition of K&R constructs problematic, and slow.  If you are
coding with ANSI prototypes, set this variable to nil to speed up
recognition of certain constructs.  By setting this variable to nil, I
have seen an increase of 20 times under some circumstance.")

(defvar c-progress-interval 5
  "*Interval used to update progress status during long re-indentation.
If a number, percentage complete gets updated after each interval of
that many seconds.   Set to nil to inhibit updating.  This is only
useful for Emacs 19.")

(defvar c-style-alist
  '(("gnu"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (arglist-intro . c-lineup-arglist-intro-after-paren)
			 (arglist-close . c-lineup-arglist)
			 ))
     )
    ("k&r"
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("bsd"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("stroustrup"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("whitesmith"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))

     )
    ("ellemtel"
     (c-basic-offset . 3)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist     . ((substatement-open before after)))
     (c-offsets-alist . ((topmost-intro        . 0)
                         (topmost-intro-cont   . 0)
                         (substatement         . 3)
			 (substatement-open    . 0)
			 (statement-case-intro . 0)
                         (case-label           . +)
                         (access-label         . -3)
                         (inclass              . 6)
                         (inline-open          . 0)
                         ))
     )
    ("java"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
 			 (knr-argdecl-intro     . 5)
 			 (substatement-open     . +)
 			 (label                 . 0)
 			 (statement-case-open   . +)
 			 (statement-cont        . +)
 			 (arglist-intro . c-lineup-arglist-intro-after-paren)
 			 (arglist-close . c-lineup-arglist)
 			 (access-label  . 0)
			 ))

     )
    )
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any cc-mode variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `c-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-SYMBOL . VALUE)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.

Note that all styles inherit from the `cc-mode' style, which is
computed at the time the mode is loaded.")

(defvar c-file-style nil
  "*Variable interface for setting style via File Local Variables.
In a file's Local Variable section, you can set this variable to a
string suitable for `c-set-style'.  When the file is visited, cc-mode
will set the style of the file to this value automatically.

Note that file style settings are applied before file offset settings
as designated in the variable `c-file-offsets'.")

(defvar c-file-offsets nil
  "*Variable interface for setting offsets via File Local Variables.
In a file's Local Variable section, you can set this variable to an
association list similar to the values allowed in `c-offsets-alist'.
When the file is visited, cc-mode will institute these offset settings
automatically.

Note that file offset settings are applied after file style settings
as designated in the variable `c-file-style'.")

(defvar c-site-default-style "gnu"
  "Default style for your site.
To change the default style at your site, you can set this variable to
any style defined in `c-style-alist'.  However, if cc-mode is usually
loaded into your Emacs at compile time, you will need to set this
variable in the `site-init.el' file before cc-mode is loaded, then
re-dump Emacs.")

(defvar c-mode-hook nil
  "*Hook called by `c-mode'.")
(defvar c++-mode-hook nil
  "*Hook called by `c++-mode'.")
(defvar objc-mode-hook nil
  "*Hook called by `objc-mode'.")
(defvar java-mode-hook nil
  "*Hook called by `java-mode'.")

(defvar c-mode-common-hook nil
  "*Hook called by `c-mode', `c++-mode', and 'objc-mode' during common init.")

(defvar c-mode-menu
  '(["Comment Out Region"     comment-region (mark)]
    ["Macro Expand Region"    c-macro-expand (mark)]
    ["Backslashify"           c-backslash-region (mark)]
    ["Indent Expression"      c-indent-exp
     (memq (following-char) '(?\( ?\[ ?\{))]
    ["Indent Line"            c-indent-command t]
    ["Fill Comment Paragraph" c-fill-paragraph t]
    ["Up Conditional"         c-up-conditional t]
    ["Backward Conditional"   c-backward-conditional t]
    ["Forward Conditional"    c-forward-conditional t]
    ["Backward Statement"     c-beginning-of-statement t]
    ["Forward Statement"      c-end-of-statement t]
    )
  "XEmacs 19 menu for C/C++/ObjC modes.")

;; Sadly we need this for a macro in Emacs 19.
(eval-when-compile
  ;; Imenu isn't used in XEmacs, so just ignore load errors.
  (condition-case ()
      (require 'imenu)
    (error nil)))

(defvar cc-imenu-c++-generic-expression
  (` 
   ((nil
     (, 
      (concat
       "^"				; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	; more than 3 tokens, right?
        
       "\\("				; last type spec including */&
       "[a-zA-Z0-9_:]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
       "\\)?"				; if there is a last type spec
       "\\("				; name; take that into the imenu entry
       "[a-zA-Z0-9_:~]+"		; member function, ctor or dtor...
 					; (may not contain * because then 
 					; "a::operator char*" would become "char*"!)
       "\\|"
       "\\([a-zA-Z0-9_:~]*::\\)?operator"
       "[^a-zA-Z1-9_][^(]*"		; ...or operator
       " \\)"
       "[ \t]*([^)]*)[ \t\n]*[^		;]" ; require something other than a ; after
 					; the (...) to avoid prototypes.  Can't
 					; catch cases with () inside the parentheses
 					; surrounding the parameters
 					; (like "int foo(int a=bar()) {...}"
        
       )) 6)    
    ("Class" 
     (, (concat 
 	 "^"				; beginning of line is required
 	 "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
 	 "class[ \t]+"
 	 "\\([a-zA-Z0-9_]+\\)"		; this is the string we want to get
 	 "[ \t]*[:{]"
 	 )) 2)))
  "Imenu generic expression for C++ mode.  See `imenu-generic-expression'.")
 
(defvar cc-imenu-c-generic-expression
  cc-imenu-c++-generic-expression
  "Imenu generic expression for C mode.  See `imenu-generic-expression'.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

;; Shut the byte-compiler up. Requires Emacs 19 or JWZ's improved
;; byte-compiler. Otherwise, comment this line out and ignore
;; any warnings.
;;(byte-compiler-options (warnings nil))

;; figure out what features this Emacs has
(defconst c-emacs-features
  (let ((major (and (boundp 'emacs-major-version)
		    emacs-major-version))
	(minor (and (boundp 'emacs-minor-version)
		    emacs-minor-version))
	(re-suite 'old-re)
	flavor comments)
    ;; figure out version numbers if not already discovered
    (and (or (not major) (not minor))
	 (string-match "\\([0-9]+\\).\\([0-9]+\\)" emacs-version)
	 (setq major (string-to-int (substring emacs-version
					       (match-beginning 1)
					       (match-end 1)))
	       minor (string-to-int (substring emacs-version
					       (match-beginning 2)
					       (match-end 2)))))
    (if (not (and major minor))
	(error "Cannot figure out the major and minor version numbers."))
    ;; calculate the major version
    (cond
     ((= major 18) (setq major 'v18))	;Emacs 18
     ((= major 4)  (setq major 'v18))	;Epoch 4
     ((= major 19) (setq major 'v19	;Emacs 19
			 flavor (if (or (string-match "Lucid" emacs-version)
					(string-match "XEmacs" emacs-version))
				    'XEmacs 'FSF)))
     ;; I don't know
     (t (error "Cannot recognize major version number: %s" major)))
    ;; Regular expression suites...
    (if (and (eq major 'v19)
	     (or (and (eq flavor 'XEmacs) (>= minor 14))
		 (and (eq flavor 'FSF) (>= minor 30))))
	(setq re-suite 'new-re))
    ;; XEmacs 19 uses 8-bit modify-syntax-entry flags, as do all
    ;; patched Emacs 19, Emacs 18, Epoch 4's.  Only Emacs 19 uses a
    ;; 1-bit flag.  Let's be as smart as we can about figuring this
    ;; out.
    (if (eq major 'v19)
	(let ((table (copy-syntax-table)))
	  (modify-syntax-entry ?a ". 12345678" table)
	  (cond
	   ;; XEmacs pre 20 and Emacs pre 19.30 use vectors for syntax tables.
	   ((vectorp table)
	    (if (= (logand (lsh (aref table ?a) -16) 255) 255)
		(setq comments '8-bit)
	      (setq comments '1-bit)))
	   ;; XEmacs 20 is known to be 8-bit
	   ((eq flavor 'XEmacs) (setq comments '8-bit))
	   ;; Emacs 19.30 and beyond are known to be 1-bit
	   ((eq flavor 'FSF) (setq comments '1-bit))
	   ;; Don't know what this is
	   (t (error "Couldn't figure out syntax table format."))
	   ))
      ;; Emacs 18 has no support for dual comments
      (setq comments 'no-dual-comments))
    ;; lets do some minimal sanity checking.
    (if (and (or
	      ;; Lucid Emacs before 19.6 had bugs
	      (and (eq major 'v19) (eq flavor 'XEmacs) (< minor 6))
	      ;; Emacs 19 before 19.21 has known bugs
	      (and (eq major 'v19) (eq flavor 'FSF) (< minor 21)))
	     (not c-inhibit-startup-warnings-p))
	(with-output-to-temp-buffer "*cc-mode warnings*"
	  (print (format
"The version of Emacs that you are running, %s,
has known bugs in its syntax.c parsing routines which will affect the
performance of cc-mode. You should strongly consider upgrading to the
latest available version.  cc-mode may continue to work, after a
fashion, but strange indentation errors could be encountered."
		     emacs-version))))
    ;; Emacs 18, with no patch is not too good
    (if (and (eq major 'v18) (eq comments 'no-dual-comments)
	     (not c-inhibit-startup-warnings-p))
	(with-output-to-temp-buffer "*cc-mode warnings*"
	  (print (format
"The version of Emacs 18 you are running, %s,
has known deficiencies in its ability to handle dual C++ comments,
i.e. C++ line style comments and C block style comments.  This will
not be much of a problem for you if you are only editing C code, but
if you are doing much C++ editing, you should strongly consider
upgrading to one of the latest Emacs 19's.  In Emacs 18, you may also
experience performance degradations. Emacs 19 has some new built-in
routines which will speed things up for you.

Because of these inherent problems, cc-mode is no longer being
actively maintained for Emacs 18, however, until you can upgrade to
Emacs 19, you may want to look at cc-mode-18.el in the cc-mode
distribution.  THIS FILE IS COMPLETELY UNSUPPORTED!  If you use it,
you are on your own, although patch contributions will be folded into
the main release."
			    emacs-version))))
    ;; Emacs 18 with the syntax patches are no longer supported
    (if (and (eq major 'v18) (not (eq comments 'no-dual-comments))
	     (not c-inhibit-startup-warnings-p))
	(with-output-to-temp-buffer "*cc-mode warnings*"
	  (print (format
"You are running a syntax patched Emacs 18 variant.  While this should
work for you, you may want to consider upgrading to Emacs 19.  The
syntax patches are no longer supported either for syntax.c or
cc-mode."))))
    (list major comments re-suite))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by cc-mode.  Here's the current
supported list, along with the values for this variable:

 Emacs 18/Epoch 4:           (v18 no-dual-comments RS)
 Emacs 18/Epoch 4 (patch2):  (v18 8-bit RS)
 XEmacs 19:                  (v19 8-bit RS)
 Emacs 19:                   (v19 1-bit RS)

RS is the regular expression suite to use.  XEmacs versions after
19.13, and Emacs versions after 19.29 use the `new-re' regex suite.
All other Emacsen use the `old-re' suite.")

(defvar c++-mode-abbrev-table nil
  "Abbrev table in use in c++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

(defvar c-mode-abbrev-table nil
  "Abbrev table in use in c-mode buffers.")
(define-abbrev-table 'c-mode-abbrev-table ())

(defvar objc-mode-abbrev-table nil
  "Abbrev table in use in objc-mode buffers.")
(define-abbrev-table 'objc-mode-abbrev-table ())

(defvar java-mode-abbrev-table nil
  "Abbrev table in use in java-mode buffers.")
(define-abbrev-table 'java-mode-abbrev-table ())

(defun c-mode-fsf-menu (name map)
  ;; Add menu to a keymap.  FSF menus suck.  Don't add them for
  ;; XEmacs. This feature test will fail on other than Emacs 19.
  (condition-case nil
      (progn
	(define-key map [menu-bar] (make-sparse-keymap))
	(define-key map [menu-bar c] (cons name (make-sparse-keymap name)))

	(define-key map [menu-bar c comment-region]
	  '("Comment Out Region" . comment-region))
	(define-key map [menu-bar c c-macro-expand]
	  '("Macro Expand Region" . c-macro-expand))
	(define-key map [menu-bar c c-backslash-region]
	  '("Backslashify" . c-backslash-region))
	(define-key map [menu-bar c indent-exp]
	  '("Indent Expression" . c-indent-exp))
	(define-key map [menu-bar c indent-line]
	  '("Indent Line" . c-indent-command))
	(define-key map [menu-bar c fill]
	  '("Fill Comment Paragraph" . c-fill-paragraph))
	(define-key map [menu-bar c up]
	  '("Up Conditional" . c-up-conditional))
	(define-key map [menu-bar c backward]
	  '("Backward Conditional" . c-backward-conditional))
	(define-key map [menu-bar c forward]
	  '("Forward Conditional" . c-forward-conditional))
	(define-key map [menu-bar c backward-stmt]
	  '("Backward Statement" . c-beginning-of-statement))
	(define-key map [menu-bar c forward-stmt]
	  '("Forward Statement" . c-end-of-statement))

	;; RMS: mouse-3 should not select this menu.  mouse-3's global
	;; definition is useful in C mode and we should not interfere
	;; with that.  The menu is mainly for beginners, and for them,
	;; the menubar requires less memory than a special click.
	t)
    (error nil)))

(defvar c-mode-map ()
  "Keymap used in c-mode buffers.")
(if c-mode-map
    ()
  ;; TBD: should we even worry about naming this keymap. My vote: no,
  ;; because Emacs and XEmacs do it differently.
  (setq c-mode-map (make-sparse-keymap))
  ;; put standard keybindings into MAP
  ;; the following mappings correspond more or less directly to BOCM
  (define-key c-mode-map "{"         'c-electric-brace)
  (define-key c-mode-map "}"         'c-electric-brace)
  (define-key c-mode-map ";"         'c-electric-semi&comma)
  (define-key c-mode-map "#"         'c-electric-pound)
  (define-key c-mode-map ":"         'c-electric-colon)
  ;; Lucid Emacs 19.9 defined these two, the second of which was
  ;; commented out...
  ;; (define-key c-mode-map "\e{" 'c-insert-braces)
  ;; Commented out electric square brackets because nobody likes them.
  ;; (define-key c-mode-map "[" 'c-insert-brackets)
  (define-key c-mode-map "\e\C-h"    'c-mark-function)
  (define-key c-mode-map "\e\C-q"    'c-indent-exp)
  (define-key c-mode-map "\ea"       'c-beginning-of-statement)
  (define-key c-mode-map "\ee"       'c-end-of-statement)
  ;; Emacs 19.30 introduces fill-paragraph-function, but it's not in
  ;; every version of Emacs cc-mode supports.
  (if (not (boundp 'fill-paragraph-function))
      ;; I'd rather use an adaptive fill program instead of this.
      (define-key c-mode-map "\eq"   'c-fill-paragraph))
  (define-key c-mode-map "\C-c\C-n"  'c-forward-conditional)
  (define-key c-mode-map "\C-c\C-p"  'c-backward-conditional)
  (define-key c-mode-map "\C-c\C-u"  'c-up-conditional)
  (define-key c-mode-map "\t"        'c-indent-command)
  (define-key c-mode-map "\177"      'c-electric-delete)
  ;; these are new keybindings, with no counterpart to BOCM
  (define-key c-mode-map ","         'c-electric-semi&comma)
  (define-key c-mode-map "*"         'c-electric-star)
  (define-key c-mode-map "\C-c\C-q"  'c-indent-defun)
  (define-key c-mode-map "\C-c\C-\\" 'c-backslash-region)
  ;; TBD: where if anywhere, to put c-backward|forward-into-nomenclature
  (define-key c-mode-map "\C-c\C-a"  'c-toggle-auto-state)
  (define-key c-mode-map "\C-c\C-b"  'c-submit-bug-report)
  (define-key c-mode-map "\C-c\C-c"  'comment-region)
  (define-key c-mode-map "\C-c\C-d"  'c-toggle-hungry-state)
  (define-key c-mode-map "\C-c\C-e"  'c-macro-expand)
  (define-key c-mode-map "\C-c\C-o"  'c-set-offset)
  (define-key c-mode-map "\C-c\C-s"  'c-show-syntactic-information)
  (define-key c-mode-map "\C-c\C-t"  'c-toggle-auto-hungry-state)
  ;; conflicts with OOBR
  ;;(define-key c-mode-map "\C-c\C-v"  'c-version)
  ;;
  ;; Emacs 19 defines menus in the mode map. This call will return
  ;; t on Emacs 19, otherwise no-op and return nil.
  (if (and (not (c-mode-fsf-menu "C" c-mode-map))
	   ;; in XEmacs 19, we want the menu to popup when the 3rd
	   ;; button is hit.  In Lucid Emacs 19.10 and beyond this is
	   ;; done automatically if we put the menu on mode-popup-menu
	   ;; variable, see c-common-init. Emacs 19 uses C-Mouse-3 for
	   ;; this, and it works with no special effort.
	   (boundp 'current-menubar)
	   (not (boundp 'mode-popup-menu)))
      (define-key c-mode-map 'button3 'c-popup-menu)))

(defvar c++-mode-map ()
  "Keymap used in c++-mode buffers.")
(if c++-mode-map
    ()
  ;; In Emacs 19, it makes more sense to inherit c-mode-map
  (if (memq 'v19 c-emacs-features)
      ;; XEmacs and Emacs 19 do this differently
      (cond
       ;; XEmacs 19.13
       ((fboundp 'set-keymap-parents)
	(setq c++-mode-map (make-sparse-keymap))
	(set-keymap-parents c++-mode-map c-mode-map))
       ((fboundp 'set-keymap-parent)
	(setq c++-mode-map (make-sparse-keymap))
	(set-keymap-parent c++-mode-map c-mode-map))
       (t (setq c++-mode-map (cons 'keymap c-mode-map))))
    ;; Do it the hard way for Emacs 18 -- given by JWZ
    (setq c++-mode-map (nconc (make-sparse-keymap) c-mode-map)))
  ;; add bindings which are only useful for C++
  (define-key c++-mode-map "\C-c:"  'c-scope-operator)
  (define-key c++-mode-map "/"      'c-electric-slash)
  (define-key c++-mode-map "<"      'c-electric-lt-gt)
  (define-key c++-mode-map ">"      'c-electric-lt-gt)
  ;; Emacs 19 defines menus in the mode map. This call will return
  ;; t on Emacs 19, otherwise no-op and return nil.
  (c-mode-fsf-menu "C++" c++-mode-map))

(defvar objc-mode-map ()
  "Keymap used in objc-mode buffers.")
(if objc-mode-map
    ()
  ;; In Emacs 19, it makes more sense to inherit c-mode-map
  (if (memq 'v19 c-emacs-features)
      ;; XEmacs and Emacs 19 do this differently
      (cond
       ;; XEmacs 19.13
       ((fboundp 'set-keymap-parents)
	(setq objc-mode-map (make-sparse-keymap))
	(set-keymap-parents objc-mode-map c-mode-map))
       ((fboundp 'set-keymap-parent)
	(setq objc-mode-map (make-sparse-keymap))
	(set-keymap-parent objc-mode-map c-mode-map))
       (t (setq objc-mode-map (cons 'keymap c-mode-map))))
    ;; Do it the hard way for Emacs 18 -- given by JWZ
    (setq objc-mode-map (nconc (make-sparse-keymap) c-mode-map)))
  ;; add bindings which are only useful for Objective-C
  (define-key objc-mode-map "/"      'c-electric-slash)
  ;; Emacs 19 defines menus in the mode map. This call will return
  ;; t on Emacs 19, otherwise no-op and return nil.
  (c-mode-fsf-menu "ObjC" objc-mode-map))

(defvar java-mode-map ()
  "Keymap used in java-mode buffers.")
(if java-mode-map
    ()
  ;; In Emacs 19, it makes more sense to inherit c-mode-map
  (if (memq 'v19 c-emacs-features)
      ;; XEmacs and Emacs 19 do this differently
      (cond
       ;; XEmacs 19.13
       ((fboundp 'set-keymap-parents)
	(setq java-mode-map (make-sparse-keymap))
	(set-keymap-parents java-mode-map c-mode-map))
       ((fboundp 'set-keymap-parent)
	(setq java-mode-map (make-sparse-keymap))
	(set-keymap-parent java-mode-map c-mode-map))
       (t (setq java-mode-map (cons 'keymap c-mode-map)))
       )
    ;; Do it the hard way for Emacs 18 -- given by JWZ
    (setq java-mode-map (nconc (make-sparse-keymap) c-mode-map)))
  ;; add bindings which are only useful for Java
  (define-key java-mode-map "/"      'c-electric-slash)
  ;; Emacs 19 defines menus in the mode map. This call will return t
  ;; on Emacs 19, otherwise no-op and return nil.
  (c-mode-fsf-menu "Java" java-mode-map))

(defun c-populate-syntax-table (table)
  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?_  "_"     table)
  (modify-syntax-entry ?\\ "\\"    table)
  (modify-syntax-entry ?+  "."     table)
  (modify-syntax-entry ?-  "."     table)
  (modify-syntax-entry ?=  "."     table)
  (modify-syntax-entry ?%  "."     table)
  (modify-syntax-entry ?<  "."     table)
  (modify-syntax-entry ?>  "."     table)
  (modify-syntax-entry ?&  "."     table)
  (modify-syntax-entry ?|  "."     table)
  (modify-syntax-entry ?\' "\""    table))

(defun c-setup-dual-comments (table)
  ;; Set up TABLE to handle block and line style comments
  (cond
   ((memq '8-bit c-emacs-features)
    ;; XEmacs 19 has the best implementation
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"    table))
   ((memq '1-bit c-emacs-features)
    ;; Emacs 19 does things differently, but we can work with it
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"   table))
   ))

(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table)
  ;; add extra comment syntax
  (modify-syntax-entry ?/  ". 14"  c-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"  c-mode-syntax-table))

(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c++-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments c++-mode-syntax-table)
  ;; TBD: does it make sense for colon to be symbol class in C++?
  ;; I'm not so sure, since c-label-key is busted on lines like:
  ;; Foo::bar( i );
  ;; maybe c-label-key should be fixed instead of commenting this out,
  ;; but it also bothers me that this only seems appropriate for C++
  ;; and not C.
  ;;(modify-syntax-entry ?: "_" c++-mode-syntax-table)
  )

(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(if objc-mode-syntax-table
    ()
  (setq objc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table objc-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments objc-mode-syntax-table)
  ;; everyone gets these
  (modify-syntax-entry ?@ "_" objc-mode-syntax-table)
  )

(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(if java-mode-syntax-table
    ()
  (setq java-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table java-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments java-mode-syntax-table)
  ;; everyone gets these
  (modify-syntax-entry ?@ "_" java-mode-syntax-table)
  )

(defvar c-hungry-delete-key nil
  "Internal state of hungry delete key feature.")
(defvar c-auto-newline nil
  "Internal state of auto newline feature.")
(defvar c-auto-hungry-string nil
  "Internal auto-newline/hungry-delete designation string for mode line.")
(defvar c-syntactic-context nil
  "Variable containing syntactic analysis list during indentation.")
(defvar c-comment-start-regexp nil
  "Buffer local variable describing how comment are introduced.")
(defvar c-conditional-key nil
  "Buffer local language-specific conditional keyword regexp.")
(defvar c-access-key nil
  "Buffer local language-specific access key regexp.")
(defvar c-class-key nil
  "Buffer local language-specific class key regexp.")
(defvar c-method-key nil
  "Buffer local language-specific method regexp.")
(defvar c-double-slash-is-comments-p nil
  "Buffer local language-specific comment style flag.")
(defconst c-protection-key
  "\\<\\(public\\|protected\\|private\\)\\>"
  "Regexp describing protection keywords.")
(defconst c-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a C/C++/ObjC symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")
(defconst c-baseclass-key
  (concat
   ":?[ \t]*\\(virtual[ \t]+\\)?\\("
   c-protection-key "[ \t]+\\)" c-symbol-key)
  "Regexp describing C++ base classes in a derived class definition.")

;; minor mode variables
(make-variable-buffer-local 'c-auto-newline)
(make-variable-buffer-local 'c-hungry-delete-key)
(make-variable-buffer-local 'c-auto-hungry-string)
;; language differences
(make-variable-buffer-local 'c-comment-start-regexp)
(make-variable-buffer-local 'c-conditional-key)
(make-variable-buffer-local 'c-access-key)
(make-variable-buffer-local 'c-class-key)
(make-variable-buffer-local 'c-method-key)
(make-variable-buffer-local 'c-double-slash-is-comments-p)
(make-variable-buffer-local 'c-baseclass-key)
(make-variable-buffer-local 'c-recognize-knr-p)
;; style variables are made buffer local at tail end of this file.

;; cmacexp is lame because it uses no preprocessor symbols.
;; It isn't very extensible either -- hardcodes /lib/cpp.
;; [I add it here only because c-mode has it -- BAW]
(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)


;; constant regular expressions for looking at various constructs
(defconst c-C++-class-key "\\(class\\|struct\\|union\\)"
  "Regexp describing a C++ class declaration, including templates.")
(defconst c-C-class-key "\\(struct\\|union\\)"
  "Regexp describing a C struct declaration.")
(defconst c-inher-key
  (concat "\\(\\<static\\>\\s +\\)?"
	  c-C++-class-key "[ \t]+" c-symbol-key
	  "\\([ \t]*:[ \t]*\\)?\\s *[^;]")
  "Regexp describing a class inheritance declaration.")
(defconst c-switch-label-key
  "\\(\\(case[( \t]+\\S .*\\)\\|default[ \t]*\\):"
  "Regexp describing a switch's case or default label")
(defconst c-C++-access-key
  (concat c-protection-key ":")
  "Regexp describing C++ access specification keywords.")
(defconst c-label-key
  (concat c-symbol-key ":\\([^:]\\|$\\)")
  "Regexp describing any label.")
(defconst c-C-conditional-key
  "\\b\\(for\\|if\\|do\\|else\\|while\\|switch\\)\\b[^_]"
  "Regexp describing a conditional control.")
(defconst c-C++-conditional-key
  "\\b\\(for\\|if\\|do\\|else\\|while\\|switch\\|try\\|catch\\)\\b[^_]"
  "Regexp describing a conditional control for C++.")
(defconst c-C++-friend-key
  "friend[ \t]+\\|template[ \t]*<.+>[ \t]*friend[ \t]+"
  "Regexp describing friend declarations in C++ classes.")
(defconst c-C++-comment-start-regexp "//\\|/\\*"
  "Dual comment value for `c-comment-start-regexp'.")
(defconst c-C-comment-start-regexp "/\\*"
  "Single comment style value for `c-comment-start-regexp'.")

(defconst c-ObjC-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in objc syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-symbol-key)
  "Regexp describing an Objective-C method intro.")
(defconst c-ObjC-access-key
  (concat "@" c-protection-key)
  "Regexp describing access specification keywords for Objective-C.")
(defconst c-ObjC-class-key
  (concat
   "@\\(interface\\|implementation\\)\\s +"
   c-symbol-key				;name of the class
   "\\(\\s *:\\s *" c-symbol-key "\\)?"	;maybe followed by the superclass
   "\\(\\s *<[^>]+>\\)?"		;and maybe the adopted protocols list
   )
  "Regexp describing a class or protocol declaration for Objective-C.")

(defconst c-Java-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in java syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-symbol-key)
  "Regexp describing a Java method intro.")
(defconst c-Java-access-key
  (concat c-protection-key)
  "Regexp describing access specification keywords for Java.")
(defconst c-Java-class-key
  (concat
   "\\(interface\\|class\\)\\s +"
   c-symbol-key				;name of the class
   "\\(\\s *extends\\s *" c-symbol-key "\\)?" ;maybe followed by superclass 
   ;;"\\(\\s *implements *[^{]+{\\)?"	;and maybe the adopted protocols list
   )
  "Regexp describing a class or protocol declaration for Java.")

;; KLUDGE ALERT.  We default these variables to their `C' values so
;; that non-cc-mode-ized modes that depend on c-mode will still work
;; out of the box.  The most glaring example is awk-mode.  There ought
;; to be a better way.
(setq-default c-conditional-key c-C-conditional-key
	      c-class-key c-C-class-key
	      c-comment-start-regexp c-C-comment-start-regexp)


;; main entry points for the modes
(defconst c-list-of-mode-names nil)

;;;###autoload
(defun c-mode ()
  "Major mode for editing K&R and ANSI C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

To see what version of cc-mode you are running, enter `\\[c-version]'.

The hook variable `c-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the hook `c-mode-common-hook' is
run first.

Key bindings:
\\{c-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'c-mode
	mode-name "C"
	local-abbrev-table c-mode-abbrev-table)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq comment-start "/* "
	comment-end   " */"
	comment-multi-line t
	c-conditional-key c-C-conditional-key
	c-class-key c-C-class-key
	c-baseclass-key nil
	c-comment-start-regexp c-C-comment-start-regexp
	imenu-generic-expression cc-imenu-c-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c-mode-hook))
(setq c-list-of-mode-names (cons "C" c-list-of-mode-names))

;;;###autoload
(defun c++-mode ()
  "Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of cc-mode you are running, enter `\\[c-version]'.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{c++-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table)
  (use-local-map c++-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end ""
	comment-multi-line nil
	c-conditional-key c-C++-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
	c-class-key c-C++-class-key
	c-access-key c-C++-access-key
	c-double-slash-is-comments-p t
	imenu-generic-expression cc-imenu-c++-generic-expression)
  (make-local-variable 'c-recognize-knr-p)
  (setq c-recognize-knr-p nil)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c++-mode-hook))
(setq c-list-of-mode-names (cons "C++" c-list-of-mode-names))

;;;###autoload
(defun objc-mode ()
  "Major mode for editing Objective C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
objc-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of cc-mode you are running, enter `\\[c-version]'.

The hook variable `objc-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the hook `c-mode-common-hook'
is run first.

Key bindings:
\\{objc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table objc-mode-syntax-table)
  (setq major-mode 'objc-mode
	mode-name "ObjC"
	local-abbrev-table objc-mode-abbrev-table)
  (use-local-map objc-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end   ""
	comment-multi-line nil
	c-conditional-key c-C-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
 	c-class-key c-ObjC-class-key
	c-baseclass-key nil
	c-access-key c-ObjC-access-key
	c-double-slash-is-comments-p t
	c-method-key c-ObjC-method-key)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'objc-mode-hook))
(setq c-list-of-mode-names (cons "ObjC" c-list-of-mode-names))

;;;###autoload
(defun java-mode ()
  "Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
java-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case and send the
message.

To see what version of cc-mode you are running, enter `\\[c-version]'.

The hook variable `java-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.

Key bindings:
\\{java-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table java-mode-syntax-table)
  (setq major-mode 'java-mode
 	mode-name "Java"
 	local-abbrev-table java-mode-abbrev-table)
  (use-local-map java-mode-map)
  (c-common-init)
  (setq comment-start "// "
 	comment-end   ""
 	comment-multi-line nil
 	c-conditional-key c-C-conditional-key
 	c-comment-start-regexp c-C++-comment-start-regexp
  	c-class-key c-Java-class-key
	c-method-key c-Java-method-key
	c-double-slash-is-comments-p t
 	c-baseclass-key nil
 	c-access-key c-Java-access-key)
  (c-set-style "Java")
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'java-mode-hook))
(setq c-list-of-mode-names (cons "Java" c-list-of-mode-names))

(defun c-common-init ()
  ;; Common initializations for c++-mode and c-mode.
  ;; make local variables
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'imenu-generic-expression) ;set in the mode functions
  ;; Emacs 19.30 and beyond only, AFAIK
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'c-fill-paragraph)))
  ;; now set their values
  (setq paragraph-start (if (memq 'new-re c-emacs-features)
			    (concat page-delimiter "\\|$")
			  (concat "^$\\|" page-delimiter))
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline t
	parse-sexp-ignore-comments t
	indent-line-function 'c-indent-line
	indent-region-function 'c-indent-region
	outline-regexp "[^#\n\^M]"
	outline-level 'c-outline-level
	comment-column 32
	comment-start-skip "/\\*+ *\\|// *"
	adaptive-fill-regexp nil)
  ;; we have to do something special for c-offsets-alist so that the
  ;; buffer local value has its own alist structure.
  (setq c-offsets-alist (copy-alist c-offsets-alist))
  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (if (boundp 'comment-indent-function)
      (progn
	   (make-local-variable 'comment-indent-function)
	   (setq comment-indent-function 'c-comment-indent))
    (make-local-variable 'comment-indent-hook)
    (setq comment-indent-hook 'c-comment-indent))
  ;; Put C menu into menubar and on popup menu for XEmacs 19. I think
  ;; this happens automatically for Emacs 19.
  (if (and (boundp 'current-menubar)
	   current-menubar
	   (not (assoc mode-name current-menubar)))
      ;; its possible that this buffer has changed modes from one of
      ;; the other cc-mode modes.  In that case, only the menubar
      ;; title of the menu changes.
      (let ((modes (copy-sequence c-list-of-mode-names))
	    changed-p)
	(setq modes (delete major-mode modes))
	(while modes
	  (if (not (assoc (car modes) current-menubar))
	      (setq modes (cdr modes))
	    (relabel-menu-item (list (car modes)) mode-name)
	    (setq modes nil
		  changed-p t)))
	(if (not changed-p)
	    (progn
	      (set-buffer-menubar (copy-sequence current-menubar))
	      (add-menu nil mode-name c-mode-menu)))))
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
	    (cons (concat mode-name " Mode Commands") c-mode-menu)))
  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'c-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string c-auto-hungry-string)
		  minor-mode-alist))))

(defun c-postprocess-file-styles ()
  "Function that post processes relevant file local variables.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  It first applies any style
setting found in `c-file-style', then it applies any offset settings
it finds in `c-file-offsets'."
  ;; apply file styles and offsets
  (and c-file-style
       (c-set-style c-file-style))
  (and c-file-offsets
       (mapcar
	(function
	 (lambda (langentry)
	   (let ((langelem (car langentry))
		 (offset (cdr langentry)))
	     (c-set-offset langelem offset)
	     )))
	c-file-offsets)))

;; Add the postprocessing function to hack-local-variables-hook.  As
;; of 28-Aug-1995, XEmacs 19.12 and Emacs 19.29 support this.
(and (fboundp 'add-hook)
     (add-hook 'hack-local-variables-hook 'c-postprocess-file-styles))

(defun c-enable-//-in-c-mode ()
  "Enables // as a comment delimiter in `c-mode'.
ANSI C currently does *not* allow this, although many C compilers
support optional C++ style comments.  To use, call this function from
your `.emacs' file before you visit any C files.  The changes are
global and affect all future `c-mode' buffers."
  (c-setup-dual-comments c-mode-syntax-table)
  (setq-default c-C-comment-start-regexp c-C++-comment-start-regexp))


;; macros must be defined before first use
(defmacro c-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; 
  ;; This function does not modify point or mark.
  (or (and (eq 'quote (car-safe position))
	   (null (cdr (cdr position))))
      (error "bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  (` (let ((here (point)))
       (,@ (cond
	    ((eq position 'bol)  '((beginning-of-line)))
	    ((eq position 'eol)  '((end-of-line)))
	    ((eq position 'bod)
	     '((beginning-of-defun)
	       ;; if defun-prompt-regexp is non-nil, b-o-d won't leave
	       ;; us at the open brace.
	       (and (boundp 'defun-prompt-regexp)
		    defun-prompt-regexp
		    (looking-at defun-prompt-regexp)
		    (goto-char (match-end 0)))
	       ))
	    ((eq position 'boi)  '((back-to-indentation)))
	    ((eq position 'bonl) '((forward-line 1)))
	    ((eq position 'bopl) '((forward-line -1)))
	    ((eq position 'iopl)
	     '((forward-line -1)
	       (back-to-indentation)))
	    ((eq position 'ionl)
	     '((forward-line 1)
	       (back-to-indentation)))
	    (t (error "unknown buffer position requested: %s" position))
	    ))
       (prog1
	   (point)
	 (goto-char here))
       ;; workaround for an Emacs18 bug -- blech! Well, at least it
       ;; doesn't hurt for v19
       (,@ nil)
       )))

(defmacro c-auto-newline ()
  ;; if auto-newline feature is turned on, insert a newline character
  ;; and return t, otherwise return nil.
  (` (and c-auto-newline
	  (not (c-in-literal))
	  (not (newline)))))

(defmacro c-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defun c-insert-special-chars (arg)
  ;; simply call self-insert-command in Emacs 19
  (self-insert-command (prefix-numeric-value arg)))

(defun c-intersect-lists (list alist)
  ;; return the element of ALIST that matches the first element found
  ;; in LIST.  Uses assq.
  (let (match)
    (while (and list
		(not (setq match (assq (car list) alist))))
      (setq list (cdr list)))
    match))

(defun c-lookup-lists (list alist1 alist2)
  ;; first, find the first entry from LIST that is present in ALIST1,
  ;; then find the entry in ALIST2 for that entry.
  (assq (car (c-intersect-lists list alist1)) alist2))


;; This is used by indent-for-comment to decide how much to indent a
;; comment in C code based on its context.
(defun c-comment-indent ()
  (if (looking-at (concat "^\\(" c-comment-start-regexp "\\)"))
      0				;Existing comment at bol stays there.
    (let ((opoint (point))
	  placeholder)
      (save-excursion
	(beginning-of-line)
	(cond
	 ;; CASE 1: A comment following a solitary close-brace should
	 ;; have only one space.
	 ((looking-at (concat "[ \t]*}[ \t]*\\($\\|"
			      c-comment-start-regexp
			      "\\)"))
	  (search-forward "}")
	  (1+ (current-column)))
	 ;; CASE 2: 2 spaces after #endif
	 ((or (looking-at "^#[ \t]*endif[ \t]*")
	      (looking-at "^#[ \t]*else[ \t]*"))
	  7)
	 ;; CASE 3: when comment-column is nil, calculate the offset
	 ;; according to c-offsets-alist.  E.g. identical to hitting
	 ;; TAB.
	 ((and c-indent-comments-syntactically-p
	       (save-excursion
		 (skip-chars-forward " \t")
		 (or (looking-at comment-start)
		     (eolp))))
	  (let ((syntax (c-guess-basic-syntax)))
	    ;; BOGOSITY ALERT: if we're looking at the eol, its
	    ;; because indent-for-comment hasn't put the comment-start
	    ;; in the buffer yet.  this will screw up the syntactic
	    ;; analysis so we kludge in the necessary info.  Another
	    ;; kludge is that if we're at the bol, then we really want
	    ;; to ignore any anchoring as specified by
	    ;; c-comment-only-line-offset since it doesn't apply here.
	    (if (save-excursion
		  (beginning-of-line)
		  (skip-chars-forward " \t")
		  (eolp))
		(c-add-syntax 'comment-intro))
	    (let ((c-comment-only-line-offset
		   (if (consp c-comment-only-line-offset)
		       c-comment-only-line-offset
		     (cons c-comment-only-line-offset
			   c-comment-only-line-offset))))
	      (apply '+ (mapcar 'c-get-offset syntax)))))
	 ;; CASE 4: use comment-column if previous line is a
	 ;; comment-only line indented to the left of comment-column
	 ((save-excursion
	    (beginning-of-line)
	    (and (not (bobp))
		 (forward-line -1))
	    (skip-chars-forward " \t")
	    (prog1
		(looking-at c-comment-start-regexp)
	      (setq placeholder (point))))
	  (goto-char placeholder)
	  (if (< (current-column) comment-column)
	      comment-column
	    (current-column)))
	 ;; CASE 5: If comment-column is 0, and nothing but space
	 ;; before the comment, align it at 0 rather than 1.
	 ((progn
	    (goto-char opoint)
	    (skip-chars-backward " \t")
	    (and (= comment-column 0) (bolp)))
	  0)
	 ;; CASE 6: indent at comment column except leave at least one
	 ;; space.
	 (t (max (1+ (current-column))
		 comment-column))
	 )))))

;; used by outline-minor-mode
(defun c-outline-level ()
  (save-excursion
    (skip-chars-forward "\t ")
    (current-column)))

;; active regions, and auto-newline/hungry delete key
(defun c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in
  ;; XEmacs 19. ignore byte-compiler warnings you might see
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defun c-update-modeline ()
  ;; set the c-auto-hungry-string for the correct designation on the modeline
  (setq c-auto-hungry-string
	(if c-auto-newline
	    (if c-hungry-delete-key "/ah" "/a")
	  (if c-hungry-delete-key "/h" nil)))
  ;; updates the modeline for all Emacsen
  (if (memq 'v19 c-emacs-features)
      (force-mode-line-update)
    (set-buffer-modified-p (buffer-modified-p))))

(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

(defun c-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero.

When the auto-newline feature is enabled (as evidenced by the `/a' or
`/ah' on the modeline after the mode name) newlines are automatically
inserted after special characters such as brace, comma, semi-colon,
and colon."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero.

When the hungry-delete-key feature is enabled (as evidenced by the
`/h' or `/ah' on the modeline after the mode name) the delete key
gobbles all preceding whitespace in one fell swoop."
  (interactive "P")
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero.

See `c-toggle-auto-state' and `c-toggle-hungry-state' for details."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))


;; COMMANDS
(defun c-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-delete-function' is called."
  (interactive "P")
  (if (or (not c-hungry-delete-key)
	  arg
	  (c-in-literal))
      (funcall c-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall c-delete-function 1)
	))))

(defun c-electric-pound (arg)
  "Electric pound (`#') insertion.
Inserts a `#' character specially depending on the variable
`c-electric-pound-behavior'.  If a numeric ARG is supplied, or if
point is inside a literal, nothing special happens."
  (interactive "P")
  (if (or (c-in-literal)
	  arg
	  (not (memq 'alignleft c-electric-pound-behavior)))
      ;; do nothing special
      (self-insert-command (prefix-numeric-value arg))
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point)))
	  (bolp (bolp)))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert-char last-command-char 1)
      (and (not bolp)
	   (goto-char (- (point-max) pos)))
      )))

(defun c-electric-brace (arg)
  "Insert a brace.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after braces based on the value of `c-hanging-braces-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the brace, or
the brace is inserted inside a literal."
  (interactive "P")
  (let* ((c-state-cache (c-parse-state))
	 (safepos (c-safe-position (point) c-state-cache))
	 (literal (c-in-literal safepos)))
    ;; if we're in a literal, or we're not at the end of the line, or
    ;; a numeric arg is provided, or auto-newlining is turned off,
    ;; then just insert the character.
    (if (or literal arg
;	    (not c-auto-newline)
	    (not (looking-at "[ \t]*$")))
	(c-insert-special-chars arg)	
      (let* ((syms '(class-open class-close defun-open defun-close 
		     inline-open inline-close brace-list-open brace-list-close
		     brace-list-intro brace-list-entry block-open block-close
		     substatement-open statement-case-open))
	    ;; we want to inhibit blinking the paren since this will
	    ;; be most disruptive. we'll blink it ourselves later on
	    (old-blink-paren (if (boundp 'blink-paren-function)
				 blink-paren-function
			       blink-paren-hook))
	    blink-paren-function	; emacs19
	    blink-paren-hook		; emacs18
	    (insertion-point (point))
	    delete-temp-newline
	    (preserve-p (= 32 (char-syntax (preceding-char))))
	    ;; shut this up too
	    (c-echo-syntactic-information-p nil)
	    (syntax (progn
		      ;; only insert a newline if there is
		      ;; non-whitespace behind us
		      (if (save-excursion
			    (skip-chars-backward " \t")
			    (not (bolp)))
			  (progn (newline)
				 (setq delete-temp-newline t)))
		      (self-insert-command (prefix-numeric-value arg))
		      ;; state cache doesn't change
		      (c-guess-basic-syntax)))
	    (newlines (and
		       c-auto-newline
		       (or (c-lookup-lists syms syntax c-hanging-braces-alist)
			   '(ignore before after)))))
	;; If syntax is a function symbol, then call it using the
	;; defined semantics.
	(if (and (not (consp (cdr newlines)))
		 (fboundp (cdr newlines)))
	    (let ((c-syntactic-context syntax))
	      (setq newlines
		    (funcall (cdr newlines) (car newlines) insertion-point))))
	;; does a newline go before the open brace?
	(if (memq 'before newlines)
	    ;; we leave the newline we've put in there before,
	    ;; but we need to re-indent the line above
	    (let ((pos (- (point-max) (point)))
		  (here (point))
		  (c-state-cache c-state-cache))
	      (forward-line -1)
	      ;; we may need to update the cache. this should still be
	      ;; faster than recalculating the state in many cases
	      (save-excursion
		(save-restriction
		  (narrow-to-region here (point))
		  (if (and (c-safe (progn (backward-up-list -1) t))
			   (memq (preceding-char) '(?\) ?}))
			   (progn (widen)
				  (c-safe (progn (forward-sexp -1) t))))
		      (setq c-state-cache
			    (c-hack-state (point) 'open c-state-cache))
		    (if (and (car c-state-cache)
			     (not (consp (car c-state-cache)))
			     (<= (point) (car c-state-cache)))
			(setq c-state-cache (cdr c-state-cache))
		      ))))
	      (let ((here (point))
		    (shift (c-indent-line)))
		(setq c-state-cache (c-adjust-state (c-point 'bol) here
						    (- shift) c-state-cache)))
	      (goto-char (- (point-max) pos))
	      ;; if the buffer has changed due to the indentation, we
	      ;; need to recalculate syntax for the current line, but
	      ;; we won't need to update the state cache.
	      (if (/= (point) here)
		  (setq syntax (c-guess-basic-syntax))))
	  ;; must remove the newline we just stuck in (if we really did it)
	  (and delete-temp-newline
	       (save-excursion
		 ;; if there is whitespace before point, then preserve
		 ;; at least one space.
		 (delete-indentation)
		 (just-one-space)
		 (if (not preserve-p)
		     (delete-char -1))))
	  ;; since we're hanging the brace, we need to recalculate
	  ;; syntax.  Update the state to accurately reflect the
	  ;; beginning of the line.  We punt if we cross any open or
	  ;; closed parens because its just too hard to modify the
	  ;; known state.  This limitation will be fixed in v5.
	  (save-excursion
	    (let ((bol (c-point 'bol)))
	      (if (zerop (car (parse-partial-sexp bol (1- (point)))))
		  (setq c-state-cache (c-whack-state bol c-state-cache)
			syntax (c-guess-basic-syntax))
		;; gotta punt. this requires some horrible kludgery
		(beginning-of-line)
		(makunbound 'c-state-cache)
		(setq c-state-cache (c-parse-state)
		      syntax nil))))
	  )
	;; now adjust the line's indentation. don't update the state
	;; cache since c-guess-basic-syntax isn't called when the
	;; syntax is passed to c-indent-line
	(let ((here (point))
	      (shift (c-indent-line syntax)))
	  (setq c-state-cache (c-adjust-state (c-point 'bol) here
					      (- shift) c-state-cache)))
	;; Do all appropriate clean ups
	(let ((here (point))
	      (pos (- (point-max) (point)))
	      mbeg mend)
	  ;; clean up empty defun braces
	  (if (and c-auto-newline
		   (memq 'empty-defun-braces c-cleanup-list)
		   (= last-command-char ?\})
		   (c-intersect-lists '(defun-close class-close inline-close)
				      syntax)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (= (preceding-char) ?\{))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal)))
	      (delete-region (point) (1- here)))
	  ;; clean up brace-else-brace
	  (if (and c-auto-newline
		   (memq 'brace-else-brace c-cleanup-list)
		   (= last-command-char ?\{)
		   (re-search-backward "}[ \t\n]*else[ \t\n]*{" nil t)
		   (progn
		     (setq mbeg (match-beginning 0)
			   mend (match-end 0))
		     (= mend here))
		   (not (c-in-literal)))
	      (progn
		(delete-region mbeg mend)
		(insert "} else {")))
	  (goto-char (- (point-max) pos))
	  )
	;; does a newline go after the brace?
	(if (memq 'after newlines)
	    (progn
	      (newline)
	      ;; update on c-state-cache
	      (let* ((bufpos (- (point) 2))
		     (which (if (= (char-after bufpos) ?{) 'open 'close))
		     (c-state-cache (c-hack-state bufpos which c-state-cache)))
		(c-indent-line))))
	;; blink the paren
	(and (= last-command-char ?\})
	     old-blink-paren
	     (save-excursion
	       (c-backward-syntactic-ws safepos)
	       (if (boundp 'blink-paren-function)
		   (funcall old-blink-paren)
		 (run-hooks old-blink-paren))))
	))))
      
(defun c-electric-slash (arg)
  "Insert a slash character.
If slash is second of a double-slash C++ style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (= (preceding-char) ?/)
		      (= last-command-char ?/)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

(defun c-electric-star (arg)
  "Insert a star character.
If the star is the second character of a C style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  ;; if we are in a literal, or if arg is given do not re-indent the
  ;; current line, unless this star introduces a comment-only line.
  (if (and (not arg)
	   (memq (c-in-literal) '(c))
	   (= (preceding-char) ?*)
	   (save-excursion
	     (forward-char -1)
	     (skip-chars-backward "*")
	     (if (= (preceding-char) ?/)
		 (forward-char -1))
	     (skip-chars-backward " \t")
	     (bolp)))
      ;; shut this up
      (let (c-echo-syntactic-information-p)
	(c-indent-line))
    ))

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, a newline might be inserted.  See
the variable `c-hanging-semi&comma-criteria' for how newline insertion
is determined.

When semicolon is inserted, the line is re-indented unless a numeric
arg is supplied, point is inside a literal, or there are
non-whitespace characters on the line following the semicolon."
  (interactive "P")
  (let* ((lim (c-most-enclosing-brace (c-parse-state)))
	 (literal (c-in-literal lim))
	 (here (point))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(c-insert-special-chars arg)
      ;; do some special stuff with the character
      (self-insert-command (prefix-numeric-value arg))
      ;; do all cleanups, reindentations, and newline insertions, but
      ;; only if c-auto-newline is turned on
      (if (not c-auto-newline) nil
	;; clean ups
	(let ((pos (- (point-max) (point))))
	  (if (and (or (and
			(= last-command-char ?,)
			(memq 'list-close-comma c-cleanup-list))
		       (and
			(= last-command-char ?\;)
			(memq 'defun-close-semi c-cleanup-list)))
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (= (preceding-char) ?}))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal lim)))
	      (delete-region (point) here))
	  (goto-char (- (point-max) pos)))
	;; re-indent line
	(c-indent-line)
	;; check to see if a newline should be added
	(let ((criteria c-hanging-semi&comma-criteria)
	      answer add-newline-p)
	  (while criteria
	    (setq answer (funcall (car criteria)))
	    ;; only nil value means continue checking
	    (if (not answer)
		(setq criteria (cdr criteria))
	      (setq criteria nil)
	      ;; only 'stop specifically says do not add a newline
	      (setq add-newline-p (not (eq answer 'stop)))
	      ))
	  (if add-newline-p
	      (progn (newline)
		     (c-indent-line)))
	  )))))

(defun c-semi&comma-inside-parenlist ()
  "Determine if a newline should be added after a semicolon.
If a comma was inserted, no determination is made.  If a semicolon was
inserted inside a parenthesis list, no newline is added otherwise a
newline is added.  In either case, checking is stopped.  This supports
exactly the old newline insertion behavior."
  ;; newline only after semicolon, but only if that semicolon is not
  ;; inside a parenthesis list (e.g. a for loop statement)
  (if (/= last-command-char ?\;)
      nil				; continue checking
    (if (condition-case nil
	    (save-excursion
	      (up-list -1)
	      (/= (following-char) ?\())
	  (error t))
	t
      'stop)))

(defun c-electric-colon (arg)
  "Insert a colon.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after colons based on the value of `c-hanging-colons-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the colon, or
the colon is inserted inside a literal.

This function cleans up double colon scope operators based on the
value of `c-cleanup-list'."
  (interactive "P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 syntax newlines
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(c-insert-special-chars arg)
      ;; insert the colon, then do any specified cleanups
      (self-insert-command (prefix-numeric-value arg))
      (let ((pos (- (point-max) (point)))
	    (here (point)))
	(if (and c-auto-newline
		 (memq 'scope-operator c-cleanup-list)
		 (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (= (preceding-char) ?:))
		 (not (c-in-literal))
		 (not (= (char-after (- (point) 2)) ?:)))
	    (delete-region (point) (1- here)))
	(goto-char (- (point-max) pos)))
      ;; lets do some special stuff with the colon character
      (setq syntax (c-guess-basic-syntax)
	    ;; some language elements can only be determined by
	    ;; checking the following line.  Lets first look for ones
	    ;; that can be found when looking on the line with the
	    ;; colon
	    newlines
	    (and c-auto-newline
		 (or (c-lookup-lists '(case-label label access-label)
				     syntax c-hanging-colons-alist)
		     (c-lookup-lists '(member-init-intro inher-intro)
				     (prog2
					 (insert "\n")
					 (c-guess-basic-syntax)
				       (delete-char -1))
				     c-hanging-colons-alist))))
      ;; indent the current line
      (c-indent-line syntax)
      ;; does a newline go before the colon?  Watch out for already
      ;; non-hung colons.  However, we don't unhang them because that
      ;; would be a cleanup (and anti-social).
      (if (and (memq 'before newlines)
	       (save-excursion
		 (skip-chars-backward ": \t")
		 (not (bolp))))
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (c-indent-line)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (c-indent-line)))
      )))

(defun c-electric-lt-gt (arg)
  "Insert a less-than, or greater-than character.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, the line will be re-indented if
the character inserted is the second of a C++ style stream operator
and the buffer is in C++ mode.

The line will also not be re-indented if a numeric argument is
supplied, or point is inside a literal."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (= (preceding-char) last-command-char)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

;; set up electric character functions to work with pending-del,
;; (a.k.a. delsel) mode.  All symbols get the t value except
;; c-electric-delete which gets 'supersede.
(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for delsel (Emacs)
    (put sym 'pending-delete t)))	; for pending-del (XEmacs)
 '(c-electric-pound
   c-electric-brace
   c-electric-slash
   c-electric-star
   c-electric-semi&comma
   c-electric-lt-gt
   c-electric-colon))
(put 'c-electric-delete 'delete-selection 'supersede) ; delsel
(put 'c-electric-delete 'pending-delete   'supersede) ; pending-del



(defun c-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; legal value only
  (let* ((oldoff (cdr-safe (assq langelem c-offsets-alist)))
	 (defstr (format "(default %s): " oldoff))
	 (errmsg (concat "Offset must be int, func, var, "
			 "or in [+,-,++,--,*,/] "
			 defstr))
	 (prompt (concat "Offset " defstr))
	 offset input interned)
    (while (not offset)
      (setq input (read-string prompt)
	    offset (cond ((string-equal "" input) oldoff)  ; default
			 ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-equal "*" input) '*)
			 ((string-equal "/" input) '/)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-int input))
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

(defun c-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`c-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     (function
		      (lambda (langelem)
			(cons (format "%s" (car langelem)) nil)))
		     c-offsets-alist)
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (c-guess-basic-syntax))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      (if (memq 'v19 c-emacs-features)
			  (cons ic 0)
			ic))
		    )))
	  (offset (c-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (eq offset '*)
      (eq offset '/)
      (integerp offset)
      (fboundp offset)
      (boundp offset)
      (error "Offset must be int, func, var, or in [+,-,++,--,*,/]: %s"
	     offset))
  (let ((entry (assq symbol c-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq c-offsets-alist (cons (cons symbol offset) c-offsets-alist))
	(error "%s is not a valid syntactic symbol." symbol))))
  (c-keep-region-active))

(defun c-set-style-1 (stylevars)
  ;; given a style's variable alist, institute the style
  (mapcar
   (function
    (lambda (conscell)
      (let ((attr (car conscell))
	    (val  (cdr conscell)))
	;; KLUDGE ALERT: special case for c-offsets-alist
	(if (not (eq attr 'c-offsets-alist))
	    (set attr val)
	  (mapcar
	   (function
	    (lambda (langentry)
	      (let ((langelem (car langentry))
		    (offset (cdr langentry)))
		(c-set-offset langelem offset)
		)))
	   val))
	)))
   stylevars))

;;;###autoload
(defun c-set-style (stylename)
  "Set cc-mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `c-style-alist'.  See that variable
for details of setting up styles."
  (interactive (list (let ((completion-ignore-case t)
			   (prompt (format "Which %s indentation style? "
					   mode-name)))
		       (completing-read prompt c-style-alist nil t))))
  (let ((vars (cdr (or (assoc (downcase stylename) c-style-alist)
		       ;; backwards compatibility
		       (assoc (upcase stylename) c-style-alist)
		       )))
	(default (cdr (assoc "cc-mode" c-style-alist))))
    (or vars (error "Invalid indentation style `%s'" stylename))
    (or default (error "No `cc-mode' style found!"))
    ;; first reset the style to `cc-mode' to give every style a common
    ;; base. Then institute the new style.
    (c-set-style-1 default)
    (if (not (string= stylename "cc-mode"))
	(c-set-style-1 vars)))
  (c-keep-region-active))

(defun c-add-style (style descrip &optional set-p)
  "Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ((VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of VARIABLE and
VALUE.  This function also sets the current style to STYLE using
`c-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename (completing-read "Style to add: " c-style-alist))
	 (description (eval-minibuffer "Style description: ")))
     (list stylename description
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style c-style-alist)))
    (if s
	(setcdr s (copy-alist descrip))	; replace
      (setq c-style-alist (cons (cons style descrip) c-style-alist))))
  (and set-p (c-set-style style)))

(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations.

Optional prefix ARG means justify paragraph as well."
  (interactive "P")
  (let* (comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t\n")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point)))))
	 (re1 (if (memq 'new-re c-emacs-features)
		  "\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$"
		"\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[ \t/*]*$"))
	 )
    (if (and c-double-slash-is-comments-p
	     (save-excursion
	       (beginning-of-line)
	       (looking-at ".*//")))
	(let (fill-prefix
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next
	       ;; to.
	      (paragraph-start (concat paragraph-start re1))
	      (paragraph-separate (concat paragraph-separate re1)))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp))
			(looking-at "[ \t]*//"))
	      (forward-line -1))
	    (if (not (looking-at ".*//"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.
	    (setq fill-prefix (buffer-substring (match-beginning 0)
						(match-end 0)))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (c-point 'bol)
				(save-excursion
				  (forward-line 1)
				  (while (looking-at fill-prefix)
				    (forward-line 1))
				  (point)))
	      (fill-paragraph arg)
	      t)))
      ;; else C style comments
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (eq (c-in-literal) 'c)
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point))))))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 ;; The prefix for each line of this paragraph
		 ;; is the appropriate part of the start of this line,
		 ;; up to the column at which text should be indented.
		 (save-excursion
		   (beginning-of-line)
		   (if (looking-at "[ \t]*/\\*.*\\*/")
		       (progn (re-search-forward comment-start-skip)
			      (make-string (current-column) ?\ ))
		     (if first-line (forward-line 1))

		     (let ((line-width (progn (end-of-line) (current-column))))
		       (beginning-of-line)
		       (prog1
			   (buffer-substring
			    (point)

			    ;; How shall we decide where the end of the
			    ;; fill-prefix is?
			    (progn
			      (beginning-of-line)
			      (skip-chars-forward " \t*" (c-point 'eol))
			      (point)))

			 ;; If the comment is only one line followed
			 ;; by a blank line, calling move-to-column
			 ;; above may have added some spaces and tabs
			 ;; to the end of the line; the fill-paragraph
			 ;; function will then delete it and the
			 ;; newline following it, so we'll lose a
			 ;; blank line when we shouldn't.  So delete
			 ;; anything move-to-column added to the end
			 ;; of the line.  We record the line width
			 ;; instead of the position of the old line
			 ;; end because move-to-column might break a
			 ;; tab into spaces, and the new characters
			 ;; introduced there shouldn't be deleted.

			 ;; If you can see a better way to do this,
			 ;; please make the change.  This seems very
			 ;; messy to me.
			 (delete-region (progn (move-to-column line-width)
					       (point))
					(progn (end-of-line) (point))))))))

		;; Lines containing just a comment start or just an end
		;; should not be filled into paragraphs they are next
		;; to.
		(paragraph-start (concat paragraph-start re1))
		(paragraph-separate (concat paragraph-separate re1))
		(chars-to-delete 0))
	    (save-restriction
	      ;; Don't fill the comment together with the code
	      ;; following it.  So temporarily exclude everything
	      ;; before the comment start, and everything after the
	      ;; line where the comment ends.  If comment-start-place
	      ;; is non-nil, the comment starter is there.  Otherwise,
	      ;; point is inside the comment.
	      (narrow-to-region (save-excursion
				  (if comment-start-place
				      (goto-char comment-start-place)
				    (search-backward "/*"))
				  ;; Protect text before the comment
				  ;; start by excluding it.  Add
				  ;; spaces to bring back proper
				  ;; indentation of that point.
				  (let ((column (current-column)))
				    (prog1 (point)
				      (setq chars-to-delete column)
				      (insert-char ?\  column))))
				(save-excursion
				  (if comment-start-place
				      (goto-char (+ comment-start-place 2)))
				  (search-forward "*/" nil 'move)
				  (forward-line 1)
				  (point)))
	      (fill-paragraph arg)
	      (save-excursion
		;; Delete the chars we inserted to avoid clobbering
		;; the stuff before the comment start.
		(goto-char (point-min))
		(if (> chars-to-delete 0)
		    (delete-region (point) (+ (point) chars-to-delete)))
		;; Find the comment ender (should be on last line of
		;; buffer, given the narrowing) and don't leave it on
		;; its own line, unless that's the style that's desired.
		(goto-char (point-max))
		(forward-line -1)
		(search-forward "*/" nil 'move)
		(beginning-of-line)
		(if (and c-hanging-comment-ender-p
			 (looking-at "[ \t]*\\*/"))
		    ;(delete-indentation)))))
		    (let ((fill-column (+ fill-column 9999)))
		      (forward-line -1)
		      (fill-region-as-paragraph (point) (point-max))))))
	    t)))))

;; better movement routines for ThisStyleOfVariablesCommonInCPlusPlus
;; originally contributed by Terry_Glanfield.Southern@rxuk.xerox.com
(defun c-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With arg, to it arg times."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
	(re-search-forward "\\W*\\([A-Z]*[a-z0-9]*\\)" (point-max) t arg)
      (while (and (< arg 0)
		  (re-search-backward
		   "\\(\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\W\\w+\\)"
		   (point-min) 0))
	(forward-char 1)
	(setq arg (1+ arg)))))
  (c-keep-region-active))

(defun c-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (c-forward-into-nomenclature (- arg))
  (c-keep-region-active))

(defun c-scope-operator ()
  "Insert a double colon scope operator at point.
No indentation or other \"electric\" behavior is performed."
  (interactive)
  (insert "::"))


(defun c-beginning-of-statement (&optional count lim sentence-flag)
  "Go to the beginning of the innermost C statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (let ((here (point))
	(count (or count 1))
	(lim (or lim (c-point 'bod)))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and sentence-flag
	     (or (nth 3 state)
		 (nth 4 state)
		 (looking-at (concat "[ \t]*" comment-start-skip))
		 (save-excursion
		   (skip-chars-backward " \t")
		   (goto-char (- (point) 2))
		   (looking-at "\\*/"))))
	(forward-sentence (- count))
      (while (> count 0)
	(c-beginning-of-statement-1 lim)
	(setq count (1- count)))
      (while (< count 0)
	(c-end-of-statement-1)
	(setq count (1+ count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    )
  (c-keep-region-active))

(defun c-end-of-statement (&optional count lim sentence-flag)
  "Go to the end of the innermost C statement.

With prefix arg, go forward N - 1 statements.  Move forward to end of
the next statement if already at end.  If within a string or comment,
move by sentences instead of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (c-beginning-of-statement (- (or count 1)) lim sentence-flag)
  (c-keep-region-active))

(defun c-beginning-of-statement-1 (&optional lim)
  ;; move to the start of the current statement, or the previous
  ;; statement if already at the beginning of one.
  (let ((firstp t)
	(substmt-p t)
	donep c-in-literal-cache
	;; KLUDGE ALERT: maybe-labelp is used to pass information
	;; between c-crosses-statement-barrier-p and
	;; c-beginning-of-statement-1.  A better way should be
	;; implemented.
	maybe-labelp
	(last-begin (point)))
    (while (not donep)
      ;; stop at beginning of buffer
      (if (bobp) (setq donep t)
	;; go backwards one balanced expression, but be careful of
	;; unbalanced paren being reached
	(if (not (c-safe (progn (backward-sexp 1) t)))
	    (progn
	      (if firstp
		  (backward-up-list 1)
		(goto-char last-begin))
	      ;; skip over any unary operators, or other special
	      ;; characters appearing at front of identifier
	      (save-excursion
		(c-backward-syntactic-ws lim)
		(skip-chars-backward "-+!*&:.~ \t\n")
		(if (= (preceding-char) ?\()
		    (setq last-begin (point))))
	      (goto-char last-begin)
	      (setq last-begin (point)
		    donep t)))

	(setq maybe-labelp nil)
	;; see if we're in a literal. if not, then this bufpos may be
	;; a candidate for stopping
	(cond
	 ;; CASE 0: did we hit the error condition above?
	 (donep)
	 ;; CASE 1: are we in a literal?
	 ((eq (c-in-literal lim) 'pound)
	  (beginning-of-line))
	 ;; CASE 2: some other kind of literal?
	 ((c-in-literal lim))
	 ;; CASE 3: are we looking at a conditional keyword?
	 ((or (looking-at c-conditional-key)
	      (and (= (following-char) ?\()
		   (save-excursion
		     (forward-sexp 1)
		     (c-forward-syntactic-ws)
		     (/= (following-char) ?\;))
		   (let ((here (point))
			 (foundp (progn
				   (c-backward-syntactic-ws lim)
				   (forward-word -1)
				   (and lim
					(<= lim (point))
					(not (c-in-literal lim))
					(looking-at c-conditional-key)
					))))
		     ;; did we find a conditional?
		     (if (not foundp)
			 (goto-char here))
		     foundp)))
	  ;; are we in the middle of an else-if clause?
	  (if (save-excursion
		(and (not substmt-p)
		     (c-safe (progn (forward-sexp -1) t))
		     (looking-at "\\<else\\>[ \t\n]+\\<if\\>")
		     (not (c-in-literal lim))))
	      (progn
		(forward-sexp -1)
		(c-backward-to-start-of-if lim)))
	  ;; are we sitting at an else clause, that we are not a
	  ;; substatement of?
	  (if (and (not substmt-p)
		   (looking-at "\\<else\\>[^_]"))
	      (c-backward-to-start-of-if lim))
	  ;; are we sitting at the while of a do-while?
	  (if (and (looking-at "\\<while\\>[^_]")
		   (c-backward-to-start-of-do lim))
	      (setq substmt-p nil))
	  (setq last-begin (point)
		donep substmt-p))
	 ;; CASE 4: are we looking at a label?
	 ((looking-at c-label-key))
	 ;; CASE 5: is this the first time we're checking?
	 (firstp (setq firstp nil
		       substmt-p (not (c-crosses-statement-barrier-p
				       (point) last-begin))
		       last-begin (point)))
	 ;; CASE 6: have we crossed a statement barrier?
	 ((c-crosses-statement-barrier-p (point) last-begin)
	  (setq donep t))
	 ;; CASE 7: ignore labels
	 ((and maybe-labelp
	       (or (and c-access-key (looking-at c-access-key))
		   ;; with switch labels, we have to go back further
		   ;; to try to pick up the case or default
		   ;; keyword. Potential bogosity alert: we assume
		   ;; `case' or `default' is first thing on line
		   (let ((here (point)))
		     (beginning-of-line)
		     (c-forward-syntactic-ws)
		     (if (looking-at c-switch-label-key)
			 t
		       (goto-char here)
		       nil))
		   (looking-at c-label-key))))
	 ;; CASE 8: ObjC or Java method def
	 ((and c-method-key
	       (setq last-begin (c-in-method-def-p)))
	  (setq donep t))
	 ;; CASE 9: nothing special
	 (t (setq last-begin (point)))
	 )))
    (goto-char last-begin)
    ;; we always do want to skip over non-whitespace modifier
    ;; characters that didn't get skipped above
    (skip-chars-backward "-+!*&:.~" (c-point 'boi))))

(defun c-end-of-statement-1 ()
  (condition-case ()
      (progn
	(while (and (not (eobp))
		    (let ((beg (point)))
		      (forward-sexp 1)
		      (let ((end (point)))
			(save-excursion
			  (goto-char beg)
			  (not (re-search-forward "[;{}]" end t)))))))
	(re-search-backward "[;}]")
	(forward-char 1))
    (error 
     (let ((beg (point)))
       (backward-up-list -1)
       (let ((end (point)))
	 (goto-char beg)
	 (search-forward ";" end 'move))))))

(defun c-crosses-statement-barrier-p (from to)
  ;; Does buffer positions FROM to TO cross a C statement boundary?
  (let ((here (point))
	(lim from)
	crossedp)
    (condition-case ()
	(progn
	  (goto-char from)
	  (while (and (not crossedp)
		      (< (point) to))
	    (skip-chars-forward "^;{}:" to)
	    (if (not (c-in-literal lim))
		(progn
		  (if (memq (following-char) '(?\; ?{ ?}))
		      (setq crossedp t)
		    (if (= (following-char) ?:)
			(setq maybe-labelp t))
		    (forward-char 1))
		  (setq lim (point)))
	      (forward-char 1))))
      (error (setq crossedp nil)))
    (goto-char here)
    crossedp))


(defun c-up-conditional (count)
  "Move back to the containing preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward to the end of the containing preprocessor conditional.
When going backwards, `#elif' is treated like `#else' followed by
`#if'.  When going forwards, `#elif' is ignored."
  (interactive "p")
  (c-forward-conditional (- count) t)
  (c-keep-region-active))

(defun c-backward-conditional (count &optional up-flag)
  "Move back across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward across a preprocessor conditional."
  (interactive "p")
  (c-forward-conditional (- count) up-flag)
  (c-keep-region-active))

(defun c-forward-conditional (count &optional up-flag)
  "Move forward across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward across a preprocessor conditional."
  (interactive "p")
  (let* ((forward (> count 0))
	 (increment (if forward -1 1))
	 (search-function (if forward 're-search-forward 're-search-backward))
	 (new))
    (save-excursion
      (while (/= count 0)
	(let ((depth (if up-flag 0 -1)) found)
	  (save-excursion
	    ;; Find the "next" significant line in the proper direction.
	    (while (and (not found)
			;; Rather than searching for a # sign that
			;; comes at the beginning of a line aside from
			;; whitespace, search first for a string
			;; starting with # sign.  Then verify what
			;; precedes it.  This is faster on account of
			;; the fastmap feature of the regexp matcher.
			(funcall search-function
				 "#[ \t]*\\(if\\|elif\\|endif\\)"
				 nil t))
	      (beginning-of-line)
	      ;; Now verify it is really a preproc line.
	      (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\)")
		  (let ((prev depth))
		    ;; Update depth according to what we found.
		    (beginning-of-line)
		    (cond ((looking-at "[ \t]*#[ \t]*endif")
			   (setq depth (+ depth increment)))
			  ((looking-at "[ \t]*#[ \t]*elif")
			   (if (and forward (= depth 0))
			       (setq found (point))))
			  (t (setq depth (- depth increment))))
		    ;; If we are trying to move across, and we find an
		    ;; end before we find a beginning, get an error.
		    (if (and (< prev 0) (< depth prev))
			(error (if forward
				   "No following conditional at this level"
				 "No previous conditional at this level")))
		    ;; When searching forward, start from next line so
		    ;; that we don't find the same line again.
		    (if forward (forward-line 1))
		    ;; If this line exits a level of conditional, exit
		    ;; inner loop.
		    (if (< depth 0)
			(setq found (point))))
		;; else
		(if forward (forward-line 1))
		)))
	  (or found
	      (error "No containing preprocessor conditional"))
	  (goto-char (setq new found)))
	(setq count (+ count increment))))
    (push-mark)
    (goto-char new))
  (c-keep-region-active))


;; commands to indent lines, regions, defuns, and expressions
(defun c-indent-command (&optional whole-exp)
  "Indent current line as C++ code, or in some cases insert a tab character.

If `c-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert a tab.  If other than nil
or t, then tab is inserted only within literals (comments and strings)
and inside preprocessor directives, but line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (let ((bod (c-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (c-indent-line))
	      beg end)
	  (save-excursion
	    (if (eq c-tab-always-indent t)
		(beginning-of-line))
	    (setq beg (point))
	    (forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end (- shift-amt) "#")))
      ;; No arg supplied, use c-tab-always-indent to determine
      ;; behavior
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not c-tab-always-indent)
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (insert-tab)
	  (c-indent-line)))
       ;; CASE 2: just indent the line
       ((eq c-tab-always-indent t)
	(c-indent-line))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (c-in-literal bod)
	    (insert-tab))
	(c-indent-line)
	)))))

(defun c-indent-exp (&optional shutup-p)
  "Indent each line in balanced expression following point.
Optional SHUTUP-P if non-nil, inhibits message printing and error checking."
  (interactive "P")
  (let ((here (point))
	end progress-p)
    (unwind-protect
	(let ((c-echo-syntactic-information-p nil) ;keep quiet for speed
	      (start (progn
		       ;; try to be smarter about finding the range of
		       ;; lines to indent. skip all following
		       ;; whitespace. failing that, try to find any
		       ;; opening brace on the current line
		       (skip-chars-forward " \t\n")
		       (if (memq (following-char) '(?\( ?\[ ?\{))
			   (point)
			 (let ((state (parse-partial-sexp (point)
							  (c-point 'eol))))
			   (and (nth 1 state)
				(goto-char (nth 1 state))
				(memq (following-char) '(?\( ?\[ ?\{))
				(point)))))))
	  ;; find balanced expression end
	  (setq end (and (c-safe (progn (forward-sexp 1) t))
			 (point-marker)))
	  ;; sanity check
	  (and (not start)
	       (not shutup-p)
	       (error "Cannot find start of balanced expression to indent."))
	  (and (not end)
	       (not shutup-p)
	       (error "Cannot find end of balanced expression to indent."))
	  (c-progress-init start end 'c-indent-exp)
	  (setq progress-p t)
	  (goto-char start)
	  (beginning-of-line)
	  (while (< (point) end)
	    (if (not (looking-at "[ \t]*$"))
		(c-indent-line))
	    (c-progress-update)
	    (forward-line 1)))
      ;; make sure marker is deleted
      (and end
	   (set-marker end nil))
      (and progress-p
	   (c-progress-fini 'c-indent-exp))
      (goto-char here))))

(defun c-indent-defun ()
  "Re-indents the current top-level function def, struct or class declaration."
  (interactive)
  (let ((here (point-marker))
	(c-echo-syntactic-information-p nil)
	(brace (c-least-enclosing-brace (c-parse-state))))
    (if brace
	(goto-char brace)
      (beginning-of-defun))
    ;; if we're sitting at b-o-b, it might be because there was no
    ;; least enclosing brace and we were sitting on the defun's open
    ;; brace.
    (if (and (bobp) (not (= (following-char) ?\{)))
	(goto-char here))
    ;; if defun-prompt-regexp is non-nil, b-o-d might not leave us at
    ;; the open brace. I consider this an Emacs bug.
    (and (boundp 'defun-prompt-regexp)
	 defun-prompt-regexp
	 (looking-at defun-prompt-regexp)
	 (goto-char (match-end 0)))
    ;; catch all errors in c-indent-exp so we can 1. give more
    ;; meaningful error message, and 2. restore point
    (unwind-protect
	(c-indent-exp)
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (save-excursion
    (goto-char start)
    ;; Advance to first nonblank line.
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (let (endmark)
      (unwind-protect
	  (let ((c-tab-always-indent t)
		;; shut up any echo msgs on indiv lines
		(c-echo-syntactic-information-p nil))
	    (c-progress-init start end 'c-indent-region)
	    (setq endmark (copy-marker end))
	    (while (and (bolp)
			(not (eobp))
			(< (point) endmark))
	      ;; update progress
	      (c-progress-update)
	      ;; Indent one line as with TAB.
	      (let (nextline sexpend sexpbeg)
		;; skip blank lines
		(skip-chars-forward " \t\n")
		(beginning-of-line)
		;; indent the current line
		(c-indent-line)
		(if (save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*#"))
		    (forward-line 1)
		  (save-excursion
		    ;; Find beginning of following line.
		    (setq nextline (c-point 'bonl))
		    ;; Find first beginning-of-sexp for sexp extending past
		    ;; this line.
		    (beginning-of-line)
		    (while (< (point) nextline)
		      (condition-case nil
			  (progn
			    (forward-sexp 1)
			    (setq sexpend (point)))
			(error (setq sexpend nil)
			       (goto-char nextline)))
		      (c-forward-syntactic-ws))
		    (if sexpend
			(progn 
			  ;; make sure the sexp we found really starts on the
			  ;; current line and extends past it
			  (goto-char sexpend)
			  (setq sexpend (point-marker))
			  (c-safe (backward-sexp 1))
			  (setq sexpbeg (point)))))
		  ;; check to see if the next line starts a
		  ;; comment-only line
		  (save-excursion
		    (forward-line 1)
		    (skip-chars-forward " \t")
		    (if (looking-at c-comment-start-regexp)
			(setq sexpbeg (c-point 'bol))))
		  ;; If that sexp ends within the region, indent it all at
		  ;; once, fast.
		  (condition-case nil
		      (if (and sexpend
			       (> sexpend nextline)
			       (<= sexpend endmark))
			  (progn
			    (goto-char sexpbeg)
			    (c-indent-exp 'shutup)
			    (c-progress-update)
			    (goto-char sexpend)))
		    (error
		     (goto-char sexpbeg)
		     (c-indent-line)))
		  ;; Move to following line and try again.
		  (and sexpend
		       (markerp sexpend)
		       (set-marker sexpend nil))
		  (forward-line 1)))))
	(set-marker endmark nil)
	(c-progress-fini 'c-indent-region)
	))))

(defun c-mark-function ()
  "Put mark at end of a C, C++, or Objective-C defun, point at beginning."
  (interactive)
  (let ((here (point))
	;; there should be a c-point position for 'eod
	(eod  (save-excursion (end-of-defun) (point)))
	(state (c-parse-state))
	brace)
    (while state
      (setq brace (car state))
      (if (consp brace)
	  (goto-char (cdr brace))
	(goto-char brace))
      (setq state (cdr state)))
    (if (= (following-char) ?{)
	(progn
	  (forward-line -1)
	  (while (not (or (bobp)
			  (looking-at "[ \t]*$")))
	    (forward-line -1)))
      (forward-line 1)
      (skip-chars-forward " \t\n"))
    (push-mark here)
    (push-mark eod nil t)))


;; for progress reporting
(defvar c-progress-info nil)

(defun c-progress-init (start end context)
  ;; start the progress update messages.  if this emacs doesn't have a
  ;; built-in timer, just be dumb about it
  (if (not (fboundp 'current-time))
      (message "indenting region... (this may take a while)")
    ;; if progress has already been initialized, do nothing. otherwise
    ;; initialize the counter with a vector of:
    ;; [start end lastsec context]
    (if c-progress-info
	()
      (setq c-progress-info (vector start
				    (save-excursion
				      (goto-char end)
				      (point-marker))
				    (nth 1 (current-time))
				    context))
      (message "indenting region..."))))

(defun c-progress-update ()
  ;; update progress
  (if (not (and c-progress-info c-progress-interval))
      nil
    (let ((now (nth 1 (current-time)))
	  (start (aref c-progress-info 0))
	  (end (aref c-progress-info 1))
	  (lastsecs (aref c-progress-info 2)))
      ;; should we update?  currently, update happens every 2 seconds,
      ;; what's the right value?
      (if (< c-progress-interval (- now lastsecs))
	  (progn
	    (message "indenting region... (%d%% complete)"
		     (/ (* 100 (- (point) start)) (- end start)))
	    (aset c-progress-info 2 now)))
      )))

(defun c-progress-fini (context)
  ;; finished
  (if (or (eq context (aref c-progress-info 3))
	  (eq context t))
      (progn
	(set-marker (aref c-progress-info 1) nil)
	(setq c-progress-info nil)
	(message "indenting region...done"))))


;; Skipping of "syntactic whitespace" for Emacs 19.  Syntactic
;; whitespace is defined as lexical whitespace, C and C++ style
;; comments, and preprocessor directives.  Search no farther back or
;; forward than optional LIM.  If LIM is omitted, `beginning-of-defun'
;; is used for backward skipping, point-max is used for forward
;; skipping.  Note that Emacs 18 support has been moved to cc-mode-18.el.

(defun c-forward-syntactic-ws (&optional lim)
  ;; Forward skip of syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum)
	;; skip preprocessor directives
	(if (and (= (following-char) ?#)
		 (= (c-point 'boi) (point)))
	    (end-of-line)
	  )))))

(defun c-backward-syntactic-ws (&optional lim)
  ;; Backward skip over syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (c-point 'bod)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)
	      (if (eq (c-in-literal lim) 'pound)
		  (beginning-of-line))
	      )))
      )))


;; Return `c' if in a C-style comment, `c++' if in a C++ style
;; comment, `string' if in a string literal, `pound' if on a
;; preprocessor line, or nil if not in a comment at all.  Optional LIM
;; is used as the backward limit of the search.  If omitted, or nil,
;; `beginning-of-defun' is used."

;; This is for all v19 Emacsen supporting either 1-bit or 8-bit syntax
(defun c-in-literal (&optional lim)
  ;; Determine if point is in a C++ literal. we cache the last point
  ;; calculated if the cache is enabled
  (if (and (boundp 'c-in-literal-cache)
	   c-in-literal-cache
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    (let ((rtn (save-excursion
		 (let* ((lim (or lim (c-point 'bod)))
			(here (point))
			(state (parse-partial-sexp lim (point))))
		   (cond
		    ((nth 3 state) 'string)
		    ((nth 4 state) (if (nth 7 state) 'c++ 'c))
		    ((progn
		       (goto-char here)
		       (beginning-of-line)
		       (looking-at "[ \t]*#"))
		     'pound)
		    (t nil))))))
      ;; cache this result if the cache is enabled
      (and (boundp 'c-in-literal-cache)
	   (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))


;; utilities for moving and querying around syntactic elements
(defun c-parse-state ()
  ;; Finds and records all open parens between some important point
  ;; earlier in the file and point.
  ;;
  ;; if there's a state cache, return it
  (if (boundp 'c-state-cache) c-state-cache
    (let* (at-bob
	   (pos (save-excursion
		  ;; go back 2 bods, but ignore any bogus positions
		  ;; returned by beginning-of-defun (i.e. open paren
		  ;; in column zero)
		  (let ((cnt 2))
		    (while (not (or at-bob (zerop cnt)))
		      (beginning-of-defun)
		      (if (= (following-char) ?\{)
			  (setq cnt (1- cnt)))
		      (if (bobp)
			  (setq at-bob t))))
		  (point)))
	   (here (save-excursion
		   ;;(skip-chars-forward " \t}")
		   (point)))
	   (last-bod pos) (last-pos pos)
	   placeholder state sexp-end)
      ;; cache last bod position
      (while (catch 'backup-bod
	       (setq state nil)
	       (while (and pos (< pos here))
		 (setq last-pos pos)
		 (if (and (setq pos (c-safe (scan-lists pos 1 -1)))
			  (<= pos here))
		     (progn
		       (setq sexp-end (c-safe (scan-sexps (1- pos) 1)))
		       (if (and sexp-end
				(<= sexp-end here))
			   ;; we want to record both the start and end
			   ;; of this sexp, but we only want to record
			   ;; the last-most of any of them before here
			   (progn
			     (if (= (char-after (1- pos)) ?\{)
				 (setq state (cons (cons (1- pos) sexp-end)
						   (if (consp (car state))
						       (cdr state)
						     state))))
			     (setq pos sexp-end))
			 ;; we're contained in this sexp so put pos on
			 ;; front of list
			 (setq state (cons (1- pos) state))))
		   ;; something bad happened. check to see if we
		   ;; crossed an unbalanced close brace. if so, we
		   ;; didn't really find the right `important bufpos'
		   ;; so lets back up and try again
		   (if (and (not pos) (not at-bob)
			    (setq placeholder
				  (c-safe (scan-lists last-pos 1 1)))
			    ;;(char-after (1- placeholder))
			    (<= placeholder here)
			    (= (char-after (1- placeholder)) ?\}))
		       (while t
			 (setq last-bod (c-safe (scan-lists last-bod -1 1)))
			 (if (not last-bod)
			     (error "unbalanced close brace at position %d"
				    (1- placeholder))
			   (setq at-bob (= last-bod (point-min))
				 pos last-bod)
			   (if (= (char-after last-bod) ?\{)
			       (throw 'backup-bod t)))
			 ))		;end-if
		   ))			;end-while
	       nil))
      state)))

(defun c-whack-state (bufpos state)
  ;; whack off any state information that appears on STATE which lies
  ;; after the bounds of BUFPOS.
  (let (newstate car)
    (while state
      (setq car (car state)
	    state (cdr state))
      (if (consp car)
	  ;; just check the car, because in a balanced brace
	  ;; expression, it must be impossible for the corresponding
	  ;; close brace to be before point, but the open brace to be
	  ;; after.
	  (if (<= bufpos (car car))
	      nil			; whack it off
	    ;; its possible that the open brace is before bufpos, but
	    ;; the close brace is after.  In that case, convert this
	    ;; to a non-cons element.
	    (if (<= bufpos (cdr car))
		(setq newstate (append newstate (list (car car))))
	      ;; we know that both the open and close braces are
	      ;; before bufpos, so we also know that everything else
	      ;; on state is before bufpos, so we can glom up the
	      ;; whole thing and exit.
	      (setq newstate (append newstate (list car) state)
		    state nil)))
	(if (<= bufpos car)
	    nil				; whack it off
	  ;; it's before bufpos, so everything else should too
	  (setq newstate (append newstate (list car) state)
		state nil))))
    newstate))

(defun c-hack-state (bufpos which state)
  ;; Using BUFPOS buffer position, and WHICH (must be 'open or
  ;; 'close), hack the c-parse-state STATE and return the results.
  (if (eq which 'open)
      (let ((car (car state)))
	(if (or (null car)
		(consp car)
		(/= bufpos car))
	    (cons bufpos state)
	  state))
    (if (not (eq which 'close))
	(error "c-hack-state, bad argument: %s" which))
    ;; 'close brace
    (let ((car (car state))
	  (cdr (cdr state)))
      (if (consp car)
	  (setq car (car cdr)
		cdr (cdr cdr)))
      ;; TBD: is this test relevant???
      (if (consp car)
	  state				;on error, don't change
	;; watch out for balanced expr already on cdr of list
	(cons (cons car bufpos)
	      (if (consp (car cdr))
		  (cdr cdr) cdr))
	))))

(defun c-adjust-state (from to shift state)
  ;; Adjust all points in state that lie in the region FROM..TO by
  ;; SHIFT amount (as would be returned by c-indent-line).
  (mapcar
   (function
    (lambda (e)
      (if (consp e)
	  (let ((car (car e))
		(cdr (cdr e)))
	    (if (and (<= from car) (< car to))
		(setcar e (+ shift car)))
	    (if (and (<= from cdr) (< cdr to))
		(setcdr e (+ shift cdr))))
	(if (and (<= from e) (< e to))
	    (setq e (+ shift e))))
      e))
   state))


(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let ((lim (or lim (c-point 'bod)))
	(placeholder (progn
		       (back-to-indentation)
		       (point))))
    (c-backward-syntactic-ws lim)
    (while (and (> (point) lim)
		(memq (preceding-char) '(?, ?:))
		(progn
		  (beginning-of-line)
		  (setq placeholder (point))
		  (skip-chars-forward " \t")
		  (not (looking-at c-class-key))
		  ))
      (c-backward-syntactic-ws lim))
    (goto-char placeholder)
    (skip-chars-forward "^:" (c-point 'eol))))

(defun c-beginning-of-macro (&optional lim)
  ;; Go to the beginning of the macro. Right now we don't support
  ;; multi-line macros too well
  (back-to-indentation))

(defun c-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  (save-excursion
    (beginning-of-line)
    (and c-method-key
	 (looking-at c-method-key)
	 (point))
    ))

(defun c-just-after-func-arglist-p (&optional containing)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.
  (save-excursion
    (c-backward-syntactic-ws)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (= (preceding-char) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>"))
	  (c-backward-syntactic-ws)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(if (and (= (preceding-char) ?:)
		 (progn
		   (forward-char -1)
		   (c-backward-syntactic-ws)
		   (looking-at "[ \t\n]*:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (and (= (preceding-char) ?\))
	   ;; check if we are looking at a method def
	   (or (not c-method-key)
	       (progn
		 (forward-sexp -1)
		 (forward-char -1)
		 (c-backward-syntactic-ws)
		 (not (or (= (preceding-char) ?-)
			  (= (preceding-char) ?+)
			  ;; or a class category
			  (progn
			    (forward-sexp -2)
			    (looking-at c-class-key))
			  )))))
      )))

;; defuns to look backwards for things
(defun c-backward-to-start-of-do (&optional lim)
  ;; Move to the start of the last "unbalanced" do expression.
  ;; Optional LIM is the farthest back to search.  If none is found,
  ;; nil is returned and point is left unchanged, otherwise t is returned.
  (let ((do-level 1)
	(case-fold-search nil)
	(lim (or lim (c-point 'bod)))
	(here (point))
	foundp)
    (while (not (zerop do-level))
      ;; we protect this call because trying to execute this when the
      ;; while is not associated with a do will throw an error
      (condition-case nil
	  (progn
	    (backward-sexp 1)
	    (cond
	     ((memq (c-in-literal lim) '(c c++)))
	     ((looking-at "while\\b[^_]")
	      (setq do-level (1+ do-level)))
	     ((looking-at "do\\b[^_]")
	      (if (zerop (setq do-level (1- do-level)))
		  (setq foundp t)))
	     ((<= (point) lim)
	      (setq do-level 0)
	      (goto-char lim))))
	(error
	 (goto-char lim)
	 (setq do-level 0))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun c-backward-to-start-of-if (&optional lim)
  ;; Move to the start of the last "unbalanced" if and return t.  If
  ;; none is found, and we are looking at an if clause, nil is
  ;; returned.  If none is found and we are looking at an else clause,
  ;; an error is thrown.
  (let ((if-level 1)
	(here (c-point 'bol))
	(case-fold-search nil)
	(lim (or lim (c-point 'bod)))
	(at-if (looking-at "if\\b[^_]")))
    (catch 'orphan-if
      (while (and (not (bobp))
		  (not (zerop if-level)))
	(c-backward-syntactic-ws)
	(condition-case nil
	    (backward-sexp 1)
	  (error
	   (if at-if
	       (throw 'orphan-if nil)
	     (error "No matching `if' found for `else' on line %d."
		    (1+ (count-lines 1 here))))))
	(cond
	 ((looking-at "else\\b[^_]")
	  (setq if-level (1+ if-level)))
	 ((looking-at "if\\b[^_]")
	  ;; check for else if... skip over
	  (let ((here (point)))
	    (c-safe (forward-sexp -1))
	    (if (looking-at "\\<else\\>[ \t]+\\<if\\>")
		nil
	      (setq if-level (1- if-level))
	      (goto-char here))))
	 ((< (point) lim)
	  (setq if-level 0)
	  (goto-char lim))
	 ))
      t)))

(defun c-skip-conditional ()
  ;; skip forward over conditional at point, including any predicate
  ;; statements in parentheses. No error checking is performed.
  (forward-sexp
   ;; else if()
   (if (looking-at "\\<else\\>[ \t]+\\<if\\>")
       3
     ;; do and else aren't followed by parens
     (if (looking-at "\\<\\(do\\|else\\)\\>")
	 1 2))))

(defun c-skip-case-statement-forward (state &optional lim)
  ;; skip forward over case/default bodies, with optional maximal
  ;; limit. if no next case body is found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp bufpos
	(safepos (point))
	(balanced (car state)))
    ;; search until we've passed the limit, or we've found our match
    (while (and (< (point) lim)
		(not donep))
      (setq safepos (point))
      ;; see if we can find a case statement, not in a literal
      (if (and (re-search-forward c-switch-label-key lim 'move)
	       (setq bufpos (match-beginning 0))
	       (not (c-in-literal safepos))
	       (/= bufpos here))
	  ;; if we crossed into a balanced sexp, we know the case is
	  ;; not part of our switch statement, so just bound over the
	  ;; sexp and keep looking.
	  (if (and (consp balanced)
		   (> bufpos (car balanced))
		   (< bufpos (cdr balanced)))
	      (goto-char (cdr balanced))
	    (goto-char bufpos)
	    (setq donep t
		  foundp t))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun c-search-uplist-for-classkey (brace-state)
  ;; search for the containing class, returning a 2 element vector if
  ;; found. aref 0 contains the bufpos of the class key, and aref 1
  ;; contains the bufpos of the open brace.
  (if (null brace-state)
      ;; no brace-state means we cannot be inside a class
      nil
    (let ((carcache (car brace-state))
	  search-start search-end)
      (if (consp carcache)
	  ;; a cons cell in the first element means that there is some
	  ;; balanced sexp before the current bufpos. this we can
	  ;; ignore. the nth 1 and nth 2 elements define for us the
	  ;; search boundaries
	  (setq search-start (nth 2 brace-state)
		search-end (nth 1 brace-state))
	;; if the car was not a cons cell then nth 0 and nth 1 define
	;; for us the search boundaries
	(setq search-start (nth 1 brace-state)
	      search-end (nth 0 brace-state)))
      ;; search-end cannot be a cons cell
      (and (consp search-end)
	   (error "consp search-end: %s" search-end))
      ;; if search-end is nil, or if the search-end character isn't an
      ;; open brace, we are definitely not in a class
      (if (or (not search-end)
	      (< search-end (point-min))
	      (/= (char-after search-end) ?{))
	  nil
	;; now, we need to look more closely at search-start.  if
	;; search-start is nil, then our start boundary is really
	;; point-min.
	(if (not search-start)
	    (setq search-start (point-min))
	  ;; if search-start is a cons cell, then we can start
	  ;; searching from the end of the balanced sexp just ahead of
	  ;; us
	  (if (consp search-start)
	      (setq search-start (cdr search-start))))
	;; now we can do a quick regexp search from search-start to
	;; search-end and see if we can find a class key.  watch for
	;; class like strings in literals
	(save-excursion
	  (save-restriction
	    (goto-char search-start)
	    (let (foundp class match-end)
	      (while (and (not foundp)
			  (progn
			    (c-forward-syntactic-ws)
			    (> search-end (point)))
			  (re-search-forward c-class-key search-end t))
		(setq class (match-beginning 0)
		      match-end (match-end 0))
		(if (c-in-literal search-start)
		    nil			; its in a comment or string, ignore
		  (goto-char class)
		  (skip-chars-forward " \t\n")
		  (setq foundp (vector (c-point 'boi) search-end))
		  (cond
		   ;; check for embedded keywords
		   ((let ((char (char-after (1- class))))
		      (and char
			   (memq (char-syntax char) '(?w ?_))))
		    (goto-char match-end)
		    (setq foundp nil))
		   ;; make sure we're really looking at the start of a
		   ;; class definition, and not a forward decl, return
		   ;; arg, template arg list, or an ObjC or Java method.
		   ((and c-method-key
			 (re-search-forward c-method-key search-end t))
		    (setq foundp nil))
		   ;; Its impossible to define a regexp for this, and
		   ;; nearly so to do it programmatically.
		   ;;
		   ;; ; picks up forward decls
		   ;; = picks up init lists
		   ;; ) picks up return types
		   ;; > picks up templates, but remember that we can
		   ;;   inherit from templates!
		   ((let ((skipchars "^;=)"))
		      ;; try to see if we found the `class' keyword
		      ;; inside a template arg list
		      (save-excursion
			(skip-chars-backward "^<>" search-start)
			(if (= (preceding-char) ?<)
			    (setq skipchars (concat skipchars ">"))))
		      (skip-chars-forward skipchars search-end)
		      (/= (point) search-end))
		    (setq foundp nil))
		   )))
	      foundp))
	  )))))

(defun c-inside-bracelist-p (containing-sexp brace-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren. BRACE-STATE is the remainder of the state of enclosing braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; places in inconvenient locations.  Its a trade-off we make for
  ;; speed.
  (or
   ;; this will pick up enum lists
   (condition-case ()
       (save-excursion
	 (goto-char containing-sexp)
	 (forward-sexp -1)
	 (if (or (looking-at "enum[\t\n ]+")
		 (progn (forward-sexp -1)
			(looking-at "enum[\t\n ]+")))
	     (point)))
     (error nil))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let (bufpos failedp)
       (while (and (not bufpos)
		   containing-sexp)
	 (if (consp containing-sexp)
	     (setq containing-sexp (car brace-state)
		   brace-state (cdr brace-state))
	   ;; see if significant character just before brace is an equal
	   (goto-char containing-sexp)
	   (setq failedp nil)
	   (condition-case ()
	       (progn
		 (forward-sexp -1)
		 (forward-sexp 1)
		 (c-forward-syntactic-ws containing-sexp))
	     (error (setq failedp t)))
	   (if (or failedp (/= (following-char) ?=))
	       ;; lets see if we're nested. find the most nested
	       ;; containing brace
	       (setq containing-sexp (car brace-state)
		     brace-state (cdr brace-state))
	     ;; we've hit the beginning of the aggregate list
	     (c-beginning-of-statement-1 (c-most-enclosing-brace brace-state))
	     (setq bufpos (point)))
	   ))
       bufpos))
   ))


;; defuns for calculating the syntactic state and indenting a single
;; line of C/C++/ObjC code
(defmacro c-add-syntax (symbol &optional relpos)
  ;; a simple macro to append the syntax in symbol to the syntax list.
  ;; try to increase performance by using this macro
  (` (setq syntax (cons (cons (, symbol) (, relpos)) syntax))))

(defun c-most-enclosing-brace (state)
  ;; return the bufpos of the most enclosing brace that hasn't been
  ;; narrowed out by any enclosing class, or nil if none was found
  (let (enclosingp)
    (while (and state (not enclosingp))
      (setq enclosingp (car state)
	    state (cdr state))
      (if (consp enclosingp)
	  (setq enclosingp nil)
	(if (> (point-min) enclosingp)
	    (setq enclosingp nil))
	(setq state nil)))
    enclosingp))

(defun c-least-enclosing-brace (state)
  ;; return the bufpos of the least (highest) enclosing brace that
  ;; hasn't been narrowed out by any enclosing class, or nil if none
  ;; was found.
  (c-most-enclosing-brace (nreverse state)))

(defun c-safe-position (bufpos state)
  ;; return the closest known safe position higher up than point
  (let ((safepos nil))
    (while state
      (setq safepos
	    (if (consp (car state))
		(cdr (car state))
	      (car state)))
      (if (< safepos bufpos)
	  (setq state nil)
	(setq state (cdr state))))
    safepos))

(defun c-narrow-out-enclosing-class (state lim)
  ;; narrow the buffer so that the enclosing class is hidden
  (let (inclass-p)
    (and state
	 (setq inclass-p (c-search-uplist-for-classkey state))
	 (narrow-to-region
	  (progn
	    (goto-char (1+ (aref inclass-p 1)))
	    (skip-chars-forward " \t\n" lim)
	    ;; if point is now left of the class opening brace, we're
	    ;; hosed, so try a different tact
	    (if (<= (point) (aref inclass-p 1))
		(progn
		  (goto-char (1+ (aref inclass-p 1)))
		  (c-forward-syntactic-ws lim)))
	    (point))
	  ;; end point is the end of the current line
	  (progn
	    (goto-char lim)
	    (c-point 'eol))))
    ;; return the class vector
    inclass-p))

(defun c-guess-basic-syntax ()
  ;; guess the syntactic description of the current line of C++ code.
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search nil)
	     (fullstate (c-parse-state))
	     (state fullstate)
	     (in-method-intro-p (and c-method-key
				     (looking-at c-method-key)))
	     literal containing-sexp char-before-ip char-after-ip lim
	     syntax placeholder c-in-literal-cache inswitch-p
	     ;; narrow out any enclosing class
	     (inclass-p (c-narrow-out-enclosing-class state indent-point))
	     )

	;; get the buffer position of the most nested opening brace,
	;; if there is one, and it hasn't been narrowed out
	(save-excursion
	  (goto-char indent-point)
	  (skip-chars-forward " \t}")
	  (skip-chars-backward " \t")
	  (while (and state
		      (not in-method-intro-p)
		      (not containing-sexp))
	    (setq containing-sexp (car state)
		  state (cdr state))
	    (if (consp containing-sexp)
		;; if cdr == point, then containing sexp is the brace
		;; that opens the sexp we close
		(if (= (cdr containing-sexp) (point))
		    (setq containing-sexp (car containing-sexp))
		  ;; otherwise, ignore this element
		  (setq containing-sexp nil))
	      ;; ignore the bufpos if its been narrowed out by the
	      ;; containing class
	      (if (<= containing-sexp (point-min))
		  (setq containing-sexp nil)))))

	;; set the limit on the farthest back we need to search
	(setq lim (or containing-sexp
		      (if (consp (car fullstate))
			  (cdr (car fullstate))
			nil)
		      (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (following-char))
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((memq literal '(string))
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  ;; we need to catch multi-paragraph C comments
	  (while (and (zerop (forward-line -1))
		      (looking-at "^[ \t]*$")))
	  (c-add-syntax literal (c-point 'bol)))
	 ;; CASE 3: in a cpp preprocessor
	 ((eq literal 'pound)
	  (c-beginning-of-macro lim)
	  (c-add-syntax 'cpp-macro (c-point 'boi)))
	 ;; CASE 4: in an objective-c method intro
	 (in-method-intro-p
	  (c-add-syntax 'objc-method-intro (c-point 'boi)))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, class, or
	   ;; inline-inclass method opening brace
	   ((= char-after-ip ?{)
	    (cond
	     ;; CASE 5A.1: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		;; TBD: watch out! there could be a bogus
		;; c-state-cache in place when we get here.  we have
		;; to go through much chicanery to ignore the cache.
		;; But of course, there may not be!  BLECH!  BOGUS!
		(let ((decl
		       (if (boundp 'c-state-cache)
			   (let ((old-cache c-state-cache))
			     (prog2
				 (makunbound 'c-state-cache)
				 (c-search-uplist-for-classkey (c-parse-state))
			       (setq c-state-cache old-cache)))
			 (c-search-uplist-for-classkey (c-parse-state))
			 )))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.2: brace list open
	     ((save-excursion
		(c-beginning-of-statement-1 lim)
		;; c-b-o-s could have left us at point-min
		(and (bobp)
		     (c-forward-syntactic-ws indent-point))
		(setq placeholder (point))
		(and (or (looking-at "enum[ \t\n]+")
			 (= char-before-ip ?=))
		     (save-excursion
		       (skip-chars-forward "^;(" indent-point)
		       (not (memq (following-char) '(?\; ?\()))
		       )))
	      (c-add-syntax 'brace-list-open placeholder))
	     ;; CASE 5A.3: inline defun open
	     (inclass-p
	      (c-add-syntax 'inline-open (aref inclass-p 0)))
	     ;; CASE 5A.4: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (c-add-syntax 'defun-open (c-point 'bol))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (= char-before-ip ?:)
		  (= char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (= char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (= (preceding-char) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     (c-recognize-knr-p
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ;; CASE 5B.3: Nether region after a C++ func decl, which
	     ;; could include a `throw' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'ansi-funcdecl-cont (c-point 'boi))
	      )))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and c-baseclass-key (looking-at c-baseclass-key))
		(and (or (= char-before-ip ?:)
			 (= char-after-ip ?:))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (= char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key))))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((= char-after-ip ?:)
	      (c-backward-syntactic-ws lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((= char-before-ip ?:)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ;; CASE 5C.3: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level compound statement or a
	   ;; member init list continuation
	   ((= char-before-ip ?,)
	    (goto-char indent-point)
	    (c-backward-syntactic-ws lim)
	    (while (and (< lim (point))
			(= (preceding-char) ?,))
	      ;; this will catch member inits with multiple
	      ;; line arglists
	      (forward-char -1)
	      (c-backward-syntactic-ws (c-point 'bol))
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      ;; now continue checking
	      (beginning-of-line)
	      (c-backward-syntactic-ws lim))
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and (= (preceding-char) ?:)
		   (save-excursion
		     (forward-word -1)
		     (not (looking-at c-access-key))))
	      (goto-char indent-point)
	      (c-backward-syntactic-ws lim)
	      (c-safe (backward-sexp 1))
	      (c-add-syntax 'member-init-cont (c-point 'boi))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(= (following-char) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a multiple inheritance line?
	     ((looking-at c-inher-key)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a template list continuation?
	     ((save-excursion
		(skip-chars-backward "^<" lim)
		;; not sure if this is the right test, but it should
		;; be fast and mostly accurate.
		(and (= (preceding-char) ?<)
		     (not (c-in-literal lim))))
	      ;; we can probably indent it just like and arglist-cont
	      (c-add-syntax 'arglist-cont (point)))
	     ;; CASE 5D.5: perhaps a top-level statement-cont
	     (t
	      (c-beginning-of-statement-1 lim)
	      ;; skip over any access-specifiers
	      (and inclass-p c-access-key
		   (while (looking-at c-access-key)
		     (forward-line 1)))
	      ;; skip over comments, whitespace
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'statement-cont (c-point 'boi)))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-access-key
		 (looking-at c-access-key))
	    (c-add-syntax 'access-label (c-point 'bonl))
	    (c-add-syntax 'inclass (aref inclass-p 0)))
	   ;; CASE 5F: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (= char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and
		      (condition-case nil
			  (progn (backward-sexp 1) t)
			(error nil))
		      (= (point) (aref inclass-p 1))
		      ))))
	    (save-restriction
	      (widen)
	      (goto-char (aref inclass-p 0))
	      (c-add-syntax 'class-close (c-point 'boi))))
	   ;; CASE 5G: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 (save-excursion
		   (c-backward-syntactic-ws lim)
		   (while (memq (preceding-char) '(?\; ?,))
		     (beginning-of-line)
		     (setq placeholder (point))
		     (c-backward-syntactic-ws lim))
		   (and (= (preceding-char) ?\))
			(or (not c-method-key)
			    (progn
			      (forward-sexp -1)
			      (forward-char -1)
			      (c-backward-syntactic-ws)
			      (not (or (= (preceding-char) ?-)
				       (= (preceding-char) ?+)
				       ;; or a class category
				       (progn
					 (forward-sexp -2)
					 (looking-at c-class-key))
				       )))))
		   )
		 (save-excursion
		   (c-beginning-of-statement-1)
		   (not (looking-at "typedef[ \t\n]+"))))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (c-point 'boi)))
	   ;; CASE 5H: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (while (and inclass-p
			  c-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (backward-sexp 1) t))
			    (looking-at c-access-key)))
		(backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (preceding-char) '(?\; ?\}))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (c-add-syntax 'inclass (c-point 'boi))))))
	   ;; CASE 5I: we are at an ObjC or Java method definition
	   ;; continuation line.
	   ((and c-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5J: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))				; end CASE 5
	 ;; CASE 6: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((/= (char-after containing-sexp) ?{)
	  (c-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 6A: we are looking at the arglist closing paren or
	   ;; at an Objective-C or Java method call closing bracket.
	   ((and (/= char-before-ip ?,)
		 (memq char-after-ip '(?\) ?\])))
	    (if (and c-method-key
		     (progn
		       (goto-char (1- containing-sexp))
		       (c-backward-syntactic-ws lim)
		       (not (looking-at c-symbol-key))))
		(c-add-syntax 'statement-cont containing-sexp)
	      (goto-char containing-sexp)
	      (c-add-syntax 'arglist-close (c-point 'boi))))
	   ;; CASE 6B: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (c-add-syntax 'arglist-intro (c-point 'boi)))
	   ;; CASE 6C: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((save-excursion
	     (goto-char containing-sexp)
	     (and (c-safe (progn (forward-sexp -1) t))
		  (looking-at "\\<for\\>")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (c-beginning-of-statement-1 containing-sexp)
	    (if (= char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 6D: maybe a continued method call. This is the case
	   ;; when we are inside a [] bracketed exp, and what precede
	   ;; the opening bracket is not an identifier.
	   ((and c-method-key
		 (= (char-after containing-sexp) ?\[)
		 (save-excursion
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 6E: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp)))
		 (save-excursion
		   (c-beginning-of-statement-1 lim)
		   (skip-chars-backward " \t([")
		   (<= (point) containing-sexp)))
	    (goto-char containing-sexp)
	    (c-add-syntax 'arglist-cont-nonempty (c-point 'boi)))
	   ;; CASE 6F: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-beginning-of-statement-1 containing-sexp)
	      (forward-char 1)
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 7: func-local multi-inheritance line
	 ((and c-baseclass-key
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-baseclass-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 7A: non-hanging colon on an inher intro
	   ((= char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 7B: hanging colon on an inher intro
	   ((= char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 7C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 8: we are inside a brace-list
	 ((setq placeholder (c-inside-bracelist-p containing-sexp state))
	  (cond
	   ;; CASE 8A: brace-list-close brace
	   ((and (= char-after-ip ?})
		 (c-safe (progn (forward-char 1)
				(backward-sexp 1)
				t))
		 (= (point) containing-sexp))
	    (c-add-syntax 'brace-list-close (c-point 'boi)))
	   ;; CASE 8B: we're looking at the first line in a brace-list
	   ((save-excursion
	      (goto-char indent-point)
	      (c-backward-syntactic-ws containing-sexp)
	      (= (point) (1+ containing-sexp)))
	    (goto-char containing-sexp)
	    ;;(if (= char-after-ip ?{)
		;;(c-add-syntax 'brace-list-open (c-point 'boi))
	    (c-add-syntax 'brace-list-intro (c-point 'boi))
	    )
	    ;;))			; end CASE 8B
	   ;; CASE 8C: this is just a later brace-list-entry
	   (t (goto-char (1+ containing-sexp))
	      (c-forward-syntactic-ws indent-point)
	      (if (= char-after-ip ?{)
		  (c-add-syntax 'brace-list-open (point))
		(c-add-syntax 'brace-list-entry (point))
		))			; end CASE 8C
	   ))				; end CASE 8
	 ;; CASE 9: A continued statement
	 ((and (not (memq char-before-ip '(?\; ?} ?:)))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (let ((after-cond-placeholder
		 (save-excursion
		   (goto-char placeholder)
		   (if (looking-at c-conditional-key)
		       (progn
			 (c-safe (c-skip-conditional))
			 (c-forward-syntactic-ws)
			 (if (memq (following-char) '(?\;))
			     (progn
			       (forward-char 1)
			       (c-forward-syntactic-ws)))
			 (point))
		     nil))))
	    (cond
	     ;; CASE 9A: substatement
	     ((and after-cond-placeholder
		   (>= after-cond-placeholder indent-point))
	      (goto-char placeholder)
	      (if (= char-after-ip ?{)
		  (c-add-syntax 'substatement-open (c-point 'boi))
		(c-add-syntax 'substatement (c-point 'boi))))
	     ;; CASE 9B: open braces for class or brace-lists
	     ((= char-after-ip ?{)
	      (cond
	       ;; CASE 9B.1: class-open
	       ((save-excursion
		  (goto-char indent-point)
		  (skip-chars-forward " \t{")
		  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		    (and decl
			 (setq placeholder (aref decl 0)))
		    ))
		(c-add-syntax 'class-open placeholder))
	       ;; CASE 9B.2: brace-list-open
	       ((or (save-excursion
		      (goto-char placeholder)
		      (looking-at "\\<enum\\>"))
		    (= char-before-ip ?=))
		(c-add-syntax 'brace-list-open placeholder))
	       ;; CASE 9B.3: catch-all for unknown construct.
	       (t
		;; Can and should I add an extensibility hook here?
		;; Something like c-recognize-hook so support for
		;; unknown constructs could be added.  It's probably a
		;; losing proposition, so I dunno.
		(goto-char placeholder)
		(c-add-syntax 'statement-cont (c-point 'boi))
		(c-add-syntax 'block-open))
	       ))
	     ;; CASE 9C: iostream insertion or extraction operator
	     ((looking-at "<<\\|>>")
	      (goto-char placeholder)
	      (and after-cond-placeholder
		   (goto-char after-cond-placeholder))
	      (while (and (re-search-forward "<<\\|>>" indent-point 'move)
			  (c-in-literal placeholder)))
	      ;; if we ended up at indent-point, then the first
	      ;; streamop is on a separate line. Indent the line like
	      ;; a statement-cont instead
	      (if (/= (point) indent-point)
		  (c-add-syntax 'stream-op (c-point 'boi))
		(c-backward-syntactic-ws lim)
		(c-add-syntax 'statement-cont (c-point 'boi))))
	     ;; CASE 9D: continued statement. find the accurate
	     ;; beginning of statement or substatement
	     (t
	      (c-beginning-of-statement-1 after-cond-placeholder)
	      ;; KLUDGE ALERT!  c-beginning-of-statement-1 can leave
	      ;; us before the lim we're passing in.  It should be
	      ;; fixed, but I'm worried about side-effects at this
	      ;; late date.  Fix for v5.
	      (goto-char (or (and after-cond-placeholder
				  (max after-cond-placeholder (point)))
			     (point)))
	      (c-add-syntax 'statement-cont (point)))
	     )))
	 ;; CASE 10: an else clause?
	 ((looking-at "\\<else\\>[^_]")
	  (c-backward-to-start-of-if containing-sexp)
	  (c-add-syntax 'else-clause (c-point 'boi)))
	 ;; CASE 11: Statement. But what kind?  Lets see if its a
	 ;; while closure of a do/while construct
	 ((progn
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (and (looking-at "while\\b[^_]")
		 (save-excursion
		   (c-backward-to-start-of-do containing-sexp)
		   (setq placeholder (point))
		   (looking-at "do\\b[^_]"))
		 ))
	  (c-add-syntax 'do-while-closure placeholder))
	 ;; CASE 12: A case or default label
	 ((looking-at c-switch-label-key)
	  (goto-char containing-sexp)
	  ;; check for hanging braces
	  (if (/= (point) (c-point 'boi))
	      (forward-sexp -1))
	  (c-add-syntax 'case-label (c-point 'boi)))
	 ;; CASE 13: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (c-add-syntax 'label (c-point 'boi)))
	 ;; CASE 14: block close brace, possibly closing the defun or
	 ;; the class
	 ((= char-after-ip ?})
	  (let* ((lim (c-safe-position containing-sexp fullstate))
		 (relpos (save-excursion
			   (goto-char containing-sexp)
			   (if (/= (point) (c-point 'boi))
			       (c-beginning-of-statement-1 lim))
			   (c-point 'boi))))
	    (cond
	     ;; CASE 14A: does this close an inline?
	     ((progn
		(goto-char containing-sexp)
		(c-search-uplist-for-classkey state))
	      (c-add-syntax 'inline-close relpos))
	     ;; CASE 14B: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close
	     ((c-most-enclosing-brace state)
	      (c-add-syntax 'block-close relpos))
	     ;; CASE 14C: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-syntax 'class-close (aref decl 0))
		    (c-add-syntax 'defun-close relpos)))))
	     )))
	 ;; CASE 15: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (goto-char containing-sexp)
	  (forward-char 1)
	  (c-forward-syntactic-ws indent-point)
	  ;; now skip forward past any case/default clauses we might find.
	  (while (or (c-skip-case-statement-forward fullstate indent-point)
		     (and (looking-at c-switch-label-key)
			  (not inswitch-p)))
	    (setq inswitch-p t))
	  ;; we want to ignore non-case labels when skipping forward
	  (while (and (looking-at c-label-key)
		      (goto-char (match-end 0)))
	    (c-forward-syntactic-ws indent-point))
	  (cond
	   ;; CASE 15A: we are inside a case/default clause inside a
	   ;; switch statement.  find out if we are at the statement
	   ;; just after the case/default label.
	   ((and inswitch-p
		 (progn
		   (goto-char indent-point)
		   (c-backward-syntactic-ws containing-sexp)
		   (back-to-indentation)
		   (setq placeholder (point))
		   (looking-at c-switch-label-key)))
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (if (= (following-char) ?{)
		(c-add-syntax 'statement-case-open placeholder)
	      (c-add-syntax 'statement-case-intro placeholder)))
	   ;; CASE 15B: continued statement
	   ((= char-before-ip ?,)
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 15C: a question/colon construct?  But make sure
	   ;; what came before was not a label, and what comes after
	   ;; is not a globally scoped function call!
	   ((or (and (memq char-before-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (c-backward-syntactic-ws lim)
		       (back-to-indentation)
		       (not (looking-at c-label-key))))
		(and (memq char-after-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (skip-chars-forward " \t")
		       ;; watch out for scope operator
		       (not (looking-at "::")))))
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 15D: any old statement
	   ((< (point) indent-point)
	    (let ((safepos (c-most-enclosing-brace fullstate)))
	      (goto-char indent-point)
	      (c-beginning-of-statement-1 safepos)
	      ;; It is possible we're on the brace that opens a nested
	      ;; function.
	      (if (and (= (following-char) ?{)
		       (save-excursion
			 (c-backward-syntactic-ws safepos)
			 (/= (preceding-char) ?\;)))
		  (c-beginning-of-statement-1 safepos))
	      (c-add-syntax 'statement (c-point 'boi))
	      (if (= char-after-ip ?{)
		  (c-add-syntax 'block-open))))
	   ;; CASE 15E: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here!)
	   ((save-excursion
	      (save-restriction
		(widen)
		(goto-char containing-sexp)
		(c-narrow-out-enclosing-class state containing-sexp)
		(not (c-most-enclosing-brace state))))
	    (goto-char containing-sexp)
	    ;; if not at boi, then defun-opening braces are hung on
	    ;; right side, so we need a different relpos
	    (if (/= (point) (c-point 'boi))
		(progn
		  (c-backward-syntactic-ws)
		  (c-safe (forward-sexp (if (= (preceding-char) ?\))
					    -1 -2)))
		  ))
	    (c-add-syntax 'defun-block-intro (c-point 'boi)))
	   ;; CASE 15F: first statement in a block
	   (t (goto-char containing-sexp)
	      (if (/= (point) (c-point 'boi))
		  (c-beginning-of-statement-1
		   (if (= (point) lim)
		       (c-safe-position (point) state) lim)))
	      (c-add-syntax 'statement-block-intro (c-point 'boi))
	      (if (= char-after-ip ?{)
		  (c-add-syntax 'block-open)))
	   ))
	 )

	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(if (looking-at c-comment-start-regexp)
	    (c-add-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(if (and (eq major-mode 'c++-mode)
		 (looking-at c-C++-friend-key))
	    (c-add-syntax 'friend))
	;; return the syntax
	syntax))))


;; indent via syntactic language elements
(defun c-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; c-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if c-strict-syntax-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+)  (setq offset c-basic-offset))
     ((eq offset '-)  (setq offset (- c-basic-offset)))
     ((eq offset '++) (setq offset (* 2 c-basic-offset)))
     ((eq offset '--) (setq offset (* 2 (- c-basic-offset))))
     ((eq offset '*)  (setq offset (/ c-basic-offset 2)))
     ((eq offset '/)  (setq offset (/ (- c-basic-offset) 2)))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (c-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

(defun c-indent-line (&optional syntax)
  ;; indent the current line as C/C++/ObjC code. Optional SYNTAX is the
  ;; syntactic information for the current line. Returns the amount of
  ;; indentation change
  (let* ((c-syntactic-context (or syntax (c-guess-basic-syntax)))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'c-get-offset c-syntactic-context)))
	 (shift-amt  (- (current-indentation) indent)))
    (and c-echo-syntactic-information-p
	 (message "syntax: %s, indent= %d" c-syntactic-context indent))
    (if (zerop shift-amt)
	nil
      (delete-region (c-point 'bol) (c-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (c-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )
    (run-hooks 'c-special-indent-hook)
    shift-amt))

(defun c-show-syntactic-information ()
  "Show syntactic information for current line."
  (interactive)
  (message "syntactic analysis: %s" (c-guess-basic-syntax))
  (c-keep-region-active))


;; Standard indentation line-ups
(defun c-lineup-arglist (langelem)
  ;; lineup the current arglist line with the arglist appearing just
  ;; after the containing paren which starts the arglist.
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is c-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (c-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (cs-curcol (save-excursion
			(goto-char (cdr langelem))
			(current-column))))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (forward-sexp -1)
		 (forward-char 1)
		 (c-forward-syntactic-ws)
		 (- (current-column) cs-curcol))
	(goto-char containing-sexp)
	(or (eolp)
	    (not (memq (following-char) '(?{ ?\( )))
	    (let ((eol (c-point 'eol))
		  (here (progn
			  (forward-char 1)
			  (skip-chars-forward " \t")
			  (point))))
	      (c-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) cs-curcol)
	))))

(defun c-lineup-arglist-intro-after-paren (langelem)
  ;; lineup an arglist-intro line to just after the open paren
  (save-excursion
    (let ((cs-curcol (save-excursion
		       (goto-char (cdr langelem))
		       (current-column)))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (c-point 'eol))
		       (current-column))))
      (- ce-curcol cs-curcol -1))))

(defun c-lineup-streamop (langelem)
  ;; lineup stream operators
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (curcol (progn (goto-char relpos)
			  (current-column))))
      (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
      (goto-char (match-beginning 0))
      (- (current-column) curcol))))

(defun c-lineup-multi-inher (langelem)
  ;; line up multiple inheritance lines
  (save-excursion
    (let (cs-curcol
	  (eol (c-point 'eol))
	  (here (point)))
      (goto-char (cdr langelem))
      (setq cs-curcol (current-column))
      (skip-chars-forward "^:" eol)
      (skip-chars-forward " \t:" eol)
      (if (or (eolp)
	      (looking-at c-comment-start-regexp))
	  (c-forward-syntactic-ws here))
      (- (current-column) cs-curcol)
      )))

(defun c-lineup-C-comments (langelem)
  ;; line up C block comment continuation lines
  (save-excursion
    (let ((stars (progn
		   (beginning-of-line)
		   (skip-chars-forward " \t")
		   (if (looking-at "\\*\\*?")
		       (- (match-end 0) (match-beginning 0))
		     0)))
	  (cs-curcol (progn (goto-char (cdr langelem))
			    (current-column))))
      (back-to-indentation)
      (if (re-search-forward "/\\*[ \t]*" (c-point 'eol) t)
	  (goto-char (+ (match-beginning 0)
			(cond
			 (c-block-comments-indent-p 0)
			 ((= stars 1) 1)
			 ((= stars 2) 0)
			 (t (- (match-end 0) (match-beginning 0)))))))
      (- (current-column) cs-curcol))))

(defun c-lineup-comment (langelem)
  ;; support old behavior for comment indentation. we look at
  ;; c-comment-only-line-offset to decide how to indent comment
  ;; only-lines
  (save-excursion
    (back-to-indentation)
    ;; indent as specified by c-comment-only-line-offset
    (if (not (bolp))
	(or (car-safe c-comment-only-line-offset)
	    c-comment-only-line-offset)
      (or (cdr-safe c-comment-only-line-offset)
	  (car-safe c-comment-only-line-offset)
	  -1000				;jam it against the left side
	  ))))

(defun c-lineup-runin-statements (langelem)
  ;; line up statements in coding standards which place the first
  ;; statement on the same line as the block opening brace.
  (if (= (char-after (cdr langelem)) ?{)
      (save-excursion
	(let ((curcol (progn
			(goto-char (cdr langelem))
			(current-column))))
	  (forward-char 1)
	  (skip-chars-forward " \t")
	  (- (current-column) curcol)))
    0))

(defun c-lineup-math (langelem)
  ;; line up math statement-cont after the equals
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (equalp (save-excursion
		     (goto-char (c-point 'boi))
		     (skip-chars-forward "^=" (c-point 'eol))
		     (and (= (following-char) ?=)
			  (- (point) (c-point 'boi)))))
	   (curcol (progn
		     (goto-char relpos)
		     (current-column)))
	   donep)
      (while (and (not donep)
		  (< (point) (c-point 'eol)))
	(skip-chars-forward "^=" (c-point 'eol))
	(if (c-in-literal (cdr langelem))
	    (forward-char 1)
	  (setq donep t)))
      (if (/= (following-char) ?=)
	  ;; there's no equal sign on the line
	  c-basic-offset
	;; calculate indentation column after equals and ws, unless
	;; our line contains an equals sign
	(if (not equalp)
	    (progn
	      (forward-char 1)
	      (skip-chars-forward " \t")
	      (setq equalp 0)))
	(- (current-column) equalp curcol))
      )))

(defun c-lineup-ObjC-method-call (langelem)
  ;; Line up methods args as elisp-mode does with function args: go to
  ;; the position right after the message receiver, and if you are at
  ;; (eolp) indent the current line by a constant offset from the
  ;; opening bracket; otherwise we are looking at the first character
  ;; of the first method call argument, so lineup the current line
  ;; with it.
  (save-excursion
    (let* ((extra (save-excursion
		    (back-to-indentation)
		    (c-backward-syntactic-ws (cdr langelem))
		    (if (= (preceding-char) ?:)
			(- c-basic-offset)
		      0)))
	   (open-bracket-pos (cdr langelem))
           (open-bracket-col (progn
			       (goto-char open-bracket-pos)
			       (current-column)))
           (target-col (progn
			 (forward-char)
			 (forward-sexp)
			 (skip-chars-forward " \t")
			 (if (eolp)
			     (+ open-bracket-col c-basic-offset)
			   (current-column))))
	   )
      (- target-col open-bracket-col extra))))

(defun c-lineup-ObjC-method-args (langelem)
  ;; Line up the colons that separate args. This is done trying to
  ;; align colons vertically.
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (first-col-column (progn
			       (goto-char relpos)
			       (skip-chars-forward "^:" eol)
			       (and (= (following-char) ?:)
				    (current-column)))))
      (if (not first-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (= (following-char) ?:)
	    (+ curcol (- first-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-ObjC-method-args-2 (langelem)
  ;; Line up the colons that separate args. This is done trying to
  ;; align the colon on the current line with the previous one.
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (prev-col-column (progn
			      (skip-chars-backward "^:" relpos)
			      (and (= (preceding-char) ?:)
				   (- (current-column) 1)))))
      (if (not prev-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (= (following-char) ?:)
	    (+ curcol (- prev-col-column (current-column)))
	  c-basic-offset)))))

(defun c-snug-do-while (syntax pos)
  "Dynamically calculate brace hanginess for do-while statements.
Using this function, `while' clauses that end a `do-while' block will
remain on the same line as the brace that closes that block.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `block-close' syntax."
  (save-excursion
    (let (langelem)
      (if (and (eq syntax 'block-close)
	       (setq langelem (assq 'block-close c-syntactic-context))
	       (progn (goto-char (cdr langelem))
		      (if (= (following-char) ?{)
			  (c-safe (forward-sexp -1)))
		      (looking-at "\\<do\\>[^_]")))
	  '(before)
	'(before after)))))


;;; This page handles insertion and removal of backslashes for C macros.

(defun c-backslash-region (from to delete-flag)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.

This function does not modify the last line of the region if the region ends 
right at the start of the following line; it does not modify blank lines
at the start of the region.  So you can put the region around an entire macro
definition and conveniently use this command."
  (interactive "r\nP")
  (save-excursion
    (goto-char from)
    (let ((column c-backslash-column)
          (endmark (make-marker)))
      (move-marker endmark to)
      ;; Compute the smallest column number past the ends of all the lines.
      (if (not delete-flag)
          (while (< (point) to)
            (end-of-line)
            (if (= (preceding-char) ?\\)
                (progn (forward-char -1)
                       (skip-chars-backward " \t")))
            (setq column (max column (1+ (current-column))))
            (forward-line 1)))
      ;; Adjust upward to a tab column, if that doesn't push past the margin.
      (if (> (% column tab-width) 0)
          (let ((adjusted (* (/ (+ column tab-width -1) tab-width) tab-width)))
            (if (< adjusted (window-width))
                (setq column adjusted))))
      ;; Don't modify blank lines at start of region.
      (goto-char from)
      (while (and (< (point) endmark) (eolp))
        (forward-line 1))
      ;; Add or remove backslashes on all the lines.
      (while (and (< (point) endmark)
                  ;; Don't backslashify the last line
                  ;; if the region ends right at the start of the next line.
                  (save-excursion
                    (forward-line 1)
                    (< (point) endmark)))
        (if (not delete-flag)
            (c-append-backslash column)
          (c-delete-backslash))
        (forward-line 1))
      (move-marker endmark nil))))

(defun c-append-backslash (column)
  (end-of-line)
  ;; Note that "\\\\" is needed to get one backslash.
  (if (= (preceding-char) ?\\)
      (progn (forward-char -1)
             (delete-horizontal-space)
             (indent-to column))
    (indent-to column)
    (insert "\\")))

(defun c-delete-backslash ()
  (end-of-line)
  (or (bolp)
      (progn
 	(forward-char -1)
 	(if (looking-at "\\\\")
 	    (delete-region (1+ (point))
 			   (progn (skip-chars-backward " \t") (point)))))))


;; defuns for submitting bug reports

(defconst c-version (concat "4.282"
			    " as included in "
			    emacs-version)
  "cc-mode version number.")
(defconst c-mode-help-address "bug-gnu-emacs@prep.ai.mit.edu"
  "Address for cc-mode bug reports.")

(defun c-version ()
  "Echo the current version of cc-mode in the minibuffer."
  (interactive)
  (message "Using cc-mode version %s" c-version)
  (c-keep-region-active))

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on cc-mode."
  (interactive)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list '(c-offsets-alist)))
    (and
     (if (y-or-n-p "Do you want to submit a report on cc-mode? ")
	 t (message "") nil)
     (require 'reporter)
     (reporter-submit-bug-report
      c-mode-help-address
      (concat "cc-mode " c-version " ("
	      (cond ((eq major-mode 'c++-mode)  "C++")
		    ((eq major-mode 'c-mode)    "C")
		    ((eq major-mode 'objc-mode) "ObjC")
		    ((eq major-mode 'java-mode) "Java")
		    )
	      ")")
      (let ((vars (list
		   ;; report only the vars that affect indentation
		   'c-basic-offset
		   'c-offsets-alist
		   'c-block-comments-indent-p
		   'c-cleanup-list
		   'c-comment-only-line-offset
		   'c-backslash-column
		   'c-delete-function
		   'c-electric-pound-behavior
		   'c-hanging-braces-alist
		   'c-hanging-colons-alist
		   'c-hanging-comment-ender-p
		   'c-tab-always-indent
		   'c-recognize-knr-p
		   'defun-prompt-regexp
		   'tab-width
		   )))
	(if (not (boundp 'defun-prompt-regexp))
	    (delq 'defun-prompt-regexp vars)
	  vars))
      (function
       (lambda ()
	 (insert
	  (if c-special-indent-hook
	      (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		      "c-special-indent-hook is set to '"
		      (format "%s" c-special-indent-hook)
		      ".\nPerhaps this is your problem?\n"
		      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	    "\n")
	  (format "c-emacs-features: %s\n" c-emacs-features)
	  )))
      nil
      "Dear Barry,"
      ))))


;; menus for XEmacs 19
(defun c-popup-menu (e)
  "Pops up the C/C++/ObjC menu."
  (interactive "@e")
  (popup-menu (cons (concat mode-name " Mode Commands") c-mode-menu))
  (c-keep-region-active))
    

(defun c-copy-tree (tree)
  ;; Lift XEmacs 19.12's copy-tree
  (if (consp tree)
      (cons (c-copy-tree (car tree))
	    (c-copy-tree (cdr tree)))
    (if (vectorp tree)
	(let* ((new (copy-sequence tree))
	       (i (1- (length new))))
	  (while (>= i 0)
	    (aset new i (c-copy-tree (aref new i)))
	    (setq i (1- i)))
	  new)
      tree)))

(defun c-mapcar-defun (var)
  (let ((val (symbol-value var)))
    (cons var (if (atom val) val
		;; XEmacs 19.12 and Emacs 19 + lucid.el have this
		(if (fboundp 'copy-tree)
		    (copy-tree val)
		  ;; Emacs 19 and Emacs 18
		  (c-copy-tree val)
		  )))
    ))

;; Dynamically append the default value of most variables. This is
;; crucial because future c-set-style calls will always reset the
;; variables first to the `cc-mode' style before instituting the new
;; style.  Only do this once!
(or (assoc "cc-mode" c-style-alist)
    (progn
      (c-add-style "cc-mode"
		   (mapcar 'c-mapcar-defun
			   '(c-backslash-column
			     c-basic-offset
			     c-block-comments-indent-p
			     c-cleanup-list
			     c-comment-only-line-offset
			     c-echo-syntactic-information-p
			     c-electric-pound-behavior
			     c-hanging-braces-alist
			     c-hanging-colons-alist
			     c-hanging-comment-ender-p
			     c-offsets-alist
			     c-recognize-knr-p
			     c-strict-syntax-p
			     c-tab-always-indent
			     c-inhibit-startup-warnings-p
			     )))
      ;; the default style is now GNU.  This can be overridden in
      ;; c-mode-common-hook or {c,c++,objc}-mode-hook.
      (c-set-style c-site-default-style)))

;; style variables
(make-variable-buffer-local 'c-offsets-alist)
(make-variable-buffer-local 'c-basic-offset)
(make-variable-buffer-local 'c-file-style)
(make-variable-buffer-local 'c-file-offsets)
(make-variable-buffer-local 'c-comment-only-line-offset)
(make-variable-buffer-local 'c-block-comments-indent-p)
(make-variable-buffer-local 'c-cleanup-list)
(make-variable-buffer-local 'c-hanging-braces-alist)
(make-variable-buffer-local 'c-hanging-colons-alist)
(make-variable-buffer-local 'c-hanging-comment-ender-p)
(make-variable-buffer-local 'c-backslash-column)



;; fsets for compatibility with BOCM
(fset 'electric-c-brace      'c-electric-brace)
(fset 'electric-c-semi       'c-electric-semi&comma)
(fset 'electric-c-sharp-sign 'c-electric-pound)
;; there is no cc-mode equivalent for electric-c-terminator
(fset 'mark-c-function       'c-mark-function)
(fset 'indent-c-exp          'c-indent-exp)
;;;###autoload (fset 'set-c-style           'c-set-style)
;; Lucid Emacs 19.9 + font-lock + cc-mode - c++-mode lossage
(fset 'c++-beginning-of-defun 'beginning-of-defun)
(fset 'c++-end-of-defun 'end-of-defun)

;; set up bc warnings for obsolete variables, but for now lets not
;; worry about obsolete functions.  maybe later some will be important
;; to flag
(and (memq 'v19 c-emacs-features)
     (let* ((na "Nothing appropriate.")
	    (vars
	     (list
	      (cons 'c++-c-mode-syntax-table 'c-mode-syntax-table)
	      (cons 'c++-tab-always-indent 'c-tab-always-indent)
	      (cons 'c++-always-arglist-indent-p na)
	      (cons 'c++-block-close-brace-offset 'c-offsets-alist)
	      (cons 'c++-paren-as-block-close-p na)
	      (cons 'c++-continued-member-init-offset 'c-offsets-alist)
	      (cons 'c++-member-init-indent 'c-offsets-alist)
	      (cons 'c++-friend-offset na)
	      (cons 'c++-access-specifier-offset 'c-offsets-alist)
	      (cons 'c++-empty-arglist-indent 'c-offsets-alist)
	      (cons 'c++-comment-only-line-offset 'c-comment-only-line-offset)
	      (cons 'c++-C-block-comments-indent-p 'c-block-comments-indent-p)
	      (cons 'c++-cleanup-list 'c-cleanup-list)
	      (cons 'c++-hanging-braces 'c-hanging-braces-alist)
	      (cons 'c++-hanging-member-init-colon 'c-hanging-colons-alist)
	      (cons 'c++-auto-hungry-initial-state
		    "Use `c-auto-newline' and `c-hungry-delete-key' instead.")
	      (cons 'c++-auto-hungry-toggle na)
	      (cons 'c++-relative-offset-p na)
	      (cons 'c++-special-indent-hook 'c-special-indent-hook)
	      (cons 'c++-delete-function 'c-delete-function)
	      (cons 'c++-electric-pound-behavior 'c-electric-pound-behavior)
	      (cons 'c++-hungry-delete-key 'c-hungry-delete-key)
	      (cons 'c++-auto-newline 'c-auto-newline)
	      (cons 'c++-match-header-strongly na)
	      (cons 'c++-defun-header-strong-struct-equivs na)
	      (cons 'c++-version 'c-version)
	      (cons 'c++-mode-help-address 'c-mode-help-address)
	      (cons 'c-indent-level 'c-basic-offset)
	      (cons 'c-brace-imaginary-offset na)
	      (cons 'c-brace-offset 'c-offsets-alist)
	      (cons 'c-argdecl-indent 'c-offsets-alist)
	      (cons 'c-label-offset 'c-offsets-alist)
	      (cons 'c-continued-statement-offset 'c-offsets-alist)
	      (cons 'c-continued-brace-offset 'c-offsets-alist)
	      (cons 'c-default-macroize-column 'c-backslash-column)
	      (cons 'c++-default-macroize-column 'c-backslash-column)
	      )))
       (mapcar
	(function
	 (lambda (elt)
	   (make-obsolete-variable (car elt) (cdr elt))))
	vars)))

(provide 'cc-mode)
;;; cc-mode.el ends here
