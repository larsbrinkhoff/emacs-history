;; Copyright (C) 1995 Free Software Foundation, Inc.
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

  "*If non-nil then recognize member files using ^M^J as line terminator.")
(defvar archive-remote-regexp "^/[^/:]*[^/:.]:"
  "*Regexp recognizing archive files names that are not local.
A non-local file is one whose file name is not proper outside Emacs.
A local copy of the archive will be used when updating.")
  "*Program and its options to run in order to extract an arc file member.
Extraction should happen to the current directory.  Archive and member
name will be added.")
  "*Program and its options to run in order to extract an lzh file member.
Extraction should happen to standard output.  Archive and member name will
be added.")
  "*If non-nil then pkzip option are used instead of zip options.
Only set to true for msdog systems!")
  "*Program and its options to run in order to extract a zip file member.
Extraction should happen to standard output.  Archive and member name will
be added.  If `archive-zip-use-pkzip' is non-nil then this program is
expected to extract to a file junking the directory part of the name.")
;; For several reasons the latter behaviour is not desirable in general.
  "*Program and its options to run in order to update a case fiddled zip member.
Options should ensure that specified directory will be put into the zip file.
Archive and member name will be added.")
  "*If non-nil then zip file members are case fiddled.
Case fiddling will only happen for members created by a system that
uses caseless file names.")
  "*Program and its options to run in order to extract a zoo file member.
Extraction should happen to standard output.  Archive and member name will
be added.")
  "Convert little endian string/vector to integer.
Alternatively, first argument may be a buffer position in the current buffer
in which case a second argument, length, should be supplied."
  "From the integer OLDMODE and the string NEWMODE calculate a new file mode.
  "Return the descriptor vector for file at point.
Does not signal an error if optional second argument NOERROR is non-nil."
;;;###autoload
  "Major mode for viewing an archive file in a dired-like way.
You can move around using the usual cursor motion commands.
  "Insert a description of a list of files annotated with proper mouse face"
  "Toggle alternative display.
To avoid very long lines some archive mode don't show all information.
This function changes the set of information shown for each files."
  "Delete file NAME and its parents up to and including `archive-tmpdir'."
  "Add current buffer to the archive in ARCBUF naming it NAME."
  "*Possibly handle a buffer with ^M^J terminated lines."
  "Change the protection bits associated with all marked or this member.
	     (dirtype (char-after (+ p 4)))
	     (lfnlen  (if (= dirtype 2) (char-after (+ p 56)) 0))
	     (ldirlen (if (= dirtype 2) (char-after (+ p 57)) 0))
	     (fnlen   (+ ldirlen
			 (if (> lfnlen 0)
			     (1- lfnlen)
			   (or (string-match "\0" namefld) 13))))
	     (efnname (concat
		       (if (> ldirlen 0)
			   (concat (buffer-substring
				    (+ p 58 lfnlen) (+ p 58 lfnlen ldirlen -1))
				   "/")
			 "")
		       (if (> lfnlen 0)
			   (buffer-substring (+ p 58) (+ p 58 lfnlen -1))
			 (substring namefld 0 fnlen))))
	     (fiddle  (and (= lfnlen 0) (string= efnname (upcase efnname))))