# Historical Emacs Software Preservation

### Included Implementations

- TECO EMACS
- Zwei/Zmacs
- Multics Emacs
- Gosling Emacs
- GNU Emacs
- Lucid Emacs
- XEmacs

### GNU Emacs Release History

Run the `build.sh` script to create a git repository with GNU Emacs
releases from 16.56 to 19.34.  Tarballs are preferred, but some
releases are reconstructed from diffs.  Timestamps will be copied from
the top entry in `src/ChangeLog`; this seems to be a good heuristic.

## Provenance

- TECO EMACS 162 from Alfred M. Szmidt.

  `ai-emacs.tgz` and `ai-emacs1.tgz`
  Released September 1981-09-20, but this copy has further modifications.  
  "Based on AI EMACS; LOCK timestamp:
  1988-03-02 or there abouts"

  "This is for ITS (from the AI system), I think the one on MC is the
  same I can check if you really are curious, and this should be 162
  but lack of version strings and what not make it interesting"

- emacs-v170.tap from Richard Alderson.

  TOPS-20 `DUMPER` tape image of TECO EMACS.  `<EMACS>EMACS.EXE` has
  timestamp May 1 1996.  
  Extracted in http://github.com/PDP-10/emacs/tree/extracted

- MIT CADR Zwei from Alfred M. Szmidt and Brad Parker.

  `nzwei-mit-48.tar.xz`
  "Based on zmacs.lisp time stamp:
  1980-10-17 or there abouts"

  `zwei-mit-78.tar.xz`
  "Based on the System patch-dir file
  ;;; Written 1/11/82 13:52:10 by BSG"

  `zwei-mit-99.tar.xz`
  "Based on zmacs.lisp timestamp:
  1985-04-13 or there abouts"

  "This is for MIT CADR, copies come from tapes for the MIT CADR
  system, and some backups that Brad Parker found several years
  ago (NN is the version of the system)"

- TI Explorer Zmacs 6 from Alfred M. Szmidt.

  `zmacs-ti-6.tar.xz` (System 6)
  "Based on zmacs.lisp time stamp:
  1980-10-17 or there abouts"

  "This is for TI Explorer (N is the version of Zmacs, not the
  operating system -- even though they are the same)"

- Symbolics Genera Zwei from Alfred M. Szmidt.

  `zwei-419.tar.xz` (Genera 8.1)
  "Based on the Zwei patch-dir file:
  ;;; Written 9/05/97 11:27:49 by LISPM"

  `zwei-430.tar.xz` (Genera 8.3)
  "Based on the Zwei patch-dir file:
  ;;; Written 2/02/93 11:06:17 by Palter"

  `zwei-436.tar.xz` (Genera 8.5 / Open Genera 2.0)
  "Based on the Zwei patch-dir file:
  ;;; Written 4/10/91 15:20:14 by Palter"

  "This is from Genera, copies come from CD-ROMs and tapes distributed
  with the Symbolics Lisp Machines (zwei-NNN where NNN is the version
  of Zwei/Zmacs)"

- zwei-lambda.tar.xz from Alfred M. Szmidt and Joe Marshall.

  "This is for/from Lambda, I do not know the version sadly :( The
  copy comes from Joe Marshall who used to work at Gigamos."

- Multics Emacs 12.9 from web.mit.edu.

  Last changed in 1989-11-14.

- Emacs 13.8 from http://decuslib.com/decus/vax85b/gnuemax/emacs/

  Possibly with VMS modifications.

- emacs-16.56.tar.gz from ftp://ftp.splode.com/pub/users/friedman/emacs/

  Noah Friedman: "In 1993 I recovered a copy of the Emacs 16.56
  sources from backup tapes at MIT because rms needed it in a court
  case with Unisys.  I put it back up for ftp a couple of years later
  and it's relatively easy to find these days."

- emacs-16.57-1.diff and emacs-16.57-2.diff from Usenet net.sources.

  It seems this is not the official 16.57.  There are no ChangeLogs
  entries.

- Emacs 17.61 from ftp://www.tuhs.org/UnixArchive/4BSD/Distributions/4.3BSD/new.tar.gz

- emacs-17.61.diff and emacs-17.64.diff from Usenet net.emacs.

- Emacs "17.VMS-2" from http://decuslib.com/decus/vax86b/gnuemacs/

- emacs_18.41.tar.gz from http://bitsavers.org/bits/MIT/gnu/

- edist_18_51.tar_z from http://decuslib.com/decus/vax88a1/gnusoftware/

- emacs.tar.Z (**modified** 18.51) from ftp://www.tuhs.org/UnixArchive/4BSD/Distributions/4.3BSD-Tahoe/new.tar.gz

- edist_18_52.tar_lzw from http://decuslib.com/decus/vax88b2/gnusoftware/

- diff-18* from http://www.nic.funet.fi/index/gnu/funet/historical-funet-gnu-area-from-early-1990s/emacs/

- edist_18_51.tar_z from http://decuslib.com/decus/vax88a1/gnusoftware/

- edist_18_52.tar_lzw from http://decuslib.com/decus/vax88b4/gnusoftware/

- emacs-18.55.tar.gz from ftp://ftp4.gwdg.de/pub/msdos/editors/emacs/

- emacs-18.55.tar.Z from ftp://ftp.cs.ait.ac.th/pub/pc/demacs/

- emacs-15.0.3.s.tar.gz from http://ftp.nice.ch/pub/next/developer/nextsources/Pre3.X/

  GNU Emacs for NeXT systems.  This is a version of 18.55, plus RMS
  changes up until July 1990.  This is interesting, because stock
  18.55 was released in August 1989, and 18.56 in January 1991.

- emacs-18.57.tar.gz from  http://www.nic.funet.fi/index/gnu/funet/historical-funet-gnu-area-from-early-1990s/emacs/

- emacs-18.58.tar.gz from  http://www.nic.funet.fi/index/gnu/funet/historical-funet-gnu-area-from-early-1990s/emacs/

- emacs-18.59.tar.gz from  http://ftp.gnu.org/old-gnu/emacs/

- emacs-19.7.tar.gz to emacs-19.21.tar.gz from http://www.nic.funet.fi/index/gnu/funet/historical-funet-gnu-area-from-early-1990s/old/

- emacs-19.22.tar.gz to emacs-19.28.tar.gz from http://www.nic.funet.fi/index/gnu/funet/historical-funet-gnu-area-from-early-1990s/emacs/

- emacs-19.29.tar.gz and emacs-19.30.tar.gz from http://ftp.tiscali.nl/pub/mirrors/sunfreeware/SOURCES/

- emacs-19.31.tar.gz from http://mirrors.slackware.com/slackware/slackware-3.1/source/e/

- emacs-19.34b.tar.gz from  http://ftp.gnu.org/old-gnu/emacs/

- lemacs* and xemacs* from http://ftp.xemacs.org/Attic/Releases/

- gosling-emacs-1999.tar and gosling-emacs-2007.tar from http://brian.org/~reid/misc

  Brian Reid: "I found two versions.
  They are in  reid.org/~brian/misc/gosling-emacs-1999.tar
     and       reid.org/~brian/misc/gosling-emacs-2007.tar

  In 1999 I updated it so it would work with gcc 2
  In 2007 I paid some Russian kid to update it so that it would work
  with gcc 3
  When gcc 4 came out it would no longer compile, and I just sort of
  walked away from it."

  "I got the source from you in 1983. I did a little bit
  of work on it so it would run under 4.3BSD. It was then untouched
  until gcc 2.0 came out; I had to change every variable-argument
  function call.

  So my 1999 version is identical to your 1983 version except for
  those two things."

  About publishing the files:

  Brian Reid: "Fine with me. I consider them to be James' property and not my own."

  James Gosling: "It’s fine with me too. Archaeology is a good thing :-)"
