/* GNU Emacs password definition file.
   Copyright (C) 1986 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

#ifdef VMS
/* On VMS, we read the UAF file and emulate some of the necessary
   fields for Emacs. */
#include "uaf.h"

struct passwd {
  char pw_name[UAF$S_USERNAME+1];
  char pw_passwd[UAF$S_PWD];
  short pw_uid;
  short pw_gid;
  char pw_gecos[UAF$S_OWNER+1];
  char pw_dir[UAF$S_DEFDEV+UAF$S_DEFDIR+1];
  char pw_shell[UAF$S_DEFCLI+1];
};
#endif /* VMS */
