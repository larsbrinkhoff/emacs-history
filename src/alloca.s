/* `alloca' standard 4.2 subroutine for 68000's and 16000's and pyramids.
   Also has _setjmp and _longjmp for pyramids.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
document "GNU Emacs copying permission notice".   An exact copy
of the document is supposed to have been given to you along with
GNU Emacs so that you can know how you may redistribute it all.
It should be in a file named COPYING.  Among other things, the
copyright notice and this notice must be preserved on all copies.  */

/* Both 68000 systems I have run this on have had broken versions of alloca.
   Also, I am told that non-berkeley systems do not have it at all.
   So replace whatever system-provided alloca there may be
   on all 68000 systems.  */

/* ASSEMBLY prevents config.h from generating any C code. */

#define ASSEMBLY

#include "config.h"

#ifdef m68000

/* Some systems want the _, some do not.  Win with both kinds.  */
.globl	_alloca
_alloca:
.globl	alloca
alloca:
	movl	sp@+,a0
	movl	a7,d0
	subl	sp@,d0
	andl	#~3,d0
	movl	d0,sp
	tstb	sp@(0)		/* Make stack pages exist  */
				/* Needed on certain systems
				   that lack true demand paging */
	addql	#4,d0
	jmp	a0@

#endif

#ifdef ns16000

	.text
	.align	2
/* Some systems want the _, some do not.  Win with both kinds.  */
.globl	_alloca
_alloca:
.globl	alloca
alloca:
/*
 * The ns16000 is a little more difficult, need to copy regs.
 * Also the code assumes direct linkage call sequence (no mod table crap).
 * We have to copy registers, and therefore waste 32 bytes.
 *
 * Stack layout:
 * new	sp ->	junk	
 *	 	registers (copy)
 *	r0 ->	new data		
 *		 | 	  (orig retval)
 *		 |	  (orig arg)
 * old  sp ->	regs	  (orig)
 *		local data
 *	fp ->	old fp
 */

	movd	tos,r1		/*  pop return addr */
	negd	tos,r0		/*  pop amount to allocate */
	sprd	sp,r2
	addd	r2,r0
	bicb	$3,r0		/*  4-byte align */
	lprd	sp,r0
	adjspb	$36		/*  space for regs, +4 for caller to pop */
	movmd	0(r2),4(sp),$4	/*  copy regs */
	movmd	0x10(r2),0x14(sp),$4
	jump	(r1)		/* funky return */
#endif

#ifdef pyramid

.globl _alloca

_alloca: addw $3,pr0	# add 3 (dec) to first argument
	bicw $3,pr0	# then clear its last 2 bits
	subw pr0,sp	# subtract from SP the val in PR0
	movw sp,pr0	# ret. current SP
	ret

.globl __longjmp
.globl _longjmp
.globl __setjmp
.globl _setjmp

__longjmp: jump _longjmp
__setjmp:  jump _setjmp

#endif /* pyramid */

