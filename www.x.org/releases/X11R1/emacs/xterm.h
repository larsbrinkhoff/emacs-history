#include <X/Xlib.h>
#include <X/Xatom.h>
#include <X/keysym.h>
#include <X/cursorfont.h>
#include <X/Xutil.h>
#include <X/X10.h>

#define XMOUSEBUFSIZE 64

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif
