This was written by [Bob Frankston](https://en.wikipedia.org/wiki/Bob_Frankston).
Published with permission.

---

I was working part-time at [Interactive
Data](https://en.wikipedia.org/wiki/Interactive_Data_Corporation)
which was using [CP/CMS](https://en.wikipedia.org/wiki/CP/CMS), and I
wanted a better editor.  I used the
[QED](https://en.wikipedia.org/wiki/QED_(text_editor)) I learned in
1966 on the [SDS-940](https://en.wikipedia.org/wiki/SDS_940) for
inspiration.  There was also a qed (qedx) on
[Multics](https://en.wikipedia.org/wiki/Multics) written by the
soon-to-be [Unix](https://en.wikipedia.org/wiki/Unix) crew.  So I
wrote my own.  When we got the
[Primes](https://en.wikipedia.org/wiki/Prime_Computer), I took that
code and put a screen interface on it to turn it into an
[Emacs](https://en.wikipedia.org/wiki/Emacs).  I was especially proud
of my support for the [VT-100](https://en.wikipedia.org/wiki/VT100)
screen support which made it quite usable at dial-up speeds (and
better than the
[ITS](https://en.wikipedia.org/wiki/Incompatible_Timesharing_System)/Multics
versions in that regard).  The program itself was written in
[PL/1](https://en.wikipedia.org/wiki/PL/I), and my initial code
predated [Bernie](https://en.wikipedia.org/wiki/Bernard_Greenberg)’s
Lisp version.

Seth’s implementation, if I remember right, wasn’t full
[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)), but
I’m not sure if it did [garbage
collection](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)).
(Later, he developed a similar language for our portable programming
but without GC).  Seth [Steinberg] had previously written an operating
system for the Architecture Machine Group (which became the [Media
Lab](https://en.wikipedia.org/wiki/MIT_Media_Lab)).

It’s worth noting that it was a small community, so there was and
continues to be lots of cross-influence.  I presume you know that
[Stallman](https://en.wikipedia.org/wiki/Richard_Stallman) modeled the
[GNU](https://en.wikipedia.org/wiki/GNU_Project) version after
Bernie’s implementation rather than his own
[TECO](https://en.wikipedia.org/wiki/TECO_(text_editor)) one in its
use of Lisp and the undo feature.
