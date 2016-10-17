		Installation and Start-Up Procedure
		for J, Version 3.2, on Unix (TM) systems


0.  The compressed tape archive contains the following files:

     j				The executable file
     readme.doc			Copyright notice
     status.doc			Implementation status
     install 			Something like this file
     message 			Message in a Bottle
     isiinfo.doc		Information about ISI
     tutorial.js		Script for running a tutorial
     tut			Sub-directory needed by tutorial.js
     tut/tut*.js		Files needed by tutorial.js

1.  These installation steps are suggested:
Start with the file j_XXX_3.2_.tar.Z, the compressed Tape ARchive
file for the system identified by XXX.

     mkdir ~/j			Create a new directory called j.
     mv j_*.tar.Z ~/j		Move the package to the new directory.
     cd ~/j			Change to the new directory.
     uncompress j_*		Expand the compressed tar file.
     tar xof j_*.tar		Extract files from the tape archive.

2.  If yours is a one user system, these steps are suggested:

     alias j ~/j/jXXX		Set an alias for j.
     j				To execute the alias.

In this way, J will be available to you from any directory.
The J tutorial (mentioned above) will be available to you
whenever you make ~/j your current directory.

3.  When you start J, you should soon see the message

   J Version 3.2  Copyright (c) 1990 1991, Iverson Software Inc.

   and a 3-space prompt.  At this point, the system is ready to
   interpret J sentences.  To terminate the session, enter   )off  .

4.  A tutorial is available.  Within a session, enter:

     )sscript 'tutorial.js'	Make the necessary definitions
     tutorial ''		Start the tutorial

5.  For shared use on a multi-user system, an appropriate installation
is needed.  The directory used for the binary file  j  will depend
on the conventions in use at your installation.  Some system
administrators may like  J  itself to be installed as /bin/j.
The  J  tutorial files may be installed in a different directory,
such as  /software/j/doc.  If this directory path is used, the
files needed by the tutorial would named: 

	/software/j/doc/tutorial.js
	/software/j/doc/tut/tut*.js	# (47 of these, numbered 0 to 46)

In such a setting, for the file  tutorial.js  to run correctly, it must
be modified.  In this file, the line

	frames=.<&read ('tut/tut'&,)&(,&'.js')&:&.>i.47

may be replaced by

	Path =. '/software/j/doc/tut'
	frames=.<&read ((Path,'/tut')&,)&(,&'.js')&:&.>i.47

Then users may be instructed to do these steps,

	% cp /software/j/doc/tut/tutorial.js .
	% j

	      )sscript 'tutorial.js'
	      tutorial ''

Or users may be instructed to do these steps:

	% j

		)sscript '/software/j/doc/tutorial.js'
		tutorial ''


-------------------------------------------------------------------------------

                          What is J?

			An introduction
			    Part 1


			     by

			Leroy J. Dickey

			September, 1991


J is a high powered general purpose programming language.  This dialect
of APL uses the ASCII character set, has boxed arrays, complex numbers,
and phrasal forms.  Any time one wants to do calculations with more
than one number at a time, or more than one name in a list, a
collective (an array) is the right structure to use, and J is designed
to make it easy to do collective calculations.

People who write programs for themselves and who want their answers
quickly will be happy with the swift and concise way programs can be
written in J.

This article consists of several examples that illustrate some of the
power of the language J.  In each of the examples presented, output
>from an interactive J session has been captured, and few lines of
explanation have been added.  Lines that are indented with four spaces
are those that were typed in to the J interpreter, and the lines that
are indented with only one space are the responses from J.  Other text
lines are comments that have been added later.  The topics that have
been chosen for inclusion do not come close to telling everything about
J, but some of them represent subjects that at one time or another the
author wished that he had known and understood.

Because my professional interests are mathematical, my examples are
primarily mathematical in nature.  But J is a general purpose computing
language, and is well suited to a broad spectrum of programming needs.


Example 1:  Functional notation

This example shows how calculations are done with a list of numbers and
how the notation is functional.  That is, each verb (function) acts on
the data to its right.   It is easy to create a collective.  Here, for
instance, are 9 numbers:

    i. 9
 0 1 2 3 4 5 6 7 8

    a =. i. 9
    a
 0 1 2 3 4 5 6 7 8

The factorials of these numbers are:
    ! a
 1 1 2 6 24 120 720 5040 40320

The reciprocals of the above:
    % ! a
 1 1 0.5 0.166667 0.0416667 0.00833333 0.00138889 0.000198413 2.48016e_5

And the sum of reciprocals of the factorials of a is:
    +/ % ! a
 2.71828


Those who know some mathematics may recognize an approximation to the
number ``e'', the base for the natural logarithms.  Of course J has
other ways to calculate this number; the point here is the use of the
natural right to left reading of the meaning of the sequence symbols
+/%!a as the sum of the reciprocals of the factorials of a.

Those who have done some programming in almost any other language, will
see that the expression +/%!i.9 is a remarkably short one to produce
an approximation to ``e''.  Moreover, it is possible to pronounce this
program in English in a way that precisely conveys its meaning, and
those who know the meaning of the words in the sentence will be able to
understand it.  And anybody who knows the mathematics and J will
recognize that the program does exactly what the words say and that it
will produce an approximation to ``e''.

The author finds this expression in J much easier to think about and
to understand than a corresponding program in a ``traditional'' language
intended to compute the same number.  Here, for example, is what one
might write in Fortran, to accomplish the same result:


	REAL SUM, FACTRL
	SUM = 1.0
	FACTRL = 1.0
	DO 10 N = 1, 8
	    FACTRL = FACTRL * N
	    SUM = SUM + 1.0 / FACTRL
     10 CONTINUE
	PRINT, SUM
	STOP
	END


Compare this Fortran program with the J program that uses only a few
key strokes: +/ % ! i. 9 .  Not only is the J program shorter, but it
is easier to understand, even for the person who has just learned the
meaning of the symbols.  Some have estimated that for typical
applications, the ratio of the number of lines of Fortran or C code to
the number of lines of J code is about 20 to 1.



Example 2:

In this example, two verbs are defined.  As before, lines indented
with four spaces were given as input during a J session, and
the ones immediately after, with one leading space, were produced by
the J interpreter.  Other lines are comments, and were added later.


    Sum =. +/
    Average =. Sum % #
    c =. 1 2 3 4
    Average c
 2.5

The first two lines create pro-verbs, the third creates a pro-noun,
and the fourth invokes the function Average with the data c.
The meaning of   Average c  is this:

		(+/ % #) 1 2 3 4

and this may be thought of as:  find Sum c, (add up the entries
in c), find # c (the count of c), and then find the quotient of those
two results.

Example 3:  Continued fractions

In this example, the verb ``pr'' (plus reciprocal), is considered.

    pr =. +%

The next input line shows a use of this hook and you can see that
the meaning is 5 plus the reciprocal of 4:

    5 pr 4
 5.25

    4 pr 5
 4.2

The function pr is defined above as  +% , and this diagram 
for    4 +% 5  may help you to understand its meaning.

 					+
 				       /   				      4    %
 				           |
 				           5

Because of the shape of the diagram, the combination of two verbs
in this way is called a hook.  To continue with our example,

    1 pr 1 pr 1			
 1.5

The above input line may be read as ``1 plus the reciprocal
of (1 plus the reciprocal of 1)''.  And here below is an analogous
line with four ones:

    1 pr 1 pr 1 pr 1
 1.66667

    1 pr (1 pr (1 pr 1))
 1.66667

    pr / 1 1 1 1		
 1.66667

    pr / 4 # 1
 1.66667

The above input line has exactly the same meaning as the three input
lines that come before it.  Notice that (of course) the value
of the result is the same.  The / is an adverb called over
and the result of ``pr /'' is a new verb, whose meaning is
derived from the verb ``pr''.

This expression could be written another way, using a traditional
mathematical notation the way that mathematicians have been using
to write continued fractions for years:

		1
   1 + ------------------------
		1
	1 + ------------------
			1
		1 + ------------
			1

This particular continued fraction is a famous one because it is
known to converge to the golden ratio.  That means that if you just
keep on going, with more and more ones, the value gets closer and
closer to the golden ratio.  One can see the values of the continued
fraction expansion converging by using , the scan adverb.  It
creates a sequence of initial sequences.  Here is a nice, compact
expression that shows the values for the first 15 partial continued
fractions.

    pr / 15$ 1
 1 2 1.5 1.66667 1.6 1.625 1.61538 1.61905 1.61765 1.61818 1.61798
      1.61806 1.61803 1.61804 1.61803

In this, you can see that the numbers seem to be getting closer and
closer to some limit, probably between 1.61803 and 1.61804.  If you
concentrate only on the odd numbered terms, (the first, the third, the
fifth, and so on), you will see a sequence of numbers that is
monotonically increasing.  On the other hand, the even numbered terms,
( 2, 1.66667, 1.625, and so on), form a sequence of numbers that is
monotonicly decreasing.  Of course these observations do not constitute
a proof of convergence, but they might convince you that a proof can
be found, and in the light of these observations, the proof probably
follows along these lines.

Can you think of a simpler way to represent and evaluate continued
fractions?  I can not!


Example 4.  Using rank operator, and fork.

In this example, a second phrasal form, the fork, is discussed.
First some data are built.

    a =. 100 200
    b =. 10 20 30
    c =. 1 2 3 4

This is the beginning of the data construction.  The verb Sum, a
modification of +, and defined as before, is used.  But in this example,
the usage is different from that used before.  So that you can have a
glimpse at what the dyadic verb Sum does, notice see what it does with
b and c:

    b Sum c
 11 12 13 14
 21 22 23 24
 31 32 33 34

Once you have seen this example, you are be ready to see and understand
the next step.  Here the collective ``data'' is built, using a, b, and c.

    data =. a Sum b Sum c
    data
 111 112 113 114
 121 122 123 124
 131 132 133 134

 211 212 213 214
 221 222 223 224
 231 232 233 234

This rank three array may be thought of as having has two
planes, with each plane having two rows and three columns.
Now see what Sum does with the data:

    Sum data
 322 324 326 328
 342 344 346 348
 362 364 366 368

Can you see what is happening?  It is adding up corresponding
elements in the two planes.  Twelve different sums are performed,
and the result is an array that is 3 by 4.

But we might wish to add up numbers in rows.  We can do it this way:

    Sum 1 data
 450 490 530
 850 890 930

And to add up numbers in each column:

    Sum 2 data
 363 366 369 372
 663 666 669 672

The expressions 1 and 2 are read as 'rank 1' and 'rank 2'.
The rank 1 objects are the rows, and the rank 2 objects are the
planes, and the rank 3 object is the whole object itself.

Now, recall the definition of Average.

    Average =. Sum % #

Now apply this proverb Average to the pronoun data, to get:

    Average data
 161 162 163 164
 171 172 173 174
 181 182 183 184

The result is the average of corresponding numbers in the two planes.
And, if we would like to know the average of the numbers in the rows,
(remember above, we asked about the sum of the entries in the rows?)
we type:

    Average 1 data
 112.5 122.5 132.5
 212.5 222.5 232.5

and finally, averaging the columns of the two planes:

    Average 2 data
 121 122 123 124
 221 222 223 224


Again, compare the sizes of these results with the sizes of
the sizes of the results above where we were asking simply about
sums.

    Sum 1 data
 450 490 530
 850 890 930

    Sum 2 data
 363 366 369 372
 663 666 669 672

What is exciting about this example is not only how easily the nouns
(data) and verbs (functions) were built, but how every verb acts in a
uniform and predictable way on the data.  Rank one action is the same
kind of action whether one is summing, or averaging, or whatever.  It
has been said that the concept of rank is one of the top ten ideas to
come along in computing in the last decade.

Conclusion:

What has been written in this article only begins to scratch the surface
of the power of J, but it does show some of the flavor.
J is powerful because one get results quickly.  It is a programming
language for people who need to write programs for themselves, but
who don't want to spend inordinately large amounts of time getting
their results.  If you have to write programs for others, this could
still be a language for you, if you get paid by the job, and not
by time.

Availability:

J is available from watserv1.waterloo.edu in the directory
languages/apl/j and is also available from certain other servers, but
the others usually have some specific hardware orientation.  At the
Waterloo site, versions of J are available that run on several kinds of
computers.

Learning J:

Because J is new, there are not many materials about J yet.
The novice should get a copy of J and let J be the guide and
the interpreter (pun intended).  There is a tutorial that has
tons of material in a small space.  This tutorial comes with
the interpreter, free of charge.  There is also a status.doc
file that comes with J.  This tell what features have been
implemented and which have not.

The author of J:

The inventor-developer of J is Kenneth E. Iverson, the same man who
invented APL, a real pioneer of computing.  Some years ago, he retired
>from IBM and went to work for I.P.Sharp Associates, a Toronto company
that developed the world's first private packet switching network,
the finest APL available, and was at one time the worlds largest
time sharing service, all APL.  The descendant of this company,
Reuter:file, still has offices in many of the world's major cities.
Now Iverson is retired (again) and lives in Toronto, where he spends
his time developing and promoting J.

The programmer for J is Roger Hui.  Roger hails from Edmonton, and has
lived in Toronto since he joined I.P.Sharp Associates in 1975.  The
source code for J, written in C, is worthy of some comment, because it
makes extensive use of macro expansion.  Its style, influenced by the
work of Arthur Whitney of Morgan Stanley in New York, reflects Roger's
strong background in APL.  Today, Roger spends much of his time
developing J and does occasional contract work in APL.

Copyright (c) 1991 by Leroy J. Dickey
Permission is granted to use this for
non-profit and educational purposes.
It may be given away, but it may not be sold.
