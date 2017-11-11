The correct value of `(rationalize 20 1)` is 19, because 19/1 is the simplest rational that differs from 20 by an amount no more than 1.  But it is not clear what the value of `(rationalize 20 -1)` should be.  Is it also 19, or should an error be signaled?

`(rationalize 20 1)` => 19 and `(rationalize 20 -1)` => 19:  Racket, MIT, Chicken with the numbers egg, Scheme48/scsh, Guile, Kawa, SISC, Chez, Vicare, Larceny, Ypsilon, Mosh, IronScheme, STklos, KSi, S7, Sagittarius, Foment, Chibi

`(rationalize 20 1)` => 19 but `(rationalize 20 -1)` signals an error: Gauche, Gambit

`rationalize` unsupported:  Chicken, Bigloo, Detroit, Stalin, Scheme->C, SCM, NexJ, JScheme, SigScheme, Shoe, Mini-Scheme, TinyScheme, Scheme 9, RScheme, Unlikely, SIOD, BDC, XLisp, Rep, Schemik, Llava, Sizzle, FemtoLisp, Dfsch, Inlab, Picrin

`(rationalize 20 1)` => 20 and `(rationalize 20 -1)` => 20: UMB, SXM, Owl Lisp

Note that for a procedure that's been in the language since R2RS there are a remarkable number of Schemes that don't support it.  I suspect this is because implementers assume that if they don't support arbitrary exact ratnums there is no point in implementing `rationalize`, though its arguments can be either exact or inexact.
