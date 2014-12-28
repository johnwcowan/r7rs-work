Testing the usual suite of Schemes to see whether promises returned by `delay` are disjoint from procedures.

Disjoint: Racket, Gauche, MIT, Gambit, Chicken, Guile, Kawa, Chibi, SCM, STklos, !SigScheme, !TinyScheme, RScheme, Dream, BDC, XLisp, Elk, UMB, VX, SXM, Sizzle, Spark, Sagittarius (note that in !TinyScheme and Dream promises can be invoked)

Not disjoint: Bigloo, Scheme48/scsh, SISC, Chez, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, Scheme 9, Schemik, Inlab, Owl Lisp

`delay` undefined: KSi, Shoe, S7, Rep, Llava, !FemtoLisp, Dfsch

Buggy implementation: Oaklisp

Note:  In Owl Lisp, repeated forces re-evaluate the delayed expression, because Owl Lisp is immutable.

See also ForceNonPromise.