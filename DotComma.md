Given `b` => 312, what is the value of {{{`}}}`(a .,b)`?

Guile, Bigloo, Shoe, Dream, S7, Elk, Sizzle, Dfsch:  `(a .,b)`, treating `.,b` as a symbol.

Racket, Gauche, MIT, Scheme48/scsh, Kawa, Chibi, SCM, Chez, STklos, KSi, Scheme 9, BDC, Rep, Schemik, Oaklisp, FemtoLisp, Inlab, Owl Lisp, Sagittarius (and Common Lisp): `(a . 312)`.

Chicken: `(a |.| 312)`.

SigScheme (which explicitly calls it a compatibility issue), Vicare, Ypsilon, Mosh, and probably the other R6RS systems: syntax error.

TinyScheme, XLisp, SXM: no support for quasiquote.
