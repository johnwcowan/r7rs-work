I tested `memq` on an improper list, first against an object that appears in the list, then against an
object that does not.

In the first case, all 39 Schemes return the tail of the improper list except Mosh, which signals an error.

In the second case, the following Schemes signal an error:  Racket, MIT, Gambit, Scheme48/scsh, Guile, SISC, Chez, SCM, !Ikarus/Vicare, Larceny, Ypsilon, !IronScheme, NexJ, KSi, !SigScheme, Shoe, Dream, BDC, Rep, Schemik, Elk, UMB, VX, Oaklisp, Owl Lisp, Scheme 9.

The following Schemes quietly return #f: Gauche, Chicken, Bigloo, Guile, Chibi, STklos, S7, XLisp, Sagittarius.