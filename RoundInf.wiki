Results from `(round (* 1.0e300 1.0e300))` (to avoid problems with Schemes that don't understand the syntax `+inf.0`):

Returns the argument:  Racket, Gauche, MIT, Chicken (with and without the numbers egg), Bigloo, Guile, Kawa, SISC, SCM, Chez, Vicare, Ypsilon, Mosh, !IronScheme, NexJ, STklos, KSi, Shoe, BDC, Rep, Schemik, Elk, VX, Spark, Dfsch, Inlab, Sagittarius

Error:  Gambit, Scheme48/scsh, SISC, S7

Returns 1.0e+600: Scheme 9

Returns 0: RScheme

Returns smallest fixnum: XLisp, Sizzle

Crashes: UMB

No `round`: !TinyScheme, Llava, !FemtoLisp

No flonums: !SigScheme, Dream, Oaklisp, Owl Lisp