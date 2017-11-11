## Empty definitions

I tested `(define x)` on the Scheme test suite.

Syntax error: Racket, Gauche, Bigloo, Kawa, Chibi, SCM, NexJ, STklos, SigScheme, Shoe, Scheme 9, Dream, RScheme, S7, UMB, Oaklisp, Sizzle, Spark, Dfsch, Owl Lisp.

Variable is bound but not defined: MIT, Scheme48/scsh, Larceny.

Variable is bound to the implementation-specific "undefined result" value:  Gambit, Chicken, Guile, SISC, Chez, Vicare, Ypsilon, Mosh, IronScheme, Sagittarius.

Variable is bound to the empty list, which also serves as the "undefined result" value: TinyScheme, XLisp, Rep, Elk, Inlab.

Variable is bound to the empty list, which is *not* the "undefined result" value: VX.

Variable is bound to a thunk which returns itself and does not appear to have any side effects: SXM.

