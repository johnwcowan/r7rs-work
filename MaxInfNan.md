== Max of +inf.0 and +nan.0 ==

R6RS requires that `(max +inf.0 x)` for any real x return `+inf.0`; it is silent about `(max x +inf.0)`, but I'd think that was entailed.  I tested `(max +inf.0 +nan.0)` against the test suite.

Racket, Gauche, MIT, Chicken (with and without the numbers egg), Scheme48, Guile, Ypsilon, Mosh, !IronScheme, STklos, Elk, VX return `+inf.0`.

Gambit, Bigloo, Kawa, SISC, Chibi, Chez, Vicare, Larceny, NexJ, UMB, Spark, !FemtoLisp, Sagittarius return `+nan.0`.  

My other Schemes throw errors, either because they don't like inexact numbers, they don't like division by 0.0, or they produce cockeyed values of `(/ 1.0 0.0)` and/or `(/ 0.0 0.0)`.

Note that the six R6RS implementations are split 3-3 (or 4-3 if Guile, which does not fully implement R6RS, is included), and that all the Java ones prefer `+nan.0`, as Java does.

== Max of +nan.0 and 0 ==

IEEE says that `(max +nan.0 0.0) should return 0.0, and R7RS says `(max +nan.0 0)` has to return an inexact number, but is silent about which number.  The latter is tested here.

Racket, Gauche, Gambit, Chicken with the numbers egg, Scheme48, Guile, Chez, Vicare, Ypsilon, Mosh, !IronScheme, NexJ, STklos, RScheme, BDC, Elk, Sagittarius return `+nan.0`.

Bigloo, scsh, Kawa, SISC, Larceny, Scheme 9, UMB return `0.0`.

Chibi, !FemtoLisp return `0`.

Plain Chicken, KSi, S7, XLisp, Rep, Schemik, Oaklisp, SXM, Sizzle, Dfsch, Inlab, Owl Lisp raise a division by zero error.

!SigScheme, Dream do not support inexact numbers.

Shoe, !TinyScheme, Llava do not support `max`.