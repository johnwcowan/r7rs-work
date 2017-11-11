This is a test of `(sqrt -1)` to determine various issues: does the value become inexact, and if we don't have complex numbers, do we get an error or a NaN?

Returns exact `+i`:  Racket, MIT, Gambit, Chibi, Chez, Vicare, Ypsilon, Mosh, IronScheme, STklos, Spark, Owl Lisp, Sagittarius

Return inexact `0.0+1.0i`: Gauche, Chicken (with the numbers egg), Scheme48/scsh, Guile, Kawa, SISC, SCM, Larceny, KSi, S7, UMB, Foment

Returns `+nan.0`: Chicken (without the numbers egg), NexJ, JScheme, RScheme, BDC, VX, Inlab, Picrin

Signals an error: Bigloo, Ikarus, Scheme 9, XLisp, Rep, Schemik, Elk, SXM, Sizzle, Dfsch

`sqrt` is undefined: SigScheme, Shoe, TinyScheme, Llava, Oaklisp, FemtoLisp

Dream hangs.

A further test of `(sqrt -1.0)` produces `0.0+1.0i`, `+nan.0`, or an error consistently with the above.
