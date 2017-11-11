This is an investigation of whether implementations return `#t` or `#f` to `(exact? (sqrt 25/4))`.  RnRS recommends, but does not require, that the value is `#t`, but implementations are free to convert 25/4 to an inexact value and return the inexact square root of that.  On implementations where exact rationals are not supported, `(exact? (sqrt 4))` was substituted.

Supports exact `sqrt`: Racket, Gauche, MIT, Gambit, Chicken with the `numbers` egg, Guile, Chez, Vicare, Larceny, Ypsilon, Mosh, IronScheme, STklos, Unlikely, Rep, Sizzle, LMU, Sagittarius, Owl Lisp

Supports only inexact `sqrt`: plain Chicken, Bigloo, Scheme48/scsh, Kawa, SISC, SCM, NexJ, JScheme, KSi, BDC, Schemik, UMB, Elk, SXM, Dfsch, Inlab, Foment, Picrin, Chibi

Does not support `sqrt`: SigScheme, Shoe, Llava, FemtoLisp, Oaklisp

Indeterminate results (`exact?` does not exist and neither `eq?` nor the printer can be relied on to distinguish): SIOD, XLisp
