This tests whether exponentiation of exact numbers produces an exact result.  The test expression was `(expt 1/3 3)`, except on systems without inexact rationals, where `(expt 3 3)` was substituted.

Exact exponentiation supported:  Racket, Gauche, MIT, Gambit, Chicken with the `numbers` egg, Bigloo, Scheme48/scsh, Guile, Kawa, SISC, Chez, Vicare, Larceny, Ypsilon, Mosh, IronScheme, STklos, KSi, TinyScheme, Scheme 9, RScheme, S7, XLisp, Rep, UBM, Sizzle, Oaklisp, Sagittarius, Picrin, Owl Lisp, Chibi

Only inexact exponentiation supported:  plain Chicken, SCM, NexJ, JScheme, Unlikely, Elk, SXM, Dfsch, Inlab

No support or broken support for `expt`:  SigScheme, Shoe, Mini-Scheme, SIOD, Schemik, Llava, FemtoLisp, LMU
