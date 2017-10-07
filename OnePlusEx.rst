This experiment finds out whether `1+x` and `1+` are valid identifiers in the Scheme implementations.  They are invalid in all versions of the Scheme standard.  We also investigate whether `1+` and `add1` are known procedures.

`1+x` is valid, `1+` is defined, `add1` is defined:  Chez, Sizzle

`1+x` is valid, `1+` is not defined, `add1` is defined:  Racket, Chicken, SISC

`1+x` is valid, `1+` is defined, `add1` is not defined:  MIT, Guile, SCM, XLisp, Rep, Elk, !FemtoLisp, Inlab

`1+x` is valid, `1+` is not defined, `add1` is not defined:  Gauche, Gambit, Bigloo, Kawa, JScheme, STklos, Shoe, !TinyScheme, Scheme 9, BDC, Schemik, Llava, Sagittarius

`1+x` and `1+` are syntax errors, `add1` is defined:  Vicare, !IronScheme, RScheme, SXM

`1+x` and `1+` are syntax errors, `add1` is not defined:  Scheme48/scsh, Larceny, Ypsilon, Mosh, KSi, !SigScheme, UMB, Dfsch, Foment, Chibi

`1+x` is read as `1 +x`, `1+` is read as `1 +`, `add1` is not defined:  NexJ, Picrin, Owl Lisp
