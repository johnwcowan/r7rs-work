R5RS and R7RS define `=` as accepting at least two arguments; it is an error to supply fewer.  However, that doesn't forbid Schemes from providing extension behavior.  This is an investigation of what Schemes in the suite do with `(=)` and `(= 1)`:

Returns `#t` for both `(=)` and `(= 1)`:  MIT, Gambit, Chicken, Guile, SCM, KSi, Sizzle

Signals an error for `(=)`, but returns `#t` for `(= 1)`:  Chez, Vicare, Ypsilon, IronScheme, JScheme, STklos, XLisp, Elk, Llava, SXM

Signals an error for `(=)`, but returns `#f` for `(= 1)`:  FemtoLisp

Signals an error in both cases:  Racket, Gauche, Bigloo, Scheme48/scsh, Kawa, SISC, Larceny, Mosh, NexJ, SigScheme, Shoe, TinyScheme, Scheme 9, RScheme, S7, BDC, Rep, Schemik, UMB, Dfsch, Inlab, Oaklisp, Sagittarius, Foment, Picrin, Owl Lisp, Chibi
