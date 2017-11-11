The formal syntax of R4RS through R7RS disallow unsigned imaginary numbers.  Here is how the test Schemes handle the input `35i`:

Undefined variable:  Racket, Gauche, MIT, Chicken (with or without the numbers egg), Bigloo, Guile, SISC, SCM, Chez, JScheme, STklos, Shoe, TinyScheme, Scheme 9, S7, BDC, XLisp, Rep, Schemik, Elk, Sizzle, FemtoLisp, Inlab, Sagittarius

Unsupported lexical syntax: Scheme48/scsh, Larceny, Ypsilon, IronScheme, NexJ, KSI, SigScheme, SXM, Dfsch, Owl Lisp

Equivalent to `+35i`: Kawa, Vicare, Chibi, RScheme (does not support complex numbers), UMB

Returns a so-called generic object: Llava
