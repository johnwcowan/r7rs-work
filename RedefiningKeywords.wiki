What happens when a syntax keyword is redefined as a variable, with `(define (cond) 32) (cond)`?

Returns 32:  Racket, Gauche, MIT, Guile, Kawa, SISC, SCM (with warning), Chez, Vicare, Larceny (with warning), Mosh, !IronScheme, NexJ, JScheme, !SigScheme, RScheme (with warning), BDC, Schemik, UMB, Elk, Llava, Sizzle, !FemtoLisp, Dfsch, Inlab, Foment, Owl Lisp, Chibi, Sagittarius

"Attempt to redefine macro" error at define time:  Gambit, SXM

"Attempt to redefine immutable identifier" error at define time: Scheme48/scsh, Ypsilon, KSi

Macro continues to shadow procedure:  Chicken, Bigloo, STklos, Shoe, !TinyScheme, Scheme 9, S7, XLisp, Rep, Picrin