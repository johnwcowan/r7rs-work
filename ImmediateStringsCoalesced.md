## Are immediate strings coalesced?

Asked to evaluate `(let ((a "hello") (b "hello")) (eq? a b))`, Racket, Gauche, MIT, Gambit, Chicken, Bigloo, SISC, Chibi, Chez, SCM, Ikarus/Vicare, Mosh, KSi, SigScheme, TinyScheme, Scheme 9, Dream, S7, BDC, XLisp, Rep, Schemik, Elk, UMB, Oaklisp, and Owl Lisp all return `#f`.

Scheme48/scsh, Guile, Kawa, Larceny, Ypsilon, IronScheme, NexJ, STklos, Shoe, RScheme, JScheme, VX, Sagittarius return `#t`.

There may be some false negatives here, because an implementation might coalesce literals in the compiler but not at the REPL.
