## Attempting to set an unbound variable

Racket, Gauche, MIT, Gambit, Bigloo, Scheme48/scsh, Guile, SISC, Chibi, SCM, Ypsilon, NexJ, STklos, KSi, SigScheme, Shoe, TinyScheme, Scheme 9, Dream, S7, Schemik, Elk, UMB, Sizzle, Spark, Dfsch, Inlab, Sagittarius complain about the attempt to use `set!` on an unbound variable.

Chicken, Kawa, Chibi, Chez, Ikarus/Vicare, Larceny, Mosh, XLisp, Rep, VX, Llava, Femtolisp do not.

Oaklisp, SXM print a warning but allow it.

Owl Lisp doesn't have variable assignment.
