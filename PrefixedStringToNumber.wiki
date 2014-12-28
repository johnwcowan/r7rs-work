I tested `(string->number "#x10")` against my usual test suite.

Racket, Gauche, MIT, Gambit, Chicken (with or without the numbers egg),
Scheme48/scsh, Guile, SISC, Chibi, Chez, SCM, !Ikarus/Vicare, Larceny,
Ypsilon, Mosh, !IronScheme, NexJ, STklos, !TinyScheme, S7, Rep,
Elk, UMB, SXI, Sizzle, Spark, Inlab, Owl Lisp, Scheme 9, Sagittarius all return 16.

Bigloo,
Kawa, KSi, !SigScheme, Dream, BDC, XLisp, Schemik, VX return #f.

Shoe,
Oaklisp, MScheme, Bus don't support `string->number`.