The empty list is self-evaluating in Gauche, MIT, Kawa, SISC, Chibi,
SCM, NexJ, STklos, !SigScheme, !TinyScheme, Scheme 9, Dream, S7, BDC,
Xlisp, Rep, Jscheme, Schemik, VX, Oaklisp, Sizzle, Spark, Femtolisp, Dfsch, Inlab, Owl Lisp, Sagittarius.

Racket, Gambit, Chicken, Bigloo, Guile, Scheme48/scsh, Chez, !Ikarus/Vicare, Larceny, Ypsilon, Mosh, !IronScheme, KSi, Shoe, Elk, RScheme, UMB, Llava, SXM,  signal a syntax error.

Larceny signals a syntax warning but then treats the empty list as self-evaluating.

In XLisp, Rep, VX, `(car '())` and `(cdr '())` evaluate to the empty list, as in Common Lisp.