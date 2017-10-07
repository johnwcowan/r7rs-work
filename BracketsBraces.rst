Here are the ways in which the test suite of Schemes interprets square brackets and curly braces.  Both are undefined in R5RS.  In R6RS, square brackets are synonyms for parentheses, but curly braces are still undefined.

Brackets are:

 * synonyms for parentheses:  Racket, Gauche, Gambit, Chicken, SXM, Spark, Bigloo, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, STklos, KSi, Oaklisp, Sizzle, Guile, SISC, Chez, Elk, Sagittarius

 * lexical syntax errors:  MIT, Scheme48/scsh, NexJ, !SigScheme, Schemik, Owl Lisp, RScheme, Scheme 9

 * vector constructors:  Rep, !FemtoLisp

 * redefinable constructors:  Kawa

 * identifier characters:  SCM, Shoe, !TinyScheme, Dream, S7, XLisp, UMB, VX, Llava, Dfsch, Inlab, Chibi, BDC

Braces are:

 * synonyms for parentheses:  Racket, Gauche, Gambit, Chicken, SXM, Spark

 * lexical syntax errors:  MIT, Scheme48/scsh, NexJ, !SigScheme, Schemik, Owl Lisp, Bigloo, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, STklos, KSi, Oaklisp, Sizzle, Scheme 9

 * user-defined type literals:  Chibi

 * containers for embedded C code: RScheme

 * identifier characters:  SCM, Shoe, !TinyScheme, Dream, S7, XLisp, UMB, VX, Llava, Dfsch, Inlab, Kawa, Rep, !FemtoLisp, Guile, SISC, Chez, Elk, BDC, Sagittarius

Note:  In some Schemes where these are identifier characters, they are self-delimiting: that is: `{abc}` is equivalent to `{ abc }`.