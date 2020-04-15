I tested the usual suite of Schemes with `'[a b c]`
to see how they reacted to the square brackets.
R6RS requires the result to be `(a b c)`.

R6RS, so evaluates to `(a b c)`: Racket, Guile, SISC,
Vicare, Larceny, Ypsilon, Mosh, IronScheme, Sagittarius

Non-R6RS but evaluates to `(a b c)`: Gauche, Gambit, Chicken, Bigloo, SISC,
Scheme 9, STklos, KSi, Schemik, Elk, Oaklisp,
SXM, Sizzle, Spark

Evaluates to the symbol `[a`, followed by undefined-variable error on `b`:
Chibi, SCM, Shoe, TinyScheme, Dream, S7, BDC, XLisp, UMB, VX, Llava, Dfsch, Inlab

Syntax error: MIT, Scheme48/scsh, NexJ, SigScheme, RScheme, Owl Lisp

Evaluates to `#(a b c)`: Rep, FemtoLisp

Evaluates to `($bracket-list$ a b c)`: Kawa
