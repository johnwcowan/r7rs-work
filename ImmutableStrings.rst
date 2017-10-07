This is an attempt to determine which Schemes treat literal strings as immutable, using the code

{{{
(define y "abcde")
(string-set! y 0 #\c)
y
}}}

Mutates `y` to the value `"cbcde"`: MIT, Gambit, Chicken, Bigloo, Detroit, Stalin, Scheme->C, SCM, Chez, Vicare, Larceny, Ypsilon, JScheme, RScheme, S7, Unlikely, BDC, XLisp, UMB, Elk, SXM, Inlab, Foment, Picrin, Chibi

Raises an error about mutating an immutable object: Racket, Gauche, Scheme48/scsh, Guile, Kawa, SISC, Mosh, !IronScheme, STklos, KSi, !SigScheme, !TinyScheme, Scheme 9, Sizzle, Sagittarius

Does not support `string-set!`: NexJ, Shoe, Rep, Llava, !FemtoLisp, Dfsch, Owl Lisp (no mutation)

Does not support `#\c`: !MiniScheme, SIOD, Schemik