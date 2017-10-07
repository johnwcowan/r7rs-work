== Symbols ==

Racket, Gauche, Gambit, Chicken, Bigloo, scsh, Guile, Kawa, Chibi, Chez, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, KSi, !SigScheme, Shoe, RScheme, S7, BDC, Rep, Schemik, Llava, Sizzle, Spark, Femtolisp, Dfsch, Owl Lisp, Sagittarius return `#f` to `(eq? 't 'T)`.

MIT, Scheme48, SISC, SCM, STklos, !TinyScheme, Scheme 9, Dream, XLisp, Elk, UMB, VX, Oaklisp, SXM, Inlab return `#t`.

== Preferred case is upper case ==

I also decided to check the case-insensitive implementations using `(string=? (symbol->string 't) "T")` to see which ones converted symbols to upper case internally.  Only XLisp (which is not really Scheme-compliant) and Oaklisp (which is somewhat R3RS-compliant) did so.

== Character names ==

Is `#\Space` a valid constant?

Racket, Gauche, MIT, Bigloo, Scheme48, Guile, Kawa, Chibi, Chez, NexJ, STklos, KSi, !SigScheme, !TinyScheme, Scheme 9, Dream, RScheme, Rep, Elk, Oaklisp, Llava, SXM, Spark, Dfsch, Inlab accept `#\Space`.

Gambit, Chicken, scsh, Vicare, Ypsilon, Mosh, !IronScheme, Shoe, S7, Schemik, UMB, Sizzle, !FemtoLisp, Owl Lisp, Sagittarius reject `#\Space`.

== Boolean constants ==

Gambit, !TinyScheme, S7, VX, Sizzle, Owl Lisp do not accept `#T` as equivalent to `#t`.

The other Schemes in the test suite do.

== Inexact constants ==

All Schemes in the test suite accept `E` as well as `e` in inexact constants (except those without inexact number support).
