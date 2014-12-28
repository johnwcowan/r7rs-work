This page reports the results of the expressions `(letrec ((x 0) (y x)) y)` and `(letrec* ((x 0) (y x)) y)`.  The former is not defined (in R6RS, it must return an error) and the latter should always return 0, provided `letrec*` is supported (it is required by R6RS and R7RS, but not by earlier reports).  It also looks at
{{{
(let ((z 0))
  (define x 0)
  (define y x)
y)
}}}

which in R5RS and earlier is defined to be the same as the `letrec` version, and in R6RS and R7RS is defined to be the same as the `letrec*` version.

Note: UMB does not support any of these constructions.


== letrec ==

Reports an error: MIT, Scheme48/scsh, Chez, Vicare, Ypsilon, Mosh, !IronScheme, !SigScheme, !TinyScheme

Returns 0: Racket, Gauche, Guile, Kawa, SISC (with a warning), SCM, Larceny, NexJ, JScheme, KSi, RScheme, XLisp, Rep, Schemik, Elk, Llava, SXM, Sizzle, !FemtoLisp, Dfsch, Inlab, Foment, Owl Lisp, Chibi, Sagittarius

Returns a different value: Gambit, Chicken, Bigloo, STklos, Scheme 9, S7, BDC

Does not support `letrec`: Shoe

== letrec* ==

Returns 0: Chicken, Bigloo, Scheme48/scsh, Guile, Chez, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, KSi, Scheme 9, S7, Foment, Chibi, Sagittarius

Does not support `letrec*`: Racket, Gauche, MIT, Gambit, Kawa, SISC, SCM, JScheme, STklos, !SigScheme, Shoe, !TinyScheme, RScheme, BDC, XLisp, Rep, Schemik, Elk, Llava, SXM, Sizzle, !FemtoLisp, Dfsch, Inlab, Owl Lisp

== internal definition ==

Reports an error: Scheme48/scsh, !SigScheme

Returns 0: Racket, Gauche, MIT, Chicken, Bigloo, Guile, Kawa, SISC (with a warning), SCM, Chez, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, JScheme, STklos, KSi, Shoe, !TinyScheme, RScheme, S7, BDC, XLisp, Rep, Schemik, Elk, Llava, Sizzle, !FemtoLisp, Dfsch, Inlab, Foment, Owl Lisp, Chibi, Sagittarius

Returns a different value: Gambit, Scheme 9, SXM