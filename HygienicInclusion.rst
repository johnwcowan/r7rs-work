Are inclusions hygienic?

Given the macro

{{{
(define-syntax m
  (syntax-rules ()
    ((_) (lambda (a) (include "add.scm")))))
}}}

where `add.scm` contains `(+ a 1)`, is the `a` that is bound inside the macro the same as the `a` in `add.scm`?  In particular, does `((m) 32)` evaluate to 33, or is `a` reported as an undefined variable?

Evaluates to 33: Racket, Gambit, Guile, Kawa, SISC, Chez, Mosh, !IronScheme, SXM

`a` is undefined: Gauche, Chicken, Chibi, STklos, S7, Foment, Owl Lisp

`include` is undefined or broken: MIT, Bigloo, Scheme48/scsh, SCM, Vicare, Larceny, Ypsilon, Scheme 9, Sagittarius

No syntax-rules macros: NexJ, JScheme, KSi, RScheme, !SigScheme, Shoe, !TinyScheme, Dream, BDC, XLisp, Rep, Schemik, Elk, UMB, Oaklisp, Llava, Sizzle, !FemtoLisp, Dfsch, Inlab
