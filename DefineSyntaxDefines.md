I tried out

{{{
(define-syntax foo
    (syntax-rules ()
      ((foo) (define x 32))))
(foo) => ???
x => ???
}}}

to see whether I got 32 as the value of x or blew up on an error such
as invalid syntax (in the define-syntax) or undefined variable.

Racket, MIT, Gambit, scsh/Scheme48, SISC, Chez, !Ikarus/Vicare, Ypsilon, Mosh !IronScheme, NexJ, KSi, !SigScheme, Shoe, !TinyScheme, Dream, RScheme, S7, XLisp, Rep, Schemik, ELK, UMB, VX, Oaklisp, Llava, Sizzle, Spark, Femtolisp, Dfsch, Inlab, Owl Lisp, Sagittarius blew up (some of them because they don't support syntax-rules).

Gauche, Chicken, Bigloo, Guile, Kawa, Chibi, SCM, Larceny (in R5RS mode), Scheme 9,
STklos, !SigScheme, BDC, SXM, VSCM were fine with it and returned 32.