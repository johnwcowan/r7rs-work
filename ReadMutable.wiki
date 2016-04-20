The following test from the REPL was used to determine which Schemes, if any, return immutable pairs from calling `read` on the input `(a . b)`:

{{{
(define x (read))
(a . b)
(set-car! x 'b)
x
}}}

!IronScheme, S7, Dfsch, Inlab don't support (read) direct from the command line, so I substituted reading from a file that contained nothing but `(a . b)`.

Output `(b . b)` per R![57]RS: Gauche, MIT, Gambit, Chicken, Bigloo, Scheme48/scsh, Guile, Kawa, SISC, Detroit, SCM, Chez, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, JScheme, STklos, KSi, !SigScheme, !MiniScheme, !TinyScheme, S7, Scheme 9, RScheme, Unlikely, SIOD, BDC, XLisp, Elk, SXM, Sizzle, !FemtoLisp, Inlab, Sagittarius, Foment, Picrin, Chibi

All pairs are immutable: Racket, Owl Lisp

Returned pair is immutable: JScheme

No usable `read`, `set-car!` or `open-input-file`: Rep, Shoe, UMB, Llava, Dfsch

