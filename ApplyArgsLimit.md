These tests try to find out the practical limits of the length of *list* in `(apply + `*list*`)`.  I used `make-list` to construct a list of one million zeros and invoked the above `apply`.  If it worked, I tested no further; if it crashed, I read the error messages and when necessary tested with shorter lists.

These tests were done on a 64-bit Linux system.

1000000 arguments works correctly:  Racket, Gauche, Bigloo, Kawa, SISC, SCM, Larceny, IronScheme, NexJ, JScheme, SigScheme, Scheme 9, KSi, Shoe, Rep, Schemik, FemtoLisp, Dfsch, Inlab, Oaklisp, Sagittarius, Chibi

500000 arguments works correctly: Scheme48/scsh

100000 arguments works correctly: S7, Chicken 5

10000 arguments works correctly: Guile, Mosh, XLisp, TinyScheme, Elk, Llava, SXM, S7

6500 arguments works correctly: Sizzle

1000 arguments works correctly: BDC

Hard limit is 126937 arguments: MIT

Hard limit is 8192 arguments: Gambit, Vicare

Hard limit is 2048 arguments: Chicken 4 

Hard limit is 2026 arguments: Foment

Hard limit is 998 arguments: RScheme

Cannot run test: UMB, Owl Lisp
