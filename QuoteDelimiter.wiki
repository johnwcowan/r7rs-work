Survey of implementations to see which treat ' as a delimiter that terminates a symbol:

I tested `(list 'a'b)` on the usual suite of Schemes.

Returned `(a b)`:  Racket, Gauche, MIT, Gambit, Chicken, Scheme48/scsh,
Kawa, Chibi, Chez, Vicare, NexJ, JScheme, STklos, KSi, !SigScheme, Scheme 9, RScheme, BDC,
XLisp, Rep, Oaklisp, Spark, Llava, !FemtoLisp, Foment, Owl Lisp, Sagittarius

Returned `(a'b)`:  Bigloo, Guile, SISC, SCM, Shoe, !TinyScheme, Dream,
S7, Schemik, Elk, UMB, VX, Sizzle, Dfsch, Inlab

Syntax error: Ikarus, Larceny, Ypsilon, Mosh, !IronScheme, SXM, MScheme

I suspect that a lot of implementations think it's simplest to
just gobble up what they see until they get to an unquestioned
delimiter.