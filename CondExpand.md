Supporting `cond-expand` from SRFI-0 and R7RS at the REPL?

Supported: Gauche, MIT, Chicken, Bigloo, Kawa, SISC, STklos, SigScheme, Scheme 9, RScheme, S7, SXM, Foment, Chibi, Sagittarius, Guile

Not supported: Racket* , Scheme48/scsh, SCM, Chez, Vicare*, Larceny*, Ypsilon*, Mosh, IronScheme, NexJ, JScheme, KSi, Shoe, TinyScheme, BDC, XLisp, Rep, Schemik, Elk, Llava, Sizzle, FemtoLisp, Dfsch, Inlab, Owl Lisp

*Said to support SRFI 0 through [a third-party library](https://code.launchpad.net/~scheme-libraries-team/scheme-libraries/srfi); not tested

Note:  Although [SRFI 0](http://srfi.schemers.org/srfi-0/srfi-0.html) says that `cond-expand` can only be portably used at the top level, all the above Schemes support nested `cond-expand`.  R7RS allows `cond-expand` anywhere a syntactic expression is permitted (it's also a library declaration).
