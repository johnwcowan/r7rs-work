I tested datum-label support with the following expression:

```
        (let ((x '(#1=(a b) #1#))) (eq? (car x) (cadr x)))
```

Gauche, MIT, Bigloo, Kawa, Chez, Ikarus/Vikare, Larceny^*^, Mosh, STklos, FemtoLisp, Sagittarius, Foment, Picrin support the syntax and return `#t`;

Racket, Chicken, Scheme48/scsh, Guile, SISC, SCM, Larceny^*^, Ypsilon, IronScheme, NexJ, JScheme, KSi, SigScheme, TinyScheme, Scheme 9, RScheme, S7, XLisp, Rep, Schemik, Elk, SXM, Sizzle, Dfsch, Inlab, Oaklisp, Owl Lisp, Chibi (native mode) report lexical syntax errors;

Gambit, Shoe, BDC, UMB, Llava, Chibi (R7RS mode) return `#f` for whatever reason.

^*^Larceny v0.98 normally recognizes the syntax, but reports a lexical error in R6RS mode (as is required by the R6RS).
