This test examines whether the SRFI 6 procedure `get-output-string` retrieves the value from a closed string output port.  The test code is:

```
(define p (output-string-port))
(write 32 p)
(close-output-port p)
(get-output-string p)
```

Returns 32:  Racket, Gauche, MIT, Gambit, Chicken, Kawa, Chez, IronScheme, STklos, Sizzle, Foment

Throws an exception saying the port is closed:  Bigloo, Guile, SCM, SigScheme, TinyScheme, RScheme, S7, Elk, SXM, Sagittarius, Picrin

Architecture-dependent: Chibi

Returns the empty string: Inlab

No built-in support for SRFI 6 (I did not attempt to load support from implementation libraries):  Scheme48/scsh, SISC, Vicare, Larceny, Ypsilon, Mosh, NexJ, JScheme, KSi, Shoe, Scheme 9, BDC, XLisp, Rep, Schemik, UMB, Llava, FemtoLisp, Dfsch, Oaklisp, Owl Lisp

