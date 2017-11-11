I created a file named "three" whose content was "3", and ran the following code in the test Schemes:

```
(define port #f)
(call-with-input-file "three"
  (lambda (p)
    (set! port p)
    (+ 'a 'b)))  ; signals an exception
(read port)
```

to see if the final `(read port)` would return 3 (meaning that the port was still open after the exception was raised) or would fail with a new exception, something like "closed port".

Printed `3`:  Racket, MIT, Chicken, Scheme48/scsh, Guile, Chibi, SCM, Chez, Vicare, Larceny, Ypsilon, Mosh, STklos, KSi, SigScheme, TinyScheme, Dream, RScheme, XLisp, Elk, VX, SXM, Spark, Inlab, Scheme 9, Sagittarius

Failed with closed-port exception: Gauche, Bigloo, Kawa, SISC, IronScheme, NexJ, S7, BDC, Sizzle

Failed for other reasons (typically because `call-with-input-file` is not supported): Shoe, Rep, Schemik, UMB, Oaklisp, Llava, Femtolisp, Dfsch, Owl Lisp
