## Complex logarithm

Guy Steele's three-part smoke test for Common Lisp involves evaluating `(atanh -2)`.  Traditionally, a Lisp passes if it returns a complex number; if it returns the *correct* complex number, so much the better.  The `atanh` function is not provided in R7RS-small, so I defined it as follows:

```
(define (atanh x)
    (/ (- (log (+ 1 x))
          (log (- 1 x)))
       2))
```

(Note:  KSi already defines `atanh` and will not allow its redefinition.)

Returns `-0.5493061443340549+1.5707963267948966i`, the *correct* complex number:  Racket, Gauche, MIT, Chicken with the numbers egg, Scheme48, Guile, Kawa, Chibi, Chez, Vicare, Larceny, Ypsilon, IronScheme, STklos, KSi, S7, Spark, Sagittarius

Returns NaN (i.e. it is attempting to use a real-number log function) when the argument is `2`, but returns the correct complex number when the argument is `2.0+0.0i`:  scsh, SISC, Mosh

Returns an incorrect complex number: SCM

No support for log of negative numbers: UMB, Owl Lisp

No complex numbers: plain Chicken, Bigloo, Ikarus, NexJ, SigScheme, Shoe, TinyScheme, Scheme 9, Dream, RScheme, BDC, XLisp, Rep, Schemik, Elk, VX, Oaklisp, Llava, SXM, Sizzle, FemtoLisp, Dfsch, Inlab.
