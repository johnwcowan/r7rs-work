This page talks about how implementations handle various aspects of zero.  Thanks to Alexey Radul for the ideas here; the research is mine.

=== Exact division by zero ===

Racket, MIT, Gambit, Chicken, Bigloo, Scheme48/scsh, Chibi, Guile, SISC, Chez, !Ikarus/Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, STklos, Shoe, S7, BDC, XLisp, Rep, Schemik, Elk, UMB, !SigScheme, SXM, Sizzle, Spark, Dfsch, Inlab, VSCM, Sagittarius report an error.

Gauche, SCM, KSi, VX return an inexact infinity.

Kawa returns one of two ''exact'' infinities depending on the sign of the numerator.  These are notated `1/0` and `-1/0`.  They are the same in the sense of `eqv?`, anomalously so since they are not operationally equivalent.  They are also distinct in the sense of `eqv?` from `+inf.0` and `-inf.0`, but `1/0` and `+inf.0` are the same in the sense of `=`, as are `-1/0` and `-inf.0`.

Scheme 9 returns its implementation-specific undefined value.

Femtolisp returns an incorrect result.

=== Inexact division by exact zero ===

Racket, Gambit, Chicken, scsh, Chibi, Guile, Shoe, S7, BDC, XLisp, Rep, Schemik, Elk, SXM, Sizzle, Spark, Dfsch, Inlab, VSCM report an error.

MIT (with floating traps ignored), Gauche, Bigloo, Scheme48, Kawa, SISC, SCM, Chez, !Ikarus/Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, STklos, UMB, VX, Sagittarius return an inexact infinity.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

Scheme 9 returns its implementation-specific undefined value.

KSi, Femtolisp return an incorrect result.

=== Division by inexact zero ===

Plain Chicken, scsh, Shoe, !TinyScheme, XLisp, Rep, Schemik, S7, SXM, Sizzle, Dfsch, Inlab, VSCM report an error.

Racket, Gauche, MIT (with floating traps ignored), Gambit, Chicken with the numbers egg, Bigloo, Scheme48, Guile, Kawa, SISC, Chibi, SCM, Chez, SCM, !Ikarus/Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, STklos, BDC, Elk, UMB, VX, Spark, Femtolisp, Sagittarius return an inexact infinity.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

Scheme 9 returns its implementation-specific undefined value.

KSi returns an incorrect result.

=== Inexact multiplication by exact zero ===

Racket, MIT, Gambit, Chez, Ypsilon, !TinyScheme, XLisp, Elk, SXM, Sizzle, Spark, Inlab return exact `0`.

Gauche, Chicken, Bigloo, Scheme48/scsh, Guile, Kawa, SISC, Chibi, SCM, !Ikarus/Vicare, Larceny, Mosh, !IronScheme, NexJ, STklos, KSi, Shoe, Scheme 9, S7, BDC, Rep, Schemik, UMB, VX, Femtolisp, Dfsch, VSCM, Sagittarius return inexact `0.0`.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

=== Multiplying NaN by exact 0 ===

Racket, MIT, Chez, Ypsilon, Elk, SXM, Sizzle, Spark, Inlab return exact `0`.

Gauche, Gambit, Chicken, Bigloo, Scheme48/scsh, Guile, Kawa, SISC, Chibi, SCM, Vicare, Larceny, !IronScheme, NexJ, STklos, KSi, Shoe, !TinyScheme, RScheme, S7, BDC, XLisp, Rep, Schemik, UMB, VX, Llava, !FemtoLisp, Dfsch, Sagittarius return `+nan.0`.

Scheme 9 does not have any form of NaN.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

=== Dividing exact zero by an inexact number ===

Racket, Gambit, !TinyScheme, Sizzle, Spark return exact `0`.

Gauche, Chicken (with or without the numbers egg), Bigloo, Scheme48/scsh, Guile, Kawa, SISC, Chibi, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, STklos, KSi, Shoe, Scheme 9, S7, BDC, XLisp, Rep, Schemik, UMB, VX, Llava, SXM, Dfsch, Inlab, Sagittarius return inexact `0.0`.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

Femtolisp returns the wrong answer.

=== Complex numbers with 0.0 imaginary part. ===

Gauche, MIT, Chicken with the numbers egg, Scheme48/scsh, Kawa, SISC, SCM, STklos, KSi, S7, UMB, Spark, Dfsch, VSCM consider `3.0+0.0i` to be a real number.

Racket, Gambit, Guile, Chibi, Chez, Vicare, Larceny, Ypsilon, Mosh, !IronScheme, Sagittarius do not.

Plain Chicken, Bigloo, Ikarus, NexJ, !SigScheme, Shoe, !TinyScheme, Dream, Scheme 9, BDC, XLisp, Rep, Schemik, Elk, VX, Oaklisp, SXM, Sizzle, Femtolisp, Inlab, Owl Lisp do not implement non-real numbers.

=== Negative zero ===

I did a test of:

{{{
(let* ((minf (* 1.0e200 -1.0e200))
       (mzero (/ 1.0 minf)))
  (list minf mzero (eqv? mzero 0.0)))
}}}

Gauche, Chicken, Bigloo,  Scheme48/scsh, SISC, Chibi, SCM, Ikarus,
Ypsilon, Mosh, !IronScheme, NexJ, STklos, Shoe, !TinyScheme, RScheme,
S7, BDC, XLisp, Schemik, Elk, UMB, VX return some variant of
`(-inf.0 -0.0 #t)`.

Racket, Gambit, Guile, Kawa, Chez, Vicare, Larceny,
Rep, Sagittarius return some variant of `(-inf.0 -0.0 #f)`.

I wasn't able to generate `-0.0` on MIT, KSi, !SigScheme, Scheme 9, Dream, Oaklisp, Owl Lisp.

These are the R6RS examples involving -0.0 (already accounted for verbally in the "Implementation extensions" section of R7RS):

{{{
(zero? -0.0)  => #t                ; -0.0 is a Scheme zero

(+ 0.0 0.0)   => 0.0               ; Sum is -0.0 only if all arguments are -0.0
(+ 0.0 -0.0)  => 0.0
(+ -0.0 0.0)  => 0.0
(+ -0.0 -0.0) => -0.0

(- 0.0)       => -0.0             ; Negation flips the sign of zero
(- -0.0)      => 0.0

(- 0.0 -0.0)  => 0.0              ; Negate all arguments but the last and then add
(- -0.0 0.0)  => -0.0
(- 0.0 0.0)   => 0.0
(- -0.0 -0.0) => 0.0

(log -1.0+0.0i) => 0.0+3.141592653589793i ; approximately
(log -1.0-0.0i) => 0.0-3.141592653589793i ; approximately

(angle -1.0+0.0i) => 3.141592653589793    ; approximately
(angle -1.0-0.0i) => -3.141592653589793   ; approximately
}}}

=== Angle of negative zero ===

Because of the branch cut of `atan`, the values of `(angle 0.0)` and `(angle -0.0)` should be 0 and pi respectively.  Furthermore, by R5RS and R6RS, `(atan y x)` is equivalent to `(angle (make-rectangular x y))` even on systems that don't support non-real numbers, so `(atan 0.0 -0.0)` should also be pi.  On many implementations, these turn out not to be the case.  

Gambit, Chicken with the numbers egg, Guile, Chez, Vicare, Larceny, !IronScheme, STklos, Sizzle return pi to both `angle` and `atan`.

Gauche, plain Chicken, SISC, Sagittarius return `0.0` to `angle`, pi to `atan`.

Scheme48 returns `-0.0` to `angle`, pi to `atan`.

scsh returns an error to `angle`, pi to `atan`.

KSi returns NaN to `angle`, an error to `atan`.

Racket, Spark return pi to `angle`, `0.0` to `atan`.

SISC, Mosh, S7 return `0.0` to both expressions.

MIT does not support `-0.0`.

NexJ, RScheme, BDC, XLisp, Rep, Elk, Inlab do not support `angle`, but returns pi to `atan`.

Bigloo, SXM do not support `angle`, but raise an error to `atan`.

VX does not support `angle`, but returns `-0.0` to `atan`.

UMB does not support `angle`, but returns `0.0` to atan.

Scheme 9 supports neither `angle` nor two-argument `atan`.

Shoe, !TinyScheme, Schemik, Llava, !FemtoLisp do not support either `angle` or `atan`.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

=== Inexact zero raised to a negative real power ===

This situation involves the log of 0, and R6RS and R7RS permit implementations to either raise an exception or return an arbitrary number.

Racket, Gauche, Gambit, Chicken, Bigloo, scsh, Kawa, Chibi, SCM, Chez, Ypsilon, Mosh, NexJ, STklos, XLisp, Rep, UMB, SXM, Spark, Dfsch, Inlab, Sagittarius return `inf.0`.

Guile, Larceny return `+nan.0`.

Scheme 9 returns its implementation-specific undefined value.

SISC, !IronScheme return `0.0`.

Elk returns `0`.

Sizzle returns its smallest representable integer (-2^32^).

MIT (even ignoring floating point traps), Scheme48, KSi, S7 raise an exception.

!SigScheme, Dream, Oaklisp, Owl Lisp do not support inexact numbers.

!TinyScheme, Schemik, Femtolisp, BDC either don't provide `expt` or don't provide `log`.

=== Inexact zero raised to a complex power whose real part is negative ===

Racket, Gauche, Gambit, Chicken with the numbers egg, Kawa, Chibi, Ypsilon, Spark, Sagittarius return `+nan.0+nan.0i`.

Guile returns `+nan.0+nan.0i` or `+inf.0+nan.0i` depending on the platform.

SISC, Chez, Vicare, !IronScheme return `0.0`.

SCM, Dfsch return `+inf.0`.

Larceny returns `+nan.0`.

MIT (even when ignoring floating point traps), Scheme48/scsh, STklos, UMB raise an exception.

Mosh returns its implementation-specific "undefined" value.

Plain Chicken, Bigloo, Ikarus, NexJ, !SigScheme, Shoe, !TinyScheme, Scheme 9, Dream, RScheme, BDC, XLisp, Rep, Schemik, Elk, VX, Oaklisp, Femtolisp, Inlab, Owl Lisp do not support non-real numbers.