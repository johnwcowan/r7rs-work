## Variable to syntax

This example determines what the behavior of the usual implementations is when an identifier is defined as a variable and then redefined as syntax.  For example:

```
        (define (noodle) (foodle))
        (define (foodle) 23)
        (noodle) => ??
        
        (define-syntax foodle (syntax-rules ()
          ((foodle) 17)))
        (noodle) => ??
        
        (define (noodle) (foodle))
        (noodle) => ??
```

Essentially all the implementations that support `syntax-rules` behave the same way on the first and third calls to `noodle`, returning 23 and 17 respectively.  The exception is Owl Lisp, which has a hyperstatic REPL that disallows all forward references.

The first definition of `noodle` refers to `foodle`, which is undefined, and is therefore assumed to be a variable.  (Guile prints a warning at this point.)  The second definition of `noodle` refers to the current (syntax) definition of `foodle`.  The question is, what happens in the second call of `noodle`, when the first definition of `noodle` is being invoked, but the definition of `foodle` has changed out from under it?

`Noodle` continues to call the old definition of `foodle` and returns 23:  Racket, Gambit, Chicken, Bigloo, Scheme48/scsh, SISC, Chez, Vicare, Larceny, Mosh, IronScheme, STklos, KSi, SigScheme, SXM, Chibi

Complains that a non-procedure is being invoked:  Gauche, MIT, Guile, Kawa, SCM, Foment, Scheme 9, Sagittarius

Complains that `foodle` is not defined: Ypsilon

No support for `syntax-rules`: NexJ, JScheme, KSi, SigScheme, Shoe, TinyScheme, RScheme, S7, BDC, XLisp, Rep, Schemik, UMB, Elk, Llava, Sizzle, FemtoLisp, Dfsch, Inlab

## Syntax to variable

Now we look at what happens when an identifier is defined as syntax and then changed to be a variable instead.  Here's an example:

```
(define-syntax fard
  (syntax-rules ()
    ((fard a b) (- a b))))

(fard 1 2) => -1

(define (fard a b) (+ a b))

(fard 1 2) => ??

(apply fard (list 1 2)) => ??
```

`fard` is successfully redefined in both operator and operand positions:  Racket, Gauche, MIT, Gambit, Scheme48/scsh, Guile, Kawa, SCM, Chez, Vicare, Larceny (prints a warning), Ypsilon, Mosh, KSi, Foment, Owl Lisp, Chibi, Sagittarius

`fard` is successfully redefined in operand position, but is still a syntax keyword in operator position: Chicken, Bigloo (prints a warning), STklos, Picrin

`fard` is successfully redefined in operator position, but is still a syntax keyword in operand position: Scheme 9

`fard` cannot be redefined: IronScheme, SXM

No support for `syntax-rules`: NexJ, JScheme, KSi, SigScheme, Shoe, TinyScheme, RScheme, S7, BDC, XLisp, Rep, Schemik, UMB, Elk, Llava, Sizzle, FemtoLisp, Dfsch, Inlab

