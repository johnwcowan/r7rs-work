[λ* : Beyond Currying](http://webyrd.net/scheme-2013/papers/HemannCurrying2013.pdf)

Notes:

The proposed name is `lambda*`, not `λ*`.

The proposal is Variant 5, which has no support for zero-argument or zero-or-more-arguments procedures, with the additional feature of Variant 8, supporting multi-expression bodies.  Section 4 says "nullary `λ∗` functions seem to be of little to no use", and I agree:  they seem to be provided only so that `lambda*` can fully subsume `lambda`, which I think is a mistake (it makes detecting an error in the number of arguments at compile time impossible).

Here is the (not yet debugged) source code:

```
(import (scheme case-lambda))

(define-syntax lambda*
  (syntax-rules ()
    ((_ a* e* ...)
     (lambda*-h a* (let () e* ...)))))

(define-syntax lambda*-h
  (syntax-rules ()
    ((_ (a a* ...) e) (posary-h (a a* ...) e))
    ((_ (a a* ... . rest) e)
     (polyvariadic-h (a a* ... . rest) e))))

(define-syntax posary-h 
  (syntax-rules ()
    ((_ (a a* ...) e)
     (letrec
       ((rec
         (case-lambda
           (() rec)
           ((a a* ...) e)
           ((a a* ... . rest)
            (apply (rec a a* ...) rest))
           (some ((lambda more (apply rec (append some more))))))))
        rec))))

(define-syntax polyvariadic-h
  (syntax-rules ()
    ((_ (a a* ... . rest) e)
    (letrec
      ((rec 
        (case-lambda
          (() rec)
          ((a a* ... . rest) e)
          (some ((lambda more (apply rec (append some more))))))))
      rec))))

```

