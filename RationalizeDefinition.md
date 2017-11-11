This implementation of `rationalize` is taken from the IEEE Scheme standard, which is not freely available.  The code is by Alan Bawden, and the theory is from Hardy and Wright's *Introduction to the Theory of Numbers*, 5th edition (1979).

```
(define (rationalize x e)
  (simplest-rational (- x e) (+ x e)))
(define (simplest-rational x y)
  (define (simplest-rational-internal x y)
    ;; assumes 0 < X < Y
    (let ((fx (floor x))
          (fy (floor y)))
      (cond ((not (< fx x))
             fx)
            ((= fx fy)
             (+ fx
                (/ (simplest-rational-internal
                    (/ (- y fy))
                    (/ (- x fx))))))
            (else
             (+ 1 fx)))))
  ;; do some juggling to satisfy preconditions
  ;; of simplest-rational-internal.
  (cond ((< y x)
         (simplest-rational y x))
        ((not (< x y))
         (if (rational? x) x (error)))
        ((positive? x)
         (simplest-rational-internal x y))
        ((negative? y)
         (- (simplest-rational-internal (- y)
                                        (- x))))
        (else
         (if (and (exact? x) (exact? y))
             0
           0.0))))
```
