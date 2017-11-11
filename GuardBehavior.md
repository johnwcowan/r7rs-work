Helmut Eller asked what this example should return?

```
(let ((events '()))
   (guard (c 
           (#t #f))
     (guard (c
             ((dynamic-wind 
                  (lambda () (set! events (cons 'c events)))
                  (lambda () #f)
                  (lambda () (set! events (cons 'd events))))
              #f))
       (dynamic-wind 
           (lambda () (set! events (cons 'a events)))
           (lambda () (raise 'error))
           (lambda () (set! events (cons 'b events))))))
   (reverse events))
```

Is it `(a b c d a b)` or `(a c d b)` or `(a b c d)` or unspecified?

Aaron Hsu replied:

The important parts here are the dynamic extent in which the cond-clauses are evaluated, and the dynamic extent of the implicit `raise` that occurs if none of the clauses fire. The extent/continuation of the `cond` evaluation is that of the whole `guard`, whereas the re-raise is that of the original `raise`.

This means that the first raise will trigger the A and B setters, and then the C and D setters will trigger. At this point, since the result is `#f`, the implementation should re-rais the object from the original calling extent, thus triggering A and B setters again, before finally returning without re-entering again.

This gives `(a b c d a b)` as the only valid result.

My tests:

We do indeed get `(a b c d a b)` from Chez, Ikarus, Vicare, Larceny, Ypsilon, Mosh, Chibi, Sagittarius.

However, IronScheme, Racket (in #lang r6rs mode), STklos all return `(a b c d)`.  In the case of IronScheme at least, that is because it supports escape continuations only, and so the outer continuation in `guard` cannot re-enter the dynamic extent of the body.

SigScheme returns `(a c d b)`, apparently evaluating the test of the cond-clause in the dynamic environment of `raise` and unwinding the stack only when the test returns true.  That's arguably "better" that `(a b c d)` as this will call other handlers in the correct environment if the test returns `#f`.

The other Schemes all report errors, typically about `guard` or `raise` being undefined, or that `(#t #f)` is not a valid procedure call.
