This page documents what happens to [SRFI 39](http://srfi.schemers.org/srfi-39/srfi-39.html)
parameters that have been mutated from their original values (as opposed to bound by
`parameterize`) when a new thread is spawned.  SRFI 39 documents three known behaviors:

(a) Create a new parameter corresponding to the old one and reinitialize it.
Scheme 48.

(b) Share the parameter across both threads so that mutating it in either thread will
affect the other thread.
Chibi, Chez, Gambit, Kawa.

(c) Copy the old parameter so that it contains the mutated value;
the two objects can be mutated separately.
Chicken, Racket, Guile, Gauche

Unknown behavior: Mosh, Larceny, Sagittarius, STklos, Ypsilon.

This page has not been tested directly against the implementations;
it depends only on documentation and discussion with the implementers.

In all known Schemes, binding a parameter has independent effects in all threads.
R7RS-small requires this behavior.

R7RS-large does not specify how to mutate a parameter, but does not forbid it either.
This means that (b) and (c) are indistinguishable.

Chez also provides `make-thread-parameter` with (c) semantics.

MIT by default does not allow mutation, but has a separate type of settable parameters
which do allow mutation.  The thread behavior of this type is unknown.

Test code:
```
(define p (make-parameter 5))
(p 17)
(define (thunk)
  (display (p)) ; (a) displays 5, (b) and (c) display 17
  (newline)
  (p 24)
  (display (p)) ; should display 24
  (newline))
(define t (make-thread thunk))
(begin
  (start-thread! t)
  (join-thread! t))
(p) ; (b) displays 24, (c) displays 17
```



