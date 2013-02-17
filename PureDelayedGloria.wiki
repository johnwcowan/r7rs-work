= Pure Delayed Evaluation =
 
== Rationale ==
 
Scheme provides delayed evaluation by the `(delay ...)` and `(force ...)`
forms.  However, the small language specifications do not mention the
interaction between delayed evaluation and threads.
 
In addition, the small language specifications (by virtue of an
example for delayed evaluation) require that promises should be
re-entrant, such that promises may also, while being forced, cause
themselves to be directly or indirectly forced.  However, it is not
clear (as of R7RS draft 9) which of the values returned by a
re-entrant delayed expression will be returned by which invocations of
force, especially if this re-entrance is not in tail position.
 
The interaction between the re-entrancy requirement and threads
generally implies the following:
 
1.  Since promise re-entrancy is a requirement of R7RS-small, but threads are not, then the re-entrancy requirement is more important and will be more likely to be implemented.
 
2.  This can mean that, in a multithreaded Scheme implementation, if multiple threads force the same promise, the delayed expression may be executed in parallel by multiple threads.  This is because a naive implementation of delayed evaluation simply uses a flag that is toggled only when the delayed expression calls its continuation/returns a value; multiple threads may enter the force code with the flag in the "not yet forced" state, and thus multiple threads might execute the delayed expression.
 
3.  A multithreaded Scheme implementation could set a flag that is toggled at the start of forcing instead, and to suspend other threads that attempt to force the promise when that flag is in the "started forcing" state; it can support direct re-entrancy by associating the flag with a thread, so that the same thread that re-enters a promise will not be suspended.  This enforces that even with multiple threads, the delayed expression will only be executed exactly once.  However, such a flag would have to be global to all promises; otherwise, consider this case: two promises p and q are re-entrant to each other (unless the specification specifically disallows mutual re-entrancy, but to require support for self-re-entrance but disallow mutual re-entrancy does not seem to fit Scheme's spirit).  Thread A starts forcing p, while thread B starts forcing q.  p's flag is set to "started forcing" and then associated with thread A, while q's flag is set to "started forcing" and then associated with thread B. Eventually, A forces q, which is in "started forcing" state but associated with thread B, and so suspends; thread B encounters the mirror situation.  Both threads are suspended, and the mutual re-entrancy does not follow the implications of the spec.
 
Finally, the small language specifications require that promises may
be used in iterative lazy algorithms, and requires the `(delay-force
...)` form to support iterative lazy algorithms.  This, together with
the re-entrancy requirement, constrains the possible implementations
to be practically just the implementation shown in SRFI-45.
 
This proposed library seeks to do the following:
 
1.  It specifies a particular interaction between delayed evaluation and threads, which users of this library must observe.
 
2.  It removes the re-entrancy requirement, to allow implementations other than the SRFI-45 one.
 
== Specification ==
 
`(pure-delay <expr>)`

 Creates a promise whose value can be forced by (pure-force ...).  The
 expression is not executed immediately.
 
`(pure-force <promise>)`

 Forces the delayed expression.
 
Conceptually, the user of this library can consider the promise to be
 in one of three states:
 
 * not-forced
 * being-forced
 * already-forced
 
The pure-force procedure attempts to move the state into
already-forced.  If the promise is already in that state, then the
promise's value is returned directly.
 
If the promise is in the not-forced state, then pure-force puts it
into the being-forced state, and executes the delayed expression using
the current dynamic environment.  If the expression returns normally
(i.e. it invokes its continuation) then its return value (which must
be a single value) is cached as the promise's value, and the promise
is put into the already-forced state.  If the expression does not
return normally (i.e. it invokes a continuation that is parent to the
`(pure-force ...) form)` then the promise is put into the not-forced
state (this tends to exclude implementations that implement threads
using Scheme-level continuations, however).

If the promise is in the being-forced state, then `(pure-force ...)`
shall suspend the current thread (to be revived later when the promise
leaves the being-forced state) if the promise was put into the
being-forced state by a different thread; the behavior is otherwise
unspecified if the promise was put into that state by the current
thread (i.e. re-entrancy, direct or indirect, is not allowed).  This
allows single-threaded Scheme implementations considerable leeway in
implementing pure promises (i.e. such implementations can just use
their normal R7RS-small promises), while constraining multithreaded
implementations to behavior that users can rely on.
 
If the delayed expression returns multiple times (i.e. it saves its
continuation and somehow causes it to be invoked multiple times) then
the behavior is unspecified.
 
`(pure-delay-force <expr>)`

This shall be similar to `(pure-delay (pure-force <expr>))` except that
additional space should not be retained; if called iteratively, then
average constant space should be consumed by `(pure-delay-force ...)`
 
== Sample Implementation ==
 
{{{
(define-library (blah pure-delay)
   (export pure-delay pure-force pure-delay-force)
   (import (scheme base))
   (import (srfi 18))
   (begin
 
(define-record-type :promise
   (promise mtx state data)
   promise?
   (mtx mtx)
   (state state state-set!)
   (data data data-set!))
 
(define (delay-force:f f)
   (promise (make-mutex) 'not-forced f))
 (define (delay:f f)
   (delay-force:f (lambda () (promise (make-mutex) 'already-forced (f)))))
 (define-syntax pure-delay
   (syntax-rules ()
     ((pure-delay x) (delay:f (lambda () x)))))
 (define-syntax pure-delay-force
   (syntax-rules ()
     ((pure-delay-force x) (delay-force:f (lambda () x)))))
 (define (pure-force p)
   (define (dispatch sub-p)
     (let ((exec
              (begin
                (mutex-lock! (mtx sub-p))
                (let ((d (data sub-p)))
                  (case (state sub-p)
                    ((already-forced)
                      (lambda ()
                        (mutex-lock! (mtx p))
                        (type-set! p 'already-forced)
                        (data-set! p d)
                        (mutex-unlock! (mtx p))
                        d))
                    ((being-forced) (lambda () (thread-sleep! 0)
 (dispatch sub-p)))
                    ((not-forced)
                      (type-set! sub-p 'shared)
                      (data-set! sub-p p)
                      (lambda () (dispatch (d))))
                    ((shared)
                      (lambda () (dispatch d))))))))
       (mutex-unlock! (mtx sub-p))
       (exec)))
   (let ((exec
            (begin
              (mutex-lock! (mtx p))
              (let ((d (data p)))
                (case (state p)
                  ((already-forced) (lambda () d))
                  ((being-forced) (lambda () (thread-sleep! 0) (pure-force p)))
                  ((not-forced)
                    (type-set! p 'being-forced)
                    (data-set! p #f)
                    (lambda () (dispatch (d))))
                  ((shared) (lambda () (pure-force d))))))))
     (mutex-unlock! (mtx p))
     (exec)))
 
))
}}}