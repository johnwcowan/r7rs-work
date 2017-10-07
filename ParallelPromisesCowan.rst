== Rationale ==

''Parallel promises'', known to Racketeers as ''[[http://docs.racket-lang.org/reference/futures.html|futures]]'', are
analogous to ordinary Scheme promises, except that implementations are allowed to execute part or all of such a promise
in parallel with regular execution.  An ordinary promise is not evaluated (and does not perform any side effects)
until it is forced, whereas parallel promises may be evaluated either completely or up to an implementation-determined
point when they are created.  If any evaluation is left to do when a parallel promise is forced, it is done
at that time.

A parallel promise, unlike a thread, is intended to provide parallelism for potentially limited computations.
It performs its work in parallel (assuming that support for parallelism is available) until it detects
an attempt to perform an operation that is too complex for the system to run safely in parallel.  The list
of such operations is necessarily implementation-dependent.  Similarly, work on a parallel promise is suspended
if it depends in some way on the current continuation, such as raising an exception.

Parallel promises are not intended to be used for concurrency, so there are no facilities for mutual exclusion.
A computation in a parallel promise might use `set!` to modify a shared variable, in which case concurrent
assignment to the variable can be visible in other parallel promises or threads. Furthermore, guarantees
about the visibility of effects and ordering are determined by the operating system and hardware — which rarely support,
for example, any guarantee of sequential consistency.
At the same time, operations that seem obviously safe may have a complex enough implementation internally
that they cannot run in parallel on a particular implementation.

== Issues ==

1) Use "future" instead of "parallel promise" along with the Racket names?

2-3) Resolved.

== Specification ==

`(parallel-delay `<expression>`)` [syntax]

Semantics: The `parallel-delay` construct is used together with
the procedure `parallel-force` to implement potentially parallel evaluation
`(parallel-delay `<expression>`)` returns an object called a
''parallel promise'' which at some point in the future can be asked (by
the `parallel-force` procedure) to complete the evaluation of <expression>, and deliver
the resulting value. The effect of <expression> returning
multiple values is unspecified.  (Racket `future` is a procedure taking a thunk)

`(parallel-delay-force `<expression>`)`  [syntax]

Semantics: The expression `(parallel-delay-force `<expression>`)` is
conceptually similar to `(parallel-delay (parallel-force `<expression>`))`, with
the difference that forcing the result of `parallel-delay-force` will
in effect result in a tail call to `(parallel-force `<expression>`)`, while
forcing the result of `(parallel-delay (parallel-force `<expression>`))` might
not. Thus iterative lazy algorithms that might result in a
long series of chains of `delay` and `force` can be rewritten
using `delay-force` to prevent consuming unbounded space
during evaluation.

`(parallel-force `''parpromise''`)`

The `parallel-force` procedure forces the value of a parallel promise created
by `parallel-delay`, `parallel-delay-force`, or `make-parallel-promise`. If no value has
been computed for the promise, then a value is computed
and returned. The value of the promise must be cached
(or ''memoized'') so that if it is forced a second time, the
previously computed value is returned. Consequently, a
delayed expression is evaluated using the parameter values
and exception handler of the call to `parallel-force` which first requested
its value. If ''parpromise'' is not a parallel promise, it may be
returned unchanged.  (Racket `touch`)

Various extensions to the semantics of `parallel-delay`, `parallel-force` and
`parallel-delay-force` may be supported:

* Calling `parallel-force` on an object that is not a parallel promise may simply return the object.

* It may be the case that there is no means by which a parallel promise can be operationally distinguished from its forced value. That is, expressions like the following may evaluate to either `#t` or to `#f`, depending on the implementation:

{{{
(eqv? (parallel-delay 1) 1) => unspecified
(pair? (parallel-delay (cons 1 2))) => unspecified
}}}

* Implementations may implement ''implicit forcing'', where the value of a parallel promise is forced by procedures that operate only on arguments of a certain type, like `cdr` and `*`. However, procedures that operate uniformly on their arguments, like `list`, must not force them.

{{{
(+ (parallel-delay (* 3 7)) 13) => unspecified

(car (list (parallel-delay (* 3 7)) 13)) => unspecified
}}}

`(parallel-promise? `''obj''`)`

Returns `#t` if its argument is a
parallel promise, and `#f` otherwise. Note that parallel promises are not
necessarily disjoint from other Scheme types such as procedures.

`(make-parallel-promise `''obj''`)`

Returns a parallel promise which,
when forced, will return ''obj'' . It is similar to `parallel-delay`, but
does not delay its argument: it is a procedure rather than
syntax. If ''obj'' is already a parallel promise, it is returned.

`(current-parallel-promise)`

Returns the parallel promise whose execution is the current continuation. If a parallel promise itself uses `parallel-force`, parallel-promise executions can be nested, in which case the descriptor of the most immediately executing parallel promise is returned. If the current continuation is not a parallel promise execution, the result is `#f`.

`(parallel-call `<func> <arg> ...`)` [syntax]

Semantics:  Wrap <func> and each <arg> in a parallel promise.  Force the parallel promises in an unspecified order.  Then apply the value of the <func> promise to the value of the <arg> promises and return the result.

== Implementation ==

Parallel implementation of this proposal is necessarily very system-dependent.  However, it is correct to implement
`parallel-delay`, `parallel-delay-force`, `parallel-force`, `parallel-promise?`, `make-parallel-promise`, `current-parallel-promise`, and `parallel-call` as
`delay`, `delay-force`, `force`, `promise?`, `make-promise`, `(lambda () #f)`, and `(syntax-rules parallel-call (syntax-rules () ((parallel-call func . args) (func . args))))` respectively.