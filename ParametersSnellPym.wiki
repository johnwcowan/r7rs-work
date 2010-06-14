= Background =

Parameters, as per [[http://srfi.schemers.org/srfi-39/srfi-39.html|SRFI-39]], are certainly useful.

 1. They can be labour-savers, passing "configuration information" down through complex call stacks without needing explicit parameter passing through functions that do nothing with the parameter other than pass it to all child procedures called, until a lower layer actually uses it

 2. They allow isolation of concerns; as per the previous point, the intermediate procedures do not need to know what configuration information is sent to which child procedures, which is useful when the child procedures are provided by external libraries, or are arbitrary closures which might invoke arbitrary external libraries.

 3. They allow dynamically scoped state, for tasks like exception handling and thread-local storage

 4. They generalise a mechanism that must be present within the implementation to support `current-input-port` and `current-output-port`, making it portably available to library authors.

I think that parameters should be specified as part of WG1 scheme rather than implemented by portable libraries, as a portable library has no way of knowing if the implementation provides threads; and if it does, it has no portable way of implementing dynamically scoped state in a thread-safe manner.

= The Proposal =

We support [[http://srfi.schemers.org/srfi-39/srfi-39.html|SRFI-39]] parameters.

However, in order to promote portability, we must take this opportunity to specify the behaviour of parameters in the presence of threads; even if WG1 does not specify a threading system, any implementation of WG1 parameters that does provide threads must conform with the following specification, or else libraries using parameters will not operate safely in threaded programs.

Parameters are defined in terms of mutable cells. When `make-parameter` is called, the parameter is bound to a new mutable cell in the current thread's dynamic environment.

When a thread is created, it inherits its parent's dynamic environment bindings, so assignment to inherited parameters will change them in the parent and in other sibling threads whose dynamic environments refer to the same mutable cells. This assignment is guaranteed to be safe and atomic between threads, regardless of whether the semantics of `set!` and other mutating operations are well-defined between threads; such parameters can reliably be used as shared state. We can also produce some wording to the effect that such assignments are write barriers, so that if some mutable structure is mutated within one thread, then a reference to that structure is written into a shared parameter cell, then another thread reading that value from that shared parameter cell will see the results of all the prior mutations by the writing thread (to the mutable structure, or to other shared parameter cells), but may or may not see the results of subsequent ones.

When `parameterize` is used, the specified parameters are bound to newly created parameter cells, initially unique to the current thread.

Eg:

{{{
(define foo (make-parameter 123))

Thread 1: (foo) => 123
Thread 2: (foo) => 123
Thread 1: (foo 1234)
Thread 1: (foo) => 1234
Thread 2: (foo) => 1234
Thread 1: (parameterize ((foo 456))
Thread 1: (foo) => 456
Thread 2: (foo) => 1234
Thread 1: (foo 4567)
Thread 1: (foo) => 4567
Thread 2: (foo) => 1234
Thread 2: (foo 12345)
Thread 1: (foo) => 4567
Thread 2: (foo) => 12345
Thread 1: Fork off thread 3
Thread 3: (foo) => 4567
Thread 3: (foo 45678)
Thread 1: (foo) => 45678
Thread 1: ) ; close parameterize
Thread 1: (foo) => 12345
Thread 2: (foo) => 12345
}}}

Note that when thread 1 forked off thread 3 from within its
parameterized dynamic scope, thread 3 inherited it - so thread 3's
assignment to the parameter was seen by thread 1, as they shared that scope.
