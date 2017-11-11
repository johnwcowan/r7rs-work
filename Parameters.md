Parameters are explained in detail in [SRFI 39](http://srfi.schemers.org/srfi-39/srfi-39.html).  They are a means of extending Scheme's dynamic environment.  Parameters are procedures with a single element of state; when you call one without an argument, it returns the state, and when you call one with a single argument, it sets its state to the passed value.  The term "parameter" is unfortunate, but is universally used.

Users may create their own parameters with `make-parameter`.  In addition, the syntax `(parameterize ((param value) ...) . body)` alters the value  returned by `(param)` in the dynamic extent of the body.  The syntax looks like `let`, except that the `param`s are expressions which evaluate to parameters rather than variable names.  In the usual case, where all parameters are bound to global variables, the resemblance is exact at the syntactic level.

It's easy to implement parameters portably on single-threaded Scheme implementations.  On multi-threaded implementations, however, there are multiple ways for parameters to interact with the creation of threads, and different Schemes take different approaches.  Parameters exported by code which provides its own implementation will not interoperate properly with native parameters on such systems.

SRFI 39 requires that Schemes implementing it treat the R5RS procedures `current-input-port` and `current-output-port` as parameters.  If the R6RS `current-error-port` is available, it is also typically a parameter.

At present, SRFI 39 is implemented by PLT, Gauche, Gambit, Chicken, Guile, Kawa, SISC, Chibi, Ikarus, Ypsilon, and STklos.
