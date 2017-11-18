## Read-eval-print loop

`(repl` [[*eval*|[*write* [*read*]]]]`)`

This procedure invokes an implementation-dependent read-eval-print loop (REPL), which reads a datum representing an expression or definition using *read*, evaluates it using *eval*, and prints the results if any using *write*.  It then loops until an eof-object is read.

By default, *eval* is `repl-eval`, *write* is `repl-write`, and *read* is `repl-read`.  The arguments are given in the order of most likely replacement, not the order of use.  Allowing these defaults to be individually overridden means that languages written in Scheme can make use of the host implementation's REPL smarts, whatever they may be.  The *write* argument should respect `current-output-port` and the *read* argument should respect `current-input-port` if it makes sense to do so.

`(repl-read)`

This procedure is implementation-dependent, but has the general effect of reading a datum from the current input port, possibly with some additional features.

`(repl-write `*obj*`)`

This procedure is implementation-dependent, but has the general effect of writing a datum to the current output port, possibly with some additional features.

`(repl-eval `*obj*`)`

This procedure is implementation dependent, but is similar to `(lambda (x) (eval x (interaction-environment))` with the interaction environment augmented with the variables listed below.  In addition, *obj* may be a definition, which extends the interaction environment.  There may be other implementation-dependent features.

The following variables are known to `repl-eval`; their values are local to a specific invocation of `repl`.  They are equivalent to the Common Lisp variables with the same names but without the leading `-` character; this is necessary because Scheme is a Lisp-1, so `+ - * /` are already bound.

`--`

The expression or definition currently being evaluated by `repl-eval`.

`-+`

The expression or definition last evaluated by `repl-eval`.

`-++`

The previous value of `-+`.

`-+++`

The previous value of `-++`.


`-*`

The result of the expression or definition most recently evaluated by `repl-eval`.  If a definition was last evaluated, the value of `-*` is implementation-dependent.  If the expression returned zero values, the value of `-*` is #f; if the expression returned more than one value, the value of `-*` is the first value returned.


`-**`

The previous value of `-*`.

`-***`

The previous value of `-**`.

`-/`

A list of the results of the expression most recently evaluated by `repl-eval`.  If a definition was last evaluated, the value of `-/` is implementation-dependent.


`-//`

The previous value of `-/`.

`-///`

The previous value of `-//`.
