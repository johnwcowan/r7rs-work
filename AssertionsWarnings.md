This SRFI is a proposal for integrating assertions
and warnings into the R7RS exception system.
This SRFI defines syntax analogous to the `error`
syntax of the `(scheme base)` library.
It neither requires nor forbids any relationship
between error, warning, and assertion objects.

`(assert obj `*message irritant ...*`)`  [syntax]

It is an error if `message` is not a string.

If *obj* is true, nothing happens.
If it is false, an object satisfying `assertion-object?`
is created using the *message* and *irritants* arguments
and then is signaled as if by `raise`.

This is syntax rather than a procedure so that information
like the source location can be captured and included in the object.

`(assertion-object? `*obj*`)`

Returns `#t` if *obj* is an object created by `assert`
or is one of a set (possibly empty) of implementation-defined objects.

`(assertion-object-message `*assertion-object*`)`

Returns the message string encapsulated in *assertion-object*`)`

`(assertion-object-irritants `*assertion-object*`)

Returns a list of the irritant objects encapsulated in *assertion-object*`)`

`(warn obj `*message irritant ...*`)`  [syntax]

It is an error if `message` is not a string.

If *obj* is true, nothing happens.
If it is false, an object satisfying `warning-object?`
is created using the *message* and *irritants* arguments
and then is signaled as if by `raise-continuable`.

If `raise-continuable` returns, the warning object
(which may have been mutated by an exception handler)
is written in an implementation-specified format
to the current error port, and `warn` returns to its caller.

This is syntax rather than a procedure so that information
like the source location can be captured and included in the object.

`(warning-object? `*obj*`)`

Returns `#t` if *obj* is an object created by `warn`
or is one of a set (possibly empty) of implementation-defined objects.

`(warning-object-message `*warning-object*`)`

Returns the message string encapsulated in *warning-object*`)`

`(warning-object-irritants `*warning-object*`)

Returns a list of the irritant objects encapsulated in *warning-object*`)`

