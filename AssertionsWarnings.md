## Specification

This SRFI is a proposal for integrating assertions
and warnings into the R7RS exception system.
This SRFI defines syntax analogous to the `error`
syntax of the `(scheme base)` library.
It neither requires nor forbids any relationship
between error, warning, and assertion objects.

The difference between the `assume` macro of
[SRFI 145](http://srfi.schemers.org/srfi-145/srfi-145.html)
and `assert` is that it is an error
if the *obj* of `assume` is false, and the
implementation can take any action that seems appropriate
either at compile time or at run time,
from raising a condition to making demons fly out of the user's nose.
However, if *obj* is false, then
then`assert` requires that no action be taken at compile time
and the creation and raising of a condition object at run time.

This SRFI recommends that any system that also implements SRFI 145
should treat `assume` it were `assert` when the program is being
compiled or executed in debug mode.  No particular method of
specifying debug mode is recommended here.


`(assert obj `*message irritant ...*`)`  [syntax]

It is an error if `message` is not a string.

If *obj* is true, nothing happens.
If it is false, an object satisfying `assertion-object?`
is created using the *message* and *irritants* arguments
and then is signaled by `raise`.

This is syntax rather than a procedure so that information
like the source location can be captured and included in the object.

`(assertion-object? `*obj*`)`

Returns `#t` if *obj* is an object created by `assert`
or is one of a set (possibly empty) of implementation-defined objects.

`(assertion-object-message `*assertion-object*`)`

Returns the message string encapsulated in *assertion-object*`)`

`(assertion-object-irritants `*assertion-object*`)`

Returns a list of the irritant objects encapsulated in *assertion-object*`)`

`(warn obj `*message irritant ...*`)`  [syntax]

It is an error if `message` is not a string.

If *obj* is true, nothing happens.
If it is false, an object satisfying `warning-object?`
is created using the *message* and *irritants* arguments
and then is signaled by `raise-continuable`.

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

`(warning-object-irritants `*warning-object*`)`

Returns a list of the irritant objects encapsulated in *warning-object*`)`

