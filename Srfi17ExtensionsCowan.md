## Macros

Here are a few convenience macros that make use of
[SRFI 17](http://srfi.schemers.org/srfi-17/srfi-17.html) getters and setters.
A *place* is either a variable or a procedure call whose operator has a corresponding setter.
To set a place that is a variable, use `set!`; to set a place of the form
`(`*proc args*`)`, call `((setter `*proc*`)` *args*`)`.
It is an error to specify a procedure that does not have a setter.

`(push! `*place item*`)` [syntax]

Calls `cons` on the values of *place* and *variable*, and sets *place* to the result.
Returns an unspecified value.

`(pop! `*place*`)` [syntax]

Calls `car` and `cdr` on the value of *place*, and sets *place* to the result of `cdr`.
Returns the result of `car`.  It is an error if the value of *place* is not a pair.

`(inc! `*place* [*delta*]`)` [syntax]

`(dec! `*place* [*delta* ]`)` [syntax]

Calls `+` or `-` on the values of *place* and *delta*, and sets *variable* to the result.
Returns an unspecified value.  It is an error if the values of *place* and *delta* are not numbers.
The default value of *delta* is 1.

`(update! `*proc place*`)` [syntax]

Applies the value of *proc* to the value of *place*, and sets *place* to the result.
Returns an unspecified value.
It is an error if the value of *proc* is not a procedure that accepts one argument.

See also [CL `define-modify-macro`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_2.htm).

