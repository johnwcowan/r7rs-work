## Macros

Here are a few macros that extend [SRFI 17](http://srfi.schemers.org/srfi-17/srfi-17.html)
getters and setters.

`(push! `*variable item*`)`

Calls `cons` on the values of *item* and *variable*, and sets *variable* to the result.
Returns an unspecified value.

`(push! (`*proc arg* ...`)` *item*`)`

Calls `cons` on the values of *item* and `(`*proc arg* ...`)`.
Then evaluates `((setter `*proc*`)` *arg* ... *result*`)`,
where *result* is the newly allocated pair.   Returns an unspecified value.
It is an error if *proc* does not have an associated setter.

`(pop! `*variable*`)`

Calls `car` and `cdr` on the value of *variable*, and sets *variable* to the result of `cdr`.
Returns the result of `car`.  It is an error if the value of *variable* is not a pair.

`(pop! (`*proc arg* ...`))`

Calls `car` and `cdr` on the value of `(`*proc arg* ...`)`.
Then evaluates `((setter `*proc*`)` *arg* ... *cdr-result*`)`,
where *cdr-result* is the result of `cdr`.   Returns the result of `car`.
It is an error if the value of *variable* is not a pair.
It is also an error if *proc* does not have an associated setter.

`(inc! `*variable* [[|*delta* ]]`)`

`(dec! `*variable* [[|*delta* ]]`)`

Calls `+` on the values of *variable* and *delta*, and sets *variable* to the result.
Returns an unspecified value.  It is an error if the value of *variable* is not a number.
The default value of *delta* is 1 for `inc!` and -1 for `dec!`.

`(inc! (`*proc arg* ...`)` [[|*delta* ]]`)`

`(dec! (`*proc arg* ...`)` [[|*delta* ]]`)`

Calls `+` on the values of `(`*proc arg* ...`)` and *delta*.
Then evaluates `((setter `*proc*`)` *arg* ... *result*`)`,
where *cdr-result* is the result of `+`.   Returns an unspecified value.
The default value of *delta* is 1 for `inc!` and -1 for `dec!`.
It is an error if the value of *variable* is not a number.
It is also an error if *proc* does not have an associated setter.

`(update! `*variable proc*`)`

Applies the value of *proc* to the value of *variable*, and sets *variable* to the result.
Returns an unspecified value.
It is an error if the value of *proc* is not a procedure that accepts one argument.

`(update! (`*proc arg* ...`)` *item*`)`

Applies *item* to the value of `(`*proc arg* ...`)`.
Then evaluates `((setter `*proc*`)` *arg* ... *result*`)`, where *result* is the result.
Returns an unspecified value.
It is an error if the value of *proc* is not a procedure that accepts one argument.
It is also an error if *proc* does not have an associated setter.

See also [CL `define-modify-macro`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_2.htm).

