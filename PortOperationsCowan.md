# Port operations

This SRFI contains a variety of operations on ports.  All of them can be implemented portably.

## Ports, generators, and accumulators

As an alternative to creating custom Scheme ports, this SRFI provides for
clean interfaces between the world of ports and the more flexible world of
[SRFI 158](https://srfi.schemers.org/srfi-158/srfi-158.html) generators and accumulators.
In what follows, *operation* is a procedure with side effects that takes
either a port (for input) or an object to write and a port (for output).

`(input-operation->generator `*operation* [ *port* ]`)`

Returns a generator which, when invoked, applies *operation*
(a procedure with one argument) to *port*
and returns what *operation* returns.
If *port* is omitted, the value of `(current-input-port)` is used.
When the generator reads an end-of-file object from *port*,
it closes the port and returns the end-of-file object.

For example,
`(input-operation->generator read inport)` returns a generator
that will read a datum from *inport* and return
its internal representation.
However, in order to
read a string of at most ten characters
from the current input port,

```
(input-operation->generator
  (lambda (p) (read-string 10 p)))
```
is required, because `read-string` (as well as `read-bytevector`) with a size limit
does not directly satisfy the contract for *operation*.

`(output-operation->accumulator `*operation* [ *port* ]`)`

Returns an accumulator which applies *operation* to its argument
and *port* and returns an unspecified value.
However, if the argument passed to the accumulator is an
end-of-file object, *operation* is not invoked
and *port* is closed; the return value is again unspecified.
If *port* is omitted, the value of `(current-output-port)` is used.

For example, `(output-operation->accumulator write-shared outport)`
returns an accumulator that writes its argument
to *outport*, representing shared structure with datum labels.
An attempt to write an end-of-file object is ignored.
However, in order to obtain an accumulator that writes
at most the first five characters of its
string argument to the current output port, then

```
(output-operation->accumulator
 (lambda (str p) (write-string str p 0 4)
```
is appropriate, because both `write-string` and `write-bytevector` with
a start argument or both start and end arguments do not fit the
contract for *operation*.

## String and bytevector port operations

These are analogous to the R7RS-small operations on file ports.
They can be implemented on top of `call-with-port`.

`(call-with-input-string `*string proc*`)`

Opens a string input port on *string* as if by
R7RS-small `open-input-string`. The port is then
passed to *proc*,
and its results are returned
with the port closed.

`(call-with-output-string `*proc*`)`

Opens a string output port as if by
R7RS-small `open-output-string`. The port is then
passed to *proc*,
and its results are discarded.
A string is extracted from the port, which is returned
with the port closed.

`(with-input-from-string `*string thunk*`)`

Opens a string input port on *string* as if by
R7RS-small `open-input-string`. The port is then
bound to the parameter `current-input-port`,
*thunk* is invoked,
and its results are returned
with the port closed and `current-input-port` restored.

`(with-output-to-string `*thunk*`)`

Opens a string output port on *string* as if by
R7RS-small `open-output-string`. The port is then
bound to the parameter `current-output-port`,
*thunk* is invoked,
and its results are discarded.
A string is extracted from the port, which is returned
with the port closed and `current-output-port` restored.

`(call-with-input-bytevector `*bytevector proc*`)`

`(call-with-output-bytevector `*proc*`)`

`(with-input-from-bytevector `*bytevector thunk*`)`

`(with-output-to-bytevector `*thunk*`)`

The same as the corresponding string port procedures,
except that a bytevector is read or written.

## Convenience I/O operations

`(binary-port-eof? `*port*`)`

Returns `#t` if the next attempt to read a byte from *port*
would return an eof-object, and `#f` otherwise.
The default port is the value of `(current-input-port)`.
It is an error to call this procedure
if the `peek-u8` procedure is not supported by *port*.

`(textual-port-eof? `*port*`)`

Returns `#t` if the next attempt to read a character from *port*
would return an eof-object, and `#f` otherwise.
The default port is the value of `(current-input-port)`.
It is an error to call this procedure
if the `peek-char` procedure is not supported by *port*.

`(read-lines ` [*input-port*]`)`

Read all remaining characters in *input-port* (as if by `read-line`),
and return a list of strings representing the lines.
The default port is the value of `(current-input-port)`.

`(read-all-bytes ` [*port*]`)`

Returns a bytevector consisting of all the bytes
that can be read from *port* before an eof-object is returned,
or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(read-all-chars ` [*port*]`)`

Returns a string consisting of all the characters (as if by `read-char`)
that can be read from *port* before an eof-object is returned,
or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(read-all ` [*port*]`)`

Returns a list consisting of all the Scheme objects
that can be read from *port* (as if by `read`)
before an eof-object is returned, or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(write-line `*string* [*port*]`)`

Write *string* to *port* (as if by `write-string`),
then write a newline to *port* (as if by `newline`).

`(print `*obj* ...`)`

Write each *obj* to the port (as if by `display`)
that is the value of `(current-output-port)`
followed by a newline.

`(debug-print `*obj* ...`)`

Write each *obj* to the port
that is the value of `(current-error-port)`
separated by single spaces and followed by a newline.
If *obj* is a boolean, character, empty list, symbol, string, or
exact integer, it must be printed as if by `display`.
In all other cases, a more terse representation may be used:
for example, printing pairs as `(...)` and vectors as `#(...)`.

Implementations should attempt to produce the output as quickly
as possible, flushing any buffers and (if useful) shutting
down thread scheduling and using ordinary blocking I/O.

## To be added

PortOperationsCowan is a handy-looking utility library (I have already implemented it),
although I think it needs a procedure (`read-line*`? `read-record`?)
which is like read-line but lets you specify the line/record ending character/string.
(I have also [already implemented this](https://gitlab.com/dpk/presrfis/blob/master/io-utils/read.scm).)
