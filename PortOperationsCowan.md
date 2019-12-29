## Ports, generators, and accumulators

As an alternative to creating custom Scheme ports, this SRFI provides for
clean interfaces between the world of ports and the more flexible world of
[SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generators and accumulators.
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
