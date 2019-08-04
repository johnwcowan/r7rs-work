## Ports, generators, and accumulators

As an alternative to creating custom Scheme ports, this SRFI provides for
clean interfaces between the world of ports and the more flexible world of
[SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generators and accumulators.
In what follows, *operation* is a procedure that accepts (perhaps optionally) a port as
one of its arguments.  A few convenience functions on string and bytevector ports that were omitted
from R7RS-small are also provided.

`(input-port->generator `*operation obj* ...`)`

Returns a generator which, when invoked, applies *operation* to *objs*
and returns what *operation* returns.
Typically one of the *objs* is an input port, though this is not a requirement.
Note that exhausting the generator does not close any underlying port.
Furthermore, if there are no *objs*, then *operation* is
already a generator and is returned.

For example,
`(input-port->generator read inport)` returns a generator
that will read a datum from *inport* and return
its internal representation.
Similarly, `(input-port->generator read-string 10)` returns a generator
that will read a string of at most ten characters
from the current input port.

`(output-port->accumulator `*operation obj* ... ]`)`

Returns an accumulator which applies its argument as well as
*objs*, in that order, to *operation* and returns an unspecified value.
Typically one of the *objs* is an output port, though this is not a requirement.
However, if the argument passed to the accumulator is an
end-of-file object, *operator* is not invoked;
this does not close any underlying port.
Furthermore, if there are no *objs*, then *operation* is
already an accumulator and is returned.

For example, `(output-port->accumulator write-shared outport)`
returns an accumulator that writes its argument
to *outport*, representing shared structure with datum labels.
An attempt to write an end-of-file object is ignored.
Similarly, `(output-port->accumulator write-string (current-output-port) 0 4)`
will return an accumulator that writes at most the first five characters of its
string argument to the current output port.

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
