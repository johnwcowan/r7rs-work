## Convenience port operations.

A few convenience functions on string ports that were omitted
from R7RS-small are provided.

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
