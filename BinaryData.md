Most Scheme implementations provide one or more
ways to represent blocks of data which are fundamentally
binary in nature - they are opaque sequences of 8-bit
bytes whose structure is to be interpreted at the
application level.  This is particularly important
for efficient I/O, and for other host system interfaces
such as pathnames which may not be valid strings.

SRFI-4 provides multiple uniform vector data-types, of
which the u8vector often gets special treatment as a
general container of binary data.  R6RS provides only
a byte-vector data-type, similar to the u8vector, with
an API that allows accessing other machine numeric types
from any offset.  We need to decide what, if any, binary
data type we will provide in WG1, including any read/write
representations.

If you view the bytes as primarily textual (as in the
pathname case), then it makes sense to provide an
external representation which allows ASCII.  PLT, for
instance provides

> #"ABC\0DEF"

where the \0 indicates a NULL byte.

On the other hand, if you view the bytes as primarily
binary, then it makes sense to encode each of the bytes
as an integer, so the above example becomes

> #vu8(65 66 67 0 68 69 70)

Erlang allows mixing both, where numbers are taken as
individual bytes and ASCII strings are flattened.  So
the same example becomes

> #vu8("ABC" 0 "DEF")

The #vu8 is the R6RS syntax.  SRFI-4 uses #u8.  The
former has the advantage that only one letter is taken
up after the #, leaving room for more future syntax
extensions (SRFI-4 uses #u, #s and #f).  The latter
has the advantage that it's more widely implemented.
