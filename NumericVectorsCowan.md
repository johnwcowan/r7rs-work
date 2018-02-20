This SRFI provides an API for specialized numeric vectors distinguished by their *representation type*.  The `u8` type is the same as the R7RS bytevector type, but the other types are all disjoint from all other Scheme types.  It may be useful for Schemes on the JVM or the CLR to use this SRFI to provide access to the platform's native numeric vectors.

This design subsumes [SRFI 4](http://srfi.schemers.org/srfi-4/srfi-4.html), and the sample implementation provides an implementation of it.  There are many procedures, but many of them can be easily inlined by even a very dumb compiler, providing high efficiency.   The procedures provided in the present proposal are the numeric-vector analogues of the [SRFI 133](http://srfi.schemers.org/srfi-133/srfi-133.html) procedures.

## Representation types

The [type] values are:

`u1`::
> unsigned 1-bit integer (bit)

`u8`::
> unsigned 8-bit integer

`s8`::
> signed 8-bit integer

`u16`::
> unsigned 16-bit integer

`s16`::
> signed 16-bit integer

`u32`::
> unsigned 32-bit integer

`s32`::
> signed 32-bit integer

`u64`::
> unsigned 64-bit integer

`s64`::
> signed 64-bit integer

`f32`::
32-bit float

`f64`::
64-bit float

`c64`::
64-bit complex number

`c128`::
128-bit complex number

