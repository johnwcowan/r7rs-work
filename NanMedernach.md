## NaN functions

The following functions are applicable not only to flonum [NaNs](NaNs.md) but to all inexact real [NaNs](NaNs.md),
which is why their names do not begin with `fl`.

`(make-nan `*payload*`)`

Returns a NaN, using the exact integer *payload* in an implementation-defined way to generate the payload bits.
In particular, the sign bit of the NaN is set from the sign of `payload`.
If the implementation does not support [NaNs](NaNs.md), it is an error.

`(nan-payload `*nan*`)`

Returns the payload of *nan*.

`(nan-signaling? `*nan*`)`

Returns `#t` if *nan* is a signaling NaN, and `#f` otherwise.

`(nan= `*x y*`)`

Returns `#t` if *x* and *y* are both NaNs with the same payload and signaling flag.
