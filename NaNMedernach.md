## NaN functions


`(make-nan `*payload* [ *signaling?* ]`)`

Returns a NaN, using the exact integer *payload*
to generate the payload bits.
If, as is usually the case, inexact real numbers are represented by IEEE 754 binary64 numbers,
then the sign of *payload* is used to set the sign bit, and the absolute
value of *payload* is used to set the low-order 52 bits.
It is an error if *payload* is zero.
In particular, the sign bit of the NaN is set from the sign of `payload`.
The quiet/signaling bit, however, is set from *signaling?*, which if omitted is false.
If the implementation's inexact real numbers have a different format, the arguments are
used in an implementation-dependent way.
If the implementation does not support NaNs, it is an error.

`(nan-payload `*nan*`)`

Returns the payload of *nan* as an exact integer

`(nan-signaling? `*nan*`)`

Returns `#t` if *nan* is a signaling NaN, and `#f` otherwise.

`(nan= `*nan1 nan2*`)`

Returns `#t` if *nan1* and *nan2* have the same payload and signaling flag,
and `#f` otherwise.
