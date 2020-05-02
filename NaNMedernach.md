## NaN functions


`(make-nan `*payload* [ *signaling?* ]`)`

Returns a NaN, using the exact integer *payload*
to generate the payload bits.
If, as is usually the case, inexact real numbers are represented by IEEE 754 binary64 numbers,
then the following rules apply:

  * The sign of *payload* is used to set the sign bit.
  * The quiet/signaling bit is set from *signaling?*, which if omitted is false,
    by setting the bit to 1 for a quiet NaN and 0 for a signaling NaN.
  * The absolute value of *payload* is used to set the low-order 51 bits;
    it is an error if the value is not between 0 and 2^51 exclusive.

If the implementation's inexact real numbers have a different format, the arguments are
used in an implementation-dependent way.
If the implementation does not support NaNs, it is an error.

`(nan-payload `*nan*`)`

Returns the payload of *nan* as an exact integer, negated if the sign bit is set.

`(nan-signaling? `*nan*`)`

Returns `#t` if *nan* is a signaling NaN, and `#f` otherwise.

`(nan= `*nan1 nan2*`)`

Returns `#t` if *nan1* and *nan2* have the same payload and signaling flag,
and `#f` otherwise.
