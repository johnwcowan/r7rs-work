## NaN functions


`(make-nan `*negative quiet payload*`)`

Returns a NaN constructed from the boolean values *negative* and *quiet*
and the exact integer *payload*.
It is an error if the value is not between 0 and 2^51 exclusive.
If, as is usually the case, inexact real numbers are represented by IEEE 754 binary64 numbers,
then the following rules apply:

  * The sign bit `#x80000000` is set if *negative* is true.
  * The binary64 exponent bits `#x7FF00000` are set unconditionally
  * The quiet bit `#x7FF80000` is set if `quiet` is true.
  * The low-order 51 bits are set from *payload*.

If the implementation's inexact real numbers have a different format,
the arguments are used in an implementation-dependent way.
If the implementation does not support NaNs, it is an error.

`(nan-negative? `*nan*`)`

Returns `#t` if the sign bit of *nan* is 1 and `#f` otherwise.

`(nan-quiet? `*nan*`)`

Returns `#t` if *nan* is a quiet NaN, and `#f` otherwise.

`(nan-payload `*nan*`)`

Returns the payload of *nan* as an exact integer.

`(nan= `*nan1 nan2*`)`

Returns `#t` if *nan1* and *nan2* have the same sign, quiet bit,
and payload; and `#f` otherwise.
