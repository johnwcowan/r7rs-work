### Decoding flonums

decode-float float => significand, exponent, sign (SRFI 144: ???, flexponent, flsign-bit)

scale-float float integer => scaled-float (SRFI 144: make-flonum)

float-radix float => float-radix (SRFI 144: always 2)

float-sign float-1 &optional float-2 => signed-float (SRFI 144: flcopysign)

float-digits float => digits1 (no SRFI 144 equivalent)

float-precision float => digits2 (no SRFI 144 equivalent)

integer-decode-float float => significand, exponent, integer-sign (see decode-float)



Description:

decode-float computes three values that characterize float.
The first value is of the same type as float and represents the significand.
The second value represents the exponent to which the radix (notated in this description by b)
must be raised to obtain the value that, when multiplied with the first result, produces the absolute value of float.
If float is zero, any integer value may be returned, provided that the identity shown for scale-float holds.
The third value is of the same type as float and is 1.0 if float is greater than or equal to zero or -1.0 otherwise.

decode-float divides float by an integral power of b so as to bring its value between 1/b (inclusive) and 1 (exclusive),
and returns the quotient as the first value. If float is zero, however, the result equals the absolute value of float
(that is, if there is a negative zero, its significand is considered to be a positive zero).

scale-float returns `(* float (expt (float b float) integer))`, where b is the radix of the floating-point representation.
float is not necessarily between 1/b and 1.

float-radix returns the radix of float.

float-sign returns a number z such that z and float-1 have the same sign 
and also such that z and float-2 have the same absolute value.
If float-2 is not supplied, its value is `(float 1 float-1)`.
If an implementation has distinct representations for negative zero and positive zero,
then `(float-sign -0.0) => -1.0`.

float-digits returns the number of radix b digits used in the representation of float (including any implicit digits,
such as a hidden bit).

float-precision returns the number of significant radix b digits present in float;
if float is a float zero, then the result is an integer zero.

For normalized floats, the results of float-digits and float-precision are the same,
but the precision is less than the number of representation digits for a denormalized or zero number.

integer-decode-float computes three values that characterize float -
the significand scaled so as to be an integer,
and the same last two values that are returned by decode-float.
If float is zero, integer-decode-float returns zero as the first value.
The second value bears the same relationship to the first value as for decode-float:

```
 (let-values* ((signif expon sign)
                      (integer-decode-float f))
   (scale-float (float signif f) expon)) =>  (abs f)
```

### NaN functions

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
This function is required because different floating-point processors implement the signaling bit in different ways:
on most processors, the most significant bit of the payload is clear if the NaN is signaling,
but on the PA-RISC and MIPS processors it is set.

`(nan= `*x y*`)`

Returns `#t` if *x* and *y* are both NaNs with the same payload.
