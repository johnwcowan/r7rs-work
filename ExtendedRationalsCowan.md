This is a WG2 proposal to extend Scheme's exact number system with exact equivalents of  `+inf.0`, `-inf.0`, `+nan.0`, but not `-0.0`.

## Proposal

Implementations may distinguish special exact numbers called *exact positive infinity, exact negative infinity*, and *exact NaN*.

Exact positive infinity is regarded as an real (but not rational) number written `1/0` that represents a value greater than the numbers represented by all rational numbers.   Exact negative infinity is regarded as an exact real (but not rational) number written `-1/0` that represents a value less than the numbers represented by all rational numbers.  Forms such as `2/0` and `-3/0` are accepted on input but are not produced on output.

Adding or multiplying an exact infinite value by any finite real value results in an appropriately signed exact infinity; however, the sum of positive and negative exact infinities is an exact NaN.  Both exact positive infinity and exact negative infinity is the reciprocal of exact zero.

An exact NaN is regarded as an exact real (but not rational) number written `0/0` that is so indeterminate that it might represent any real value, including positive or negative infinity.

An exact NaN always compares false to any number, including a NaN.  An exact arithmetic operation where one operand is an exact NaN returns an exact NaN, unless the implementation can prove that the result would be the same if the exact NaN were replaced by any rational number.  Dividing exact zero by exact zero results in exact NaN.

Note that both the real and the imaginary parts of an exact complex number can be exact infinities or exact NaNs.

## Implementations

Kawa Scheme implements exact positive and negative infinities, but not exact NaN.  Operations that would produce exact NaN, such as `(+ 1/0 -1/0), signal an error instead.
