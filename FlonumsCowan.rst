== Abstract ==

''Flonums'' are a subset of the inexact real numbers provided by a Scheme implementation.  In most Schemes, the flonums and the inexact reals are the same.  

== Rationale ==

Flonum arithmetic is already supported by many systems, mainly to remove type-dispatching overhead. Standardizing flonum arithmetic increases the portability of code that uses it. Standardizing the range or precision of flonums would make flonum operations inefficient on some systems, which would defeat their purpose. Therefore, this SRFI specifies some of the semantics of flonums, but makes the range and precision implementation-dependent.

The sources of the procedures in this SRFI are R7RS-small, [[http://srfi.schemers.org/srfi-141/srfi-141.html|SRFI 141]], [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.3|the R6RS flonum library]], and the [[http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/math.h.html|C99/Posix library]] `<math.h>`, which should be available directly or indirectly to Scheme implementers.  (The C90 version of `<math.h>` lacks ``arcsinh``, ``arccosh``, ``arctanh``, ``erf``, and ``tgamma``.)

Scheme implementations that use IEEE 754 floating point numbers should follow the specifications of that standard.

== Specification ==

It is required that if two flonums are equal in the sense of `=`, they are also equal in the sense of `eqv?`.  That is, if 12.0f0 is a 32-bit inexact number, and 12.0 is a 64-bit inexact number, they cannot both be flonums.  In this situation, it is recommended that the 64-bit numbers be flonums.

It is an error, except as otherwise noted, for an argument not to be a flonum.  If the mathematically correct result is a non-real number, the result is `+nan.0` if the implementation supports that number, or an arbitrary flonum if not.

Flonum operations must be at least as accurate as their generic counterparts applied to flonum arguments.  In some cases, operations should be more accurate than their naive generic expansions because they have a smaller total roundoff error.

This SRFI uses ''x, y, z'' as
parameter names for flonum arguments, and ''ix''
as a name for an integer-valued flonum argument, i.e., a flonum for which the
`integer?` predicate returns true.

=== Constants ===

The following (mostly C99) constants are provided as Scheme variables.

`fl-e`

Bound to the value of the mathematical constant ''e''.  (C99 `M_E`)

`fl-log2-e`

Bound to the value of `(fllog fl-e 2.0)`.  (C99 `M_LOG2E`)

`fl-log10-e`

Bound to the value of `(fllog fl-e 10.0)`.  (C99 `M_LOG10E`)

`fl-ln-2`

Bound to the value of `(fllog 2.0)`  (C99 `M_LN2`)

`fl-ln-10`

Bound to the value of `(fllog 10.0)`  (C99 `M_LN10`)

`fl-pi`

Bound to the value of the mathematical constant π.  (C99 `M_PI`)

`fl-pi/2`

Bound to the value of `(fl/ fl-pi 2.0)`  (C99 `M_PI_2`)

`fl-pi/4`

Bound to the value of `(fl/ fl-pi 4.0)`  (C99 `M_PI_4`)

`fl-1/pi`

Bound to the value of `(fl/ 1.0 fl-pi)`.  (C99 `M_1_PI`)

`fl-2/pi`

Bound to the value of `(fl/ 2.0 fl-pi)`.  (C99 `M_2_PI`)

`fl-2/sqrt-pi`

Bound to the value of `(fl/ 2.0 (flsqrt fl-pi))`.  (C99 `M_2_SQRTPI`)

`fl-sqrt-2`

Bound to the value of `(flsqrt 2.0)`.  (C99 `M_SQRT2`)

`fl-1/sqrt-2`

Bound to the value of `(fl/ 1,0 (flsqrt 2.0))`.  (C99 `M_SQRT1_2`)

`fl-maximum`

Bound to the value of the largest finite flonum.

`fl-fast-+*`

Bound to `#t` if `(fl+* x y z)` is known to be faster than `(fl+ (fl* x y) z), or `#f` otherwise.  (C99 `FP_FAST_FMA`)

`fl-integer-exponent-zero`

Bound to whatever exact integer is returned by `(flinteger-binary-log 0)`.  (C99 `FP_ILOGB0`)

`fl-integer-exponent-nan`

Bound to whatever exact integer is returned by `(flinteger-binary-log +0.nan)`.  (C99 `FP_ILOGBNAN`)

`fl-radix`

Bound to the floating-point radix as an exact integer (2 on most machines).  (C99 `FLT_RADIX`)

== Constructors ==

`(flonum `''number''`)`

Returns the closest flonum equivalent to ''number'' in the sense of `=` and `<`.

`(fladjacent `''x y''`)`

Returns a flonum adjacent to ''x'' in the direction of ''y''.  Specifically: if ''x < y'', returns the smallest flonum larger than ''x''; if ''x > y'', returns the largest flonum smaller than ''x''; if ''x = y'', returns ''x''.  (C99 `nextafter`)

`(flcopysign `''x y''`)`

Returns a flonum whose magnitude is the magnitude of ''x'' and whose sign is the sign of ''y''.  (C99 `copysign`)

`(make-flonum `''x n''`)`

Returns the flonum `(fl* x (expt 2 n))`, where ''n'' is a non-negative integer with an implementation-dependent maximum value.  (C99 `ldexp`)

`(make-binary-flonum `''x n''`)`

Returns the flonum `(fl* x (expt fl-radix n))`, where ''n'' is a non-negative integer with an implementation-dependent maximum value.  (C99 `scalbn`)

=== Accessors ===

`(flinteger-fraction `''x''`)`

Returns two values, the integral part of ''x'' as a flonum and the fractional part of ''x'' as a flonum.  (C99 `modf`)

`(flexponent `''x''`)`

Returns the value of `(* (fltrunc (fllog (flabs x) fl-radix)) (flsgn x))`.  (C99 `logb`)

`(flinteger-exponent `''x''`)`

Returns the same as `flexponent` as an exact integer.  If ''x'' is zero, returns `fl-integer-exponent-zero`; if ''x'' is a NaN, returns `fl-integer-exponent-nan`; if ''x'' is infinite, returns a large implementation-dependent exact integer.  (C99 `ilogb`)

`(flnormalized-fraction-exponent `''x''`)`

Returns two values, a correctly signed fraction ''y'' whose absolute value is between 0.5 (inclusive) and 1.0 (exclusive), and an exponent ''n'' such that `(fl* y (flexpt 2 n))` is equal to ''x''.  (C99 `frexp`)

=== Predicates ===

`(flonum? `''obj''`)`

Returns `#t` if ''obj'' is a flonum and `#f` otherwise.

`(fl= `''x y z'' ...`)`

`(fl< `''x y z'' ...`)`

`(fl> `''x y z'' ...`)`

`(fl<= `''x y z'' ...`)`

`(fl>= `''x y z'' ...`)`

These procedures return `#t` if their arguments are (respectively): equal, monotonically increasing, monotonically decreasing, monotonically nondecreasing, or monotonically nonincreasing; they return `#f` otherwise. These predicates must be transitive.

`(flinteger? `''x''`)‌‌

`(flzero? `''x''`)`

`(flpositive? `''x''`)`

`(flnegative? `''x''`)`

`(flodd? `''ix''`)`

`(fleven? `''ix''`)`

`(flfinite? `''x''`)`

`(flinfinite? `''x''`)`

`(flnan? `''x''`)`

These numerical predicates test a flonum for a particular property, returning `#t` or `#f`. The `flinteger?` procedure tests whether the flonum is an integer, `flzero?` tests whether it is `fl=?` to zero, `flpositive?` tests whether it is greater than zero, `flnegative?` tests whether it is less than zero, `flodd?` tests whether it is odd, `fleven?` tests whether it is even, `flfinite?` tests whether it is not an infinity and not a NaN, `flinfinite?` tests whether it is an infinity, and `flnan?` tests whether it is a NaN.

Note that `(flnegative? -0.0)` must return `#f`;
otherwise it would lose the correspondence with
`(fl< -0.0 0.0)`, which is `#f`
according to IEEE 754.

=== Arithmetic ===

`(fl+ `''x''`)`

`(fl* `''x''`)`

Return the flonum sum or product of their flonum
arguments.  In general, they should return the flonum that best
approximates the mathematical sum or product.

`(fl+* `''x y z''`)`

Returns `(fl+ (fl* x y) z)`, possibly faster.  If the constant `fl-fast-fl+*`
is `#t`, it will definitely be faster.  (C99 `fma`)

`(fl- `''x y'' ...`)`

`(fl/ `''x y'' ...`)`

With two or more arguments, these procedures return the 
difference or quotient of their arguments, associating to the
left.  With one argument, however, they return the additive or
multiplicative inverse of their argument.

In general, they
should return the flonum that best approximates the mathematical
difference or quotient.

`(flmax `''x'' ...`)`

`(flmin `''x'' ...`)`

Return the maximum/minimum argument.  If there are no arguments,
these procedures return `+inf.0`/`-inf.0` if the implementation
provides these numbers, and `fl-maximum` / its negation otherwise.

`(flabs `''x''`)`

Returns the absolute value of ''x''.

`(flabsdiff `''x y''`)`

Returns `(flabs (fl- x y))` without internal overflow.  (C99 `fdim`)

`(flsgn `''x''`)`

Returns `(flcopy-sign 1.0 x)`.

`(flnumerator `''x''`)`

`(fldenominator `''x''`)`

Returns the numerator/denominator of ''x''
as a flonum; the result is computed as if ''x'' was represented as
a fraction in lowest terms.  The denominator is always positive.  The
denominator of 0.0 is defined to be 1.0.

`(flfloor `''x''`)`

`(flceiling `''x''`)`

`(flround `''x''`)`

`(fltruncate `''x''`)`

These procedures return integral flonums for flonum arguments that are
not infinities or !NaNs.  For such arguments, `flfloor` returns the
largest integral flonum not larger than ''x''.  The `flceiling`
procedure
returns the smallest integral flonum not smaller than ''x''.
The `fltruncate` procedure returns the integral flonum closest to ''x'' whose
absolute value is not larger than the absolute value of ''x''.
The `flround` procedure returns the closest integral flonum to ''x'',
rounding to even when ''x'' represents a number halfway between two integers.


Although infinities and !NaNs are not integers, these procedures return
an infinity when given an infinity as an argument, and a NaN when
given a NaN.

=== Exponents and logarithms ===

`(flexp `''x''`)`

Corresponds to `exp`.

`(flexp2 `''x''`)`

Corresponds to `(expt 2.0 x)`.  (C99 `exp2`)

`(flexp-1 `''x''`)`

Equivalent to `(flexp (fl- x 1))`, but is much more accurate for values of x near 1.  It is recommended for use in algorithms where accuracy is important.  (C99 `expm1`)

`(flsquare `''x''`)`

Returns the square of ''x''.

`(flsqrt `''x''`)`

Returns the principal square root of ''x''. For -0.0,
`flsqrt` should return -0.0.

`(flcbrt `''x''`)`

Returns the cube root of ''x''.

`(flhypot `''x y''`)`

Returns `(flsqrt (+ (flsquare x) (flsquare y)))`.

`(flexpt `''x y''`)`

Returns ''x'' raised to the power ''y''.  If ''x'' is zero, then
the result is zero.

`(fllog `''x''`)`

Returns the natural logarithm of ''x''.

`(fllog1+ `''x''`)`

Equivalent to `(fllog (fl+ 1 x))`, but is much more accurate for values of x near 1.  It is recommended for use in algorithms where accuracy is important.  (C99 `log1p`)

`(fllog2 `''x''`)`

Returns the base-2 logarithm of ''x''.

`(fllog10 `''x''`)`

Returns the base-10 logarithm of ''x''.

=== Trigonometric functions ===

`(flsin `''x''`)`

`(flcos `''x''`)`

`(fltan `''x''`)`

`(flasin `''x''`)`

`(flacos `''x''`)`

`(flatan `''x'' [''y'']`)`

`(flsinh `''x''`)`

`(flcosh `''x''`)`

`(fltanh `''x''`)`

`(flasinh `''x''`)`

`(flacosh `''x''`)`

`(flatanh `''x''`)`

These are the usual trigonometric functions. The `flatan` function, when passed two arguments, returns `(flatan (/ y x))` without requiring the use of complex numbers (C99 `atan2`).


=== Integer division ===

The following procedures are the flonum counterparts of procedures from [[http://srfi.schemers.org/srfi-141/srfi-141.html|​SRFI 141]]:

{{{
flfloor/ flfloor-quotient flfloor-remainder
flceiling/ flceiling-quotient flceiling-remainder
fltruncate/ fltruncate-quotient fltruncate-remainder
flround/ flround-quotient flround-remainder
fleuclidean/ fleuclidean-quotient fleuclidean-remainder
flbalanced/ flbalanced-quotient flbalanced-remainder
}}}

They have the same arguments and semantics as their generic counterparts,
except that it is an error if the arguments are not flonums.

`(flremquo `''x y''`)`

Returns two values, the result of `(flround-remainder x y)` and the low-order ''n'' bits (as a correctly signed exact integer) of the rounded quotient.  The value of ''n'' is implementation-dependent but at least 3.  This function can be used to reduce the argument of the inverse trigonometric functions, while preserving the correct quadrant or octant.

=== Special functions ===

`(flgamma `''x''`)`

Returns Γ(''x''), the gamma function applied to ''x''.  This is equal to (''x''-1)! for
integers.  (C99 `tgamma`)

`(flloggamma `''x''`)`

Returns two values, log Γ(|''x''|) without internal overflow, and sgn(Γ(''x'')).  (C99 `lgamma`)

`(first-bessel `''x n''`)`

Returns the ''n''th order Bessel function of the first kind applied to ''x'', J,,n,,(x).  (C99 `j0`, `j1`, `jn`)

`(second-bessel `''x n''`)`

Returns the ''n''th order Bessel function of the second kind applied to ''x'', Y,,n,,(x).  (C99 `y0`, `y1`, `yn`)

`(erf `''x''`)`

Returns the error function erf(''x'').  (C99 `erf`)

`(erfc `''x''`)`

Returns the complementary error function, `(- 1 (erf x))`.  (C99 `erfc`)
