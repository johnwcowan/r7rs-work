## Use cases for the six division functions of R7RS draft 6

### Truncate

Truncate is really not defensible, but it's everywhere, including R5RS.  If you want to transcribe an algorithm from some computer language that doesn't have exact rationals, it will usually have truncating division, and it pays to have a way to express this.  On the other hand, when you use truncating division, you usually ought to be using something else, and maybe you should rethink the algorithm.

By rule, bowling averages are truncated.  There may be other domain-specific uses.

### Floor

The floor function is almost always used for integer division if truncation isn't: for example, the Basic `int()` function is `floor-quotient`.  Therefore it too is useful for algorithmic conversion.  It is generally felt to be better than truncation.  There are a number of mathematical applications given on [the Wikipedia page](http://en.wikipedia.org/wiki/Floor_and_ceiling_functions).

The R5RS `modulo` is the flooring remainder, on the assumption that `(modulo (/ n d))` is the residue of `n` mod `d`.  However, that doesn't work if `d` is negative; the `euclidean-remainder` does what you want in this case.

### Ceiling

If you want to know how many fixed-sized containers that hold (physically or conceptually) `d` items each you need to hold `n` items altogether, you want `(ceiling-quotient (/ n d))`.

Similarly, if items are 3 for $1.00, then they are normally $0.34 per item, a variant of the same notion.  This prevents customers from making three transactions at $0.33 each in order to save a penny.

### Round

Rounding is important when multiple calculations are being done, because it is as close to unbiased as possible.  IEEE floating-point operations report rounded versions of transcendental numbers so that errors will be likely to offset one another rather than accumulate.

### Euclidean

Euclidean division is useful for reducing a hash value safely.  If we have a signed number `p` and wish to reduce it mod `q`, then `(euclidean-remainder (/ n d))` is guaranteed to produce a non-negative number `r` such that `(< 0 r (abs d))` , no matter the signs of `n` and `d`.

### Centered

Centered division is analogous to Euclidean division, except that the remainder satisfies `(< (- (abs (/ q 2))) r (abs (/ q 2)))`.  When d is 2^k^ for some *k*, this is the interval of integers representable in two's-complement with `(- k 1)` bits.
