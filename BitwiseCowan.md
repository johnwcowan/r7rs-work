# Bitwise arithmetic procedures

## Abstract

This SRFI proposes a coherent and comprehensive set of procedures for
performing bitwise logical operations on integers; it is
accompanied by a reference implementation of the spec in terms of a set of
seven core operators. The sample implementation is portable, as efficient
as practical with pure Scheme arithmetic (it is worthwhile replacing the
core ops with C or assembly language if possible), and open source.
The precise semantics of these operators is almost never an issue. A
consistent, portable set of *names* and *parameter conventions*, however, is.

Hence this SRFI, which is based mainly on [SRFI 33](http://srfi.schemers.org/srfi-33/srfi-33.html), with some changes and additions from [Olin's late revisions to SRFI 33](http://srfi.schemers.org/srfi-33/mail-archive/msg00023.html) (which were never consummated).
SRFI 33 was never finalized, but is a reasonably comprehensive proposal.
A few procedures have been added from [SRFI 60](http://srfi.schemers.org/srfi-60/srfi-60.html) and
the general vector [SRFI 133](http://srfi.schemers.org/srfi-133/srfi-133.html).
SRFI 60 (based on SLIB) is smaller but has a few procedures of its own;
some of its procedures have both native (often CL) and SRFI 33 names.
[R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.4)
is a subset of SRFI 60, except that all procedure names begin with a `bitwise-` prefix.

Among the applications of bitwise operations are: hashing,
Galois-field calculations of error-detecting and error-correcting codes,
cryptography and ciphers,
pseudo-random number generation,
register-transfer-level modeling of digital logic designs,
Fast-Fourier transforms,
packing and unpacking numbers in persistant data structures,
space-filling curves with applications to dimension reduction
and sparse multi-dimensional database indexes,
and generating approximate seed values for root-finders
and transcendental function algorithms.

## Procedure index

```
bitwise-not
bitwise-and   bitwise-ior 
bitwise-xor   bitwise-eqv
bitwise-nand  bitwise-nor 
bitwise-andc1 bitwise-andc2
bitwise-orc1  bitwise-orc2 

arithmetic-shift bit-count integer-length

bitwise-if 
bit-set? copy-bit bit-swap
any-bit-set? every-bit-set?
first-set-bit

bit-field bit-field-any? bit-field-every?
bit-field-clear bit-field-set
bit-field-replace  bit-field-replace-same
bit-field-rotate bit-field-reverse
bit-field-append

integer->list list->integer
integer->vector vector->integer
bits
bitwise-fold bitwise-for-each bitwise-unfold
```

## Rationale

### General design principles

* These operations interpret exact integers using two's-complement representation.

* The associative bitwise ops are required to be n-ary. Programmers can *reliably* write `bitwise-and` with 3 arguments, for example.

* The word `or` is never used by itself, only with modifiers: `xor`, `ior`, `nor`,
> `orc1`, or `orc2`.  This is the same rule as Common Lisp.

* Extra and redundant functions such as `bitwise-count`, `bitwise-nor`
> and the bit-field ops have been included.  Settling on a standard
> choice of names makes it easier to read
> code that uses these sorts of operations. It also means computations
> can be clearly expressed using the more powerful ops rather than
> synthesized with a snarled mess of `bitwise-and`s, `bitwise-or`s, and `bitwise-not`s.
> What we gain is having an agreed-upon set of names by which we can refer
> to these functions. If you believe in "small is beautiful," then what is your motivation
> for including anything beyond `bitwise-nand`?

* The programmer doesn't have to re-implement the redundant functions, and stumble
> over the boundary cases and error checking. The programmer can express
> himself using a full palette of building blocks.

* Compilers can directly implement many of these ops for great efficiency gains
> without requiring any tricky analysis.

* Logical right or left shift operations are excluded
> because they are not well defined on general integers; they are only defined
> on integers in some finite range. Remember that, in this library, integers
> are interpreted as *semi-infinite* bit strings that have only a finite
> number of ones or a finite number of zeros. Logical shifting operates on bit
> strings of some fixed size. If we shift left, then leftmost bits "fall off"
> the end (and zeros shift in on the right). If we shift right, then zeros
> shift into the string on the left (and rightmost bits fall off the end). So
> to define a logical shift operation, *we must specify the size of the
> window*. Typically this is the width of the underlying machine's register
> set (e.g., 32 bits). This is blatantly machine-specific and unportable, and
> clearly not the right thing for Scheme's more machine-independent general
> integers.

### Common Lisp

The core of this design design mirrors the structure of Common Lisp's pretty closely.  Here are some differences:

* "load" and "deposit" are the wrong verbs (e.g., Common Lisp's `ldb` and `dpb` ops),
> since they have nothing to do with the store.
>
* `boole` has been removed; it is not one with the Way of Scheme.  Boolean functions
> are directly encoded in Scheme as first-class functions.
>
* The name choices are more in tune with Scheme conventions (hyphenation,
> using `?` to mark a predicate, etc.). Common Lisp's name choices were more
> historically motivated, for reasons of backward compatibility with
> Maclisp and Zetalisp.
>
* The prefix `log` has been changed to `bitwise-` (e.g, `lognot` to `bitwise-not`),
> as the prefix `bitwise-` more accurately reflects what they do.
>
* The six trivial binary boolean ops that return constants, the left or right arguments,
> and the `bitwise-not` of the left or right arguments, do not appear in this SRFI.

### SRFI 33

This SRFI contains all the procedures of SRFI 33, and retains their original names with these exceptions:

* The name `bitwise-merge` is replaced by `bitwise-if`, the name used in SRFI 60 and R6RS.

* The name `extract-bit-field` (`bit-field-extract` in Olin's revisions) is replaced by `bit-field`, the name used in SRFI 60 and R6RS.

* The names `any-bits-set?` and `all-bits-set?` are replaced by `any-bit-set?` and `every-bit-set?`, in accordance with Olin's revisions.

* The name `test-bit-field?` has been renamed `bit-field-any?` and supplemented with
> `bit-field-every?`, in accordance with Olin's revisions.

* Because  `copy-bit-field` means different things in SRFI 33 and SRFI 60,
> SRFI 33's name `copy-bit-field` (`bit-field-copy` in Olin's revisions)
> has been changed to `bit-field-replace-same`.

### SRFI 60

SRFI 60 includes six procedures that do not have SRFI 33 equivalents.  They are incorporated into this SRFI as follows:

* The names `rotate-bit-field` and `reverse-bit-field` are replaced by `bit-field-rotate` and `bit-field-reverse`, in parallel with Olin's revisions.

* The procedures `copy-bit`, `integer->list` and `list->integer` are incorporated into this SRFI unchanged.

* The procedure `booleans->integer` is a convenient way to specify a bitwise integer: it accepts an arbitrary number of boolean arguments and returns a non-negative integer.  So in this SRFI it has the short name `bits`, roughly analogous to `list`, `string`, and `vector`.

### Other sources

* The following procedures are inspired by SRFI 133:  `bit-swap`, `bit-field-append`,
`bitwise-fold`, `bitwise-for-each`, `bitwise-unfold`.

* The procedure `bit-field-set` is the counterpart of `bit-field-clear`.

### Argument ordering and semantics

* In general, these procedures place the bitstring arguments to be operated on first.
> Where the operation is not commutative, the "destination" argument that provides
> the background to be operated on is placed before the "source" argument that provides
> the bits to be transferred to it.

* In SRFI 33, `bitwise-nand` and `bitwise-nor` accepted an arbitrary number of arguments
> even though they are not commutative.  Olin's late revisions made them dyadic, and so
> does this SRFI.

* Common Lisp bit-field operations use a *byte spec* to encapsulate the position and
> size of the field.  SRFI 33 bit-field operations had leading *position* and *size*
> arguments instead.  These
> have been replaced in this SRFI by trailing *start* (inclusive) and *end* (exclusive)
> arguments, the convention used not only in SRFI 60 and R6RS but also in most other
> subsequence operations in Scheme standards and SRFIs.

## Specification

In the following procedure specifications all parameters and return values
are exact integers unless otherwise indicated (except that procedures with
names ending in `?` are predicates, as usual). It is
an error to pass values of other types as arguments to these functions.

Bitstrings are represented by exact integers, using a two's-complement encoding of
the bitstring. Thus every integer represents a semi-infinite bitstring, having
either a finite number of zeros (negative integers) or a finite number of
ones (non-negative integers). The bits of a bitstring are numbered from the
rightmost/least-significant bit: bit !#0 is the rightmost or 2^0^ bit, bit !#1 is
the next or 2^1^ bit, and so forth.

### Basic operations

`(bitwise-not `*i*`)`

Returns the bitwise complement of *i*; that is, all 1 bits are changed
to 0 bits and all 0 bits to 1 bits.

```
  (bitwise-not 10) => -11
  (bitwise-not -37) => 36
```

The following ten procedures correspond to the useful set
of non-trivial two-argument boolean functions. For each such function,
the corresponding bitwise operator
maps that function across a pair of bitstrings in a bit-wise fashion.
The core idea of this group of functions is this bitwise "lifting"
of the set of dyadic boolean functions to bitstring parameters.

`(bitwise-and `*i* ...`)`[#BR]]
`(bitwise-ior `*i* ...`)`[#BR]]
`(bitwise-xor `*i* ...`)`[#BR]]
`(bitwise-eqv `*i* ...`)`

These operations are associative.  When passed no arguments, the procedures
return the identity values -1, 0, 0, and -1 respectively.

The `bitwise-eqv` procedure produces the
complement of the `bitwise-xor` procedure.  When applied to three
arguments, it does *not* produce a 1 bit
everywhere that a, b and c all agree. That is, it does *not* produce

```
     (bitwise-ior (bitwise-and a b c)
                  (bitwise-and (bitwise-not a)
                               (bitwise-not b)
                               (bitwise-not c)))
```

Rather, it produces `(bitwise-eqv a (bitwise-eqv b c))` or the equivalent
`(bitwise-eqv (bitwise-eqv a b) c)`.

```
      (bitwise-ior 3  10)     =>  11
      (bitwise-and 11 26)     =>  10
      (bitwise-xor 3 10)      =>   9
      (bitwise-eqv 37 12)     => -42
      (bitwise-and 37 12)     =>   4
```

`(bitwise-nand `*i j*`)`[#BR]]
`(bitwise-nor `*i j*`)`[#BR]]
`(bitwise-andc1 `*i j*`)`[#BR]]
`(bitwise-andc2 `*i j*`)`[#BR]]
`(bitwise-orc1 `*i j*`)`[#BR]]
`(bitwise-orc2 `*i j*`)`

These operations are not associative.

```
      (bitwise-nand 11 26) =>  -11
      (bitwise-nor  11 26) => -28
      (bitwise-andc1 11 26) => 16
      (bitwise-andc2 11 26) => 1
      (bitwise-orc1 11 26) => -2
      (bitwise-orc2 11 26) => -17
```

### Integer operations

`(arithmetic-shift `*i count*`)`

Returns the arithmetic left shift when *count*>0; right shift when *count*<0.

```
    (arithmetic-shift 8 2) => 32
    (arithmetic-shift 4 0) => 4
    (arithmetic-shift 8 -1) => 4
    (arithmetic-shift -100000000000000000000000000000000 -100) => -79
```

`(bit-count `*i*`)`

Returns the population count of 1's (*i* >= 0) or 0's (*i* < 0).  The result is always non-negative.

```
    (bit-count 0) =>  0
    (bit-count -1) =>  0
    (bit-count 7) =>  3
    (bit-count  13) =>  3 ;Two's-complement binary: ...0001101
    (bit-count -13) =>  2 ;Two's-complement binary: ...1110011
    (bit-count  30) =>  4 ;Two's-complement binary: ...0011110
    (bit-count -30) =>  4 ;Two's-complement binary: ...1100010
    (bit-count (expt 2 100)) =>  1
    (bit-count (- (expt 2 100))) =>  100
    (bit-count (- (1+ (expt 2 100)))) =>  1
```

`(integer-length `*i*`)`

The number of bits needed to represent *i*, i.e.

```
	(ceiling (/ (log (if (negative? integer)
			     (- integer)
			     (+ 1 integer)))
		    (log 2)))
```

The result is always non-negative.
>
For non-negative *i*, this is the number of bits needed to
represent *i* in an unsigned binary representation. For all *i*,
`(+ 1 (integer-length `*i*`))` is the number of bits needed
to represent *i* in a signed twos-complement
representation.
>
```
    (integer-length  0) => 0
    (integer-length  1) => 1
    (integer-length -1) => 0
    (integer-length  7) => 3
    (integer-length -7) => 3
    (integer-length  8) => 4
    (integer-length -8) => 3
```

`(bitwise-if `*mask i j*`)`

Merge the bitstrings *i* and *j*, with bitstring *mask* determining
from which string to take each bit. That is, if the *k*th bit of *mask*
is 0, then the *k*th bit of the result is the *k*th bit of *i*, otherwise
the *k*th bit of *j*.  This is equivalent to:

```
        (bitwise-ior (bitwise-and (bitwise-not mask) i)
                     (bitwise-and mask j))
```

### Single-bit operations

`(bit-set? `*index i*`)`

Is bit *index* set in bitstring *i* (where *index* is a non-negative exact
integer)?  As always, the rightmost/least-significant bit in *i* is bit 0.

```
    (bit-set? 1 1) =>  false
    (bit-set? 0 1) =>  true
    (bit-set? 3 10) =>  true
    (bit-set? 1000000 -1) =>  true
    (bit-set? 2 6) =>  true
    (bit-set? 0 6) =>  false
```

`(copy-bit `*index i boolean*`)`

Returns an integer the same as *i* except in the *index*th bit,
which is 1 if *boolean* is `#t` and 0 if *boolean* is `#f`.

```
(copy-bit 0 0 #t) => #b1
(copy-bit 2 0 #t) => #b100
(copy-bit 2 #b1111 #f) => #b1011
```


`(bit-swap `*index,,1,, index,,2,, i*`)`

Returns an integer the same as *i* except that the *index,,1,,*th bit
and the *index,,2,,*th bit have been exchanged.

```
(bit-swap 0 2 4) => #b1
```

`(any-bit-set? `*test-bits i*`)`[#BR]]
`(every-bit-set? `*test-bits i*`)`

Determines if any/all of the bits set in bitstring *test-bits* are set
in bitstring *i*. I.e.,  returns `(not (zero? (bitwise-and `*test-bits i*`)))`
or `(= `*test-bits*` (bitwise-and *test-bits i*)))` respectively.

`(first-set-bit `*i*`)`

Return the index of the first (smallest index) 1 bit in bitstring *i*.
Return -1 if *i* contains no 1 bits (i.e., if *i* is zero).

```
    (first-set-bit 1) => 0
    (first-set-bit 2) => 1
    (first-set-bit 0) => -1
    (first-set-bit 40) => 3
    (first-set-bit -28) => 2
    (first-set-bit (expt  2 99)) => 99
    (first-set-bit (expt -2 99)) => 99
```

### Bit field operations

These functions operate on a contiguous field of bits (a "byte," in
Common-Lisp parlance) in a given bitstring. The *start* and *end*
arguments, which are not optional, are
non-negative exact integers specifying the field: it is the *end-start* bits
running from bit *start* to bit *end*-1.

`(bit-field `*i start end*`)`

Returns the field from *i*, shifted
down to the least-significant position in the result.

`(bit-field-any? `*i start end*`)`

Returns true if any of the field's bits are set in bitstring *i*, and false otherwise.

`(bit-field-every? `*i start end*`)`

Returns false if any of the field's bits are not set in bitstring *i*, and true otherwise.

`(bit-field-clear `*i start end*`)`[#BR]]
`(bit-field-set `*i start end*`)`

Returns *i* with the field's bits set to all 0s/1s.

`(bit-field-replace `*dst src start end*`)`

Returns *dst* with the field replaced
by the least-significant *end-start* bits in *src*.

`(bit-field-replace-same `*dst src start end*`)`

Returns *dst* with its field replaced
by the corresponding field in *src*.

`(bit-field-rotate `*i count start end*`)`

Returns *i* with the field cyclically permuted
by *count* bits towards high-order.

`(bit-field-reverse `*i start end*`)`

Returns *i* with the order of the bits in the field reversed.

`(bit-field-append `*i start end* ...`)`

It is an error if the number of arguments is not a multiple of three.  The field specified
by each triple of *i start end* arguments is extracted, and the
fields are concatenated in left-to-right order and returned as an integer.

### Bits as booleans

`(integer->list `*i* [#|*len* ]]`)`[#BR]]
`(integer->vector `*i* [#|*len* ]]`)`

Returns a list/vector of *len* booleans corresponding to each bit of the non-negative integer *i*.
`#t` is returned for each 1; `#f` for 0. The *len* argument defaults to `(integer-length `*i*`)`.

`(list->integer `*list*`)`[#BR]]
`(list->integer `*list*`)`

Returns an integer formed from the booleans in *list/vector*;
it is an error if *list/vector* contains non-booleans.
A 1 bit is coded for each `#t`; a 0 bit for `#f`.
Note that the result is never a negative integer.
`integer->list` and `list->integer` are inverses in the sense of `equal?`,
and so are `integer->vector` and `vector->integer`.

`(bits `*bool* ...`)`

Returns the integer coded by the `bool` arguments.

### Fold, unfold, and generate

It is an error if the arguments named *proc, stop?, mapper, successor*
are not procedures.
The arguments named *seed* may be any Scheme object.

`(bitwise-fold `*proc seed i*`)`

For each bit *b* of *i* from bit 0 to `(integer-length `*i*`)`, *proc* is called as
`(`*proc b r*`)`, where *r* is the current accumulated result.  The initial value of *r*
is *seed*, and the value returned by *proc* becomes the next accumulated result.  When
all bits are exhausted, the final accumulated result becomes the result of `bitwise-fold`.

`(bitwise-for-each `*proc i*`)`

Repeatedly applies *proc* to the bits of *i* starting with 0 and ending with
`(integer-length `*i*`)`.  The values returned by *proc* are discarded.  Returns
an unspecified value.

`(bitwise-unfold `*stop? mapper successor seed*`)`

Generates a non-negative integer bit by bit, starting with bit 0.
If the result of applying *stop?* to the current state
(whose initial value is *seed*) is true, return the
currently accumulated bits as an integer.  Otherwise, apply *mapper*
to the current state to obtain the next bit of the result by interpreting
a true value as a 1 bit and a false value as a 0 bit.  Then get a new state
by applying *successor* to the current state, and repeat this algorithm.

`(make-bitwise-generator `*i*`)`

Returns a [SRFI 121](http://srfi.schemers.org/srfi-121/srfi-121.html)
generator that generates all the bits of *i* starting
with bit 0.  Note that it is an infinite generator.

## Comparison of proposals

The following table compares the names of the bitwise (aka logical) functions of Common Lisp, SRFI 33, Olin's revisions, SRFI 60, R6RS, and this SRFI.

|=Function=|=CL=|=SRFI 33=|=SRFI 33 late revs=|=SRFI 60=|=R6RS=|=This SRFI=|
|Bitwise NOT|`lognot`|`bitwise-not`|`bitwise-not`|`lognot`, `bitwise-not`|`bitwise-not`|`bitwise-not`|
|Bitwise AND|`logand`|`bitwise-and`|`bitwise-and`|`logand`, `bitwise-and`|`bitwise-and`|`bitwise-and`|
|Bitwise IOR|`logior`|`bitwise-ior`|`bitwise-ior`|`logior`, `bitwise-ior`|`bitwise-ior`|`bitwise-ior`|
|Bitwise XOR|`logxor`|`bitwise-xor`|`bitwise-xor`|`logxor`, `bitwise-xor`|`bitwise-xor`|`bitwise-xor`|
|Bitwise EQV|`logeqv`|`bitwise-eqv`|`bitwise-eqv`|---|---|`bitwise-eqv`|
|Bitwise NAND|`lognand`|`bitwise-nand`|`bitwise-nand`|---|---|`bitwise-nand`|
|Bitwise NOR|`lognor`|`bitwise-nor`|`bitwise-nor`|---|---|`bitwise-nor`|
|Bitwise AND with NOT of first arg|`logandc1`|`bitwise-andc1`|`bitwise-andc1`|---|---|`bitwise-andc1`|
|Bitwise AND with NOT of second arg|`logandc2`|`bitwise-andc2`|`bitwise-andc2`|---|---|`bitwise-andc2`|
|Bitwise OR with NOT of first arg|`logorc1`|`bitwise-orc1`|`bitwise-orc1`|---|---|`bitwise-orc1`|
|Bitwise OR with NOT of second arg|`logorc2`|`bitwise-orc2`|`bitwise-orc2`|---|---|`bitwise-orc2`|
|Arithmetic shift|`ash`|`arithmetic-shift`|`arithmetic-shift`|`ash`, `arithmetic-shift`|`bitwise-arithmetic-shift`|`arithmetic-shift`|
|Population count|`logcount`|`bit-count`|`bit-count`|`logcount`, `bit-count`|`bitwise-bit-count`|`bit-count`|
|Integer length|`integer-length`|`integer-length`|`integer-length`|`integer-length`|`bitwise-integer-length`|`integer-length`|
|Mask selects source of bits|---|`bitwise-merge`|`bitwise-merge`|`bitwise-if`, `bitwise-merge`|`bitwise-if`|`bitwise-if`|
|Test single bit|`logbitp`|`bit-set?`|`bit-set?`|`logbit?`, `bit-set?`|`bitwise-bit-set?`|`bit-set?`|
|See if any mask bits set|`logtest`|`any-bits-set?`|`any-bit-set?`|`logtest`, `any-bit-set?`|---|`any-bit-set`|
|See if all mask bits set|---|`all-bits-set?`|`every-bit-set?`|---|---|`every-bit-set?`|
|Replace single bit|---|---|`copy-bit`|`bitwise-copy-bit`|---|`copy-bit`|
|Swap bits|---|---|---|---|---|`bit-swap`|
|Find first bit set|---|`first-bit-set`|`first-set-bit`|`log2-binary-factors`, `first-set-bit`|---|`first-set-bit`|
|Extract bit field|`ldb`|`extract-bit-field`|`extract-bit-field`|`bit-field`|`bitwise-bit-field`|`bit-field`|
|Test bit field (any)|`ldb-test`|`test-bit-field?`|`bit-field-any?`|---|---|`bit-field-any?`|
|Test bit field (every)|---|---|`bit-field-every?`|---|---|`bit-field-every?`|
|Clear bit field|`mask-field`|`clear-bit-field`|`bit-field-clear`|---|---|`bit-field-clear`|
|Set bit field|---|---|---|---|---|`bit-field-set`|
|Replace bit field|`dpb`|`replace-bit-field`|`bit-field-replace`|`copy-bit-field`|`bitwise-copy-bit-field`|`bit-field-replace`|
|Replace corresponding bit field|`deposit-field`|`deposit-field`|`copy-bit-field`|---|---|`bit-field-copy-same`|
|Rotate bit field|---|---|---|`rotate-bit-field`|`bitwise-rotate-bit-field`|`bit-field-rotate`|
|Reverse bit field|---|---|---|`reverse-bit-field`|`bitwise-reverse-bit-field`|`bit-field-reverse`|
|Append bit fields|---|---|---|---|---|`bit-field-append`|
|Integer to boolean list|---|---|---|`integer->list`|---|`integer->list`|
|Integer to boolean vector|---|---|---|---|---|`integer->vector`|
|Boolean list to integer|---|---|---|`list->integer`|---|`list->integer`|
|Boolean vector to integer|---|---|---|---|---|`vector->integer`|
|Booleans to integer|---|---|---|`booleans->integer`|---|`bits`|
|Bitwise fold|---|---|---|---|---|`bitwise-fold`|
|Bitwise for-each|---|---|---|---|---|`bitwise-for-each`|
|Bitwise unfold|---|---|---|---|---|`bitwise-unfold`|




