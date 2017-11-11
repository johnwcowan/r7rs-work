(note this is a work in progress)

## Introduction

Bitwise operations are an essential component of computing in binary. They are used in efficient implementations of data structures (sets, hash tables, etc.), in reading and writing wire protocols and file formats, in encryption algorithms and chess-playing programs. However, while previous Scheme standards have provided comprehensive support for numeric types and basic operations over these types, bitwise operations have been curiously missing from the standard until R6RS. This proposal is an attempt to standardize as much of R6RS's bitwise-arithmetic semantics as is useful in a small core language.

Most Schemes that supply bitwise operators do so using one of two conventions. The first is the Common Lisp convention where bitwise arithmetic procedures are named after their Common Lisp counterparts: `logand`, `logior`, etc. The second uses longer names; e.g., `bitwise-and`, `bitwise-ior`, etc., and is shared with R6RS. It is this convention which will be specified here; though this proposal deviates from the R6RS names in some cases, substituting shorter names commonly used in R5RS implementations.

## The Specification

All operations are defined only over the exact integers, and assume the arguments to be semi-infinite-precision two's complement, meaning that any positive argument is considered to be padded on the left by an infinite number of 0 bits; and any negative argument is considered to be padded on the left by an infinite number of 1 bits. These procedures are to be considered optional, and it is yet to be decided whether they belong in the core language or in a module.

`(bitwise-and int ...)`

Returns the bitwise AND of its arguments. A bit is set in the result if the corresponding bit is set in all of its arguments. If called with no arguments, returns -1 (all 1 bits).

`(bitwise-ior int ...)`

Returns the bitwise inclusive OR of its arguments. A bit is set in the result if the corresponding bit is set in any of its arguments. If called with no arguments, returns 0 (all 0 bits).


`(bitwise-xor int ...)`

Returns the bitwise exclusive OR of its arguments. A bit is set in the result if the corresponding bit is set in an odd number of its arguments. If called with no arguments, returns 0 (all 0 bits).

`(bitwise-not int)`

Returns the bitwise NOT, or complement, of its argument. If a bit is set in the argument, the corresponding bit will be cleared in the result, and vice-versa.

`(bitwise-if int1 int2 int3)`

Returns the bitwise IF of its arguments. If a bit is set in *int1*, then the value of corresponding bit in the result will be the value of the corresponding bit in *int2*; otherwise the value of the corresponding bit in *int3*.

`(arithmetic-shift int1 int2)`

Returns *int1* shifted to the left by *int2* bits (right if *int2* is negative).

`(bitwise-bit-set int1 int2)`

Returns *int1* with bit *int2* set. Equivalent to `(bitwise-or int1 (arithmetic-shift 1 int2))`.

`(bitwise-bit-clear int1 int2)`

Returns *int1* with bit *int2* cleared. Equivalent to `(bitwise-and int1 (not (arithmetic-shift 1 int2)))`.

`(bitwise-bit-set? int1 int2)`

Returns `#t` if bit *int2* is set in *int1* and `#f` otherwise.

`(bit-count int)`

Returns the number of 1 bits in *int*. If *int* is negative, returns the number of 0 bits.

`(integer-length int)`

Return the number of bits it takes to represent *int*. This is equal to the position of the most significant 1 bit in *int* (0 bit if *int* is negative), where 0 represents the least significant bit.
