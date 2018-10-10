## Specification

This is a library for doing bitwise operations in the style
of [SRFI 151](http://srfi.schemers.org/srfi-151/srfi-151.html),
but on [SRFI 160](http://srfi.schemers.org/srfi-160/srfi-160.html) u1vectors
rather than integers.

### Bitwise operations

```

bitvector-not
bitvector-and   bitvector-ior 
bitvector-xor   bitvector-eqv
bitvector-nand  bitvector-nor 
bitvector-andc1 bitvector-andc2
bitvector-orc1  bitvector-orc2 

bitvector-logical-shift  bitvector-count 
bitvector-integer-length bitvector-if 

bitvector-any-bit-set? bitvector-every-bit-set?
first-set-bit

bit-field bit-field-any? bit-field-every?
bit-field-clear bit-field-set
bit-field-replace  bit-field-replace-same
bit-field-rotate bit-field-reverse

bitvector->list list->bitvector
bitvector-fold bitvector-for-each bitvector-unfold
make-bitvector-generator
```

### Mutators

These functions mutate the (first) bitvector argument and return
an unspecified value.

```

bitvector-not!
bitvector-and!   bitvector-ior!
bitvector-xor!   bitvector-eqv!
bitvector-nand!  bitvector-nor!
bitvector-andc1! bitvector-andc2!
bitvector-orc1!  bitvector-orc2!

bitvector-logical-shift!
bitvector-if!

bitvector-field-clear!   bitvector-field-set
bitvector-field-replace  bitvector-field-replace-same
bitvector-field-rotate!  bitvector-field-reverse
```

### Conversion

`(bitvector->list `*bv*`)`

`(list->bitvector `*vec*`)`

`(bitvector->integer `*bv*`)`

`(integer->bitvector `*integer*`)`

Note that when converting between bitvectors and integers the order of bits is reversed,
because bitvectors are numbered in a big-endian way (the first bit is 0) whereas the bits
of integers are numbered in a little-endian way (the least significant bit is 0).  This
may seem strange, but provides consistent results.
