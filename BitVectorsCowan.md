## Specification

### Constructor

`(make-bitvector ` [[|*size* []] ]( *fill*)`)`

Returns a newly allocated bitvector.  If *fill* is omitted or `#f`, then all the bits are `#f`; if *fill* is `#t`, the the first *size* bits are `#t` and all other bits are `#f`.

### Predicate

`(bitvector? `*obj*`)`

Returns `#t` if *obj* is a bitvector, and `#f` otherwise.

### Bitwise operations

Same as [BitwiseCowan](BitwiseCowan.md), with appropriate name changes.

### Mutators

Issue:  Linear update, or guaranteed mutation?

```
bitvector-not!
bitvector-and!   bitvector-ior!
bitvector-xor!   bitvector-eqv!
bitvector-nand!  bitvector-nor! 
bitvector-andc1! bitvector-andc2!
bitvector-orc1!  bitvector-orc2! 

copy-bit!      bit-swap!

bitvector-field-clear!   bitvector-field-set
bitvector-field-replace  bitvector-field-replace-same
bitvector-field-rotate!  bitvector-field-reverse
```

### Conversion

`(bitvector->vector `*bv*`)`

`(vector->bitvector `*vec*`)`

`(bitvector->integer `*bv*`)`

`(integer->bitvector `*integer*`)`

