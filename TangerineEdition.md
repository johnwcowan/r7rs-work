This is the definition of the Tangerine Edition.


**Data structures**

Strings: No final result: this will be reballoted for the Green Edition. 

Mappings: [SRFI 146](http://srfi.schemers.org/srfi-146/srfi-146.html)
as `(scheme mapping)` and `(scheme mapping hash)`.

Regular expressions: [SRFI 115](http://srfi.schemers.org/srfi-115/srfi-115.html) as `(scheme regex)`.

Generators: [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html)
as `(scheme generator)`, superseding and upward compatible with the Red Edition library of the same name.

**Numeric types and operations**

Integer division: [SRFI 141](http://srfi.schemers.org/srfi-141/srfi-141.html) as `(scheme division)`

Bitwise integer operations:  [SRFI 151](http://srfi.schemers.org/srfi-151/srfi-151.html)
as `(scheme bitwise)`.

Fixnums:  [SRFI 143](http://srfi.schemers.org/srfi-143/srfi-143.html)
as `(scheme fixnum)`.

Flonums:  [SRFI 144](http://srfi.schemers.org/srfi-144/srfi-144.html)
as `(scheme flonum)`

**Numeric and semi-numeric data structures**

Bytevectors: [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2)
`(rnrs bytevectors)` as `(scheme bytevector)` (note singular form).

Numeric vectors: 
[SRFI 160](http://srfi.schemers.org/srfi-160/srfi-160.html) as
`(scheme vector @)`
where `@` is any of `base, u8, s8, u16, s16, u32, s32, u64, s64, f32, f64, c64, c128`.

**Formatting**

Formatting: [SRFI 159](http://srfi.schemers.org/srfi-159/srfi-159.html)
as `(scheme show)`.

**Required numeric types**

The Tangerine Edition requires all implementations to provide unbounded
exact integers, unbounded exact rationals, inexact reals, and exact and inexact
complex numbers.  This is the same numeric tower that R6RS requires.

