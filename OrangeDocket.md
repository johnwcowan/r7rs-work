This is an early draft of proposals for the Orange Edition (numbers) of R7RS-large. For other dockets, see [wiki:WG2Dockets].

**Numeric types and operations**

Integer division: [SRFI 141](http://srfi.schemers.org/srfi-141/srfi-141.html)

Bitwise integer operations:  [SRFI 151](http://srfi.schemers.org/srfi-151/srfi-151.html) (superseding SRFI 142), [SRFI 60](http://srfi.schemers.org/srfi-60/srfi-60.html), [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.4).

Fixnums:  [SRFI 143](http://srfi.schemers.org/srfi-143/srfi-143.html), [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.2)

Flonums:  [SRFI 144](http://srfi.schemers.org/srfi-144/srfi-1443.html),
[R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.3)

Random numbers: [SRFI 27](http://srfi.schemers.org/srfi-27/srfi-27.html), plus [AdvancedRandomGauche](AdvancedRandomGauche.md)

Prime numbers:  [PrimesGauche](PrimesGauche.md).

Extended exact numbers:  [ExtendedRationalsCowan](ExtendedRationalsCowan.md) [Orange]

Natural number predicates (from WG1):  `exact-positive-integer?` and `exact-non-negative-integer?`.

NaN dissector API (sign, quiet/signaling status, and integer tag): [NanMedernach](NanMedernach.md)

**Numeric and semi-numeric data structures**

Numeric vectors:  [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2,), [SRFI 4](http://srfi.schemers.org/srfi-4/srfi-4.html), [SRFI 63](http://srfi.schemers.org/srfi-63/srfi-63.html) (supersedes SRFI 25 and SRFI 47; lexical syntax in [SRFI 58](http://srfi.schemers.org/srfi-58/srfi-58.html)), [SRFI 66](http://srfi.schemers.org/srfi-66/srfi-66.html), [SRFI 74](http://srfi.schemers.org/srfi-74/srfi-74.html), [NumericVectorsCowan](NumericVectorsCowan.md).

C-style structs:  [ByteStructuresTaylanub](https://github.com/TaylanUB/scheme-bytestructures).

Integer sets:  [IntegerSetsCowan](IntegerSetsCowan.md)

Descriptive statistics:  [TallyCowan](TallyCowan.md)

Multidimensional arrays: [SRFI 122](http://srfi.schemers.org/srfi-122/srfi-122.html) .

**Enumerations**

Enumerations:  [EnumsCowan](EnumsCowan.md)

Enumeration sets:  [EnumContainersCowan](EnumContainersCowan.md)

Enumeration maps:  [EnumContainersCowan](EnumContainersCowan.md)

**Formatting**

`number->string` and `string->number` with non-ASCII decimal digits: [NumberStringUnicode](NumberStringUnicode.md)

`hex-digit-value`: same as `digit value`, but understands A-F and a-f too

Formatting: [SRFI 159](http://srfi.schemers.org/srfi-159/srfi-159.html) or [SRFI 48](http://srfi.schemers.org/srfi-48/srfi-48.html) (intermediate), possibly with [SRFI 29](http://srfi.schemers.org/srfi-29/srfi-29.html) or [GettextCowan](GettextCowan.md)

**Carryovers from the Red Edition**

Strings: [SRFI 13](http://srfi.schemers.org/srfi-13/srfi-13.html) (comprehensive), [SRFI 118](http://srfi.schemers.org/srfi-118/srfi-118.html) (adjustable), [SRFI 130](http://srfi.schemers.org/srfi-130/srfi-130.html) (cursor-based), [SRFI 140](http://srfi.schemers.org/srfi-140/srfi-140.html) (immutable), [SRFI 152](http://srfi.schemers.org/srfi-152/srfi-152.html) (basic)

Ordered sets and bags:  [SRFI 153](http://srfi.schemers.org/srfi-153/srfi-153.html)

Mappings: [SRFI 146](http://srfi.schemers.org/srfi-146/srfi-146.html)

Regular expressions: [SRFI 115](http://srfi.schemers.org/srfi-115/srfi-115.html)


