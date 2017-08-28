This is an early draft of proposals for the Orange Edition (numbers) of R7RS-large. For other dockets, see [wiki:WG2Dockets].

'''Numeric types and operations'''

Integer division: [[http://srfi.schemers.org/srfi-141/srfi-141.html|SRFI 141]]

Bitwise integer operations:  [[http://srfi.schemers.org/srfi-151/srfi-151.html|SRFI 151]] (superseding SRFI 142), [[http://srfi.schemers.org/srfi-60/srfi-60.html|SRFI 60]], [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.4|R6RS]].

Fixnums:  [[http://srfi.schemers.org/srfi-143/srfi-143.html|SRFI 143]], [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.2|R6RS]]

Flonums:  [[http://srfi.schemers.org/srfi-144/srfi-1443.html|SRFI 144]],
[[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-12.html#node_sec_11.3|R6RS]]

Random numbers: [[http://srfi.schemers.org/srfi-27/srfi-27.html|SRFI 27]], plus AdvancedRandomGauche

Prime numbers:  PrimesGauche.

Extended exact numbers:  ExtendedRationalsCowan [Orange]

Natural number predicates (from WG1):  `exact-positive-integer?` and `exact-non-negative-integer?`.

NaN dissector API (sign, quiet/signaling status, and integer tag): NanMedernach

'''Numeric and semi-numeric data structures'''

Numeric vectors:  [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2,|R6RS]], [[http://srfi.schemers.org/srfi-4/srfi-4.html|SRFI 4]], [[http://srfi.schemers.org/srfi-63/srfi-63.html|SRFI 63]] (supersedes SRFI 25 and SRFI 47; lexical syntax in [[http://srfi.schemers.org/srfi-58/srfi-58.html|SRFI 58]]), [[http://srfi.schemers.org/srfi-66/srfi-66.html|SRFI 66]], [[http://srfi.schemers.org/srfi-74/srfi-74.html|SRFI 74]], NumericVectorsCowan.

C-style structs:  [[https://github.com/TaylanUB/scheme-bytestructures|ByteStructuresTaylanub]].

Integer sets:  IntegerSetsCowan

Descriptive statistics:  TallyCowan

Multidimensional arrays: [[http://srfi.schemers.org/srfi-122/srfi-122.html|SRFI 122]] .

'''Enumerations'''

Enumerations:  EnumsCowan

Enumeration sets:  EnumContainersCowan

Enumeration maps:  EnumContainersCowan

'''Formatting'''

`number->string` and `string->number` with non-ASCII decimal digits: NumberStringUnicode

`hex-digit-value`: same as `digit value`, but understands A-F and a-f too

Formatting: [[http://srfi.schemers.org/srfi-159/srfi-159.html|SRFI 159]] or [[http://srfi.schemers.org/srfi-48/srfi-48.html|SRFI 48]] (intermediate), possibly with [[http://srfi.schemers.org/srfi-29/srfi-29.html|SRFI 29]] or GettextCowan

'''Carryovers from the Red Edition'''

Strings: [[http://srfi.schemers.org/srfi-13/srfi-13.html|SRFI 13]] (comprehensive), [[http://srfi.schemers.org/srfi-118/srfi-118.html|SRFI 118]] (adjustable), [[http://srfi.schemers.org/srfi-130/srfi-130.html|SRFI 130]] (cursor-based), [[http://srfi.schemers.org/srfi-140/srfi-140.html|SRFI 140]] (immutable), [[http://srfi.schemers.org/srfi-152/srfi-152.html|SRFI 152]] (basic)

Ordered sets and bags:  [[http://srfi.schemers.org/srfi-153/srfi-153.html|SRFI 153]]

Mappings: [[http://srfi.schemers.org/srfi-146/srfi-146.html|SRFI 146]]

Regular expressions: [[http://srfi.schemers.org/srfi-115/srfi-115.html|SRFI 115]]


