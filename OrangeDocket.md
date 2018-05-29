**Numeric types and operations**

Random numbers: [SRFI 27](http://srfi.schemers.org/srfi-27/srfi-27.html), plus [AdvancedRandomGauche](AdvancedRandomGauche.md)

Prime numbers:  [PrimesGauche](PrimesGauche.md).

Natural number predicates (from WG1):  `exact-positive-integer?` and `exact-non-negative-integer?`.

NaN dissector API (sign, quiet/signaling status, and integer tag): [NanMedernach](NanMedernach.md)

R6RS versions of `real?`, `rational?`, `integer?`: these are false if the imaginary part is an inexact zero; the issue here is what to name them to avoid confusion

**Numeric and semi-numeric data structures**

C-style structs:  [ByteStructuresTaylanub](https://github.com/TaylanUB/scheme-bytestructures).

Integer sets:  [IntegerSetsCowan](IntegerSetsCowan.md)

Descriptive statistics:  [TallyCowan](TallyCowan.md)

Maximal and minimal elements of lists and vectors:

**Enumerations**

Enumerations: [EnumsCowan](EnumsCowan.md)

Enumeration sets and maps: [EnumContainersCowan](EnumContainersCowan.md)

