**Numeric types and operations**

Random numbers: [SRFI 27](http://srfi.schemers.org/srfi-27/srfi-27.html),
plus [AdvancedRandomGauche](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/AdvancedRandomGauche.html)
or [RandomnessCommonLisp](RandomnessCommonLisp.md)
or [RandomnessElf](https://regmedia.co.uk/2018/10/01/roig_paper.pdf)

Natural number predicates (from WG1): `exact-positive-integer?` and `exact-non-negative-integer?`.

Float and NaN dissector API (sign, quiet/signaling status, and integer tag): [FloatCLMedernach](FloatCLMedernach.md)

R6RS versions of `real?`, `rational?`, `integer?`: these are false if the imaginary part is an inexact zero; the issue here is what to name them to avoid confusion

Bignum encoding: [BignumEncodingCowan](BignumEncodingCowan.md)

Combinator sublibrary: [SRFI 162](http://srfi.schemers.org/srfi-162/srfi-162.html)

Logistic functions: [LogisticRiastradh](LogisticRiastradh.md)

**Numeric and semi-numeric data structures**

C-style structs:  [ByteStructuresTaylanub](https://github.com/TaylanUB/scheme-bytestructures).

Integer sets:  [IntegerSetsCowan](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/IntegerSetsCowan.html)

Descriptive statistics:  [TallyCowan](TallyCowan.md)

Ranges:  [RangesCowan](RangesCowan.md)

BitVectors: [BitVectorsCowan](BitVectorsCowan.md)

Bytestrings: [BytestringsCowan](BytestringsCowan.md)

**Enumerations**

Enumerations: [EnumsCowan](EnumsCowan.md)

Enumeration sets and maps: [EnumContainersCowan](EnumContainersCowan.md)

**Other**

Combinations: [CombinationsGauche](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/CombinationsGauche.html)

Number to string with Unicode: [NumberStringUnicode](NumberStringUnicode.md)

