== Procedures ==

These procedures allow the creation and processing of numerals using any set of Unicode digits.

`(number->numeral `''z zero''`)`

`(numeral->number `''string zero''`)`

It is an error if ''zero'' is not a character whose Unicode general category is Nd (decimal digit) and whose numeric value is zero.

These procedures behave identically to `number->string` and `string->number` from the `(scheme base)` library, except that where `number->string` generates and `string->number` accepts a `0` character, these procedures generate and accept a character equal to ''zero''.  Similarly, the successor of ''zero'' is generated and accepted in place of `1`, the successor of the successor of ''zero'' is generated and accepted in place of `2`, and so on.  Note that the radix is always decimal.