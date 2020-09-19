## Abstract

These procedures allow the creation and interpretation of numerals
using any set of Unicode digits that support positional notation.

## Rationale

Although the positional decimal numeral system most widely used to write numbers
is often called the
[Hindu-Arabic numeral system](https://en.wikipedia.org/wiki/Hindu%E2%80%93Arabic_numeral_system),
the form of the digits 0-9 that evolved in Europe and are now used worldwide
is not their only possible representation.  In particular, it is not usually
used with either the various Indic scripts or the Arabic script.  The digits that are
used instead are functionally identical, but their shape is different,
and each one has a different set of digit characters in Unicode.
For example, the number 12345 is written as ١٢٣٤٥ in Eastern Arabic digits
(used with Persian, Urdu, and other languages),
and १२३४५ in Devanagari digits (used with Hindi and other languages).

Although R7RS-small Scheme permits
non-European digits to be used in identifiers,
there is very little support for using them in numbers.
The `digit-value` procedure allows converting
a single character between `#\0` and `#\9` to its
equivalent in another digit set by specifying the 0 digit: 
thus `(digit-value #\5 #\०) => #\५`, which is Devanagari digit 5.
(The 0 digit can be specified as `#\x966` instead.)
This SRFI allows numbers of arbitrary types
to be converted to use any digit set.

No support is provided for bases other than 10,
because such bases are rarely used with any other digit set,
and because it is unclear what characters should be used
to represent digits greater than 9.
Likewise, there is no support for numerals that are not positional,
such as Roman numerals or traditional Tamil numerals, which
have nothing corresponding to 0 but do have numerals
for 10, 1000, and 1000, so that 2718 would be ௨௲௭௱௰௮,
literally "2 1000 7 100 10 8".


## Specification

`(number->numeral `*z zero*`)`  
`(numeral->number `*string zero*`)`

These procedures behave identically to `number->string`
and `string->number` from the `(scheme base)` library,
except that where `number->string` generates,
and `string->number` accepts, a `0` character,
these procedures generate and accept a character equal to *zero*.
Similarly, the successor (in Unicode ordering)
of *zero* is generated and accepted in place of `1`,
the successor of the successor of *zero*
is generated and accepted in place of `2`, and so on.

It is an error if *zero* is not one of the characters
with Unicode general category equal to Nd (decimal digit)
and numeric value equal to 0.

Examples:

```
(number->numeral 3.1415৩ #\x9E6) => "৩.১৪১৫"    ; BENGALI DIGIT ZERO
(numeral->number "๓๕๕/๑๑๓" #\xE50) => 355/113   ; THAI DIGIT ZERO
```
