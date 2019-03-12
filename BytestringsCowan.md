##Constructor

`(bytestring `*arg* ...`)`

Converts *args* into a sequence of small integers and
returns them as a bytevector as follows:

If *arg* is an integer in the range 0-255, it is added
to the result.

If *arg* is a printable ASCII character (that is, its
codepoint is in the range 32-126 inclusive), it is
converted to its codepoint and added to the result.

If *arg* is a bytevector, its elements are added to the result.

If *arg* is a string of printable ASCII characters, it is
converted to a sequence of codepoints which are added to the result.

Otherwise, an error satisfying `bytestring-error?` is signaled.

##Conversion

`(bytevector->hex-string `*bytestring*`)`

`(hex-string->bytevector `*string*`)`

Converts between a bytevector and a string containing pairs of
hexadecimal digits.

`(bytevector->base64 `*bytevector* [*digits*]`)`

`(base64->bytevector `*string* [*digits*]`)`

Converts between a bytevector and its base-64 encoding as a string.
The 64 digits are represented by the characters 0-9, A-Z, a-z, and
the symbols + and /.  However, there are different variants of
base-64 encoding which use different representations of the 62nd
and 63rd digit.  If the optional argument *digits* (a two-character
string) is provided, those two characters will be used as the
62nd and 63rd digit instead.

`(list->bytestring `*list*`)`

Convert a list, which must contain suitable arguments for `bytestring`, into a bytevector.

`(bytestring->list `*list*`)`

Convert a bytestring into a list containing suitable arguments for `bytestring`.  The implementation
will compress consecutive printable ASCII codepoints into a string, but will not compress other values
into a bytevector; they will be returned as integers.

##Selection

`(bytestring-pad `*bytevector len char-or-u8*`)`

`(bytestring-pad-right `*bytevector len char-or-u8*`)`

`(bytestring-trim `*bytevector pred*`)`

`(bytestring-trim-right `*bytevector pred*`)`

`(bytestring-trim-both `*bytevector pred*`)`

##Replacement

`(bytestring-replace `*bytevector1 bytevector2 start1 end1 [start2 end2]*`)`

##Comparison

`bytestring=? ` *bytevector1 bytevector2*`)`

`bytestring<? ` *bytevector1 bytevector2*`)`

`bytestring>? ` *bytevector1 bytevector2*`)`

`bytestring<=? ` *bytevector1 bytevector2*`)`

`bytestring>=? ` *bytevector1 bytevector2*`)`

Comparisons are lexicographical: shorter bytevectors
compare before longer ones, all elements being equal.

`bytestring-ci=? ` *bytevector1 bytevector2*`)`

`bytestring-ci<? ` *bytevector1 bytevector2*`)`

`bytestring-ci>? ` *bytevector1 bytevector2*`)`

`bytestring-ci<=? ` *bytevector1 bytevector2*`)`

`bytestring-ci>=? ` *bytevector1 bytevector2*`)`

The same as the corresponding procedures without `-ci`, except that
the comparison is done as if any
elements in either *bytevector* in the range 65-90 have
had 32 added to them.

##Prefixes and suffixes

`(bytestring-prefix-length `*bytevector1 bytevector2*`)`

`(bytestring-prefix-length `*bytevector1 bytevector2*`)`

`(bytestring-suffix-length `*bytevector1 bytevector2*`)`

`(bytestring-prefix? `*bytevector1 bytevector2*`)`

`(bytestring-suffix? `*bytevector1 bytevector2*`)`
  
##Searching

`(bytestring-index `*bytevector pred* [*start* [*end*]]`)
`
`(bytestring-index `*bytevector pred* [*start* [*end*]]`)`

Search *bytevector* from *start* to *end* / from *end*
to *start* for the first byte that satisfies *pred*, and
return the index into *bytevector* containing that byte.

`(bytestring-break `*bytevector pred*`)`

`(bytestring-span `*bytevector pred*`)`

Return two values, a bytevector containing the maximal
sequence of characters (searching from the beginning
to the end that do not satisfy / do satisfy *pred*,
and another bytevector containing the remaining characters.

##Joining and splitting

`(bytestring-join `*bytevector-list delimiter grammar*`)`

`(bytestring-split `*bytevector delimiter grammar*`)`

##Output
`(write-bytestring `*port arg* ...`)`

Outputs each *arg* to *port* using the same interpretations as `bytestring`, but does not create any bytevectors.

##Exception

`(bytestring-error? `*obj*`)`

Returns `#t` if *obj* is an object signaled by `bytestring`,
`list->bytestring`, or `write-bytestring` in the
circumstances described above.
