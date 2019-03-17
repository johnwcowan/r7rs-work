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

`(bytestring->list `*bytevector*`)`

Convert a bytevector into a list containing suitable arguments for `bytestring`.
If `bytestring` is applied to the list, the resulting bytevector will be the same
(in the sense of `bytevector=?`) as *bytevector*, but the exact contents of
the list are not specified by this SRFI.

##Selection

`(bytestring-pad `*bytevector len char-or-u8*`)`

`(bytestring-pad-right `*bytevector len char-or-u8*`)`

Returns a bytevector with the contents of *bytevector* plus sufficient additional bytes
at the beginning/end containing *char-or-u8* (which can be either an
ASCII character or an integer in the range 0-255) such that the
length of the result is at least *len*.

`(bytestring-trim `*bytevector pred*`)`

`(bytestring-trim-right `*bytevector pred*`)`

`(bytestring-trim-both `*bytevector pred*`)`

Returns a bytevector with the contents of *bytevector*, except that consecutive
bytes at the beginning / the end / both the beginning and the end that satisfy
*pred* are not included.

##Replacement

`(bytestring-replace `*bytevector1 bytevector2 start1 end1 [start2 end2]*`)`

Returns a bytevector with the contents of *bytevector1*, except that the
bytes indexed by *start1* and *end1* are not included but are replaced by
the bytes of *bytevector2* indexed by *start* and *end*.

##Comparison

`bytestring=? ` *bytevector1 bytevector2*`)`

Returns `#t` if *bytevector1* and *bytevector2* are the
same length and contain the same bytes in the same order;
returns `#f` otherwise.

`bytestring<? ` *bytevector1 bytevector2*`)`

`bytestring>? ` *bytevector1 bytevector2*`)`

`bytestring<=? ` *bytevector1 bytevector2*`)`

`bytestring>=? ` *bytevector1 bytevector2*`)`

Returns `#t` if *bytevector1* is
less than / greater than / less than or equal to / greater than or equal to
*bytevector2*.  Comparisons are lexicographical: shorter bytevectors
compare before longer ones, all elements being equal.

`bytestring-ci=? ` *bytevector1 bytevector2*`)`

`bytestring-ci<? ` *bytevector1 bytevector2*`)`

`bytestring-ci>? ` *bytevector1 bytevector2*`)`

`bytestring-ci<=? ` *bytevector1 bytevector2*`)`

`bytestring-ci>=? ` *bytevector1 bytevector2*`)`

The same as the corresponding procedures without `-ci`, except that
the comparison is done as if any
elements in either *bytevector* that are in the range 65-90 have
had 32 added to them.

##Prefixes and suffixes

`(bytestring-prefix-length `*bytevector1 bytevector2*`)`

`(bytestring-suffix-length `*bytevector1 bytevector2*`)`

Returns the length of the common prefix / suffix of
*bytevector1* and *bytevector2* as an exact integer.

`(bytestring-prefix? `*bytevector1 bytevector2*`)`

`(bytestring-suffix? `*bytevector1 bytevector2*`)`

Returns `#t` if *bytevector1* is a prefix / suffix
of *bytevector2*, and `#f` otherwise.
  
##Searching

`(bytestring-index `*bytevector pred* [*start* [*end*]]`)`

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

`(bytestring-join `*bytevector-list delimiter* [*grammar*]`)`

Pastes the bytevectors in *bytevector-list*  together using the *delimiter* bytevector.
The *grammar* argument is a symbol that determines how the delimiter is used, and defaults to `infix`.
It is an error for grammar to be any symbol other than these four:

  * `infix` means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty string.
  * `strict-infix` means the same as 'infix if the list is non-empty, but will signal an error satisfying `bytestring-error?` if given an empty list.
  *  `suffix` means a suffix or terminator grammar: insert the delimiter after every list element.
  *  `prefix` means a prefix grammar: insert the delimiter before every list element.

`(bytestring-split `*bytevector delimiter* [*grammar*]`)`

Divides the elements of *bytevector* and returns a list of bytevectors using the
*delimiter* byte.  Delimiter bytes are not included in the result bytevectors.
The *grammar* argument has the same default and meaning as in `bytestring-join`,
except that `infix` and `strict-infix` mean the same thing.

##Output
`(write-bytestring `*port arg* ...`)`

Outputs each *arg* to *port* using the same interpretations as `bytestring`,
but does not create any bytevectors.

##Exception

`(bytestring-error? `*obj*`)`

Returns `#t` if *obj* is an object signaled by `bytestring`,
`list->bytestring`, `bytestring-join`, `bytestring-split`
or `write-bytestring` in the circumstances described above.
