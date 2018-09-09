##Constructor

`(bytestring `*arg* ...`)`

Converts *args* into a sequence of small integers and
concatenates them as a bytevector.

If *arg* is an integer in the range 0-255, it is added
to the result.

If *arg* is a printable ASCII character (that is, its
codepoint is in the range 32-126 inclusive), it is
converted to its codepoint and added to the result.
If the codepoint is not in this range, an error
satisfying `codepoint-error?` is signaled.

If *arg* is a bytevector, its elements are added to the result.

If *arg* is a string of printable ASCII characters, it is
converted to a sequence of codepoints which are added to the result.
If any codepoint is not in the range 32-126 inclusive, an error
satisfying `codepoint-error?` is signaled.

##Conversion

`(bytevector->hex-string `*bytestring*`)`

Converts a bytevector into a string containing pairs of
hexadecimal digits.

`(hex-string->bytevector `*string*`)`

Converts a string containing pairs of hexadecimal digits
into a bytevector.

##Selection

`(bytestring-pad `*bytevector len [char-or-u8 start end]*`)`

`(bytestring-pad-right `*bytevector len [char-or-u8 start end]*`)`

`(bytestring-trim `*bytevector [pred start end]*`)`

`(bytestring-trim-right `*bytevector [pred start end]*`)`

`(bytestring-trim-both `*bytevector [pred start end]*`)`

##Replacement

`(bytestring-replace `*bytevector1 bytevector2 start1 end1 [start2 end2]*`)`

##Comparison

`bytestring=? ` *bytevector1 bytevector2*`)`

`bytestring<? ` *bytevector1 bytevector2*`)`

`bytestring>? ` *bytevector1 bytevector2*`)`

`bytestring<=? ` *bytevector1 bytevector2*`)`

`bytestring>=? ` *bytevector1 bytevector2*`)`

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

`(bytestring-prefix-length `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`

`(bytestring-prefix-length `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`

`(bytestring-suffix-length `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`

`(bytestring-prefix? `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`

`(bytestring-suffix? `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`
  
##Searching

`(bytestring-contains `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`

`(bytestring-contains-right `*bytevector1 bytevector2 [start1 end1 start2 end2]*`)`

`(bytestring-break `*bytevector pred [start end]*`)`

`(bytestring-span `*bytevector pred [start end]*`)`

##Joining and splitting

`(bytestring-join `*bytevector-list [delimiter grammar]*`)`

`(bytestring-split `*bytevector delimiter [grammar limit start end]*`)`

##Exception

`(bytestring-error? `*obj*`)`

Returns `#t` if *obj* is an object signaled by `bytestring` in the
circumstances described above.
