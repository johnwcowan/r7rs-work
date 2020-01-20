SRFI 167 does not define a particular means of encoding and decoding multiple Scheme values
for use by a byte-oriented key-value store.
The sample implementation, however, does provide a specific format.
Although there is no requirement that other implementations use this method also,
interchange is made easier if all implementations use the same method.
This explains the method as it currently exists, with some suggested enhancements.

The library `(srfi 167 pack)` exports two procedures, `pack` and `unpack`.
The first accepts any number of arguments and returns a bytevector containing
the encoded sequence; the second accepts a single bytevector argument
and returns the decoded sequence as a list.

The following types can be encoded: integers from -2^63 to 2^63-1,
double precision floats, strings, symbols, booleans, and subsequences
whose members are any of these types.
In addition there is a representation for a null object which is not
the same (in the sense of `eq?`) as any other encodable object.
(Floats and subsequences are currently not implemented.)
Each value is a single-byte type followed by zero or more data bytes.
Here is a table of type bytes:

```
0x00 null object
0x01 bytevector
#x02 string
#x03 symbol
#x05 start of subsequence
#x06 end of subsequence (proposed)
#x08 (reserved)
#x09-0x13 negative integer
0x14 0
0x15-0x1C positive integer
#x1D (reserved)
0x21 float
#x26 #f
0x27 #t
#xFF (reserved)
```

The codes `#x08`, `#x1D`, and `#xFF` are permanently reserved.
Other codes not listed in the above table are reserved for future use.

The encodings for the null object, `0`, `#f`, and `#t` have no data bytes.

The data bytes of an encoded byvector are the bytes of the bytevector
after they have undergone *byte stuffing*.  The byte stuffing algorithm
converts any existing `#x00` byte to the two-byte sequence `#x00 #xFF`,
and appends an `#x00` byte to the end of the bytevector.
Because `#xFF` is not a type code, such an `#x00` byte
cannot be mistaken for the terminating byte.

A string is converted to bytes using the UTF-8 encoding and the resulting
bytes are stuffed to become the data bytes.  Similarly, a symbol is converted
to a string, then to a bytevector, and then stuffed.

A floating-point number has exactly 8 data bytes representing an IEEE binary64
representation.  It is unspecified whether the number appears in big-endian order
(typical of network transmission) or little-endian order (typical of most hardware).

A positive integer uses the smallest number of data bytes that can represent it,
stored in big-endian order.
The number of data bytes used, *k*, is specified by a type code of `0x14 + *k*`.

A negative integer uses the smallest number of data bytes that can represent it.
They are stored in big-endian order using *one's complement*; that is, each
data byte is the same as that used by the corresponding positive number,
but with the bits of the byte complemented.  Thus `#x01` becomes `#xFE`.
The number of data bytes used, *k*, is specified by a type code of `0x14 - *k*`.

A subsequence is simply the start of subsequence type code followed by the encodings
of the members of the sequence followed by the end of subsequence type code.
