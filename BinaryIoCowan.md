## Bignums

Since Schemes may support unlimited size bignums it is useful to support the binary encoding
of such values.

A BER (Basic Encoding Rules from X.690) compressed integer is an unsigned integer in base 128,
most significant digit first, where the high bit is set on all but the final (least significant)
byte. Thus any size integer can be encoded, but the encoding is efficient and small integers
don't take up any more space than they would in normal char/short/int encodings.
This is commonly used to encode an unlimited length field, and can form the basis
for other variable length encodings.

Examples of integers converted to BER byte sequences:

            3 => #x03
          555 => #x84 #x2B
    123456789 => #xBA #xEF #x9A #x15

## Obsolete from here down

This will become a modified BER encoder plus a schema-based C struct converter.

## Input and output

The default value of *port* is the current input or output port, as appropriate.
It is an error if the ports passed to these procedures are not binary ports.
Procedure names  with `@` are used as an abbreviation for different numeric
types: see [SRFI 160](http://srfi.schemers.org/srfi-160/srfi-160.html) for
the full list.

`(read-@ ` [*port*]`)`

Reads the appropriate number of bytes from *port* in native byte order
and returns a number of the appropriate type
(exact integer or inexact real or complex number).

`(read-@le ` [*port*]`)`

Reads the appropriate number of bytes from *port* in little-endian byte order
and returns a number of the appropriate type (exact integer or inexact real or complex number).

`(read-@be ` [*port*]`)`

Reads the appropriate number of bytes from *port* in big-endian ("network") byte order
and returns a number of the appropriate type (exact integer or inexact real or complex number).

`(write-@ `*number* [*port*]`)`

Writes *number* to *port* in the appropriate format using native byte order.

`(write-@le `*number* [*port*]`)`

Writes *number* to *port* in the appropriate format using little-endian byte order.

`(write-@be `*number* [*port*]`)`

Writes *number* to *port* in the appropriate format using big-endian ("network") byte order.

`(read-ber-integer ` [*port*]`)`

Reads a BER-encoded integer of arbitrary size
from *port* and returns it as an exact integer.

`(write-ber-integer ` *exact-integer* [*port*]`)`

Writes *exact-integer* using BER encoding to *port*.
It is an error if *exact-integer* is negative.

`ber-integer-size` _int_

Return the number of bytes required to encode _int_ in BER format.

`bytevector-ber-integer-ref` _bytevector k_

Reads and returns an exact integer starting at offset _k_ in _bytevector_.

`bytevector-ber-integer-set!` _bytevector k exact-integer_

Writes the exact integer _exact-integer_ to _bytevector_ in BER format starting at offset _k_
It is an error if _exact-integer_ is negative.

`(read-utf8-string ` *k* [*port*]`)`

Read *k* bytes from *port*, interpreting them as a sequence of characters encoded in UTF-8,
and return the sequence as a string.  It is an error if the implementation
forbids any of the characters in strings, except that if the implementation
forbids null characters in strings and the final character is a null,
then it is discarded.

`(write-utf8-string ` *string* [*port*]`)`
Convert the characters of *string* to UTF-8
and write the bytes to *port*.

`(read-bytevector-until `*byte* [*port*]`)`

Read bytes from *port* until a byte equal to *byte* is read. 
Return two values: all the other bytes as a bytevector,
and *byte*.



## Implementation

See [SRFI 56](http://srfi.schemers.org/srfi-56/srfi-56.html) for implementation.



