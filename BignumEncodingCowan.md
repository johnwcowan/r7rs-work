##Bignum Encodings

Since Schemes may support unlimited size bignums it is useful to support the binary encoding of such values.

A BER (Basic Encoding Rules from X.690) compressed integer is an unsigned integer in base 128, most significant digit first, where the high bit is set on all but the final (least significant) byte. Thus any size integer can be encoded, but the encoding is efficient and small integers don't take up any more space than they would in normal char/short/int encodings. This is commonly used to encode an unlimited length field, and can form the basis for other variable length encodings.

Examples of integers converted to BER byte sequences:

            3 => #x03
          555 => #x84 #x2B
    123456789 => #xBA #xEF #x9A #x15

`ber-integer-size` _int_

Return the number of bytes required to encode _k_ in BER format.

`bytevector-ber-integer-ref` _bytevector k_

Reads and returns an exact integer starting at offset _k_ in _bytevector_.

`bytevector-ber-integer-set!` _bytevector k int_

Writes _int_ to _bytevector_ in BER format starting at offset _k_. It is an error if _int_ is not a positive integer.

