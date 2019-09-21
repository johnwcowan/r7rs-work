## Procedures

`(asn1-read `[*port*]`)` -> object

Read a single object from *port*, which is a byte input port, and return it.
The default port is the value of `(current-input-port)`.

`(asn1-write `*obj* [*port*]`)` -> unspecified

Write *obj* to *port*, which is a byte output port.
The default port is the value of `(current-output-port)`.

## Formats

General format of an object: 1 or 2 type bytes
followed by 1-9 length bytes
followed by the number of content bytes specified in the length.

Length bytes:

  * If length is less than 2^7 bytes, length byte `00` through `7F`.
  * If length is less than 2^15 bytes, meta-length byte `82` followed by 2 length bytes
    representing a big-endian 2's-complement integer.
  * If length is less than 2^31 bytes, meta-length byte `84` followed by 4 length bytes
    representing a big-endian 2's-complement integer.
  * If length is less than 2^63 bytes, meta-length byte `88` followed by 8 length bytes
    representing a big-endian 2's-complement integer.

Lists:  Type byte `E0`
followed by length bytes
followed by the encoded elements of the list.

Integers:  Type byte `02` followed by length byte `00`, `01`, `02`, `04`, or `08`
followed by 0, 1, 2, 4, or 8 content bytes
representing a big-endian 2's-complement integer.

Floats:  Type byte `DB` followed by length byte `08`
followed by 8 content bytes
representing a big-endian IEEE binary64 float.

Strings:  Type byte `OC` followed by length bytes
followed by content bytes
representing a UTF-8 encoding of the string's content.

Symbols:  Type byte `DD` followed by length bytes
followed by content bytes representing a UTF-8 encoding of the symbol's name.

Nulls:  Type byte `05` followed by length byte `00`
followed by nothing.

Booleans:  Type byte `01` followed by 1 length byte `01`
followed by 1 content byte which is `00` for false and `FF` for true.

Mappings / hash tables:  Type byte `E4`
followed by length bytes
followed by encoded objects, alternating between keys and values.

Skipping unknown types.

  * If type byte is `1F`, `3F`, `5F`, `7F`, `9F`, `BF` or `DF`, skip one additional type byte.
  * Read and interpret length bytes.
  * Skip number of bytes equal to the length.
  
Note:  If interoperability with other ASN.1 systems is important, use `30` (vector) instead of `E0` for lists,
and do not encode floats, symbols, or mappings.

Equivalent textual format: [Core S-expressions](CoreSexps.md).

