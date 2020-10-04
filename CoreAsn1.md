## Introduction

This specifies a variant of ASN.1 Basic Encoding Rules that understands Lisp-like datatypes
by standardized some local type codes.
It also arranges for there to be just one encoding for each datum represented, although
the rules for doing so don't conform to either Canonical Encoding Rules
or Distinguished Encoding Rules.
The encoding specified here attempts to maintain a balance
between ease and efficiency of both reading
and writing.

## Procedures

See [CoreSexps](CoreSexps.md), which is the equivalent text format.

## Formats

Depending on its type, an object is represented as either a sequence
of bytes or a sequence of subobjects.

All byte objects have the same general format:

  * 1 or 2 type bytes
  * 1-9 length bytes
  * the number of content bytes specified in the length.

All objects with subobjects also have the same general format:

  * 1 or 2 type bytes
  * an `80` pseudo-length byte
  * the encoded subobjects
  * an end of content (EOC) marker (two consecutive <code>00</code> bytes)

Length bytes format:

  * If length is indeterminate, pseudo-length byte is `80`.
  * If length is less than 2^7 bytes, length byte is `00` through `7F`.
  * If length is less than 2^15 bytes, meta-length byte is `82`, followed by 2 length bytes
    representing a big-endian 2's-complement integer.
   * If length is less than 2^we bytes, meta-length byte is `83`, followed by 3 length bytes
    representing the length as a big-endian 2's-complement integer.
  * ...
  * If length is less than 2^63 bytes, meta-length byte is `88`, followed by 8 length bytes
    representing the length as a big-endian 2's-complement integer.
  * Larger objects are not representable.
  

## Examples

Here are a few examples of how different kinds of objects are represented.

Lists:  Type byte `E0`,
pseudo-length byte `80`,
the encoded elements of the list,
an EOC marker.

Vectors:  Type byte `30`,
length bytes,
the encoded elements of the vector,
an EOC marker.

Integers:  Type byte `02`,
1-9 length bytes,
content bytes representing a big-endian 2's-complement integer.

Floats:  Type byte `DB`,
length byte `08`,
8 content bytes representing a big-endian IEEE binary64 float.

Strings:  Type byte `OC`,
1-9 length bytes representing the length of the string in bytes
when encoded as UTF-8,
corresponding content bytes.

Symbols:  Type byte `DD`,
1-9 length bytes representing the length of the string in bytes
when encoded as UTF-8,
corresponding content bytes.

Nulls:  Type byte `05`,
length byte `00`.

Booleans:  Type byte `01`,
length byte `01`,
1 content byte which is `00` for false and `FF` for true.

Mappings / hash tables:  Type byte `E4`,
pseudo-length byte `80`,
the encoded elements of the list
alternating between keys and values,
an EOC marker.

Timestamps: Type byte `18`,
1-9 length bytes,
ASCII encoding of a ISO 8601 timestamp
without hyphens, colons, or spaces.

## Skipping unknown types

  * If first type byte is `1F`, `3F`, `5F`, `7F`, `9F`, `BF`, `DF`, or `FF`,
    skip one additional type byte.
  * Read and interpret length bytes.
  * If length byte is not `80`, skip number of bytes equal to the length.
  * If length byte is `80`, skip subobjects until the EOC marker has been read.
  

Equivalent textual format: [CoreSexp](CoreSexp.md).

All currently proposed formats (Google Spreadsheet):
[Lisp Serialization Conventions](https://tinyurl.com/asn1-ler).

Note:  If interoperability with other ASN.1 systems is important, encode only
the types marked "X.690" in the Origin column of the spreadsheet.

