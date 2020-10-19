## Introduction

This specifies a variant of both S-expressions and
ASN.1 Basic Encoding Rules that express Lisp-like datatypes.
It also arranges for there to be just one encoding for each datum represented, although
the textual rules don't quite correspond to any Lisp syntax,
and the binary rules don't conform to either ASN.1 Canonical Encoding Rules
or ASN.1 Distinguished Encoding Rules.
The encodings specified here attempt to provide extensibility
and maintain a balance between ease and efficiency
of both reading and writing.

## Issues

Should textual bytevectors use hexdigits (easier to read), base64 (shorter),
or either format at the writer's discretion (marked how?).

## Procedures

`(core-read-textual `*proc* [*port*])  
`(core-read-binary `*proc* [*port*])

Reads a Core textual or binary value from *port*, by default `(current-input-port)`.
If the external representation of an object is read whose type is unknown,
*proc* is called with three arguments:

 * a symbol representing a tag
   or `#f if there is none
 * a number representing a corresponding number,
   or `#f if there is none
 * a number, string, symbol, bytevector, list,
   or `#f` if the code/tag is stand-alone
   
The value returned by *proc* is substituted
 in the result for the unknown representation.

`(core-write-textual `*proc* [*port*])  
`(core-write-binary `*proc* [*port*])

Writes a Core textual or binary value to *port*, by default `(current-output-port)`.
If an object is to be written whose representation is unknown,
*proc* is called with the object, and returns three values:

 * a symbol representing a tag
   or `#f if there is none
 * a number representing a corresponding number,
   or `#f if there is none
 * a number, string, symbol, bytevector, list,
   or `#f` if the code/tag is stand-alone
   
The values returned by *proc* are used to format
the representation of the object.

## Basic textual syntax

  * Integers: optional sign followed by sequence of digits
  
  * Floats: optional sign followed by sequence of digits with optional decimal point
    followed by optional exponent (`E` followed by sign followed by digits).
    Either the decimal point or the exponent can be omitted but not both.
    
  * Symbols: lower-case ASCII letter
    optionally followed by sequence of lower-case ASCII letters and ASCII digits.
    Alternatively, any characters surrounded by vertical bars.  The only escapes are `\\` and `\|`

  * Strings:  Enclosed in double quotes.  The only escapes are `\"` and `\\`.

  * Bytevectors:  Enclosed in curly braces.  Hex digits, with an optional hyphen
    between every two digits.  This is related to UUID syntax.

  * Lists: Enclosed in parentheses.

  * Tags: Used to extend syntax.  All tags begin with `#` followed by:
      * nothing (vector)
      * type number in hex (data follows)
      * a single letter (no data follows)
      * tag identifier (lower case ASCII letters and digits,
        begins with a letter; data follows)

## Whitespace and comments

Whitespace outside strings is ignored completely,
except for separating numbers and identifiers
from adjacent numbers and identifiers.
Commas are considered whitespace.
Whitespace by itself is not a valid S-expression.
  
`;` (except in strings) introduces a comment
that goes up to but not including the end of line and is discarded.
A comment by itself is not a valid S-expression.

## ASN.1 formats

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
  
## ASN.1 examples

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
1 length byte,
ASCII encoding of a ISO 8601 timestamp
without hyphens, colons, or spaces.

## Skipping unknown ASN.1 types

  * If first type byte is `1F`, `3F`, `5F`, `7F`, `9F`, `BF`, `DF`, or `FF`,
    skip one additional type byte.
  * Read and interpret length bytes.
  * If length byte is not `80`, skip number of bytes equal to the length.
  * If length byte is `80`, skip subobjects until the EOC marker has been read.
  
## Detailed formats

All currently proposed formats (Google Spreadsheet):
[Lisp Serialization Conventions](https://tinyurl.com/asn1-ler).

Note:  If interoperability with other ASN.1 systems is important, encode only
the types marked "X.690" in the Origin column of the spreadsheet.
