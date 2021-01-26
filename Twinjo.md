## Abstract

This SRFI specifies Twinjo, a general and extensible method of
serializing Scheme data in a way that other languages
can straightforwardly handle.  Twinjo provides two formats:
Twinjo Text, a variant of Lisp S-expressions, and
Twinjo Binary, a subset of ASN.1 Basic Encoding Rules.
It makes no use of ASN.1 schemas.

It also arranges for there to be just one encoding
for each datum represented, although
the Twinjo Text rules don't quite correspond to any Lisp syntax,
and the Twinjo Binary rules don't conform to either of the usual subsets,
ASN.1 Canonical Encoding Rules (CER)
or ASN.1 Distinguished Encoding Rules (DER).
Twinjo provides effectively unlimited extensibility
and attempts to maintain a balance between ease and efficiency
for both reading and writing.

## Issues

Should bytevectors in Twinjo Text use hexdigits (easier to comprehend), base64 (shorter),
or either format at the writer's discretion (marked how?).

## Basic Text syntax

  * Integers: optional sign followed by sequence of digits
  
  * Floats: optional sign followed by sequence of digits with optional decimal point
    followed by optional exponent (`E` followed by sign followed by digits).
    Either the decimal point or the exponent can be omitted but not both.
    
  * Symbols: lower-case ASCII letter
    optionally followed by sequence of lower-case ASCII letters and ASCII digits.
    Alternatively, any characters surrounded by vertical bars.  The only escapes are `\\` and `\|`

  * Strings:  Enclosed in double quotes.  The only escapes are `\"` and `\\`.

  * Bytevectors:  Enclosed in curly braces.  Hex digits, with an optional hyphen
    between consecutive digits.  This is related to UUID syntax.
    Rationale: so that bytevectors can be prefixed with a tag without needing
    to support nested tags.

  * Lists: Enclosed in parentheses.

  * Tags: Used to extend syntax.
    Consists of `#` followed by:
      * nothing (datum follows)
      * `X` followed by type number in upper-case hex (datum follows)
      * a single lower-case ASCII letter (no datum follows)
      * ASCII lower-case letter followed by
        zero or more lower-case ASCII letters or digits (datum follows)

## Whitespace and comments

Whitespace outside strings is ignored completely,
except for separating
from adjacent tokens when ambiguity would result.
For example, `#f 32 (1.0 2.0)` is not the same as
`#f32 (1.0 2.0)`.
Whitespace by itself is not a valid S-expression.
  
`;` (except in strings and symbols) introduces a comment
that goes up to but not including the end of line and is discarded.
A comment by itself is not a valid S-expression.

## Binary syntax

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
   * If length is less than 2^23 bytes, meta-length byte is `83`, followed by 3 length bytes
    representing the length as a big-endian 2's-complement integer.
  * ...
  * If length is less than 2^63 bytes, meta-length byte is `88`, followed by 8 length bytes
    representing the length as a big-endian 2's-complement integer.
  * Larger objects are not representable.
  
## Binary examples

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

## Skipping unknown binary types

  * If first type byte is `1F`, `3F`, `5F`, `7F`, `9F`, `BF`, `DF`, or `FF`,
    skip one additional type byte.
  * Read and interpret length bytes.
  * If length byte is not `80`, skip number of bytes equal to the length.
  * If length byte is `80`, skip subobjects until the EOC marker has been read.
  
## Specific type representations

All currently proposed formats (Google Spreadsheet):
[Twinjo data type serializations](https://tinyurl.com/asn1-ler).

Note:  If binary interoperability with other ASN.1 systems is important, encode only
the types marked "X.690" in the Origin column of the spreadsheet.

## Procedures

`(twinjo-read-text `*proc* [*port*])  
`(twinjo-read-binary `*proc* [*port*])

Reads an Twinjo Text or Binary value from *port*, by default `(current-input-port)`.
If the external representation of an object is read whose type is unknown,
*proc* is called with three arguments:

 * a symbol representing a non-hex tag in Twinjo Text,
   or `#f` otherwiser
 * a number representing a hex tag in Twinjo Text
   or Twinjo Binary,
   or #f in Twinjo Text if the tag is non-hex.
 * a number, string, symbol, bytevector, list,
   or `#f` if the code/tag is stand-alone
   
The value returned by *proc* is inserted
into the result in place of the unknown representation.
Alternatively, *proc* can signal an error satisfying `twinjo-error?`.

`(twinjo-write-text `*proc* [*port*])  
`(twinjo-write-binary `*proc* [*port*])

Writes an Twinjo Text or Binary value to *port*,
by default to `(current-output-port)`.
If an object is to be written whose representation is unknown,
*proc* is called with the object, and is expected to return three values:

 * a symbol representing a non-hex tag
   or `#f` if there is none
 * a number representing a Twinjo Text hex tag
   or Twinjo Binary type code,
   or `#f` if there is none
 * a number, string, symbol, bytevector, list,
   or `#f` if the code/tag is stand-alone
   
The values returned by *proc* are used to format
the representation of the object.
Alternatively, *proc* can signal an error satisfying `twinjo-error?`.

`(twinjo-error `*message irritant ...*`)`

Constructs and raises an exception that satisfies `twinjo-error?`
and allows access to *message* (a string) and *irritants*.

`(twinjo-error? `*obj*`)`

Returns `#t` if *obj* is a condition object created by
`twinjo-error` or any other implementation-specified object.

`(twinjo-message *twinjo-error*`)

Returns the message associated with *twinjo-error*.

`(twinjo-irritants *twinjo-error*`)

Returns the list of irritants associated with *twinjo-error*.

`max-byte-object`  
`max-compound-object`  
`max-nesting-depth`

These parameters are set to constraint the sizes of
objects read by `twinjo-read-text` and `twinjo-read-binary`.
It is an error if their values are not exact non-negative integers.
If the length of a byte object exceeds `(max-byte-object)`,
or a compound object is found to have more than
`(max-compound-object)` subobjects,
or an object is nested more than `(max-nesting-depth)`,
an error satisfying `twinjo-error` is signaled.
The initial values of these parameters are implementation-specified.
Note that there is no value meaning "unlimited".
