## Procedures

`(make-core-object `*code tag value*`)`

Returns a Core object, where *code* is an exact integer representing a code,
*tag* is a symbol representing a tag, and *value* is a
number, string, bytevector, or list.  These are used to represent
objects whose code/tag is not understood by the implementation.

`(core-object? `*obj*`)`

`(core-object-code `*core-obj*`)`  
`(core-object-tag `*core-obj*`)`  
`(core-object-value `*core-obj*`)`

Accessors for Core objects.  If the code or tag is unknown, return `#f`.

`(read-textual `[*port*])  
`(read-binary `[*port*])  
`(write-textual `obj [*port*])  
`(write-binary `obj [*port*])

`read-length-limit` (parameter)

Limit on the length of list, string, bytevector, and binary objects read.
An error is signaled if the limit is violated.

`read-depth-limit` (parameter)

Limit on the depth of structures read.
An error is signaled if the limit is violated.

`read-conversion` (parameter)

Procedure to be called when an object with unknown tag or type code is read.
Accepts a Core object and returns the appropriate Scheme object, or `#f` if none
(in which case the read operation fails).

`write-conversion` (parameter)

Procedure to be called when an object of unknown Scheme type is to be written.
Accepts a Scheme object and returns a Core object with an integer code or string
tag and a number, string, bytevector, or list to serialize.
Returns `#f` if no known serialization (in which case the write operation fails).

## Basic syntax

  * Integers: optional sign followed by sequence of digits
  
  * Decimals: optional sign followed by digits followed by `.` followed by digits
  
  * Floats: decimal followed by optional exponent
    (`E` followed by sign followed by digits)

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


Equivalent binary format: [CoreAsn1](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/CoreAsn1.md).

All currently proposed formats: [Lisp Serialization Conventions](https://tinyurl.com/asn1-ler).