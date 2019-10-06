## Procedures

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
Accepts a type code (integer or string) and a value
(number, string, bytevector, or list); returns the appropriate Scheme object, or #f if none
(in which case read fails).

`write-conversion` (parameter)

Procedure to be called when an object of unknown Scheme type is to be written.
Accepts a Scheme object and returns two values, a type
code (as integer) and a number, string, bytevector, or list to serialize.
Returns `#f #f` if no known serialization (in which case write fails).

## Basic syntax

  * Integers: optional sign followed by sequence of digits
  
  * Decimals: optional sign followed by digits followed by `.` followed by digits
  
  * Floats: decimal followed by optional exponent
    (`E` followed by sign followed by digits)

  * Strings:  Enclosed in double quotes.  The only escapes are `\"` and `\\`.

  * Bytevectors:  Enclosed in curly braces.  Hex digits, with an optional hyphen
    between every two digits.  This is related to UUID syntax.

  * Lists: Enclosed in parentheses.

  * Tags: Used to extend syntax when followed by one of the other basic syntaxes.  Formats:
      * `#` by itself
      * `#` followed by type number in hex
      * `#` followed by a tag identifier (lower case ASCII letters and digits, begins with a letter)

## Whitespace and comments

Whitespace outside strings is ignored completely,
except for separating numbers and identifiers
from adjacent numbers and identifiers.
Commas are considered whitespace.
Whitespace by itself is not a valid S-expression.
  
`;` except in strings introduces a comment
that goes up to but not including the end of line and is discarded.
A comment by itself is not a valid S-expression.


Equivalent binary format: [CoreAsn1](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/CoreAsn1).

All currently proposed formats: [Lisp Serialization Conventions](https://tinyurl.com/asn1-ler).