## Procedures

(read-textual [port])  
(read-binary [port])  
(write-textual obj [port])  
(write-binary obj [port])

read-length-limit (parameter)

Limit on the length of list, string, and bytevector objects read.
An error is signaled if the limit is violated.

read-depth-limit (parameter)

Limit on the depth of list structures read.

read-conversion (parameter)

Procedure that accepts a type code (integer or string) and a value
(number, string, bytevector, or list); returns the appropriate Scheme object, or #f if none
(in which case read fails).

write-conversion (parameter)

Procedure that accepts a Scheme object and returns two values, a type
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

Table of formats: [Lisp Serialization Conventions](https://docs.google.com/spreadsheets/d/1V-7E5d3fLON5DrVeHkVvp9h5SRgcteOgnPl8KvWTA3M).
