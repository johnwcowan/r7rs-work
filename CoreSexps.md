Syntax of core S-expressions (everyone must support these):

  * Lists are enclosed in parentheses, must be proper, may nest indefinitely deep.
  
  * Integers match `/[+-]\d+/`.  Integers outside the 64-bit range may not interoperate.
  
  * Floats match `/[+-]?\d+\.\d+([Ee][+-]?\d/`.  Floats outside the IEEE binary64 range may not interoperate.
  
  * Strings are enclosed in double quotes and can contain the full Unicode repertoire, 
    The only escapes recognized are `\\` and `\"`.
    
  * Symbols: `/[$a-z][a-zA-Z0-9_-]*`.
    Symbols that distinguish between upper and lower case or between `-` and `_` may not interoperate.
    By convention, symbols beginning with `$` are meta-symbols and have special purposes.
    
  * There should be a representation of null (not the same as the empty list or false),
    but there is no agreement on what it is.
    
  * There should be a representation for booleans,
    but there is no agreement on what they are.
  
  * Whitespace outside strings is ignored completely.
    Whitespace by itself is not a valid S-expression.
  
  * `;` except in strings introduces a comment
    that goes up to but not including the end of line and is discarded.
    A comment by itself is not a valid S-expression.
    
Sources: Scheme `read`, Common Lisp `read` with default readtable,
Python [sexpdata library](https://sexpdata.readthedocs.io/en/latest/),
[Wikipedia s.v. "S-expression"](https://en.wikipedia.org/wiki/S-expression).

Equivalent binary format: [CoreAsn1](CoreAsn1.md).