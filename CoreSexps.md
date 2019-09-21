Syntax of core S-expressions (everyone must support these) are given below.
Regular expressions are anchored at both ends, as if they began with `^` and ended with `$`.

  * Lists are enclosed in parentheses, must be proper, may nest indefinitely deep.
  
  * Integers match `/[+-]\d+/`.  Integers outside the 64-bit range may not interoperate.
  
  * Floats match `/[+-]?\d+\.\d+([Ee][+-]?\d/`.  Floats outside the IEEE binary64 range may not interoperate.
  
  * Strings are enclosed in double quotes and can contain the full Unicode repertoire, 
    The only escapes recognized are `\\` and `\"`.
    
  * Symbols: `/[$a-z][a-zA-Z0-9_-]*`.
    Symbols that distinguish between upper and lower case or between `-` and `_` may not interoperate.
    By convention, symbols beginning with `$` are meta-symbols and have special purposes.
    Neither /nil/ nor /null/ matches a symbol, because the first is used for the null object (see below)
    and the second has special properties in Common Lisp.
    
  * Null object (distinct Boolean false, and the empty list):  `/null/`.
    
  * Booleans: There is no agreement on a common representation,
    so this SRFI standardizes on `/#t/` and `/#f/`.
    These are native in Scheme, and aren't used for anything
    in Elisp or in the standard Common Lisp readtable
    (to which they can be easily added).

  * Mappings (including hash tables):
    There is no standard representation in any Lisp,
    so this SRFI standardizes on `#{` followed by
    alternating keys and values followed by `}`,
    under the influence of Python and JSON.
  
  * Whitespace outside strings is ignored completely,
    except for separating numbers and identifiers
    from adjacent numbers and identifiers.
    Commas are considered whitespace.
    Whitespace by itself is not a valid S-expression.
  
  * `;` except in strings introduces a comment
    that goes up to but not including the end of line and is discarded.
    A comment by itself is not a valid S-expression.
    
Sources: Scheme `read`, Common Lisp `read` with default readtable,
Python [sexpdata library](https://sexpdata.readthedocs.io/en/latest/),
[Wikipedia s.v. "S-expression"](https://en.wikipedia.org/wiki/S-expression),
Python syntax, JSON syntax.

Equivalent binary format: [CoreAsn1](CoreAsn1.md).