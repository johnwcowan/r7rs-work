Syntax of core S-expressions (everyone must support these) are given below.
Regular expressions are anchored at both ends, as if they began with `^` and ended with `$`.

Issue: should there be a format for fixed-point exact integers such as `#e`
followed by digits and a decimal point?

Issue: Should we use hex format for bytevectors, like #u8[00-12-34-56-78] (hyphens optional),
instead of #u8(0 18 52 86 120)?  My inclination is to say no.

  * Lists are enclosed in parentheses, must be proper, may nest indefinitely deep.
  
  * Vectors are prefixed by `#`, enclosed in parentheses, may nest indefinitely deep.
  
  * Integers match `/[+-]\d+/`.
  
  * Floats match `/[+-]?\d+\.\d+([Ee][+-]?\d/`.
  
  * Strings are enclosed in double quotes and can contain the full Unicode repertoire, 
    The only escapes recognized are `\\` and `\"`.
    
  * Symbols either match `/[$a-z][a-zA-Z0-9_-]*` or are enclosed in vertical bars
    and can contain the full Unicode repertoire.
    The regex `/nil/` does not match a symbol because of its special properties in Common Lisp.
    
  * Null object (distinct from Boolean false, and the empty list):  `/#null()/`.
    
  * Booleans: There is no agreement on a common representation,
    so this SRFI standardizes on `/#t/` and `/#f/`.
    These are native in Scheme, and aren't used for anything
    in Elisp or in the standard Common Lisp readtable
    (to which they can be easily added).

  * Mappings (including hash tables):
    There is no standard representation in any Lisp,
    so this SRFI standardizes on `#date{` followed by
    alternating keys and values followed by `}`,
    under the influence of Python and JSON.
    
  * Bytevectors: There is no agreement on a common representation,
    so this SRFI standardizes on `#u8(` followed by numbers in the range 0-255
    followed by `)`.
    
  * Dates: There is no agreement on a common representation,
    so this SRFI standardizes on `#date"` followed by
    an 8601 date without hyphens, colons, or spaces
    followed by `"`.
  
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

Equivalent binary format: [CoreAsn1](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/CoreAsn1).