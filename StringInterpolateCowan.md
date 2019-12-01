This is a sketch of a function `string-interpolate`.
It accepts a string, the *base-string*,
and a mapping from strings to Scheme datums
(objects that have an external representation).
It provides a useful subset of the functionality of
[SRFI 109](http://srfi.schemers.org/srfi-109/srfi-109.html)
without requiring changes to Scheme's lexical syntax.

Everything except an `&` (ampersand) character is copied
directly from *base-string* to the output of `string-interpolate`.
The following sequences beginning with `&` are understood and handled specially:

  * `&&` is replaced by a single `&` in the output.
  
  * `&[...]` looks up the string between the square brackets
    in the mapping and replaces it with its value converted to
    a string as if by `display`.  Thus if the *base-string* is
    `"value: &[foo]"` and `"foo"` is mapped to `32`, then the output
    is `"value: 32"`.  Interpolation by name rather than position
    allows the *base-string* to be translated into a different language
    with a different word order.
    
  * `&;` suppresses all following characters up to but not including the next
    newline character, or the end of *base-string* if there is no
    such newline.  This removes an embedded single-line comment, and allows a
    visual signal of the end of a line when the line has trailing spaces.
    
  * `&|` suppresses all preceding characters up to but not including the previous
    newline character, or the beginning of *base-string* if there is no
    such newline.  This allows removing a purely visual indentation at the beginning of a line
    that is not meant to be part of the output.
    
  * `&#|...|#` is an embedded comment and is suppressed entirely.  Nested `#|...|#`
    sequences in the comment are handled correctly.
  
An `&` followed by any other character is an error.
  
  