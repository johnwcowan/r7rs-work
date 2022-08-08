This is a sketch of a function `string-interpolate`:

`(string-interpolate `*base-string dto dictionary* [*escape-proc*]`)`

It accepts a string, the *base-string*,
and a *dictionary* that maps from strings to Scheme datums
(objects that have an external representation).
It provides a useful subset of the functionality of
[SRFI 109](https://srfi.schemers.org/srfi-109/srfi-109.html)
without requiring changes to Scheme's lexical syntax.

Everything up to an `$` character is copied
directly from *base-string* to the output of `string-interpolate`.
The following sequences beginning with `$` are understood and handled specially:

  * `$$` is replaced by a single `$` in the output.
  
  * `$[...]` converts the string between the square brackets to a symbol,
    looks it up in the mapping, passes the result through the *escape-procedure* (whose
    default is `values`), converts it to a string as if by `display`,
    and includes in the output of `string-interpolate`.
        
  * `$;` suppresses all following characters up to but not including the next
    newline character, or the end of *base-string* if there is no
    such newline.  This removes an embedded single-line comment, and allows a
    visual signal of the end of a line when the line has trailing spaces.
    
  * `$|` suppresses all preceding space characters up to but not including the previous
    newline character, or the beginning of *base-string* if there is no
    such newline.  This allows removing a purely visual indentation
    that is not meant to be part of the output.
    
  * `$#|...|#` is an embedded comment and is suppressed entirely.  Nested `#|...|#`
    sequences in the comment are handled correctly.
  
An `$` followed by any other character is an error.
  
Interpolation by name rather than position
is more robust and
allows the *base-string* to be translated into a different language
with a different word order.  Escaping protects against injection of
unsanitized input.

Suppose that the procedure `html-escape` translates any `&` in its input to `&amp;`
and any `<` into `&lt;`, leaving all other characters unchanged.
Then the result of `(string-interpolate "Value: $[v]" equal-alist-dto '((v . 32)))`
is `Value: 32`, and the result of `(string-interpolate "<p>$[inequality]</p>"
equal-alist-dto ((inequality . "3 < 4")) html-escape)` is `<p>3 &lt; 4</p>`.
'
