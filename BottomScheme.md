**This is not an R7RS page; it's just John Cowan's notes on an idea that used to live on a scrap of paper.**

Bottom Scheme is a tiny subset of R7RS-small.
It is not really a Scheme at all, because it omits
assignment, macros, modules, proper tail calls except self-calls and named `let`,
multiple values, `call/cc`, `dynamic-wind`, mutable pairs and strings,
I/O (except for `read-char` and `write-char`),
and essentially all non-primitive procedures.
What it is, is the stupidest thing you could possibly call Scheme.
Even a *really stupid* compiler could translate it into C
(or GW Basic, for that matter).

## Specification

This list is organized according to the R7RS-small report.

2.1 Identifiers

No case-folding directives.

2.2 Whitespace

Only `;` comments.

2.3 Other notations

Parens, apostrophe, quoted strings, `#t` and `#f`, and number, vector, and bytevector constants.  No backquoting, character constants, number bases, or labels.

4.1 Primitive expression types

Variables, constants, procedure calls, `lambda` without improper formals, `if`.
Procedures are only self-tail-recursive.

4.2 Derived expression types

Only `cond` without `=>`, `and`, `or`, `let`, `letrec`, `begin`, and named `let` (with the restriction that the bound procedure can only be invoked in tail positions within the lexical scope of the `let`).

5.3 Variable definitions

Only `define`, either at top level or internally.

5.5 Record-type definitions

SRFI 137, but returning a list of five procedures rather than returning five values.

6.1 Equivalence predicates

Only `eqv?` (for which `eq?` is a synonym).
Programmers are encouraged to provide their own definition of `equal?`.

6.2.1 Numerical types

All types are supported.

6.2.2 Exactness

The only exact numbers are integers within a fixed range.

6.2.4 Implementation extensions

Inexact real numbers are IEEE doubles;
inexact complex numbers are pairs of IEEE doubles in the rectangular representation.

6.2.5 Syntax of numerical constants

`/` is not supported, because all exact numbers are integers.
Base and exactness prefixes are not supported.  The notations `-0.0`, `+inf.0`, `-inf.0`, and `+nan.0` are supported.

6.2.6 Numerical operations

Predicates:  `number?`, `real?`, `exact?`, `inexact?`.

Arithmetic:  `+`, `-`, `*`, `/` with two arguments only; `/` always returns an inexact value.

Transcendental functions:  `exp`, `log` (one argument),
`sin`, `cos`, `tan`, `asin`, `acos`, `atan` (one argument),
`sqrt`, `expt` always return complex numbers.

Complex: `make-rectangular`, `real-part`, `imag-part`.  Conversion: `exact`, `inexact`.

As an enhancement to R7RS-small, the non-generic arithmetic functions
`fx+`, `fx-`, `fx*`, `fl+`, `fl-`, `fl*`, `fl/`, `cx+`, `cx-`, `cx*`, `cx/` are provided.

6.3 Booleans

Only `#t` and `#f` notations, `not`, `boolean?`.

6.4 Pairs and lists

Pairs are immutable.  Only `pair?`, `cons`, `car`, `cdr`, `null?`'
The empty list is provided.

6.5 Symbols

Only `symbol?`, `symbol->string`, `string->symbol`.

6.6 Characters

Not supported; use single-character strings instead.

6.7 Strings

Full support for string literals.  Strings are immutable.
All Unicode characters are supported except U+0000 (NUL).
Only `string?`, `string-length`, `string=?`, `string<?`, `string>?`, `substring`, `list->string`.

6.8 Vectors

Full support for vector literals.
Only `vector?`, `make-vector` (one argument), `vector-length`, `vector-ref`, `vector-set!`.

6.9 Bytevectors

Full support for bytevector literals.
Only `bytevector?`, `make-bytevector` (one argument), `bytevector-length`,
`bytevector-u8-ref`, `bytevector-u8-set!`.

6.10 Control features

Only `procedure?`, `apply`.

6.11 Exceptions

Only `error`.

6.13 Input and output

Only `read-char` (no arguments), `eof-object`, `eof-object?`, `write-char` (one argument),
`display` (mostly for debugging).

## Implementation

These notes assume a 64-bit system.

With the basic object 64 bits in size, NaN-boxing is a plausible technique.
In this scheme, IEEE doubles are represented as immediates,
and all other objects are stuffed
into the NaN space by setting the top 12 bits to 1.
This limits them to 52 bits in size,
which is enough to hold 64-bit pointers in current architectures,
since they are only 47 bits in size (excluding the kernel area).
Because a pointer to a 64-bit value always has the low-order three bits zero,
they can be used for the following tagging scheme:

* 000 - 48-bit fixnum
* 001 - pointer to compnum
* 010 - immediate `#t`, `#f`, empty list, end of file object, and undefined-value pseudo-object
* 011 - pointer to vector (the -1 element is a 48-bit fixnum length)
* 100 - pointer to Scheme pair (direct dereference gets the cdr)
* 101 - pointer to bytevector (padded to multiple of 64 bits, preceded by a 48-bit fixnum length)
* 110 - pointer to string (padded to multiple of 64 bits, preceded by a 48-bit fixnum length)
* 111 - pointer to procedure, symbol, or record (first word points to a type object)

In order to make pointers more efficient,
we can flip the top 12 bits before storing them.
That way all pointers and fixnums will Just Work,
and doubles will simply need to be flipped back.

