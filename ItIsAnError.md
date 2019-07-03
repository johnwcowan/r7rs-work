## It is an error

Here's a list of places where R7RS-small says something "is an error".
There are many other cases, of course, notably domain errors in procedure calls.

The names in brackets represent a rough-and-ready classification of these errors, and don't constitute a proposal.
The list of categories is [domain], [mismatch], [syntax], [read], [immutable], [undefined], [non-continuable].

1.3.2 procedure domain errors [domain]

1.3.2 too many or too few arguments [mismatch]

1.3.3. reuse of auxiliary syntax [syntax]

1.3.3. violation of start-end constraints [domain]

2.4 non-numeric datum labels [read]

2.4 forward references in datum labels [read]

2.4 recursion in datum labels [read]

2.4 circular code [syntax]

3.4 violations of immutability [immutable]

4.1.1 references to undefined variables [undefined]

4.1.3 `()` as code [syntax]

4.1.4. repeated variables in `lambda` lists [syntax]

4.1.6 `set!` of unbound variable [undefined]

4.2.1 `=>` in `cond` or `case` doesn't accept one argument [domain]

4.2.1 duplicated `case` keys [syntax]

4.2.2 repeated variables in `let`, `letrec`, `let-values`, `let-values*` lists or equivalent internal variable definitions [syntax]

4.2.2 direct recursion in `letrec` or `letrec*` or equivalent internal variable definitions [syntax]

4.2.2 mismatch between `let-values` formals and returned values [mismatch]

4.2.4 repeated variables in `do` [syntax]

4.2.6 non-parameter in `parameterize` [domain]

4.2.8 `unquote-splicing` of a non-list [domain]

4.2.9 uncaught case in `case-lambda` [mismatch]

4.3.1 repeated keywords in `let-syntax` [syntax]

4.3.2 non-identifiers in `syntax-rules` [syntax]

4.3.2 repeated pattern variable in `syntax-rules` [syntax]

4.3.2 unhandled syntax in `syntax-rules` [syntax]

4.3.2 unreconstructible `syntax-rules` output [syntax]

5.2 unknown identifiers in `only`, `except`, `rename` import specs [syntax]

5.2 importing an identifier more than once with different bindings [syntax]

5.2 redefining or mutating imported bindings [syntax]

5.2 referring to unimported identifiers [syntax]

5.3.3 duplicate variables in `define-values` [syntax]

5.4 use of a syntax keyword before its definition [syntax]

5.4 hopelessly muddled or ambiguous definitions [syntax]

5.5 duplicate field names in records [syntax]

5.5 undefined field name in record constructor [syntax]

5.5 invoke record accessor or mutator on record of wrong type [domain]

6.2.3 use anything but an exact integer as a container index [domain]

6.2.6 division by exact 0 [domain]

6.2.7 unusual conversion radix [domain]

6.2.7 impossible conversion from number to string [domain]

6.4 `car` or `cdr` of non-list [domain]

6.4 `list-ref`, `list-set!`, `list-tail` invalid index [domain]

6.4 alist is not an alist [domain]

6.4 attempt to copy a circular list [domain]

6.5 mutating symbol names [immutable]

6.7 violate forbidden-character restrictions [domain]

6.7 `string-ref`, `string-set!` invalid index [domain]

6.7 attempt to convert non-character to string or to fill string with non-character [domain]

6.7 violate `string-copy!` restrictions [domain]

6.8 `vector-ref`, `vector-set!` invalid index [domain]

6.8 violate `vector-copy!` restrictions [domain]

6.9 `bytevector-u8-ref`, `byte-vector-u8-set!` invalid index [domain]

6.9 violate `bytevector-copy!` restrictions [domain]

6.9 invalid UTF-8 byte sequences in `utf8->string` [domain]

6.10 mapping procedures must accept as many containers as are available [domain]

6.10 lists being mapped must not all be circular [domain]

6.10 mapping procedures must not mutate inputs [immutable]

6.10 `call/cc` must accept one argument [domain]

6.10 `with-exception-handler` handler and thunk arguments must accept a condition object and no arguments respectively [domain]

6.11 attempt to continue from a non-continuable exception [non-continuable]

6.13.1 procedure passed to a call procedure must accept one argument [domain]

6.13.1 trying to get a string or bytevector out of a port that is not a string or bytevector port [domain]

6.13.2 attempt operations on a closed port [closed]

6.14 file names must be strings [file]

6.14 attempt to mutate command-line strings [immutable]

6.14 indecipherable environment variable [domain]

6.14 attempt to mutate environment variables or values or features [immutable]

7.1 identifier starting with a combining character [read]

## Implementation restrictions

And here's a list of places where R7RS-small speaks of implementation restrictions:

1.3.2 out of memory

1.3.2 overly large exact number

6.2.2 incomplete numeric tower

6.2.2 cannot produce exact result of arithmetic operation

6.2.6 `max` or `min` needs to report an inexact number but cannot

6.2.6 inexact or exact conversion can't represent result




