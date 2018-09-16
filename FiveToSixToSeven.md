These are the language changes listed in R6RS Appendix E, with notes on the extent to which R7RS-small has taken them up.


* Scheme source code now uses the Unicode character set.  Specifically, the character set that can be used for identifiers has been greatly expanded.
    * Permitted but not required.
* Identifiers can now start with the characters `->`.
    * Adopted.
* Identifiers and symbol literals are now case-sensitive.
    * Adopted.
* Identifiers and representations of characters, booleans, number objects, and `.` must be explicitly delimited.
    * Adopted.
* `#` is now a delimiter.
    * Rejected for compatibility with existing Schemes.
* Bytevector literal syntax has been added.
    * Adopted in principle, but SRFI 4 lexical syntax is used.
* Matched square brackets can be used synonymously with parentheses.
    * Rejected on the grounds that implementations may wish to use brackets for better things.
* The read-syntax abbreviations `#'` (for `syntax`), `#`-backquote (for `quasisyntax`), `#`, (for `unsyntax`), and `#,@` (for `unsyntax-splicing`) have been added.
    * Irrelevant, because `syntax-case` is not part of R7RS-small.
* `#` can no longer be used in place of digits in number representations.
    * No longer required, but still permitted.
* The external representation of number objects can now include a mantissa width.
    * Rejected.
* Literals for NaNs and infinities were added.
    * Adopted.
* String and character literals can now use a variety of escape sequences.
    * Adopted.
* Block and datum comments have been added.
    * Adopted.
* The `#!r6rs` comment for marking report-compliant lexical syntax has been added.
    * Rejected; there is no R7RS analogue of it.
* Characters are now specified to correspond to Unicode scalar values.
    * Adopted in part: implementations are not required to handle all Unicode scalar values.
* Many of the procedures and syntactic forms of the language are now part of the `(rnrs base (6))` library.  Some procedures and syntactic forms have been moved to other libraries.
    * Adopted in principle, but the details differ.
* The base language has the following new procedures and syntactic forms: `letrec*`, `let-values`, `let*-values`, `real-valued?`, `rational-valued?`, `integer-valued?`, `exact`, `inexact`, `finite?`, `infinite?`, `nan?`, `div`, `mod`, `div-and-mod`, `div0`, `mod0`, `div0-and-mod0`, `exact-integer-sqrt`, `boolean=?`, `symbol=?`, `string-for-each`, `vector-map`, `vector-for-each`, `error`, `assertion-violation`, `assert`, `call/cc`, `identifier-syntax`.
    * All adopted except the `-valued` procedures, the division procedures (R7RS-small has different ones), assertions, and identifier syntax.
* The following procedures have been removed: `char-ready?`, `transcript-on`, `transcript-off`, `load`.
    * Only `transcript-on` and `transcript-off` are removed.
* The case-insensitive string comparisons (`string-ci=?`, `string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?`) operate on the case-folded versions of the strings rather than as the simple lexicographic ordering induced by the corresponding character comparison procedures.
    * Adopted.  However, the non-`ci` versions are implementation-dependent in R7RS-small.
* Libraries have been added to the language.
    * Adopted, but the details differ slightly.
* A number of standard libraries are described.
    * Adopted, but the details differ.
* Many situations that "were an error" now have defined or constrained behavior.  In particular, many are now specified in terms of the exception system.
    * Rejected for backward compatibility with existing Schemes.
* The full numerical tower is now required.
    * Rejected.
* The semantics for the transcendental functions has been specified more fully.
    * Adopted.
* The semantics of `expt` for zero bases has been refined.
    * Adopted.
* In `syntax-rules` forms, a `_` may be used in place of the keyword.
    * Adopted.
* The `let-syntax` and `letrec-syntax` no longer introduce a new environment for their bodies.
    * Rejected for backward compatibility with existing Schemes.
* For implementations that support [NaNs](NaNs.md) or infinities, many arithmetic operations have been specified on these values consistently with IEEE 754.
    * Adopted.
* For implementations that support a distinct -0.0, the semantics of many arithmetic operations with regard to -0.0 has been specified consistently with IEEE 754.
    * Adopted.
* Scheme's real number objects now have an exact zero as their imaginary part.
    * Rejected for backward compatibility with existing Schemes.
* The specification of `quasiquote` has been extended.  Nested quasiquotations work correctly now, and `unquote` and `unquote-splicing` have been extended to several operands.
    * Adopted in part.
* Procedures now may or may not refer to locations.  Consequently, `eqv?` is now unspecified in a few cases where it was specified before.
    * Mostly rejected, although `eq?` may now distinguish between procedures where `eqv?` does not.
* The mutability of the values of `quasiquote` structures has been specified to some degree.
    * Rejected.
* The dynamic environment of the *before* and *after* procedures of `dynamic-wind` is now specified.
    * Adopted.
* Various expressions that have only side effects are now allowed to return an arbitrary number of values.
    * Rejected for backward compatibility with existing Schemes.
* The order and semantics for macro expansion has been more fully specified.
    * Irrelevant.
* Internal definitions are now defined in terms of `letrec*`.
    * Adopted.
* The old notion of program structure and Scheme's top-level environment has been replaced by top-level programs and libraries.
    * Adopted in parellel with the old semantics.
* The denotational semantics has been replaced by an operational semantics.
    * The denotational semantics was updated rather than replaced.
