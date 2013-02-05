These are the editorial corrections to the seventh draft in the order they were made: 

 * Added `#` to vector literals in certain examples
 * Added cross-reference for import declarations
 * Fixed example and added new example for `digit-value`
 * Improved discussion of transitivity for `=`, `<`, etc.
 * Noted that `substring` is a special case of `string-copy`
 * Improved wording describing alternative exponent markers
 * Fixed `read-bytevector!` argument order to match `write-bytevector`
 * Added includers to formal syntax of primitive procedures
 * Corrected formal syntax of <program> to match prose
 * Removed confusing term "top level" in favor of "outermost" (for syntax) and "global" (for bindings)
 * Replaced redundant non-digit with dot subsequent
 * Changed "distinct" to "normally distinct" in the explanation of negative zero
 * Explained that `equal?` is defined in terms of itself
 * Clarified that objects used to signal errors may or may not be error-objects
 * Explain that "is an error" situations may signal an error, extend the language, or fail catastrophically
 * Changed "name" to "character" in example of character escape, \x03BB
 * Moved comma outside literal string that is the printed representation of the number twenty-eight.
 * Fixed bug in derived expression for `let*-values`.
 * Revised wording of non-termination requirement for `display`.
 * Refined wording about `eqv?` in non-IEEE inexact implementations.

This page is now obsolete: see the eighth draft.