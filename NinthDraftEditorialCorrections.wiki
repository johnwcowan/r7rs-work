Two substantive changes were made between the ninth draft and the tenth (post-ballot) draft:

* The R5RS semantics of `eqv?` as applied to procedures was restored: it must report true if they have the same location tags, and must report false if they have different behavior (results or side effects) on some arguments, but may report either true or false when comparing other procedures
* The `eq?` predicate is now required to report true when two procedures have the same location tag, but may report either true or false when comparing other procedures

These are the editorial corrections to the ninth draft in reverse order.  All are present in the tenth draft.

* Fixed two instances of `eq?` that should have been `eqv?`, introduced by a last-minute change
* Covered additional language changes in the changes list
* Removed `s`, `f`, `d`, and `l` from the formal syntax of an exponent marker
* Added `#true` and `#false` to language changes
* Corrected commenters' names
* Changed rationale of `eqv?` to be normative
* Improved language that defines when inexact numbers are not `eqv?`
* Cleaned up bibliography to include only cited works, including SRFIs
* Added grammar for `case-lambda` to tail-recursion section
* Warning about <init>s was incorrectly on `letrec` instead of `letrec*`
* Fixed minor editorial nits from a last-minute review
* Updated acknowledgements of commenters
* Rule out `xbeef` as a character name because it is a hex scalar value
* Restored R5RS results of `gen-counter` and `gen-loser`
* Cleaned up definition of "variable definition"
* Cleaned up zero-or-more values continuations language
* Falling off a `cond-expand` is now undefined behavior (no particular behavior was voted on)
* Minor typos fixed (thanks to Jussi Piitulainen)
* Corrected changes not to imply that character and string comparisons accept 0 or 1 arguments
* Added note about the order of arguments in R6RS vs. R7RS `bytevector-copy!`
* Restored reference to `eqv?` in the storage model (as opposed to "operational equivalence")
* Improved allocation discussion to handle empty objects specially
* Changed "as following" to "as follows"
* Cleaned up all cases of "forbidden in strings" to be uniform, removing redundant references
* Made the definitions of character comparisons defer to `char->integer` to handle non-Unicode
* Fixed quasiquotation and symbol-equality examples
* Removed procedure inequivalence from language changes list
* Squeezed vertical space to save a whole page
* Removed "equal," from definition of `string<?` and friends