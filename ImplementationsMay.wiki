Things that implementations explicitly ''may'' do:

* Define external representations for additional types of objects
* Share empty strings, vectors, and bytevectors (detectably by `eq?` or `eqv?` or neither)
* Recognize additional cases of tail calls.
* Use different orders of evaluation in procedure calls
* Return a non-promise as-is from `force`
* Treat promises the same as their forced values in some cases
* Return literal structure or newly allocated objects in `quasiquote`
* Write `quote` and friends using lexical syntax
* Provide macro facilities other than `syntax-rules`
* Provide additional `features` identifiers
* Treat additional characters (such as FF) as whitespace
* Detect procedure equivalence beyond what is required
* Share structures between constants
* Use multiple internal representations of numbers
* Provide more than one format for inexact numbers
* Use floating-point arithmetic for inexact numbers
* Silently coerce the unrepresentable result of an exact operation to an inexact value of the highest precision available
* Return an exact result to an inexact operation if it can prove it is safe
* Support only a limited range of numbers of any type
* May support only some types of numbers
* Coerce an exact constant to an inexact value
* Distinguish `+inf.0`, `-inf.0`, `+nan.0`, and `0.0`
* Use `+nan.0` to represent non-real numbers
* Accept non-standard exponent marker
* Allow the default inexact-number precision to be set by the user
* Represent some irrational numbers exactly
* Extend the number system with non-standard types
* Return inexact values from `make-polar`
* Silently coerce exact non-real numbers to inexact
* Restrict the domain of `string->number` in specified ways
* Support any or all Unicode characters
* Support non-Unicode characters
* Support additional character names
* Forbid characters (other than ASCII ones) from appearing in strings
* Return the argument from case-folding procedures if no change was made
* Always downcase upper-case sigma to normal lower-case sigma
* Condition objects that satisfy `file-error?` or `read-error?` but not `error?`
* Support arguments other than 5 to `scheme-version-environment` and `null-environment`
* Make the result of `scheme-report-environment` and `null-environment` mutable
* Extend `eval` to allow other objects
* Provide port types other than textual and binary
* Provide input/output ports
* Recognize alternative line-ends
* Use locale information to encode and decode environment variables
* Vary the epoch of `current-jiffy` between runs of a program
* Bind a record-type name to a syntactic or procedural object
* Process `cond-expand` and `import` library declarations before `import`, `include`, and `include-ci` declarations
* Load libraries more than once
* Provide a REPL
* In the REPL, treat all variables as bound
* Allow the REPL to read input from a file
* Allow additional Unicode characters in identifiers
* Process "is an error" situations in a variety of ways
* Raise an exception on domain errors
* Provide non-portable extensions of any kind
