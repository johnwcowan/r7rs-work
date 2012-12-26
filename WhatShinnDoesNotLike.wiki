Alex Shinn is the only active member of WG1 who is also an implementer.  Here is a preliminary list of things he has to implement as part of R7RS-compliance in Chibi Scheme that he voted against.  However, he has said that he sometimes votes for R5RS choices in order to provide a conservative voice.

{{{
Shinn: I felt for various reasons that the WG was not taking
enough of a conservative stance, and so in many cases voted
for R5RS options reflexively, arguing devil's advocate even
when I disagreed with it.

But I really do hate multiple values.
}}}

 * the `call-cc` alias
 * `_` in syntax-rules
 * SRFI 46
 * dotted-tail patterns in syntax rules
 * block comments
 * char folding
 * `case-lambda`
 * `=>` in `case`
 * case-folding dircctives
 * `string-set!` in `(scheme base)`
 * `begin` rather than `body` as a library declaration
 * requiring `equal?` to return `#t` if `eqv?` does 
 * making let-syntax introduce a lexical contour
 * new integer division operators
 * procedures not having locations
 * `let-values` and `let*-values`
 * allowing `cond-expand`, `include`, and `include-ci` at top level
 * `error-object?`, `error-object-message`, and `error-object-irritants`
 * everything to do with multiple values
 * more than two arguments for char and string comparisons
 * vertical bars around symbols
 * `close-port`
 * `make-promise`
 * `(scheme ...)` instead of `(r7rs ...)`
 * UAX 29 based string case conversion
 * fundamental I/O in `(scheme base)`
 * `interaction-environment` in `(scheme repl)`
 * record types in `(scheme base)`
 * semicolon-based rather than brace-based escape sequences
 * R6RS rather than dotted-ratio syntax for non-rational reals
 * `-nan.0`
 * `make-promise`
 * ''start'' and ''end'' arguments in various procedures
 * `string-copy!` and `vector-copy!`
 * numeric syntax in programs must agree with `string->number` and `read`
 * support for undesirable characters and arbitrary numbers in library names
 * location-free procedures
 * complex numbers with `+nan.0` portions
 * predicates for R5RS signaled conditions
 * `emergency-exit`
 * type-check rule for auto-forcing
 * support for bare CR as newline
 * `digit-value`
 * singular library names
 * multiple imports in program files

See also WhatGambitWontSupport.
