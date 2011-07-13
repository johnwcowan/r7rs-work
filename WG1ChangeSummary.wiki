This is a very brief summary of what's changed in WG1 relative to R5RS so far.  It is emphatically '''not''' authoritative, just a quick thing to point people to who haven't been following the process and aren't up for reading through whole drafts.  I may have left out some tickets.

 * Modules with `import`, `export`, `begin` (for embedded code), `include`, `include-ci`, and `cond-expand`
 * `Syntax-rules`: ellipsis escaping, SRFI 46, tail patterns, and the `_` wildcard
 * Identifiers are case-sensitive
 * Identifiers and other syntax must be delimited with whitespace
 * Extended identifier syntax, including `\` and `|...|` escapes
 * `#;` and `#| ... |#` comment styles
 * List of named characters and string esscapes standardized
 * Exception system
 * The only standardized condition is `error` (with accessors and predicate)
 * Inexact infinities on systems that support them
 * IEEE semantics for transcendental functions
 * Unicode is supported but not required, including folding and normalization
 * Binary I/O
 * String and bytevector ports
 * SRFI 9 records
 * `Transcript-on` and `transcript-off` are gone
 * `Letrec*`, `let-values`, and `let*-values`
 * `Syntax-error`
 * `Map`, `for-each`, `assoc`, and `member` extended as in SRFI 1
 * Internal `define-syntax`
 * Bytevectors
 * Support for cyclic list structure as in SRFI 1
 * Read and write cyclic data, with `write-simple` to suppress writing
 * `exact-integer?` and `exact-integer-sqrt`
 * `Current-error-port`
 * `Delete-file` and `file-exists`
 * `Finite?` and `nan?` as in R6RS but supporting complex numbers too
 * `Command-line`, `exit`, `get-environment-variable`, `get-environment-variables`
 * `Call/cc`
 * SRFI 39 parameters
 * Access to TAI and elapsed time
 * `Case-lambda`
 * `=>` in `case`
 * Case folding flags
 * DivisionRiastradh
 * `Port?`, `close-port`, and `port-closed?`
 * Real numbers have #e0 as the imaginary part
 * Nested quasiquotes with partial specification for mutability
 * Procedures need not be tagged with locations
 * Dynamic-wind thunks run in the outer dynamic environment
 * `Read-line`
 * Physical newline in a string means `\n`
 * `Environment` from R6RS
 * Multiple values can be sent to continuations which discard them
 

  