# What we need to implement an R7RS runtime over R6RS

## Lexical syntax

* Support `#u8` rather than `#vu8`
* Support datum labels (`#n=` and `#n#`)
* Support `#true` and `#false`
* Vertical-bar escapes around symbols

## Syntax (and associated procedures)

* Support `include` syntax with the `(include)` library
* Support `cond-expand` syntax (somehow)
* Support `delay-force` syntax and `promise?` and `make-promise` procedure (may need to reimplement promises)
* Support SRFI 39 parameters (Chez's parameters aren't compatible)
* Add support for `(... <template>)`, which removes the special meaning of ellipsis
* `syntax-error` needs to supply a *who* argument
* Support `define-values` syntax
* Translate R7RS `define-record-type` to R6RS `define-record-type`; the hard part is the constructor.

## Libraries

* Reshape `define-library` forms into `library` forms with the help of the `include` library

## Equivalence predicates

* Location tags for procedures (used by `eq?`)

## Numbers

* `finite?` is true of non-real numbers if both the real and the imaginary parts are finite

* `infinite?` is true of non-real numbers if either the real or the imaginary parts or both are non-finite
* `nan?` is true of non-real numbers if either the real or the imaginary parts or both are NaN
* Define `floor/`, `floor-quotient`, `floor-remainder`, truncate/`, `truncate-quotient`, `truncate-remainder` in terms of `div` and `mod`: see r6rs-lib Chapter 19
* Define `rationalize` per IEEE 1178

## Pairs and lists

* Define `make-list`
* Define `list-ref`
* Define `list-set!`
* Define `member` in terms of `member` and `memp`
* Define `assoc` in terms of `assoc` and `assp`
* Define `list-copy`

## Characters

* Define `digit-value`
* Accept `\|` string escape

## Strings

* Define `string-copy!`
* Define `string-fill!`

## Vectors

* Define `vector->string` and `string->vector`
* Define `vector-copy` and `vector-copy!`

## Bytevectors

* `bytevector-copy` takes *start* and *end* optional arguments
* `bytevector-copy!` takes different arguments
* Define `bytevector-append`
* `utf8->string` and `string->utf8` take *start* and *end* optional arguments

## Exceptions

* `error` needs to supply a *who* argument
* `error-object-message` and `error-object-irritants` are `condition-message` and `condition-irritants` (with safety checks)

## Environments and evaluation

* `eval` accepts definitions
* Define `scheme-environment` and `null-environment` based on `(rnrs r5rs)`

## Input and output

* Define `open-binary-input-file` and `open-binary-output-file` using `open-file-input-port` and `open-file-output-port`
* Define `input-port-open?` and `output-port-open?` (using exceptions somehow)
* Define `get-output-string` using second value of `open-string-output-port`
* Define `get-output-bytevector` using second value of `open-bytevector-output-port`
* Define `read-bytevector` and `write-bytevector`
* Define `read-bytevector!` and `write-bytevector!`
* Define `peek-char` and `peek-u8` using `lookahead-char` and `lookahead-u8`'
* Define `read` based on R7RS lexical syntax
* Define `write-simple` as `write`
* Define `write-circular` and `write-shared`
* Define `load`
* Define `command-line`, `exit`, `get-environment-variable`, `get-environment-variables`, `current-second`, `current-jiffy`, `jiffies-per-second`, `features` (not portable)
