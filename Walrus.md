## What we need to implement an R7RS runtime over R6RS

# Lexical syntax

* Support `#u8` rather than `#vu8`
* Support datum labels (`#n=` and `#n#`)

# Libraries

* Reshape `define-library` forms into `library` forms with the help of the `include` library

# Syntax (and associated procedures)

* Support `include` syntax with the `(include)` library
* Support `cond-expand` syntax (somehow)
* Support `delay-force` syntax
* Support `promise?` and `make-promise` procedure (may need to reimplement promises)
* Support SRFI 39 parameters (Chez's parameters aren't compatible)
* Add support for `(... <template>)`, which removes the special meaning of ellipsis
* `syntax-error` needs to supply a *who* argument
* Support `define-values` syntax

