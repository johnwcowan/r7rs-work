Some SRFIs export identifiers that are exported by `(scheme base)` or by another SRFI.  When the libraries that export an identifier give it the same or a compatible semantics, there need not be any conflict; all libraries that export the identifier can export the same binding, with its most general semantics.  When two libraries export the same identifier with truly incompatible semantics, however, the conflict is real and cannot be resolved without renaming or other workarounds.

In the table below, an identifier whose SRFI semantics generalizes its R7RS semantics is counted as a real conflict.  When an identifier's R7RS semantics generalizes its SRFI semantics, the conflict is counted as superficial.

||'''SRFI'''||'''real conflicts with R7RS'''||'''real conflicts with R6RS'''||'''real conflicts with SRFIs'''||'''superficial conflicts with R7RS'''||
||SRFI 0|| || || || `cond-expand` ||
||SRFI 1|| || `assoc` `member` `for-each` `map` `make-list` `list-copy` || || `cons` `list` `make-list` `list-copy` `pair?` `null?` `car` `cdr` ... `cddddr` `list-ref` `length` `append` `reverse` `map` `for-each` `member` `memq` `memv` `assoc` `assq` `assv` `set-car!` `set-cdr!` ||
||SRFI 2|| || || || ||
||SRFI 4|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 5|| `let` || `let` || || ||
||SRFI 6|| || || || ||
||SRFI 7|| || || || ||
||SRFI 8|| || || || ||
||SRFI 9|| || `define-record-type` || || `define-record-type` ||
||SRFI 10|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 11|| || || || ||
||SRFI 13|| `string-downcase` `string-upcase` `string-for-each` `string-map` `string-hash` || (many) || `string-hash` `string-hash-ci` `string-compare` `string-compare-ci` || `string?` `make-string` `string` `string->list` `list->string` `string-length` `string-ref` `string-copy` `string-copy` `string-set!` `string-fill!` `string-append` ||
||SRFI 14|| || || || ||
||SRFI 16|| || || || ||
||SRFI 17|| `set!` || `set!` || || ||
||SRFI 18|| || || `current-time` `time?` || `with-exception-handler` `raise` ||
||SRFI 19|| || || `current-time` `time?` || ||
||SRFI 21|| || || `current-time` `time?` || `with-exception-handler` `raise` ||
||SRFI 22|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 23|| || || || `error` ||
||SRFI 25|| `equal?` || `equal?` || `equal?` `make-array` `array-rank` `array-ref` `array-set!` || ||
||SRFI 26|| || || || ||
||SRFI 27|| || || || ||
||SRFI 28|| || || `format` || ||
||SRFI 29|| || || `format` || ||
||SRFI 30|| || || || ||
||SRFI 31|| || || || ||
||SRFI 34|| || || || `with-exception-handler` `guard` `raise` ||
||SRFI 35|| || (??) || || ||
||SRFI 36|| || (??) || || ||
||SRFI 37|| || || || ||
||SRFI 38|| || || || ||
||SRFI 39|| || || || `make-parameter` `parameterize` ||
||SRFI 40|| || || (deprecated by SRFI 41) || ||
||SRFI 41|| || || || ||
||SRFI 42|| || || || ||
||SRFI 43|| `list->vector` `vector-copy` `vector-map` `vector-for-each` || `vector-fill!` `vector->list` `list->vector` `vector-map` `vector-for-each` || || `make-vector` `vector` `vector-append` `vector?` `vector-ref` `vector-length` `vector-set!` `vector-fill!` `vector-copy!` `vector->list` ||
||SRFI 44|| `equal?` `string-set!` `vector-set!` || `equal?` `string-set!` `vector-set!` || `set?` `set-contains?` `set-delete` `set-delete!` `set-union` `set-union!` `set-intersection` `set-intersection!` `set-difference` `set-difference` (etc) || `make-list` `list` `list?` `list-ref` `make-vector` `vector` `vector-copy` `vector->list` `vector?` `vector-ref` `vector-set!` `make-string` `string` `string-copy` `string->list` `string?` `string-ref` `string-set!` ||
||SRFI 45|| || `delay` `force` || || `delay` `force` ||
||SRFI 46|| || `syntax-rules` || || `syntax-rules` ||
||SRFI 47|| `equal?` || `equal?` || (superseded by SRFI 63) || ||
||SRFI 48|| || || `format` || ||
||SRFI 49|| alternative lexical syntax || alternative lexical syntax || (yes) || ||
||SRFI 51|| || || || ||
||SRFI 54|| || || || ||
||SRFI 55|| || || || ||
||SRFI 57|| `define-record-type` || `define-record-type` || `define-record-type` ||
||SRFI 58|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 59|| || || || ||
||SRFI 60|| || || || ||
||SRFI 61|| `cond` || `cond` || || ||
||SRFI 62|| || || || ||
||SRFI 63|| `equal?` || `equal?` || (supersedes SRFI 47) || ||
||SRFI 64|| || || || ||
||SRFI 66|| || || || ||
||SRFI 67|| || || `string-compare` `string-compare-ci` `if3` `if=?` `if<?` `if>?` `if<=?` `if>=?` `if-not=?` `=?` `<?` `>?` `<=?` `>=?` || ||
||SRFI 69|| `string-hash` || || `string-hash` || ||
||SRFI 70|| lexical syntax for NaNs; `quotient` `remainder` `modulo` `gcd` `lcm` `expt` || lexical syntax for NaNs; `quotient` `remainder` `modulo`  `gcd` `lcm` `expt` || || (many) ||
||SRFI 71|| `let` `let*` `letrec` || `let` `let*` `letrec` || `let` || ||
||SRFI 72|| `define-syntax` `let-syntax` `letrec-syntax` `syntax-rules` || `define-syntax` `let-syntax` `letrec-syntax` `syntax-rules` || ||
||SRFI 74|| || || || ||
||SRFI 78|| || || || ||
||SRFI 86|| || || || ||
||SRFI 87|| || `case` || || `case` ||
||SRFI 88|| alters lexical syntax, lambda semantics || alters lexical syntax, lambda semantics || || ||
||SRFI 89|| extends `begin` syntax || extends `begin` syntax || || ||
||SRFI 90|| || || || ||
||SRFI 94|| || || || ||
||SRFI 95|| || || || ||
||SRFI 98|| || || || `get-environment-variable` `get-environment-variables` ||
||SRFI 99|| `define-record-type` || `define-record-type` || `define-record-type` || ||
||SRFI 100|| || || || ||
||SRFI 101|| `quote` `cons` `list` `make-list` `pair?` `null?` `list?` `car` `cdr` `caar` ... `cddddr` `list-ref` `list-tail` `length` `append` `reverse` `for-each` `map` || (ditto) || || ||
||SRFI 105|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 106|| || || || ||
||SRFI 107|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 108|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 109|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 110|| extends lexical syntax || extends lexical syntax || || ||
||SRFI 111|| || || || ||
||SRFI 112|| || || || ||
||SRFI 113|| || || `set?` `set-contains?` `set-delete` `set-delete!` `set-union` `set-union!` `set-intersection` `set-intersection!` `set-difference` `set-difference` (etc) || ||
||SRFI 114|| || || `if3` `if=?` `if<?` `if>?` `if<=?` `if>=?` `if-not=?` `=?` `<?` `>?` `<=?` `>=?` || ||
||SRFI 115|| || || || ||
||SRFI 116|| || || || ||

