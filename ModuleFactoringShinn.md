I propose the following changes from the first draft:

* Rename `(scheme io)` as `(scheme io base)`
* Add `(scheme io)` as a convenience composite of all four I/O modules
* Move `interaction-environment` to the `(scheme eval)` module
* Move `case-lambda` to a new `(scheme case-lambda)` module
* If `exact-integer-sqrt` still returns multiple values, move it to a new module
* Rename `(scheme unicode)` to `(scheme char)`
* Move `char-alphabetic?`, `char-numeric?`, `char-upper-case?`, `char-lower-case?` and `char-whitespace?` to `(scheme char)`
* Move the normalization procedures to `(scheme char normalization)`

Depending on discussion, I would consider moving
the following out of the core:

* `syntax-rules`
* `define-record-type`
* blob procedures
* `string-ref` and `string-set!`
