This is an explanation of what's optional in IEEE and R5RS Scheme, grouped for ease of understanding.

=== Not present in IEEE, required in R5RS ===

Laziness: `delay, force`

Macros: `define-syntax, let-syntax, letrec-syntax`

Lists: `list-ref, list-tail`

Strings: `list->string, string->list, string-copy, string-fill!`

Vectors: `list->vector, vector->list, vector-fill!`

Multiple values: `call-with-values, values`

Eval: `eval, null-environment, scheme-report-environment`

=== Not present in IEEE, optional in R5RS ===

Arithmetic: Subtraction and division with more than two arguments

REPL: `interaction-environment`

Files: `with-input-to-file, with-output-to-file`

Loading: `load`

=== Optional in both IEEE and R5RS ===

Inexact rationals: `exp, log, sin, cos, tan, asin, acos, atan, sqrt`

Complex numbers: `angle, imag-part, magnitude, make-polar, make-rectangular, real-part`

