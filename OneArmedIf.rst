The Scheme standards say that a one-armed `if` returns an unspecified value if the first argument is false.  The great majority of all Schemes tested have an "unspecified value" value, which is not the same (in the sense of `eq?`) to any other value, and is returned as the value of `(if #f #f)` and in similar circumstances.  The printing of this value is often suppressed by the REPL.  The following are exceptions:

`(if #f #f)` => `#f`:  Bigloo, JScheme, Dream, Owl Lisp

`(if #f #f)` => `()`:  NexJ, !TinyScheme, Elk, UMB, Llava, Dfsch, Inlab

`(if #f #f)` => `#t`:  Shoe, !FemtoLisp

`(if #f #f)` => a value which is both `#f` and `()`: XLisp, Rep

SXM returns a closure which when invoked apparently loops forever.

Note that although R6RS allows returning an unspecified number of unspecified values, including no values, no known Scheme implementation actually does so.