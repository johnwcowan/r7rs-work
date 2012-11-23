== Multiple values procedures ==

`(call-with-all-values `''producer'' ...` `''consumer''`)`

Invoke the ''producers'' in arbitrary order and pass all values returned by them in the given order to ''consumer''.  This extends `call-with-values` by allowing more than one ''producer''.  Equivalent to Common Lisp's `multiple-value-call`.

`(values-list `''list''`)`

Returns the elements of the list as multiple values.  Equivalent to Common Lisp's `values-list`.

== Multiple values syntax ==

`(set!-values (`''lambda-list''`) `''expression''`)`

Assigns the values returned by ''expression'' to the variables in ''lambda-list''.
Returns an undefined value.  Equivalent to Common Lisp's `multiple-variable-setq`.

'''Is this module even worth having any more?  Most of its former content is part of WG1 now.'''