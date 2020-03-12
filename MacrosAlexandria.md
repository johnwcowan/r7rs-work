`(let-list vars list . body)`

Bind (possibly improper) list of variables to the first n elements of *list*.

`(if-let let-bindings then else)`

Do all bindings specified by *let-bindings*.  If all bound values are true,
evaluate *then*, otherwise evaluate *else*.

`(if-let* let-bindings then else)`

Similar to `if-let*`, but does bindings sequentially in the style of `let*`.

`(when-let let-bindings . body)`

Do all bindings specified by *let-bindings*.  If all bound values are true,
evaluate the definitions and expressions in *body*.  Returns an unspecified value.

`(when-let* let-bindings . body)`

Similar to `when-let`, but does bindings sequentially in the style of `let*`.

`(case-using pred expr case-clause ...)`

Similar to `case`, but uses *pred* instead of `eqv?` to do case matching.

`(case-match expr match-clause ...`)

Similar to `match`, but it is an error unless *expr* evaluates to a list
 whose car is either a symbol (matches exactly that symbol)
or a list of symbols (matches any symbol on the list).
The cdr of the pattern matches in the normal way.

`(andmap proc expr ...)`

Similar to `and`, but maps values of *exprs* through *proc* before testing for truth.

`(ormap proc expr ...)`

Similar to `or`, but maps values of *exprs* through *proc* before testing for truth.


