The following are situations in which R5RS programs are required to
signal an error:

* specifying an unsupported version number to `scheme-report-environment`
* failure to open a file via any of the standard port opening procedures
* encountering EOF while trying to `read` an incomplete datum

----

The following is a list of all "error situations" in R5RS,
originally collected in a [post](http://www.r6rs.org/r6rs-editors/2006-February/000917.html)
by Will Clinger to the R6RS editors list.  The semantics of
these situations are unspecified in R5RS.

## Syntax errors:

* ...it is an error to alter a constant (i.e. the value of a literal expression) using a mutation procedure like set-car! or string-set!.
* Since it is an error to modify constant objects (those returned by literal expressions), implementations are permitted, though not required, to share structure between constants where appropriate. Thus the value of `eqv?` on constants is sometimes implementation-dependent.
* It is an error for a variable to appear more than once in *formals*.
* It is an error for a variable to appear more than once in the list of variables being bound.
* It is an error for a variable to appear more than once in the list of variables being bound.
* It is an error for a variable to appear more than once in the list of do variables.
* It is an error for a <keyword> to appear more than once in the list of keywords being bound.
* It is an error for the same pattern variable to appear more than once in a pattern.
* A subpattern followed by `...` can match zero or more elements of the input. It is an error for `...` to appear in the list of literals. Within a pattern the identifier `...` must follow the last element of a nonempty sequence of subpatterns.
* It is an error to use a macro keyword, within the scope of its binding, in an expression that does not match any of the patterns.
* It is an error if the output cannot be built up as specified.
* Although macros may expand into definitions and syntax definitions in any context that permits them, it is an error for a definition or syntax definition to shadow a syntactic keyword whose meaning is needed to determine whether some form in the group of forms that contains the shadowing definition is in fact a definition, or, for internal definitions,

## Evalutation-time errors:

###  Environment errors:

* It is an error to attempt to store a new value into a location that is denoted by an immutable object.
* It is an error to reference an unbound variable.
* One restriction on letrec is very important: it must be possible to evaluate each init expression without assigning or referring to the value of any variable. If this restriction is violated, then it is an error.
* If variable is not bound, however, then the definition will bind variable to a new location before performing the assignment, whereas it would be an error to perform a <tt>set!</tt> on an unbound variable.
* It is an error to apply mutation procedures like `string-set!` to strings returned by this procedure.

###  Typechecking errors:

* For example, it is an error for a procedure to be passed an argument that the procedure is not explicitly specified to handle, even though such domain errors are seldom mentioned in this report. Implementations may extend a procedure's domain of definition to include such arguments.
* The `length`, `vector-length`, and `string-length` procedures must return an exact integer, and it is an error to use anything but an exact integer as an index.
* It is an error to take the car of the empty list.
* It is an error to take the cdr of the empty list.
* It is an error if list has fewer than k elements.
* It is an error if list has fewer than k elements.
* It is an error to read from a closed port. (Do we add a closed-port? predicate ?)

## Function domain errors: ==

* It is an error if no possible result makes this expression true.
* The error case can occur only when z is not a complex number or is a complex number with a non-rational real or imaginary part.
* If a file with the given name already exists, the effect is unspecified.
