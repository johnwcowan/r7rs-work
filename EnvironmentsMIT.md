This SRFI provides the ability to create and control top-level
Scheme environments, which are used in R[567]RS Scheme
to give meaning to variables and syntactic keywords
that are referred to in contexts where they have not been lexically bound.
These environments can be passed to `eval` as a second argument, but otherwise
cannot be controlled or manipulated.

In R5RS there were only three such environments; R6RS and R7RS added the ability
to create environments based on a set of import specifiers.  The environments
of this SRFI are built on top of those environments, but add the ability to
create, obtain, and destroy bindings and to dcreate either fully mutable
or mutable environments, as well as an inheritance relationship
between environments that makes them a forest of trees.

## Constructors

`(interaction-environment)`   [R5RS, R6RS, R7RS]

Returns a unique fully mutable environment containing an implementation-specified
set of bindings.  On R6RS systems, at least the `(rnrs base`) bindings must be
included; on R7RS systems, at least the `(scheme base)` bindings must be included.
The intention is that this environment is the one used to
evaluate expressions entered to the system REPL.

`(scheme-report-environment `*version*`)`  [R5RS, R6RS, R7RS]

If *version* is the exact integer 5, returns an environment containing the bindings
of the R5RS (in R5RS and R6RS systems) or of the `(scheme r5rs)` library (in R7RS
systems).  These are almost but not quite the same (e.g. `transcript-on` and
`transcript-off` are not part of the R7RS library).  The returned environment may
be mutable or immutable.

Implementations may also support other values of *version*,
in which case they return a specifier for an environment
containing bindings corresponding to the specified version
of the report. If version is neither 5 nor another value
supported by the implementation, an error is signaled.

`(null-environment `*version*`)`  [R5RS, R6RS, R7RS]

This procedure behaves the same as `scheme-report-environment`,
except that only syntactic keywords are bound and not variables.

`(environment `*list* ...`)`  [R6RS, R7RS]

This procedure returns a specifier for the environment that
results by starting with an empty environment and then
importing each list, considered as an import set, into it.
In R6RS systems, the 
bindings of the resulting environment
are immutable; R7RS additionally requires that the environment
itself is immutable (no bindings may be added or removed).

`(make-environment `*environment*`)`

Creates and returns a newly allocated environment that inherits
from *environment*.  If a lookup in this environment does not
find a suitable binding, *environment* (known as the parent
environment) will be searched next.

## Predicates

`(environment? `*obj*`)`

Returns `#t` if *obj* is an environment in the sense of this SRFI,
and `#f` on all other standard Scheme objects.  However, if the
implementation reifies local lexical environments, which this SRFI
does not require, `environment?` may return `#t` on them as well.

`(environment-has-parent? `*env*`)`

Returns `#t` if *env* has a parent environment and
`#f` if it does not.  It is unspecified whether the interaction
environment, as well as environments returned by
`scheme-report-environment` and `null-environment`, have parents or not.
Environments returned by `environment` do not have parents.

`(environment-bound? `*environment symbol*`)`

Returns `#t` if *symbol* is bound in *env*, and `#f` otherwise.

`(environment-assigned? `*environment symbol*`)`

Returns `#t` if *symbol* is not only bound in *env* but
assigned to a value, and `#f` otherwise.

Such unassigned symbols may arise from the
expansion of `letrec` or `letrec*`, although they are not normally
observable.  In addition, a Scheme implementation may treat all
possible symbols as bound, but not necessarily assigned, in the
interaction environment.

`(environment-assignable? `*env symbol*`)`

Returns `#t` if *symbol* is assignable in *env*
(it is bound and can be modified) and `#f` otherwise.

`(environment-definable? `*env symbol*`)`

Returns `#t` if *symbol* is definable in *env*
(it is either bound and can be assigned, or is unbound and
can be bound in the environment), and `#f` otherwise.

## Accessors

`(environment-parent `*env*`)`

Returns the parent environment of *env*; it is an error if
*env* has no parent (see `environment-has-parent?`).

`(environment-bound-names `*env*`)`

Returns a list of all the names bound in *env*, excluding those
bound only in the ancestors of *env*.  It is an error to mutate
this list.

`(environment-bindings `*env*`)`

Returns a list of the bindings in *env*, *env*, excluding those
bound only in the ancestors of *env*.  However, if the environment
treats all symbols as bound, it need not return every possible
synbol.  It is an error to mutate this list.

Each element of the list is itself a list, whose first element
is the bound symbol and whose second element, if there is one,
is the assigned value.

`(environment-reference-type `*env symbol*`)`

Returns a symbol, one of `unbound`, `unassigned`, `macro`, or `normal`
representing the status of *symbol* in *env* and its ancestors.
The difference between `macro` and `normal` is that the former means that
*symbol* is a syntactic keyword, whereas *normal* means it represents a variable.

`(environment-lookup `*env symbol*`)`

Returns the assigned value of `symbol` in *env* or its ancestors.  It is an
error if *symbol* is not a normal symbol in the sense of `environment-reference-type`.

`(environment-lookup-macro `*env symbol*`)`

Returns the assigned value of `symbol` in *env* or its ancestors.  It is an error
if *symbol* is not a syntactic keyword.  The assigned value is a syntax transformer
whose implementation is system-dependent.

`(environment-assign! `*env symbol value*`)`

Assigns *value* to *symbol* in *env*, making *symbol* a normal symbol in the
sense of `environment-reference-type`.  It is an error if *symbol* is not
assignable in *env*.

`(environment-define `*env symbol value*`)`

Defines *symbol* as *value* in *env*, making *symbol* a normal symbol in the
sense of `environment-reference-type`.  It is an error if *symbol* is not
definable in *env*.

`(environment-define-macro `*env symbol value*`)`

Defines *symbol* as *value* in *env*, making *symbol* a syntactic keyword.
No check is made to see if *value* is really a syntax transformer.
It is an error if *symbol* is not definable in *env*.







