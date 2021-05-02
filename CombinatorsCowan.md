This proposal contains various procedures that accept and return procedures, as well as a few others, drawn from an earlier version of Chicken.  Common Lisp has a few of them too, and more come from [the Standard Prelude from *Programming Praxis*](http://programmingpraxis.com/contents/standard-prelude/).


## Combinators

These procedures are documented in an unusual style.  Rather than showing only how the procedures themselves are invoked, it also shows how the returned procedures would be invoked.  This is done in order to make the descriptions easier to understand.  For example, if `complement` were documented in the standard style, the description would say "Returns a procedure which, when applied to an argument, returns `#t` when *proc* would return `#f` when applied to the same argument, and `#f` otherwise", which is more convoluted and harder to understand.  However, this is merely a documentation style; it would be pointless to actually invoke these procedures in this fashion.

`((constantly `*obj* ...`)` *arg* ...`)`

Returns the *objs* as its values, ignoring *args*.

`((complement `*proc*`)` *obj*`)`

Returns `#t` when `(`*proc obj*`)` returns `#f`, and `#f` otherwise.

`((swap `*proc*`)` *obj₁ obj₂*`)`

Invokes `(`*proc obj₂ obj₁*`)`.

`((flip `*proc*`)` *arg1 arg2*`)`

Returns `(`*proc arg2 arg1*`)`.

`((on-left `*proc*`)` *obj₁ obj₂*`)`

Returns (*proc obj₁*).

`((on-right `*proc*`)` *obj₁ obj₂*`)`

Returns (*proc obj₂*).

`((conjoin `*predicate* ...`)` *arg* ...`)`

Returns `#t` if the *args* satisfy all the *predicates*, and `#f` otherwise.

`((disjoin `*predicate* ...`)` *arg* ...`)`

Returns `#t` if the *args* satisfy any of the *predicates*.

`((each-of `*proc* ... `)` *arg* ...`)`

Applies each of the *procs* in turn to *args*, discarding the results and returning an unspecified value.

`((all-of `*predicate*`)`

Applies *predicate* to each element of *list* in turn, and immediately returns `#f` if *predicate* is not satisfied by that element; otherwise returns `#t`.

`((any-of `*predicate*`)` *list*`)`

Applies *predicate* to each element of *list* in turn, and if *predicate* is satisfied by that element, returns the result of calling *predicate*; otherwise returns `#f`.  If *list* is empty, returns `#t`.

`((on `*reducer mapper*`)` *obj* ...`)`

Applies *mapper* to each *obj* and then applies *reducer* to the results.

`((left-section `*proc arg* ...`)` *obj* ...`)`

Applies *proc* to *args* concatenated with *objs*.

`((right-section `*proc arg* ...`)` *obj* ...`)`

Applies *proc* to *objs* concatenated with *args*, where *args* are in reverse order.

## Syntax-like procedures

These are Scheme procedures that correspond to basic syntax.  They are derived from [the If class of Samizdat](https://github.com/danfuzz/samizdat/blob/master/doc/library-guide/If.md).

`(begin-procedure `*thunk* ...`)`

Invokes *thunks* in order, and returns what the last thunk returns, or an unspecified value if there are no thunks.

`(if-procedure `*value then-thunk* [*else-thunk*]`)`

If *value* is true, invokes *then-thunk* and returns what it returns.  Otherwise, invokes *else-thunk* and returns what it returns.  If *else-thunk* is not specified, returns an unspecified value.

`(if-not-procedure `*value else-thunk*`)`

If *value* is true, returns an unspecified value.  Otherwise, invokes *else-thunk* and returns what it returns.

`(value-procedure `*value then-proc else-thunk*`)`

If *value* is true, invokes *then-proc* on it and returns what *then-proc* returns.  Otherwise, invokes *else-thunk* and returns what it returns.

`(case-procedure `*value thunk-alist* [*else-thunk* ]`)`

Searches *thunk-alist* for *value* (as if by `assv`).  If there is a matching entry in *thunk-alist*, its cdr is invoked as a thunk, and `case-procedure` returns what the thunk returns.  If there is no such entry in *thunk-alist*, invokes *else-thunk* and returns what it returns, or returns an unspecified value if *else-thunk* is not provided.

`(and-procedure `*thunk* ...`)`

Invokes each *thunk* in the order given, and returns `#f` immediately if any of them return `#f`.  Otherwise returns the value of the last thunk, or `#t` if there are none.

`(or-procedure `*thunk* ...`)`

Invokes each *thunk* in the order given, and if any of them returns true, `or-procedure` returns that value immediately.  Otherwise returns `#f`.

`(loop-procedure `*thunk*`)`

Invokes *thunk* repeatedly.  Does not return.

`(while-procedure `*thunk*`)`

Invokes *thunk* repeatedly until it returns false.  Returns false.

`(until-procedure `*thunk*`)`

Invokes *thunk* repeatedly until it returns true.  Returns the last value.


## Other procedures

`(always `*obj* ...`)`

Ignores its arguments and always returns `#t`.

`(never `*obj* ...`)`

Ignores its arguments and always returns `#f`.

`(boolean *obj*`)`

If *obj* is true, returns `#t`; otherwise returns `#f`.

`(identity `*obj*`)`

Returns *obj*.
