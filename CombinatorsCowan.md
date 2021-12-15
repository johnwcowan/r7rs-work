This SRFI contains various procedures that accept and return procedures,
as well as a few others, drawn from
[an earlier version of Chicken](http://wiki.call-cc.org/eggref/4/combinators).
Common Lisp has a few of them too, and more come from
[the Standard Prelude from *Programming Praxis*](http://programmingpraxis.com/contents/standard-prelude/).

**TODO**: add examples


## Combinators

These procedures are documented in the style of
[SRFI 219](https://srfi.schemers.org/srfi-219/srfi-219.html).
Rather than showing only how the procedures themselves are invoked,
it also shows how the returned procedures would be invoked.
This is done in order to make the descriptions easier to understand.
For example, if `complement` were documented in the standard style,
the description would say "Returns a procedure which, when applied to an argument,
returns `#t` when *proc* would return `#f` when applied to the same argument,
and `#f` otherwise", which is more convoluted and harder to understand.
However, this is merely a documentation style;
it would be pointless to actually invoke these procedures in this fashion.

**TODO**: reorganize the procedure list.

`((constantly `*obj* ...`)` *arg* ...`)`

Returns the *objs* as its values, ignoring *args*.

`((complement `*proc*`)` *obj*`)`

Returns `#t` when `(`*proc obj*`)` returns `#f`, and `#f` otherwise.

`((flip `*proc*`) . ` *objs*`)`

Returns `(apply `*proc* `(reverse `*objs* `)`.

`((swap `*proc*`)` *obj₁ obj₂ obj* ...`)`

Returns `(`*proc obj₂ obj₁ obj* ...`)`.

`((on-left `*proc*`)` *obj₁ obj₂*`)`

Returns (*proc obj₁*).

`((on-right `*proc*`)` *obj₁ obj₂*`)`

Returns (*proc obj₂*).

`((conjoin `*predicate* ...`)` *arg* ...`)`

Returns `#t` if the *args* satisfy all the *predicates*, and `#f` otherwise.
If a predicate is not satisfied, no more *predicates* are invoked.

`((disjoin `*predicate* ...`)` *arg* ...`)`

Returns `#t` if the *args* satisfy any of the *predicates*, and `#f` otherwise.
If a predicate is satisfied, no other predicates are invoked.

`((each-of `*proc* ... `)` *arg* ...`)`

Applies each of the *procs* in turn to *args*,
discarding the results and returning an unspecified value.

`((all-of) `*predicate*`)` *list*`)`

Applies *predicate* to each element of *list* in turn,
and immediately returns `#f` if *predicate* is not satisfied by that element;
otherwise returns the result of the last call to *predicate*.
If *list* is empty, returns `#t`.

`((some-of `*predicate*`)` *list*`)`

Applies *predicate* to each element of *list* in turn,
and if *predicate* is satisfied by that element,
immediately returns the result of calling *predicate*;
otherwise returns `#f`.  If *list* is empty, returns `#f`.

`((on `*reducer mapper*`)` *obj* ...`)`

Applies *mapper* to each *obj* and then applies *reducer* to the results.

`((left-section `*proc arg* ...`)` *obj* ...`)`

Applies *proc* to *args* concatenated with *objs*.

`((right-section `*proc arg* ...`)` *obj* ...`)`

Applies *proc* to *objs* concatenated with the value of `(reverse `*args*`)`.

`((apply-chain `*proc* ...`)` *arg ...)`

Applies the last *proc* to *args* returning zero or more values,
then applies the previous *proc* to the values, returning more values,
until the first proc has been invoked; its values are returned.
For example, `(apply-chain car cdr)` returns a procedure that
behaves like `cadr`.

`((arguments-all ` *proc*`)` *args*`)`  
`((arguments-each/ ` *proc*`)` *args*`)`

[Invokes predicates, giving up after `#f` or `#t` respectively
is reached.]

`((arguments-drop ` *proc n*`)` *arg*`)`  
`((arguments-drop-right ` *proc n*`)` *arg*`)`  
`((arguments-take ` *proc n*`)` *arg*`)`  
`((arguments-take-right ` *proc n*`)` *arg*`)`

[Apply *proc* to the *args* after taking/dropping *n* arguments
from *args*.]

`((group-by `*key-proc* [=]`)` *list*`)`

Takes the elements of *list* and applies *key-proc*
to each of them to get their keys.  Elements that
whose keys are the same (in the sense of =,
which defaults to `equal?`)
are grouped into newly allocated lists, and a list of
these lists is returned.  Within each list, the elements
appear in the same order as they do in *list*; in addition,
the first elements of each list also appear in the same
order as they do in *list*.  If *list* is the empty list,
it is returned.

## Syntax-like procedures

These are Scheme procedures that correspond to basic syntax.
As usual in Lisps, *thunk* means a procedure that does not require arguments.

`(begin-procedure `*thunk* ...`)`

Invokes *thunks* in order, and returns what the last thunk returns,
or an unspecified value if there are no thunks.

`(if-procedure `*value then-thunk* *else-thunk*`)`

If *value* is true, invokes *then-thunk* and returns what it returns.
Otherwise, invokes *else-thunk* and returns what it returns.

`(when-procedure `*value thunk* ...`)`  
`(unless-procedure `*value thunk* ...`)`

If *value* is false/true, immediately returns.
Otherwise, invokes each *thunk* in turn and then returns.
n all cases an unspecified value is returned.

`(value-procedure `*value then-proc else-thunk*`)`

If *value* is true, invokes *then-proc* on it
and returns what *then-proc* returns.
Otherwise, invokes *else-thunk* and returns what it returns.

`(case-procedure `*value thunk-alist* *else-thunk*`)`

Searches *thunk-alist* for *value* (as if by `assv`).
If there is a matching entry in *thunk-alist*,
its cdr is invoked as a thunk, and `case-procedure` returns what the thunk returns.
If there is no such entry in *thunk-alist*,
invokes *else-thunk* and returns what it returns.

`(lazy-and-procedure `*thunk* ...`)`  
`(eager-and-procedure `*thunk* ...`)`

Invokes each *thunk* in the order given.
The `lazy` version returns false immediately if any of the thunks return false,
whereas the `eager` version always invokes all the thunks.
If the last thunk is evaluated, its value is returned.
Returns `#t` if there are no thunks.

`(lazy-or-procedure `*thunk* ...`)`  
`(eager-or-procedure `*thunk* ...`)`

Invokes each *thunk* in the order given.
The `lazy` version returns the value of a thunk
immediately if the thunk returns false,
whereas the `eager` version always invokes all the thunks.
If the last thunk is evaluated, its value is returned.
Returns `#f` if there are no thunks.

`(loop-procedure `*thunk*`)`

Invokes *thunk* repeatedly.  Does not return unless via `call/cc`.

`(while-procedure `*thunk*`)`

Invokes *thunk* repeatedly until it returns false.
Returns an unspecified value.

`(until-procedure `*thunk*`)`

Invokes *thunk* repeatedly until it returns true.
Returns an unspecified value.

## Other procedures

`(always `*obj* ...`)`

Ignores its arguments and always returns `#t`.

`(never `*obj* ...`)`

Ignores its arguments and always returns `#f`.

`(boolean `*obj*`)`

If *obj* is true, returns `#t`; otherwise returns `#f`.

`(identity `*obj*`)`

Returns *obj*; normally passed to a higher-order procedure rather than being invoked directly.  Equivalent to `values` with a single argument but with a clearer name.
