Compound objects are a generalization of SRFI 35 and R6RS compound conditions.
They encapsulate an immutable sequence of subobjects, which can be
any object except another compound object.  These procedures treat
non-compound objects as if they were compound objects with one subobject.

`(make-compound-object ` *obj* ...`)`

Create a compound object containing the *objs* in the specified order.
If any *obj* is itself a compound object, it is flattened into its subobjects,
which are then added to the result in sequence.

`(compound-object? `*obj*`)`

Returns `#t` if *obj* is a compound object, and `#f` otherwise.

`(compound-object-subobjects `*obj*`)`

If *obj* is a compound object, returns a list of its subobjects; it is an error to
mutate this list.  Otherwise, it returns a list containing only *obj*.

`(compound-object-length `*obj*`)`

If *obj* is a compound object, returns the number of its subobjects as an exact
integer.  Otherwise, it returns 1.

`(compound-object-ref `*obj k*`)`

If *obj* is a compound object, returns the *k*th subobject.  Otherwise,
*obj* is returned.  In either case, it is an error if *k* is less than
zero or greater than or equal to the length of *obj*.

`(compound-object-filter `*pred obj*`)`

If *obj* is a compound object, returns a list of its subobjects that satisfy
*pred*; it is an error to mutate this list.

If *obj* is not a compound object, it returns a list of *obj* if *obj* satisfies *pred*,
or the empty list if *obj* does not satisfy *pred*.

`(make-compound-predicate `*pred*`)`

Returns a predicate that accepts one argument *obj* and behaves as follows:

If *obj* is a compound
object such that at least one of its subobjects satisfies *pred*, the predicate
returns `#t`, otherwise `#f`.

If *obj* is not a compound object, the predicate applies *pred* to *obj* and
returns what *pred* returns, or `#f` if *pred* returns false.

`(make-compound-accessor `*pred accessor default*`)`

Returns a procedure that accepts one argument *obj* and behaves as follows:

If *obj* is a compound object, *accessor* is applied to
the first subobject that satisfies *pred* and the result is returned;
if there is no such subobject,
returns *default*.

If *obj* is not a compound object, then if the object satisfies *pred*,
it applies *accessor* to *obj* and returns what it returns,
but if *obj* does not satisfy *pred*, *default* is returned.
