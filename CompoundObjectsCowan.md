Compound objects are a generalization of SRFI 35 and R6RS compound conditions.
They encapsulate an immutable sequence of subobjects, which can be
any object except another compound object.  These procedures treat
non-compound objects as if they were compound objects with one subobject.

`(make-compound-object ` *list*`)`

Create a compound object containing the objects in *list* in the specified order.
If any object is itself a compound object, it is flattened into its subobjects,
which are then added to the result in sequence.

`(compound-object ` *obj* ...`)`

The same as `make-compound-object`, except that it accepts multiple arguments instead of a list.

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

`(compound-object-map `*mapper obj*`)`

If *obj* is a compound object, returns a newly allocated compound object
whose subobject result from invoking *mapper* on each subobject of *obj*.
Although the subobjects of the result are in the same order as the subobjects of *obj*,
the order in which *mapper* is applied is unspecified.

If *obj* is not a compound object, it returns a compound object containing
the result of applying *mapper* to *obj*.

`(compound-object-filter `*pred obj*`)`

If *obj* is a compound object, returns a newly allocated compound object
that contains the subobjects of *obj* that satisfy
*pred*; it is an error to mutate this list.

If *obj* is not a compound object, it returns a compound object containing just *obj* if *obj* satisfies *pred*,
or an empty compound object if *obj* does not satisfy *pred*.

`(make-compound-predicate `*pred*`)`

Returns a predicate that accepts one argument *obj* and behaves as follows:

If *obj* is a compound
object such that at least one of its subobjects satisfies *pred*, the predicate
returns what *pred* returns, otherwise `#f`.

If *obj* is not a compound object, the predicate applies *pred* to *obj* and
returns what *pred* returns.

`(make-compound-accessor `*pred accessor default*`)`

Returns a procedure that accepts one argument *obj* and behaves as follows:

If *obj* is a compound object, *accessor* is applied to
the first subobject that satisfies *pred* and the result is returned;
if there is no such subobject,
returns *default*.

If *obj* is not a compound object, then if the object satisfies *pred*,
it applies *accessor* to *obj* and returns what it returns,
but if *obj* does not satisfy *pred*, *default* is returned.
