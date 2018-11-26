Compound objects are a generalization of SRFI 35 and R6RS compound conditions.
They encapsulate a sequence of subobjects.  These procedures attempt to treat
non-compound objects as if they were compound objects with one subobject.

`(make-compound-object ` *obj* ...`)`

Create a compound object containing the *objs* in the specified order.
If any *obj* is itself a compound object, it is flattened into its subobjects,
which are then added to the result in sequence.

`(compound-object? `*obj*`)`

Returns `#t` if *obj* is a compound object, and `#f` otherwise.

`(compound-object-subobjects `*obj*`)`

If *obj* is a compound object, returns a list of its subobjects; it is an error to
mutate this list.  Otherwise, it returns *obj*.

`(compound-predicate `*pred*`)`

Returns a predicate that accepts one argument *obj* and behaves as follows:
It returns `#t` if *obj* is a compound
object such that one of its subobjects satisfies *pred*, the predicate
returns `#t`, otherwise `#f`.
Id *obj* is not a compound object, the predicate applies *pred* to *obj* and
returns what *pred* returns.

`(compound-accessor `*pred accessor default*`)`

Returns a procedure that accepts one argument *obj* and behaves as follows:
If *obj* is a compound object, *accessor* is applied to
the first subobject that satisfies *pred*, or *default* if there is no such subobject.
If *obj* is not a compound object, then if the object satisfies *pred*,
it applies *accessor* to *obj* and returns what it returns,
but if *obj* does not satisfy *pred*, *default* is returned.
