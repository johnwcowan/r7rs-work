Compound objects are a generalization of SRFI 35 and R6RS compound conditions.
They encapsulate a sequence of sub-objects.

`(make-compound-object ` *obj* ...`)`

Create a compound object containing the *objs* in the specified order.
If any *obj* is itself a compound object, it is flattened into its sub-objects,
which are then added to the result in sequence.

`(compound-object? `*obj*`)`

Returns `#t` if *obj* is a compound object, and `#f` otherwise.

`(compound-object-subobjects `*obj*`)`

If *obj* is a compound object, returns a list of its subobjects; it is an error to
mutate this list.  Otherwise, it returns *obj*.

`(compound-predicate `*pred*`)`

Returns a predicate that returns `#t` if its argument satisfies *pred* or is a compound
object such that one of its sub-objects satisfies *pred*.  Otherwise the predicate
returns `#f`.

`(compound-accessor `*pred`)`

Returns a procedure that accepts a single argument and returns as follows:
If its argument is a compound object, the first sub-object that satisfies
*pred* is returned.  Otherwise, the argument itself is returned.
