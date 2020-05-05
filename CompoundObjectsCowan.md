Compound objects are a generalization of SRFI 35 and R6RS compound conditions,
and are suitable for use in creating and handling conditions.
They encapsulate an immutable sequence of subobjects, which can be
any object except another compound object.  These procedures treat
non-compound objects as if they were compound objects with one subobject.

By convention, a subjobject of the form `(a)` or `(a (b . value1) (c . value2) ...)`
determines the compound object's overall type and any associated key-value properties.
A compound object may contain more than one such subobject.

## Procedures

`(make-compound ` *list*`)`

Create a compound object containing the objects in *list* in the specified order.
If any object is itself a compound object, it is flattened into its subobjects,
which are then added to the result in sequence.

`(compound ` *obj* ...`)`

The same as `make-compound`,
except that it accepts multiple arguments instead of a list.

`(compound? `*obj*`)`

Returns `#t` if *obj* is a compound object, and `#f` otherwise.

`(compound-type? `*obj*`)`

Returns `#t` if *obj* is a pair whose car is a symbol
and whose cdr is an alist with keys that are symbols.

`(compound-subobjects `*obj*`)`

If *obj* is a compound object, returns a list of its subobjects
Otherwise, returns a list containing only *obj*.

`(compound-values `*obj*`)`

If *obj* is a compound object, returns its subobjects as multiple values.
Otherwise, returns *obj*.

`(compound-length `*obj*`)`

If *obj* is a compound object, returns the number of its subobjects as an exact
integer.  Otherwise, it returns 1.

`(compound-ref `*obj k*`)`

If *obj* is a compound object, returns the *k*th subobject.  Otherwise,
*obj* is returned.  In either case, it is an error if *k* is less than
zero or greater than or equal to the length of *obj*.

`(compound-map `*mapper obj*`)`

If *obj* is a compound object, returns a compound object
whose subobjects result from invoking *mapper* on each subobject of *obj*.
Although the subobjects of the result are in the same order as the subobjects of *obj*,
the order in which *mapper* is applied to them is unspecified.

If *obj* is not a compound object, returns a compound object
whose only subobject is the result of applying *mapper* to *obj*.

`(compound-filter `*pred obj*`)`

If *obj* is a compound object, returns a compound object
that contains the subobjects of *obj* that satisfy *pred*.

If *obj* is not a compound object, it returns a compound object
whose only subobject is *obj* if *obj* satisfies *pred*,
or an empty compound object if *obj* does not satisfy *pred*.

`(compound-predicate `*pred obj*`)`

If *obj* is a compound
object such that at least one of its subobjects satisfies *pred*,
returns what *pred* returns when applied to the first such subobject;
otherwise returns `#f`.

If *obj* is not a compound object, applies *pred* to *obj* and
returns what *pred* returns.

`(compound-accessor `*pred accessor obj default*`)`

If *obj* is a compound object, *accessor* is applied to
the first subobject that satisfies *pred* and the result is returned;
if there is no such subobject, *default* is returned.

If *obj* is not a compound object, then if the object satisfies *pred*,
it applies *accessor* to *obj* and returns what it returns,
but if *obj* does not satisfy *pred*, *default* is returned.

`(compound-type-properties `*sym obj*`)`

If *obj* is a compound object, then if it contains a subobject
satisfying `compound-type?` whose car is *sym*, then it
returns the cdr of the first such type object; otherwise it returns `#f`.

If *obj* is not a compound object, then if it satisfies `compound?`
and its car is *sym*, then it returns the cdr of *obj*; otherwise it returns `#f`.
