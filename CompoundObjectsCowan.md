Compound objects are a generalization of SRFI 35 and R6RS compound conditions,
and are suitable for use in creating and handling conditions among other purposes.
They encapsulate an immutable sequence of subobjects, which can be
any object except another compound object.  

## Rationale

Compound objects belong to a disjoint type.  Consequently, they can
be used to represent multiple otherwise unrelated aspects of a value
or situation.  Because they are sequences, they can be used to
represent priorities of interpretation from higher to lower.  Most
of the operations described
in this section treat a simple condition identically to a compound
condition with itself as its own sole component. 

## Specification

When a compound object is created, a type for it may be specified:
this is either a symbol or `#f`, meaning that the object has no type.
Associated key-value properties are also specified in the form of an alist.
It is an error to mutate an alist passed to any of these procedures.

## Procedures

`(make-compound ` *typesym props list*`)`

Create a compound object whose type is *typesym*
and whose properties are in *props*.
It contains the objects in *list* in the specified order.
If any object in *list* is itself a compound object,
it is flattened into its subobjects,
which are then added to the compound object in sequence.

`(compound ` *typesym alist obj* ...`)`

The same as `make-compound`,
except that it accepts multiple arguments instead of a list.

`(compound? `*obj*`)`

Returns `#t` if *obj* is a compound object, and `#f` otherwise.

`(compound-type `*obj*`)`

If *obj* is a compound type, returns its type symbol.
If not, returns `#f`.

`(compound-properties `*obj*`)`

If *obj* is a compound type, returns its properties.
If not, returns `()`.


`(compound-subobjects `*obj*`)`

If *obj* is a compound object, returns a list of its subobjects.
Otherwise, returns a list containing only *obj*.

`(compound-length `*obj*`)`

If *obj* is a compound object, returns the number of its subobjects as an exact
integer.  Otherwise, it returns 1.

`(compound-ref `*obj k*`)`

If *obj* is a compound object, returns the *k*th subobject.
Otherwise, *obj* is returned.
In either case, it is an error if *k* is less than
zero or greater than or equal to `(compound-length `*obj*`)`.

`(compound-map `*typesym alist mapper obj*`)`

If *obj* is a compound object, returns a compound object
of type *typesym* and with the properties in *alist*.
The subobjects result from invoking *mapper* on each subobject of *obj*.
Although the subobjects of the result are in the same order as the subobjects of *obj*,
the order in which *mapper* is applied to them is unspecified.
If any resulting subobject is itself a compound object, it is flattened into its subobjects,
which are then added to the result in sequence.

If *obj* is not a compound object, returns a compound object
whose typesym is `#f`, whose alist is `()`, and
whose only subobject is the result of applying *mapper* to *obj*.

`(compound-map->list `*mapper obj*`)`

If *obj* is a compound object, returns a list
whose elements result from invoking *mapper* on each subobject of *obj*.
Although the elemnts of the result are in the same order as the subobjects of *obj*,
the order in which *mapper* is applied to them is unspecified.

If *obj* is not a compound object, returns a list
whose only element is the result of applying *mapper* to *obj*.

`(compound-filter `*typesym alist pred obj*`)`

Returns a compound object
whose typesym is *typesym* and whose properties are in *alist*.
It contains the subobjects of *obj* that satisfy *pred*.

If *obj* is not a compound object, it returns a compound object
whose only subobject is *obj* if *obj* satisfies *pred*,
or an empty compound object if *obj* does not satisfy *pred*.

`(compound-predicate `*pred*`)`

Returns a procedure taking a single argument *obj*
that behaves as follows:

If *obj* is an object that:

 * satisfies *pred*
 * is a compound object whose type is not `#f` 
   and its type satisfies *pred*
 * at least one of its subobjects satisfies *pred*

then the procedure returns `#t`.

Otherwise it returns `#f`.

`(compound-accessor `*pred accessor default*`)`

Returns a procedure taking a single argument *obj*
that behaves as follows:

If *obj* is a compound object, *accessor* is applied to
the first subobject of *obj* that satisfies *pred* and the result is returned;
if there is no such subobject, *default* is returned.

If *obj* is not a compound object, then if the object satisfies *pred*,
it applies *accessor* to *obj* and returns what it returns.
If *obj* does not satisfy *pred*, *default* is returned.

