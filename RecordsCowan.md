## Ridiculously Simple Records

`(make-type `*size*` `*parent*` `*id*`)`

Constructs and returns a new type object.  *Size* is an exact integer representing the number of slots in the records produced from the type object, exclusive of any slots provided by the parent type *parent*.  *Parent* may also be `#f`, in which case there is no parent type.  ``Id`` is an object to label the type with.

`(make-constructor `*type*`)`

Returns a procedure that constructs records of type *type*.  The values of the slots are undefined.

`(make-type-predicate `*type*`)`

Returns a procedure of one argument that answers `#t` if its argument is of type *type* and `#f` otherwise.  An argument is of type *type* if it was built by a constructor produced from any subtype of *type*.

`(make-getter `*type*` `*slotnum*`)`

Returns a procedure of one argument that returns the slot numbered *slotnum* in its argument, an object of type *type*.  Slots are numbered starting at 0 in the top-level type.

`(make-setter `*type*` `*slotnum*`)`

Returns a procedure of two arguments that sets the slot numbered *slotnum* in its first argument, an object of type *type*, to the value of its second argument.  Slots are numbered starting at 0 in the top-level type.

## Type Introspection

`(type-size `*type*`)`

`(type-parent `*type*`)`

`(type-id `*type*`)`

These return the properties of *type*.
