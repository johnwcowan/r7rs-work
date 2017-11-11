## Abstract

A storage class is a group of storage objects with the same behavior.  A storage object maps a non-negative exact integer index into a storage location.  There are standard storage classes, but it is also possible for programmers to create their own storage classes.  Each storage class allows creating a storage object of a given size, accessing a location by its index, and mutating a location by its index to a new value.  Note that the procedures used to do this need not be the standard procedures such as `make-vector`, `vector-ref`, and `vector-set!`; they may be more efficient equivalents.  Storage classes allow arrays (see [ArraysCowan](ArraysCowan.md)) and similar objects to be polymorphic in the type of storage they use.

## Constructors

`(make-storage-class `*constructor accessor mutator sizer*`)`

Returns a storage class with the specified procedures as constructor, accessor, mutator, and sizer.  See below for the invocation protocols.

`(make-bytestructure-storage-class `*bytestructure-descriptor*`)`

TBD.

## Predicates

`(storage-class? `*obj*`)`

Returns `#t` if *obj* is a storage class, and `#f` otherwise.

## Accessors

`(storage-class-constructor `*storage-class*`)`

Returns the constructor of *storage-class*.  This procedure returns a storage object belonging to the storage class, and can be called with one or two arguments: the first is an exact non-negative integer specifying the size of the object.  If objects of the class do not have a fixed size, the size must be specified as `#f`.  The second is a value to fill all the elements with.  If the second argument is omitted, the elements will have arbitrary contents.  If the class does not require storage objects (because the values are algorithmically generated, for example), the constructor returns `#f`.

`(storage-class-accessor `*storage-class*`)`

Returns the accessor of *storage-class* as a procedure.  This procedure takes two arguments, a storage object and an exact non-negative integer, and returns the value of the element indexed by the integer.  It is an error if the index is greater than or equal to the size.

`(storage-class-mutator `*storage-class*`)`

Returns the mutator of *storage-class* as a procedure.  This procedure takes three arguments, a storage object, an exact non-negative integer, and a value.  It mutates the element of the object specified by the index to be the value.  It is an error if the index is greater than or equal to the size, or if the object is not capable of storing the value.

`(storage-class-sizer `*storage-class*`)`

Returns the sizer of *storage-class* as a procedure.  This procedure takes one argument, a storage object.  It returns the size of the object specified when the object was created.  This may be an exact non-negative integer or `#f`.

## Invokers

`(make-storage-object *storage-class n* [#|*fill* ]]`)`

Returns a newly allocated storage object with class *storage-class* and length *n*, filled with value *fill*, if specified.

`(storage-object-ref `*storage-class storage-object n*`)`

Returns the *nth* element of *storage-object* as seen through the lens of *storage-class*.  It is an error if *n* is not less than the size of *storage-object*.

`(storage-object-set! `*storage-class storage-object n value*`)`

Mutates the *nth* element of *storage-object* as seen through the lens of *storage-class* so that its value is *value*.  It is an error if *n* is not less than the size of *storage-object*.

`(storage-object-length `*storage-class storage-object*`)` seen through the lens of *storage-class*.

Returns the size of *storage-object* as seen through the lens of *storage-class*.

## Standard storage classes

`vector-storage-class`

Used to create and manipulate a Scheme vector as storage.

`u8vector-storage-class`

`s8vector-storage-class`

`u16vector-storage-class`

`s16vector-storage-class`

`u32vector-storage-class`

`s32vector-storage-class`

`u64vector-storage-class`

`s64vector-storage-class`

`f32vector-storage-class`

`f64vector-storage-class`

`c64vector-storage-class`

`c128vector-storage-class`

Used to create and manipulate native numeric vectors (see [NumericVectorsCowan](NumericVectorsCowan.md)) as storage.

`sparse-storage-class`

Used to create and manipulate an object of implementation-specified type (run-length-encoded vector, hash table, tree, etc.) that provides a sparse representation of the mapping between indexes and arbitrary Scheme objects.  Note that there must be some representation of the fill value within the storage object if there is one.

