## Arrays

## Specialized and generalized arrays

## Intervals, storage classes, and array constructors

`(make-interval `*lower-bounds upper-bounds*`)`

Returns a multidimensional interval
whose lower bounds are specified by the vector *lower-bounds*
and whose upper bounds are specified by the vector *upper-bounds*.

`(interval-contains-multi-index `*interval index0 index1* ...`)`

Returns `#t` if the element specified by the *indexes*
is included in *interval*.

`generic-storage-class`

A variable containing a storage class representing a heterogeneous vector.
However, there is no guarantee that an object of the generic storage class is necessarily
a Scheme vector.

`@-storage-class`

Twelve variables containing storage classes that represent homogeneous vectors.
The character `@` can be replaced by `u8`, `s8`, ... `c128`, the datatypes
of [SRFI 160](http://srfi.schemers.org/srfi-160/srfi-160.html) homogeneous vectors.
However, there is no guarantee that an object of an @-storage-class is necessarily
a SRFI 160 @vector.

`(make-specialized-array `*interval* [ *storage-class* [ *safe?* ] ]`)`

Returns a specialized array whose dimensions are specified by *interval*
using as its backing store an object of the class specified by *storage-class*
(`generic-storage-class` by default).

If the *safe?* argument is true, operations on the returned array will be checked
for being within *interval* in the sense of `interval-contains-multi-index`.
If it is false, operations are not checked.  If *safe* is omitted, the value
is implementation-dependent.

## Predicates

`(interval? `*object*`)`

Return `#t` if *object* is an interval object
and `#f` otherwise.

`(array? `*object*`)`

Return `#t` if *object* is an array
and `#f` otherwise.

`(specialized-array? `*object*`)`

Returns `#t` if *object* is a specialized array
and `#f` otherwise.

`(mutable-array? `*object*`)`

Returns `#t` if *object* is a mutable array, either
generalized or specialized,
and `#f` otherwise.

`(translation? `*object*`)`

Returns `#t` if *object* is a vector representing a translation of array indices
(that is, if every element is an exact integer)
and `#f` otherwise.

`(permutation? `*object*`)`

Returns `#t` if *object* is a vector representing a permutation of array indices
(that is, if each element is a distinct integer in the range 0 (inclusive)
to the length of *vector* exclusive*)
and `#f` otherwise.

`(array-any `*pred array1 array2* ...`)`

Invokes *pred* on the corresponding elements of the *arrays* in lexicographic order.
As soon as a call to *pred* returns true, its value is returned by `array-any`.
If all calls return false, `array-any` returns false.

`(array-every `*pred array1 array2* ...`)`

Invokes *pred* on the corresponding elements of the *arrays* in lexicographic order.
As soon as a call to *pred* returns false, `array-any` returns false.
If all calls return true, the value of the last call is returned by `array-any`.

## Individual array elements

There are no generic `array-ref` or `array-set!` procedures provided.
Instead, individual array elements can be retrieved or changed
using the `array-getter` and `array-setter` procedures, as follows:

`((array-getter `*array*`)` *index0 index1* ...`)`

Returns the value of *array* specified by the *indexes*.

`((array-setter `*array*`)` *newvalue index1 index2* ...`)`

Changes the value of array specified by the *indexes* to *newvalue*.

## Affine transformations

`(array-extract `*array interval*`)`

`(array-tile `*array intvector*`)`

`(array-translate `*array translation*`)`

`(array-permute `*array permutation*`)`

`(array-curry `*array dimension*`)`

`(array-reverse `*array flip?*`)`

`(array-sample `*array scales*`)`

## Mapping and folding

`(array-map `*f array1 array2* ...`)`

`(array-for-each `* array1 array2* ...`)`

`(array-fold `*kons knil array*`)`

`(array-fold-right `*kons knil array*`)`

`(array-outer-product `*proc array1 array2*`)`


## Mutators

`(array-assign! `*dst src*`)`

Copies the elements of the array *src* into the corresponding locations
of the array *dst*, which must be mutable.
It is an error if the arrays do not have the same interval.

`(array-swap! `*array1 array2*`)`

Swaps the corresponding elements of *array1* and *array2*,
which must both be mutable.
It is an error if the arrays do not have the same interval.

## Conversions

`(list->specialized-array `*interval storage-class safe?*`)`

Copies the elements of *list* into the positions
of a newly created specialized array in lexicographic order.
The arguments *interval*, *storage-class*, and *safe?* have the same meaning
as in `make-specialized-array`.  Returns the newly created array.

`(array->specialized-array `*array storage-class safe?*`)`

Copies the elements of *array* into the corresponding positions
of a newly created specialized array in lexicographic order.
The arguments *storage-class* and *safe?* have the same meaning
as in `make-specialized-array`.  Returns the newly created array.

`(array-list `*array*`)`

Returns a list containing the elements of *array* in lexicographic order.

`