## Introduction

Arrays are the simplest and oldest data structures in programming languages.
They go back to Fortran I, which was released in 1957 and provided
simple one- and two-dimensional arrays of integer or floating-point values
as its only data structures.
Arrays have been supplemented by many other data structures
and have grown in complexity since then,
but the core ideas are the same.

This guide is partly a tutorial introduction to the SRFI,
but also contains some less-technical reference material
that may be more accessible to programmers who are not mathematicians.
It is intended to be correct but definitely not complete;
in particular, only a few of the procedures on intervals
and the reflective procedures on arrays are even mentioned in passing.
In case of discrepancies between this guide and the SRFI, the SRFI prevails.

Arrays as defined in the SRFI are generalizations
of ordinary Scheme vectors in two different ways.
Whereas a vector is indexed by a single exact integer,
arrays are indexed by multiple exact integers, one for
each dimension of the array.
Thus a 1-dimensional array, like a vector, is indexed
by a single number,
and a 2-dimensional array (a matrix) is indexed by
two numbers representing the row and column.

To avoid confusion, this guide uses the term "vector"
to refer only to Scheme vectors, never to 1-dimensional arrays.
The number of dimensions available is unlimited, although
the reference implementation is most efficient when handling
arrays with 4 dimensions or less.

In addition, whereas the smallest index of a Scheme vector
is always 0, the smallest index of each dimension of an array
can be any exact integer.  For example, a matrix may have 100
rows numbered from 1 to 100 and 11 columns numbered from -5 to +5.

It is possible to simulate arrays with vectors of vectors
(of vectors ...), but at considerable cost in efficiency.
By intention, the SRFI describes many array operations that can
be made especially efficient in both space and time.

The SRFI does not support *degenerate arrays*.  These come in two kinds.
An array with zero dimensions contains by convention a single element,
whereas an array where the lower bound of any dimension is greater than
or equal to the upper bound contains no elements at all.

When this guide refers to a 3 x 4 array (or similar),
that is shorthand for a 2-dimensional array with 3 rows and 4 columns,
with both lower bounds equal to 0.

The term *lexicographic order* represents a particular order of
traversing the elements of an array in which the first dimension
varies most slowly and the last dimension varies most quickly.
This is also known as *row-major order*, because if a matrix
is stored in memory with the elements of the first row first,
followed by the elements of the other rows in order,
then lexicographic order is equivalent to the order in memory.
For example, lexicographic order over a 2 x 3 matrix with both lower bounds 0
is as follows:  (0,0), (0,1), (0,2), (1,0), (1,1), (1, 2).

By the same token, *column-major order* varies the first dimension
most quickly and the last dimension most slowly.
Most programming languages use row-major order, with the
significant exceptions of Fortran (for historical reasons)
and languages oriented towards mathematical and scientific work
such as Matlab, Octave, S-Plus, R, Julia, and Scilab
(for compatibility with Fortran).

## Specialized and generalized arrays

The SRFI provides two (or three) kinds of arrays, specialized and generalized,
where generalized arrays can be immutable or mutable,
but it attempts to hide the differences between them as much as is practical.

A specialized array corresponds to arrays in other programming languages:
it is mutable and maintains a *storage object* which holds the values of the array.
A storage object is one-dimensional, and maps an exact integer from 0 (inclusive)
to the number of elements in its array (exclusive) to the value.  The mapping
between the multiple indices of the array and the single index of the storage
object is managed by the implementation.

Specialized arrays guarantee that
retrieving the value of a given array element
specified by its indices always retrieves
the same value unless a mutation is performed, in which case
retrieval of an element returns the value of the latest mutation
of that element.

More than one specialized array may in certain circumstances
share the same storage object.  In this case, a mutation performed on an
element of one array is typically (but not always) visible in all other
arrays that share its storage object.

Generalized arrays do not have storage objects maintained by the implementation.
Instead they are constructed using a getter procedure
that computes and returns the value of a specified array element.
This procedure may or may not return the same value over time in the way
that specialized arrays do.

For example, a getter that fetches the value of a particular row
and column of a relational database table will produce different results if the
table is mutated through some other process.  This sort of immutable array
is not truly immutable;
it is simply that it cannot be mutated through the SRFI's procedures.
Another example is an array whose elements are generated by an algorithm.
If this algorithm is pure and functional, the array is truly immutable.

It is also possible to create a generalized array with both a getter and a
setter procedure, in which case it is considered mutable.
Because of the ability to do arbitrary mutations, it
must be backed by mutable storage of some sort,
though it need not be as stereotyped as a storage object.

It is an error if a mutable generalized array does not provide
the same guarantees as a specialized array.
It is also an error if the setter procedure mutates any other elements of the array
than the one specified by its caller.

## Intervals, storage classes, and array constructors

In order to construct an array,
the lower and upper bounds of all its dimensions must be provided.
In the SRFI, these are represented using an immutable object
called a (multi-dimensional) *interval*,
which can be constructed using the following procedure:

`(make-interval `*lower-bounds upper-bounds*`)`

Returns an interval
whose lower bounds are specified by the vector *lower-bounds*
and whose corresponding upper bounds are specified by the vector *upper-bounds*.
For example, the interval for a two-dimensional array whose first dimension
(rows) ranges from 0 to 9 and whose second dimension ranges from -5 to +5, inclusive,
is created with `(make-interval #(0 -5) #(10 6))`.

Note that an interval can be used to construct more than one array.

In order to construct a specialized array, however,
it is also necessary to specify the *storage class*,
which represents the type of the storage object for an array.
The following standard storage classes are provided:

`generic-storage-class`

A storage class that allows any value to be stored, as a Scheme vector does.
However, a storage object belonging to
this class is not necessarily a Scheme vector.

`@-storage-class`

Storage classes that allow particular sets of numbers to be stored
The character `@` is shorthand for one of `u8`, `s8`, ... `c128`,
the twelve homogeneous-vector datatypes
of [SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html).
However, a storage object belonging to
one of these classes is not necessarily a SRFI 160 @vector.

The procedure `make-storage-class` can be used to create
a specialized kind of backing store.  For example, a hash table
along with a default value such as 0
can provide a compact backing store for sparse arrays.

With intervals and storage classes available, we can construct arrays:

`(make-specialized-array `*interval* [ *storage-class* [ *safe?* ] ]`)`

Returns a specialized array whose dimensions are specified by *interval*
using as its backing store an object of the class specified by *storage-class*
(`generic-storage-class` by default).

If the *safe?* argument is true, operations on the returned array will be checked
for being within *interval* in the sense of `interval-contains-multi-index`.
If it is false, operations are not checked.  If *safe* is omitted, a program-wide
value may have been set by `specialized-array-default-safe?`, but if not, the
safety is implementation-defined.

`(make-array `*interval getter* [ *setter* ]`)`

Returns a generalized array whose dimensions are specified by *interval*.
The *getter* procedure accepts array indices as multiple arguments
and computes and returns the value of an array element specified by those indices

The *setter* procedure accepts a new value
followed by array indices as multiple arguments
and changes the array element specified by those indices to the new value,
returning an unspecified value.
If *setter* is not provided, the generalized array is immutable.

## Predicates

The following predicates are useful for type dispatch:

`(interval? `*object*`)`

Return `#t` if *object* is an interval
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

The following special-purpose predicate
allows a program to check in advance
whether a particular sequence of indices is
usable with a particular array:

`(interval-contains-multi-index? `*interval index0 index1* ...`)`

Returns `#t` if the element specified by the *indexes*
is included in *interval*, and `#f` otherwise.
The interval associated with
an array can be obtained using the `array-domain` accessor.

## Individual array elements

There are no generic `array-ref` or `array-set!` procedures provided.
Instead, individual array elements can be retrieved or changed
using the `array-getter` and `array-setter` procedures, as follows:

`((array-getter `*array*`)` *index0 index1* ...`)`

Returns the value of *array* specified by the *indexes*.

`((array-setter `*array*`)` *newvalue index1 index2* ...`)`

Changes the value of array specified by the *indexes* to *newvalue*.

Note that specialized arrays have getters and setters just like generalized
arrays, although they are provided by the implementation rather than the user.

## Array transformations

An array transformation is a procedure that takes an array and returns another array,
perhaps with a different interval but the same array elements (though some of them may not be
visible in the new array).  Any mutation of one
array is also a mutation of the other.

The SRFI provides a set of *affine* transformations, meaning that the value
of each upper bound in the new array is equal to the sum of a subset
of the upper bounds of the old array plus a constant value,
and likewise for the lower bounds.
Because the composition of two affine transformations is itself affine,
extracting or mutating element values is just as efficient on an array with
no transformations applied as an array with five or fifty transformations applied.

Except as noted, these transformation procedures
accept any kind of array (specialized, generalized immutable,
or generalized mutable) and return an array of the same kind.

`(array-extract `*array interval*`)`

Returns an array that is a rectangular subset of *array*
whose bounds are specified by *interval*, like
a rectangular sub-part of a spreadsheet.
It is an error unless *interval* is a subset of the interval of *array*.

`(array-translate `*array translation*`)`

Returns an array with the same number of elements as *array*, but with the
bounds of each dimension shifted upward or downward.  The amount of shift
for each dimension is given by the corresponding element of *translation*,
a vector of exact integers.

The `translation?` predicate can be used to
verify that *translation* represents
a meaningful translation of array indices
(that is, if it is a vector where every element is an exact integer).

`(array-permute `*array permutation*`)`

Returns an array which has the same dimensions, bounds, and elements as *array*,
but with the dimensions in a different order specified by *permutation*.

The `permutation?` predicate can be used to
verify that *permutation* represents
a meaningful permutation of array indices
(that is, if each element is a distinct exact integer
in the range 0 (inclusive)
to the length of *permutation* (exclusive)).

`(array-curry `*array dimension*`)`

Returns an array of arrays.
Each of the inner arrays has the last *dimension* dimensions of *array*,
whereas the outer array has the earlier dimensions of *array*.
Thus currying a three-dimensional array specifying *dimension* as 1
gives a two-dimensional array made up of one-dimensional arrays with identical structure.

`(array-tile `*array tile-sizes*`)`

Returns an array of arrays.
Both the outer array and the inner arrays (the tiles)
have the same number of dimensions as *array*,
but the elements are distributed in the tiles in such a way that the difference
between the upper and the lower bound
of each dimension is equal to the corresponding element of the vector *tile-sizes*
(except near the upper bounds of the original array, where there aren't enough elements).

The bounds of the inner arrays preserve the original indices of each element in *array*.
The outer array has all lower bounds 0 and has the necessary upper bounds to represent
the total number of tiles required for each dimension.

For example, a 5 x 3 array tiled using an *tile-sizes* array of `#(2 1)` will
produce a 3 x 3 outer array containing six 2 x 1 arrays and three 1 x 1 arrays.
The nine inner arrays will tile the plane
represented by the original array, with each element in exactly one inner array.
The first two rows will contain three 2 x 1 arrays;
the last row will contain three 1 x 1 arrays.

It is an error unless
the *tile-siezes* argument has the same length as the number of dimensions of *array*.

Note: The routines `array-tile` and `array-curr`y both decompose an array into subarrays,
but in different ways.
For example, if `A` is a 10 x 10 array,
then `(array-tile A '#(1 10))` returns an 10 x 1 array
each element of which is a 1 x 10 array,
whereas `(array-curry A 1)` returns a 1-dimensional array of size 10
each element of which is likewise a 1-dimensional array of size 10.

`(array-reverse `*array flip?*`)`

Returns an array with the same dimensions, bounds, and elements as *array*,
except that a subset of the dimensions are reversed.  The `flip?` argument
is a vector of booleans: a dimension is reversed if the corresponding boolean
is true, and unreversed otherwise.

`(array-sample `*array scales*`)`

Returns an array which contains only every *k*th element along each of its
*n* dimensions, where *k* is equal to the *n*th element of *scales*.
It is an error unless *scales* is a vector of size *n* containing
positive exact integers.

For example, a 6 x 6 array scaled using `#(2 3)`
will be a 3 x 2 array that provides the first, third, and fifth rows
and the first and fourth columns of the original array.

`(specialized-array-share `*array interval interval-mapping*`)`

Constructs a new specialized array that shares the body of the specialized array *array*.
It is an error to apply this procedure to a generalized array.
Returns a specialized array whose elements are retrieved
as if by the following algorithm:
Pass the indices of the desired element to the procedure *interval-mapping*,
which accepts them and returns the same number of values
as the number of dimensions in *array*.
Then use the returned values to specify the element of *array* to be retrieved.

It is an error if *interval-mapping* is not affine and one-to-one,
which means that it is possible to precompute the possible results
of the transformation when the new array is created; after that,
*interval-mapping* is not invoked.

For example, the interval mapping specified by `(lambda (i) (values i i))`
will transform a square matrix into a 1-dimensional array accessing the diagonal
elements.

## The whole array

The following procedures accept a predicate and some arrays
and apply the predicate to each corresponding element of the arrays:

`(array-any `*pred array1 array2* ...`)`

Invokes *pred* on the corresponding elements of the *arrays* in lexicographic order.
As soon as a call to *pred* returns a true value, that value is returned by `array-any`.
If all calls return false, `array-any` returns false.

`(array-every `*pred array1 array2* ...`)`

Invokes *pred* on the corresponding elements of the *arrays* in lexicographic order.
As soon as a call to *pred* returns false, `array-any` returns false.
If all calls return a true value, the value of the last call is returned by `array-any`.
If no calls were made, `#t` is returned.

## Mapping and folding

`(array-map `*proc array1 array2* ...`)`

Returns an immutable generalized array
whose elements are the result of applying *proc*
to all the *arrays*, which must all have equivalent intervals.
Rather than doing the mapping all at once,
as `map` and `vector-map` do, elements are mapped
each time they are retrieved.

`(array-outer-product `*proc array1 array2*`)`

Returns an immutable generalized array
whose values are the result of applying
*proc* to each element of *array1* and each element of
*array2*.  The interval of the new array is the
interval of *array1* concatenated with *array2*.
Like `array-map`, `array-outer-product` computes
elements each time they are retrieved.

For example, if *proc* is `+` and the *arrays*
are both matrices, then the sum of the (1,2) element
of *array1* and the (3,5) element of *array2*
is placed in the (1,2,3,5) element of the result.
For another example, if *proc* is `*` and both
*array1* and *array2* contain 10 elements indexed
from 1 to 10 and containing values from 1 to 10,
then `outer-product` will return a multiplication table.

`(array-for-each `*proc array1 array2* ...`)`

Invokes `proc` for its side effects on the elements of
*arrays* in lexicographic order and returns an unspecified value.

`(array-fold `*kons knil array*`)`  
`(array-fold-right `*kons knil array*`)`

Performs a [SRFI 1](https://srfi.schemers.org/srfi-1/srfi-1.html)
`fold` or `right-fold` on the elements of *array* taken in lexicographic order.

## Mutators

These procedures allow bulk mutation of arrays.  The
arrays must have equivalent intervals.

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
of a newly created specialized array of the same dimensions
in lexicographic order.
The arguments *storage-class* and *safe?* have the same meaning
as in `make-specialized-array`.  Returns the newly created array.

`(array->list `*array*`)`

Returns a list containing the elements of *array* in lexicographic order.

`