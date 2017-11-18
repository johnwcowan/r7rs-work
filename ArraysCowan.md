## Specification

### Terminology

An *array* is an object of a disjoint type, a container with elements arranged according to a rectilinear coordinate system.  An array can have any number of dimensions or *axes*, including zero; this number is called the *rank* of the array.  Arrays of rank zero contain exactly one element.  Note that "rank" is a Fortran, Common Lisp, and APL term that has nothing to do with matrix ranks in the sense of linear algebra.

Each axis has an *extent* represented by two exact integers, the first representing the smallest possible coordinate for that axis, and the second representing the largest possible coordinate plus one.  *Extent* is also used, by a mild abuse of language, for the difference between the two values.  The smallest coordinates are collected into a Scheme vector known as the *lower bound* of the array; the largest coordinates are collected into another Scheme vector known as the *upper bound* of the array.  An *index* of the array is any Scheme vector of exact integers which has the same number of elements as the array's rank, and whose values lie between the lower bound (inclusive) and the upper bound (exclusive) of the corresponding axis.

An array can be a general array, meaning each element can be any Scheme object, or it can be a specialized array, meaning that each element can only belong to a given restricted type.  This is accomplished by separating array objects from the underlying *storage objects*, which can be Scheme vectors or numeric vectors or other objects.  Any object which can map a non-negative exact integer into an appropriate value can serve as a storage object by writing a *storage class* for it.  See [StorageClassesCowan](StorageClassesCowan.md) for the storage class API.  Note that if the extent of any axis is non-positive, the array has no elements.

In order to map an array's index (a Scheme vector of exact integers) into a *storage index* (a single exact integer), each array maintains another associated vector of exact integers, the *stride*, as well as a single additional exact integer, the *offset*.  Multiplying each element of the stride by the corresponding element of the array index, summing the results, and adding the offset produces the corresponding storage index.  Therefore, the offset is the storage index of the element whose array index consists of all zeros.

Procedures that accept an array object and return a new array sharing the same storage object but with different upper bound, lower bound, stride, and/or offset are known as *array transformations*, and this SRFI provides a number of them.  The SRFI also provides other procedures for operating on arrays, all of which have the property that they are meaningful no matter what the elements of the array are.  So `array-map` can be used to sum two matrices, since that is done element-wise over the `+` operation; but there is no operation provided for ordinary matrix multiplication, because it depends on the array elements being solely numeric.

In the same way that the names *start* and *end* are applied to optional numerical indexes that default to the smallest element of a sequence (list, vector, string, or whatever) and the largest element plus one, in this SRFI they default to the lower bound and the upper bound of an array.

In certain procedures, the elements of an array are processed in *lexicographic order*, also known as *row-major order*.  This means the order in which the highest-numbered axis changes most rapidly, and the other axes change only when the following axis returns to its lowest value.

### General requirements

Where a procedure that is passed to any procedure defined in this SRFI receives an index as an argument, it is an error for that procedure to mutate the index.

Note that array objects are immutable, but their storage objects are usually mutable.  It is possible to create arrays that are prohibited from mutating their storage objects.

The procedures of this SRFI that are not transformations may return arrays whose stride is implementation-dependent (so that the order of elements in the storage object may be either row-major or column-major), but the offset is always 0.

### Predicates

`(array? `*obj*`)`

Returns `#t` if *obj* is an array, and `#f` otherwise.

`(array-mutable? `*array*`)`

Returns `#t` if *array* can mutate its storage object, and `#f` otherwise.

### Constructors

`(make-array `*storage-class lower-bound upper-bound* [[|*fill* ]]`)`

Returns a newly allocated mutable array (with a newly allocated storage object of the specified storage-class) that has the specified bounds.  The values of the elements are unspecified if the *fill* argument is omitted.

`(array-tabulate `*proc storage-class lower-bound upper-bound mutable?*`)`

Returns a newly allocated array (with a newly allocated storage object of the specified storage-class) that has the specified bounds.  The values of the elements are computed by calling *proc* on every possible index of the array in lexicographic order.  If *mutable?* is true, the array can mutate its storage object.

`(array-broadcast `*array obj*`)`

Returns a newly allocated array whose bounds and storage class are the same as *array* and all of whose elements are *obj*.

### Metadata accessors

`(array-storage-class `*array*`)`

Returns the storage class with which *array* was created.

`(array-storage-object`*array*`)`

Returns the storage object underlying this array.  Note that this can be `#f` in the case of storage classes without actual storage.

`(array-rank `*array*`)`

Return the rank (number of dimensions) of *array*.

`(array-lower-bound `*array*`)`

Returns the index specifying the lower bound of *array*.  It is an error to mutate this index.

`(array-upper-bound `*array*`)`

Returns the index specifying the upper bound of *array*.  It is an error to mutate this index.

`(array-stride `*array*`)`

Return the stride of *array*.  It is an error to mutate this vector.

`(array-offset `*array*`)`

Returns the offset of *array*.

`(array-index->storage-index `*array index*`)`

Converts the *index* of *array* to the equivalent storage index.

### Accessors

`(array-ref `*array index*`)`

Returns the value of the element of *array* specified by *index*.  Note that this is different from the `array-ref` of most Lisp systems, which specifies the index as a sequence of arguments.

`(array-recursive-ref `*array index* ...`)`

Applies `array-ref` to the *array* using the first *index*.  If there are more arguments, apply `array-ref` to the result using the next *index*.  Repeat until there are no more indexes, returning the last result.  It is an error if any intermediate result is not an array.  (APL enlist.)

`(array-for-each `*proc array* [[|*start* [* ]] ]( *end)`)`

Iterates over the elements of *array* in lexicographic order, starting at the index *start* and ending just before the index *end*, and calling *proc* on each element.  Each invocation of *proc* receives the value of the element at that index.  The value returned by *proc* is discarded.

`(array-for-each-index `*proc array* [[|*start* [* ]] ]( *end)`)`

Iterates over the indexes of *array* in lexicographic order, starting at the index *start* and ending at the index *end*, and calling *proc* on each index.  The value returned by *proc* is discarded.

### Mutators

These procedures return an unspecified value.

`(array-set! `*array index value*`)`

Sets the value of the element of *array* specified by *index* to *value*.  Note that this is different from the `array-set!` of most Lisp systems, which specifies the index as a sequence of arguments.

`(array-tabulate! `*proc array* [[|*start* [* ]] ]( *end)`)`

Iterates over the indexes of *array* in lexicographical order starting at the index *start* (each dimension is inclusive) and ending at the index *end* (each dimension is exclusive), and calling *proc* on each index.  Whatever *proc* returns becomes the value of the array at the index.

## The whole array

`(array-map `*proc array* ...`)`

Returns a newly allocated array with the same bounds as the *arrays*; it is an error if they do not all have the same bounds.   For each valid index value, *proc* is invoked, passing each corresponding element of the *arrays*.  Whatever *proc* returns becomes the value of the storage element corresponding to that index in the result array.  The order of invocations of *proc* is not specified.

`(array-map! `*proc array* ...`)`

It is an error if the *arrays* do not all have the same bounds.  For each valid index value, *proc* is invoked, passing each corresponding element of the *arrays*.  Whatever *proc* returns becomes the value of the storage element corresponding to that index in the first specified array.  The order of invocations of *proc* is not specified.  Returns an undefined value.

`(array-fold `*proc seed array* ...`)`

Returns a newly allocated array with the same bounds as the *arrays*; it is an error if they do not all have the same bounds.   For each valid index value, *proc* is invoked in lexicographic order, passing each corresponding element of the *arrays* and a seed, whose initial value is *seed*.  *Proc* returns two values, the value of the storage element corresponding to that index in the result array, and the new value of the seed.

`(array-reduce `*proc array axis* [[|*n* ]]`)`

Returns a newly allocated array whose rank is one less than the rank of *array*, by combining all the groups of elements of length *n* (where the default is the extent of *axis*) along *axis* using *proc*, a two-argument procedure.  The order and number of invocations of *proc* is unspecified.  If there is only one such element, it is unchanged.  (APL reduce.)

`(array-cumulate `*proc array axis*`)`

Returns a newly allocated array whose bounds are the same as the bounds of *array*.  Each element along *axis* is constructed by reducing (as if by `array-reduce`) successive prefixes of the elements of *array* along that axis.  (APL scan.)

`(array-count `*pred array*`)`

Returns an exact integer containing the number of elements in *array* that satisfy *pred*.

`(array-index `*pred array*`)`

Returns the index of the first element of *array* in lexicographic order that satisfies *pred*.

`(array-compress `*array booleans axis*`)`

Returns an array with the same bounds as *array* except possibly along the *axis* dimension.  The array is sliced along *axis* and the elements of *booleans* (a vector of boolean values) are used to decide which slices to incorporate into the result: if the corresponding boolean is `#t`, the slice is incorporated, otherwise not.  (APL compress.)

`(array-expand `*array booleans nil axis*`)`

Returns an array with the same bounds as *array* except possibly along the *axis* dimension.  *Array* is sliced along *axis* and the elements of *booleans* (a vector of boolean values) are used to decide where, if anywhere, *nil* is to be interpolated: if the corresponding boolean is `#t`, *nil* is interpolated, otherwise the next slice is incorporated.  It is an error if the size of *booleans* is not to the extent of the *axis* dimension in the result.  It is also an error if *nil* does not have the same bounds as a slice. (APL expand.)

`(array-rearrange `*array vector axis*`)`

Returns an array with the same bounds as *array*.  *Array* is sliced along the *axis* dimension, and the slices are reassembled in the order given by *vector*.  The slice whose number appears in the first element of *vector* appears first in the result, and so on.  It is an error if *vector* is not a vector of exact integers.  (Generalized version of APL rotate.)

## Transformations

These procedures return arrays which share their storage object with the array argument.

`(array-transform `*proc array*`)`

The procedure *proc* specifies an affine function that returns an index of *array* when given an index of the returned array. The array does not retain a dependency on *proc*.  (SRFI 25 share-array.)

`(array-diagonal `*array*`)`

Returns a one-dimensional array which contains the diagonal elements of *array* (that is, the elements whose indices are all the same integer).

`(array-reshape `*lower-bound upper-bound array*`)`

Returns an array with the specified *bounds* whose elements in lexicographic order are the same as the elements of *array* in lexicographic order.  It is an error if there are too many or too few elements.  (APL reshape.)

`(array-restride `*stride offset array*`)`

Returns an array with the specified stride and offset whose elements in lexicographic order are the same as the elements of *array* in lexicographic order.  The strides and offset can be arbitrary, and no attempt is made to recalculate the bounds, so the result might be unusable or only partly usable.

`(array-reverse `*array axis*`)`

Returns an array with the same bounds as *array*, but whose elements on the specified *axis* are reversed.  (APL reverse.)

`(array-transpose `*array*`)`

Returns an array whose axes appear in the reverse order of the axes of *array*.  This implies that the upper and lower bound are the reverse of the bounds of *array*.  (APL monadic transpose.)

`(array-rearrange-axes `*array vector*`)`

Returns an array whose axes are an arbitrary permutation of the axes of *array*.  *Vector* specifies how to do the permutation: the axis whose number appears in the first element of *vector* appears as the first axis of the result, and so on.  (APL dyadic transpose with integer-valued vector.)

`(array-slice `*array start end*`)`

Returns a smaller array with the same rank as *array* whose elements begin at the "lower left" corner specified by *start* and end at the "upper right" corner specified by*end*.  Unlike *array-copy*, the result shares its storage object with *array*. (APL take and drop.)

`(array-squeeze `*array vector*`)`

Returns an array with the ranks specified by the elements of *vector* removed from *array*.  It is an error if the extents of the specified ranks are not equal to 1.  (!NumPy squeeze)

`(array-unsqueeze `*array rank*`)`

Returns an array whose rank is one greater than the rank of *array*.  This is accomplished by inserting a new axis numbered 'axis' whose extent is (0:1).  (!NumPy expand.)

## Copying and conversion

These procedures return arrays which do not share their storage objects with any existing arrays.

When these procedures return arrays, both the array and the underlying storage object are newly allocated

`(array-copy `*array mutable?* [[|*start* []] ]( *end*)`)`

Returns an array containing the elements of *array* starting at *start* (inclusive) and ending at *end* (exclusive).  The lower bound of the resulting array is all zeros, and the upper bound is determined by subtracting *start* from *end* element-wise.  The stride and offset are implementation-defined.  The resulting array is mutable if *mutable?* is true.  The returned array has the same storage class as *array*.

`(array-copy! `*to at from* [[|*start* []] ]( *end*)`)`

Copies the elements of array *from* from index *start* (inclusive) to index *end* (inclusive) onto array *to* starting at index *at*.  It is an error if there are not enough elements in *to* to make this possible.  The storage objects of *from* and *to* need not belong to the same storage classes.

`(array-append `*axis array* ...`)`

Returns a newly allocated array consisting of the *arrays* concatenated along *axis*.  It is an error unless the storage classes of the arrays are the same, and the result has the same storage class.  It is also an error unless the bounds of all the arrays are the same, with the possible exception of *axis*.  The *axis*th element of the lower bound of the result is 0; the corresponding element of the upper bound is the sum of the extents of the *arrays*.

`(array-repeat `*array axis repeat*`)`

Append *repeat* copies of *array* along axis *axis*, as if by `array-append`.  (Variant of NumPy repeat.)

`(array-reclassify `*array storage-class*`)`

Returns an array with the same bounds and elements as *array*, but whose storage class is specified by *storage-class*.

`(array->nested-vector `*array*`)`

`(array->nested-list `*array*`)`

Returns a newly allocated vector/list whose elements are also newly constructed vectors/lists, and so on as far down as necessary to cover every axis of the array.  Bottom-level Scheme vectors/lists contain the elements of *array*.  Thus, if *array* has rank 1, the result is a vector/list; if the array has rank 2, the result is a vector/list whose elements are vectors/lists, and so on.  As a special case, if *array* has rank 0, the sole element is returned.

`(nested-vector->array `*nested-vector storage-class rank*`)`

`(nested-list->array `*nested-list storage-class rank*`)`

Returns a newly allocated array of the *storage-class* with rank *rank* whose elements are initialized from the vector *nested-vector* or list *nested-list*.  It is an error if this argument is not rectangular.  As a special case, if *rank* is 0, the sole element is *nested-vector* or *nested-list*, which in that case need not be a Scheme vector/list.

## Inner and outer products

These procedures return arrays which do not share their storage objects with any existing arrays.

`(array-inner-product `*storage-class proc1 proc2 array1 array2*`)`

Returns a newly allocated array using the *storage-class* whose bounds are equal to the bounds of *array1* without their last elements, concatenated with the bounds of *array2* without their first elements.  It is an error if the omitted upper bounds are not numerically equal; it is also an error if the omitted lower bounds are not numerically equal.  It is an error if both arrays have rank 0.

Each element of the result array results from applying *proc2* to the corresponding elements of the last vectors of *array1* and the first vectors of *array2* and then reducing them with *proc1* to a single value.  The order and number of invocations of the procedures is unspecified.

In particular, if both arrays have rank 1, the last and first vectors are the whole of the arrays, and the result has rank 0; if both arrays have rank 2, the last vectors of *array1* are the column-wise vectors, and the first vectors of *array2* are the row-wise vectors, and the result has rank 2.  (APL inner product.)

Example:  `(array-inner-product vector-storage-class + * array1 array2)`, where the arrays have rank 1, computes the usual dot product of two vectors.

`(array-outer-product `*storage-class proc array1 array2*`)`

Returns a newly allocated array using the *storage-class* whose bounds are the concatenation of the bounds of *array1* and *array2*.  Each element of the new array is the result of applying *proc* to every element of *array1* and every element of *array2*.  The order and number of invocations of *proc* is unspecified.  (APL outer product.)

## Input/output

The external representation of an array consists of a `#` character, followed by a sequence of digits indicating the rank of the array, followed by the letter `a`, followed by a coded representation of the storage class, all with no intervening whitespace.  This prefix is followed, after optional whitespace, by the representation of a nested list produced as if by `array->nested-list`.  The prefix is interpreted case-insensitively.

Standard numeric storage classes are encoded by using the first few characters of the name of the storage class.  Thus, the representation of an array of rank 2 using `u32-storage-class` begins with `#2au32`.  Other storage classes, including `vector-storage-class`, `sparse-storage-class`, and user-created storage classes, are encoded using the empty string.

`(array-read ` [[|*input-port* ]]`)`

Reads the external representation of an array from *input-port* (the current input port if *input-port* is not specified) and returns the corresponding array.  If the coded representation of the storage class is not recognized, `vector-storage-class` is used; this permits the addition of new coded storage classes in a backward compatible way.

`(array-write `*array* [[|*stream* ]]`)`

Writes the external representation of *array* from *output-port* (the current output port if *output-port* is not specified) and returns an unspecified value.

This SRFI recommends, but does not require, that the standard Scheme procedures `read`, `write`, and `display` be extended to deal with external representations of arrays.  On R7RS systems, if `read` accepts the external representation of arrays, it must also be allowed in Scheme code, in which case array constants are self-quoting.




