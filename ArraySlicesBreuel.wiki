This is not yet a formal proposal.

{{{
In pretty much all modern array languages, the array type is a descriptor
for the layout of shared storage, and this is reflected throughout the
entire design and API of the array facility.

Subscripting and slicing operations return new descriptors, not new storage.
Sharing rather than copying is a reasonable default because most array code
is written functional style anyway, and the copy operations are unnecessary
overhead (this is somewhat analogous to why lists in Scheme are usually both
shared and mutable).

The descriptors are general enough that they allow rectangular subarrays,
arbitrary permutations of the axes, and different strides as views of the
same data.   (Arbitrary element permutations or boolean masks are not
selected by the descriptors.)

With these kinds of descriptors, you can basically write most array
functions on the last or first axis and efficiently apply them to arbitrary
axes.

A slice is basically a descriptor without underlying storage.  Subscripting
an array with a slice creates a descriptor that associates the slice with

the storage.  There is actually an algebra of slices, so subscripting an
array with a slice basically composes the two slices to compute a new slice
that is then applied to the underlying storage.  (I don't think any of the
existing array languages express it quite that cleanly, but that's what's
going on abstractly... should probably write this up some time.)

The underlying storage for arrays is not made available as a separate
"simple array" type because there really is no need to; you can do
everything you want to do by using a 1D descriptor for that data.

In a sense, the fundamental data type in an array language is the slice, and
the array is kind of an afterthought, it's what happens when you associate a
slice with data via indexing.

(SLICE lo hi step) -> a 1D slice
(SLICE-AXIS s i) -> return the 1D slice representing axis i of slice s
(SLICE-PRODUCT s1 s2 ...) -> a cartesian product of slices
(SLICE-ASSOCIATE s v) -> associate a slice with a data vector, yielding an
array
(SLICE-COMPOSE s1 s2) -> compose two slices
(SLICE-OFFSET-LIST s) -> returns the list of offsets implied by the slice

(SLICE-TRANSPOSE s permutation) -> permutes the axes of the slice

(ARRAY-SLICE a) -> return the slice associated with an array
(ARRAY-DATA a) -> return the data vector associated with an array
(ARRAY->LIST a) -> returns the elements of the array in the order implied by
its slice

With these, ARRAY-REF becomes a special case of slicing (of course, it might
still be implemented more efficiently, but array subscripting with integers
is pretty rarely used anyway):

(ARRAY-INDEX a s) =
    (SLICE-ASSOCIATE (SLICE-COMPOSE (ARRAY-SLICE a) s) (ARRAY-DATA a)))

(ARRAY-REF a i1 i2 ...) =
    (CAR
        (ARRAY->LIST
            (ARRAY-INDEX a (SLICE-PRODUCT (SLICE i1 i1 1) (SLICE i2 i2 1)
...)))))

(ARRAY-TRANSPOSE a axes) =

    (SLICE-ASSOCIATE (SLICE-TRANSPOSE (ARRAY-SLICE a) axes) (ARRAY-DATA a)))

(ARRAY-REDUCE-LAST a f) -> returns a new array in which f is mapped over
slices representing the last axis

Functions like ARRAY-FILTER, ARRAY-PERMUTE, ARRAY-TAKE, etc. of course have
to return copies, since slices can't express the kind of complicated sharing
they would imply.

Just to be clear: the above isn't some complicated generalization from other
array languages, it's pretty much the standard functionality that is
available in many array languages.  (There are some details I have glossed
over related to array bounds etc.)
}}}

