## Abstract

IN PROGRESS

This is a base library for [SRFI 231](https://srfi.schemers.org/srfi-231/srfi-231.html).

## Rationale ##

This library is intended to be part of the R7RS Foundations, along with an appropriate
lexical syntax such as
[Common Lisp](http://www.lispworks.com/documentation/lw50/CLHS/Body/02_dhl.htm),
[SRFI 58](https://srfi.schemers.org/srfi-58/srfi-58.html), or
[SRFI 163](https://srfi.schemers.org/srfi-163/srfi-163.html).  SRFI 231 is too large
to live in the Foundations and belongs in the Batteries, so this SRFI provides a bare minimum
subset.  It is downward compatible with SRFI 231, and is analogous
to the `(srfi 160 base)` library described in
[SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html).
It also provides some of the multidimensional facilities of 
[CL arrays](http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_the_arrays_dictionary.html).  The small [Guile array library](https://www.gnu.org/software/guile/manual/html_node/Array-Procedures.html) was also helpful in defining the scope of this library.

## Specification

### Intervals

An interval is an immutable object specifying the upper and lower bounds of the dimensions of an array.

`(make-interval `*arg1* [ *arg2* ]`)`

Returns an interval specifying the shape of an array.  If *arg2* is not present, then the
upper bounds are specified by the vector *arg1* and the lower bounds are all zero.  If *arg2*
is present, then the vector *arg1* specifies the lower bounds and the vector *arg2* specifies
the upper bounds.  It is an error if *arg1* and *arg2* are different lengths.

`(interval-lower-bounds->vector `*interval*`)`  
`(interval-upper-bounds->vector `*interval*`)`

Returns the lower/upper bounds vectors of *interval*.  It is an error to mutate the result.

`(interval-volume `*interval*`)`

Computes the difference between the elements of the upper and lower bounds of *interval*, and
returns their product.

`(interval-contains-multi-index? `*interval . multi-index*`)`

Returns `#t` if the indices specified by the second through the last arguments
refer to a location in the shape specified by *interval*.

`(interval-for-each `*f interval*`)`

Iterates over all possible indices in *interval* and invokes *f* (which must
take the same number of arguments as the length of the lower bounds of *interval*)
on each possibility in row-major order.

### Storage classes

A storage class is an opaque object (for the purposes of this SRFI)
that specifies the range of objects that can be contained in an array.
The global variable `generic-storage-class` is used to create heterogeneous
arrays: the others are used to create homogeneous arrays of characters,
signed integers, unsigned integers (including bits), floats, and complex floats.

Variable: `generic-storage-class`  
Variable: `char-storage-class`  
Variable: `s8-storage-class `  
Variable: `s16-storage-class`  
Variable: `s32-storage-class`  
Variable: `s64-storage-class`  
Variable: `u1-storage-class`  
Variable: `u8-storage-class`  
Variable: `u16-storage-class`  
Variable: `u32-storage-class`  
Variable: `u64-storage-class`  
Variable: `f8-storage-class`  
Variable: `f16-storage-class`  
Variable: `f32-storage-class`  
Variable: `f64-storage-class `  
Variable: `c64-storage-class`  
Variable: `c128-storage-class`

### Constructors

`(make-specialized-array `*interval [ storage-class [ initial-value [ `#t` ] ] ]

Returns an array whose dimensions are specified by *interval*, whose range of values
is specified by *storage-class*, and whose initial value is specified by *initial-value.
The fourth argument, if present, must be `#t`, indicating that the array is safe:
that is, an error is signaled if the indices are outside the range of *interval*.
Unsafe arrays are not supported by this SRFI.


### Predicates

`(specialized-array? `*obj*`)`

Returns `#t` if *obj* is a specialized array and `#f` otherwise.

### Accessors

`(array-storage-class `*array*`)`

Returns the storage class with which *array* was created.

`(array-domain `*array*`)`

Returns the domain (i.e. the interval object) with which *array* was created.

`(array-dimension `*array*`)`

Returns the number of dimensions of *array*.

`(array-ref `*array . multi-index*`)`

Returns the value stored in the element of *array* specified by the indices of *multi-index*.
It is an error if *multi-index does not specify an element of *array*.
### Mutators

`(array-set! `*array object . multi-index*`)`

Sets the value of *array* specified by the indices of *multi-index* to *object*.  It is an error
if *object* does not belong to the storage class of *array*.  It is also an error
if *multi-index does not specify an element of *array*.

### Conversion

`(array->list `*list*`)`

Returns a newly allocated list of the elements of *array* in row-major order.

`(list->array `*interval list [ storage-class [ mutable? [ `#t` ] ] ]*`)`

Returns an array created as if by `make-specialized-array` with arguments
*interval*, *storage-class* and *mutable?* and populated by the elements of *list* in
row-major order.

`(list*->array `*dimensions nested-list [ storage-class [ mutable? [ `#t` ] ]*`)`

Returns an array with *dimensions* dimensions,
created as if by `make-specialized-array` with arguments *storage-class*
and *mutable*.  However, *nested-list* specifies not only the contents of
the array but its upper bounds (the lower bounds are all zero).

`(array->list* `*array*`)

Returns a nested list containing the elements of *array*.

### Lexical syntax

All the lexical syntaxes take the form of `#`*dims* followed by a prefix
followed by a nested list.  *Dims* specifies the number of dimensions of the array.
The reason that *dims* must be present
is that whereas `#2A((1  2) ( 3 4) (5 6))` is a 2 x 3 2-dimensional array,
`#1A((1 2) (3 4) (5 6))` is a 1-dimensional array with three elements, each of which
is a list of two elements.  The prefixes look like this:

In Common Lisp lexical syntax, the prefix takes the form `A`.
The only storage class is the `general-storage-clsas`, and the
lower bounds cannot be specified.

In SRFI 58 lexical syntax, the prefix takes the form `A`*bounds*`:`*tag*. where
*bounds* takes the form *dim*, *dim*`*`*dim*, ... repreesenting the upper bounds;
where the lower bounds are all zero. *Tag* represents the storage class,
and is optional, in which case the `generic-storage-class` is intended.

In SRFI 163 lexical syntax, the prefix takes the form *tag*`@`*bounds*, where
each *bound* takes the form *lower*`:`*length*, specifying the lower bound
and the length of the dimension, or else the form *lower*, where the length
of the dimension is implicit from the nested list.  Bounds are separated by`@`;
if there are no bounds, the initial `@` is omitted.
The tag for a `generic-storage-class` array is `A`.
