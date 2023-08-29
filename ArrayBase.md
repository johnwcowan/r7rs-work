## Abstract

IN PROGRESS

This is a base library for [SRFI 231](https://srfi.schemers.org/srfi-231/srfi-231.html).

## Rationale ##

This library is intended to be part of the R7RS Foundations, along with an appropriate
lexical syntax such as [SRFI 58](https://srfi.schemers.org/srfi-58/srfi-58.html) or
[SRFI 163](https://srfi.schemers.org/srfi-163/srfi-163.html).  SRFI 231 is too large
to live in the Foundations, and belongs in the Batteries, so this SRFI provides a bare minimum
subset.  It is downward compatible with SRFI 231, and is analogous
to the `(srfi 160 base)` library described in
[SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html).
It also provides some of the multidimensional facilities of 
[CL arrays](http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_the_arrays_dictionary.html).

## Specification

160 base: make-@vector, @vector, @vector?, @vector-length,
@vector-ref, @vector-set!, @vector->list, list->@vector

CL: make-array, arrayp,  aref, (setf aref), array-dimension, array-dimensions, array-element-type,
array-in-bounds-p, array-rank, array-row-major-index, array-total-size, arrayp, row-major-aref

Guile: array?, typed-array?, make-array, make-typed-array, list->array, list->typed-array,
array-ref, array-in-bounds, array-set!, array-shape, array-dimensions, array-length, array-rank,
array->list, array-copy, array-copy!, array-copy-in-order!, array-fill!, array-equal?,
array-map!, array-map-in-order!, array-for-each, array-index-map!

### Intervals

An interval is an object specifying the upper and lower bounds of the dimensions of an array

make-interval arg1 #!optional arg2  
interval-lower-bounds->vector interval  
interval-upper-bounds->vector interval  
interval-contains-multi-index? interval . multi-index

### Storage classes

A storage class is an opaque object (for the purposes of this SRFI)
that specifies the range of objects that can be contained in an array.
The global variable `generic-storage-class` is used to create heterogeneous
arrays: the others are used to create homogeneous arrays:

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

make-specialized-array interval [ storage-class [ initial-value [ #t ] ] ]

### Predicates

specialized-array? obj

### Accessors

array-storage-class array  
array-indexer array  
array-ref array . multi-index

### Mutators

array-set! array object . multi-index


