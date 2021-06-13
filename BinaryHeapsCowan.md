## Binary heaps

Binary heaps are mutable collections that can contain any Scheme object provided
there exists a total ordering on the objects expressed by a
[SRFI 128](https://srfi.schemers.org/srfi-128/srfi-128.html) comparator.
They are intended to be a thin veneer over vectors.  Binary heaps are disjoint from other types of Scheme objects.

## Procedures

See [Pileup](http://nikodemus.github.io/pileup/), a Common Lisp implementation of heaps.
This SRFI will not support the special threading requirements, and of course
a Great Renaming will be performed.