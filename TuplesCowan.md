Tuples are persistent vectors layered on top of regular Scheme vectors.

Reference: [Henry Baker's paper on shallow binding](http://home.pipeline.com/~hbaker1/ShallowBinding.html).

Constructors: make-tuple, tuple, list->tuple, vector->tuple, vector->tuple/shared, tuple-copy; maybe tuple-append, tuple-append-subtuples, tuple-unfold, tuple-unfold-right, tuple-reverse-copy

Updater:  tuple-set returns the new tuple

Accessor: tuple-vector returns the underlying vector, which must not be mutated, and is invalidated by the next tuple-set

Convenience procedures passed to the vector: tuple-length, tuple-ref, tuple->list

SRFI 133 mutators:  tuple-swap, tuple-fill, maybe tuple-reverse

For other operations, use tuple-vector and [SRFI 133](http://srfi.schemers.org/srfi-133/srfi-133.html), but not the mutators.