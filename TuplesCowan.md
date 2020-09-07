Tuples are persistent vectors layered on top of heterogeneous or homogeneous vectors.

Reference: [Henry Baker's paper on shallow binding](http://home.pipeline.com/~hbaker1/ShallowBinding.html).

Constructors: make-tuple, tuple, list->tuple, vector->tuple, vector-as-tuple.

Updater:  tuple-set, tuple-swap, tuple-fill, tuple-transfer (i.e. vector-copy!);
all return the new tuple.

Accessor: tuple-as-vector returns the underlying vector, which must not be mutated,
and is invalidated by the next tuple update operation.

Convenience procedures passed to the vector: tuple-length, tuple-ref, tuple->list

For other operations, use tuple-as-vector and
[SRFI 133](http://srfi.schemers.org/srfi-133/srfi-133.html),
but not the mutators, with caution.

Implementation:  A tuple is a record with: storage, storage-class, index, value, parent.

Note: Allow homogeneous tuples to be non-disjoint with
[SRFI 196](http://srfi.schemers.org/srfi-196/srfi-196.html) ranges.
