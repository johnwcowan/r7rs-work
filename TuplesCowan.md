Tuples are persistent vectors layered on top of regular Scheme vectors.

Reference: [Henry Baker's paper on shallow binding](http://home.pipeline.com/~hbaker1/ShallowBinding.html).

Constructors: make-tuple, tuple, list->tuple, vector->tuple, vector-as-tuple.

Updater:  tuple-set, tuple-swap, tuple-fill; all return the new tuple.

Accessor: tuple-vector returns the underlying vector, which must not be mutated,
and is invalidated by the next tuple update operation.

Convenience procedures passed to the vector: tuple-length, tuple-ref, tuple->list

For other operations, use tuple-vector and [SRFI 133](http://srfi.schemers.org/srfi-133/srfi-133.html),
but not the mutators.