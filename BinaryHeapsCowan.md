## Binary heaps

Binary heaps are mutable collections that can contain any Scheme object provided there exists a total ordering on the objects expressed by a [SRFI 114](http://srfi.schemers.org/srfi-114/srfi-114.html) comparator.  They are intended to be a thin veneer over vectors.  Binary heaps are disjoint from other types of Scheme objects.

## Procedures

### Constructors

`(heap `*comparator* *element*` ...)`

Returns a newly allocated heap ordered by *comparator*, and containing the *elements*.  This operation should be O(n) in the size of the heap.

### Predicates

`(heap? `*obj*`)`

Returns `#t` if *obj* is a heap, and `#f` otherwise.

`(heap-contains? `*heap* *element*`)`

Returns `#t` if *element* is a member of *heap* (in the sense of the comparator) and `#f` otherwise.

### Accessors

`(heap-min `*heap* *element*`)`

Returns the smallest element of the heap (in the sense of the comparator).  This operation should be O(log N) in the size of the heap.

### Mutators

`(heap-adjoin! `*heap* *element*`)`

Adds *element* to *heap*.  Returns an unspecified value.  This operation should be O(log N) in the size of the heap.

`(heap-pop! `*heap*`)`

Removes the smallest element of the heap (in the sense of the comparator) and returns it.  This operation should be O(log N) in the size of the heap.

`(heap-pop-adjoin! `*heap* *element*`)`

Removes the smallest element of the heap (in the sense of the comparator), pushes *element* on the heap, and returns the popped value.  This operation should be O(log N) in the size of the heap.

`(heap-adjoin-pop! `*heap* *element*`)`

Pushes *element* on the heap, then removes the smallest element of the heap (in the sense of the comparator) and returns it.  This operation should be O(log N) in the size of the heap.

`(heap-pop-elements! `*heap n*`)`

Returns a list containing the *n* smallest elements popped from *heap*.

### The whole heap

`(heap-copy `*heap*`)`

Returns a newly allocated heap containing the elements of *heap* with the same *<* procedure.

`(heap-size `*heap*`)`

Returns the number of elements in *heap*.

`(heap-map `*proc* *comparator* *heap*`)`

Applies *proc* to each element of *heap* in arbitrary order and returns a newly allocated heap ordered by *comparator* and containing the values of the applications.  This operation should be O(N) in the size of the heap.

`(heap-for-each `*proc* *heap*`)`

Applies *proc* to each element of *heap* in arbitrary order, discarding the returned values.  Returns an unspecified value.  This operation should be O(N) in the size of the heap.

### Conversion

`(heap->list `*heap*`)`

`(heap->vector `*heap*`)`

Returns a newly allocated list or vector containing the members of *heap* in arbitrary order.

`(list->heap `*comparator* *list*`)`

`(vector->heap `*comparator* *vector*`)`

Returns a newly allocated heap containing the elements of *list* or *vector* and ordered by *comparator*.

`(heap->sorted-list! `*heap*`)`

`(heap->sorted-vector! `*heap*`)`

Returns a newly allocated list or vector containing the members of *heap* in increasing order.  *Heap* will be destroyed in the process.

### Generator functions

`(heap->generator `*heap*`)`

Returns a generator that yields the elements of *heap* in increasing order, destroying *heap* in the process.

`(generator->heap `*comparator generator*`)`

Returns a heap built using *comparator* and the values of *generator*.

