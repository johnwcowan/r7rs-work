## Cycle type

Cycles are an immutable ordered, but unindexed, container type similar to circular lists.  Unlike lists, however, cycles are fully bidirectional, so many of the procedures are provided in forward and reversed pairs.

### Constructors and type conversion ===

`(cycle `*element* ...`)`

Returns a cycle containing *elements*.  Order is preserved.

`(list->cycle `*list*`)`

`(list->cycle/reverse `*list*`)`

Returns a cycle whose elements are the elements of *list*.  Order is preserved (reversed).

`(cycle->list `*cycle*`)`

`(reversed-cycle->list `*cycle*`)`

Returns a list whose elements are those of *cycle*.  Order is preserved (reversed).

`(cycle-unfold `*continue? successor mapper seed*`)`

`(cycle-unfold/reverse `*stop? successor mapper seed*`)`

Start with an empty list.  If the result of applying the predicate *stop?* to *seed* is true, apply `make-cycle` to the list and return the result.  (The list need not actually be created.)

Otherwise, apply the procedure *mapper* to *seed*.  The value of *mapper* is prepended onto the list.  Then get a new seed by applying the procedure *successor* to *seed*, and repeat this algorithm.  Convert the list to a cycle in forward (reverse) order and return the cycle.

### Predicates

`(cycle? `*obj*`)`

Returns `#t` if *obj* is a cycle, and otherwise returns `#f`.

`(cycle-empty? `*obj*`)`

Returns `#t` if *obj* is an empty cycle, and otherwise returns `#f`.

`(cycle=? `*equivalence cycle,,1,, cycle,,2,,*`)`

Return `#t` if *cycle,,1,,* and *cycle,,2,,* contain the same values (in the sense of the *equivalence* predicate) in the same order, independent of their rotations; otherwise return `#f`.

Example:  `(cycle=? eqv? (cycle 1 2 3) (3 1 2)) => t`

### Accessors

`(cycle-front `*cycle*`)`

Returns the front element of *cycle*.

`(cycle-back`*cycle*`)`

Returns the back element of *cycle*.

`(cycle-take `*cycle k*`)`

`(cycle-take/reverse `*cycle k*`)`

Returns a cycle containing the first *k* elements of *cycle* in forward (reverse) order.

`(cycle-drop `*cycle k*`)`

`(cycle-drop/reverse `*cycle k*`)`

Returns a cycle containing all but the last *k* elements of *cycle* in forward (reverse) order.

`(cycle-split-at`*cycle k*`)`

`(cycle-split-at/reverse`*cycle k*`)`

Returns two values, both cycles, containing the first *k* elements of *cycle* in forward (reverse) order and containing all but the last *k* elements of *cycle* in forward (reverse) order.

### Rotation

`(cycle-step `*cycle*`)`

`(cycle-step/reverse `*cycle*`)`

Returns a cycle obtained from *cycle* by a rotation of a single step forward (backward).

`(cycle-rotate `*cycle k*`)`

`(cycle-rotate/reverse `*cycle k*`)`

Returns a cycle obtained from *cycle* by a rotation of *k* steps forward (backward), where *k* is an exact non-negative integer.

`(cycle-rotate-while `*cycle predicate*`)`

`(cycle-rotate-while/reverse `*cycle predicate*`)`

Returns two values: a cycle obtained from *cycle* by a forward (backward) rotation of as many steps as possible while the value of `cycle-front` satisfies `predicate`, and the number of steps.

`(cycle-rotate-until `*cycle predicate*`)`

`(cycle-rotate-until/reverse `*cycle predicate*`)`

Returns two values: a cycle obtained from *cycle* by a forward (backward) rotation of as few steps as possible until the value of `cycle-back` satisfies `predicate`, and the number of steps.

### The whole cycle

`(cycle-length `*cycle*`)`

Returns the number of elements in *cycle*.

`(cycle-reverse `*cycle*`)`

Return a cycle containing the same elements as this cycle but in reverse order.  Navigating a reversed cycle forward is the same as navigating the original cycle backward.

`(cycle-count `*cycle predicate*`)`

Returns the number of elements of *cycle* which satisfy *predicate*.

`(cycle-append `*cycle ...*`)`

`(cycle-append/reverse `*cycle ...*`)`

Returns a cycle containing all the elements of *cycles* in the order given, each in forward (reverse) order.  Note that `cycle-append/reverse` is not the same as appending the *cycles* and reversing the result.

`(cycle-zip `*stop? cycle* ...`)`

Returns a cycle of lists (not cycles) which contain the respective elements of each *cycle*.  The predicate *stop?* is invoked on each such list before it is added to the result, and when it returns true, the procedure terminates.

### Mapping and folding on elements

`(cycle-map `*proc n cycle* ...`)`

`(cycle-map/reverse `*proc n cycle* ...`)`

It is an error unless *proc* is a procedure taking as many arguments as there are *cycles* and returning a single value. `cycle-map` applies *proc* to the elements of the cycle(s) in forward (reverse) order *n* times and returns a cycle of the corresponding results.

`(cycle-for-each `*proc n cycle* ...`)`

`(cycle-for-each/reverse `*proc n cycle* ...`)`

It is an error unless *proc* is a procedure taking as many arguments as there are *cycles*. `cycle-for-each` applies *proc* to the elements of the cycle(s)  in forward (reverse) order *n* times and discards any results.  Returns an unspecified value.

`(cycle-fold `*proc nil n cycle* ...`)`

`(cycle-fold/reverse `*proc nil n cycle* ...`)`

It is an error unless *proc* is a procedure taking as many arguments as there are *cycles*, plus one additional argument, and returning a single value. `cycle-fold` applies *proc* *n* times to the elements of the cycle(s) in forward (reverse) order and the value previously returned by *proc*.  On the first call to *proc*, the additional argument is *nil*.  Returns the result of the final call to *proc*.

## Filtering and partitioning

`(cycle-filter `*cycle predicate*`)`

Returns a cycle containing those elements which satisfy *predicate*.  Order is preserved.

`(cycle-remove `*cycle predicate*`)`

Returns a cycle containing those elements which do not satisfy *predicate*.  Order is preserved.

`(cycle-partition `*cycle predicate*`)`

Returns two values, a cycle containing those elements which satisfy *predicate*, and another cycle containing those elements which do not.  Order is preserved.

## Searching

`(cycle-any `*cycle predicate*`)`

If any element of *cycle* satisfies *predicate*, the result of *predicate* is returned, and `#f` otherwise.

`(cycle-every `*cycle predicate*`)`

If any element of *cycle* does not satisfy *predicate*, `#f` is returned, and `#t` otherwise.

`(cycle-find `*cycle predicate*`)`

`(cycle-find/reverse `*cycle predicate*`)`

Returns the first element of *cycle* that satisfies *predicate*, searching in forward (reverse) order, and `#f` if there is none.  Note that it is not possible to use these procedures to determine if a cycle contains `#f`.

`(cycle-take-while `*cycle pred*`)`

`(cycle-take-while/reverse `*cycle pred*`)`

Returns a list containing the first (last) elements of *cycle* that satisfy *pred*.

`(cycle-drop-while `*cycle pred*`)`

`(cycle-drop-while/reverse `*cycle pred*`)`

Returns a list containing all but the first (last) elements of *cycle* that satisfy *pred*.
