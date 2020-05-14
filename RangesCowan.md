# Ranges

Ranges are immutable
collections that can be enumerated but are represented algorithmically
rather than by a per-element data structure.

A range is specified by a lower bound (which can be of any type),
a length (number of elements), a
comparator, and an indexer function that maps the lower bound and a
non-negative exact integer less than the length
into a value of the range.

A numeric range is a special case of a range specified by 
an inclusive lower bound (default 0), an exclusive upper bound,
and a step value (default 1), all of which can be exact or inexact real numbers.
Its comparator is the natural comparator on real numbers, and its indexer is
`(lambda (bound n) (+ bound (* n step)))`.

## Constructors

`(range `*comparator lower-bound length indexer*`)`

Returns a range with the given parameters (see above).

`(numeric-range `*start end* [*step*]`)`

Returns a numeric range with the given parameters (see above).
If the step argument is omitted, it is 1.

## Predicates

`(range? `*obj*`)`

Returns `#t` if *obj* is a range and `#f` otherwise.

`(range-contains? `*range value*`)`

Returns true if value is an element of *range*.

`(range-includes? `*range value*`)`

Returns true if value is 
greater than or equal to the range start
and less than or equal to the range end,
whether or not it is an element of the range.

`(range-empty? `*range*`)`

Returns true if *range* is empty.

## Accessors

`(range-element-comparator `*range*`)`

Returns the comparator of *range*.

`(range-length `*range*`)`

Returns the length (number of elements) of *range*.

`(range-indexer `*range*`)`

Returns the indexer of *range*.

`(range-ref `*range index*`)`

Returns the *index*th element of *range*.  It is an
error if index is less than 0 or greater than 
or equal to the length of *range*.

`(range-start `*range*`)`

Shorthand for `(range-ref `*range* `0)`

`(range-end `*range*`)`

Shorthand for `(range-ref `*range* `(- (range-length `*range*`) 1))`


## Iteration

`(range-split-at `*range index*`)`

Returns two values which are ranges.  The first value
contains all elements of *range* from the zeroth element
to the *index*th element exclusive.  The second value contains
all elements of *range* from the *index*th element inclusive
to the last element.

`(range-take `*range count*`)`

Returns a range which contains the first *count* elements of *range*.

`(range-take-right `*range count*`)`

Returns a range which contains the last *count* elements of *range*.

`(range-drop `*range count*`)`

Returns a range which contains all except the first *count* elements
of *range*.

`(range-drop-right `*range count*`)`

Returns a range which contains all except the last *count* elements of
range.

`(range-count `*pred range*`)`

Returns the number of elements of *range* which satisfy *pred*.

`(range-any `*pred range*`)`

Returns true if any of the elements of *range* satisfy *pred*.
Specifically it returns the last value returned by *pred*.
Otherwise, `#f` is returned.

`(range-every `*pred range*`)`

Returns true if all the elements of *range* satisfy *pred*,
specifically it returns the last value returned by *pred* or `#t` if
*pred* was never invoked.  Otherwise, `#f` is returned.

`(range-map->list `*proc range*`)`

Returns a list of the results of applying *proc* to each element
of *range* in order.  However, the order in which *proc* is applied to the elements
is unspecified.

`(range-for-each `*proc range*`)`

Applies *proc* to each element of *range* in order.
Returns an unspecified result.

`(range-filter->list `*pred range*`)`

`(range-remove->list `*pred range*`)`

Returns a list containing the elements of *range* that
satisfy / do not satisfy *pred*.

`(range-fold `*range proc nil*`)`

Invokes *proc* on each member of *range* in order, passing the result of
the previous invocation as a second argument. For the first invocation,
*nil* is used as the second argument. Returns the result of the last
invocation, or *nil* if there was no invocation.

`(range-fold-right `*range proc nil*`)`

Invokes proc on each member of *range* in reverse order, passing the result of
the previous invocation as a second argument. For the first invocation,
*nil* is used as the second argument. Returns the result of the last
invocation, or *nil* if there was no invocation.

`(range-reverse `*range*`)`

Returns a range which contains the same elements as *range*, but in reverse order.


## Searching

`(range-index `*pred range*`)`

Returns the index of the first element of *range* that satisfies *pred*,
or `#f` if there is none.

`(range-index-right `*pred range*`)`

Returns the index of the last element of *range* that satisfies *pred*,
or `#f` if there is none.

`(range-take-while `*pred range*`)`

Returns a range containing the elements of *range* that
satisfy *pred* up to the first one that does not.

`(range-take-while-right `*pred range*`)`

Returns a range containing the elements of *range* that
satisfy *pred* up to the first one that does not,
considered from the end to the beginning.

`(range-drop-while pred `*range*`)`

Returns a range that omits elements of *range* that satisfy
*pred* until the first one that does not.

`(range-drop-while-right `*pred range*`)`

Returns a range the omits the last elements of *range* that
satisfy *pred* until the last one that does not.

## Conversion

`(range->list `*range*`)`

Returns a list containing the elements of *range* in order.

`(range->generator `*range*`)`

Returns a SRFI 158 generator that generates the elements of *range* in order.


