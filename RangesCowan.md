# Ranges

Ranges are
collections that can be enumerated but are represented algorithmically
rather than by a per-element data structure.

A range is specified by a lower bound, which can be of any type,
a length (number of elements), a
comparator, and an indexer function that maps the lower bound and a
non-negative exact integer less than the length
into a value of the range.

A numeric range is a special case of a range specified by 
an inclusive lower bound (default 0), an exclusive upper bound,
and a step value (default 1), all of which can be exact or inexact real numbers.
Its comparator is the natural comparator on real numbers, and its indexer is
`(lambda (bound n) (+ bound (* n step)))`.

## Terminology

In arguments, *range* means any range.

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

`(range-empty? `*range*`)`

Returns true if *range* is empty.

## Accessors

`(range-start `*range*`)`

`(range-end `*range*`)`

Returns the upper and lower bounds of *range*.

`(range-element-comparator `*range*`)`

Returns the comparator of *range*.

`(range-length `*range*`)`

Returns the length (number of elements) of *range*.

`(range-indexer `*range*`)`

Returns the indexer of *range*.

`(range-ref `*range index*`)`

Returns the indexth element of *range*.  It is an
error if index is less than 0 or greater than 
or equal to the length of *range*.

## Iteration

`(range-split-at `*range index*`)`

Returns two values which are ranges.  The first value
contains all elements of *range* from the zeroth element
to the indexth element exclusive.  The second value contains
all elements of *range* from the indexth element inclusive
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
Specifically it returns the last value returned by *pred* or `#t` if
*pred* was never invoked.  Otherwise, `#f` is returned.

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

Returns the index of the first element of *range* that satisfies *pred*.

`(range-index-right `*pred range*`)`

Returns the index of the last element of *range* that satisfies *pred*.

`(range-take-while `*pred range*`)`

Returns a range containing the elements of *range* that
satisfy *pred* up to the first one that does not.

`(range-take-while-right `*pred range*`)`

Returns a range containing the elements of *range* from the
last one that satisfies *pred* up to the end.

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

`(range-consolidate `*range-list*`)`

Consolidates a list of ranges into as few ranges as possible.
It is an error if any of the ranges have different comparators.
Consecutive and overlapping ranges are merged, and all the resulting
ranges are sorted into increasing order in the sense of the
shared comparator.

## Interval relations

Let sA be the lower bound (start) of range A,
and eA be the value of the upper bound (end) of range A;
and likewise for range B.

`(range-congruent? `*rangea rangeb*`)`

Two ranges are congruent iff sA = sB and eA = eB.

`(range-encloses? `*rangea rangeb*`)`

Range A encloses range B iff they are not congruent, sA <= sB, and eA >= eB.
This relation can be exhaustively partitioned into three sub-relations:

`(range-strictly-encloses? `*rangea rangeb*`)`

Range A strictly encloses range B iff sA < sB and eA > eB.

`(range-encloses-with-suffix? `*rangea rangeb*`)`

Range A encloses range B with a suffix iff sA = sB and eA > eB.

`(range-encloses-with-prefix? `*rangea rangeb*`)`

Range A encloses range B with a prefix iff sA < sB and eA = eB.

`(range-fits-within? `*rangea rangeb*`)`

Range A fits within range B iff they are not congruent, sA >= sB, and eA <= eB.
This relation can be exhaustively partitioned into three sub-relations:

`(range-strictly-fits-within? `*rangea rangeb*`)`

Range A fits strictly within range B iff sA > sB and eA < eB.

`(range-prefix? `*rangea rangeb*`)`

Range A is a prefix of range B iff sA = sB and eA < eB.

`(range-suffix? `*rangea rangeb*`)`

Range A is a suffix of range B iff sA > sB and eA = eB.

`(range-overlaps? `*rangea rangeb*`)`

Range A and range B overlap iff either sA or eA (but not both) is greater than sB and less than eB.
This relation can be exhaustively partitioned into two sub-relations:

`(range-overlap-start? `*rangea rangeb*`)`

If sA < sB < eA < eB, then range A overlaps the start of range B.

`(range-overlap-end? `*rangea rangeb*`)`

If sB < sA < eB < eA, then range A overlaps the end of range B.

`(range-precedes? `*rangea rangeb*`)`

Range A precedes range B iff eA <= sB.
This relation can be exhaustively partitioned into two sub-relations:

`(range-strictly-precedes? `*rangea rangeb*`)`

Range A strictly precedes range B iff eA < sB.

`(range-immediately-precedes? `*rangea rangeb*`)`

Range A immediately precedes range B iff eA = sB.

`(range-follows? `*rangea rangeb*`)`

Range A follows range B iff sA >= eB.
This relation can be exhaustively partitioned into two sub-relations:

`(range-strictly-follows? `*rangea rangeb*`)`

Range A strictly follows range B iff sA < eB.

`(range-immediately-follows? `*rangea rangeb*`)`

Range A immediately follows range B iff sA = eB.

In all these procedures it is an error if the ranges don't have the same comparators.
