Ranges

Ranges come in two types, definite and indefinite.  Definite ranges are
collections that can be enumerated but are represented algorithmically
rather than by a per-element data structure.  Indefinite ranges are just defined
portions of an ordered domain.

A definite range is specified by a lower bound, which can be of any type,
a length (number of elements), a
comparator, and an indexer function that maps the lower bound and a
non-negative exact integer less than the length
into a value of the range.

A numeric range is a variety of definite range specified by 
an inclusive lower bound (default 0), an exclusive upper bound,
and a step value (default 1), all of which can be exact or inexact real numbers.
Its comparator is the natural comparator on real numbers, and its indexer is
`(lambda (bound n) (+ bound (* n step)))`.

An indefinite range has just bounds and a comparator and can't be enumerated.
It is possible to specify the inclusive or exclusive nature of the bounds.

## Terminology

In arguments, *range* means any range, *drange* means any definite range.

## Constructors

definite-range comparator lower-bound length indexer

Returns a definite range with the given parameters (see above).

numeric-range start end [step]

Returns a numeric range with the given parameters (see above).
If the step argument is omitted, it is 1.

indefinite-range comparator start end

Returns an indefinite range whose inclusive lower bound
is *start* and exclusive upper bound is *end*.

closed-indefinite-range comparator start end

Returns an indefinite range whose inclusive lower bound
is *start* and inclusive upper bound is *end*.

open-indefinite-range comparator start end

Returns an indefinite range whose exclusive lower bound
is *start* and exclusive upper bound is *end*.

## Predicates

range? obj

Returns true if *obj* is a range.

definite-range? range

Returns true if *range* is a definite range.

range-contains? range value

Returns true if value is an element of *range* (if *range* is
definite) or is within the bounds of *range* (if *range* is indefinite).

range-empty? range

Returns true if *range* is empty.

## Accessors

range-start range

range-end range

Returns the upper and lower bounds of *range*.

range-element-comparator range

range-indexer drange

Returns the comparator and indexer of *drange*.

range-ref drange index

Returns the indexth element of *drange*.  It is an
error if index is less than 0 or greater than 
or equal to the length of *drange*.

## Iteration

range-split-at drange index

Returns two values which are dranges.  The first value
contains all elements of *drange* from the zeroth element
to the indexth element exclusive.  The second value contains
all elements of *drange* from the indexth element inclusive
to the last element.

range-take drange count

Returns a definite range which contains the first count elements of *drange*.

range-take-right drange count
Returns a definite range which contains the last count elements of *drange*.

range-drop drange count

Returns a definite range which contains all except the first count elements
of *drange*.

range-drop-right drange count

Returns a definite range which contains all except the last count elements of
drange.

range-reverse drange

Returns a definite range which contains all the elements of *drange*, but in
reverse order.

range-count pred drange

Returns the number of elements of *drange* which satisfy *pred*.

range-any pred drange

Returns true if any of the elements of *drange* satisfy *pred*.

range-every pred drange

Returns true if all the elements of *drange* satisfy *pred*.

range-map->list proc drange

Returns a list of the results of applying *proc* to each element
of *drange*.  The order in which *proc* is applied to the elements
is unspecified.

range-for-each proc drange

Applies *proc* to each element of *drange* in order.
Returns an unspecified result.

range-fold drange proc nil

Invokes *proc* on each member of *drange* in order, passing the result of
the previous invocation as a second argument. For the first invocation,
*nil* is used as the second argument. Returns the result of the last
invocation, or *nil* if there was no invocation.

range-fold-right drange proc nil
Invokes proc on each member of *drange* in reverse order, passing the result of
the previous invocation as a second argument. For the last invocation,
*nil* is used as the second argument. Returns the result of the last
invocation, or *nil* if there was no invocation.

## Searching

range-index pred drange

Returns the index of the first element of *drange* that satisfies *pred*.

range-index-right pred drange

Returns the index of the last element of *drange* that satisfies *pred*.

range-take-while pred drange

Returns a definite range containing the elements of *drange* that
satisfy *pred* up to the first one that does not.

range-take-while-right pred drange

Returns a definite range containing the elements of *drange* from the
last one that satisfies *pred* up to the end.

range-drop-while pred drange

Returns a definite range that omits elements of *drange* that satisfy
*pred* until the first one that does not.

range-drop-while-right pred drange

Returns a definite range the omits the last elements of *drange* that
satisfy *pred* until the last one that does not.

## Conversion

range->list drange

Returns a list containing the elements of *drange* in order.

range->generator drange

Returns a SRFI 121 generator that generates the elements of *drange*.

## Interval relations

Two ranges are congruent iff their start and end properties are equal. 

Let sA be the start property of range A, and eA be the value of the end property of range A; and likewise for range B.

Range A encloses range B iff they are not congruent, sA <= sB, and eA >= eB. This relation can be exhaustively partitioned into three sub-relations:

Range A strictly encloses range B iff sA < sB and eA > eB.   
Range A encloses range B with a suffix iff sA = sB and eA > eB.   
Range A encloses range B with a prefix iff sA < sB and eA = eB.

Range A fits within range B iff they are not congruent, sA >= sB, and eA <= eB. This relation can be exhaustively partitioned into three sub-relations:

Range A fits strictly within range B iff sA > sB and eA < eB.   
Range A is a prefix of range B iff sA = sB and eA < eB.   
Range A is a suffix of range B iff sA > sB and eA = eB.

Range A and range B overlap iff either sA or eA (but not both) is greater than sB and less than eB. This relation can be exhaustively partitioned into two sub-relations:

If sA < sB < eA < eB, then range A overlaps the start of range B.   
If sB < sA < eB < eA, then range A overlaps the end of range B.

Range A precedes range B iff eA <= sB. This relation can be exhaustively partitioned into two sub-relations:

Range A strictly precedes range B iff eA < sB.   
Range A immediately precedes range B iff eA = sB.

Range A follows range B iff sA >= eB. This relation can be exhaustively partitioned into two sub-relations:

Range A strictly follows range B iff sA < eB.   
Range A immediately follows range B iff sA = eB.

The following procedures return true if the corresponding relationships
hold between the two range arguments: `range-congruent?`, `range-encloses?`
`range-strictly-encloses?`, `range-encloses-with-suffix?`
`range-encloses-with-suffix?`, `range-fits-within?`
`range-fits-strictly-within?`, `range-prefix-of?`, `range-suffix-of?`
`range-overlaps?`, `range-overlaps-start?`, `range-overlaps-end?`
`range-precedes?`, `range-strictly-precedes?`, `range-immediately-precedes`
`range-follows?`, `range-strictly-follows?`, `range-immediately-follows?`.
It is an error if the ranges don't have the same comparators.
