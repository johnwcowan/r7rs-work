# Interval

An interval is analogous to a range, but it is not a collection;
it just has bounds and a comparator and can't be enumerated.
Unlike a range,
it is possible to specify the inclusive or exclusive nature of the bounds.

## Constructors

`(interval `*comparator start end*`)`

Returns an interval whose inclusive lower bound
is *start* and exclusive upper bound is *end*.

`(inverse-interval `*comparator start end*`)`

Returns an interval whose exclusive lower bound
is *start* and inclusive upper bound is *end*.

`(closed-interval `*comparator start end*`)`

Returns an interval whose inclusive lower bound
is *start* and inclusive upper bound is *end*.

`(open-interval `*comparator start end*`)`

Returns an interval whose exclusive lower bound
is *start* and exclusive upper bound is *end*.

## Predicates

`(interval? `*obj*`)`

Returns `#t` if *obj* is a interval and `#f` otherwise.

`(interval-contains? `*interval value*`)`

Returns true if value is within the bounds of *interval*.

`(interval-empty? `*interval*`)`

Returns true if *interval* is empty.

`(interval-start-inclusive? `*interval*`)`

Returns `#t` if *interval* includes its lower bound and `#f` otherwise.

`(interval-end-inclusive? `*interval*`)`

Returns `#t` if *interval* includes its upper bound and `#f` otherwise.

## Accessors

`(interval-start `*interval*`)`

`(interval-end `*interval*`)`

Returns the upper and lower bounds of *interval*.

`(interval-element-comparator `*interval*`)`

Returns the comparator of *interval*.

## Interval relations

In all these procedures it is an error if the intervals don't have the same comparators.

Let sA be the lower bound of interval A,
and eA be the value of the upper bound of interval A;
and likewise for interval B.  The inclusive/exclusive
nature of the bounds is respected.

`(interval-congruent? `*intervala intervalb*`)`

Two intervals are congruent iff sA = sB and eA = eB.

`(interval-encloses? `*intervala intervalb*`)`

Interval A encloses interval B iff they are not congruent, sA <= sB, and eA >= eB.
This relation can be exhaustively partitioned into three sub-relations:

`(interval-strictly-encloses? `*intervala intervalb*`)`

Interval A strictly encloses interval B iff sA < sB and eA > eB.

`(interval-encloses-with-suffix? `*intervala intervalb*`)`

Interval A encloses interval B with a suffix iff sA = sB and eA > eB.

`(interval-encloses-with-prefix? `*intervala intervalb*`)`

interval A encloses interval B with a prefix iff sA < sB and eA = eB.

`(interval-fits-within? `*intervala intervalb*`)`

Interval A fits within interval B iff they are not congruent, sA >= sB, and eA <= eB.
This relation can be exhaustively partitioned into three sub-relations:

`(interval-strictly-fits-within? `*intervala intervalb*`)`

Interval A fits strictly within interval B iff sA > sB and eA < eB.

`(interval-prefix? `*intervala intervalb*`)`

Interval A is a prefix of interval B iff sA = sB and eA < eB.

`(interval-suffix? `*intervala intervalb*`)`

Interval A is a suffix of interval B iff sA > sB and eA = eB.

`(interval-overlaps? `*intervala intervalb*`)`

Interval A and interval B overlap iff either sA or eA (but not both) is greater than sB and less than eB.
This relation can be exhaustively partitioned into two sub-relations:

`(interval-overlap-start? `*intervala intervalb*`)`

If sA < sB < eA < eB, then interval A overlaps the start of interval B.

`(interval-overlap-end? `*intervala intervalb*`)`

If sB < sA < eB < eA, then interval A overlaps the end of interval B.

`(interval-precedes? `*intervala intervalb*`)`

interval A precedes interval B iff eA <= sB.
This relation can be exhaustively partitioned into two sub-relations:

`(interval-strictly-precedes? `*intervala intervalb*`)`

Interval A strictly precedes interval B iff eA < sB.

`(interval-immediately-precedes? `*intervala intervalb*`)`

Interval A immediately precedes interval B iff eA = sB.

`(interval-follows? `*intervala intervalb*`)`

Interval A follows interval B iff sA >= eB.
This relation can be exhaustively partitioned into two sub-relations:

`(interval-strictly-follows? `*intervala intervalb*`)`

Interval A strictly follows interval B iff sA < eB.

`(interval-immediately-follows? `*intervala intervalb*`)`

Interval A immediately follows interval B iff sA = eB.

## Interval set operations

`(interval-union `*intervala intervalb*`)`

Returns an interval containing all the elements of either *intervala* or *intervalb*,
or `#f` if no such interval exists.

`(interval-union `*intervala intervalb*`)`

Returns an interval containing all the elements of either *intervala* or *intervalb*,
or `#f` if no such interval exists.

`(interval-intersection `*intervala intervalb*`)`

Returns an interval containing all the elements of both *intervala* and *intervalb*,
or `#f` if no such interval exists.

`(interval-difference `*intervala intervalb*`)`

Returns an interval containing all the elements of *intervala* but not *intervalb*,
or `#f` if no such interval exists.


