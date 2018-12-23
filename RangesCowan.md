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
However, it is possible to specify for each bound whether it is inclusive or
exclusive.

## Terminology

In arguments, *range* means any range, *drange* means any definite range.

## Constructors

definite-range constructor lower-bound length indexer

numeric-range [start] end [step]

closed-indefinite-range comparator start end

open-indefinite-range comparator start end

open-closed-indefinite-range comparator start end

closed-open-indefinite-range comparator start end

## Predicates

range? obj

definite-range? range

range-contains? range value

range-empty? range

## Accessors

range-start range

range-end range

range-element-comparator range

range-indexer drange

range-ref drange index

## Iteration

range-split-at drange index

range-take drange count

range-take-right drange count

range-drop drange count

range-drop-right drange count
 
range-reverse drange

range-count pred drange

range-any pred drange

range-every pred drange

range-map->list proc drange

range-for-each proc drange

range-fold drange kons nil

range-fold-right drange kons nil

## Searching

range-index pred drange

range-index-right pred drange

range-take-while pred drange

range-take-while-right pred drange

range-drop-while pred drange

range-drop-while-right pred drange

## Conversion

range->list drange

range->generator drange

## Interval relations

range-congruent? range1 range2

range-encloses? range1 range2

range-encloses-strictly? range1 range2

range-prefix? range1 range2

range-suffix? range1 range2

range-overlaps? range1 range2

range-overlaps-strictly? range1 range2

range-overlaps-start? range1 range2

range-overlaps-end? range1 range2

range-precedes? range1 range2

range-precedes-strictly? range1 range2

range-precedes-immediately? range1 range2

range-follows? range1 range2

range-follows-strictly? range1 range2

range-follow-immediately? range1 range2

