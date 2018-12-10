Ranges

A numeric range has an inclusive lower bound (default 0), an exclusive upper bound,
and a step value (default 1), all of which can be exact or inexact real numbers.

A general range has bounds of any type, a numeric step value (default 1), a
comparator, and an indexer function that maps the lower bound and an integer
into a value of the range.

An indefinite range has just bounds and a comparator and can't be enumerated.
However, it is possible to specify for each bound whether it is inclusive or
exclusive; for other range types, the lower bound is inclusive and the upper
bound exclusive, as usual in Scheme.

Constructors

numeric-range  
general-range  
indefinite-range

Predicates (all range types)

range?  
indefinite-range?  
range-contains?  
range-empty?  
range=?

Accessors (all range types)

range-start  
range-end  
range-step  
range-element-comparator  
range-indexer  
range-ref  
range-next  
range-previous 

Iteration (definite ranges only)

range-split-at  
range-take  
range-take-right  
range-drop  
range-drop-right  
range-reverse  
range-count  
range-any  
range-every  
range-map  
range-for-each  
range-fold  
range-fold-right  
range-filter  
range-remove  
range-partition

Searching (definite ranges only)

range-index  
range-index-right  
range-skip  
range-skip-right  
range-take-while  
range-take-while-right  
range-drop-while  
range-drop-while-right

Conversion (definite ranges only)

range->list  
range->generator

Interval relations (all range types)

range-congruent?  
range-encloses?  
range-encloses-strictly?  
range-prefix?  
range-suffix?  
range-overlaps?  
range-overlaps-strictly?  
range-overlaps-start?  
range-overlaps-end?  
range-precedes?  
range-precedes-strictly?  
range-precedes-immediately?  
range-follows?  
range-follows-strictly?  
range-follow-immediately?
