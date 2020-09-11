## Introduction

A fold procedure performs an operation called
*folding* over various data types described in various SRFIs.  In
the SRFIs mentioned below, the numbers in parentheses are the SRFIs
where the fold procedure is defined.

Without too much loss of generality, we may understand a fold
procedure as accepting a procedure *proc*, one or more seeds, and
one or more sequences, all of the same type.  The tags [seqs]
and [seeds] indicate which ones take which.  No fold procedure can
accept both multiple sequences and multiple seeds, though some take
only one of each.  In particular, if the sequence type lacks a
natural order, only one sequence can be folded over.  Some procedures
accept only a single sequence and a single seed.

The procedure is invoked on the first element of the sequence and
the seed, and returns a new seed value to be used with the next
element.  When the end of the sequence is reached, the final result
of *proc* is returned.

The issue is this: How is *proc* called, with the new value from
the sequence(s) first and the old value (built up from the seed and
previous sequence values) second, or vice versa?

## New before old

The following procedures accept the new value before the old.

`fold` (1) [seqs]  
`string-fold` (13, 130, 140, 152)  
`char-set-fold` (14)  
`hash-table-fold` (69, 125)  
`set-fold` (113)  
`regex-fold` (115)  
`ipair-fold` (116) [seqs]  
`ideque-fold` (134)  
`textual-fold` (135)  
`mapping-fold` (146)  
`hashmap-fold` (146)  
`bitwise-fold` (151)  
`generator-fold` (158) [seqs]  
`array-fold` (179)  
`json-fold` (180)  
`maybe-fold` (189)  
`either-fold` (189)  
`range-fold` (196) [seqs]  
`enum-set-fold` (209)

## Old before new

The following procedures accept the old value before the new.
The numbers in parentheses are the SRFIs where the fold procedure
is defined.

`stream-fold` (41)  
`vector-fold` (43, 133)  
`@vector-fold` (160)  
`bitvector-fold` (178)

## Args-fold

Finally, there is the special case of `args-fold` (27).
This procedure uses new-before-old order: the current option is the
first argument passed to *proc* and the remaining arguments are the
multiple seeds.  and it returns the same number of new seeds as its
values.


## Right folds

All the `*-fold-right` functions have the same order
as the corresponding `*-fold` (i.e. left fold)
functions.  In SRFI 41 there is no `stream-fold-right` in the API, but it's
provided as an example, where the order is the opposite of `stream-fold`,
that is, new before old, where the old value is the costate.
