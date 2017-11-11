## Introduction

This sub-proposal defines things you can do with ordered maps that are painful with hash-based mappings.  It's a cut-down version of [ImmutableMapsWortman](ImmutableMapsWortman.md).

## Rationale

Maps designed to work with comparators that have ordering procedures can readily expose additional APIs that don't make sense for comparators using hash functions.  For example, determining the smallest key in a hash table requires at least linear time, whereas determining the smallest key in a tree of some sort generally does not.  This is a list of functions that it would be useful to expose.

## Specification

### Accessors

`(map-min-key `*map*`)`

`(map-max-key `*map*`)`

Returns the least/greatest key of *map*.  It is an error for *map* to be empty. Takes O(log n) time; O(1) is optimal.

`(map-min-value `*map*`)`

`(map-max-value`*map*`)`

Returns the value associated with the least/greatest key of *map* (*not* the least/greatest value).  It is an error for *map* to be empty. Takes O(log n) time; O(1) is optimal.

`(map-key-predecessor `*map obj failure*`)`

`(map-key-successor `*map obj failure*`)`

Returns the key that immediately precedes/succeeds `obj` in `map`'s ordering. If no such association exists because *obj* is the minimum/maximum key, or because *map* is empty, returns the result of invoking the thunk *failure*. Takes O(log n) time.

### Filtering

`(map-range= `*map obj*`)`

`(map-range< `*map obj*`)`

`(map-range> `*map obj*`)`

`(map-range<= `*map obj*`)`

`(map-range>= `*map obj*`)`

Returns a map containing only the associations of `map` whose keys are equal to / less than / greater than / less than or equal to / greater than or equal to *obj*.  Takes O(log n) time, where *n* is the number of associations in the map .

Note that since map keys are unique, `imap-range=` returns a map with at most one association.

### Mapping and folding

`(imap-map/monotone `*proc map* [#|*comparator* ]]`)`

Returns a map containing the result of invoking *proc* on every association in *map*.  It is an error unless *proc* is a *monotone* unary procedure that preserves the order of map associations. Observe that mapping a map of unique associations with a monotone function yields a map of unique associations, so association uniqueness follows from the monotonicity assumption. If *comparator* is given, it is the comparator of the result; otherwise the result uses the same comparator as *map*. Takes O(n) time.


