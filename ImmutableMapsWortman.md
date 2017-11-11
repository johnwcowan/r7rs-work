## Introduction

This proposal defines immutable maps. A structure is immutable when all its operations leave the structure unchanged and still available to any procedure that holds a pointer to it. Note that none of the procedures specified here ends with a `!`.

## Rationale

Immutable maps are the analogue of hash tables, and have a similar API.  However, hash tables are changed by mutating them directly, whereas immutable map procedures return modified versions of the map, leaving the original version undisturbed.  In addition, whereas the keys of a hash table requires a hash function either explicitly or as part of a comparator, the keys of an immutable map require an ordering function and may be retrieved from the map in key order.

Immutable maps are also an alternative to association lists, which also store key-value associations, but with different key constraints (none) and efficiency guarantees (linear).

In the same way that a list of key-value dotted pairs can implement an association list, a set of key-value dotted pairs can implement a map. Implementations may use this approach, or may implement a distinct data structure specific to maps.

The following conditions are errors:

* if the comparator does not provide an ordering procedure

* if a procedure defined in this SRFI is invoked on maps with distinct comparators (in the sense of `eq?`)

* if an object is mutated while it is contained in a map

* if an object is added to a map which does not satisfy the type test predicate of the comparator

* if an object is added to or removed from a map while iterating over it

These requirements can be satisfied by many flavors of *self-balancing binary trees.* Red-black trees, 1-2 brother trees, and labeled 2-3 finger trees are particularly convenient in an immutable context.

## Specification

Immutable maps (also known as imaps) are disjoint from all other Scheme types with the possible exception of immutable sets and bags.

We specify required time efficiency upper bounds using big-O notation. We note when, in some cases, there is "slack" between the required bound and the theoretically optimal bound for an operation. Implementations may use data structures with amortized time bounds, but should document which bounds hold in only an amortized sense. The use of randomized data structures with expected time bounds is discouraged.

### Constructors

If two associations are to be inserted into a map that are equal in the sense of the map's comparator but are not `eqv?`, the first to be specified or generated prevails.

`(imap `*comparator* ( *key value* ...]`)`

Returns a map using *comparator*.  For each pair of arguments, an association is added to the map with *key* as its key and *value* as its value.  Takes O(n log n) time.

`(imap-unfold `*stop? mapper successor seed*`)`

Invokes the predicate *stop?* on *seed*.  If it returns false, generate the next result by applying *mapper* to *seed*, generate the next seed by applying *successor* to *seed*, and repeat this algorithm with the new seed.  If *stop?* returns true, return a map containing the results.  Takes O(n log n) time.

### Predicates

`(imap? `*obj*`)`

Returns `#t` if *obj* is a map, and `#f` otherwise.  Takes O(1) time.

`(imap-empty? `*map*`)`

Returns `#t` if *map* contains zero associations, and `#f` otherwise.  Takes O(1) time.

`(imap-contains? `*map obj*`)`

Returns `#t` if *map* contains *obj* as a key, and `#f` otherwise.  Takes O(log n) time.

### Accessors

`(imap-ref `*map key* [#|*failure* []] ]( *success*)`)`

Extracts the value associated to *key* in *map*, invokes the procedure *success* on it, and returns its result; if *success* is not provided, then the value itself is returned.  Otherwise, `imap-ref` invokes the procedure *failure* on no arguments and returns its result, but if *failure* is not provided, it is an error.  Takes O(log n) time.

`(imap-ref/default `*imap key default*`)`

Semantically equivalent to, but may be more efficient than, `(imap-ref `*map key* `(lambda () `*default*`))`.

`(imap-min-key `*map*`)`

`(imap-max-key `*map*`)`

Returns the least/greatest key of *map*.  It is an error for *map* to be empty. Takes O(log n) time; O(1) is optimal.

`(imap-min-value `*map*`)`

`(imap-max-value`*map*`)`

Returns the value associated with the least/greatest key of *map* (*not* the least/greatest value).  It is an error for *map* to be empty. Takes O(log n) time; O(1) is optimal.

`(imap-comparator `*map*`)`

Returns the comparator of *map*.  Takes O(1) time.

`(imap-key-predecessor `*map obj failure*`)`

`(imap-key-successor `*map obj failure*`)`

Returns the key that immediately precedes/succeeds `obj` in `map`'s ordering. If no such association exists because *obj* is the minimum/maximum key, or because *map* is empty, returns the result of invoking the thunk *failure*. Takes O(log n) time.

### Functional update

`(imap-add `*map key value*`)`

Returns a map which contains the associations of *map* and an association with *key* and *value* as well.  If there is already an association of *map* whose key is equal (in the sense of the comparator) to *key*, the existing key prevails.  Takes O(log n) time.

`(imap-add-all `*map key-list value-list*`)`

Returns a map which contains the associations of *map* and associations constructed from the corresponding associations of *key-list* and *value-list* as well.  It is an error if the associations of *key-list* are not distinct and increasing in the sense of the comparator of *map*.  If there is already an association of *map* which is equal (in the sense of the comparator) to an association of *list*, the key of *map* prevails.  Takes O(k log n) time, where *k* is the length of *list*.

`(imap-replace `*map key value*`)`

Returns a map which contains the associations of *map* and an association with *key* and *value* as well.  If there is already an association of *map* whose key is equal (in the sense of the comparator) to *key*, *key* prevails.  Takes O(log n) time.

`(imap-delete `*map key*`)`

Returns a map which contains the associations of *map* with the exception of any association whose key is *key*.  If there is already an association of *map* whose key is equal (in the sense of the comparator) to *key*, the existing key prevails.  Takes O(log n) time.

`(imap-delete-keys `*map key-list*`)`

Returns a map which contains the associations of *map*, excluding any associations whose keys appear in *key-list*.  It is an error if the associations of *list* are not distinct and increasing in the sense of the comparator of *map*. Takes O(k log n) time, where *k* is the length of *key-list*.

`(imap-push `*map key value*`)`

If an association with *key* is found in *map*, then return a map with the value of *key* updated to the result of invoking `cons` on *value* and  the original value.  If the value is not found, an error satisfied by `imap-key-not-found?` is signaled.  It is an error if the value is not a pair.  Takes O(log n) time.

`(imap-pop `*map key*`)`

If an association with *key* is found in *map*, then return two values, *map* with the value of *key* updated to the cdr of the original value, and the car of the original value.  If the value is not found, an error satisfied by `imap-key-not-found?` is signaled.  It is an error if the value is not a pair.  Takes O(log n) time.

`(imap-search `*map obj failure success*`)`

A continuation-based universal update procedure. Attempts to find an association in *map* whose key is equal (in the sense of the comparator) to *obj*.  When such an association is found, `imap-search` calls *(success key update remove)*.  The *success* procedure either tail-calls * (update new-value ret)* to return a map that associates the matched key with *new-value*, or else tail-calls *(remove ret)* to remove the matched association  from *map*.

When no such association is found, `imap-search` calls *(failure insert ignore)*, which either tail-calls *(insert new-value ret)* to insert an association whose key is *obj* and whose value is *new-value* into *map*, or else tail-calls *(ignore ret)* .`

In all cases, `imap-search` returns two values, a map reflecting the indicated modification (if any) and the value *ret* produced by one of the continuations. It runs in O(log n) time.

(This procedure is based on an analogous procedure for hash tables suggested by Alexey Radul and attributed to Taylor Campbell.)

### The whole map

`(imap-size `*map*`)`

Returns the size of *map* as an exact integer.  May take O(n) time, though O(1) is optimal.

`(imap-find `*map pred failure*`)`

For each association of *map*, invoke *proc* on its key and value. If *proc* returns true on a value, then return that value. If all the calls to *proc* return `#f`, return the result of invoking the thunk *failure*.  Takes O(log n) time.

`(imap-count `*pred map*`)`

Returns the number of associations in *map* which satisfy *pred* as an exact integer.  Takes O(n) time.

`(imap-any `*pred map*`)`

`(imap-every `*pred map*`)`

Invokes *pred* in order on the keys and values of *map*, passing a key and a value to each invocation, until one of them returns a true/false value, which is then returned.  If there are no such associations, returns `#f`/`#t`.  Takes O(n) time.

### Filtering

`(imap-range= `*map obj*`)`

`(imap-range< `*map obj*`)`

`(imap-range> `*map obj*`)`

`(imap-range<= `*map obj*`)`

`(imap-range>= `*map obj*`)`

Returns a map containing only the associations of `map` whose keys are equal to / less than / greater than / less than or equal to / greater than or equal to *obj*.  Takes O(log n) time, where *n* is the number of associations in the map .

Note that since map keys are unique, `imap-range=` returns a map of at most one association.

`(imap-filter `*pred map*`)`

`(imap-remove `*pred map*`)`

Returns a map containing only those associations on whose keys *pred* returns true/false. Takes O(n log n) time; O(n) is optimal.

`(imap-partition`*pred map*`)`

Returns two values, `(imap-filter `*pred map*`)` and `(imap-remove `*pred map*`)` respectively, but may be more efficient.

### Mapping and folding

`(imap-map/monotone `*proc map* [#|*comparator* ]]`)`

Returns a map containing the result of invoking *proc* on every association in *map*.  It is an error unless *proc* is a *monotone* unary procedure that preserves the order of map associations. Observe that mapping a map of unique associations with a monotone function yields a map of unique associations, so association uniqueness follows from the monotonicity assumption. If *comparator* is given, it is the comparator of the result; otherwise the result uses the same comparator as *map*. Takes O(n) time.

`(imap-map`*proc map* [#|*comparator* []] ]( *merger*)`)`

Like `imap-map/monotone`, except that *proc* is not required to be monotone. The `merger` procedure is used to select among any duplicate associations (in the sense of the comparator of *map*) that might be created; it returns the value to be used; if absent, the association chosen is implementation-specific.  Takes O(n log n) time.

`(imap-map-values `*proc map*`)`

Invokes *proc* on the key and value of every *association* in *map*.  The result is a map which associates each key with the result of the corresponding invocation.  Takes O(n) time.

`(imap-for-each `*proc map*`)`

Invokes *proc* on the key and value of every *association* in *map*.  The result is unspecified. Takes O(n) time.

`(imap-collect `*proc map*`)`

Invokes *proc* on the key and value of every *association* in *map*.  The results are collected into a list in order, which is returned. Takes O(n) time.

`(imap-fold `*proc nil map*`)`

The fundamental map iterator. Equivalent to, but may be more efficient than, `(fold `*proc base* ` (imap->increasing-list `*map*`))`.  Takes O(n) time.

`(imap-fold `*proc nil map*`)`

The fundamental map iterator. Equivalent to, but may be more efficient than, `(fold-right `*proc base* ` (imap->increasing-list `*map*`))`.  Takes O(n) time.

### Subsets

Note: The following three predicates do not obey the trichotomy law and therefore do not constitute a total order on sets.

`(imap=? `*set1 set2* ...`)`

Returns `#t` if each *map* contains the same associations, and `#f` otherwise.

`(imap<? *set1 set2* ...`)`

Returns `#t` if each *map* other than the last is a proper subset of the following map, and `#f` otherwise.

`(imap>? *set1 set2* ...`)`

Returns `#t` if each *map* other than the last is a proper superset of the following map, and `#f` otherwise.

`(imap<=? *set1 set2* ...`)`

Returns `#t` if each *map* other than the last is a subset of the following map, and `#f` otherwise.

`(imap>=? *set1 set2* ...`)`

Returns `#t` if each *map* other than the last is a superset of the following map, and `#f` otherwise.

### Conversion

`(imap-keys `*imap*`)`

`(imap-values `*imap*`)`

Returns a list of the keys/values of *imap* in increasing order. Takes O(n) time.

`(imap-entries `*imap*`)`

Returns two values, lists of the keys and values of *imap* in increasing order, but may be more efficient. Takes O(n) time.

`(imap->alist `*map*`)`

Returns an association list containing the associations of `map` in increasing order. Takes O(n) time.

`(ordered-alist->imap comparator list)`

Returns a map containing the associations of *list* and using *comparator*. It is an error for *alist* to be anything other than an alist in increasing order. Takes O(n log n) time; O(n) is optimal.

`(alist->imap `*comparator list [#|*merger* ]]`)`

Returns a map containing the associations of *alist* and using *comparator*.It is an error unless *alist* is a proper association list, but it may contain duplicates and need not be in order.  The *merger* procedure is used to select among any duplicate keys (in the sense of the comparator of *map*) that might be created; it accepts the existing and new keys and returns the key to be used.  Takes O(n log n) time.

`(generator->imap `*generator*`)

`(imap->generator `*imap*`)`

Converts a [SRFI 121](http://srfi.schemers.org/srfi-121/srfi-121.html) generator to and from a map.  The generator produces pairs containing
the keys in the cars and the values in the cdrs.

### Maps as functions

The following procedures provide functions based on maps.  In this way, for example, lists can be processed by `map` using the procedure returned from a map by `imap-accessor`.

`(imap-accessor `*map* [#|*failure* []] ]( *success*)`)`

Curried version of `imap-ref`.  Returns a procedure of one argument, a key, which returns what `imap-ref` returns when invoked with the the passed arguments.

`(imap-accessor/default `*map default*`)`

Curried version of `imap-ref/default`.  Returns a procedure of one argument, a key, which returns what `imap-ref/default` returns when invoked with the passed arguments.

### Maps as sets

`(imap-union `*map* ... `)`

`(imap-intersection `*map* ... `)`

`(imap-difference `*map* ... `)`

`(imap-xor `*map,,1,, map,,2,,*`)`

Returns a map containing the union/intersection/difference/symmetric difference of the arguments. All the arguments must be sets sharing an equivalent comparator. The map operator is applied in a left-associative order. If an association is found in more than one set, the first map in the argument list prevails.  May take O(kn log n) time, where k is the number of sets and n is the number of associations involved, though O(kn) time is optimal.

## Exceptions

### Exceptions

`(imap-key-not-found? `*obj*`)`

Returns `#t` if *obj* is an object raised by the `imap-ref` procedure or any other procedure that accesses maps when the key is not found and there is no failure procedure, and `#f` otherwise.

