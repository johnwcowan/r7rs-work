## Tables

**This proposal is at once too vague and too complex, so I'm withdrawing it.  I may create a replacement later.**

This proposal defines an interface to tables, which are widely recognized as a fundamental data structure for a wide variety of applications.  A table is a data structure that:

1. Is disjoint from all other types.
1. Provides a mapping from some set of *keys* to some set of *values* associated to those keys.  Keys and values may be any Scheme objects.
1. Provides an *equivalence function* which defines when a proposed key is the same as an existing key.  No table may contain more than one value for a given key.
1. Supports mutation as the primary means of setting the contents of a table.
1. Assumes that keys are immutable; mutating a key leads to unspecified behavior.
1. May be *immutable*, meaning that associations may not be added or deleted and that the value associated with a key may not be changed.

No particular implementation such as a-lists, red-black trees, or hash tables is mandated by this proposal.  Implementations SHOULD provide the most efficient implementation in time or space or both possible that is consistent with their larger goals; this may mean different implementations for tables of different lengths.

Incorporating this proposal as a standard feature in WG1 Scheme implementations makes it possible to write efficient and portable programs that use tables.

I have added equivalences to SRFI-69 (Basic hash tables) where they exist.  Names are based on those in [CompleteSequenceCowan](CompleteSequenceCowan.md).

## Constructors

`(make-table `[*equivalence-function*]` . `*args*`)`

Creates a new table with no associations. *Equivalence-function* is a predicate that should accept two keys and return a boolean telling whether they denote the same key value; it defaults to `equal?`.  It must be reflexive, symmetrical, and transitive.  (SRFI-69 `make-hash-table`)


Implementations MAY use the *args* for implementation-specific extensions.

`(table `[*value*](*key*) ... `)`

Creates a new table and populates it with the associations based on the successive *key* and *value* arguments.  The implementation may take the arguments into account in deciding what kind of table to create, but should not assume that no other types of keys or values will ever exist.   (Not in SRFI-69)

`(immutable-table `[*value*](*key*) ... `)`

The same as ``table``, except that the resulting table is immutable.   (Not in SRFI-69)


`(alist->table `*alist*` . `*args*`)`

Creates a new table as if by invoking `(make-table . `*args*`)` which maps the car of every element in *alist* to the cdr of the same element.  If some key occurs multiple times in alist, the value in the first association will take precedence over later ones. (SRFI-69 `alist->hash-table`)

`(alist->immutable-table `*alist*` . `*args*`)`

The same as `alist->table`, except that the resulting table is immutable.  (Not in SRFI-69)


## Copiers

`(table-copy `*table*`)`

Creates a new mutable table with the same equivalence predicate, associations, and implementation-dependent properties (if any) as *table*. (SRFI-69 `hash-table-copy`)

`(table-immutable-copy `*table*`)`

Returns an immutable table with the same equivalence predicate, associations, and implementation-dependent properties (if any) as *table*.  If *table* is immutable, implementations need not make a new table.  (Not in SRFI-69)


## Predicates

`(table? `*obj*`)`

Returns `#t` if *obj* is a table.  (SRFI-69 `hash-table?`)

`(table-key-exists? `*table*` `*key*`)`

Returns `#t` if there is any association to *key* in *table*. (SRFI-69 `hash-table-exists?`)

## Accessors

`(table-ref `*table*` `*key* [[|*thunk* ]]`)`

Returns the value associated to *key* in *table*. If no value is associated to *key* and *thunk* is given, it is called with no arguments and its value is returned; if *thunk* is not given, an error is signalled.  (SRFI-69 `hash-table-ref`)

`(table-length `*table*`)`

Returns the number of associations in *table*.  (SRFI-69 `hash-table-size`)

`(table-keys `*table*`)`

Returns a list of the keys in *table*. The order of the keys is unspecified.  (SRFI-69 `hash-table-keys`)

`(table-values `*table*`)`

Returns a list of the values in *table*. The order of the keys is unspecified, and may be inconsistent with the results of `table-keys`.  (SRFI-69 `hash-table-values`)

`(table-equivalence-function `*table`)`

Returns the equivalence function of *table*.  (SRFI-69 `hash-table-equivalence-function`)

## Mutators

It is an error to apply any of these to an immutable table.

`(table-set! `*table*` `*key*` `*value*`)`

Sets the *value* associated to *key* in *table*. The previous association (if any) is removed.  (SRFI-69 `hash-table-set!`)

`(table-delete! `*table*` `*key*`)`

Removes any association to *key* in *table*. It is not an error if no association for that *key* exists.  (SRFI-69 `hash-table-delete!`)

`(table-update! `*table*` `*key*` `*procedure* [[|*thunk* ]]`)`

Semantically equivalent to, but may be implemented more efficiently than, the following code:

`(table-set! `*table*` `*key*` (`*procedure* `(table-ref `*table*` `*key*` `*thunk*`)))`

(SRFI-69 `hash-table-update!`)

## Iterators

`(table-map `*table*` `*procedure*` . `*args*`)`

Returns a new table as if by invoking `(make-table . `*args*`)`.  The new table is the result of mapping *procedure*, which takes two arguments, over *table*.  It is applied to the key and value of each association in *table*, and returns two values, the key and value to be placed in the new table.  If the table is mutated while `table-map` is running, the behavior is unpredictable.  (Not in SRFI-69)

`(table-for-each `*table*` `*procedure*`)`

The same as `table-map`, except that no new table is constructed; returns undefined values.  If the table is mutated while `table-for-each` is running, the behavior is unpredictable.  (SRFI-69 `hash-table-walk`)

`(table->alist `*table*`)`

Returns an association list such that the car of each element is a key in *table* and the corresponding cdr of the element is the value associated to the key.  The order of the elements is unspecified.  (SRFI-69 `hash-table->alist`)

