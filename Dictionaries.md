## SRFI 1 alists

assoc assq assv

alist-cons alist-copy

alist-delete alist-delete!

## SRFI 125

Constructors: make-hash-table, hash-table, hash-table-unfold, alist->hash-table

Predicates: hash-table?, hash-table-contains?, hash-table-exists? (deprecated), hash-table-empty?, hash-table=?, hash-table-mutable?

Accessors: hash-table-ref, hash-table-ref/default

Mutators: hash-table-set!, hash-table-delete!, hash-table-intern!, hash-table-update!, hash-table-update!/default, hash-table-pop!, hash-table-clear!

The whole hash table: hash-table-size, hash-table-keys, hash-table-values, hash-table-entries, hash-table-find, hash-table-count

Mapping and folding: hash-table-map, hash-table-for-each, hash-table-walk (deprecated), hash-table-map!, hash-table-map->list, hash-table-fold, hash-table-prune!

Copying and conversion: hash-table-copy, hash-table-empty-copy, hash-table->alist

Hash tables as sets: hash-table-union!, hash-table-merge! (deprecated), hash-table-intersection!, hash-table-difference!, hash-table-xor!

## SRFI 146

Constructors: mapping, mapping-unfold, mapping/ordered, mapping-unfold/ordered

Predicates: mapping?, mapping-contains?, mapping-empty?, mapping-disjoint?

Accessors: mapping-ref, mapping-ref/default, mapping-key-comparator

Updaters: mapping-adjoin, mapping-adjoin!, mapping-set, mapping-set!, mapping-replace, mapping-replace!, mapping-delete, mapping-delete!, mapping-delete-all, mapping-delete-all!, mapping-intern, mapping-intern!, mapping-update, mapping-update!, mapping-update/default, mapping-update!/default, mapping-pop, mapping-pop!, mapping-search, mapping-search!

The whole mapping: mapping-size, mapping-find, mapping-count, mapping-any?, mapping-every?, mapping-keys, mapping-values, mapping-entries

Mapping and folding: mapping-map, mapping-map->list, mapping-for-each, mapping-fold, mapping-filter, mapping-filter!, mapping-remove, mapping-remove!, mapping-partition, mapping-partition!

Copying and conversion: mapping-copy, mapping->alist, alist->mapping, alist->mapping!

Submappings: mapping=?, mapping<?, mapping>?, mapping<=?, mapping>=?

Set theory operations: mapping-union, mapping-intersection, mapping-difference, mapping-xor, mapping-union!, mapping-intersection!, mapping-difference!, mapping-xor!

