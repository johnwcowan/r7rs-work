## Enumeration sets

Based on bitvectors indexed by the enum's ordinal.  Procedures:

```
Constructors: eset, enum-type->eset, list->set

Predicates: eset?, eset-contains?, eset-empty?, eset-disjoint?

Updaters: eset-adjoin, eset-adjoin!, eset-replace, eset-replace!, eset-delete, eset-delete!, eset-delete-all, eset-delete-all!

The whole eset: eset-size, eset-find, eset-count, eset-any?, eset-every?

Mapping and folding: eset-map->list, eset-for-each, eset-fold, eset-filter, eset-filter!, eset-remove, eset-remove!

Copying and conversion: eset-copy, eset->list, list->set!

Subsets: eset=?, eset<?, eset>?, eset<=?, eset>=?

Set theory operations: eset-union, eset-intersection, eset-difference, eset-xor, eset-union!, eset-intersection!, eset-difference!, eset-xor!

```

## Enumeration maps

Based on vectors indexed by the enum's ordinal.  Procedures:

```
Constructors

alist->emap

Predicates

emap?          emap-contains?      emap=?

Accessors

emap-ref       emap-ref/default

Mutators

emap-set!      emap-set-entries!
emap-delete!   emap-delete-keys!
emap-clear!

The whole enum map

emap-size
emap-keys      emap-values         emap-entries
emap-find      emap-count
emap-any       emap-every
emap-map       emap-map-values
emap-for-each  emap-map!
emap-collect   emap-fold
emap-filter!   emap-remove!

Copying and conversion

emap-copy
emap->alist

Enum maps as functions

emap-accessor  emap-accessor/default
emap-union     emap-intersection
emap-difference emap-xor

Comparators
emap-comparator make-emap-comparator
```
