
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
