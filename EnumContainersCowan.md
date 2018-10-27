## Enumeration sets

Based on bitvectors indexed by the enum's ordinal.  Procedures:

```
Predicates: set?, set-contains?, set-empty?, set-disjoint?

Accessors: set-member, set-element-comparator

Updaters: set-adjoin, set-adjoin!, set-replace, set-replace!, set-delete, set-delete!, set-delete-all, set-delete-all!, set-search!

The whole set: set-size, set-find, set-count, set-any?, set-every?

Mapping and folding: set-map, set-for-each, set-fold, set-filter, set-filter!, set-remove, set-remove!, set-partition, set-partition!

Copying and conversion: set-copy, set->list, list->set, list->set!

Subsets: set=?, set<?, set>?, set<=?, set>=?

Set theory operations: set-union, set-intersection, set-difference, set-xor, set-union!, set-intersection!, set-difference!, set-xor!

```

## Enumeration maps

Based on vectors indexed by the enum's ordinal.  Procedures:

```
Constructors

enum-type->enum-map                        enum-map-unfold

Predicates

enum-map?          enum-map-contains?      enum-map=?

Accessors

enum-map-ref       enum-map-ref/default

Mutators

enum-map-set!      enum-map-set-entries!
enum-map-delete!   enum-map-delete-keys!
enum-map-push!     enum-map-pop!
enum-map-search!   enum-map-clear!

The whole enum map

enum-map-size 
enum-map-keys      enum-map-values         enum-map-entries
enum-map-find      enum-map-count
enum-map-any       enum-map-every
enum-map-map       enum-map-map-values
enum-map-for-each  enum-map-map!
enum-map-collect   enum-map-fold
enum-map-filter!   enum-map-remove!

Copying and conversion

enum-map-copy
enum-map->alist

Enum maps as functions

enum-map-accessor  enum-map-accessor/default
enum-map-union     enum-map-intersection
enum-map-difference enum-map-xor

Comparators
enum-map-comparator make-enum-map-comparator
```
