**THIS IS NOT A PROPOSAL.  It's just a dumping ground for some stuff I don't want to lose track of.  There will be a proper proposal later.**

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
