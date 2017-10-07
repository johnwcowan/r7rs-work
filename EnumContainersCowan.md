'''THIS IS NOT A PROPOSAL.  It's just a dumping ground for some stuff I don't want to lose track of.  There will be a proper proposal later.'''


== Enumeration sets ==

Except as noted below, the procedures for creating and manipulating enumeration sets are the same as those for sets, except that `set` is replaced by `enum-set` in their names.  Wherever a newly allocated enumeration set is returned, it has the same enumeration type as the source sets.  It is an error to operate on enumeration sets of different types in a single procedure.

`(enum-type->enum-set `''enum-type''`)`

Returns a newly allocated enumeration set.  The possible elements of the set are the enum objects in ''enum-type''.  The set contains all possible elements.  The approximate R6RS equivalent is `enum-set-universe`.

`(enum-set `''enum-type''` `''element'' ...`)`

Returns a newly allocated enumeration set.  The possible elements of the set are the symbols in ''enum-type''. The set is initialized to contain the ''elements''.  There is no R6RS equivalent.

`(list->enum-set `''enum-type''` `''list''`)`

Returns a newly allocated enumeration set.  The possible elements of the set are the symbols in ''enum-type''. The set is initialized to contain the elements of ''list''.  There is no R6RS equivalent.

`(enum-set-complement `''enum-set''`)`

Returns a newly allocated enumeration set that is the complement of ''enum-set''.  This procedure is also in R6RS.

`(enum-set-projection `''enum-set''` `''enum-type''`)`

Returns a newly allocated enumeration set of type ''enum-type''.  Its elements are the enum objects in ''enum-type'' which have the same names as members of ''enum-set''.  Enum objects without corresponding names are ignored.  This procedure is also in R6RS, but uses a second enum-set in place of ''enum-type''.

There will probably be more, depending on how integer sets turn out:  `enum-set-complement`, `enum-set-complement!`, `enum-set-min`, `enum-set-max`.

== Enumeration maps ==

Based on vectors indexed by the enum's ordinal.  Procedures:

{{{
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
}}}