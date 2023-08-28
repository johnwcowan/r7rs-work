Type classes for sets and bags based on [SRFI 113](https://srfi.schemers.org/srfi-113/srfi-113.html).
Sets provide the `set-*` procedures,
(33 generic procedures, 8 primitives);
bags provide the `set-*` and `bag-*` procedures
(40 generic procedures, 10 primitives).

Predicates: set?(P), set-contains?, set-empty?, set-disjoint?, set-pure?(P)

Accessors: set-member(P), set-element-comparator(P)

Updaters: set-adjoin!, set-replace!, set-delete!, set-delete-all!, set-search!

The whole set: set-size(P), set-find, set-count, set-any?, set-every?

Mapping and folding: set-map(P), set-for-each(P), set-fold, set-filter(P), set-remove, set-partition

Copying and conversion: set->list

Subsets: set=?, set<?, set>?, set<=?, set>=?

Set theory operations: set-union, set-intersection, set-difference, set-xor

Bag procedures: bag-sum, bag-product, bag-element-count(P),
bag-for-each-unique(P), bag-fold-unique, bag-increment, bag-decrement
