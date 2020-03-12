A bimap is a pair of [dictionaries](Dictionaries.md), one mapping keys to unique values,
the other mapping the values back to their keys.  Bimaps are mutable even if the
underlying dictionaries are linear-update.

Accessors: bimap-dict, bimap-reverse-dict.  It is an error to mutate either dictionary separately.

Constructors: make-bimap, bimap, bimap-unfold, dict->bimap.

Mutators:  bimap-search, bimap-set, bimap-adjoin, bimap-delete, bimap-delete-all, bimap-replace, bimap-intern,
bimap-update, bimap-update/default.

Convenience procedures: bimap-ref, bimap-ref/default.