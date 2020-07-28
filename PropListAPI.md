Plists are lists with alternating keys and values.
Traditional plists use `eq?`, but this one uses `eqv?`
so that numbers and characters can be used as keys.

This SRFI is based on CL, but is also designed to work with
[the dictionaries system](Dictionaries.md).


Note that this API is for property lists themselves,
not for symbols with property lists attached.
In a system where symbols have plists, you need
a primitive to retrieve the plist associated with the symbol
and another to replace it, and this SRFI
can then accept symbols as well as plists.

(plist-get plist key [ failure [ success ] ])

Search for the first value whose key is key
and call the success or failure continuation.

(plist-get/default plist key default)

The same, but returns the default on failure.

(plist-get-properties plist list-of-keys)

Searches plist for the first key that is eqv?
to any of the keys in list-of-keys.
Returns three values: the key, the value, and the
tail of the plist following the value.
If none of the keys are found, return three #f values.

(plist-put! plist key value)

If the key is in the plist,
the pair pointing to its value is mutated.
If the key is not in the plist,
the key and values are destructively
appended to the plist.

This procedure returns the new plist,
which normally would be the same as the old plist,
unless the old plist is empty.

(plist-remove plist key)

If the key is in the plist, remove both key and value.
If not, do nothing.

This procedure returns the new plist,
which normally would be the same as the old plist,
unless the old plist is empty.

(plist-search! plist key failure success)

Does a lookup, insert, replace, or delete.
See SRFI 125 hash-table-search!.

(plist-size plist)

Number of keys in the plist.

(plist-map! proc plist)

Proc takes two arguments, key and value, and returns the new value.

(plist-filter! pred plist)

Pred takes two arguments, key and value.
When pred is satisfied, key and value are removed.

(plist-for-each proc plist)

Calls proc on each key and value, returns unspecified value.


