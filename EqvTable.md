This table contains some facts about how `eqv?` works in R5RS, R6RS, and R7RS draft 6, and a **currently incomplete** proposal about what we should do for R7RS.

`eqv?` always returns false when two objects have different types, so the rows of this table indicate what it does when both arguments are of the stated type.  Note that "unspecified" means either `#t` or `#f`, but no other value.

|Type|R5RS|R6RS|R7RS draft|Proposal|
|Booleans|True iff both `#t` or both `#f`|True iff both `#t` or both `#f` (via `boolean=?`)|True iff both `#t` or both `#f`|True iff both `#t` or both `#f` (via `boolean=?`)|
|Symbols|True iff names are spelled the same|True iff names are spelled the same (via `symbol=?`)|True iff names are spelled the same|True iff names are spelled the same (via `symbol=?`)|
|Characters|True iff `char=?` is true|True iff `char=?` is true|True iff `char=?` is true|True iff `char=?` is true|
|Mutable pairs|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|
|Immutable pairs|True iff contain the same locations|True iff contain different locations and/or return same values to `car` or `cdr`|True iff contain the same locations|True iff contain different locations and/or return same values to `car` or `cdr`|
|Mutable strings|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|
|Immutable strings|True iff contain the same locations|True iff contain the same locations and/or all elements are the same|True iff contain the same locations|True iff contain the same locations and/or all elements are the same|
|Mutable vectors|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|
|Immutable vectors|True iff contain the same locations|True iff contain the same locations and/or all elements are the same|True iff contain the same locations|True iff contain the same locations and/or all elements are the same|
|Mutable bytevectors|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|
|Immutable bytevectors|True iff contain the same locations|True iff contain the same locations and/or all elements are the same|True iff contain the same locations|True iff contain the same locations and/or all elements are the same|
|Mutable records|Unspecified|True iff contain the same locations|True iff contain the same locations|True iff contain the same locations|
|Immutable records|Unspecified|True iff contain the same locations and/or all elements are the same|True iff contain the same locations|True iff contain the same locations|
|Procedures with distinct behaviors|False|False|False|False|
|All other procedures|True iff are the same locations|Unspecified|Unspecified|???|
|Exact rationals|True iff `=` is true|True iff `=` is true|True iff `=` is true|True iff `=` is true|
|Exact non-rationals|True iff `=` is true|True iff `=` and not distinguishable by Scheme arithmetic|True iff `=` is true|True iff `=` is true|
|Inexact numbers except NaNs|True iff `=` is true|True iff `=` and not distinguishable by Scheme arithmetic|True iff `=` is true|
|!NaNs|Unspecified|False|Unspecified|???|
|Ports|Unspecified|True iff contain the same locations|Unspecified|True iff contain the same locations|
|Empty list|True (only one exists)|True (only one exists)|True (only one exists)|
|Eof-objects|Unspecified|True (only one exists)|Unspecified|Unspecified|
