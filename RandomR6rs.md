This is a dumping ground for random R6RS base library procedures that might be wanted in WG2.

`boolean=?` compares booleans for identity.  **Added to R7RS-small.**

`symbol=?` compares symbols for identity.  **Added to R7RS-small.**

`real-valued?`, `rational-valued?`, and `integer-valued?` test whether a given number object can be coerced to the specified type without loss of numerical accuracy. Specifically, the behavior of these predicates differs from the behavior of `real?`, `rational?`, and `integer?` on complex number objects whose imaginary part is inexact zero.  **Rejected for R7RS-small.**

`infinite?` returns `#t` on `+inf.0` and `-inf.0`  **Added to R7RS-small.**

`assert` raises an error if its argument is `#f`.  **Rejected for R7RS-small.**

There are now [RevoteDocket](RevoteDocket.md) entries for the `assert` and `*-valued` procedures.

