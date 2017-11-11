# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

# WG1 Ballot Items To Finalize By Jul. 6

## WG1 - Core

### #125 Allow procedures not to be locations (making EQV? unspecified in some additional cases)

* **Options:** r6rs, r5rs, undecided
* **Default:** r5rs
* **Results:** r5rs

By unanimous consent, the WG returned to the R5RS semantics.

### #467 Allow eqv? and eq? to return different answers on procedures as well as integers and characters

This proposal stems from [remarks](http://lists.r6rs.org/pipermail/r6rs-discuss/2012-July/006405.html) by Alaric Snell-Pym and Will Clinger on the r6rs public mailing list.  If `eq?` is allowed to return `#f` on two procedures when `eqv?` nevertheless returns `#t`, as is already the case for numbers and characters, then more intelligent implementation-specific procedure comparisons using `eqv?` are possible, while still keeping `eq?` simple enough to inline easily.

Note that this is orthogonal to the question of #460, how `eqv?` works on procedures.  There should be little or no backward-compatibility hit for this change.

* **Proposals:**
* **same:** `eq?` and `eqv?` always return the same on procedures, per R5RS and R6RS
* **different:** `eq?` may return `#f` on procedures even when `eqv?` returns `#t` (but not vice versa)
* **Options:** same, different, undecided
* **Default:** same
* **Results:** different

By unanimous consent, the WG adopted the following partial definition of the behavior of `eq?` on procedures:

> On procedures, `eq?` must return true if the arguments' location tags are equal.


