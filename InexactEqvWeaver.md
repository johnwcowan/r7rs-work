## Proposed R7RS definition for `eqv?` on numbers

[[477 for detailed background](See)

Objects *obj,,1,,* and *obj,,2,,* are *substantially different* if and only if one of the following holds:

* *Obj,,1,,* and *obj,,2,,* are both numbers, at least one is numerically equal to itself (see `=`), and they are not numerically equal (see `=`) to each other.

* *Obj,,1,,* and *obj,,2,,* are not both numbers, and they are different (in the sense of `eqv?`).

Inexact numbers *z,,1,,* and *z,,2,,* are *operationally equivalent* if and only if for all procedures *f* that can be defined as a finite composition of the standard numerical operations specified in section 6.2.6, `(f z1)` and `(f z2)` either both raise exceptions or yield results that are not substantially different.

The `eqv?` procedure returns `#t` if one of the following holds:

[...]

* *Obj,,1,,* and *obj,,2,,* are both exact numbers and are numerically equal (see `=`).

* *Obj,,1,,* and *obj,,2,,* are both inexact numbers, at least one is numerically equal to itself (see `=`), and the implementation is able to prove that *obj,,1,,* and *obj,,2,,* are operationally equivalent.  Implementations must be able to prove that two inexact numbers with the same internal representation are operationally equivalent.

The `eqv?` procedure returns `#f` if one of the following holds:

[...]

* One of *obj,,1,,* and *obj,,2,,* is an exact number but the other is an inexact number.

* *Obj,,1,,* and *obj,,2,,* are exact numbers for which the `=` procedure returns `#f`.

* *Obj,,1,,* and *obj,,2,,* are inexact numbers, at least one is numerically equal to itself (see `=`), and the implementation is unable to prove that *obj,,1,,* and *obj,,2,,* are operationally equivalent.

## Rationale for the above definition

The novel feature of this definition is the auxiliary predicate *substantially different*, which is needed to gracefully avoid circularities and the problems associated with NaNs, both of which plagued the R6RS definition.

The circularity problem is addressed by defining substantially different on numbers in terms of `=` instead of `eqv?`.  The NaN problem (see below) is addressed by making sure that two numbers can only be substantially different from each other if at least one of them is `=` to itself.

Note that there is considerable freedom in how "substantially different" is defined.  As long as it is capable of making the most coarse distinctions between numbers, that's good enough, because it should always be possible to choose a procedure *f* that amplifies even the finest distinction between any two inexact numbers that are not operationally equivalent.

For example, suppose that in addition to the usual `+0.0` and `-0.0`, an experimental numeric representation was able to distinguish (x/y+0.0) from (x/y-0.0) for any exact rational x/y.  It would still be possible to amplify that distinction to an infinite difference by subtracting x/y and then taking the reciprocal.

Note also that there is considerable freedom in the choice of procedures to allow in the construction of *f*.  The main requirements are that they are sufficient to amplify arbitrary fine distinctions into coarse ones that are substantially different, and that the procedures are pure functions, i.e. they must not use `eqv?` or `eq?` (directly or indirectly), they must not cause visible side effects, and their return values must depend only on their arguments.  It needn't be a comprehensive set.

## Nontrivial changes in the formulation of `eqv?` since R6RS

* **Nontrivial change**: "rational numbers" to "exact numbers" in the clause requiring `#f` if "Obj,,1,, and obj,,2,, are rational numbers for which the `=` procedure returns `#f`."

> Note: This clause was redundant, but is kept for the case of exact numbers, so that we may restrict our definition of the predicate "operationally equivalent" to inexact numbers only.

* Stylistic change: Move the following language into the new
> auxiliary predicate *operationally equivalent*:

> "[...] yield {the same,different} results (in the sense of `eqv?`) when passed as arguments to any other procedure that can be defined as a finite composition of Scheme's standard arithmetic procedures"

* **Nontrivial change**: Restrict use of the predicate "operationally equivalent" to cases where both arguments are inexact.

* Stylistic change: Move the "same results (in the sense of `eqv?`)" language into the new auxiliary predicate *substantially different*.

* **Significant change**: Eliminate the circularity by changing the definition of "substantially different" for two numbers to use `=` instead of `eqv?`.

* **Significant change**: Fix the NaN problem by making sure that two numbers can only be substantially different if at least one of them is numerically equal to itself.

* **Significant change**: Restrict the set of procedures that can be used to construct *f*, and use the R7RS terminology, changing "Scheme's standard arithmetic procedures" to "the standard numerical operations specified in section 6.2.6"

* **Significant change**: Properly handle the case where `(f z_1)` or `(f z_2)` raise exception(s).  Change "`(f z_1)` and `(f z_2`) yield results that are not substantially different" to "`(f z_1)` and `(f z_2)` either both raise exceptions or yield results that are not substantially different."

* **Significant change**: Make sure that the case where both arguments are [NaNs](NaNs.md) is left unspecified by requiring, in the inexact clauses, that at least one argument is numerically equal to itself.

* Simplification: Remove the redundant check for numerical equality as a prerequisite for `eqv?` returning `#t`.

> Rationale: If *obj,,1,,* and *obj,,2,,* are not numerically equal (and not both NaNs), it trivially follows that *obj,,1,,* and *obj,,2,,* are not operationally equivalent, since `(+ obj_1`) and `(+ obj_2)`are substantially different.

* **Significant change**: Relax the requirements for returning `#t` for `eqv?` on inexacts to the cases where "the implementation is able to prove" operational equivalence.

> We now require only that the "implementations must be able to prove that two inexact numbers with the same internal representation are operationally equivalent.

> Rationale: It may be difficult or impossible for the implementation to prove that two inexact numbers are operationally equivalent.
