# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

## WG1 - Core

### #460 Semantics of `eqv?`

The semantics of `eqv?` are highly contended, and as a result we are
bringing this up for a third and final vote.

The earlier votes on #125 (`eqv?` may not return true for procedures)
and #229 (`eqv?` is unspecified for [NaNs](NaNs.md)) were voted on and treated as
orthogonal.  There have been no objections to these, and so the
results still stand.  We're only focusing on the core `eqv?` semantics
for inexact numbers.

This is fundamentally a tale of three equivalences: mathematical,
operational, and representational.

```
Ultra-brief history:

R2RS was purely mathematical, defining `eqv?` on numbers in terms of
`=`.  R3RS defined a simple operational equivalence by distinguishing
exact and inexact.  This was not a complete operational equivalence
because R3RS already had multiple precisions and implicit NaNs and
signed zeros.  R[45]RS dropped the discussion of operational
equivalence but kept the exactness separate.  R6RS re-introduced the
notion of operational equivalence, this time with a complete definition.
The R7RS 7th draft introduced an incomplete notion of representational
equivalence - two numbers are `eqv?` iff they have the same IEEE-style
representation.
```

For this final vote there are three proposals under consideration,
corresponding to the complete forms of each equivalence.  Variations
may be proposed and added if a suitable rationale is given.

The `r5rs` proposal follows R5RS exactly, in the spirit of
mathematical equivalence (assuming exacts and inexacts are different
types to begin with).  The advantage of this is it's very simple and
it appeals to idealists - people who want to pretend that they are
computing real mathematical values without such thing as [NaNs](NaNs.md) or
negative zeros.  The disadvantage of this is, as with most things that
appeal to idealists, it does not match reality.  Our computers use
crude hacks for efficiency, and even if someone manages to build an
ideal Scheme, it will likely be impractical and most implementations
will continue to use those hacks.  Moreover, mathematical equivalence
is already available via the `=` procedure.  This is not a practical
equivalence relation for a standard.  The text for the true case of
`eqv?` for inexact numbers under `r5rs` is:

```
  (3) obj1 and obj2 are both numbers, have the same exactness, and are
  numerically equal (see `=`).
```

The `r6rs` proposal follows R6RS exactly in the spirit of operational
equivalence (with a small correction to avoid making everything
unspecified via NaN transitivity).  The advantages of this is that
it's exactly what you want to distinguish if two values will always
behave the same, for example for compiler optimizations or
memoization.  The disadvantage is that the definition is complicated
and difficult to nail down - it doesn't account for non-standard
extensions an implementation may provide which could distinguish
certain new values.  The `r6rs` text is:

```
  (3.1) obj1 and obj2 are both exact numbers and are numerically
  equal (see `=`)

  (3.2) obj1 and obj2 are both inexact numbers, are numerically equal
  (see `=`), and yield the same results (in the sense of `eqv?` and
  excluding `+nan.0`) when passed as arguments to any other procedure
  that can be defined as a finite composition of Scheme’s standard
  arithmetic procedures.
```

Finally, the `representational` proposal is based on the previous
`same-bits` in the spirit of representational equivalence.  Two
numbers are `eqv?` if they are represented the same way.  This is
potentially finer grained than operational equivalence - it may in
fact make useless distinctions, but it is generally safer to
over-distinguish than to under-distinguish.  The `representational`
text is:

```
  (3.1) obj1 and obj2 are both exact numbers and are numerically
  equal (see `=`)

  (3.2) obj1 and obj2 are both numbers of the same composite numeric
  type, whose corresponding fields are all `eqv?`

    * numbers in the style of the IEEE 754-2008 floating point
      standard are considered composites of their radix, precision,
      maximum exponent, sign, exponent, and significand

    * non-real complex numbers are composites of their real and
      imaginary parts
```

* **References:**
* [eqv? issues summarized](https://groups.google.com/d/msg/scheme-reports-wg1/BGvDFtD6A1M/5pHmfXHtvEIJ)
* [the history of eqv? on numbers](https://groups.google.com/d/msg/scheme-reports-wg1/2Nv6oIND8HI/Z2HXPQMNFooJ)
* [Weaver's objection](http://lists.scheme-reports.org/pipermail/scheme-reports/2012-November/002914.html)
* [Lucier on IEEE 754](http://www.math.purdue.edu/~lucier/r7rs-eqv-discuss)
* **Options:** r5rs, r6rs, representational
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs, representational, r5rs
* [Ganz](WG1BallotGanz.md): r6rs, representational, r5rs
* [Gleckler](WG1BallotGleckler.md): representational, r6rs, r5rs
* [Hsu](WG1BallotHsu.md): r6rs, representational, r5rs
* [Lucier](WG1BallotLucier.md): representational, r6rs, r5rs
* [Medernach](WG1BallotMedernach.md): r6rs, representational, r5rs
* [SnellPym](WG1BallotSnellPym.md): r6rs, representational, r5rs

* **Results:** r6rs, representational
* **Ratios:** 4:3
* **Rationales:**

`Cowan`::
> I think the `r5rs` choice is Just Wrong as long as IEEE is the dominant representation of inexact numbers: there needs to be a way to distinguish between `0.0` and `-0.0` for memoization and other purposes, as they behave differently for some arithmetic procedures.  For IEEE (always excepting NaNs, which are implementation-dependent), `r6rs` and `representational` mean the same thing: with only one bit pattern per operationally distinct number, `representational` can't discriminate between them.  In the non-IEEE space, I think the `r6rs` choice is more principled, but I can live with `representational`, though it may end up discriminating between non-IEEE numbers that aren't really different.
`Gleckler`::
> The representational definition clearly and concretely defines what an implementer should do to implement `eqv?` for inexact numbers, which makes it better than the r6rs option, and it takes into account issues like precision, which makes it better than the r5rs option.  The r6rs option is better than the r5rs option for the same reason.  Thus my ordering of preferences.
`Hsu`::
> I want operational equivalence in as implementation neutral a fashion that abstracts as much as possible away from implementational details. While r6rs may appear more subtle, the fact that it does not define itself in terms of implementation details directly is a plus for me.
`Lucier`::
> The representational definition clearly and concretely defines what an implementer should do to implement `eqv?` for inexact numbers, which makes it better than the r6rs option, and it takes into account issues like precision, which makes it better than the r5rs option.  The r6rs option is better than the r5rs option for the same reason.  Thus my ordering of preferences.
`Medernach`::
> IMHO the only sensible choice is operational equivalence, at the end of the day it is all about one implementation being consistent in its choice of equivalence classes. This left an open door for implementation and at the same time avoid inconvenience to users.  **representational** is maybe too low-level, I mean the underlying concept here is about equivalence class and I would let implementors freedom/discretion of how they check for it. Again and of course de-composition is the way to go most of the time. I am sure the 2 proposals are not too far from each other.  I suggest therefore that an editorial note may be added to the **r6rs** option saying something along the line of that: *One possible implementation is to check subpart eqv?-eness if it is consistent with equivalence classes wished to be provided by the implementation.*
`SnellPym`::
> I think that r6rs and representational will be equivalent in most implementations; certainly, I think an implementation should strive as best it can to provide operation equivalence of numbers, and in bizarre cases where it cannot (numbers represented as infinite lazy streams, perhaps? `(exact? pi)` might be fun), I'd rather that the implementation state that it's not completely R7RS conformant in this case (I'm sure this won't be the *only* numeric semantic surprise such an implementation has, and probably not even the biggest...) than be forced to go for some ugly implementation-exposing hack.
