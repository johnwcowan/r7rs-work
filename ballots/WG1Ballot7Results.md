# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

# WG1 Ballot Items To Finalize By Sep. 30

## WG1 - Core

### #121 The semantics of expt for zero bases has been refined

The R5RS definition of expt is:

```
 -- procedure: expt z1 z2
     Returns Z1 raised to the power Z2.  For z_1 ~= 0

                          z_1^z_2 = e^z_2 log z_1

     0^z is 1 if z = 0 and 0 otherwise.
```

however exponents with negative real parts are undefined.
R6RS attempted to clarify this with:

```
     0.0^z is 1.0 if z = 0.0, and 0.0 if (real-part z) is positive.
     For other cases in which the first argument is zero, either
     an error is signalled or an unspecified number is returned.
```

(Ignore the change in exactness, which was strictly editorial
and the examples clarify that the rules ignore exactness.)

This is unique in all the reports of a result either
signalling an error or returning a value.  The motivation
for this was because R6RS consistently removed uses of the
"is an error" terminology which would more naturally fit
this situation.

An alternative, `r5rs-error`, is to restore the "is an error"
text since we are not avoiding this in R7RS:

```
     The value of 0^z is 1 if (zero? z), 0 if (real-part z)
     is positive, and an error otherwise.  Similarly for 0.0^z,
     with inexact results.
```

The `/real` variant restricts the domain for the zero
base exception to the real numbers.  This is because
0^z^ is mathematically undefined for non-real z, and
implementations do not agree on the result.

* **Options:** r5rs, r5rs-error, r5rs-error/real, r6rs, r6rs/real, undecided
* **Default:** r6rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r5rs-error, r5rs-error/real, r5rs
* [Ganz](WG1BallotGanz.md): r5rs-error, r5rs-error/real
* [Gleckler](WG1BallotGleckler.md): r5rs-error/real, r5rs-error, r5rs, r6rs/real, r6rs
* [Hsu](WG1BallotHsu.md): r6rs, r5rs-error, r5rs
* [Medernach](WG1BallotMedernach.md): r5rs-error, r5rs, undecided, r5rs-error/real, r6rs, r6rs/real
* [Shinn](WG1BallotShinn.md): r5rs-error/real, r5rs-error, r5rs, r6rs/real, r6rs
* [SnellPym](WG1BallotSnellPym.md): r5rs-error, r5rs, r6rs
* **Results:** **r5rs-error**, r5rs-error/real, r5rs, r6rs, r6rs/real, undecided
* **Ratios:** 5:2, 7:0, 6:1, 7:0, 7:0
* **Rationales:**

`Cowan`::
> I agree that the R6RS rule makes no sense in an R7RS context. However, it's worth saying explicitly that the oddball zero cases are errors.
`Ganz`::
> This seems consistent with #367. According to Wikipedia, for pos real b, b^c^ = e^cln(b)^ (the parens may be missing in the R5RS snippet?). The zero base, non-real exponent case can be defined to return nans and we should not preclude that.
`Gleckler`::
> R7RS isn't making the is-an-error change. I'm choosing "/real" over non-"/real" because there isn't enough agreement to support the latter.
`Medernach`::
> As I understand the above text is just false: 0^0^ and 0.0^0.0^ are mathematicaly undefined, this is because it is not continuous there. Just take x^(-1/log(x))^, when x -> 0 it is equal (and therefore converges) to 1/e instead of 1 ! Provided this is changed I prefer the openness of r5rs-error. Bradley's argument convinces me to retain 0^0^=1 (i.e. only if we have an exact 0 as exponent) as a practical convention.
`Shinn`::
> The entire rationale for R6RS not using this option doesn't apply to R7RS.
`SnellPym`::
> The R6RS approach isn't applicable, and I prefer explicit errors.

### #472 clarify semantics of non-library library declarations

In items #91, #148 and #150 we voted to allow the
use of `include`, `include-ci` and `cond-expand`
at the "top-level" respectively, but there remains
some confusion as to their semantics.

Here "top-level" refers to repl and program body
top-levels, but not library bodies.

One interpretation is that these behave like library
declarations, and can expand into `import` forms.
In this case, for a purely static implementation of
R7RS libraries, they must first be statically scanned
from all top-level forms.  They cannot be used
outside the top-level, and are not even available
as bindings otherwise.  This is the `declaration`
proposal.

Another interpretation is that they are just normal
macros with the obvious definitions (cond-expand
in terms of the output of the `features` macro),
are available in the `(scheme base)` library, and
consequently can't be used to expand into `import`
since imports have already been resolved.  This is
the `syntax` proposal.

Alternately, we could provide `both`.  If you think
this is all too confusing you could also vote `remove`,
to drop these extensions.

* **Options:** declaration, syntax, both, remove
* **Default:**
* **Voters:**
* [Cowan](WG1BallotCowan.md): declaration, both, syntax, remove
* [Ganz](WG1BallotGanz.md): syntax, remove
* [Gleckler](WG1BallotGleckler.md): remove, syntax, declaration
* [Hsu](WG1BallotHsu.md): syntax, remove, both, declaration
* [Medernach](WG1BallotMedernach.md): syntax, remove, declaration, both
* [Shinn](WG1BallotShinn.md): remove, syntax, both, declaration
* [SnellPym](WG1BallotSnellPym.md): declaration, syntax, both, remove
* **Results:** **syntax**, remove, declaration, both
* **Ratios:** 5:2, 5:2, 6:1
* **Rationales:**

`Cowan`::
> `Declaration` is the option that makes sense to me, *without* however permitting declarations in included files (they are currently forbidden). I see no reason in these cases to make a distinction between library bodies on the one hand and programs and REPLs on the other. The `syntax` option allows them to be used in random nested places, which I consider to be unnecessary.
`Ganz`::
> I don't like the idea of forms being "inherently" top-level only.
`Gleckler`::
> There's just too much confusion in this area.
`Hsu`::
> These are common and useful forms, but having them as a separate declaration form, especially for `include` and the like, is very confusing IMO, especially for implementations that will choose to provide a syntactic `include` nonetheless.
`Shinn`::
> With the confusion I'd just as soon remove these. If we're going to have it, it's more useful as syntax (as the original commenter wanted), and it encourages better encapsulation to force declarations into libraries.
`SnellPym`::
> "declaration" seems the simplest. "both" seems the most complex. "remove" seems to be throwing the baby out with the bathwater.

### #473 library declaration locations in top-level

R6RS allows only a single library declaration, `import`,
at the beginning of a program body, and this must
contain all imported libraries.

Pending the result of ticket #472 we may also allow
`include(-ci)` and `cond-expand` to expand into
imports, and so the single form restriction would not
make sense.  However, it would be reasonable to
restrict all library declarations to the beginning of
a program - the first non-declaration would begin
the real body.  This is the `beginning-only` option.

The advantage of the `r6rs` proposal is that it would
not require any changes in existing R6RS program
loading implementations to support.  If the result of
ticket #472 indicates multiple declaration types are
available this option would automatically become
invalid, so you don't need to vote against it on those
grounds.

The advantage of the `beginning-only` option is
that it becomes possible to statically determine
all program imports without expansion, which was
the primary motivation of a static library system.

The final alternative is `any-top-level`, which
allows these forms anywhere at the top-level,
possibly interspersed with definitions.  The advantage
of this is that you can cut&paste repl sessions
(for which interspersed imports are always allowed)
as a program.  The disadvantage is that programs
can no longer be resolved separately from expansion.

* **Options:** r6rs, beginning-only, any-top-level
* **Default:**
* **Voters:**
* [Cowan](WG1BallotCowan.md): beginning-only, any-top-level, r6rs
* [Ganz](WG1BallotGanz.md): any-top-level, beginning-only
* [Gleckler](WG1BallotGleckler.md): r6rs, beginning-only
* [Hsu](WG1BallotHsu.md): any-top-level, beginning-only
* [Medernach](WG1BallotMedernach.md): beginning-only, r6rs, any-top-level
* [Shinn](WG1BallotShinn.md): r6rs, beginning-only, any-top-level
* [SnellPym](WG1BallotSnellPym.md): beginning-only, any-top-level, r6rs
* **Results:** **beginning-only**, any-top-level, r6rs
* **Ratios:** 5:2, 5:2
* **Rationales:**

`Cowan`::
> Note that this is about programs only, not REPLs or library bodies. I really, really dislike both `any-top-level` and `beginning-only`. The first is too flexible, the second, not flexible enough. Very reluctantly I choose `beginning-only` because it preserves static analysis. I see no benefit to the `r6rs` option at all, given that R6RS systems will have to provide additional support for R7RS library syntax anyway.
`Ganz`::
> I think that import should generally act like a multi-define, and so should be usable like a top-level define. The question of redefining import is a separate one, and should be discussed separately.
`Gleckler`::
> As long as we're only restricting what the standard supports but are not restricting how implementations may extend their own implementations, I'm fine with this. In that case, preserving R6RS compatibility is a good idea.
`Shinn`::
> If applicable we should strive for at least this much compatibility with R6RS. Otherwise, we definitely should not allow `any-top-level` which defeats the purpose of having a static library system.
`SnellPym`::
> beginning-only is a conservative minimum to require; implementations might choose to be more flexible without becoming incompatible.

### #405 Retract language requiring force to accept non-promises

#405 lumped together several issues, one of which was a requirement
(as opposed to an option) to make `force` applied to a non-promise
return its argument, as opposed to it being an error.  Thus, it would
require `(force 2) => 2`.  However, R6RS
requires `(force 2)` to signal an error, and many non-R6RS Schemes also
signal an error (see [ForceNonPromise](ForceNonPromise.md) for details).  These facts were not
considered at the time.

Vote `retain` to retain this requirement, or `retract` to retract it
and leave the result of `(force 2)` implementation-dependent.

* **Options:** retain, retract
* **Default:** retain
* **Voters:**
* [Cowan](WG1BallotCowan.md): retract
* [Ganz](WG1BallotGanz.md): retain
* [Gleckler](WG1BallotGleckler.md): retract
* [Hsu](WG1BallotHsu.md): retract
* [Lucier](WG1BallotLucier.md): disjoint
* [Medernach](WG1BallotMedernach.md): retract
* [Read](WG1BallotRead.md): disjoint
* [Shinn](WG1BallotShinn.md): retract
* **Results:** **retract**, disjoint, retain
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> I can't see forcing all R6RS systems into non-compliance over this small point.
`Ganz`::
> If a programmer needs to know what is and is not a suspension before forcing it, suspensions are not that different from thunks (so why bother). It should be possible for a portable program to be lazy (sorry) and not have to worry about whether something is a suspension or not. This requirement does not break any programs, and there is no other reasonable value to return. Also, extending forcing in this way seems consistent with the implicit forcing that occurs on primitive application.
`Gleckler`::
> There isn't enough agreement among implementations to impose the new requirement.
`Shinn`::
> This was just an oversight when the item was originally proposed - there's no grounds to require this.
