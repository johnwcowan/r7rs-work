= Instructions =

    * You may list as many of the options as you want in order of preference.
    * Options are comma-delimited (ignoring space) and case-insensitive.
    * You can pipe-delimit (|) options you want to give equal weight to.
    * You may write in your own option if you announce it to the list first.
    * You may specify a variant with option/variant, for example srfi-1/module to vote for srfi-1 but clarify it should be in a separate module. Please also include the srfi-1 option in this case.
    * You can write a free-form rationale after the "preferences" line,
    * module means "yes, but I want it in a separate module",
    * wg2 means "no, but I think it should go in WG2".
    * undecided means I want to discuss this issue further.
    * Abstain on any item by leaving the preferences blank. 

= WG1 Ballot Items To Finalize By Sep. 18 =

== WG1 - Core ==

=== #121 The semantics of expt for zero bases has been refined ===

The R5RS definition of expt is:

{{{
 -- procedure: expt z1 z2
     Returns Z1 raised to the power Z2.  For z_1 ~= 0

                          z_1^z_2 = e^z_2 log z_1

     0^z is 1 if z = 0 and 0 otherwise.
}}}

however exponents with negative real parts are undefined.
R6RS attempted to clarify this with:

{{{
     0.0^z is 1.0 if z = 0.0, and 0.0 if (real-part z) is positive.
     For other cases in which the first argument is zero, either
     an error is signalled or an unspecified number is returned.
}}}

(Ignore the change in exactness, which was strictly editorial
and the examples clarify that the rules ignore exactness.)

This is unique in all the reports of a result either
signalling an error or returning a value.  The motivation
for this was because R6RS consistently removed uses of the
"is an error" terminology which would more naturally fit
this situation.

An alternative, `r5rs-error`, is to restore the "is an error"
text since we are not avoiding this in R7RS:

{{{
     0.0^z is 1.0 if z = 0.0, and 0.0 if (real-part z) is positive.
     For other cases in which the first argument is zero, either
     an error is signalled or an unspecified number is returned.
}}}

  * '''Options:''' r5rs, r5rs-error, r5rs-error/real, r6rs, r6rs/real, undecided
  * '''Default:''' r6rs
  * '''Preferences:''' r5rs-error/real, r5rs-error, r5rs, r6rs/real, r6rs

The entire rationale for R6RS not using this option
doesn't apply to R7RS.

=== #472 clarify semantics of non-library library declarations ===

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

  * '''Options:''' declaration, syntax, both, remove
  * '''Default:''' 
  * '''Preferences:''' remove, syntax, both, declaration

With the confusion I'd just as soon remove these.
If we're going to have it, it's more useful as syntax
(as the original commenter wanted), and it encourages
better encapsulation to force declarations into libraries.

=== #473 library declaration locations in top-level ===

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

  * '''Options:''' r6rs, beginning-only, any-top-level
  * '''Default:''' 
  * '''Preferences:''' r6rs, beginning-only, any-top-level

If applicable we should strive for at least this much
compatibility with R6RS.  Otherwise, we definitely
should not allow `any-top-level` which defeats the
purpose of having a static library system.

=== #405 Retract language requiring force to accept non-promises ===

#405 lumped together several issues, one of which was a requirement
(as opposed to an option) to make `force` applied to a non-promise
return its argument, as opposed to it being an error.  Thus, it would
require `(force 2) => 2`.  However, R6RS
requires `(force 2)` to signal an error, and many non-R6RS Schemes also
signal an error (see ForceNonPromise for details).  These facts were not
considered at the time.

Vote `retain` to retain this requirement, or `retract` to retract it
and leave the result of `(force 2)` implementation-dependent.

  * '''Options:''' retain, retract
  * '''Default:''' retain
  * '''Preferences:''' retract

This was just an oversight when the item was originally
proposed - there's no grounds to require this.
