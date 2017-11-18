# Instructions

* You may list as many of the options as you want in order of preference.
* Options are comma-delimited (ignoring space) and case-insensitive.
* You can pipe-delimit (|) options you want to give equal weight to.
* You may write in your own option if you announce it to the list first.
* You may specify a variant with option/variant, for example srfi-1/library to vote for srfi-1 but clarify it should be in a separate library.
* You can write a free-form rationale after the "preferences" line,
* library means "yes, but I want it in a separate library",
* wg2 means "no, but I think it should go in WG2".
* undecided means I want to discuss this issue further.
* Abstain on any item by leaving the preferences blank.

# WG1 Ballot Items To Finalize By Mar. 31

## WG1 - Core

### #229 Are NaN values EQV?

We voted that `eqv?` return `#t` if both arguments are any value which
writes as `+nan.0`.  The description of this item was ill-formed and
confusing, as objected to in:

http://lists.scheme-reports.org/pipermail/scheme-reports/2011-September/001507.html

We therefore are re-opening the item, with amended descriptions.

The `different` proposal is that we add a single clause requiring
`(eqv? +nan.0 x)` to return `#f` for any `x`.  This is the behavior
that results for any R5RS implementation that adds support for +nan.0
as an IEEE float without any special handling for it in `eqv?`.

The `unspecified` proposal is to make the results explicitly unspecified,
as specified in R6RS.

The `same` proposal, contrary to both standards, is that we add a clause to
the definition of `eqv?` saying that if both arguments are NaN
values with the same bit pattern, `eqv?` must return `#t`.  Thus `eq?`
implies `eqv?`.  However, if two values both print as `+nan.0` they
may or may not be `eqv?`.  This also requires additional checks for
floating point comparisons.

Testing with `(equal? (/ 0.0 0.0) (/ 0.0 0.0))` to get the same
bit pattern but non-object-identity, we get the following results:

The following 8 implementations return #t: Chez, Gambit, Guile, Ikarus/Vicare, Kawa, Larceny, Racket, STklos.

The following 6 implementations return #f: Bigloo, Chibi, Chicken, Gauche, MIT Scheme, Scheme48.

[SigScheme](SigScheme.md) and Scheme 9 don't have `+nan.0`. SISC currently has a bug
where `(= nan.0 x)` is true for any `x`.


* **Options:** same, different, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #275 Support -nan.0 as a synonym for +nan.0

Excluding `-nan.0` was an oversight, and it's gratuitously
incompatible with R6RS as well as current practice.  Racket, Gauche,
Chicken, Guile, Chez, Ikarus, Larceny, Ypsilon, STklos all support
`+nan.0` and `-nan.0` as equivalent forms.  MIT, Bigloo, Scheme48/scsh,
SISC, SCM, Scheme 9 don't support either form.  Only Gambit and Chibi
support `+nan.0` but not `-nan.0`.

STklos prints both `+nan.0` and `-nan.0` as `-nan.0`.

Vote `yes` to allow `-nan.0`, `no` to disallow it.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #278 Shrink division routines to just truncate and floor

Bradley Lucier says:

I don't see the `centered-*` operators as somehow a "completion" of
the other division operators.  In the small language I'd recommend
only the `truncate-*` and `floor-*` operators for two reasons: they
are the only division operators that have an established history of
use in computer programming and mathematics, and they form a minimal
extension of R5RS.  (I'm not saying that the other division operators
have never been used in mathematics or programming (see CL), but small
Scheme is not supposed to be a kitchen-sink language.)

Vote `shrink` to prune to `truncate-*` (R5RS) and `floor-*` (R5RS `modulo`), moving
the extra operators to the large language; `shrink/core` to do the same as `shrink`
but move the remaining operators to the core language; or `keep` to keep all 18
division operators in the small language.

* **Options:** shrink, shrink/core, keep, undecided
* **Default:** keep
* **Preferences:**

### #280 Make vectors self-quoting

Currently vectors are the only type represented by a readable datum
that are neither self-quoting nor meaningful Scheme expressions
(i.e. symbols and lists).  The proposal is to make them
self-quoting as well.

Currently Racket, Gauche, MIT, Guile, Kawa, Chibi, SCM, STklos, Scheme
9, Scheme 7, UMB, VX, Oaklisp treat vectors as self-quoting.

Gambit, Chicken, Bigloo, Scheme48/scsh, SISC, Ikarus, Larceny,
Ypsilon, IronScheme, Mosh, KSi, SigScheme, Elk treat unquoted
vectors as errors.

Vote `yes` to make them self-quoting, `no` to make it an explicit
error, or `unspecified` to leave unspecified as in R5RS.

The only other reasonable alternative semantics for this unspecified
case would be to treat #(...) as (vector ...) (i.e. in contrast to this proposal
to evaluate the contents rather than quoting them).  No known
implementations make this extension, and it is dubious due to the
fact that it makes what appears to be quoted data to be evaluated,
and so is not listed as an option.  The possibility of this extension,
however, could serve as an argument to leave it unspecified.

* **Options:** yes, no, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #282 Map and friends should call their procedures in the same dynamic environment

The specifications of `map`, `for-each`, and other procedures that
accept a procedure as an argument and call it, should specify that the
argument procedures will always be called in the dynamic environment
of the call to `map`, `for-each`, etc.

This is an R6RS fix.

Vote `yes` to add the clarification and `no` to leave it out.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #283 Initial characters in non-ASCII identifiers should exclude digits and combiners

Identifiers beginning with a character of type Nd, Mc, or Me should be
forbidden.  This is an R6RS issue.

Nd is a numeric character, which in the case of ASCII 0-9 is already
forbidden, but currently unspecified for non-ASCII digits.

Mc and Me are enclosing marks and spacing combining marks respectively, which are logically attached to the preceding character.

Vote `yes` to forbid (which would still allow this as an
implementation-dependent extension for either numbers or symbols).

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #285 R6RS base compatibility: symbol=?

This is equivalent to `eq?` on symbols, and provides R6RS base
compatibility as well as completing the set of type-specific
comparisons.  See also #316.

Vote `yes` to add this procedure.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #286 Numeric *-valued procedures for R5RS and R6RS-base compatibility

`Real-valued?`, `rational-valued?`, and `integer-valued?` test whether
a given number object can be coerced to the specified type without
loss of numerical accuracy.  They are equivalent to the versions of
`real?`, `rational?`, and `integer?` that exist in R5RS.

Specifically, the behavior of these predicates differs from the
behavior of `real?`, `rational?`, and `integer?` on complex number
objects whose imaginary part is inexact zero.

These procedures provide R6RS base compatibility as well.

* Vote `yes` to add `*-valued` procedures;
* Vote `no` to leave out the `*-valued` procedures;
* Vote `r5rs` to leave them out *and* revert `real?`, `rational?`, and `integer?` to R5RS semantics
* vote `r5rs+strictly` to do what `r5rs` does, and also add `strictly-*?` procedures to provide the R6RS semantics of `real?`, `rational?`, and `integer?`.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #287 R6RS base compatibility: assert

`Assert` raises an error if its argument is `#f`.  This provides R6RS
base compatibility.

Vote `basic` to add this syntax.  Vote `optionals` to make `assert` optionally accept, after its
expression argument, a single `message` argument and zero or more `irritant` arguments
in the same manner as the `error` procedure.  Vote `no` in order not to add `assert`.

* **Options:** basic, optionals, no, undecided
* **Default:** no
* **Preferences:**

### #288 R6RS base compatibility: infinite?

`Infinite?` returns `#t` if its value is a real number, or if its
value is a complex number and either the real or the imaginary part
would return `#t` to `infinite?`.  This provides R6RS base
compatibility, with extensions for complex numbers analogous to that
provided by `finite?` and `nan?`.

This was in the draft at one point, but was never actually voted on,
so the editors removed it.

Vote `yes` to add this procedure.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

## WG1 - Numerics

### #290 Proposed square procedure

Bradley Lucier writes (lightly edited):

A `square` primitive is useful in calculating with bignums because
squaring a bignum is generally cheaper than multiplying two different
bignums of the same size. For example, Gambit's runtime checks
trivially whether the two arguments in `(* a b)` are `eq?` before
calling the appropriate algorithm.  Generally, it may be better to be
able to express this primitive directly.

[[He|also points out that given `square` in the small language, we can
have `flsquare` in the large language, though having the
latter doesn't actually require having the former.]]

In addition, there are 20,340 Google hits for [(square x)" ss|scm]("(define).

Vote `yes` to add this procedure.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

## WG1 - Core

### #291 Require an error to be signalled if input files cannot be opened

For `with-input-from-file`, `with-output-to-file`,
`call-with-input-file`, `call-with-output-file`, R5RS just says that
the file should exist.  However, `open-input-file` requires an error
to be signalled if the file cannot be opened, whether because it does
not exist for some other reason like the lack of permissions.  This
inconsistency doesn't seem useful.

The proposal is to change these wrapper procedures to also require an error
to be signalled if the file cannot be opened.  All major Schemes
already implement this.

Vote `yes` to require signalling an error if the files cannot be opened.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #292 Add case-insensitive normalization-insensitive comparisons

mdmkolbe writes on Slashdot:

Given that on a system with Unicode, you almost never want to do a
non-normalizing case-insensitive match and that it is hard for a user
to efficiently implement their own normalizing case-insensitive match,
it seems an odd corner of the rectangle to omit.

(end quotation)

Alternatively we could specify that `-ci` procedures always normalize,
or that `-ni` procedures are always case-insensitive, since the
details of the normalization are not exposed anyway.

* **Proposals:**
* **normalize-ci:** specify that *-ni procedures normalize their arguments
* **case-fold-ni:** specify that *-ni procedures case-fold their arguments
* **ci-ni:** add new *-ci-ni procedures that perform both operations
* **none:** leave as-is, although *-ni may still fold
* **remove:** remove the *-ni procedures altogether
* **remove+normalize-ci:** remove *-ni procedures, allow *-ci procedures to normalize
* **Options:** normalize-ci, case-fold-ni, ci-ni, remove, none
* **Default:** none
* **Preferences:**

### #293 Make it an error for <test> values to return other than one value

Currently nothing is said about the <test> of `if`, `cond`, `and`,
`or`, etc. returning zero values or multiple values.  The proposal is
to make this an explicit error.  Remember that this does not mean an error is
*signalled*.

Vote `yes` to make an explicit error.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #294 Make it an error for the <expression> of a set! to return other than one value

Currently nothing is said about what happens if the <expression> of a
`set!` returns zero values or multiple values.  The proposal is to make this
an explicit error.  Remember that this does not mean an error is
*signalled*.

Vote `yes` to make an explicit error.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #295 Make it an error for <init>s in binding forms to return other than one value

Right now nothing is said.  The proposal is to make this
an explicit error.  Remember that this does not mean an error is
*signalled*.

Vote `yes` to make an explicit error.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #297 Removing case-folding flags

The case-folding flags `#!fold-case` and `#!no-fold-case` are the only
reader flags in the draft, however their need is reduced (though not
eliminated) by the library declaration `include-ci`.  Do we still need
flipflop flags to turn case-folding on and off in part of a file?

If we remove these we maintain backwards compatibility with R5RS
library code, however we lose the ability to support R5RS programs or
toggle case-folding in the REPL or data files, etc.

* **Options:** keep, remove, undecided
* **Default:** keep
* **Preferences:**

### #303 "lazy" is a confusing name

[on feedback from Marc Feeley.](Based)

`delay` and `force` were simple balanced concepts, but the
introduction of `lazy` somewhat confuses the issue - when is `delay`
appropriate and when is `lazy`?  A simple solution would be to rename
`lazy` to `delay-force`, indicating it is simply the composition of
`delay` and `force`, and letting people see directly in code the
balance of `delay`s and `force`s.

* **Options:** delay-force, lazy, undecided
* **Default:** lazy
* **Preferences:**

### #304 symbol literal syntax wastes characters

[on feedback from Marc Feeley.](Based)

Currently symbols can either be delimited with pipes |...|
with optional hex escapes inside, or include hex escapes
directly without the pipes.  This wastes two characters
that were reserved in R5RS, the pipe and the backslash,
when either one by itself would be sufficient to represent
all symbols.  This is especially unfortunate because both
characters are used as extensions in various Schemes -
the pipe being another symbol character in SCSH (to
represent shell-style pipes and C-style operators) and
the backslash used in Gambit's infix syntax.  We should
reconsider if we really need to take up both of these
characters.

We can also consider new sequences, for instance \|...|
with optional hex escapes inside uses only \, has the
readability advantages of |...|, and still leaves room for
other \ escapes since the following | character is required.
However, such new sequences have no existing support
among implementations.

* **Proposals:**
* **delimited-only:** |...| syntax with internal escapes, \ outside is undefined, Gambit-compatible
* **backslash-only:** \xNN; only, with | valid in identifiers, SCSH-compatible
* **both:** both as in the current draft
* **neither:** remove both
* **backslash-delimited:** \|...| syntax with internal escapes
* **Options:** delimited-only, backslash-only, both, neither, backslash-delimited, undecided
* **Default:** both
* **Preferences:**

### #305 Should we move the c...r and c....r procedures into a new library?

They have been required for a long time, but Alex Shinn says:

I definitely think everything but the one and two depth combinations
should be removed from `(scheme base)`.  Their use is generally a code
smell.  People should use destructuring, records, or SRFI-1
`first..tenth` accessors.

Ray Dillinger (Bear) adds:

The historic use of these entities was as accessors for structured
aggregates implemented with cons cells.  In a language that directly
supports records, they have a reduced mission.

Vote `base` to keep all in the base library or `library` to move the 3- and 4-letter accessors to a separate library.

* **Options:** base, library, remove, undecided
* **Default:** base
* **Preferences:**

### #307 "eager" is a confusing name

[on feedback from Marc Feeley](Based)

The `eager` procedure is named particularly unfortunately because it
sounds as though it is in some way paired with `lazy`, and there is
anecdotal evidence it was voted in on this misunderstanding.  In fact,
it is completely unrelated to `lazy`, being just a utility procedure
that has never been seen used in practice.  Perhaps a better name for
it would be `promise` or `make-promise`, since it just creates an
(already computed) promise value.

Vote `eager`, `promise` or `make-promise` to specify the name, or
`remove` to remove this procedure altogether.

* **Options:** eager, promise, make-promise, remove, undecided
* **Default:** eager
* **Preferences:**

### #308 Allow circular lists in LIST-REF for SRFI-1 compatibility

Allow the argument of `list-ref` to be circular.  It is still an error
to use an index >= the length of the list.  None of my test
implementations has a problem with this.

Vote `circular` to explicitly allow circular lists, `error` to add an
"is an error" disclaimer, or `unspecified` to leave as is.

* **Options:** circular, error, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #309 Allow circular lists in MAP and FOR-EACH for SRFI-1 compatibility

Allow circular lists as the list arguments to `map` and `for-each`. If
all arguments are circular, these procedures will not terminate unless
the mapping procedure forces a non-local exit.  The result of `map` is
not circular.  Implementations that stop when the shortest list runs
out and don't make gratuitous tests shouldn't have a problem with
this: R5RS allows, R6RS forbids, and R7RS requires this behavior.

Vote `circular` to explicitly allow circular lists, `error` to add an
"is an error" disclaimer, or `unspecified` to leave as is.
Unspecified leaves open the theoretical extension of returning a new
circular list with the corresponding mapped results.

* **Options:** circular, error, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #310 Rationalize start/end/(fill) arguments in sequence procedures

When we approved [CompleteSequenceCowan](CompleteSequenceCowan.md) in ticket #64, we adopted
[[http://srfi.schemers.org/srfi-43/srfi-43.html#vector-fill-bang|SRFI
43]] syntax and semantics for `vector-copy`, meaning that it takes
optional *start, end, fill* arguments.  This is inconsistent with
various other copier procedures in R7RS as inherited from R5RS, as
well as what is provided in SRFI 43 and its relatives
[SRFI 1](http://srfi.schemers.org/srfi-1/srfi-1.html) (for lists) and
[SRFI 13](http://srfi.schemers.org/srfi-13/srfi-13.html) (for strings).
There are four plausible courses of action:

* **Proposals:**
* *nothing* (default):  The only virtue here is that it requires the least thinking and editing.  Several comments have criticized it.
* *r5rs:*  Claw back ``vector-copy`` to just accept the source vector, all of which is to be copied.  This provides self-consistency, consistency with R5RS, and maximum simplicity.  The SRFIs will be provided as R7RS-large packages which will export the more complex and powerful versions.
* *srfi:*  Enhance `vector-fill!`, `vector->list`, `string->list`, `string-copy`, `string-fill!` to support optional *start* and *end* arguments.  This provides some self-consistency, backward compatibility with R5RS, consistency with the SRFIs, and some loss of simplicity.
* *srfi-plus:*  Same as *SRFIs*, but also add optional *start, end, fill* arguments to `list-copy` and optional *fill* argument to `string-copy`.  This provides maximal function, full self-consistency, backward compatibility with R5RS, and backward compatibility with the SRFIs.
* **Options:** nothing, r5rs, srfi, srfi-plus, undecided
* **Default:** nothing
* **Preferences:**

### #311 Remove tail call guarantee for guard clauses

The current draft guarantees the guard clauses (not the body) of a
guard form to be in tail call position, but the need for this is
unclear (who needs an unbounded number of active exceptions), and
there may be worthwhile guard implementations where this is not the
case.

* **Options:** remove, keep, undecided
* **Default:** keep
* **Preferences:**

### #312 unquoting and identifiers beginning with @

The current draft allows `@` to begin an identifier, which would require
some comment about unquoting, i.e. to distinguish whether `,@foo` is
`(unquote @foo)` or `(unquote-splicing foo)`.

The options are `invalid` (disallow @ at the beginning of an
identifier, as in R5RS), `unquote` to indicate that `,@foo` is `(unquote @foo)`, and
`unquote-splicing` to indicate that `,@foo` is `(unquote-splicing foo)`.

If `unquote-splicing` is chosen, a
note will be added saying that if you want to unquote an identifier beginning with `@` you
need to either insert whitespace or escape the identifier, e.g. either `, @foo`
or `,|@foo|`.

Note that if we don't choose `invalid` then SXML retroactively becomes
valid syntax.

* **Options:** invalid, unquote, unquote-splicing, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #315 null character may not be usable in strings

We should probably make (string-set! str n #\null) unspecified.  Note that R7RS implementations can already restrict the set of characters that are allowed in strings.

Vote `yes` to add a clause to this effect, and `no` to leave it as legal.

* **Options:** yes, no, undecided
* **Default:** yes
* **Preferences:**

### #316 R6RS base compatibility: boolean=?

This is equivalent to `eq?` on booleans, and provides R6RS base
compatibility as well as completing the set of type-specific
comparisons.  See also #285.

Vote `yes` to add these three procedures.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #317 escape from with-input-from-file

The draft states for with-input-from-file and with-output-to-file:

> If an escape procedure is used to escape
> from the continuation of these procedures, their
> behavior is implementation-dependent.

but now that we have dynamic-wind there's no particular reason to keep
this restriction, nor is it difficult to implement.

Vote `parameterize` to specify the current-in/output-port are bound
dynamically as with parameterize in these cases, or `unspecified` to
leave unspecified.

* **Options:** parameterize, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #319 Make special treatment of CAPITAL SIGMA optional

Currently we require that if the characters GREEK LETTER CAPITAL
SIGMA, SMALL SIGMA, and SMALL FINAL SIGMA are supported by an
implementation, that a CAPITAL SIGMA in a string passed to
`string-downcase` be changed to SMALL FINAL SIGMA just before a word
break, and SMALL SIGMA otherwise.  Word breaks are defined by UAX #29,
and are no simple matter.  The proposal is to make this behavior optional,
allowing CAPITAL SIGMA to be downcased to SMALL SIGMA in every case.

Vote `yes` to make optional.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #320 Add new cond-expand feature to Appendix B: exact-complex

(In this ticket, "complex" is used for readability; it is synonymous
with "non-real".)

This feature is true in implementations that support complex numbers
such that both the real and the imaginary parts are exact; that is, if
`(eqv? 3+4i 3.0+4.0i)` evaluates to `#f`.  This feature is false if
complex numbers are not supported or if only inexact complex numbers
are supported.  Most of the applications of complex numbers use
inexact numbers, but some applications may require exactness: this
feature allows those applications to fail fast on implementations that
cannot support them.

Existing implementations:

* Exact complex numbers: Racket, MIT, Gambit, Chicken with the `numbers` egg, Scheme48/scsh, Kawa, Chibi, Chez, Vicare, Ypsilon, Mosh, IronScheme, STklos, Wraith
* No exact complex numbers: Gauche, Guile, SISC, SCM, Scheme 7, KSi, UMB, Stalin
* No complex numbers: Chicken without the `numbers` egg, Bigloo, Ikarus, RScheme, Scheme 9, Oaklisp, Elk, VX, Sixx, Sizzle, Dream, Owl Lisp, Psyche

Vote `yes` to add this feature.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #321 Add get-features from [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) to R7RS-small

This procedure returns a list of symbols corresponding to the feature
identifiers which the implementation treats as true.  More details at
[EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md).

Vote `yes` to add this procedure.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:**

### #322 Add [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) (other than get-features) to R7RS-small

[EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) is a library providing *at run time* what
Common Lisp calls environment enquiries such as the name of the OS.
Implementations can currently expose these as `cond-expand` feature
identifiers, but there is no way to determine things like the name of
the implementation at run time so that it can be written to a log
file, for example.

Vote `yes` to add [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) (other than
`get-features`), and `no` to leave out.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:**

### #323 Eliminate some cond-expand feature identifiers

Reduce the standardized `cond-expand` feature identifiers to `r7rs`,
`exact-closed`, `ratio`s, `ieee-float`, and `full-unicode`, plus the
name and name-plus-version of the implementation.  The others can't
affect the behavior of strictly conforming programs, and it's not
clear if they apply to compile time or run time on implementations
that distinguish the two.  See also ticket #320 for `exact-complex`.

Argument against: Keeping them in the standard encourages all
implementations that use them to spell them the same way: `darwin`,
not `macosx`.

Vote `full` to keep the full list as in draft-6, `implementation` to
keep only the implementation features, or `numerics` to keep the list
described above.

* **Options:** full, implementation, numerics
* **Default:** full
* **Preferences:**

### #259 Remove `(library <name>)` cond-expand features

The `(library <name>)` feature test which is true if the given library
is available (at compile time).  This was used because we voted for
[CondExpandCowan](CondExpandCowan.md), but the original syntax was just `<name>` which is
ambiguous and therefore invalid.  The switch to `(library <name>)` was
added editorially, but not officially voted on.

Vote `keep` to keep and `remove` to remove.

* **Options:** keep, remove, wg2, undecided
* **Default:** keep
* **Preferences:**

### #324 allow |\ as escape for | within a |-escaped identifier

Allow `\|` to represent a vertical bar in an identifier enclosed in
vertical bars (the current BNF disallows | anywhere in the escape).

Note this item is nullified if |...| escapes are removed in item #304.

Vote `pipe` to allow just the vertical bar escaped, `string` to allow
the same set of escapes as in string literals (plus pipe), and `none`
to leave as is.

* **Options:** pipe, string, none, undecided
* **Default:** none
* **Preferences:**

### #325 Eliminate bytevector-copy!

`(bytevector-copy! from to)` is equivalent to
`(bytevector-copy-partial! from 0 (bytevector-length) to 0)`.

The proposal is to remove the existing `bytevector-copy!` from the
small language, and rename `bytevector-copy-partial!` to
`bytevector-copy!`, with the order of arguments `to at from start
end`, the same order used in SRFI 43's `vector-copy!`.  Note that SRFI
43 will be part of the large language.

Vote `yes` to eliminate and rename as proposed, and `no` to leave
as-is.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #326 Add destructive list-copy!, string-copy!, and vector-copy!

From Per Bothner:

Copying a slice from one vector/string into another is such a
fundamental operation that it should be added, IMO, considering that
it's tedious to write if "by hand", and that a standard library
routine is likely to be much more efficient (especially for strings,
since that avoids the need for boxing and unboxing the characters).
[[JC:|Many implementations represent characters as immediates,
however.]]

One could also argue that "character" operations don't really make
semantic sense in a Unicode world, and so `string-set!` has limited
usefulness.  Thus `string-copy` [start/end arguments](with) and
`string-copy!` are the actual useful "primitive" operations.

JC: These would be the five-argument versions based on the current
`bytevector-copy-partial!`, possibly with renumbering of arguments
depending on the outcome of #325.

Vote `yes` to add these destructive operations as proposed, `nolist` to add `string-copy!` and `vector-copy!` only, or `no` for none of them.

* **Options:** yes, nolist, no, undecided
* **Default:** no
* **Preferences:**

### #327 Specify that read, the program reader, and string->number accept the same syntax

Currently there is no guarantee of this.  Obviously the
`string->number` only applies to the case where the radix is 10 or
specified.

Specifying `same` is problematic in the presence of batch compilation
- the compile-time and runtime may not even support the same numeric
tower.

* **Proposals:**
* *same*: The lexical syntax for numbers accepted by `string->number` and `read`, as well as the corresponding syntax of literal numbers in programs, must be the same.
* *run-time*: The lexical syntax for numbers accepted by `string->number` and `read` must be the same, but the relationship with the the corresponding syntax of literal numbers in programs is unspecified.
* *unspecified*: The relationships between lexical syntax for numbers accepted by `string->number` and `read`, as well as the corresponding syntax of literal numbers in programs, is unspecified.
* **Options:** same, run-time, unspecified, undecided
* **Default:** unspecified
* **Preferences:**

### #328 names for inexact->exact and exact->inexact

R6RS changed these names to the more sensible exact and inexact.
We need to decide if we want to follow suit, or provide both names,
or write a disclaimer.

Vote `r6rs` for the short names, `r5rs` for the long names, or `both`
for both.

* **Options:** r5rs, r6rs, both, undecided
* **Default:** r5rs
* **Preferences:**

### #329 Add IEEE compatibility library

The `(scheme ieee)` library exports the standard identifiers of IEEE
1178-1990.  By my current reckoning, those identifiers are as follows:

`- * / + < <= = > >= abs acos and angle append apply asin assoc assq
assv atan begin boolean? call-with-current-continuation car case cdr
ceiling char->integer char-alphabetic? char-ci<? char-ci<=? char-ci=?
char-ci>? char-ci>=? char-downcase char-lower-case? char-numeric?
char-upcase char-upper-case? char-whitespace? char? char<? char<=?
char=? char>? char>=? close-input-port close-output-port complex? cond
cons cos current-input-port current-output-port define denominator
display do eof-object? eq? equal? eqv? even? exact->inexact exact? exp
expt floor for-each gcd if imag-part inexact->exact inexact?
input-port? integer->char integer? lambda lcm length let let* letrec
list list-ref list? log magnitude make-polar make-rectangular
make-string make-vector map max member memq memv min modulo negative?
newline not null? number->string number? numerator odd?
open-input-file open-output-file or output-port? pair? peek-char
positive? procedure? quasiquote quote quotient rational? rationalize
read read-char real-part real? remainder reverse round set-car!
set-cdr! set! sin sqrt string string->number string->symbol
string-append string-ci<? string-ci<=? string-ci=? string-ci>?
string-ci>=? string-length string-ref string-set! string? string<?
string<=? string=? string>? string>=? substring symbol->string symbol?
tan truncate vector vector-length vector-ref vector-set! vector? write
write-char zero?`

As with any library other than `(scheme base)`, implementations SHOULD
(rather than MUST) provide this.

Vote `yes` to add this library.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #330 Add R5RS compatibility library

The `(scheme r5rs)` library exports the standard identifiers of R5RS
Scheme other than `transcript-{on,off}`.  By my current reckoning, those identifiers are as follows:

`- * / + < <= = > >= abs acos and angle append apply asin assoc assq
assv atan begin boolean? call-with-current-continuation
call-with-values car case cdr ceiling char->integer char-alphabetic?
char-ci<? char-ci<=? char-ci=? char-ci>? char-ci>=? char-downcase
char-lower-case? char-numeric? char-ready? char-upcase
char-upper-case? char-whitespace? char? char<? char<=? char=? char>?
char>=? close-input-port close-output-port complex? cond cons cos
current-input-port current-output-port define define-syntax delay
denominator display do dynamic-wind eof-object? eq? equal? eqv? eval
even? exact->inexact exact? exp expt floor for-each force gcd if
imag-part inexact->exact inexact? input-port? integer->char integer?
interaction-environment lambda lcm length let let-syntax let* letrec
letrec-syntax list list->string list->vector list-ref list-tail list?
load log magnitude make-polar make-rectangular make-string make-vector
map max member memq memv min modulo negative? newline not
null-environment null? number->string number? numerator odd?
open-input-file open-output-file or output-port? pair? peek-char
positive? procedure? quasiquote quote quotient rational? rationalize
read read-char real-part real? remainder reverse round
scheme-report-environment set-car! set-cdr! set! sin sqrt string
string->list string->number string->symbol string-append string-ci<?
string-ci<=? string-ci=? string-ci>? string-ci>=? string-copy
string-fill! string-length string-ref string-set! string? string<?
string<=? string=? string>? string>=? substring symbol->string symbol?
tan truncate values vector vector->list vector-fill! vector-length
vector-ref vector-set! vector? with-input-from-file
with-output-to-file write write-char zero?`

As with any library other than `(scheme base)`, implementations SHOULD
(rather than MUST) provide this.  A disclaimer will be added that the
semantics may not be exactly the same.

Vote `yes` to add this library.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #331 Add R6RS base compatibility library

The `(scheme r6rs base)` library exports the standard identifiers of
the base library of R6RS.  By my current reckoning, those identifiers
are as follows:

`- * / + < <= = > >= abs acos and angle append apply asin atan begin
boolean? call/cc call-with-current-continuation call-with-values car
case cdr ceiling char? char<? char<=? char=? char>? char>=?
char->integer complex? cond cons cos define define-syntax denominator
dynamic-wind eq? equal? eqv? even? exact exact? exact-integer-sqrt exp
expt finite? floor for-each gcd guard if imag-part import inexact
inexact? integer? integer->char lambda lcm length let let* let*-values
letrec letrec* letrec-syntax let-syntax let-values list list?
list->string list->vector list-ref list-tail log magnitude make-polar
make-rectangular make-string make-vector map max min nan? negative?
not null? number? number->string numerator odd? or pair? positive?
procedure? quasiquote quote rational? rationalize real? real-part
reverse round set! sin sqrt string string? string<? string<=? string=?
string>? string>=? string->list string->number string->symbol
string-append string-copy string-for-each string-length string-ref
substring symbol? symbol->string tan truncate values vector vector?
vector->list vector-fill! vector-for-each vector-length vector-map
vector-ref vector-set! zero?`

As with any library other than `(scheme base)`, implementations SHOULD
(rather than MUST) provide this.  Full compliance will depend on voting for
the procedures `*-valued`, `assert`, `boolean=?`, `symbol=?`.  A disclaimer
will be added that the semantics will not be exactly the same.

Vote `yes` to add this library.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #332 Allow multiple name pairs in export renaming

Currently, to export `my:foo` and `my:bar` as `foo` and `bar`, one
must write `(export (rename my:foo foo) (rename my:bar bar))`.  This
proposal allows `(export (rename (my:foo foo) (my:bar bar)))`.  This
is incompatible with R6RS, but compatible with the `rename` sub-form
of `import`.

Vote `multiple` to allow multiple renames in one rename clause as with
the import version, `r6rs` to allow the R6RS-compatible syntax in the
current draft, or `both` to allow both forms.

* **Options:** r6rs, multiple, both, undecided
* **Default:** r6rs
* **Preferences:**

### #333 Require eof-objects to be disjoint from basic Scheme types

It's already a requirement that an eof-object cannot have an external
representation, which means it cannot be any of the basic types in
Section 3.2 except procedure or port.  This is very improbable, and in
fact none of my 40 test Schemes returns either a procedure or a port.

Doing this would allow `eof-object?` to be added to the list of
disjoint type predicates in Section 3.2.

Vote `yes` to explicitly list the eof-object as a separate disjoint type.

* **Options:**
* **Default:**
* **Preferences:**

### #334 Use proper case for the feature identifiers in Appendix B

Specifically R7RS, IEEE-float, full-Unicode, Windows, POSIX, Unix,
Darwin, Linux, BSD, FreeBSD, Solaris, PPC, SPARC, JVM, CLR, LLVM,
ILP32, LP64, ILP64.

Note this is incompatible with existing implementations which provide
these features.  The correct case can often be ambiguous, and it's
easiest to keep everything consistently lower case.

Vote `mixed` for mixed case and `lower` for lower case.

* **Options:** lower, mixed, undecided
* **Default:** lower
* **Preferences:**

### #335 Specify behavior of default exception handler

If an exception is caught and leaves the current dynamic extent,
obviously the *after* thunk must be run, but an uncaught exception has
no semantics and is basically reverting to "is an error" semantics,
i.e. nasal demon territory.

Possibly we should tighten this up in the standard, i.e. specify that
there is a default exception handler which enters a continuation
outside the extent of the whole program before exiting.

Vote `unwind` to specify that there is a default exception handler
which leaves the current dynamic extent causing a full unwind (and
thus forbidding a debugger), `exit` to specify that (modulo any
diagnostic information) the program must simply exit without
unwinding, or `unspecified` to leave this as is.

* **Options:** unwind, exit, unspecified
* **Default:** unspecified
* **Preferences:**

### #344 Should dynamic-wind handlers be invoked from EXIT?

Currently the report is silent about whether dynamic-wind handlers are
invoked when `exit` is called.

The options are the same as in #335 above.

* **Options:** unwind, exit, unspecified
* **Default:** unspecified
* **Preferences:**

### #337 Add eof-object procedure

`eof-object` returns an object which answers `#t` to `eof-object?`.
This procedure is present in R6RS, where it must return the *unique*
end-of-file object; that is not required here.

From Vincent Manis:

This isn't just an attempt to create a vain orthogonality; there are
good reasons why arbitrary code might wish to return an eof
object. For example, a DBMS interface might have a routine that
returns one row, as a list or a vector, at a time; after the last, it
is perfectly reasonable to return an eof object.

An argument against providing this is that the constructor may be
trivially written, as shown [below]. A similar argument could be
applied to `zero?`, `newline`, `quotient`, `remainder`, and `modulo`,
among others. R7RS is not afraid to provide easy-to-implement
procedures in the name of simplicity, orthogonality, or historical
compatibility.  The lack of an eof constructor is worth
remedying.

```
(let* ((p (open-input-string ""))
       (x (read p)))
  (close-port p)
  x)
```

Vote `eof-object` for a procedure of that name, or `none` to not add any such procedure.

* **Options:** eof-object, none, undecided
* **Default:** none
* **Preferences:**

### #339 Restrict identifiers in library names for compatibility with file system restrictions

Currently the identifiers in library names can be any identifier.
Under this proposal, the identifiers must not include any of `| \ ?* <
" : > + [[|]] /` or control characters after escapes are expanded.

If this proposal fails, its content will be included non-normatively
as a *should not*.

Vote `yes` to restrict with *must not*.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #340 Include non-normative note about the file-system based implementations of libraries

Libraries do not necessarily have any mapping to files, nor does an
implementation necessarily run on a system with a filesystem, however
for those implementations which do so it may be worth adding such a
note.

A library file contains a single library.  A library named (A1 A2 AN)
is in a file named "A1/A2/AN.sld" ("sld" for "Scheme Library
Definition" or some other standardized file extension), relative to
some "library path".  For portability, library component names should
be integers or lower-case identifiers that avoid certain prohibited
characters.  When a library or top-level imports some other library,
the corresponding file is found in the obvious way.

Alternately, this can be left entirely to WG2 and/or packaging systems
such as Snow.

Vote `yes` to add such a note or `no` to leave it out.

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #341 Permit ambiguous imports of identifiers which are never used

It is currently an error to attempt to import the same identifier from more
than one library into another library or a top-level program, even if the identifier is not
used anywhere in the new library or program.  That requires programmers to make an
arbitrary decision to exclude it from one library or the other.

Vote `yes` to agree with this proposal to require that, within a
single static library (not with the environment procedure where any
identifier may be subsequently used), an implementation must allow
such multiple imports if the identifier is not referenced and does not
occur in a syntax-rules template (which introduces conflicts with
low-level macros introduced by WG2).

* **Options:** yes, no, undecided
* **Default:** no
* **Preferences:**

### #342 Have READ-BYTEVECTOR(!) return 0 at EOF

Currently, `read-bytevector` and `read-bytevector!` return an EOF
object at EOF; otherwise, `read-bytevector` returns a non-empty
bytevector and `read-bytevector!` returns the number of bytes read.
Returning #u8() and 0, respectively, at EOF instead would make the
results always the same type.  This change would introduce the
ambiguity that one would not be able to detect EOF when reading a
bytevector of length 0 (which is to say, not reading any bytes at
all).

Vote `zero` to return #u8() and 0 as in the proposal, and `eof-object`
to return the eof-object as in the current draft.  Vote `zero!` to
make the change only for `read-bytevector!`.

* **Options:** zero, eof-object, undecided
* **Default:** eof-object
* **Preferences:**

### #343 Editorial: divide domain explanations to be split before and after descriptions

All Scheme standards up to and including R6RS and R7RS draft-6 have
consistently placed the full domain at the beginning of each entry.
In most cases the domain consists only of the implicit type
restrictions from the prototype, but in some cases there are
additional domain restrictions that cannot be conveniently included in
the prototype such as the following `map` restrictions:

> It is an error if *proc* does not accept as many arguments as
> there are *lists* and return a single value.

It has been suggested to move this to an appropriate later point in the entry,
to put more emphasis on the initial entry description.  This has the
disadvantage of splitting the domain into two places, which can more
easily cause oversights and make quick domain confirmations difficult.

An alternative is to separate the additional domain restrictions from
the initial description, as a separate short paragraph immediately
following the prototype and possibly de-emphasized by making it smaller.
his would keep the domain in one place and still allow
let the first line of the description stand out prominently in the
initial paragraph.

Vote `start` for the status quo, `start-split` for the separate
de-emphasized option, or `later` to move additional restrictions to a
later point.

* **Options:** start, start-split, later, undecided
* **Default:** start
* **Preferences:**

### #345 Should 0.0 and -0.0 be distinct in the sense of EQV?

Currently, the draft report implies that 0.0 and -0.0 must be the same
in the sense of `eqv?`, because `eqv?` defers to `=` for numbers
(with the possible exception of [NaNs](NaNs.md)).

Vote `same` for the status quo, `different` to change to "must be
different", or `unspecified` to change to "may be different".

* **Options:** same, different, unspecified, undecided
* **Default:** same
* **Preferences:**

### #349 Define exact integers to be at least 24 bits

Currently, R7RS (tracking R5RS) does not constrain the sizes of exact
integers beyond being required to represent the indices of strings,
vectors and bytevectors.

R6RS requires systems to support "practically unlimited" size exact
integers.  It also requires that a subset of these exist, called
*fixnums*, which must support at least the range -2^23^ to 2^23^-1.
(All practical Schemes have larger ranges for their fixnums).
This proposal suggests that we adopt this range as
the minimum range of R7RS exact integers.

The immediate issue here is that a library name may contain
(non-negative) exact integers as well as identifiers in R7RS.  For
such names to be portable, there must be a portable range of exact
integers.

See [FixnumInfo](FixnumInfo.md) to see what 39 existing Schemes do.

Vote `24` to require 24 bits of precision, `16` to require 16 bits of precision,
or `none` to leave this entirely unspecified.

* **Options:** 24, 16, none, undecided
* **Default:** none
* **Preferences:**

### #354 mutating exports

We define mutating imports to be an error, however
the standard currently says nothing about what
happens when an exported binding is mutated from
within the library where it's defined.
In many common library implementations there
will be no effect (i.e. the import effectively gets
a copy of the original), whereas in a namespace
based implementation the change will be reflected,
so a conservative approach is to add a note saying
the result is unspecified.

Vote `shared` to force the binding to be shared
and the change reflected everywhere it's imported,
`separate` to force the binding to be separate,
`none` to make no comment, and `unspecified`
or `error` to add a clarification to the standard
to that effect.

* **Options:** shared, separate, none, unspecified, error, undecided
* **Default:** none
* **Preferences:**

### #358 change epoch of current-second

A formal comment has proposed changing the epoch of current-second to
1970-01-01 00:00:00 TAI rather than 1970-01-01 00:00:10 TAI (00:00:00
UTC).

The actual time systems are independent of an epoch - the epoch is
just convenient for computer systems.

The UTC-centric epoch was chosen (despite the use of TAI time) mostly
because it is used in popular TAI times such as libtai and Olson's
time library.

See http://lists.scheme-reports.org/pipermail/scheme-reports/2012-March/001943.html for more details.

Vote `utc` for the current draft's start-of-1970-in-utc epoch, or
`tai` for the proposed start-of-1970-in-tai epoch.

* **Options:** utc, tai, undecided
* **Default:** utc
* **Preferences:**
