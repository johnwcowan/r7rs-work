
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

= WG1 Ballot Items To Finalize By July 31 =

== WG1 - Core ==

=== #460 Semantics of `eqv?` ===

Earlier we voted on #125, #229 and #345 separately without regard to
the formal semantics of `eqv?` from a top level.  We need to first
decide what the definition of `eqv?` is, and consider if there should
be any exception cases as a secondary effect.

The debate is fundamentally one of whether we define `eqv?` in terms
of "operational equivalence" as in R6RS or a simpler rule
(e.g. resolve by `=`) as in earlier standards.

R2RS had the simplest historical rule which was simply to use `=`.

The term "operational equivalence" appears in R3RS but for numbers the
definition is the same as in R4RS and R5RS, which is `=` plus the same
exactness.  This is the `r5rs` option, with the "true" cases written formally as:

{{{
  The `eqv?` procedure returns #t if:

  (1) obj1 and obj2 are both booleans and are the same according
  to the `boolean=?` procedure.

  (2) obj1 and obj2 are both symbols and are the same symbol
  according to the `symbol=?` procedure.

  (3) obj1 and obj2 are both numbers, have the same exactness, are
  numerically equal (see `=`).

  (4) obj1 and obj2 are both characters and are the same
  character according to the `char=?` procedure.

  (5) obj1 and obj2 are both the empty list.

  (6) obj1 and obj2 are pairs, vectors, bytevectors, records,
  or strings that denote the same location in the store.

  (7) obj1 and obj2 are procedures whose location tags are equal.
}}}

Note that (7) is an exception case which will be decided
separately in #125.  Furthermore, an exception to make NaNs
unspecified regardless of the semantics here will be decided in
#229.

The `r6rs` vote replaces (3) with the following operational
equivalence semantics:

{{{
  (3.1) obj1 and obj2 are both exact numbers and are numerically
  equal (see `=`)

  (3.2) obj1 and obj2 are both inexact numbers, are numerically
  equal (see `=`), and yield the same results (in the sense of
  `eqv?`) when passed as arguments to any other procedure that
  can be defined as a finite composition of Scheme’s standard
  arithmetic procedures.
}}}

where "standard arithmetic procedures" refers arguably to either
11.7 or 11.7.4.3.  For R7RS it would apply to the "Numbers"
section 6.2.6.  R6RS further adds an extra case which is not
applicable because we don't guarantee record-types are first-class
objects:

{{{
  (8) obj1 and obj2 are record-type descriptors that are
  specified to be `eqv?` in library section on “Procedural
  layer”.
}}}

The `r6rs/all` option changes (3.2) to a finite composition of
any of the implementations arithmetic procedures.  The intention
is that `decode-float` if provided could distinguish NaNs, but
something like `eq?` (which could distinguish the same bit
pattern in different locations) would not be considered
arithmetic and not apply.  This does leave the
definition "arithmetic" open to some interpretation.

In contrast to R6RS, R7RS does not require the full numeric tower.
This means that any definition of operational equivalence would render
many numbers unspecified from the perspective of the standard, yet
users could rely on consistency within their own implementation, and
broad agreement amongst most implementations which provide the full
tower.

Finally, the `same-bits` option replaces (3) with:

{{{
  (3.1) obj1 and obj2 are both exact numbers and are numerically
  equal (see `=`)

  (3.2) obj1 and obj2 are both inexact real numbers conforming to the
  IEEE 754-2008 standard, and they have the same radix,
  precision, maximum exponent, sign, exponent, and significand as
  described in IEEE 754-2008

  (3.3) obj1 and obj2 are both inexact real numbers, are not implemented using
  IEEE 754-2008, and are numerically equal (see `=`)

  (3.4) obj1 and obj2 are both complex numbers whose real and imaginary
  parts are `eqv?`
}}}

Keep in mind the semantics of `eqv?` also affect `memv`, `assv` and
`case`.

  * '''References:'''
    * [[https://groups.google.com/d/msg/scheme-reports-wg1/BGvDFtD6A1M/5pHmfXHtvEIJ|eqv? issues summarized]]
    * [[https://groups.google.com/d/msg/scheme-reports-wg1/2Nv6oIND8HI/Z2HXPQMNFooJ|the history of eqv? on numbers]]
  * '''Options:''' r5rs, r6rs, r6rs/all, same-bits
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #229 eqv? and NaN ===

As announced previously this is being re-opened due to incorrect
formulation in the previous ballot, and in response to formal comment
#423.

Regardless of the result of #460, the semantics implies that `eqv?`
return `#f` on comparing any two NaN objects.  It is reasonable to
want to consider any two NaNs as the "same" since they behave the same
under any operation, even though none of the results are `=`.  Moreover,
it is very common to use a shortcut `eq?` pointer comparison before
falling back on general `eqv?` logic.  In deference to this R6RS makes
an exception and allows the result to be unspecified, and we should
consider allowing this exception.

This proposal is only to allow an explicit exception to make
NaN comparisons unspecified, regardless of the semantics.
Vote `no-exception` (or anything other than `unspecified`)
to require NaN comparisons to follow directly from #460.

The default of `unspecified` still holds from the previous invalidated
ballot.

  * '''Options:''' no-exception, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #125 Allow procedures not to be locations (making EQV? unspecified in some additional cases) ===

Procedures are another case for contention with `eqv?`.  In R3RS, an
operational equivalence was defined for procedures, and this was
subsequently removed.

R6RS went the other direction and allowed the exact same procedure `x`
to return `#f` for `(eqv? x x)`, and R7RS currently reaffirms this.
The rationale behind this is for compiler optimizations such as
inlining local procedures, that is given:

{{{
(let ((square (lambda (x) (* x x))))
  (eqv? square square))
}}}

it is reasonable for a compiler to perform the optimization:

{{{
(eqv? (lambda (x) (* x x)) (lambda (x) (* x x)))
}}}

in which case the result would naturally return `#f`.

Vote `yes` to allow the result to be `#f`.

The default of `yes` still holds from the previous invalidated ballot.

  * '''Options:''' yes, no, undecided
  * '''Default:''' yes
  * '''Preferences:''' 

=== #393 Defining record equality ===

Currently, R7RS-small says that when `equal?` is applied to records
that are not `eqv?` (that were constructed by different calls to the
record constructor), the result may be `#t` or `#f` at the
implementation's discretion.  The proposal is to treat records of the same
type like pairs, strings, vectors, and bytevectors: that is, their
contents are recursively compared.

Vote `recursive` to require recursive comparison of the record's
fields, `identity` to return `#t` iff `eqv?` does, and `unspecified`
to leave this open.

Note `equal?` is already required to handle cycles regardless.

  * '''Options:''' recursive, identity, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #306 What does "full Unicode" support mean in Appendix B? ===

Specifically, does it indicate case-folding and normalization support
for the repertoire of any particular version of Unicode, or any
version greater than 5 or 6 or 6.1, or no particular version?

Full unicode refers to the set of characters available.  Case-folding
and character predicates are required to work according to the Unicode
standard for all supported characters.  The question of which version
of Unicode the property refers to is important.  We could require a
specific version (and all further revisions), or always require the
latest official Unicode standard, in which case an implementation
would not be compliant until it was updated with each new standard.  Alternatively, we could parameterize the feature identifier, so that implementations might claim to support (full-unicode 6), (full-unicode 6.1), etc.

  * '''Options:''' at-least-6, at-least-6.1, latest, parameterize, undecided, unspecified
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #458 Remove the formal semantics from the report ===

There have been a bunch of complaints about the formal semantics: it's
incomplete, it cannot be mechanized with a proof assistant, it doesn't
help either users or implementers very much, and so on.  See in
particular #453.

The semantics have been updated to include `dynamic-wind`, however the
other arguments still hold.

This proposal is to remove it from the report altogether, and to urge
the Steering Committee to create a new WG to produce one, likely in a
"rolling" style with increasingly comprehensive releases, on its own
schedule.  Some members of the current WG have expressed interest in
serving on such a group, and others have expressed their complete lack
of interest, so a new WG seems the best choice if this is done.

Alternately, we can adapt the operational semantics from R6RS.

  * '''Options:''' remove, keep, operational, undecided
  * '''Default:''' keep
  * '''Preferences:''' 

=== #398 Allow repeated keys in `case` ===

R5RS says it's an error for a key to appear in more than one clause of
`case` (or twice in the same clause, but that's trivial).  R6RS allows
the same key to appear more than one clause, but insists on
left-to-right testing throughout, like `cond`.  The R6RS editors
thought this was better for machine-generated code, though worse for
hand-written code.

The proposal is a compromise: allow keys to appear in more than one clause,
but behave as if the key appeared only in the first (leftmost) clause.
This allows hash-table or other non-left-to-right implementations.

  * '''Options:''' r5rs, r6rs, leftmost, unspecified, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #85 Blobs, bytevectors, byte-vectors, octet-vectors, or something else?

Following exactly in the footsteps of R6RS we voted for a `blob` API
and then changed the name to `bytevector`.

Formal comment #435 argues that `u8vector` is in more common use, so
this item is being re-opened.  The default is the current draft
`bytevector`, and for any member leaving the preferences are left
blank their votes from ballot 3 will be used.

  * '''Options:''' blob, bytevector, byte-vector, u8vector, octet-vector, undecided
  * '''Default:''' bytevector
  * '''Preferences:''' 

== WG1 - Library System ==

=== #353 No use before import in libraries ===

For ease of implementation, the proposal is to make it an error for an
imported identifier to be referenced or defined in a library before
the library declaration that imports it.  This allows strict
left-to-right processing of library declarations, with no need to
delay processing till the end of the library.

Therefore, this would be an error (but still permitted as an extension
in Schemes that can easily provide it):

{{{
(module
  (begin (define x y))
  (import (library defining y))
}}}

This would necessitate replacing the penultimate paragraph of section
5.5.1 with:

One possible implementation of libraries is as follows: After all
`cond-expand` library declarations are expanded, a new environment is
constructed for the library consisting of all imported bindings. The
expressions and declarations from all `begin`, `include`, and
`include-ci` declarations are expanded in that environment in the
order in which they occur in the library declaration.  Alternatively,
`cond-expand` and `import` declarations may be processed in left to
right order interspersed with the processing of expressions and
declarations, with the environment growing as imported bindings are
added to it by each `import` declaration.

Vote `yes` to add the restriction, or `no` to leave it out.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #359 Limit numbers used in library names ===

This is a proposal to limit numbers in library names to the range 0 to
32767.  Currently, there is no portable lower bound which all Schemes
can assume as the maximum size of an integer.

Numbers are mostly used for SRFI-based libraries anyway, which are not
likely to reach either limit.

The option `uint15` for the proposal as stated (0 to 32767), `int16`
for -32768 to 32767, int24 for -2^23^ to 2^23^-1, etc.

Vote `unspecified` to make no explicit requirement on the integers
allowed in library names.

  * '''Options:''' uint15, int16, uint16, int24, uint24, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #441 Make sure a program/library loads any imported libraries at most once ===

Add the following text to the discussion of library loading:

 Regardless of the number of times that a library is loaded, each
 program or library that imports bindings from a library will receive
 bindings from a single loading of that library, regardless of the
 number of `import` or `cond-expand` declarations in which it appears.

to make it clear that, for example,

{{{
(import (prefix (foo) 'foo:))
(import (only (foo) bar))
}}}

will cause `bar` and `foo:bar` to come from the same instantiation of
the library '(foo)'

Vote `yes` to add this requirement.

  * '''Options:''' yes, no, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #402 Add an export-all form. ===

Add an export-all form to the library declaration that means "export
all identifiers that are defined in the library with begin, include,
and include-ci but none that are imported with import."

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #448 Add library declaration include-library-declarations ===

The proposed `include-library-declarations` allows a library to
incorporate a file containing arbitrary library declarations, not just
Scheme code (definitions and expressions).  This allows, for example,
the exports of a module to be written directly in the library file,
and its imports in a separate file.

An alternative would be something like `(export-from <library>)` to
export the same bindings as another library.  This does require the
clumsiness of actually defining the identifiers in the other library
if it is abstract.

  * '''Options:''' include-library-declarations, export-from, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #449 Clarify library loading rules ===

R7RS currently says:

 Within a program, each imported library is loaded at least once, and,
 if imported by more than one program or library, may possibly be
 loaded additional times.

Richard Kelsey thinks this is too liberal, and proposes:

 Regardless of the number of times that a library is loaded, each
 program or library that imports bindings from a library will receive
 bindings from a single loading of that library, regardless of the
 number of `import` or `cond-expand` forms in which it appears.

Aaron Hsu, however, thinks this is too restrictive, and proposes
(backed up by actual R6RS implementations):

 If a library's definitions are referenced in the expanded form of a
 program or library body, then that library must be loaded before the
 expanded program or library body is evaluated. This rule applies
 transitively.

 Similarly, during the expansion of a library, if a syntax keyword
 imported from a library is needed to expand the library, then the
 imported library must be visited before the expansion of the
 importing library.

  * '''Proposals:'''
    * '''one:''' Kelsey's proposal
    * '''one-or-more:''' current draft
    * '''zero-or-more:''' Hsu's proposal, R6RS status-quo
    * '''zero-or-one:''' Kelsey's proposal with Hsu's relaxation
  * '''Options:''' one, one-or-more, zero-or-one, zero-or-more
  * '''Default:''' one-or-more
  * '''Preferences:''' 

== WG1 - Numerics ==

=== #366 Add (log z b) for logarithm of z to the base b ===

Coverage for this R6RS feature is currently sparse: only Gauche, Chez,
Vicare, Larceny, Ypsilon, Mosh, !IronScheme, KSi, RScheme, Rep support
it.  But it is convenient when working in bases other than ''e'' such
as 10, 2, or 16, and it is just a few extra lines of code, since `(log
z b)` => `(/ (log z) (log b))` for arbitrary complex numbers ''z, b''.

Vote `yes` to add the optional second argument from R6RS.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #367 Inexact division by exact zero ===

Draft 6 says that it's an error for an argument of `/` (other than the
first) to be an exact zero.  R6RS, however, says that it's an error
only if ''all'' the arguments are exact.  In other words, `(/ 2.0 0)`
is an error according to the draft, but in R6RS it returns `+inf.0`
(assuming the implementation supports it).  The proposal is to adopt
the R6RS wording.

Cowan tested `(/ 2.0 0)` in the usual set of Schemes:

 * Racket, Gambit, Chicken (with the numbers egg), Guile, Chibi, Elk, Spark report an error.
 * Gauche, Bigloo, Scheme48/scsh, Kawa, SISC, Chez, SCM, !Ikarus/Vicare, Larceny, Ypsilon, Mosh, !IronScheme, NexJ, STklos, RScheme, BDC, UMB, VX return `+inf.0`.
 * MIT, scsh, Shoe, !TinyScheme, Scheme 7, XLisp, Rep, Schemik, Inlab always report an error when dividing by zero, exact or inexact.
 * KSi, Scheme 9 produce incorrect results.
 * !SigScheme, Dream, Oaklisp, Owl Lisp don't support inexact numbers.

Vote `error` for the current draft semantics that it is an error,
`all-error` for the R6RS semantics that it is only an error if all
arguments are exact, or `unspecified` to make this case unspecified.

  * '''Options:''' error, all-error, unspecified, undecided
  * '''Default:''' error
  * '''Preferences:''' 

=== #369 Require that - and / allow an arbitrary number of arguments ===

R5RS requires that `-` and `/` accept one or two arguments, and labels
support for more than two as "optional".  R6RS requires such support.
The proposal is to require it.

All Schemes in the test suite support more than two arguments except
Scheme48/scsh.  (Owl Lisp does not support variadic procedures of any
kind.)

Vote `require` for required n-ary behavior and `optional` to leave it
optional as in R5RS.  Alternately, vote `forbidden` to make this
always an error in all implementations.

  * '''Options:''' required, optional, forbidden, undecided
  * '''Default:''' optional
  * '''Preferences:''' 

=== #370 Log of exact and inexact zero ===

R5RS and draft 6 of R7RS don't say what `(log 0.0)` and `(log 0)`
return.  R6RS requires `-inf.0` and an exception respectively.  The
proposal is to say that `(log 0.0)` returns `-inf.0` on systems that
have `-inf.0`, and that `(log 0)` is an error.

In Racket, Gambit, Chicken (with the numbers egg), Guile, Chibi, Chez,
!Ikarus/Vicare, Larceny, Ypsilon, Mosh, !IronScheme, STklos, Spark,
`(log 0.0)` returns `-inf.0` and `(log 0)` raises an exception.

Gauche, MIT, Chicken (without the numbers egg), Bigloo, Scheme48/scsh,
Kawa, SISC, SCM, NexJ, KSi, RScheme, XLisp, Rep, VX, SXM, Inlab return
`-inf.0` in both cases.

Elk, UMB, Oaklisp raise an exception in both cases.

Scheme 7 returns the wrong answer in both cases.

!SigScheme, Shoe, !TinyScheme, Dream, BDC, Owl Lisp don't support `log`.

Scheme 9 apparently goes into an infinite loop in both cases.

Vote `r6rs` for the R6RS behavior of returning `-inf.0` and raising an
error, respectively.  Vote `infinity` to always return `-inf.0`.

  * '''Options:''' r6rs, infinity, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #407 Dividing exact 0 by an inexact number ===

This proposal allows `(/ 0 `''x''`)`, where ''x'' is an inexact
number, to return an exact value.  Currently only Racket, Gambit,
!TinyScheme, Sizzle, Spark do this; see [[:Zero|Zero]] for details.

Vote `zero` to allow (but not require) this to return exact 0.  Vote
`no-nan` to allow it to return 0 except when `x` is `+nan.0`, where it
would return `+nan.0`.

  * '''Options:''' zero, no-nan, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #410 Infinity vs. NaN in max and min ===

Currently R7RS says nothing about the value of `(max +inf.0 +nan.0)`
or `(min -inf.0 +nan.0)`.  R6RS required these functions to return the
infinite value, but this was adopted by some but not all R6RS
implementations (see MaxInfNan for details).  R5RS implementations are
also divided.

The proposal is to allow R7RS implementations to provide either value.

Vote `both` to explicitly add a note that either are allowed,
`infinity` to require the infinite value as in R6RS, `nan` to require
returning `+nan.0`, and `unspecified` leave unspecified (i.e. the same
as `both` but without the note).

  * '''Options:''' both, infinity, nan, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #395 Infinite and NaN complex numbers ===

Currently both `infinite?` and `nan?` return `#t` to a complex number
like `+inf.0+nan.0i`.  Is this the Right Thing, or should `infinite?`
only return `#t` if neither part is a NaN?

Note it is reasonable for an implementation to not support partial nan
complex numbers.

Vote `disjoint` to ensure that `infinite?` and `nan?` are disjoint
predicates as in the proposal, or `overlap` to allow the current
behavior.

  * '''Options:''' overlap, disjoint, unspecified, undecided
  * '''Default:''' overlap
  * '''Preferences:''' 

=== #364 truncate, floor, ceiling round should return a non-finite argument ===

Currently R7RS is silent on what truncate, floor, ceiling, and round
do when the argument is `+inf.0`, `-inf.0`, or `+nan.0`. R6RS has them
return the argument, which seems reasonable.

Tests were made for `(round (* 1.0e200 1.0e200))` on a variety of
implementations.

Racket, Gauche, Chicken (with and without the numbers egg), Bigloo,
Guile, Kawa, Chibi, Chez, SCM, Ikarus/Vicare?, Larceny, Ypsilon, Mosh,
IronScheme, !NexJ, STklos, KSi, Shoe, BDC, Rep, Schemik, Elk, Spark
all return the argument.

MIT, Gambit, Scheme48/scsh, SISC, Scheme 9, Scheme 7, signal errors.

SigScheme, TinyScheme, Dream, UMB don't work for one or another
reason.

Oaklisp and Owl Lisp don't do flonums.

XLisp only has fixnums and flonums, and returns the largest or
smallest fixnum as the case may be.

RScheme returns a variety of slightly strange values: (round +inf.0),
for example, is 0, but (round -inf.0) is -inf.0.

Vote `input` to return the input, `error` to specify "it is an error",
and `unspecified` to leave unspecified as in the current draft.

  * '''Options:''' input, error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #392 Exact positive and non-negative integer predicates ===

There are two useful subsets of the exact numbers, both of which are
commonly called natural numbers, depending on who's talking.
Logicians, set theorists, and computer scientists include 0, other
mathematicians mostly don't.  This proposal adds the predicates
`exact-positive-integer?` and `exact-non-negative-integer?`, analogous
to `exact-integer?`.  Because of the ambiguity, the name
`natural-number?` is not proposed.

Vote `yes` to add these two procedures.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' 

== WG1 - Read/Write ==

=== #380 Is support of TAB as a whitespace character required or not? ===

2.2 says:

Whitespace characters include the space and newline characters.
(Implementations may provide additional whitespace characters such as
tab or page break.)

However, 7.1.1 has:

<intraline whitespace> -> <space or tab>
<whitespace> -> <intraline whitespace> | <newline> | <return>

So 2.2 implies that supporting tabs is allowed but not required, yet
7.1.1 implies supporting tabs is required.

Vote `required` to require support for tab as a whitespace character
by `read`.  `char-whitespace?` is required to return `#t` for it
regardless.

  * '''Options:''' required, optional, undecided
  * '''Default:''' optional
  * '''Preferences:''' 

=== #388 Specify what `display` does with circular lists ===

Currently we don't specify what `display` does with circular lists.
Should it generate labels like `write`, or loop like `write-simple`,
or leave it unspecified?

  * '''Options:''' labels, loop, unspecified
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #447 #!fold-case and #!no-fold-case have no final delimiter ===

The `#!fold-case` and `#!no-fold-case` directives are read as
comments, which means that they are treated as whitespace (section
2.2).  Unlike the other kinds of comments, their final delimiter is
implicit.  This means that `(1#!no-fold-cases)` reads as `(1 s)`.
This seems unfortunate.

  * '''Proposals:''' 
    * '''identifier:''' add the formal syntax `<lexical-directive> --> #! <identifier>` and then make the interpretation of `<identifier>` implementation-dependent, except for the standard cases `#!fold-case` and `#!no-fold-case`. (Per Bothner, Richard Kelsey)
    * '''delimiter:''' the directives must be followed by delimiter (John Cowan)
    * '''comment:''' the draft status-quo
  * '''Options:''' identifier, delimiter, comment, undecided
  * '''Default:''' comment
  * '''Preferences:''' 

=== #442 write procedure is not backwards compatible ===

There is concern that the output of `write` cannot be read by non-R7RS
implementations.  This is not a strict requirement, but is reasonable
if using simple sexp-based file/interchange formats.

Specifically, even though there are no cycles in

  `(let ((x (list 2))) (write (list x x)))`

it previously output "((2) (2))" but now outputs "(#0=(2) #0#)".

The WG concern is that R5RS write is unsafe, easily causing infinite
loops, and should therefore not be the default.  Thus we renamed this
"write-simple", requiring programmers to know they are writing a
"simple" data structure up front.

Arguably, there are three procedures desired:

  * write-cyclic: uses labels only to avoid cycles
  * write-shared: uses labels for all shared structure
  * write-simple: won't use labels - it is an error to pass a cyclic structure

although even for `write-shared` people sometimes want to treat
containers such as strings separately.

Note the algorithms for detecting shared structure differ from those
for detecting cycles, so providing both -shared and -cyclic imposes an
additional implementation burden.

  * '''Proposals:'''
    * '''write+simple:''' the current draft status quo
    * '''write+shared:''' change `write` back and add `write-shared` to explicitly handle sharing
    * '''write+cyclic:''' change `write` back and add `write-cyclic` to handle only cycles
    * '''write+shared+cyclic:''' change `write` back and add both `write-shared` and `write-cyclic`
    * '''write+simple+shared:''' `write` handles cycles only, provide `write-simple` and `write-shared` separately
  * '''Options:''' write+simple, write+shared, write+cyclic, write+shared+cyclic, write+simple+shared, unspecified, undecided
  * '''Default:''' write+simple
  * '''Preferences:''' 

=== #219 Bring back readable boolean literals ===

Scheme used to use `#!true` and `#!false` before abbreviating to
the `#t` and `#f` syntax.

In draft 4 we added these back in as aliases, without the "!" now
that tokens are required to be delimited so there would be no ambiguity.

Some objections were made to the new syntax which generated
a lot of discussion, so we are re-opening this ticket.  The default
is the previous decision to add `#true` and `#false` as aliases.

The primary objection is that boolean literals are very common,
and this introduces needless incompatibilities with non-R7RS
systems, and potential confusion in documentation.

The counter-argument is that these are more readable and
friendly to beginners, and allow easy visual distinction in long lists
of booleans.  We retain full backwards compatibility and are
under no obligation for non-R7RS systems to be able to run R7RS code.

Note that Racket and Chibi independently adopted this same
syntax unaware of each other.  Chicken also supports this via
its SRFI-38 implementation.

  * '''References:'''
  * '''Proposals:'''
     * '''long:''' #true and #false
     * '''bang-long:''' #!true and #!false
  * '''Options:''' long, bang-long, none, undecided
  * '''Default:''' long
  * '''Preferences:''' 

=== #443 Recommend sources of character names ===

Currently, we allow implementations to provide their own names for
characters, but provide no guidance for them.  There are two plausible
sources: the [[http://unicode.org/Public/UNIDATA/NamesList.txt|names in
the Unicode Standard]], and the [[http://www.w3.org/TR/xml-entity-names/
entity|names specified by W3C]] for use in HTML, MathML, and other
markup standards (ultimately derived from ISO SGML character entity
sets).

The Unicode names are in all upper case and contain significant spaces
and apostrophes as name characters, which would require some mapping
to make valid Scheme identifiers.  The W3C name list is incomplete
though fairly large (currently 2237 names), covering mainly the Greek
and Cyrillic scripts and non-ASCII punctuation and symbols.  It
distinguishes between iota (small) and Iota (capital).

Vote `w3c` for the W3C list, `unicode` to use the Unicode list with
names mapped by converting to lowercase and replacing any
non-identifier character (space and apostrophe) with hyphens.  Vote
`unspecified` to leave the character name extensions entirely up to
the implementation.

  * '''Options:''' w3c, unicode, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

== WG1 - Base Library ==

=== #140 Removing `quotient`, `remainder`, `modulo` ===

With the acceptance of #278, we reduced the set of division operators
to `truncate-*` and `floor-*` and move these into the base library.
Three of these procedures are simply aliases for `quotient`,
`remainder` and `modulo`, so it is worth considering removing the old
names.

Since the old names are in IEEE Scheme we need strong justification
for removing them from (scheme base), and even if we do so they will
remain available in (scheme r5rs).

We have precedence for changing names, but only in the case when the
existing names were both actively misleading and had already been
changed in R6RS.  Specifically, in ticket #328 we replaced the names
`inexact->exact` and `exact->inexact` with the more accurate `exact`
and `inexact`.

Arguably the new division operator names are clearer, but the old
names are not actually misleading.

Vote `yes` to remove the old names from (scheme base), or `no` to
leave them in as aliases.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #378 Rename GET-FEATURES to just FEATURES ===

This is compatible with Chicken, and "more Scheme-like, less
Java-like".  Okay, it's bikeshedding.

  * '''Options:''' features, get-features, undecided
  * '''Default:''' get-features
  * '''Preferences:''' 

=== #384 Merge `bytevector-copy` and `bytevector-copy-partial` ===

Under this proposal, the name would be `bytevector-copy` and the
signature would be

  `(bytevector-copy `''bytevector'' [[''start''|[''end'']]]`)`

Vote `yes` for this simplification.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #385 Merge `write-bytevector` and `write-bytevector-partial` ===

One proposal is `port-last` with a signature of:

  `(write-bytevector ''bytevector'' [[''start''|[''end'' [''port'']]]])`

This has the disadvantage of being required to call
`bytevector-length` when writing to a specific port.

Alternately we could do `offsets-last`:

  `(write-bytevector ''bytevector'' [[''port''|[''start'' [''end'']]]])`

which has the disadvantage of separating the bytevector from its
offsets.

Alternately, vote `separate` to keep these as two separate procedures.

  * '''Options:''' port-last, offsets-last, separate, undecided
  * '''Default:''' separate
  * '''Preferences:''' 

=== #387 Add start/end arguments to string->vector and vector->string ===

This is a proposal to add optional start (inclusive) and end
(exclusive) arguments to `string->vector` and `vector->string`.  We
now have start (inclusive) and end (exclusive) arguments for
`string->list` and `vector->list`, but our non-R5RS and non-SRFI
procedures to convert directly between strings and vectors don't
provide these.

Vote `yes` to add these optional arguments.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #391 Add predicates for R7RS signalled conditions ===

R7RS requires an error to be signalled (which means an exception is
raised as if by `raise`) in the following circumstances:

 1. Trying to open for input or delete a file that does not exist or is otherwise inaccessible.
 1. Specifying an argument to `scheme-report-environment` that the implementation doesn't support.  (It must support 7 and may support other values.)
 1. An EOF is encountered while `read` is in the middle of a datum.
 1. Using `expt` to raise zero to the power of a non-real number (alternatively an arbitrary number may be returned).

This proposal is to provide four standard predicates that identify
these specific conditions, to be used in `guard` clauses or in
`with-exception` handlers as a portable means of detecting these
errors.  Although these predicates may return `#t` on other objects,
if one reports `#t` on an object, the others must report `#f`.
Proposed names are `file-error?`, `scheme-report-error?`,
`read-error?`, and `expt-error?` respectively.

Vote `yes` to add these procedures, or `file-only` to only add the
`file-error?` predicate, and file+read to add the `file-error?` and
`read-error?` predicates.

  * '''Options:''' yes, file-only, file+read, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #400 Define record? . ===

We should define the predicate record? so that it's possible to
distinguish instances of record types from all other types.  It should
not be necessary to enumerate all record type predicates in order to
determine whether an object is an instance of a record.

This is Alexey Radul's suggestion.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #425 Add read-string, read-string!, write-string procedures to (scheme base) ===

This was requested by Formal Comment #424.

These procedures would be provided for parallelism with the
byte-vector I/O operations:

||Byte||Character||Bytevector||String||
||read-u8||read-char||read-bytevector(!)||read-string(!)||
||write-u8||write-char||write-bytevector||write-string||

If #385 passes, optional ''start'' (inclusive) and ''end'' (exclusive)
index arguments would be added to `write-string`.  Otherwise
`write-partial-string` would be provided.

Vote `yes` to add all three, `immutable` to add only `read-string` and
`write-string`, or `no` to leave them out.

  * '''Options:''' yes, immutable, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #433 full conversion cycle for containers ===

Marc Feeley proposes it should be possible to convert from any
container type to another, possibly via an intermediary such as

  `(list->B (A->list a))`

proposing specifically "list" be the universally available
intermediary, although "vector" would also be worth considering.

The container types are list, vector, string and bytevector.  String
and bytevector are second-class in that they are not general-purpose
container types, and may raise errors converting from lists or
vectors.

Vote `list` for the proposal to add the following procedures to
complete the cycle:

  * list->bytevector
  * bytevector->list

Vote `vector` to add the equivalent procedures to allow converting
between any of the types and vectors, specifically the following two
new procedures:

  * vector->bytevector
  * bytevector->vector

Vote `list+vector` to add both list and vector conversions.

The `latin-1` proposal also adds the Latin-1-centric ideas of string to
bytevector conversion, where each element of the bytevector is
converted to/from a character with char->integer/integer->char.

The `matrix` proposal requires all 4^3^=64 conversions.

  * '''Options:''' matrix, list, vector, list+vector, latin-1, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #444 Add vector-append procedure ===

This is for completeness with `append` and `string-append`.  See #436
for the Formal Comment that triggered this ticket.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #451 Add bytevector-append procedure ===

This is for consistency with `append`, `string-append`, and
`vector-append` (per ticket #444) procedures.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #445 Bidirectional ports and port-open? ===

Replace `port-open?` with `input-port-open?` and `output-port-open?`,
since a bidirectional port can be closed on one side without the
other.  See Formal Comment #439.

Vote `replace` to replace `port-open?` with just the two new versions,
or `add` to have all three.

  * '''Options:''' replace, add, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #450 Eliminate default for fill argument in vector-copy ===

Marc Feeley writes:

It is a bad idea for the ''fill'' parameter of `vector-copy` to have a
default. When ''fill'' is absent, it should be an error when ''start''
and ''end'' are not within the bounds of the sequence. Otherwise, some
index calculation errors (off-by-one on ''end'') may go
unnoticed. Moreover, when it is supplied, ''fill'' should also be used
when ''start'' is less than 0, for consistency with the case where
''end'' is greater to the length of the sequence.

Vote `required` to make the fill parameter required, `error` to make
it an error in the case that fill is absent yet needed, `remove` to
remove the fill parameter and signal a runtime error if end is longer
than the input vector, or `default` for the current status quo.

  * '''Options:''' required, error, remove, default, undecided
  * '''Default:''' default
  * '''Preferences:''' 

=== #404 Make handlers take a raise-continuable? argument. ===

Pass exception handlers a second, Boolean argument that declares
whether the exception is continuable.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #464 Add optional start and end parameters to utf8->string and string->utf8. ===

Per ticket 464, add optional start and end arguments to `utf8->string`
and `string->utf8`.

Vote `both` to add optional start and end arguments to both,
`string->utf8` or `utf8->string` to add them to only one procedure, or
`neither` to leave both unchanged.

  * '''Options:''' both, string->utf8, utf8->string, neither
  * '''Default:''' neither
  * '''Preferences:''' 

== WG1 - Optional Libraries ==

=== #373 (exit #t) should be the same as (exit) ===

See Formal Comment #372 for the argument.  Cowan writes: "I support this proposal.  I
don't support the alternative proposal to just say that any true value
reports success and only #f reports failure, for there is usually only
one kind of success (0 on Posix and Windows, "" on Plan 9, 2 on VMS)
and many kinds of failure."

It is reasonable and convenient to use `#t`/`#f` as generic
success/failure for portable programs, with `(exit)` as a shorthand
for the "normal" completion `(exit #t)`.

Another reasonable extension is fallback for certain success values
that the implementation cannot understand.  Specifically, `0` is
commonly used for success on Posix systems, and the empty string "" as
success on Plan9.  We could require that if the implementation does
not know how to pass these value types (string or number) to the OS,
then it should recognize `0` and `""` as true.  Any value other than
these which cannot be passed to the OS should be treated as a generic
error.  That way, a program written for Posix that alternatively uses
`(exit 0)` and `(exit <n>)` will still work as desired on a Plan9
system, only losing details of the type of failure (and likewise for
Plan9 programs running on Posix).

In either case, unless someone makes a proposal to the contrary,
unknown values should always be treated as generic failure, and never
raise an exception or fail to exit (from #374).

  * '''Proposals:''' 
    * '''boolean:''' Only `#t`/`#f` are as described as above, and all other values are passed (as best as possible) to the OS and therefore implementation-defined
    * '''extended-true:''' `#f` is generic failure, `#t` generic success, and `""` and `0` are generic success if not otherwise understood by the OS
  * '''Options:''' boolean, extended-true, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #375 Add EMERGENCY-EXIT procedure ===

This procedure provides instant guaranteed process exit without
running `dynamic-wind` thunks.  This is a low-level and dangerous
procedure.

Vote `emergency-exit` to add this procedure, or `no` to leave it out.
If you want to write in an alternate name, be sure to include
`emergency-exit` as a secondary option after it.

  * '''Options:''' emergency-exit, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #394 Ditching SCHEME-REPORT-ENVIRONMENT and NULL-ENVIRONMENT ===

Cowan writes:

"I have reluctantly come to the same conclusion as the R6RS editors:
that in a Scheme with libraries, `scheme-report-environment` and
`null-environment` don't make much sense.  They are not in IEEE Scheme
or R4RS, so there is no formal barrier to removing them.

"Semantically, `scheme-report-environment` holds all the identifiers in
R5RS, excepting any which the implementation doesn't provide, like
`make-rectangular` if it does not have complex numbers.
`Null-environment`, on the other hand, contains only the syntax
keywords with none of the standard procedures: it is not an empty
environment.  R6RS preserves these procedures only in the R5RS
compatibility library, where they expose only R5RS content.

"When adapting the definition to R7RS, I changed
`scheme-report-environment` to contain all the identifiers in all the
standard libraries that the implementation provides, and
`null-environment` all the syntax keywords in those libraries.  This
was the best I thought I could do, but now I think that it provides
very little utility.

"It's possible to construct any specific environment you want by using
the `environment` procedure, which turns a sequence of import-specs
into an environment.  In particular, we now have the `(scheme r5rs)`
library, which essentially provides what
`(scheme-environment-procedure 5)` should provide, and there is no
portable use of any argument other than 5."

Vote `remove` to remove these two procedures entirely, or `move` to
move them from (scheme eval) and provide them only as portability
options in `(scheme r5rs)`, where only the argument 5 is required to
be supported.  Vote `keep` to leave them as-is.

  * '''Options:''' remove, move, keep, undecided
  * '''Default:''' keep
  * '''Preferences:''' 

=== #413 EVAL accepts DEFINE ===

The proposal is to require `eval` to accept definitions as well as
expressions, as long as the specified environment is mutable.  See
EvalDefine for which Schemes already handle this.

  * '''Options:''' yes, no, unspecified, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #399 clarify which primitives are allowed to implicitly force ===

The standard allows the following extension to force:

  Some implementations may implement "implicit forcing," where the
  value of a promise is forced by primitive procedures like `cdr'
  and `+'

We should remove this note or tighten the definition.

A simple definition is any primitive that would require a type-check
can perform implicit forcing.  This would include all type predicates
themselves except for `promise?`.  Note if #405 passes, then in
implementations which support this extension an object could return
`#t` for `promise?` in addition to one other type.

  * '''Options:''' remove, type-check, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #405 Make promises first-class ===

Currently there is no way to inspect an object to see if it's a
promise.  This proposal makes promises first-class by adding a
`promise?` predicate.  It also requires that if the argument to
`make-promise` is a promise, it is returned without rewrapping it, and
that if `force` is given a non-promise argument, it returns it
unchanged.  (These things cannot be provided by the user without a
`promise?` predicate, and are trivial to provide with it.)

Vote `disjoint` to add `promise?` and make it a disjoint type, or
`yes` to add it as a not-necessarily disjoint predicate.

  * '''Options:''' disjoint, yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #462 end of line definition ===

The definition of read-line allows implementation defined extensions
to the set of end of line sequences. This is arguably too loose, as an
implementation could define "a" as and end of line. On the other hand,
if we do want to leave this in it may make sense to remove "\r", which
is no longer used in any contemporary OS.

Vote `no-extensions` to forbid implementation defined extensions,
`no-return` to remove a single return from the list of required end of
lines, and `none` to leave as-is.

  * '''Options:''' no-extensions, no-return, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #452 provide digit-value support for non-decimal-digits ===

In ballot 4, in symmetry with the new Unicode definition of
`char-numeric?` and as an analog to CL's `digit-char-p`, we provided
`digit-value`.

An informal comment was made questioning this procedure, and
suggesting if provided at all it be extended to hex digits.

Vote `ascii-hex` to support only the ASCII hex digits a-f,A-F (in
addition to full Unicode numeric digits), `unicode-hex` to support all
Unicode variants of a-f,A-F (need to define formally).

Vote `ascii-radix` or `unicode-radix` to have both `digit-value` and `char-numeric?` take a radix argument, such that `char-numeric?` returns #t and `digit-value` returns the appropriate value for characters representing non-numeric digits of that radix under ASCII or Unicode character encodings, respectively, and for characters representing numeric digits under Unicode.  Implementations are required to support at least the radix values: 2, 8, 10, and 16, and may support others.

Vote `remove` to remove `digit-value` entirely, `remove-radix` to remove `digit-value` entirely, but add the radix argument to `char-numeric?` as described above, or `keep` to keep as is.

  * '''Options:''' ascii-hex, unicode-hex, ascii-radix, unicode-radix, remove, remove-radix, keep, undecided
  * '''Default:''' keep
  * '''Preferences:''' 

== WG1 - Non-normative ==

=== #411 Reference implementation ===

Our charter calls for one or more reference implementations.  As of
today, Chibi is very close to being so.  The proposal is to bless it
as a sample or model implementation, but not technically a reference
implementation -- if it disagrees with the standard, the standard
wins.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #463 library naming conventions  ===

We currently use the singular form of data types for library names,
e.g. `(scheme char)` and `(scheme file)`.  R6RS prefers the plural, as
in `(scheme lists)` and `(scheme records)`. We should decide
officially which is preferred.

  * '''Options:''' singular, plural, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 


== WG1 - Late additions ==

=== #465 Add jiffy-modulus to specify when, if ever, current-jiffy wraps ===

If the value of `current-jiffy` is to be both space-efficient (that is, a fixnum) and reasonably precise (say, microsecond timing), it needs to wrap around: 30-bit fixnums on a 32-bit system will wrap every 17 minutes.  That means an application needs to know what the maximum value is before it wraps back to zero.  The `jiffy-modulus` function returns the maximum value of the current jiffy plus 1.  Alternatively, jiffies can be signed and wrap from (- (jiffy-modulus) 1) to (- (jiffy-modulus)), which is easier for the implementation but harder for the user.

  * '''Options:''' unsigned, signed, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #466 case folding of character names ===

In ticket #11 we voted to make the reader case-sensitive
by default. In ticket #92 we further added the R6RS
#!fold-case and #!no-fold-case reader extensions. In
both cases the terminology was lax and simply referred
to "reader case sensitivity", and all discussion centered
around symbols, although in R6RS character names were
also affected.

Case folding will apply to numeric literals, booleans and
bytevectors regardless, as they do in both R5RS and R6RS.
We need to clarify how character names and the case
folding directives themselves are handled.

The default is `r6rs`, where character names are case
sensitive by default and folded by the `#!fold-case` flag:

​http://www.r6rs.org/final/html/r6rs-app/r6rs-app-Z-H-4.html#node_chap_B

Alternately character names could be made to ignore
the reader directives and always or never fold case.
Never folding case breaks R5RS and earlier compatibility
without any easy workaround.

These same settings apply to the `include-ci` syntax.

  * '''Proposals:'''
    * '''r6rs:''' character names behave like symbols, directives are sensitive
    * '''r6rs+directives:''' like `r6rs` but directives can also be case-folded
    * '''always-fold:''' like `r6rs` but character names and directives always fold case
    * '''never-fold:''' like `r6rs` but character names and directives never fold case
  * '''Options:''' r6rs, r6rs+directives, always-fold, never-fold, undecided
  * '''Default:''' r6rs
  * '''Preferences:''' 

=== #467 Allow eqv? and eq? to return different answers on procedures as well as integers and characters ===

This proposal stems from [[http://lists.r6rs.org/pipermail/r6rs-discuss/2012-July/006405.html|remarks]] by Alaric Snell-Pym and Will Clinger on the r6rs public mailing list.  If `eq?` is allowed to return `#f` on two procedures when `eqv?` nevertheless returns `#t`, as is already the case for numbers and characters, then more intelligent implementation-specific procedure comparisons using `eqv?` are possible, while still keeping `eq?` simple enough to inline easily.

Note that this is orthogonal to the question of #460, how `eqv?` works on procedures.  There should be little or no backward-compatibility hit for this change.

  * '''Proposals:'''
    * '''same:''' `eq?` and `eqv?` always return the same on procedures, per R5RS and R6RS
    * '''different:''' `eq?` may return `#f` on procedures even when `eqv?` returns `#t` (but not vice versa)
  * '''Options:''' same, different, undecided
  * '''Default:''' same
  * '''Preferences:''' 
