== WG1 - Core ==

=== #32 user-defined types ===

Do we support any means of creating disjoint user-defined types, such
as in SRFI-9, SRFI-99 or the R6RS record system?

  * '''Proposals:'''
    * '''cowan:''' RecordsCowan
    * '''hsu:''' RecordsArcfide
    * '''medernach:''' AggregatesMedernach
    * '''rush:''' UserAggregatesRush
    * '''snellpym:''' UniqueTypesSnellPym
  * '''Options:''' srfi-9, srfi-57, srfi-99, r6rs, cowan, hsu, medernach, rush, snellpym, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' medernach, snellpym, rush, hsu, srfi-9, cowan, wg2, srfi99, r6rs, snellpym+inheritance, snellpym+mutate, snellpym+inheritance+mutate, srfi-57, none, undecided

IMHO Core Scheme should have the ability to create disjoint kind of data, this is the essence of data types. On top of that capability the various kind of records could be build. I don't think much more is needed inside the core, let other features be for WG2. SRFI9 is nice but I am a bit afraid about its real extension potential to make it part of WG1 standard.

UniqueTypesSnellPym proposal is in the spirit of this, however I dislike to select fields by numbers and the list of flags for mutation looks hairy to me: it means for instance that I have to know the number of fields of the parent type, even worse: adding a field in a parent force to shift all the hierarchy of subtypes, this is too much tight together.  Instead of a list of flags my proposal allow to have all fields mutable and controlling mutability by exporting with a module system only what mutators I allow to use.

Update:  I overtook that RecordsCowan inheritance is based on field numbering. Then there are the same problems than in snellpym+inheritance. However it is a nice and simple proposal but I prefer widespread SRFI-9 over it (no inheritance than number-based inheritance).


=== #50 byte-vectors ===

Several SRFIs, R6RS, and most Scheme implementations support some sort
of uniform packed integer vectors.  In particular, these are necessary
for efficient binary I/O, and for memory mapping, so WG2 will
certainly want them.

Do we provide a syntax and basic API for these in WG1?

  * '''Proposals:'''
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2|R6RS byte-vectors]]
    * '''cowan:''' BlobAPI
    * '''snellpym:''' BlobsAndSRFI4SnellPym
  * '''Options:''' r6rs, cowan, snellpym, wg2, none, undecided
  * '''Default:''' none
  * '''Preferences:''' cowan, r6rs, snellpym, undecided, wg2, none


=== #55 Lazy evaluation ===

R5RS provides a simple mechanism for easy cases of lazy evaluation.
It does not support generalized lazy evaluation, because all built-in
procedures are eager whether they 'need' to be or not.  The relevant
identifiers are `delay` and `force`; they are not present in IEEE
Scheme.  SRFI 45 argues that this pair is insufficient for expressing
common lazy algorithms in a way that preserves tail recursion, and
adds `lazy` (equivalent to `(delay (force ...))`, but acting
atomically) and `eager`.  The semantics of `delay` and `force` remain
downward compatible.

Vote `srfi-45` to add just the bindings `lazy` and `eager` in addition
to `delay` and `force`, not all of the `srfi-45` utilities.  Vote
`none` to remove `delay` and `force` from the standard.

  * '''Options:''' r5rs, srfi-45, none, wg2, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' srfi-45, r5rs, wg2, undecided, none


=== #57 Random Numbers ===

Random numbers are useful for a wide variety of applications,
including cryptography, statistics, games, simulations and genetic
programming.  Do we want to provide an interface to random number
generation in WG1 Scheme?

  * '''Proposals:'''
    * '''srfi-27:''' [[http://srfi.schemers.org/srfi-27/srfi-27.html|SRFI-27]]
    * '''cl:''' RandomnessCommonLisp
    * '''cowan:''' RandomCowan
    * '''hsu:''' RandomnessArcfide
  * '''Options:''' srfi-27, cl, cowan, hsu, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' cl/module, srfi-27/module, cowan/module, cl, srfi-27, cowan, hsu, wg2, undecided, none

This is really a module issue, let people choose among a set the one which fits the best their needs. Standardize names only for helping code reuse. By the way if one need repeatability why not roll their own random stream from a saved persistent table ? (as good old random number tables one could buy for those who knew about it :)


=== #62 Environment Variables ===

Currently, there is no standard way to communicate with the context
from which a Scheme program was started.  This has become pretty
standardized over time: a list of strings ("command-line arguments")
and a map from strings to strings ("environment variables") on input,
and a small integer or string on output ("exit value").  Scheme should
recognize these realities.

We have `command-line` and `exit` from ModulesShinn, so the question
remains if we should add SRFI-98 environment accessors.

  * '''Options:''' srfi-98, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' srfi-98/module, wg2, none, undecided

This should not be enforced, as embedded devices don't need it. Providing it as a module is a good choice.


=== #68 "Undefined value" vs. "undefined values" ===

In R5RS, many procedures and syntax forms return an "undefined value".
In R6RS, the corresponding convention is to return "undefined values",
meaning an undefined number (including zero) of undefined values.  How
shall R7RS go?

Vote `r5rs` for a single undefined value, `r6rs` for zero or more
undefined values, or `zero` for exactly zero values.  Anything other
than `r5rs` would break R5RS (and IEEE) compatibility.

  * '''Options:''' r5rs, r6rs, zero, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' r6rs, zero, r5rs, undecided

What is the rationale behind this ? Is it to allow partial functions returning an <undefined> value and to propagate it ? Then IMHO the best semantic is to return empty value.

Update: I am now convinced that R6RS phrasing is the more flexible option. However I exhort using "(values)" whenever possible.

=== #49 Undefined value API ===

Assuming a single "undefined" value (dependent on the result of #68),
users sometimes want to test for this value.  If we enforce a unique
undefined value, one approach is to generate this value explicitly to
test against, such as `(void)` provided by some implementations.
Alternately we could provide a test such as `undefined?`.  Either
approach breaks compatibility with existing extensions, and may be
prohibitively difficult to implement for compilers lacking a separate
undefined value type.  Some programmers also consider testing for this
value sign of a broken design.

Vote `generate` for a `(void)` procedure, `test` for `undefined?`, and
`both` for both.

  * '''Options:''' generate, test, both, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' none, generate, undecided, wg2, both, test

Testing undefined values is strange to say the least...  It seems better to have error handling instead when appropriate.


=== #51 support for cyclic structures in primitives ===

list?, length, equal? and other fundamental primitives may diverge
when given cyclic data.  In the former two cases, avoiding this is
simple and not inefficient, and the equivalents are already provided
in SRFI-1.  In the latter case a
[[http://www.r6rs.org/r6rs-editors/2006-February/000969.html|proposal]]
was made and rejected on the R6RS list.  In the former case, R6RS
seems to require `list?` return `#f` and `length` raise an error.

Do we want to specify the behavior when these primitives encounter
cyclic data?

Options are `equal?` to specify `equal?` must terminate on cyclic
input, `r6rs` to specify R6RS behavior for `list?` and `length`,
`srfi-1` to specify the SRFI-1 semantics (where `length` returns `#f`)
and `equal?+r6rs` or `equal?+srfi-1` are options for both.

  * '''Proposals:''' 
    * '''equal?:''' `equal?` is required to do proper checks for cyclic structure to not diverge
    * '''r6rs:''' `list?` should return `#f` and `length` raise an error for cyclic lists
    * '''srfi-1:''' `length` is equivalent to `length+` and returns `#f` for a cyclic list
  * '''Options:''' equal?,r6rs,srfi-1,equal?+r6rs,equal?+srfi-1,no,module,wg2,undecided
  * '''Preferences:''' equal?+r6rs,equal?,r6rs,module,wg2,undecided,no,equal?+srfi-1,srfi-1,

Don't let 'length' returns #f, please raise an error instead.


=== #69 Dynamic Parameters ===

Old-fashioned Lisps used dynamic extent of variables.  Although Scheme
has switched to lexical scope, the concept of a dynamic environment
can be useful in special cases.

Instead of special variables, SRFI-39 provides first-class "parameter"
objects with dynamic bindings.  Do we want to provide something
similar?

  * '''Proposals:'''
    * '''srfi-39:''' [[http://srfi.schemers.org/srfi-39/srfi-39.html|SRFI-39]]
    * '''cowan:''' ImmutableParametersCowan
    * '''snellpym:''' ParametersSnellPym
  * '''Options:''' cowan, snellpym, srfi-39, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' cowan, srfi-39, wg2, snellpym, undecided, none, 


=== #70 accessing the system time ===

Short of a full time and date library, a single procedure

  (current-seconds)

returning the epoch time in seconds, possibly as a real number, would
be useful and is sufficient to implement a full library (though access
to the host system's timezone would be desirable in that case).

Since some systems may not have access to a clock, we could make this
an optional procedure.  Alternately, it could be defined as a simple
counter in such cases, providing an accurate notion of time ordering
but no sense of duration. Finally, it could return `#f` in the absense
of a clock.

  * '''Proposals:'''
    * '''cowan:''' TimeCowan
    * '''required:''' `(current-seconds)` must return seconds since epoch
    * '''optional:''' `(current-seconds)` is an optional procedure
    * '''counter:''' `(current-seconds)` may just be a counter, returning 0, 1, 2, ...
    * '''return-false:''' `(current-seconds)` may return `#f` if unsupported
  * '''Options:''' cowan, required, optional, counter, return-false, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' optional, cowan/module, required, counter, undecided, wg2, none, return-false

We should make a difference between a date (social convention about naming points in time) and a measure of a time interval. IMHO this has to be optional and inside a module. ( And if this is optional we need of course a standard feature identifier to know whether it is present or not. )

About counter option, I would prefer not current-seconds returns tick instead of seconds : If only ticker is available it may be better to have a "current-tick" function instead in order to know that this is not seconds but ticks. 



=== #109 elapsed time ===

Should we have functions allowing a program to compute elapsed time,
as distinct from calendar time?

TimeCowan contains a proposal.

  * '''Options:''' cowan, yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' Chronometer/module, yes, wg2, cowan, undecided, no

This has to be optional feature.  A timer (or a ticker) is a definitely distinct from calendar time.  I don't get the "jiffy" rationale.  Something like the Chronometer proposal is more appealing to me because the reference time point is clearly stated.


=== #78 Should we have case-lambda? ===

Should we provide case-lambda as in SRFI 16 and R6RS?  It provides
simple overloading of procedures based on the number of their
arguments, and does not require that optional arguments appear only
after mandatory ones.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, wg2, module, no, undecided


=== #82 missing port? procedure ===

It's not clear whether R5RS requires a PORT? procedure or not.  It's
listed in Section 3.3.2 under Disjointness of Types, but not under
section 6.6.1 under Ports.  R6RS requires it.  Racket, Gauche, MIT
Scheme, Gambit, Chicken, Guile, SISC support it; Scheme48/scsh, Kawa,
and Chibi currently do not.

Shall we require it?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' yes

=== #107 port status detection ===

Currently there's no way to determine whether a port is open or
closed, short of trying to read/write to it and catching an error.
Do we want to add an interface to this?

  * '''Options:''' port-open?, port-closed?, both, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' port-open?, port-closed?, wg2, both, undecided, no


=== #87 Allow multiple producers in `call-with-values` ===

In R5RS and R6RS, `call-with-values` takes two arguments, both
procedures.  The first is a ''producer'' of multiple values; the
second is a ''consumer'', to which the multiple values returned by
''producer'' are passed as arguments.

A possible extension is to allow multiple ''producer'' arguments,
flattening all the produced values together, analogous to Common
Lisp's `multiple-value-call`.

Do we add this extension?

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' no, wg2, undecided, yes

I don't see the added value of this.


=== #88 SRFI 87: => in CASE ===

SRFI-87 extends `case` with a `=>` clauses, analogous to the use of
`=>` in `cond` clauses, which allows you to pass the item actually
matched to a procedure.

Do we add this extension?

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' no

We trivially have <key> already in lexical scope, don't we ?


=== #89 SRFI 61: COND => with generator and guard ===

SRFI-61 extends `=>` clauses in `cond` with an optional ''guard''
form, such that after the value is generated and found to be true,
it's further checked against the guard.  If the guard returns `#f` the
clause fails and processing proceeds to the next clause, otherwise the
clause is accepted as normal.

Do we add this extension?

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' no, wg2, undecided, yes

Not for WG1, Ok for WG2 if one wants it.

=== #90 Multiple values in COND => clauses ===

Currently, `=>` clauses in `cond` accept a single value from the
''generator'' (right-hand side) and pass it to the ''receiver''
(left-hand side).  Shall we allow the generator to return multiple
values and pass them to the receiver?  If both this ticket and #89
pass, multiple values would also be allowed for generator/guard `cond`
clauses.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' no, wg2, undecided, yes


=== #91 INCLUDE at the REPL ===

Should we allow `(include "''filename''")` at the REPL?  This is
distinct from `import` in that it just loads code without altering the
module structure.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, wg2, undecided, no


=== #92 Case-folding flags ===

The default reader in R7RS will default to case-sensitive, but users
may wish to override this in some situations.  R6RS allows at the
top-level #!case-fold and #!no-case-fold read syntax to control the
case-sensitivity of the current file.  Many existing R5RS
implementations, on the other hand, use #ci and #cs, with the
difference that they refer to the next datum only.

Note PortsCowan provides a separate means of controlling
case-sensitivity per-port.

Vote `per-datum` for the next-datum-only #ci/#cs syntax.

  * '''Options:''' r6rs, per-datum, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:''' r6rs, none, wg2, undecided, per-datum


=== #116 Source File Character Encoding ===

The standard currently says nothing about the character encoding
system of source files.  Do we require this to be a fixed encoding
such as UTF-8, use an out-of-band specification like the Emacs (and
Python) `-*- coding: foo -*-` convention, or just leave it
unspecified?

  * '''Options:''' utf-8, emacs, unspecified, undecided
  * '''Default:''' none
  * '''Preferences:''' unspecified, emacs, utf-8, undecided


=== #93 Removing string mutability ===

R6RS relegated `string-set!` to a module, and many modern languages
tend towards making strings immutable.  Removing entirely, however,
breaks IEEE Scheme compatibility and should only be considered if you
believe mutable strings are fundamentally broken.

Do we remove `string-set!`?  Vote `yes` to remove, `module` to
relegate to a module as in R6RS, or `no` to keep as is.

  * '''Options:''' yes, no, module, undecided
  * '''Default:''' no
  * '''Preferences:''' module, yes, undecided, no


=== #83 Auxiliary Keywords ===

In R6RS auxiliary keywords (such as `else` in `cond` and `case` forms)
are explicitly exported from the `(rnrs base (6))` library.  Do we
want to bind and export these from the core library?

If `else` is bound in the default module, then it must be imported at
the call site whenever using it in `cond` or it won't match
hygienically.

If `else` is '''not''' bound in the default module, then it must not
be bound or imported at the call site whenever using it in `cond` or
it won't match hygienically.

Another option is to specify for `cond` and `case` that they match the
`else` identifier literally, ignoring any hygiene.  This breaks
compatibility with R5RS and R6RS.

  * '''Options:''' bound, unbound, unhygienic, undecided
  * '''Preferences:''' undecided


=== #101 exactness and `eqv?`/`equal?` ===

In R5RS `eqv?`/`equal?` are in some sense the broadest tests for
equality, comparing structural equality, but also tests for the same
exactness, so that

   {{{(equal? 0 0.0) => #f}}}

whereas

   {{{(= 0 0.0) => #t}}}

Some users consider this confusing, others sometimes want an `equal?`
that behaves like `=` for numbers.

Do we want to change `equal?` and `eqv?` in this way, or add a
separate exactness-agnostic procedure?  Vote `yes` to change,
`equal=?` or `inexact-equal?` for separate procedures of those names
(plus the analogous `eqv=?` or `inexact-eqv?`), or `no` to leave as
is.  Alternately, write in a separate name.

  * '''Options:''' yes, equal=?, inexact-equal?, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' no

=== #102 module syntax name ===

A bikeshed color issue, we need to choose the
actual names for the module syntax for the winner
of #2.

`import`, `export` and `include` are fairly universal
and no alternate is suggested unless someone wants
to write in a proposal.

The enclosing syntax can be called either
`library` as in R6RS, `module` or some other proposal.

  * '''Options:''' library, module, undecided
  * '''Default:''' library
  * '''Preferences:''' module, library


=== #103 module body syntax name ===

Similar to #102, we need to choose a name
for the form to include Scheme code directly
in a module form.  This can be `body` as in
the proposal, `begin` or some other name.

  * '''Options:''' body, begin, scheme, code, undecided
  * '''Default:''' body
  * '''Preferences:''' body, begin


=== #105 case-insensitive module includes ===

The `include` module form includes files literally
with the default case-sensitivity.  An `include-ci`
form could include files case-insensitively
without resorting to the reader hacks proposed in
#92, allowing existing R5RS libraries to be used
without modification.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' undecided


=== #106 conditional code selection ===

Users invariably want some way to conditionally select code depending
on the implementation and/or feature set available. CondExpandCowan
allows conditional expansion in the style of SRFI-0 within the module language.
[[http://srfi.schemers.org/srfi-0/srfi-0.html|SRFI-0]] provides
`cond-expand`, [[http://srfi.schemers.org/srfi-103/srfi-103.html|SRFI-103]]
provides a library naming extension, and numerous other personal hacks exist.

Do we want to include something along these lines in WG1 Scheme?

  * '''Proposals:'''
    * '''cowan:''' CondExpandCowan
    * '''srfi-0:''' `cond-expand` only defined as a top-level module form
    * '''srfi-103:''' the search path extension used by R6RS implementations
  * '''Options:''' cowan, srfi-0, srfi-103, none, wg2, undecided
  * '''Default:''' none
  * '''Preferences:'''  undecided, cowan, srfi-0, srfi-103 | wg2, none

Definitely needed, however not so sure if it is the right choice.


=== #108 immutable data interface ===

R5RS specifies literal data in source code as immutable, but otherwise
provides no way to generate or introspect immutable data.

One proposal is given in ImmutableData, providing `mutable?`,
`make-immutable` and `immutable->mutable`.

Racket, for which all pairs are immutable in the default language,
needs some way to generate shared and cyclic data structures at
runtime, and provides the `shared` syntax for this.  It also has an
`immutable?` utility as the complement to `mutable?` above.

  * '''Proposals:'''
    * '''medernach:''' ImmutableData
    * '''racket:''' `shared`, `immutable?` ([http://docs.racket-lang.org/reference/shared.html])
  * '''Options:''' medernach, racket, no, undecided
  * '''Default:''' no
  * '''Preferences:''' undecided, medernach | shared, no

As stated earlier I personnaly think these are orthogonal features and I like both. Now there was not enough discussions about it to be included.


=== #111 require `equal?` to return `#t` if `eqv?` does ===

Currently `equal?` is strictly broader than `eqv?` except in the
pathological case of comparing the same circular list with itself, for
which `eqv?` returns true and `equal?` may loop infinitely.  We could
explicitly require `equal?` to check and return `#t` in this case,
which most implementations do as a performance hack anyway.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' undecided

== WG1 - Exceptions ==

=== #18 exception system ===

R6RS provided a detailed exception system with support for raising and
catching exceptions, using a hierarchy of exception types.

Do we use this, or parts of it, or a new exception system?  The `r6rs`
option is just for the core exception handling.

  * '''Proposals:'''
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-8.html#node_sec_7.1|R6RS Exceptions]] - `with-exception-handler`, `guard`, `raise`, `raise-continuable`
    * '''cowan:''' ExceptionHandlingCowan
  * '''Options:''' cowan, r6rs, wg2, none, undecided
  * '''Default:''' none
  * '''Preferences:''' none, wg2, undecided, r6rs, cowan

I feel exceptions as inappropriate for WG1: we already have the flexibility of call-with-current-continuation.  Moreover existing exceptions taxonomy are difficult to unify adequatly without making some complex, ad hoc, and unfortunate kludge.


=== #19 when to throw an error ===

R5RS defines many things as "is an error" without any specification of
what happens in that situation.  R6RS goes to the opposite extreme and
specifies as much as possible what exceptions are raised when.

Taking into account the system provided by ticket #18, we need to come
up with guidelines for when exceptions should be raised, and clarify
which R5RS "error" situations should raise an exception or be left
unspecified.

R5RS specifies only 3 situations where an error is required to be
signalled, leaving most situations unspecified as described in
ErrorSituations.

  * '''Options:''' r5rs, r6rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' undecided

Discussions needed.

== WG1 - I/O ==

=== #28 binary I/O ports ===

Do we provide any binary input or output ports, and if so how do we
construct them and operate on them?  Can binary and textual operations
be mixed on the different port types?

PortsCowan provides binary port operations along with other
extensions.

R6RS provides an entirely new I/O system, as well as a separate
R5RS-compatible I/O system.

The withdrawn SRFI-91 provides yet another I/O system supporting
binary ports.

Note this item as well as #29 and #31 specify semi-orthogonal aspects
of I/O systems which are typically specified together by individual
proposals.  If the same proposal doesn't win for all three, the
aspects will be merged as needed.

  * '''Proposals:''' 
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2|R6RS Port I/O]]
    * '''r6rs-simple:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.3|R6RS Simple I/O]]
    * '''srfi-91:''' [[http://srfi.schemers.org/srfi-91/srfi-91.html|SRFI-91]]
    * '''cowan:''' PortsCowan (subset relevant to binary I/O)
  * '''Options:''' r6rs, r6rs-simple, srfi-91, cowan, none, undecided
  * '''Default:''' none
  * '''Preferences:''' cowan, r6rs-simple, r6rs, srfi-91, none, undecided


=== #29 port encodings ===

Do we support encoding and decoding text from ports with different
character encoding systems?  Different end-of-line conventions?
Different normalizations?  How are encoding errors handled?

  * '''Proposals:''' 
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2|R6RS Port I/O]]
    * '''srfi-91:''' [[http://srfi.schemers.org/srfi-91/srfi-91.html|SRFI-91]]
    * '''cowan:''' PortsCowan (subset relevant to port encodings)
  * '''Options:''' r6rs, srfi-91, cowan, none, undecided
  * '''Default:''' none
  * '''Preferences:''' cowan, r6rs, srfi-91, none, undecided


=== #31 custom ports ===

Do we provide a mechanism for custom ports, on which for instance
string ports could be created?

R6RS as well as a number of Scheme implementations provide custom
ports with various APIs.

  * '''Proposals:''' 
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2|R6RS Port I/O]]
  * '''Options:''' r6rs, none
  * '''Default:''' none
  * '''Preferences:''' undecided

Discussions and review needed


== WG1 - Libraries ==

=== #36 hash-tables ===

R6RS and SRFI-69 both provide hash-table interfaces.  Do we provide
either of these, or try to provide some primitives on which efficient
hash-tables can be implemented?

  * '''Options:''' r6rs, srfi-69, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' r6rs, wg2, undecided, no


=== #113 directory contents ===

We've decided to add file-exists? and delete-file,
essential for a large class of scripts, but still
have no way to get a list of files in a directory.
Do we want to provide an interface to this?

  * '''Proposals:'''
    *  '''cowan:''' DirectoryPortsCowan
    *  '''directory-files:''' return a list of all files in the dir
    *  '''directory-streams:''' [[http://www.scsh.net/docu/html/man-Z-H-4.html#node_sec_3.3|SCSH directory stream interface]]
  * '''Options:''' directory-files, directory-streams, no, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' directory-streams, cowan, directory-files, undecided, wg2, no


== WG1 - Macros ==

=== #48 let-syntax ===

`let-syntax` and `letrec-syntax` has known ambiguities in their
behavior. We have the option of altering the semantics to correct this
behavior, defining which behavior we intend, or removing `let-syntax`
entirely.  We could also leave this ambiguity unspecified.

The question of whether or not to introduce a new lexical scope
(i.e. whether internal `define`s are visible outside the `let-syntax`)
is straightforward.

If we don't introduce a new lexical scope, the question arises whether
or not internal `define-syntax` forms are allowed and whether they
apply to the body of the `let-syntax`, forms following the
`let-syntax`, or both.

If internal `define-syntax` applies to the body, we may also wish to
specify what happens when said `define-syntax` redefines an identifier
bound by the enclosing `let-syntax`.  This varies by implementation
and may be difficult for macro expanders to change, so is left
unspecified in the proposals below.

  * '''Proposals:'''
    * '''hsu:''' LetSyntaxArcfide
    * '''remove:''' remove both of these forms from the standard
    * '''lexical:''' introduces a new lexical contour
    * '''define:''' allows splicing `define`/`begin`
    * '''syntax:''' allows `define-syntax`
    * '''syntax-body:''' allows `define-syntax` only applying to the body
    * '''syntax-follows:'''  allows `define-syntax` only applying to following forms
  * '''Options:''' hsu, remove, lexical, define, syntax, syntax-body, syntax-follows, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' lexical, remove, undecided


=== #97 syntax-rules special literals ===

`...` and with the result of #6 also `_` have special meaning in
syntax-rules patterns, so they are not treated as pattern variables by
default.

However their behavior when used in the literals list of
syntax-rules is ambiguous, and simply breaks in most implementations.

Rather than breaking, it makes sense to go ahead and treat
them as normal literals, overriding their special meanings.

In particular, there are many existing R5RS macros which
make use of `_` in the literals and are thus broken outright
by #6. Allowing them as literals fixes these macros.

  * '''Options:''' literal, error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' literal, unspecified, undecided


== WG1 - Modules ==

=== #3 module naming convention ===

We need a naming convention for the core modules and standard
libraries of the new module system.

In R5RS everything is effectively in a single module.  R6RS provides a
much more fine-grained breakdown of modules which could be
retro-fitted to the bindings we include in our standard.

John Cowan has proposed a number of module factorings in items #71,
#72, #73, #74, #75, #76, #77, as well as an I/O module breakdown in
PortsCowan.

Since the naming and breakdown must be internally consistent I'm
grouping these into a single ballot item.  Members desiring to put
forth a new proposal should specify where all bindings belong, or
specify a subset of the bindings and default the rest to some other
proposal.

Note some ballots specify explicitly whether or not the bindings in
question are intended to be in a module or the core language.  In
these cases we still need to decide to which module they belong.
Where specific votes contradict general factoring proposals, the
specific vote wins.

  * '''Proposals:'''
    * '''r5rs:''' one single module
    * '''r6rs:'''
    * '''cowan:''' #71, #72, #73, #74, #75, #76, #77
  * '''Options:''' r5rs, r6rs, cowan, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' cowan, r6rs, r5rs, undecided


== WG1 - Numerics ==

=== #79 rational-expt ===

Often a rational-only exponentiation function is useful; that is, a
rational number raised to an integer power.  Should we add this
procedure to the core so that exponentiation is available even if
inexact rationals are not provided or not imported?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' no, undecided

I don't get the point, isn't it already the job of expt ?


=== #81 What numeric tower variants should be supported? ===

NumericTower lists a plausible set of ten from fixnums only to the
full monty.  Which ones should we allow an implementation to provide?
R5RS requires only fixnums large enough to handle string and vector
indexes, while R6RS requires the full numeric tower.

Vote on '''the minimum level of support''' you want to '''require'''
(implementations may of course still provide more than this).  I've
included only the most likely options below, write in other options if
needed.

Note quaternions are a fairly rare numeric type, known to be provided
only by extensions to [[http://www.ccs.neu.edu/home/dorai/squat/squat.html|scm]]
and [[http://wiki.call-cc.org/eggref/4/quaternions|chicken]], and thus
may be difficult for other implementations to support if required.

  * '''Proposals:'''
    * '''r5rs:''' fixnum (`inexact?` may always be false)
    * '''inexact-only:''' inexact (`exact?` may be the same as `integer?`)
    * '''inexact:''' fixnum, inexact
    * '''rational:''' fixnum, inexact, rational
    * '''complex:''' fixnum, inexact, complex
    * '''r6rs:''' fixnum, inexact, rational, complex
    * '''quaternion:''' fixnum, inexact, rational, complex, quaternion
  * '''Options:''' r5rs, inexact-only, inexact, rational, complex, r6rs, quaternion, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' r5rs, inexact, inexact-only, rational, undecided, complex, r6rs, quaternion

Why do we have to require anything beyond fixnums ? Some embedded systems lacks floating point arithmetics.
That issue aside, why not standardize these features as modules ?


=== #100 integral division ===

R5RS provides quotient, modulo and remainder for integral
division. R6RS extended this with div/mod and div0/mod0. A thorough
analysis of possible division operations is provided in
DivisionRiastradh, which includes a proposal for five separate
division operator pairs.  We need to choose which API we'll provide.

  * '''Proposals:'''
    * '''riastradh:''' DivisionRiastradh
  * '''Options:''' r5rs, r6rs, riastradh, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' r5rs


== WG1 - Reader Syntax ==

=== #12 symbol literal extensions ===

In R5RS, symbols parsed as any sequence of valid symbol characters
that does not begin with a character that can begin a number.  The
three exceptions `+`, `-` and `...` are also provided.  This allows
parsing with only constant lookahead to determine type.

R6RS added additional exceptions for symbols beginning with `->`, a
common idiom, still allowing parsers to determine type with a constant
lookahead.

John Cowan proposes allowing anything that cannot be parsed as a
number to be a valid symbol.  This removes the special case
exceptions, but may require arbitrary lookahead.

Alex Shinn proposes symbols are any sequence of valid symbol
characters that does not have a prefix which is a valid number.  This
removes the special case exceptions, allows constant lookahead, and
allows extensions to number syntax.

  * '''Proposals:'''
    * '''r5rs:''' symbols may not begin with `-`, except for `-` itself
    * '''r6rs:''' symbols may not begin with `-[^>]`
    * '''cowan:''' symbols are anything that doesn't parse as a number
    * '''shinn:''' symbols may not begin with a number prefix
  * '''Options:''' r5rs, r6rs, cowan, shinn, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' r5rs, shinn, r6rs


=== #84 Need to decide on a list of named character escapes ===

The WG has voted to have a list of character names.

The list in R5RS and the longer list in R6RS are only informative.  I
suggest adopting the R6RS list and making it normative.

  * '''Proposals:'''
    * '''r5rs:''' space, newline
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html#node_sec_4.2.6|R6RS Characters]]
    * '''shinn:''' space, tab, newline, return, escape, null, alarm, backspace
  * '''Options:''' r5rs, r6rs, shinn
  * '''Default:''' r5rs
  * '''Preferences:''' shinn, r5rs, r6rs


=== #104 list of mnemonic string escapes ===

Similar to #84, we need to choose a specific list of mnemonic escapes
like \n and \t to be recognized in strings.

  * '''Proposals:'''
    * '''r5rs:''' `\\`, `\"`
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html#node_sec_4.2.7|R6RS Strings]]
    * '''shinn:''' `\\`, `\"`, `\t`, `\n`, `\r`, `\e`, `\a`, `\b`
  * '''Options:''' r5rs, r6rs, shinn
  * '''Default:''' r5rs
  * '''Preferences:''' shinn, r5rs, r6rs
 