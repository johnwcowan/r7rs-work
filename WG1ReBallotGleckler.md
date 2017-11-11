Notes about results:

* you may list as many of the options as you want in order of preference
* you are encouraged to list all options
* options are comma-delimited (ignoring space) and case-insensitive
* you may write in your own option if you announce it to the list first
* everything but the `preferences` line is free-form, and may be used for a rationale
* `wg2` means "pass this issue to WG2"
* `undecided` means I want to discuss this issue further
* abstain by leaving the preferences blank

# WG1 Ballot Items To Finalize By Oct. 31

## WG1 - Modules

### #2 Module System

As per the charter, we need a module system
proposal which allows sharing of code between
implementations.

This is one issue where we can't default to
the R5RS, since it has no module system. If
we can't come to consensus, we will have to
take the R6RS module system as-is.

Note the **r6rs--** option is just the
R6RS module system without versioning or
phasing.

* **Proposals:**
* **ganz:** [ModulesGanz](ModulesGanz.md)
* **hsu:** [ModulesAndPackagesArcfide](ModulesAndPackagesArcfide.md)
* **shinn:** [ModulesShinn](ModulesShinn.md)
* **Options:** ganz, hsu, shinn, r6rs, r6rs--, undecided
* **Preferences:** shinn, r6rs--, hsu, r6rs

I've changed my top preference to [ModulesShinn](ModulesShinn.md) for two primary
reasons: 1) It is a compatible subset of R6RS, with two syntactic
differences that should be easy to implement, so it should be easy to
use R6RS libraries that only make use of its subset of the R6RS module
system; and 2) it includes the `include` form, allowing code to be
separated from its library specification, which makes it easy to use
code that is intended to be loaded at top level, e.g. in R5RS
implementations, eliminates unnecessary indentation, and feels more
compatible with interactive development.

## WG1 - Core

### #57 Simple randomness

Student programs often want a small amount of randomness, not
necessarily of very high quality.  Shall we provide a simple interface
to a random variables in WG1 Scheme?

* **Proposals:**
* **cowan:** [RandomCowan](RandomCowan.md)
* **Options:** cowan/core, cowan/module, srfi-27/core, srfi-27/module, no, wg2, undecided
* **Preferences:** wg2, srfi-27/module, srfi-27/core, no, cowan/module

[RandomCowan](RandomCowan.md) does not allow control over the seed, so it is of such limited usefulness as to not be worth including.  The API defined by SRFI 27 does allow control of the seed, and makes random sources first class, both of which are good ideas.  However, the API is awkward, especially `random-source-state-ref` and `random-source-state-set!`.  I'd like to see WG2 do a survey of existing implementations and find something better than both of these proposals.

## WG1 - Exceptions

### #18 Exception System

R6RS provided a detailed exception system with
support for raising and catching exceptions, using
a hierarchy of exception types.

Do we use this, or parts of it, or a new exception
system?

* **Proposals:**
* **cowan:** [ExceptionHandlingCowan](ExceptionHandlingCowan.md)
* **Options:** cowan/core, cowan/module, r6rs/core, r6rs/module, wg2, none, undecided
* **Preferences:** r6rs/module, r6rs/core, wg2, cowan/module, cowan/core

While the R6RS exception system is not perfect, I'm happy with it.  In
WG1, it belongs in a module, not in the core.

If we don't agree to use the R6RS system, then I'd rather see WG2
refine it instead of including [ExceptionHandlingCowan](ExceptionHandlingCowan.md) in WG1, since
the [ExceptionHandlingCowan](ExceptionHandlingCowan.md) proposal doesn't explain the rationale for
its deviations from R6RS.  I've studied the mailing list archive and
can't find a convincing argument for [ExceptionHandlingCowan](ExceptionHandlingCowan.md), either,
so I'm sticking with R6RS or, as a fallback position, WG2.

The largest flaw I see with the R6RS condition system is that its
condition taxonomy is too coarse and focused on operating-system
issues.  Compare it with the taxonomy of Gambit or MIT Scheme, for
example.  (See [ExceptionTaxonomies](ExceptionTaxonomies.md) for details of the condition
taxonomies of many Scheme implementations.)

Some people have proposed taking the R6RS exception system but not its
condition taxonomy.  If we do, I hope we'll still standardize on some
taxonomy rather than none.  Without a common taxonomy, it's hard to
share code.

## WG1 - I/O

### #52 read/write cyclic data

SRFI-38 standardizes the #0=(1 . #0#) shared
structure notation for read/write.  In the case
of write, this can be expensive to compute, but
otherwise the common case of the repl printing
a cyclic structure results in an infinite loop.

Do we want to add support for this, as an option
or separate set of procedures?

`srfi-38` for separate procedures or `native` to require `read` and
`write` to handle cyclic notation.

* **Options:** srfi-38/core, srfi-38/module, native, no, wg2, undecided
* **Preferences:** srfi-38/module, srfi-38/core, wg2, no, native

SRFI 38 supports this notation, but doesn't require that `read` and
`write` support it, so writing doesn't have to become more expensive.
Since many programs won't need this feature, it belongs in a module.

## WG1 - Macros

### #8 SRFI-46 ellipse specifier in syntax-rules

As an alternative to #7, SRFI-46 proposed
allowing an optional ellipse specified as
an identifier before the literals list in
syntax-rules:

> (syntax-rules ::: ()
> <ellipse now represented as ::: instead of ...>)

Do we allow this?

* **Options:** yes/core, yes/module, no, wg2, undecided
* **Preferences:** no, wg2, yes/module

### #9 tail patterns in syntax-rules

SRFI-46 and R6RS both allow a fixed number of
tail patterns following an ellipsis in a syntax-rules
pattern:

> (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn)

R6RS further allows dotted tail patterns

> (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px)

where Px only matches a dotted list.

Do we allow either or both of these extensions?

* **Options:** tail/core, tail/module, dotted-tail/core, dotted-tail/module, both/core, both/module, no, wg2, undecided
* **Preferences:** both, tail, wg2

I don't feel strongly about this issue, but the extensions seem
harmless.

## WG1 - Numerics

### #22 mantissa widths

R6RS introduced the concept of mantissa widths
as an alternative to the R5RS #s in numbers.
Do we want either or both of these?

* **Options:** r5rs, r6rs, both, no, wg2, undecided
* **Preferences:** both, wg2, r6rs, r5rs

I don't have a good enough understanding of the importance of this
issue to have a strong opinion, so I'm following GJS, who is of course
a great mathematician and programmer, and have chosen "both" as my
first preference.

## WG1 - Reader Syntax

### #11 case-sensitivity

Does the reader fold case by default, and if so how?

Yes to fold-case (R5RS) no to preserve case (R6RS), additional votes
to come later from specific proposals.

* **Options:** yes, no, implementation-determined, undecided
* **Preferences:** yes, implementation-determined

### #14 alternate comment syntax

R6RS provides support for #; nested sexp comments,
and #| ... |# nested block comments.  Do we include
either or both of these?

* **Options:** sexp, block, both, no, wg2, undecided
* **Preferences:** both, block, wg2, sexp, no

## WG1 - Strings and Chars

### #26 string normalization

R6RS provides procedures to explicitly convert
strings back and forth between the four Unicode
normalization forms.

The previous phrasing of this option was overly vague, referring to
"any form of normalization."  I've had to treat `yes` votes as
undecided for lack of a better default.  If you voted `yes` before
please choose one of the following options or write in your own
proposal.

* generic - `string-normalize` converts to a single implementation-defined normal form
* separate - `string-compose-canonical`, `string-decompose-canonical` and `string-decompose-compatibility` gives orthogonal control over the normalization being performed
* specific - `string-normalize-{nfd,nfc,nfkd,nfkc}` converts explicitly to the four normal forms defined in the Unicode standard
* agnostic - `string-ni=?' etc. provides an API of basic normalization insensitive procedures without explicitly converting the strings, analagous to `string-ci=?'

Note [UnicodeCowan](UnicodeCowan.md) currently provides specific normalization
procedures.

* **Options:** generic/core, generic/module, separate/core, separate/module, specific/core, specific/module, agnostic/core, agnostic/module, no, wg2, undecided
* **Preferences:** wg2, specific/module

The complexity of Unicode should be, as much as possible, banished
from WG1.  If it is included, however, I'll follow John's lead, since
he is a Unicode expert, and vote for specific/module.
