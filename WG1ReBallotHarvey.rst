Notes about results:

  * you may list as many of the options as you want in order of preference
  * you are encouraged to list all options
  * options are comma-delimited (ignoring space) and case-insensitive
  * you may write in your own option if you announce it to the list first
  * everything but the `preferences` line is free-form, and may be used for a rationale
  * `wg2` means "pass this issue to WG2"
  * `undecided` means I want to discuss this issue further
  * abstain by leaving the preferences blank

= WG1 Ballot Items To Finalize By Oct. 31 =

== WG1 - Modules ==

=== #2 Module System ===

As per the charter, we need a module system
proposal which allows sharing of code between
implementations.

This is one issue where we can't default to
the R5RS, since it has no module system. If
we can't come to consensus, we will have to
take the R6RS module system as-is.

Note the '''r6rs--''' option is just the
R6RS module system without versioning or
phasing.

  * '''Proposals:'''
    * '''ganz:''' ModulesGanz
    * '''hsu:''' ModulesAndPackagesArcfide
    * '''shinn:''' ModulesShinn
  * '''Options:''' ganz, hsu, shinn, r6rs, r6rs--, undecided
  * '''Preferences:''' shinn, undecided, r6rs--, ganz, hsu, NO, r6rs

I know there's no NO option, but according to John's excellent chart,
the r6rs option is the only one that's way, way, way worse than nothing,
because of "phasing: yes."  Shinn gets the edge on simplicity: top-level
only, explicit exports, etc.  Ganz beats out Hsu also on simplicity,
since Hsu has two columns in the table with different properties, already
way too complicated.

I feel compelled to reiterate that none of these proposals strikes me
as jewel-like.

== WG1 - Core ==

=== #57 Simple randomness ===

Student programs often want a small amount of randomness, not
necessarily of very high quality.  Shall we provide a simple interface
to a random variables in WG1 Scheme?

  * '''Proposals:'''
    * '''cowan:''' RandomCowan
  * '''Options:''' cowan/core, cowan/module, srfi-27/core, srfi-27/module, no, wg2, undecided
  * '''Preferences:''' cowan/module, srfi-27/module, cowan/core, srfi-27/core

== WG1 - Exceptions ==

=== #18 Exception System ===

R6RS provided a detailed exception system with
support for raising and catching exceptions, using
a hierarchy of exception types.

Do we use this, or parts of it, or a new exception
system?

  * '''Proposals:'''
    * '''cowan:''' ExceptionHandlingCowan
  * '''Options:''' cowan/core, cowan/module, r6rs/core, r6rs/module, wg2, none, undecided
  * '''Preferences:''' none, wg2, cowan/module, r6rs/module

== WG1 - I/O ==

=== #52 read/write cyclic data ===

SRFI-38 standardizes the #0=(1 . #0#) shared
structure notation for read/write.  In the case
of write, this can be expensive to compute, but
otherwise the common case of the repl printing
a cyclic structure results in an infinite loop.

Do we want to add support for this, as an option
or separate set of procedures?

`srfi-38` for separate procedures or `native` to require `read` and
`write` to handle cyclic notation.

  * '''Options:''' srfi-38/core, srfi-38/module, native, no, wg2, undecided
  * '''Preferences:''' srfi-38/module, srfi-38/core, wg2, no, native

== WG1 - Macros ==

=== #8 SRFI-46 ellipse specifier in syntax-rules ===

As an alternative to #7, SRFI-46 proposed
allowing an optional ellipse specified as
an identifier before the literals list in
syntax-rules:

  (syntax-rules ::: ()
     <ellipse now represented as ::: instead of ...>)

Do we allow this?

  * '''Options:''' yes/core, yes/module, no, wg2, undecided
  * '''Preferences:''' wg2, yes/module, no

=== #9 tail patterns in syntax-rules ===

SRFI-46 and R6RS both allow a fixed number of
tail patterns following an ellipsis in a syntax-rules
pattern:

  (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn)

R6RS further allows dotted tail patterns

  (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px)

where Px only matches a dotted list.

Do we allow either or both of these extensions?

  * '''Options:''' tail/core, tail/module, dotted-tail/core, dotted-tail/module, both/core, both/module, no, wg2, undecided
  * '''Preferences:''' wg2, tail/module, both/module, no

== WG1 - Numerics ==

=== #22 mantissa widths ===

R6RS introduced the concept of mantissa widths
as an alternative to the R5RS #s in numbers.
Do we want either or both of these?

  * '''Options:''' r5rs, r6rs, both, no, wg2, undecided
  * '''Preferences:''' no, wg2, r5rs

== WG1 - Reader Syntax ==

=== #11 case-sensitivity ===

Does the reader fold case by default, and if so how?

Yes to fold-case (R5RS) no to preserve case (R6RS), additional votes
to come later from specific proposals.

  * '''Options:''' yes, no, implementation-determined, undecided
  * '''Preferences:''' yes

Oh, please yes, let's not adopt all of C's mistakes.  As I said before,
I can't imagine why people think bug-avoidance important enough to put
up with hygienic macros, and yet want such a bug-attractor as making
"foo" and "Foo" mean two different things.

People have raised Unicode as an argument here, but there is a perfectly
good Unicode case-folding standard; they invented it precisely so that
programming languages can be international in scope without having to
endorse the horror of making semantically identical glyphs turn
semantically different.


=== #14 alternate comment syntax ===

R6RS provides support for #; nested sexp comments,
and #| ... |# nested block comments.  Do we include
either or both of these?

  * '''Options:''' sexp, block, both, no, wg2, undecided
  * '''Preferences:''' no, wg2

... but I don't feel strongly about it, and I wish the voting mechanism
gave us a way to say /how much/ we care about things.  I'd spend all my
votes on case folding and no-r6rs-modules, of the things so far on this
ballot, if I could.

== WG1 - Strings and Chars ==

=== #26 string normalization ===

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

Note UnicodeCowan currently provides specific normalization
procedures.

  * '''Options:''' generic/core, generic/module, separate/core, separate/module, specific/core, specific/module, agnostic/core, agnostic/module, no, wg2, undecided
  * '''Preferences:''' agnostic/core, agnostic/module, wg2, no

