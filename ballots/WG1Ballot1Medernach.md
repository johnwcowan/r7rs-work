# Instructions

* You may list as many of the options as you want in **order of preference**.
* Options are comma-delimited (ignoring space) and case-insensitive.
* You can pipe-delimit (|) options you want to give equal weight to.
* You may write in your own option if you announce it to the list first.
* You may specify a variant with `option/variant`, for example `srfi-1/module` to vote for `srfi-1` but clarify it should be in a separate module.  Please also include the `srfi-1` option in this case.
* You can write a free-form rationale after the "preferences" line,
* `module` means "yes, but I want it in a separate module",
* `wg2` means "no, but I think it should go in WG2".
* `undecided` means I want to discuss this issue further.
* Abstain on any item by leaving the preferences blank.

# WG1 New Ballot Items

# WG1 Ballot Items To Finalize By Oct. 12

## Working Group 1

### #1 Which VCS do we use?

We need a VCS to keep track of changes to the standard as we start
drafting it.

* **Options:** bzr, darcs, git, hg, monotone, svn, undecided
* **Preferences:** svn, hg, git

## WG1 - Modules

### #2 Module System

As per the charter, we need a module system
proposal which allows sharing of code between
implementations.

This is one issue where we can't default to
the R5RS, since it has no module system. If
we can't come to consensus, we will have to
take the R6RS module system as-is.

Note the `r6rs--` option is just the
R6RS module system without versioning or
phasing.

* **Proposals:**
* **ganz:** [ModulesGanz](ModulesGanz.md)
* **hsu:** [ModulesAndPackagesArcfide](ModulesAndPackagesArcfide.md)
* **shinn:** [ModulesShinn](ModulesShinn.md)
* **Options:** ganz, hsu, shinn, r6rs, r6rs--, undecided
* **Default:** r6rs
* **Preferences:** shinn, ganz, undecided, r6rs--, hsu, r6rs

This is only a preference indication, more work need to be done in this area before deciding a final module system.

## WG1 - Core

### #40 SRFI vs. R6RS precedence

Given equal technical merit and compatible extensibility for WG2,
should WG1 prefer SRFIs or standardized behaviors from R6RS when faced
with the choice. For example, a version of syntax-violation
vs. syntax-error.

This is a meta-item, to be used only as a guideline.

* **Options:** srfi,r6rs,undecided
* **Preferences:** undecided

It is difficult to decide of a general rule. What matters mainly is what people really use and find most useful.


### #37 transcript-on and transcript-off

These were relegated to a compatibility library
in R6RS.  Do we want to keep them, drop them, or
move them to a library?

Yes means to keep them in the core, as in R5RS,
and no means to remove them entirely.

* **Options:** yes, no, module, wg2, undecided
* **Default:** yes
* **Preferences:** module, no

### #38 letrec*

R6RS added letrec* and defined the semantics
of internal define to be equivalent.  Do we
want to add this?

Choose `letrec*` just to add the syntax, `define` to change the
behavior of internal define, or `yes`/`both` for both.

* **Options:** both, letrec*, define, no, module, wg2, undecided
* **Default:** no
* **Preferences:** both, define, wg2

### #41 Should we adopt the SRFI-1 extension to MAP and FOR-EACH?

This extension allows the list arguments to be of unequal length, and
stops the procedure whenever any of them run out.  R5RS says the lists
*must* be of the same length, R6RS says they *should* be.

`Yes` to allow unequal length.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** no, module

Silently accepting lists of unequal length is error-prone, especially if argument lists are received in parameters and if we are not sure about their length, it means we need to check at every map / for-each call if lists are on the same length, really not a good idea to my opinion. If one wants a mapping function accepting unequal length lists it is easy to write its own version.

### #42 Should we adopt the SRFI-1 extension to ASSOC and MEMBER?

This extension accepts a third argument, the equality predicate to be
used.  Alternatively we could use the R6RS predicates ASSP and MEMP.

* **Options:** srfi-1, r6rs, no, wg2, undecided
* **Default:** no
* **Preferences:** yes, module

### #33 dynamic-wind

New to R5RS, do we reaffirm the sometimes debated dynamic-wind?

Removing this would require a strong rationale indicating that it's
fundamentally flawed.

* **Options:** yes, no, module, wg2, undecided
* **Default:** yes
* **Preferences:** module, yes, no

### #34 multiple values

New to R5RS, do we reaffirm multiple values, specifically the
procedures `call-with-values` and `values`?

Removing this would require a strong rationale indicating that it's
fundamentally flawed.

Note if these forms are removed or placed in a module, for consistency
none of the core library should return multiple values (as is the case
in R5RS).

`Yes` to keep them, `no` to remove them, and `module` to relegate them
to a module.

* **Options:** yes, no, module, wg2, undecided
* **Default:** yes
* **Preferences:** yes, module, wg2

### #54 optional arguments

Scheme's primitive mechanism of improper lambda-lists allows for
optional arguments, but only with extra machinery.  CL, DSSSL, and
some Schemes provide a special word such as `#!optional` in
lambda-lists, showing that the arguments which follow are optional and
may have default values.  SRFI-89 provides both optional and keyword
arguments via `lambda*` and `define*` and without introducing #!foo
special tokens.

Note the original ticket description mentions `case-lambda`, but this
is easily provided as a separate module, and will be a separate item.

* **Options:** dsssl, srfi-89, no, wg2, undecided
* **Default:** no
* **Preferences:** wg2, srfi-89, no

### #57 Simple randomness

Student programs often want a small amount of randomness, not
necessarily of very high quality.  Shall we provide a simple interface
to a random variables in WG1 Scheme?

* **Proposals:**
* **cowan:** [RandomCowan](RandomCowan.md)
* **Options:** cowan, srfi-27, no, wg2, undecided
* **Default:** no
* **Preferences:** cowan/module, srfi-27/module, wg2, undecided, no, cowan/core, srfi-27/core

This is really a module issue, let people choose among a set the one which fits the best their needs. Standardize names only for helping code reuse. By the way if one need repeatability why not roll their own random stream from a saved persistent table ? (as good old random number tables for those who knew about it :)

### #59 current-error-port

Pretty much all Schemes except embedded ones provide a notion of
current error distinct from current output.  Should this be exposed as
a Scheme output port?

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** yes,module,wg2

### #60 Simple file operations

Should WG1 provide a module equivalent to the (rnrs files) module?
This provides `delete-file` and `file-exists?`, which are pretty much
necessities for any file-driven programming.

Note [PortsCowan](PortsCowan.md) automatically includes these - voting for them here
guarantees them even if not included by a specific proposal.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** module,yes,wg2

### #64 Consistency in sequence procedures

Should we add the 10 procedures mentioned at [CompleteSequenceCowan](CompleteSequenceCowan.md) in
order to make the Scheme sequence types consistent?  They are
`make-list copy-list list-set! string-map string-for-each
string->vector copy-vector vector-map vector-for-each vector->string`,
all with the obvious interface and semantics.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** yes,module,wg2

### #65 Precision indicators

R5RS requires that Scheme support five indicators for the precision of
floating-point values, not only the default `e` but also `s`, `f`,
`d`, and `l`.  Only a few Schemes actually support more than one
precision, so this is mostly noise.  Shall we make it an optional
feature?

* **Options:** required, optional, no, wg2, undecided
* **Default:** required
* **Preferences:** optional,wg2

### #66 Add EXACT-INTEGER?

Should we add an EXACT-INTEGER? predicate? Currently, to determine
whether a number is both an integer and exact, we must test for both,
which requires some hackery or poor pattern matching to optimize in
existing Scheme implementations.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** undecided

### #44 Testing function arity

We would like a standard for checking function arity.
SRFI-102 proposes a way to check function arity:

* **Options:** srfi-102, no, wg2, undecided
* **Default:** no
* **Preferences:** srfi-102,wg2

### #51 support for cyclic structures in primitives

list?, length, equal? and other fundamental primitives may diverge
when given cyclic data.  In the former two cases, avoiding this is
simple and not inefficient, and the equivalents are already provided
in SRFI-1.  In the latter case a
[proposal](http://www.r6rs.org/r6rs-editors/2006-February/000969.html)
was made and rejected on the R6RS list.  In the former case, R6RS
seems to require `list?` return `#f` and `length` raise an error.

Do we want to specify the behavior when these primitives encounter
cyclic data?

Options are `equal?` to specify `equal?` must not terminate on cyclic
input, `r6rs` to specify R6RS behavior for `list?` and `length`,
`srfi-1` to specify the SRFI-1 semantics (where `length` returns `#f`)
and `equal?+r6rs` or `equal?+srfi-1` are options for both.

* **Options:** equal?, r6rs, srfi-1, equal?+r6rs, equal?+srfi-1, no, wg2, undecided
* **Default:** no
* **Preferences:** native, srfi-38/core, srfi-38/module, wg2, undecided, no

About shared structures I would prefer an alternative parenthesis syntax for declaring this kind of values something like this:

> (make-shared ((a 'foo) (b (1 (a b) c)) (c #(2 b))) (list a b c))

> equivalent to in SRFI-38 external representation:

> ('foo #1=(1 ('foo #1#) #2#) #2=#(2 #1#))

> The rationale behind it is to avoid wild mutations when building shared structures, and a more human readable notation.

> Maybe this has to be discussed in a module for graph-like or circular data ?


### #58 exact-integer-sqrt

Should WG1 include `exact-integer-sqrt` from R6RS?  It allows square
root operations in Schemes that don't provide inexact arithmetic, and
has different semantics from `sqrt`, as it rounds its argument down to
the nearest exact square.

> (exact-integer-sqrt k) => (values s r) ; k = s^2 + r

`r6rs`/`yes` for R6RS semantics, `list` to use a list instead of MV,
or `single` to only return `s`.

* **Options:** r6rs, list, single, no, wg2, undecided
* **Default:** no
* **Preferences:** undecided

### #61 finite? nan?

Shall we add these numeric predicates defined on the IEEE floating
point values from #20?

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** undecided

### #63 call/cc short name

Should we allow `call/cc` as an equivalent to
`call-with-current-continuation`?

* **Options:** yes, no, module, wg2, undecided
* **Default:** yes
* **Preferences:** yes

### #53 Implicit BEGIN to implicit LET-NIL

In general, in places where an implict BEGIN occurs, it is possible to
change this to an implicit LET-NIL and remain backwards
compatible. Should we do this?

This is a meta-item to be used as a guideline, and specific places
would need to be brought up for review.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** yes, wg2

## WG1 - Exceptions

### #18 Exception System

R6RS provided a detailed exception system with
support for raising and catching exceptions, using
a hierarchy of exception types.

Do we use this, or parts of it, or a new exception
system?  The `r6rs` option is just for the core
exception handling, not the conditions hierarchy.

* **Proposals:**
* **cowan:** [ExceptionHandlingCowan](ExceptionHandlingCowan.md)
* **Options:** cowan, r6rs, wg2, none, undecided
* **Default:** none
* **Preferences:** none, wg2, undecided, cowan/module, r6rs/module, cowan/core, r6rs/core

We already have call-with-current-continuation, don't we ? I feel exceptions as inappropriate for WG1. Especially because of the complex / ad hoc exception taxonomy, mainly because reunifying existing taxonomies leads to over-specifications.
However I strive myself for a good interruption system, this is not really continuation: when a process is interrupted it is not waiting for a value, it could be resumed safely only once and it could be interrupted at (quite) any moment. I would like to be able to interrupt a computation because of a timer or because I ran many prediction branch in parallel and want to stop false ones, or just because something from the "outside world" just happens (user requesting a break, hosting machine is shutting down, memory is running out, etc. who knows ?)  IMHO trying to enumerate all exceptional situations (ie having a taxonomy) and standardizing them is not a good design for WG1. This simply has to do with cancelling a transaction or making continuations checkpoints and restart from there requested by something external. Everyone always wants critical programs to fall back consistently. Maybe this has something to do with parallelism, but not only (On a single process I could setup a clock, if I finish before it this is nice else I give the work done so far, latter if needed I could resume the work where I was or from latest checkpoint, etc.)
Some really simple and generic interface like: "After an asynchronous call to interrupt (named ?) process the "interruptor" have a checkpoint (continuation-like ie expecting a value) and an interrupted process which could be discarded or resumed." is more flexible, schemish and appealing to me.

### #17 error

Do we support the near ubiquitous SRFI-23 error procedure,
and if so should it use the SRFI-23 signature, R6RS, or
type-dispatch on the first argument to allow both?

Note [ExceptionHandlingCowan](ExceptionHandlingCowan.md) currently includes a SRFI-23 compatible
`error` procedure.

* **Options:** srfi-23, r6rs, both, no, module, wg2, undecided
* **Default:** no
* **Preferences:** srfi-23,wg2

## WG1 - I/O

### #30 string ports

Do we support string ports, as implemented by SRFI-6
or as by R6RS?

Note that currently [PortsCowan](PortsCowan.md) provides SRFI-6 string ports.

* **Options:** srfi-6, r6rs, no, module, wg2, undecided
* **Default:** no
* **Preferences:** srfi-6,module,wg2

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

* **Options:** srfi-38, native, no, wg2, undecided
* **Default:** no
* **Preferences:** native,srfi-38,module,wg2

## WG1 - Macros

### #7 (... ...) ellipse escaping in syntax patterns

A popular extension, formalized in the R6RS,
is to allow "(... <templ>)" in a syntax-rules template
to be an escape for "<templ>".  Do we use this, and
if so what does (... <t1> <t2>) mean?

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** yes/core, yes/module, wg2, undecided, no

### #39 syntax-error

Should we have syntax-error parallel to SRFI-23 error?  This is evoked
when macros are expanded.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** yes,wg2

### #5 syntax-rules

Do we keep syntax-rules in the core, relegate
it to a standard module, or leave it out entirely
(possibly letting WG2 specify it).

`Yes` to keep in core, `no` to remove from Scheme entirely.

* **Options:** yes, no, module, wg2, undecided
* **Default:** yes
* **Preferences:** yes,module,wg2

### #10 identifier syntax

R6RS introduced identifier syntax as a way to
expand identifiers in non-macro positions.

Orthogonal to the overall macro system and what
types of expanders are provided, do we provide
a means to specify identifier syntax?

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** no

### #47 internal define-syntax

R6RS extends define-syntax to be allowed
in local lexical contexts.  Do we allow
this as well?

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** yes,wg2

### #6 syntax-rules _ patterns

R6RS adds _ as a wild-card pattern, breaking
some existing R5RS macros.  Do we add the _ wildcard,
or leave it as a normal identifier as in R5RS?

Yes to add, no for R5RS.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** yes

### #8 SRFI-46 ellipse specifier in syntax-rules

As an alternative to #7, SRFI-46 proposed
allowing an optional ellipse specified as
an identifier before the literals list in
syntax-rules:

> (syntax-rules ::: ()
> <ellipse now represented as ::: instead of ...>)

Do we allow this?

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** yes/core, yes/module, wg2, undecided, no

As a convenience, for me this is better than (... ...) which makes macros generating macros quite unreadable (to my opinion).

### #9 tail patterns in syntax-rules

SRFI-46 and R6RS both allow a fixed number of
tail patterns following an ellipsis in a syntax-rules
pattern:

> (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn)

R6RS further allows dotted tail patterns

> (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px)

where Px only matches a dotted list.

Do we allow either or both of these extensions?

* **Options:** tail, dotted-tail, both, no, wg2, undecided
* **Default:** no
* **Preferences:** tail/core, tail/module, wg2, no, both/core, both/module, dotted-tail/core, dotted-tail/module, undecided

I don't grasp the meaning of dotted tail patterns, if we have an ellipsis how to know where to stop ? I mean this looks like not consistent with the dotted lambda notation ?

## WG1 - Numerics

### #20 inexact infinities

R6RS provides support for inexact infinities
and NaN objects.  Do we keep these, and if so
do we use the same literal syntax and arithmetic
as in R6RS?

`Yes` to keep them with the same syntax and semantics of R6RS, or
write in a separate proposal for some other syntax/semantics.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** no,wg2

### #21 limited type arithmetic

R6RS provides libraries for limited type arithmetic on fixnums only
and flonums only (i.e. `fx+`, `fl*` etc.).  Do we want these?

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** module,wg2

### #22 mantissa widths

R6RS introduced the concept of mantissa widths
as an alternative to the R5RS #s in numbers.
Do we want either or both of these?

* **Options:** r5rs, r6rs, both, no, wg2, undecided
* **Default:** no
* **Preferences:** r6rs, wg2, r5rs, no, undecided, both

## WG1 - Reader Syntax

### #11 case-sensitivity

Does the reader fold case by default, and if so how?

Yes to fold-case (R5RS) no to preserve case (R6RS), additional votes
to come later from specific proposals.

* **Options:** yes, no, unspecified, undecided
* **Default:** yes
* **Preferences:** yes, implementation-determined, undecided, no

Really I tell you preserving case is error-prone !

### #15 #\foo character names

R6RS greatly extends the list of character names,
as well as allowing #\xNN numeric escapes for characters.
Do we allow any or all of these names?

`mnemonic` for `#\tab` and friends, `numeric` for `#\xNN` as in R6RS,
and `yes`/`both` for both.

The exact list of added names is to be decided later.

* **Options:** mnemonic, numeric, both, no, wg2, undecided
* **Default:** no
* **Preferences:** mnemonic,wg2

### #13 [brackets] as (parens)

R6RS allows [] brackets as identical to parenthesis,
with the condition that they must balance.  Do we
accept this extension, propose some other use for
brackets, or leave them unspecified?

`Yes` for R6RS, `no` for R5RS, or write in a proposal for some other
meaning for brackets.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Preferences:** no

### #14 alternate comment syntax

R6RS provides support for #; nested sexp comments,
and #| ... |# nested block comments.  Do we include
either or both of these?

* **Options:** sexp, block, both, no, wg2, undecided
* **Default:** no
* **Preferences:** both, sexp, block, wg2, no, undecided

### #16 symbol escapes

R6RS provides character escapes in symbols of the form `\xnnnn;`,
where nnnn is 1-5 hex digits.  Do we accept this extension?  Do we
also allow |...| to escape a whole symbol or a part of one?

In all existing standards pipes are reserved and the |...| syntax is
unspecified.  In most implementations it's recognized, but there are
at least a few implementations where pipes are normal character
constituents.

* **Options:** numeric, quoted, both, no, wg2, undecided
* **Default:** no
* **Preferences:** no,wg2

### #67 string escapes

R6RS provides character escapes in strings of the form \xnnnn;, where
nnnn is 1-5 hex digits, as well as \n, \t etc. C-like escapes for
common control characters. Do we accept either or both of these
extensions?

* **Options:** numeric, mnemonic, both, no, wg2, undecided
* **Default:** no
* **Preferences:** mnemonic,wg2

## WG1 - Strings and Chars

### #24 char and string folding

R6RS provided operations to alter the case of strings and characters
(upcase, downcase, titlecase and foldcase) using locale-independent
Unicode mappings.  Do we provide equivalent mappings?

Note in a Unicode implementation individual character casings are
incomplete, and string case is not defined as a simple mapping of case
over the constituent characters.

Note [UnicodeCowan](UnicodeCowan.md) currently provides mappings at both levels.

* **Options:** strings, chars, both, no, module, wg2, undecided
* **Default:** no
* **Preferences:** module,both,wg2,no

### #26 string normalization

R6RS provides procedures to explicitly convert
strings back and forth between the four Unicode
normalization forms.

The previous phrasing of this option was overly vague, referring to
"any form of normalization."  I've had to treat `yes` votes as
undecided for lack of a better default.  If you voted `yes` before
please choose one of the following options or write in your own
proposal.

* agnostic - `string-ni=?' etc. provides an API of basic normalization insensitive procedures without explicitly converting the strings, analagous to `string-ci=?'
* generic - `string-normalize` converts to a single implementation-defined normal form
* separate - `string-compose-canonical`, `string-decompose-canonical` and `string-decompose-compatibility` gives orthogonal control over the normalization being performed
* specific - `string-normalize-{nfd,nfc,nfkd,nfkc}` converts explicitly to the four normal forms defined in the Unicode standard

Note [UnicodeCowan](UnicodeCowan.md) currently provides specific normalization
procedures.

* **Options:** generic, separate, specific, agnostic, no, wg2, undecided
* **Default:** no
* **Preferences:** agnostic/module, separate/module, specific/module, generic/module, wg2, agnostic/core, separate/core, specific/core, generic/core, undecided, no

### #27 string-ref/set! access time

R6RS suggests string-ref and string-set! work
in O(1) time, implying strings are implemented
as character arrays.  Do we reaffirm this?

`Yes` for required constant time.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Preferences:** no

### #23 character set

R5RS said almost nothing about character sets.
R6RS specified full Unicode.  Do we specify a
character set, or limit the options in any way?

* **Proposals:**
* **cowan:** [UnicodeCowan](UnicodeCowan.md)
* **Options:** cowan, r5rs, wg2, undecided
* **Default:** r5rs
* **Preferences:** wg2,cowan,r5rs

----

# WG1 Controversial Ballot Items

## WG1 - Core

### #50 Byte-Vectors

Several SRFIs, R6RS, and most Scheme implementations
support some sort of uniform packed integer vectors.
In particular, these are necessary for efficient
binary I/O, and for memory mapping, so WG2 will
certainly want them.

Do we provide a syntax and basic API for these in WG1?

* **Proposals:**
* **cowan:** BlobAPI
* **snellpym:** [BlobsAndSRFI](BlobsAndSRFI.md)4SnellPym
* **Options:** cowan, snellpym, wg2, none, undecided
* **Default:** none
* **Preferences:** cowan

### #69 Parameters

Most Scheme implementations provide some form of dynamic bindings such
as those provided by SRFI-39 parameters.

* **Proposals:**
* **cowan:** [ImmutableParametersCowan](ImmutableParametersCowan.md)
* **snellpym:** [ParametersSnellPym](ParametersSnellPym.md)
* **Options:** cowan, snellpym, srfi-39, wg2, none, undecided
* **Default:** none
* **Preferences:** cowan

I completely agree that some other mechanism is better in order to share data between threads and separating concern is needed here.

### #32 user-defined types

Do we support any means of creating disjoint
user-defined types, such as in SRFI-9, SRFI-99
or the R6RS record system?

* **Proposals:**
* **hsu:** [RecordsArcfide](RecordsArcfide.md)
* **rush:** [UserAggregatesRush](UserAggregatesRush.md)
* **snellpym:** [UniqueTypesSnellPym](UniqueTypesSnellPym.md)
* **Options:** hsu, rush, snellpym, srfi-9, srfi-99, no, wg2, undecided
* **Default:** no
* **Preferences:** rush,snellpym,srfi-9

## WG1 - Libraries

### #36 hash-tables

R6RS and SRFI-69 both provide hash-table interfaces.
Do we provide either of these, or try to provide
some primitives on which efficient hash-tables can
be implemented?

* **Options:** srfi-69, r6rs, no, wg2, undecided
* **Default:** no
* **Preferences:** module,srfi-69,wg2
