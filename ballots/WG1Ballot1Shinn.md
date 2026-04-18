# WG1 New Ballot Items

Notes about results:

* you may list as many of the options as you want in order of preference
* options are comma-delimited (ignoring space) and case-insensitive
* you may write in your own option if you announce it to the list first
* everything but the `preferences` line is free-form, and may be used for a rationale
* `module` means "yes, but I want it in a separate module"
* `wg2` means "no, but I think it should go in WG2"
* `undecided` means I want to discuss this issue further
* abstain by leaving the preferences blank
* items up for final vote will be marked as such (none are final now)

## WG1 - Modules

### 2 Module System

As per the charter, we need a module system
proposal which allows sharing of code between
implementations.

This is one issue where we can't default to
the R5RS, since it has no module system. If
we can't come to consensus, we will have to
take the R6RS module system as-is.

* **Proposals:**
* **hsu:** [ModulesAndPackagesArcfide](ModulesAndPackagesArcfide.md)
* **shinn:** [ModulesShinn](ModulesShinn.md)
* **Options:** ganz, hsu, shinn, r6rs, r6rs--, undecided
* **Preferences:** shinn, r6rs--, r6rs, ganz, hsu

Anything beyond a static syntax is specifying too much, and
would make integration with existing systems difficult to impossible.

## WG1 - Core

### 50 Byte-Vectors

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
* **Preferences:** undecided

I think the proposals need more work.

### 69 Parameters

Most Scheme implementations provide some form of dynamic bindings such
as those provided by SRFI-39 parameters.

* **Proposals:**
* **cowan:** [ImmutableParametersCowan](ImmutableParametersCowan.md)
* **snellpym:** [ParametersSnellPym](ParametersSnellPym.md)
* **Options:** cowan, snellpym, srfi-39, wg2, none, undecided
* **Preferences:** undecided

I think we should have SRFI-9, with `parameterize` required
to be thread-safe and direct mutation unspecified in the context
of threads.

## WG1 - Exceptions

### 18 Exception System

R6RS provided a detailed exception system with
support for raising and catching exceptions, using
a hierarchy of exception types.

Do we use this, or parts of it, or a new exception
system?

* **Proposals:**
* **cowan:** [ExceptionHandlingCowan](ExceptionHandlingCowan.md)
* **Options:** cowan, wg2, none, undecided
* **Preferences:** r6rs, cowan, wg2

The core R6RS exception system is fine, it was the
extensive condition hierarchy and required exception
situations I had issues with.

----

# WG1 Controversial Ballot Items

## WG1 - Core

### 40 SRFI vs. R6RS precedence

Given equal technical merit and compatible extensibility for WG2,
should WG1 prefer SRFIs or standardized behaviors from R6RS when faced
with the choice. For example, a version of syntax-violation
vs. syntax-error.

* **Options:** srfi,r6rs,undecided
* **Preferences:** srfi

### 32 user-define types

Do we support any means of creating disjoint
user-defined types, such as in SRFI-9, SRFI-99
or the R6RS record system?

* **Proposals:**
* **hsu:** [RecordsArcfide](RecordsArcfide.md)
* **rush:** [UserAggregatesRush](UserAggregatesRush.md)
* **snellpym:** [UniqueTypesSnellPym](UniqueTypesSnellPym.md)
* **Options:** hsu,rush,snellpym,srfi-9,srfi-99,no,wg2,undecided
* **Preferences:** srfi-9, srfi-99

SRFI-9 doesn't extend well, as shown by SRFI-99's ugliness.
A fully keyword-based approach may be better.

### 51 support for cyclic structures in primitives

list?, length, equal? and other fundamental primitives may diverge
when given cyclic data.  In the former two cases, avoiding this is
simple and not inefficient, and the equivalents are already provided
in SRFI-1.  In the latter case a proposal was made and rejected on the
R6RS list.

Do we want to specify the behavior when these primitives encounter
cyclic data?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

Definitely or list? and length, equal? might need more consideration.

### 58 exact-integer-sqrt

Should WG1 include `exact-integer-sqrt` from R6RS?  It allows square
root operations in Schemes that don't provide inexact arithmetic, and
has different semantics from `sqrt`, as it rounds its argument down to
the nearest exact square.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, yes, wg2

### 61 finite? nan?

Shall we add these numeric predicates?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, yes, wg2

### 63 call/cc short name

Should we allow `call/cc` as an equivalent to
`call-with-current-continuation`?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** no

Emphatically no.  Aliases do **not** belong in a small standard.

### 53 Implicit BEGIN to implicit LET-NIL

In general, in places where an implict BEGIN occurs, it is possible to
change this to an implicit LET-NIL and remain backwards
compatible. Should we do this?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** no

I'd only consider this on a case-by-case scenario.

## WG1 - I/O

### 52 read/write cyclic data

SRFI-38 standardizes the #0=(1 . #0#) shared
structure notation for read/write.  In the case
of write, this can be expensive to compute, but
otherwise the common case of the repl printing
a cyclic structure results in an infinite loop.

Do we want to add support for this, as an option
or separate set of procedures?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

It's crucial to be able to have control over whether
or not shared structure is handled on read and write.

## WG1 - Libraries

### 36 hash-tables

R6RS and SRFI-69 both provide hash-table interfaces.
Do we provide either of these, or try to provide
some primitives on which efficient hash-tables can
be implemented?

* **Options:** srfi-69,r6rs,no,module,wg2,undecided
* **Preferences:** r6rs, wg2

SRFI-69 is broken.

## WG1 - Macros

### 6 syntax-rules _ patterns

R6RS adds _ as a wild-card pattern, breaking
some existing R5RS macros.  Do we add the _ wildcard,
or leave it as a normal identifier as in R5RS?

Yes to add, no for R5RS.

* **Options:** yes,no,wg2,undecided
* **Preferences:** no

### 8 SRFI-46 ellipse specifier in syntax-rules

As an alternative to #7, SRFI-46 proposed
allowing an optional ellipse specified as
an identifier before the literals list in
syntax-rules:

> (syntax-rules ::: ()
> <ellipse now represented as ::: instead of ...>)

Do we allow this?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** no

### 9 tail patterns in syntax-rules

SRFI-46 and R6RS both allow a fixed number of
tail patterns following an ellipsis in a syntax-rules
pattern:

> (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn)

R6RS further allows dotted tail patterns

> (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px)

where Px only matches a dotted list.

Do we allow either or both of these extensions?

* **Options:** tail,dotted-tail,both,no,module,wg2,undecided
* **Preferences:** tail

The dotted-tail is useful, but breaks the rule of the
pattern language where what you see is what you get.

## WG1 - Numerics

### 21 limited type arithmetic

R6RS provides libraries for limited type arithmetic
on fixnums only and flonums only.  Do we want these?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** wg2

### 22 mantissa widths

R6RS introduced the concept of mantissa widths
as an alternative to the R5RS #s in numbers.
Do we want either or both of these?

* **Options:** r5rs,r6rs,both,no,wg2,undecided
* **Preferences:** r5rs

## WG1 - Reader Syntax

### 11 case-sensitivity

Does the reader fold case by default, and if so how?

Yes to fold-case (R5RS) no to preserve case (R6RS), additional votes
to come later from specific proposals.

* **Options:** yes,no,undecided
* **Preferences:** no, yes, unspecified

There are many pros and cons, but all things being equal
I prefer case-sensitivity if for no other reason than it
is more expressive - it can be used to write case-folding
macros.

## Working Group 1

### 1 Which VCS do we use?

There is the question of the right VCS to use. I prefer
Monotone. Currently we are having an email vote on the list. I have
entered this ticket to play with the Trac ticketing system. We can
finalize the ticket once we have chosen a VCS.

* **Options:** bzr,darcs,git,hg,monotone,svn,undecided
* **Preferences:** hg,git,darcs

----

# WG1 Old Items

## WG1 - Core

### 37 transcript-on and transcript-off

These were relegated to a compatibility library
in R6RS.  Do we want to keep them, drop them, or
move them to a library?

Yes means to keep them in the core, as in R5RS,
and no means to remove them entirely.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, no

### 38 letrec*

R6RS added letrec* and defined the semantics
of internal define to be equivalent.  Do we
want to add this?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 41 Should we adopt the SRFI-1 extension to MAP and FOR-EACH?

This extension allows the list arguments to be of unequal length, and
stops the procedure whenever any of them run out.  R5RS says the lists
*must* be of the same length, R6RS says they *should* be.

`Yes` to allow unequal length.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 42 Should we adopt the SRFI-1 extension to ASSOC and MEMBER?

This extension accepts a third argument, the equality predicate to be
used.  Alternatively we could use the R6RS predicates ASSP and MEMP.

* **Options:** srfi-1,r6rs,no,module,wg2,undecided
* **Preferences:** srfi-1,r6rs

### 33 dynamic-wind

New to R5RS, do we reaffirm the sometimes debated dynamic-wind?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes, module

### 34 multiple values

New to R5RS, do we reaffirm multiple values, specifically the
procedures `call-with-values` and `values`?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, yes

I dislike MV for the complexity it introduces into any
combinators or higher order utilities, but would rather
preserve backwards compatibility by relegating it to a module.

### 54 optional arguments

Scheme's primitive mechanism of improper lambda-lists allows for
optional arguments, but only with extra machinery.  CL, DSSSL, and
some Schemes provide a special word such as `#!optional` in
lambda-lists, showing that the arguments which follow are optional and
may have default values.  SRFI-89 provides both optional and keyword
arguments via `lambda*` and `define*` and without introducing #!foo
special tokens.

Note the original ticket description mentions `case-lambda`, but this
is easily provided as a separate module, and will be a separate item.

* **Options:** dsssl,srfi-89,no,wg2,undecided
* **Preferences:** wg2, no

Too much to work out, too many differing opinions, let's leave this to wg2.

### 57 Simple randomness

Student programs often want a small amount of randomness, not
necessarily of very high quality.  Shall we provide a simple interface
to a random variables in WG1 Scheme?

* **Options:** srfi-27,no,wg2,undecided
* **Preferences:** wg2, no

This is actually pretty crucial for simple games, but
I'd still rather leave it up to wg2.  Providing just
`current-seconds` is enough to allow users to implement
a much better random library than any of the simplified
interfaces suggested.

### 59 current-error-port

Pretty much all Schemes except embedded ones provide a notion of
current error distinct from current output.  Should this be exposed as
a Scheme output port?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes, module

### 60 Simple file operations

Should WG1 provide a module equivalent to the (rnrs files) module?
This provides `delete-file` and `file-exists?`, which are pretty much
necessities for any file-driven programming.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, wg2

### 64 Consistency in sequence procedures

Should we add the 10 procedures mentioned at [CompleteSequenceCowan](CompleteSequenceCowan.md) in
order to make the Scheme sequence types consistent?  They are
`make-list copy-list list-set! string-map string-for-each
string->vector copy-vector vector-map vector-for-each vector->string`,
all with the obvious interface and semantics.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, wg2

These should be in a module.  Arguably, the list equivalents like
map and for-each should be in modules too.  Their extremely useful
but a loop syntax is generally faster and easier to read.

### 65 Precision indicators

R5RS requires that Scheme support five indicators for the precision of
floating-point values, not only the default `e` but also `s`, `f`,
`d`, and `l`.  Only a few Schemes actually support more than one
precision, so this is mostly noise.  Shall we make it an optional
feature?

* **Options:** required,optional,no,wg2,undecided
* **Preferences:** optional

### 66 Add EXACT-INTEGER?

Should we add an EXACT-INTEGER? predicate? Currently, to determine
whether a number is both an integer and exact, we must test for both,
which requires some hackery or poor pattern matching to optimize in
existing Scheme implementations.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 44 Testing function arity

We would like a standard for checking function arity.
SRFI-102 proposes a way to check function arity:

* **Options:** srfi-102,no,wg2,undecided
* **Preferences:** no

## WG1 - Exceptions

### 17 error

Do we support the near ubiquitous SRFI-23 error procedure,
and if so should it use the SRFI-23 signature, R6RS, or
type-dispatch on the first argument to allow both?

* **Options:** srfi-23,r6rs,both,no,module,wg2,undecided
* **Preferences:** srfi-23

Definitely not the R6RS version, it breaks too much code.

## WG1 - I/O

### 30 string ports

Do we support string ports, as implemented by SRFI-6
or as by R6RS?

* **Options:** srfi-6,r6rs,no,module,wg2,undecided
* **Preferences:** srfi-6, r6rs

## WG1 - Macros

### 7 (... ...) ellipse escaping in syntax patterns

A popular extension, formalized in the R6RS,
is to allow "(... <templ>)" in a syntax-rules template
to be an escape for "<templ>".  Do we use this, and
if so what does (... <t1> <t2>) mean?

* **Options:** yes,no,wg2,undecided
* **Preferences:** yes, wg2

### 39 syntax-error

Should we have syntax-error parallel to SRFI-23 error?  This is evoked
when macros are expanded.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes, module

### 5 syntax-rules

Do we keep syntax-rules in the core, relegate
it to a standard module, or leave it out entirely
(possibly letting WG2 specify it).

`Yes` to keep in core, `no` to remove from Scheme entirely.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module, yes

### 10 identifier syntax

R6RS introduced identifier syntax as a way to
expand identifiers in non-macro positions.

Orthogonal to the overall macro system and what
types of expanders are provided, do we provide
a means to specify identifier syntax?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** no

Identifier syntax is actually really cool.
I bash it only because it's a big semantic change,
and not suitable for a small R5RS-compatible language.
I would not be overly opposed to it in wg2.

### 47 internal define-syntax

R6RS extends define-syntax to be allowed
in local lexical contexts.  Do we allow
this as well?

* **Options:** yes,no,wg2,undecided
* **Preferences:** yes

## WG1 - Numerics

### 20 inexact infinities

R6RS provides support for inexact infinities
and NaN objects.  Do we keep these, and if so
do we use the same literal syntax and arithmetic
as in R6RS?

* **Options:** yes,no,wg2,undecided
* **Preferences:** yes, wg2

## WG1 - Reader Syntax

### 15 #\foo character names

R6RS greatly extends the list of character names,
as well as allowing #\xNN numeric escapes for characters.
Do we allow any or all of these names?

* **Options:** mnemonic,numeric,both,no,wg2,undecided
* **Preferences:** both

Conservative in the names - we don't need to specify all
control characters from 0..31.

### 13 [brackets] as (parens)

R6RS allows [] brackets as identical to parenthesis,
with the condition that they must balance.  Do we
accept this extension, propose some other use for
brackets, or leave them unspecified?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** no

Over my dead body! :)

### 14 alternate comment syntax

R6RS provides support for #; nested sexp comments,
and #| ... |# nested block comments.  Do we include
either or both of these?

* **Options:** sexp,block,both,no,wg2,undecided
* **Preferences:** sexp

I don't like or use the block comments, but I suspect
I'll get out-voted on this.

### 16 symbol escapes

[[This|ticket was originally about string escapes, but commenters have
been talking about symbol escapes instead.]]

R6RS provides character escapes in symbols of the form `\xnnnn;`,
where nnnn is 1-5 hex digits.  Do we accept this extension?  Do we
also allow |...| to escape a whole symbol or a part of one?

* **Options:** numeric,quoted,both,wg2,undecided
* **Preferences:** both

### 67 string escapes

R6RS provides character escapes in symbols of the form \xnnnn;, where
nnnn is 1-5 hex digits, as well as \n, \t etc. C-like escapes for
common control characters. Do we accept either or both of these
extensions?

* **Options:** numeric,mnemonic,both,no,wg2,undecided
* **Preferences:** both

## WG1 - Strings and Chars

### 24 char and string folding

R6RS provided operations to alter the case
of strings and characters (upcase, downcase, titlecase
and foldcase) using locale-independent Unicode
mappings.  Do we provide equivalent mappings?

* **Options:** strings,chars,both,no,module,wg2,undecided
* **Preferences:** strings

### 26 string normalization

R6RS provides procedures to explicitly convert
strings back and forth between the four Unicode
normalization forms.  Do we provide any sort
of string normalization?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** no, wg2

Totally inappropriate for wg1.

### 27 string-ref/set! access time

R6RS suggests string-ref and string-set! work
in O(1) time, implying strings are implemented
as character arrays.  Do we reaffirm this?

`Yes` for required constant time.

* **Options:** yes,no,wg2,undecided
* **Preferences:** no

### 23 character set

R5RS said almost nothing about character sets.
R6RS specified full Unicode.  Do we specify a
character set, or limit the options in any way?

* **Proposals:**
* **cowan:** [UnicodeCowan](UnicodeCowan.md)
* **Options:** cowan,r5rs,wg2,undecided
* **Preferences:** cowan

Modulo string-normalization, and perhaps some
details to be worked out later since it's a
large proposal.
