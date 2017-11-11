# WG1 New Ballot Items

Notes about voting:

* you may list as many of the options as you want in order of preference
* options are comma-delimited (ignoring space) and case-insensitive
* you may write in your own option if you announce it to the list first
* everything but the `preferences` line is free-form, and may be used for a rationale
* `module` means "yes, but I want it in a separate module"
* `wg2` means "no, but I think it should go in WG2"
* `undecided` means I want to discuss this issue further
* abstain by leaving the preferences blank
* items up for final vote will be marked as such (none are final now)

## WG1 - Core

### 37 transcript-on and transcript-off

These were relegated to a compatibility library
in R6RS.  Do we want to keep them, drop them, or
move them to a library?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 38 letrec*

R6RS added letrec* and defined the semantics
of internal define to be equivalent.  Do we
want to add this?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

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

* **Options:** srfi-1,r6rs,no,wg2,undecided
* **Preferences:** srfi-1

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
* **snellpym:** [UniqueTypesSnellPym](UniqueTypesSnellPym.md)
* **hsu:** [RecordsArcfide](RecordsArcfide.md)
* **Options:** snellpym,hsu,srfi-9,no,wg2,undecided
* **Preferences:** hsu

### 33 dynamic-wind

New to R5RS, do we reaffirm the sometimes debated dynamic-wind?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** undecided

### 34 multiple values

New to R5RS, do we reaffirm multiple values, specifically the
procedures `call-with-values` and `values`?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** undecided

### 51 support for cyclic structures in primitives

list?, length, equal? and other fundamental primitives may diverge
when given cyclic data.  In the former two cases, avoiding this is
simple and not inefficient, and the equivalents are already provided
in SRFI-1.  In the latter case a proposal was made and rejected on the
R6RS list.

Do we want to specify the behavior when these primitives encounter
cyclic data?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** undecided

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
* **Preferences:** undecided

### 57 Simple randomness

Student programs often want a small amount of randomness, not
necessarily of very high quality.  Shall we provide a simple interface
to a random variables in WG1 Scheme?

* **Options:** srfi-27,no,wg2,undecided
* **Preferences:** wg2

### 58 exact-integer-sqrt

Should WG1 include `exact-integer-sqrt` from R6RS?  It allows square
root operations in Schemes that don't provide inexact arithmetic, and
has different semantics from `sqrt`, as it rounds its argument down to
the nearest exact square.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

### 59 current-error-port

Pretty much all Schemes except embedded ones provide a notion of
current error distinct from current output.  Should this be exposed as
a Scheme output port?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 60 Simple file operations

Should WG1 provide a module equivalent to the (rnrs files) module?
This provides `delete-file` and `file-exists?`, which are pretty much
necessities for any file-driven programming.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

### 61 finite? nan?

Shall we add these numeric predicates?

* **Options:** finite,nan,both,no,module,wg2,undecided
* **Preferences:** both

### 63 call/cc short name

Should we allow `call/cc` as an equivalent to
`call-with-current-continuation`?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 64 Consistency in sequence procedures

Should we add the 10 procedures mentioned at [CompleteSequenceCowan](CompleteSequenceCowan.md) in
order to make the Scheme sequence types consistent?  They are
`make-list copy-list list-set! string-map string-for-each
string->vector copy-vector vector-map vector-for-each vector->string`,
all with the obvious interface and semantics.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

### 65 Precision indicators

R5RS requires that Scheme support five indicators for the precision of
floating-point values, not only the default `e` but also `s`, `f`,
`d`, and `l`.  Only a few Schemes actually support more than one
precision, so this is mostly noise.  Shall we make it an optional
feature?

* **Options:** required,optional,no,wg2,undecided
* **Preferences:** required

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
* **Preferences:** srfi-102

### 53 Implicit BEGIN to implicit LET-NIL

In general, in places where an implict BEGIN occurs, it is possible to
change this to an implicit LET-NIL and remain backwards
compatible. Should we do this?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

## WG1 - Exceptions

### 17 error

Do we support the near ubiquitous SRFI-23 error procedure?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** r6rs

## WG1 - I/O

### 30 string ports

Do we support SRFI-6 string ports, reaffirmed by R6RS?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** undecided

### 52 read/write cyclic data

SRFI-38 standardizes the #0=(1 . #0#) shared
structure notation for read/write.  In the case
of write, this can be expensive to compute, but
otherwise the common case of the repl printing
a cyclic structure results in an infinite loop.

Do we want to add support for this, as an option
or separate set of procedures?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** undecided

## WG1 - Libraries

### 36 hash-tables

R6RS and SRFI-69 both provide hash-table interfaces.
Do we provide either of these, or try to provide
some primitives on which efficient hash-tables can
be implemented?

* **Options:** srfi-69,r6rs,no,module,wg2,undecided
* **Preferences:** undecided

## WG1 - Macros

### 6 syntax-rules _ and ... patterns

R6RS adds _ as a wild-card pattern, breaking
some existing R5RS macros.  Do we keep the _?

* **Options:** yes,no,wg2,undecided
* **Preferences:** yes

### 7 (... ...) ellipse escaping in syntax patterns

A popular extension, formalized in the R6RS,
is to allow "(... <templ>)" in a syntax-rules template
to be an escape for "<templ>".  Do we use this, and
if so what does (... <t1> <t2>) mean?

* **Options:** yes,no,wg2,undecided
* **Preferences:** yes

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

* **Options:** tail,dotted-tail,both,no,wg2,undecided
* **Preferences:** both

### 39 syntax-error

Should we have syntax-error parallel to SRFI-23 error?  This is evoked
when macros are expanded.

There is a definition in JRM's Syntax-Rules Primer using syntax-rules,
but it relies on the syntax-rules implementation reporting an
unmatchable pattern with a complaint that includes the pattern.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 5 syntax-rules

Do we keep syntax-rules in the core, relegate
it to a standard module, or leave it out entirely
(possibly letting WG2 specify it).

`Yes` to keep in core, `no` to remove from Scheme entirely.

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

### 10 identifier syntax

R6RS introduced identifier syntax as a way to
expand identifiers in non-macro positions.

Orthogonal to the overall macro system and what
types of expanders are provided, do we provide
a means to specify identifier syntax?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** undecided

### 47 internal define-syntax

R6RS extends define-syntax to be allowed
in local lexical contexts.  Do we allow
this as well?

* **Options:** yes,no,wg2,undecided
* **Preferences:** no

## WG1 - Numerics

### 20 inexact infinities

R6RS provides support for inexact infinities
and NaN objects.  Do we keep these, and if so
do we use the same literal syntax and arithmetic
as in R6RS?

* **Options:** yes,no,wg2,undecided
* **Preferences:** undecided

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
* **Preferences:** undecided

## WG1 - Reader Syntax

### 11 case-sensitivity

Does the reader fold case by default, and if so how?

Yes to fold-case (R5RS) no to preserve case (R6RS), additional votes
to come later from specific proposals.

* **Options:** yes,no,undecided
* **Preferences:** yes

### 15 #\foo character names

R6RS greatly extends the list of character names,
as well as allowing #\xNN numeric escapes for characters.
Do we allow any or all of these names?

* **Options:** numeric,mnemonic,both,no,wg2,undecided
* **Preferences:** both

### 13 [brackets] as (parens)

R6RS allows [] brackets as identical to parenthesis,
with the condition that they must balance.  Do we
accept this extension, propose some other use for
brackets, or leave them unspecified?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** yes

### 14 alternate comment syntax

R6RS provides support for #; nested sexp comments,
and #| ... |# nested block comments.  Do we include
either or both of these?

* **Options:** sexp,block,both,no,wg2,undecided
* **Preferences:** sexp

### 16 symbol escapes

[#This|ticket was originally about string escapes, but commentators have
been talking about symbol escapes instead.]]

R6RS provides character escapes in symbols of the form `\xnnnn;`,
where nnnn is 1-5 hex digits.  Do we accept this extension?  Do we
also allow |...| to escape a whole symbol or a part of one?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** numeric

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
* **Preferences:** both

### 26 string normalization

R6RS provides procedures to explicitly convert
strings back and forth between the four Unicode
normalization forms.  Do we provide any sort
of string normalization?

* **Options:** yes,no,module,wg2,undecided
* **Preferences:** module

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

## Working Group 1

### 1 Which VCS do we use?

There is the question of the right VCS to use. I prefer
Monotone. Currently we are having an email vote on the list. I have
entered this ticket to play with the Trac ticketing system. We can
finalize the ticket once we have chosen a VCS.

* **Options:** bzr,darcs,git,hg,monotone,svn,undecided
* **Preferences:** undecided, bzr | git | hg | monotone | svn

