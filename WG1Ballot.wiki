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

= WG1 Ballot Items To Finalize by July 1st =

= Previous Undecided and Re-opened Ballot Items =

=== #32 user-defined types ===

Do we support any means of creating disjoint user-defined types, such
as in SRFI-9, SRFI-99 or the R6RS record system?

WG1 voted '''srfi-9''' before.  New arguments against filter
constructors were raised, so the ticket was re-opened.

References:
  * https://groups.google.com/d/topic/scheme-reports-wg1/BX2F10MO6_k/discussion

  * '''Proposals:'''
    * '''cowan:''' RecordsCowan
    * '''gleckler:''' RecordsGleckler, which is just RecordsCowan plus RecordsArcfide
    * '''hsu:''' RecordsArcfide
    * '''medernach:''' AggregatesMedernach
    * '''rush:''' UserAggregatesRush
    * '''snellpym:''' UniqueTypesSnellPym
  * '''Options:''' srfi-9, srfi-57, srfi-99, r6rs, cowan, hsu, medernach, rush, snellpym, none, wg2, undecided
  * '''Default:''' srfi-9
  * '''Preferences:''' 

=== #28 Binary I/O ports ===

Do we provide any binary input or output ports, and if so how do we
construct them and operate on them?  Can binary and textual operations
be mixed on the different port types?

BinaryPortsCowan provided binary port operations, being a mild
revision of the relevant parts of PortsCowan.  It has been removed
by Cowan in favor of PortsShinn.

PortsShinn provides binary port operations, with similar operations to
BinaryPortsCowan but keeping the binary/textual ports disjoint.

R6RS provides an entirely new I/O system, as well as a separate
R5RS-compatible I/O system.

The withdrawn SRFI-91 provides yet another I/O system supporting
binary ports.

Note this item as well as #29 and #31 specify semi-orthogonal aspects
of I/O systems which are typically specified together by individual
proposals.  If the same proposal doesn't win for all three, the
aspects will be merged as needed.

WG1 voted weakly in favor of PortsCowan before.

  * '''Proposals:''' 
    * '''r6rs:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2|R6RS Port I/O]]
    * '''r6rs-simple:''' [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.3|R6RS Simple I/O]]
    * '''srfi-91:''' [[http://srfi.schemers.org/srfi-91/srfi-91.html|SRFI-91]]
    * '''shinn:''' PortsShinn
  * '''Options:''' r6rs, r6rs-simple, srfi-91, cowan, shinn, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

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

WG1 voted '''unbound''' previously.  New issues were brought up on the
list so the ticket was re-opened.

References:
  * [wiki:Keywords]
  * http://lists.scheme-reports.org/pipermail/scheme-reports/2011-April/thread.html

  * '''Options:''' bound, unbound, unhygienic, undecided
  * '''Default:''' unbound
  * '''Preferences:''' 

=== #3 module naming convention ===

We need a naming convention for the core modules and standard
libraries of the new module system.

The existing break down is based on John Cowan's earlier proposal of
factorings in items #71, #72, #73, #74, #75, #76, #77, as well as an
I/O module breakdown in PortsCowan.  There have been various tickets
proposing changing this, so we are re-opening the ticket.

  * '''Proposals:'''
    * '''draft-1:''' the first draft
    * '''r5rs:''' one single module
    * '''r6rs:''' no proposal yet
    * '''cowan:''' ModuleFactoringCowan
    * '''gleckler:''' ModuleFactoringGleckler
    * '''shinn:''' ModuleFactoringShinn
    * '''medernach:''' ModuleFactoringMedernach
  * '''Options:''' draft-1, r5rs, r6rs, cowan, shinn, medernach, undecided
  * '''Default:''' draft-1
  * '''Preferences:''' 

= New Ballot Items =

== WG1 - Core ==

=== #85 Blobs, bytevectors, byte-vectors, octet-vectors, or something else? ===

Now that we have blobs, we have to decide what to call them.  R6RS
uses bytevector, SRFI-4 and SRFI-68 uses u8vector, while the original
WG1 proposal used blob (which is therefore the default).

  * '''Options:''' blob, bytevector, byte-vector, u8vector, octet-vector, undecided
  * '''Default:''' blob
  * '''Preferences:''' 

=== #118 Simple literals must be explicitly delimited. ===

In R5RS syntax such as `#t#f` is left unspecified - some readers may
parse this as the true literal followed by false.  R6RS requires
identifiers, characters, booleans, number objects, and `.` to be
terminated with a "delimiter" or by the end of input.

References:
  * http://scheme-punks.org/wiki/index.php?title=ERR5RS:Lexical_Syntax
  * http://lists.r6rs.org/pipermail/r6rs-discuss/2007-June/002649.html

  * '''Options:''' delimited, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #119 Whether to treat # as a delimiter. ===

In R5RS `foo#f` is a valid identifier, whereas R6RS requires `#` to
act as a delimiter, so that this would parse as the identifier `foo`
followed by the false literal.

  * '''Options:'''  delimiter, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #123 Extend unquote and unquote-splicing to multiple arguments ===

This is a change also made by R6RS (and CL).

References:
  * http://lists.scheme-reports.org/pipermail/scheme-reports/2011-April/000448.html
  * http://www.rhinocerus.net/forum/lang-scheme/98742-quasiquote-syntax-rules-macro.html
  * http://www.mail-archive.com/guile-user@gnu.org/msg03899.html

  * '''Options:''' multiple, single, undecided
  * '''Default:''' single
  * '''Preferences:''' 

=== #124 Nested quasiquote semantics ===

References:
  * http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.17
  * http://lists.nongnu.org/archive/html/chicken-hackers/2010-12/msg00008.html

  * '''Proposals:'''
    * '''r5rs:''' unspecified
    * '''r6rs:''' strict and multiple (implies multiple for #123)
    * '''chicken:''' strict at level 0 (option 2 in second reference)
    * '''strict:''' strict at all levels (R6RS with single for #123)
  * '''Options:''' r5rs, r6rs, chicken, strict, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #125 Allow procedures not to be locations (making EQV? unspecified in some additional cases) ===

This is a change also made by R6RS, specifically:

> A quasiquote expression may return either fresh, mutable objects or literal structure
> for any structure that is constructed at run time during the evaluation of the expression.
> Portions that do not need to be rebuilt are always literal

  * '''Options:''' r6rs, r5rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #126 Partly specify the mutability of the values of quasiquote structures ===

This is a change also made by R6RS.

  * '''Options:''' r6rs, r5rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #127 Specify the dynamic environment of the ''before'' and ''after'' procedures of dynamic-wind ===

R5RS is slightly ambiguous, saying

> BEFORE is called whenever execution enters the dynamic extent of the
> call to THUNK and AFTER is called whenever it exits that dynamic
> extent.

without saying clearly whether ''before'' and ''after'' themselves are
called before or after the dynamic extent is entered or exited.

  * '''Proposals:'''
    * '''outside:''' called outside the dynamic extent (R6RS)
    * '''inside:''' called inside the dynamic extent
    * '''unspecified:''' R5RS
  * '''Options:''' outside, inside, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #135 let-values and let*-values ===

These R6RS procedures were part of #77 (modularization of multiple
values), but were never explicitly voted up or down by WG1, so I'm
opening a new ticket for them.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #137 Current-seconds semantics still open ===

In issue #70, WG1 voted to make `current-seconds` an optional
procedure, but there is no guidance about what it returns.

If we choose to specify this further, the big question is whether or
not to include leap seconds - i.e. do we specify it as TAI or POSIX
time (the choice of the epoch itself is less controversial and
defaults to the POSIX epoch).  TAI time has the advantage that it
measures real, unambiguous time, and two calls to current-seconds more
than a second apart are guaranteed to actually differ.  POSIX time has
the advantage of bug-for-bug compatibility with POSIX systems - the
times are ambiguous, but they already have to deal with that.

The other issue is whether to return an integral number of seconds and
lose the ability to specify subsecond real times, or return an inexact
real (flonum) number of seconds and have to deal with variable
precision depending on the date.

TimeCowan is equivalent to the `posix-integer` option, and in addition
changes the name to `current-posix-second`.

  * '''Proposals:'''
    * '''cowan:''' TimeCowan
    * '''posix-integer:''' POSIX time as an exact integer value
    * '''posix-flonum:''' POSIX time as an inexact real value
    * '''tai-integer:''' TAI time as an exact integer value
    * '''tai-flonum:''' TAI time as an inexact real value
  * '''Options:''' cowan, unspecified, undecided, none
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #147 Allow literal file spec lists in include and include-ci ===

This could allow implementation-specific extensions to support files
don't have character-string names.  On the other hand, such names
probably shouldn't be used as source files, and there are other ways
to support this.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #148 Allow include-ci at top level ===

Currently `include-ci` is allowed as a module declaration but not at top level,
as `include` is.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #149 blob ports ===

We've voted to add string ports, which are character ports backed by
Scheme strings.  Since we have blobs another potential extension is
blob ports, which binary ports backed by blobs.  These are described
in PortsCowan, but it's unclear if they were specifically voted for or
against in the previous ballot.

  * '''Options:''' cowan, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #150 cond-expand at top level ===

Currently `cond-expand` is only valid as a module declaration.  Should
we allow it at top level in a program?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #153 Renaming blob procedures ===

The blob procedures don't follow the same system as the rest.  I
propose these changes:

{{{
copy-blob => blob-copy
copy-blob! => blob-copy!
partial-blob => blob-copy-partial
copy-partial-blob! -> blob-copy-partial!
}}}

Note this is modulo the choice of "blob" or "bytevector"
or whichever.

  * '''Options:''' new, original, remove, undecided
  * '''Default:''' original
  * '''Preferences:''' 

=== #154 Physical newline in a string equivalent to \n (that is, U+000A) ===

R5RS leaves this situation undefined, but R6RS, CL, and most languages
that allow it (C does not) treat physical newline and escaped newline
as equivalent, even if the local representation of line endings is
\r\n or U+0085 or what not.  Another possibility is to treat string literals
broken across lines as errors.

  * '''Options:''' unix, local, error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #155 Make recursively defined code an explicit error ===

Allowing examples like these will make code-walkers (including
compilers and interpreters) excessively complicated:

#1=(begin (display #\x) . #1#)

(lambda #2=(a b c #2#) ...)

(+ . #3=(1 2 3 . #3#))

  * '''Options:''' error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #156 Replace "an error is signalled" with "an implementation-dependent object is raised as if by `raise`" ===

The following situations are described in draft 1 (and R5RS) with "an
error is signalled":

 1. The ''file-spec'' given to `call-with-input-file`,
 `call-with-output-file`, `open-input-file`, or `open-output-file`
 represents a file that cannot be opened.

 2. An end of file is read from a port by `read` after the beginning
 of an object's external representation, but the external
 representation is incomplete and therefore not parsable.

I propose that in both cases the implementation be required to raise
an exception as if by applying `raise` (that is, non-continuably) to
an implementation-defined object, which means it can be caught by the
R7RS exception system.  Note that there is no requirement to create a
fresh object.

  * '''Options:''' signal, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #162 Remove DELAY and FORCE altogether ===

They are present in R4RS and R5RS, but not IEEE Scheme (which is our
baseline).  There are problems with a straightforward implementation
that SRFI 45 fixes, but we voted down SRFI 45.  Given that, we should
consider removing them from the standard altogether.  (Of course this
does not mean compliant implementations can't provide them, it just
means they won't be in a standard module.)

Since the inconsistency was raised and people are going so far as
to remove these, we can entertain votes for SRFI-45's `lazy` again.

  * '''Options:''' remove, keep, lazy, undecided
  * '''Default:''' keep
  * '''Preferences:''' 

=== #164 Meaning of char-numeric? ===

The current draft, like R6RS, defines `char-numeric?` according to the
nonexistent Unicode Numeric property.  That has to be fixed.  Options:

 1. '''Any.''' `char-numeric?` returns `#t` if the character's
 Numeric_Type property value is other than `None`.  This means that
 many hanzi are both alphabetic and numeric.

 2. (Omitted, because it does not preserve IEEE Scheme)

 3. '''ASCII.''' Define `char-numeric?` to return `#t` only for ASCII
 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9.  This retains compatibility witht
 R5RS, and we can still use `char-numeric?` to parse numbers, and
 safely use `(- (char->integer c) (char->integer #\0))` to obtain the
 digit value the character represents.  (Note: R5RS programs that use
 `char-numeric?` to parse numbers will break if we adopt the current
 draft's definition of `char-numeric?`).  Gauche, Gambit, and Chicken
 (without the utf8 egg) work like this.

 4. '''Digit.''' Define `char-numeric?` as equivalent to the
 Numeric_Digit property (general category value of Nd).  Guile 2.0,
 Kawa, Larceny, Ypsilon, Mosh, and IronScheme work like this.

 5. '''Number.''' Define `char-numeric?` as equivalent to the Number
 property (general category values of Nd, Nl, No).  Scheme48, Chez,
 and Ikarus work like this.

  * '''Options:''' any, number, digit, ascii, undecided
  * '''Default:''' ascii
  * '''Preferences:''' 

=== #166 Add predicate and accessors for error objects ===

(Email from Vincent Manis)

Problem: It's impossible to write a portable error handler that writes
out the ''message'' and ''irritants'' that were passed to `error`.

This comes about because `error` creates an "implementation-defined
object". I would assume that this hides the whole exception class
hierarchy a WG2 implementation might provide. Since the ''message''
and ''irritants'' arguments to `error` are presumably living in this
implementation-defined object, it should be simple enough to provide
accessors to extract them, so that the above "portable error handler"
can be written.

Suggestion: Add the following procedures:

`(error-object? `''object''`)`

Returns `#t` if ''object'' is something created by `error`, `#f`
otherwise. Any constraints on type disjointness are up to the
implementation.

`(error-object-message `''object''`)`

Returns the message of ''object''.

`(error-object-irritants `''object''`)`

Returns a list of the irritants of ''object''.

  * '''Options:''' manis, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #167 Add constructor for error objects ===

(Email from Vincent Manis)

Problem: Raising arbitrary objects as exceptions has been found to be
nasty in some other languages (Python and C++ in particular).

This one is a tad speculative, but I'm reluctant to encourage people
to write things like `(raise 4)`, because of course it doesn't respect
any module boundaries. I think the intent with the descriptions of
`raise` and `raise-continuable` was to allow exception hierarchies to
be added in WG2 without constraining them here. I would suggest adding
a new procedure:

`(make-error-object `''message''` `''obj'' ...`)`

to creates the implementation-defined object `error` is supposed to
create, and adding a sentence to the `raise` and `raise-continuable`
entries that says "The effect of applying this procedure to an object
not created via `make-error-object` is implementation-defined." This
allows WG2 to do what it wants regarding exception objects, and to
limit the types of exception objects allowed, without breaking
anything in WG1. `Error` can be defined as:

{{{
 (define (error message . objs)
   (raise (apply make-error-object message objs)))
}}}


  * '''Options:''' manis, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #169 Add standard-*-port procedures ===

These return the initial values of the corresponding `current-*-port`
procedures, and can be used to access the implementation-provided
standard input, output, and error streams.

  * '''Options:''' r6rs, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #171 Duplicate identifiers in define-record-type ===

What happens if `define-record-type` is specified with two fields that
have the same `accessor` identifiers provided for both fields?  More
generally, we need to say what happens when any two identifiers are
non-unique.

This ticket deals specifically with the situation where two
identifiers (accessors or mutators) of two field clauses in a
`define-record-type` form are identical. This is not meant to address
field names and what happens or what it means if the field names are
symbolically equivalent but lexically distinct.

  * '''Options:''' error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #173 Unifying BEGINs ===

In R5RS, there are three kinds of BEGINs:

1) All subforms are expressions; this can be used wherever an
expression can be used.  (4.2.3)

2) All subforms are definitions; this can be used wherever an internal
definition can be used.  (5.2.2)

3) Subforms can be definitions or expressions intermixed in any order;
this can be used only at top level.  (In R7RS we extend this to module
top level as well).  (5.1)

In particular,

{{{
(define (x)
 (define y 32)
 (begin
   (define z 45)
   (set! y z))
 y)
}}}

is not licensed by any of these provisions, and consequently is not
valid R5RS Scheme.  Nevertheless, all of my usual Schemes accept the
above definition except Scheme48/scsh and SSCM -- actually, SSCM fails
when you invoke x rather than when you define it.  So I'm proposing
that we unify them for R7RS.

  * '''Options:''' cowan, r5rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #174 Safe uses of multiple values ===

Currently, uses of `values` where the values are discarded anyway is
illegal, but all the usual Schemes except SCM and SSCM accept them (I
tested with `begin`).  Should we go with something close to the R6RS
wording?

"The continuations of all non-final expressions within a sequence of
expressions, such as in `lambda`, `begin`, `let`, `let*`, `letrec`,
`letrec*`, `case`, and `cond` forms, take an arbitrary number of
values."

The definition of `begin` would need to change too:

{{{
(define-syntax begin
  (syntax-rules ()
    ((begin exp)
     exp)
    ((begin exp1 exp2 ...)
     (call-with-values
         (lambda () exp1)
       (lambda args
         (begin exp2 ...))))))
}}}

  * '''Options:''' safe-values, r5rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #45 Record-let syntax and semantics ===

{{{
(record-let <record-data> ((<variable> <field>) ...)
  <body>)
}}}

Where each <variable> is filled with the corresponding data <field>
from <record-data> as in a <let> expression, then the <body> is
evaluated with these bindinds added and last expressions is
returned. It is an error if the <record-data> does not contain
corresponding <fields>.

Notice that this works directly on the data itself and that the data
may contain more fields than the one cited in the record-let
expression allowing code to be reused for inherited records.

Do we need to be able to check at runtime if a given record data has
a given field ?


  * '''Options:''' record-let, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #172 Multiple returns from `map` ===

R6RS specifies that `map` does not mutate previous results if there
are multiple returns from map. Should we include this language?

  * '''Options:''' r6rs, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #178 Shadowing with internal definitions ===

From Andre Von Tonder:

On p 19, some shadowing problems that would break lexical scope are
declared to be errors.  However, I believe there are other examples
that shold be errors that are not covered by the report.
 
In R6RS a more general criterion was used - please see R6RS for
details.
 
Here is an example that does not violate the WG1 report but should be
an error becasue it violates lexical scoping.  It does not violate the
WG1 criterion because the meaning of x is not needed to determine
whether (foo x p ) is a definition.

{{{
    (let ((x #f))
      (let-syntax ((foo (syntax-rules (x)
                          ((_ x y) (define y 'outer))
                          ((_ _ y) (define y 'inner)))))
        (let ()
          (foo x p)
          (define x #f) ;; this should be an error because
                        ;; it shadows the previous line where
                        ;; x has already been used in its outer sense
                        ;; during expansion
          p)))
}}}

Here is another example that WG1 allows but that would cause violation
of lexical scoping, because the macro would be evaluated first and
treat ... as a placeholder in a region where it is shadowed to be the
variable bound to 1:

{{{
    (let ()
      (define-syntax list-macro
        (syntax-rules ()
          ((_ x ...) (list x ...))))
      (define ... 1)    ;; This shadows ... in previously expanded macro
                        ;; body and will be a violation of lexical scoping
      (list-macro 1 2)) ;; if the last line evaluates to (1 2)
}}}

OTOH, it is unclear to me if WG1 allows this or not.

{{{
    (let ((x #f))
      (let-syntax ((foo (syntax-rules (x)
                          ((_ x y) (define y 'outer))
                          ((_ _ y) (define y 'inner)))))
        (let ()
          (define x #f)
          (foo x p)
          p)))
}}}

  * '''Options:''' r6rs, r5rs, tonder, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

== WG1 - Modules ==

=== #112 REPL redefinitions ===

R5RS leaves unspecified the semantics of redefining a standard binding
in the REPL.  Do we want to specify semantics, or some set of allowed
behavior for this in the WG1 standard?

REPLs may allow redefinition.  The sixteen cases that occur are
redefining to/from syntax/non-syntax locally/imported, and the issue
is what happens to previous references to the definition.  The general
possibilities are:

  1. redefinition signals an error
  2. previous references are overridden (generally not possible if it the previous definition was syntax)
  3. previous references are preserved (indicating a new binding was created, often preferred if replacing non-syntax with syntax to avoid runtime errors)
  4. the semantics are left unspecified

So all 64 combinations for these 4 values in the following 4x4 matrix
are feasible:

|| From/To       || import || import syntax || define || define-syntax ||
|| import        ||   ?    ||       ?       ||   ?    ||       ?       ||
|| import syntax ||   ?    ||       ?       ||   ?    ||       ?       ||
|| define        ||   ?    ||       ?       ||   ?    ||       ?       ||
|| define-syntax ||   ?    ||       ?       ||   ?    ||       ?       ||

Not all 64 combinations necessarily make sense.  The default from R5RS
is "unspecified", which means all 16 values are unspecified.  Note in
most implementations there is no such thing as a "reference" to
existing syntax, since macros are expanded once, but this is not the
case for SCM or Wraith Scheme.

  * '''Proposals:'''
    * '''override:''' override for all 16 values (non-syntax to syntax can break closure references)
    * '''preserve:''' preserve for all 16 values (must always create a new definition, not mutate, contrary to most implementations)
    * '''common:''' most common behavior among implementations - override, except preserve for non-syntax to syntax
    * '''simple:''' override, except unspecified for non-syntax to syntax
    * '''dynamic:''' override, except unspecified for syntax to anything (compatible with SCM/Wraith)
  * '''Options:''' override, preserve, common, dynamic, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #132 Imports override previous imports? ===

The current draft describes importing different bindings for the same
identifier as "an error."  R6RS explicitly requires this to signal an
error.  Do we want to change this?

This ticket refers only to modules - the top-level semantics are
decided in ticket #112.

  * '''Options:''' override, preserve, error, unspecified, undecided
  * '''Default:''' error
  * '''Preferences:''' 

=== #160 Interleaving of imports and code in a module ===

Given

{{{
   (module (name)
     (begin c1 ...)
     (import (A))
     (begin c2 ...)
     (import (B))
     (begin c3 ...))
}}}

the intention, reference implementation, and specification from
Scheme48 on which the syntax was based say that all imports establish
the initial environment and then the code is expanded in order, but
interleaving the imports is conceivable.

  * '''Options:''' shinn, interleave, unspecified, undecided
  * '''Default:''' shinn
  * '''Preferences:''' 

=== #163 Allow modules at the REPL? ===

Should users be allowed to enter a `module` form at the REPL?

Note that there are actually many varying approaches to generating
moduls at runtime, and Scheme48 and Chibi use an out-of-band REPL
operation to create new modules, leaving the `module` binding open.

  * '''Options:''' yes, no, unspecified, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #141 What are the semantics of modules with respect to separate compilation? ===

ModulesShinn says that the bodies of libraries are evaluated
before any of the bodies of the importing library; does that include,
eg, "at compile time" rather than at "run time"?  It's not clear.

  * '''Options:''' compile-time, unspecified, undecided
  * '''Default:''' undecided
  * '''Preferences:''' 

=== #158 mutating imports ===

Currently the semantics of calling set! or define
on an imported binding is undefined.  Do we
want to specifically make this an error?

  * '''Options:''' error, allowed, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #159 base environments ===

What is the base environment provided by the repl,
scripts, and the result of (scheme-report-environment 7)?

The intention was the base script environment was empty,
scheme-report-environment was (scheme base), and repls
were an implementation-defined superset thereof, but there
are other options and we need to clarify this.

  * '''Options:''' shinn, undecided
    * '''shinn:''' intention as described above
  * '''Default:''' shinn
  * '''Preferences:''' 

=== #161 module argument to eval ===

It would be useful to allow modules as an argument to eval in addition
to environments.  This could be done with a special syntax, or just
the module name as a list.

R6RS provides a procedure `environment` which just
takes a list that looks like an import spec an generates
the corresponding environment.

  * '''Options:''' r6rs, none, undecided
  * '''Default:''' r6rs
  * '''Preferences:''' 

=== #139 `exit` ===

The ballot statement for #62 said we had voted for `exit` when we
voted for ModulesShinn, but that page doesn't mention `exit`.  So we
need to vote on it.

  * '''Options:''' yes, no, undecided
  * '''Default:''' yes
  * '''Preferences:''' 

=== #144 strip prefix on import ===

I'm thinking that for importing code that defines its external symbols
as `foo:this`, `foo:that`, and `foo:tother`, there should be a type of
import clause that strips a specified prefix from imported symbols.
This is equivalent to renaming on import or renaming on export, but
less painful, in the same way as the `prefix` import clause does.

Specific proposal: `(strip-prefix <import-set> <prefix-identifier>)`.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

== WG1 - I/O ==

=== #133 Provide read-line ===

This is an R6RS procedure that was part of PortsCowan, but never
explicitly voted up or down by WG1.  It reads a single line up to a
line delimiter from a given port (the current input by default) and
discards the line delimiter.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #170 Add with-error-to-file procedure ===

Since we now have `current-error-port`, arguably we should have
`with-error-to-file` for completeness.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #176 Are string ports exclusively character ports? ===

From scheme-reports discussion list, by John Cowan:

> Jeronimo Pellegrini scripsit:
> > According to Section 6.7.1, "Conversely, not all character ports are
> > binary ports -- for example, the /string ports/ discussed below".  It
> > is not really clear to wether the document *requires* string ports not
> > to be binary or if it was just an example of a port that *could* be
> > character but not binary.
>
> I haven't thought about it, but I guess it *could* be the latter, if the
> environment provides a default encoding for string ports.

  * '''Options:''' character-only, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #177 Distinguish file and string ports? ===

Should there exist predicates that identify string and file ports?

  * '''Options:''' string-port?, file-port?, both, neither, undecided
  * '''Default:''' neither
  * '''Preferences:''' 

=== #131 Output procedures return value ===

Output procedures (display, write, newline) currently return
unspecified value, do we wish to make them return something (like in
case of an error) or not?

Need proposals.

  * '''Options:''' r5rs, undecided
  * '''Default:''' 
  * '''Preferences:''' 

=== #134 Provide flush-output-port ===

This is an R6RS procedure that was part of PortsCowan, but never
explicitly voted up or down by WG1.  It flushes implementation output
buffers on the specified port, the current output port by default.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

== WG1 - Numerics ==

=== #117 Real numbers have imaginary part #e0 ===

In R6RS, a complex number with imaginary part 0 is only real if the
imaginary part is an exact 0.  In R5RS, this was not true, and the
requirement was simply that `(zero? (imag-part Z))` be true.

  * '''Options:''' exact-only, any-zero, unspecified, undecided
  * '''Default:''' any-zero
  * '''Preferences:''' 

=== #120 Define the semantics of the transcendental functions more fully ===

R6RS has an extended description of the transcendental functions.  Do
we want to include this?

TODO: explain the exact diff, why it is desirable, and whether any
reasonable alternatives are possible.

References:
  * http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.7.3.2

  * '''Options:''' r6rs, r5rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #121 The semantics of expt for zero bases has been refined ===

This is a change also made by R6RS.

R5RS says:

  Returns z1 raised to the power z2. For z1 /= 0, z1^z2^ = e^z2^ log z1; 0^z^ is 1 if z = 0 and 0 otherwise.

R6RS says:

  Returns z1 raised to the power z2. For nonzero z1, this is e^z2^ log z1. 0.0^z^ is 1.0 if z = 0.0, and 0.0 if (real-part z) is positive. For other cases in which the first argument is zero, either an exception is raised [...] or an unspecified number object is returned.


  * '''Options:''' r6rs, r5rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' 

=== #122 Make infinity, NaN, and -0.0 semantics (when supported) consistent with IEEE 754 ===

R5RS does not explicitly describe these values.  We have to decide
whether to require that, if an implementation provides any of these
values, they must be consistent with IEEE 754.

R6RS both requires these values and requires they be consistent with
IEEE 754.

  * '''Options:''' ieee-754, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #175 Control of significant digits or decimal places in NUMBER->STRING ===

Vincent Manis pleads for a way to write numbers with a specified precision:

http://lists.scheme-reports.org/pipermail/scheme-reports/2011-May/000709.html

I (Alaric Snell-Pym) wondered if this should be done via
NUMBER->STRING or via an optional extra argument to ROUND etc
specifying a precision, as a number like `0.01` to get two decimal
places. How to provide significant figures rather than DP without
introducing a base-10 dependency is left as an exercise to the reader
(as is the task of deciding if I'm mad for not wanting a base-10
dependency)

  * '''Options:''' manis, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #138 DivisionRiastradh domain ===

Zero as a divisor aside, what should the domain of the proposed
procedures be?

 1. Any real numbers?
 1. Integers only?
 1. Exact integers only?

  * '''Options:''' reals, integers, exact-integers
  * '''Default:''' 
  * '''Preferences:''' 

=== #217 DivisionRiastradh exactness preservation ===

What about exactness preservation?

 1. Not exactness preserving
 1. Exactness preserving unless the implementation can prove that an inexact argument can't affect the result (as in the case of an exact zero dividend and an inexact divisor)
 1. Exactness preserving in all cases

  * '''Options:''' not-exactness-preserving, exactness-preserving, exactness-preserving-unless
  * '''Default:''' 
  * '''Preferences:''' 

=== #140 Removing `quotient`, `remainder`, `modulo` ===

Are we removing the IEEE Scheme functions `quotient`, `remainder`, and
`modulo` from WG1 Scheme?  If so, we need a special justification, due
to the charter text:

> Existing features of IEEE Scheme may be removed only if a strong
> case can be made that they are fundamentally flawed. Insofar as
> practical, the language should be backwards compatible with the IEEE
> standard, the R5RS standard, and an appropriate subset of the R6RS
> standard.

Here's what DivisionRiastradh says:

> Unfortunately, most programming languages give nondescript names
> such as DIV(IDE), QUOT(IENT), MOD(ULO), and REM(AINDER) to these
> operations. The language should make clear to programmers what
> division operations their programs are performing, especially when
> negative dividends and divisors can arise, but perhaps may not often
> arise during testing.
>
> [...]

> The R5RS gives the names `quotient` and `remainder` to the
> truncating division operator pair, and the name `modulo` to the
> remainder half of the flooring division operator pair. For all these
> three procedures in the R5RS, the dividend may be any integer, and
> the divisor may be any nonzero integer.

On the other hand, we may prefer relegating them to a
backward-compatibility module.

Vote "yes" to keep, "no" to remove, and "module" to relegate to a
module.

  * '''Options:''' yes, no, module, undecided
  * '''Default:''' yes
  * '''Preferences:''' 

=== #151 Extend `finite?` and `nan?` to non-real values ===

R6RS specifies the domain of `finite?` and `nan?` as the real numbers
only.  I propose that `finite?` return `#t` on a non-real value iff
both the real part and the imaginary part are finite and not `+nan.0`,
and that `nan?` return `#t` on a non-real value iff either the real or
the imaginary part is `+nan.0`.

  * '''Proposals:'''
    * '''cowan:''' the above description
  * '''Options:''' cowan, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' 

=== #152 exact-integer-sqrt inconsistent with multiple values module ===

R5RS does not actually specify any procedures which return multiple
values, and so the decision to separate multiple values to a module
was reasonable.  However, we also voted to make `exact-integer-sqrt`,
which is in the base module, return multiple values, namely the root
and the remainder.  That would make the procedure useless unless
multiple values are provided.

We can either make multiple values not a module, make
`exact-integer-sqrt` return a list (or single integer) rather than
multiple values, relegate `exact-integer-sqrt` to a new module, remove
it altogether, or do nothing and leave the inconsistency.

  * '''Options:''' values-in-core, return-list, return-pair, return-root-only, new-module, remove, nothing, undecided
  * '''Default:''' nothing
  * '''Preferences:''' 

=== #180 Make case and cond clauses into bodies ===

Andy Wingo suggests: make the clauses in `case` and `cond` forms
(without `=>`, naturally) be BODY instances, to allow them to have
definitions.  It is well defined AFAIK, and costs nothing.

The counter-argument is that it doesn't "look" like the sort of place
definitions are allowed.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #181 Add WHEN and UNLESS to the base module ===

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #182 Add WHILE and UNTIL ===

These trivial syntaxes add familiarity for new Scheme programmers
coming from other languages, as will almost always be the case.  LOOP
is too big and named-LET too alien.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #183 Escaped newline removes following whitespace? ===

Andy Wingo suggests the R6RS handling of escaped embedded newlines:

{{{
    "asdadf \
    asdfadf"
}}}

in R6RS has the same meaning as "asdf asdfadf".  It allows you to
nicely indent strings that you need to line-break for width.  I
suggest that the production

{{{
   \ NEWLINE WHITESPACE*
}}}

within string literals be elided.

Note an alternate method for handling embedded strings with nice
indentation is scribble syntax.

We voted on various string syntaxes previously but did not
specifically propose this R6RS extension.  We should have a rationale
if we don't follow it.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #184 Require CHAR=?, STRING=? etc. to accept arbitrary numbers of arguments? ===

R5RS makes a point of specifying that supporting more than two
arguments is optional.  (Everything not explicitly mentioned is
optional, so this may have significance.)  R6RS requires accepting 2
or more arguments.  Currently Racket, Gambit, Guile, Chez, Ikarus,
Larceny, Ypsilon, Mosh, and Scheme 9 support the feature, whereas
Gauche, MIT, Chicken, Bigloo, Scheme48/scsh, Kawa, SISC, Chibi,
STklos, and SSCM don't.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #185 Add sixth "centered" division operator ===

From the Guile manual:

* Scheme Procedure: centered/ x y
* Scheme Procedure: centered-quotient x y
* Scheme Procedure: centered-remainder x y

These procedures accept two real numbers x and y, where the divisor y
must be non-zero. centered-quotient returns the integer q and
centered-remainder returns the real number r such that x = q*y + r and
-|y/2| <= r < |y/2|. centered/ returns both q and r, and is more
efficient than computing each separately.

Note that centered-quotient returns x/y rounded to the nearest
integer. When x/y lies exactly half-way between two integers, the tie
is broken according to the sign of y. If y > 0, ties are rounded
toward positive infinity, otherwise they are rounded toward negative
infinity. This is a consequence of the requirement that -|y/2| <= r <
|y/2|.

Note that these operators are equivalent to the R6RS operators div0,
mod0, and div0-and-mod0.

--Andy Wingo

Taylor Campbell thinks these are useless.  We should probably have use
cases for _any_ division operator we include.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #195 Editorial: proposed rewording for `begin` ===

The documentation for `begin' specifies that it is a sequential
construct; but really it splices as well, and also of course it's a
keyword for the module system currently.  This is inaccurate of the
spec to say that "begin is for sequencing".

Suggestion: adopt the language of R6RS section 11.4.7.

--Andy Wingo

We should explain somewhere the four kinds of `begin`s: (begin expr
...), (begin decl ...), top-level begin, and module-top-level begin.
Note that R7RS like R5RS does not have (begin decl ... expr ...).

Vote `yes` to adopt the R6RS description, modified for differences in
the language.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #198 Make it an error for a procedure mapped by MAP and friends to mutate the result list/string/vector ===

This is possibly difficult to enforce, and can break existing R5RS
programs written in very bad style.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #199 Make it an error for a procedure mapped by MAP and friends to return more than once ===

This is possibly difficult to enforce, and can break existing R5RS
programs.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #200 Completing the blob procedures ===

Add `blob`, `blob-map`, `blob-for-each`, and blob conversion functions
to and from lists/vectors/strings.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #205 Roll partial-blob-copy(!) into blob-copy(!) ===

... with extra arguments.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #206 Provide read-syntax for blobs ===

R6RS provides a `#vu8(...)` read-syntax for bytevectors.  SRFI-4 uses
`#u8(...)`.

  * '''Options:''' r6rs, srfi-4, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 

=== #207 Editorial: Polar complex numbers are inexact ===

Add a note saying that `1@2` and `(make-polar 1 2)` MAY evaluate to an
inexact complex number.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #208 Is || a valid identifier? ===

The grammar in 7.1.1 allows `||` as an <identifier>. However, page 5
suggests the `|...|` form is only for convenience (e.g. `|foo bar|` is
equivalent to `foo\x20;bar`). There's no way to normalise `||` to
anything without the vertical bars that's a valid identifier. Was that
intentional, or should the rule be

{{{
<vertical bar> <symbol element>+ <vertical bar>
}}}

Vote `remove` to remove the `|...|` syntax altogether.

  * '''Options:''' remove, empty-valid, empty-invalid, undecided
  * '''Default:''' empty-valid
  * '''Preferences:''' 

=== #191 Include CLOSE-PORT ? ===

Should we include `close-port`, as a generic version of
`close-input-port` and `close-output-port`?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #188 Clarify wording of `and` and `or` definitions ===

The definitions of `and` and `or` may be slightly confusing. Reword
them to be more clear. One possible hiccup is that the current
language permits the return of different false values, while a clearer
wording may preclude this.

R6RS provides a clearer definition that does not provide wiggle room
for multiple false values. Should we use that?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #187 Clarify duplicate bindings in `let*` ===

The language of the standard could clarify that duplicate bindings are
permitted in the clauses of a `let*`.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #215 initial value argument to make-blob ===

`make-blob` should either have an initial value argument, or rationale
why it is inconsistent with `make-vector` and `make-string`.

Vote `yes` for an initial value argument.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' 

=== #216 Controlling use of reader labels on output ===

There are cases when one does not want to output reader labels for
shared structure, such as when you don't care (and want the output to
be more legible), or when you know that the time or space requirements
to construct the table will be too large.

We could offer a parameter to control this, or have a separate
procedure (e.g. `write/simple`) which doesn't use the reader labels.

Finer grained control may also let use specify a predicate for which
values are interesting (e.g. never use labels for strings), or only
use labels for cycles, etc.

  * '''Options:''' parameter, write/simple, none, undecided
  * '''Default:''' none
  * '''Preferences:''' 
