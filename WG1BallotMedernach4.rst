
Backup of Fourth ballot

== WG1 - Core ==

=== #102 module syntax name ===

We decided on `module` earlier, and the current draft reflects that,
but some comments from the community suggest we revisit this issue.

Emails from Aaron Hsu and Denis Washington:

{{{
[AH] I do not buy the argument that we are making things better by
using `module` instead of `library` in this case. The module term is
much more common throughout, including systems in Chez, PLT, Scheme48
(I believe), among others.

[...]

[DW] (Bigloo and Chicken are two others which use `module` for
existing forms.) I feel that clashing with all of these
implementations substantially increases the burden for these systems'
implementors to adopt to R7RS.

[...]

[DW] What about `define-library`? It might be slightly confusing as it
sounds a bit procedural for a purely syntactic construct, but it does
not seem to clash with any existing implementation (as far as a quick
Google search reveals, at least [[as|well as direct testing --JC]]) and
preserves the "library" term, which is common, well-known, clear and
in line with previous Scheme specs (R6RS and, in a way, R5RS' usage of
the term "library procedure").
}}}

If we choose a unique name such as `define-library` then there is no
chance of conflicts, but the name itself may not be aesthetically
pleasing.

If we choose an existing name, implementations may have difficulty
distinguishing between their native form and the R7RS module syntax,
possibly requiring a command-line flag for "R7RS mode" or some such.

TODO: Create a list of existing names used, and ways implementations
may detect the difference in the event of a conflict.

  * '''Options:''' module, library, define-module, define-library, abstraction, component, r7rs-module
  * '''Default:''' module
  * '''Preferences:''' r7rs-module, abstraction, library, component, undecided, define-library, define-module, module, package

  Other possible names without clashing:
  abstraction
  component
  r7rs-module
  
  Name clash occurs with:
  module
  package (Snow)
  define-module (Gauche)
  

=== #145 RFC 2119 compliance ===

R6RS introduces a description of requirement levels following
[[http://tools.ietf.org/html/rfc2119|RFC 2119]] use of the modal verbs
"may", "should", "must", "shall", "should not", "must not", "shall
not."

Do we want to incorporate this?  If so, we'll also need to revise
existing uses of those phrases, and possibly introduce them where
needed.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, undecided, no

=== #185 Add sixth "centered" division operator ===

We are re-opening the sixth "centered" division operator:

  * `(centered/ x y)`
  * `(centered-quotient x y)`
  * `(centered-remainder x y)`

These correspond to the R6RS operators `div0`, `mod0` and
`div0-and-mod0` defined as in the Guile manual:

{{{
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
}}}

Vote `centered` to add the sixth operator, `no` to stick with the five
operators, and `remove` to drop the full five sets of operators from
the small language.

  * '''Options:''' centered, no, remove, undecided
  * '''Default:''' no
  * '''Preferences:''' centered, no, undecided, remove

=== #193 values and procedure arguments ===

A suggestion was made to specify that arguments in a procedure call
and "init bindings" (presumably in `let` and friends) should evaluate
to exactly one value.  Currently it is "an error" to pass a non-single
value to any continuation not created with call-with-values, which
means implementations are free to add their own handling of this
(e.g. raising an error or taking just the first value as in CL).

Do we want to require implementations to signal an error in these
cases?  Not currently many implementations of MV would not be able to
detect these cases efficiently.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' no

The current situation is that some implementations truncate values to the first one, others signal an error, others reifie values, etc. Hence we cannot reach a consensus on this issue and as such one could not rely on it to write portable robust code using multiple values in a single value context. Ok I agree now that signaling an error is a bit too much.


=== #202 Semi-Editorial: Should we remove the specific syntaxes from the BNF in section 7? ===

These date back to R4RS, when Scheme had a fixed syntax and it made
sense to enumerate it here.  Are they still doing useful work, now
that syntax forms can be changed at will?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' no, yes, undecided

 This is intended to describe fundamental Scheme with no macro extensions.

=== #212 Let LOAD take an optional environment argument ===

Change `load` to take a second argument which is the environment to
load into.  The default is `(interaction-environment)`.

See also #161.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, no, undecided

It is a change worth to consider.


=== #220 New DIGIT-VALUE procedure ===

This accepts a character which is a numeric digit and returns its
value as a digit, or `#f` if it's not a digit:

{{{
(digit-value #\3) => 3
(digit-value #\x0664) => 4
(digit-value #\x0EA6) => 0
}}}

You need the following list of zero-value characters to implement this
for all of Unicode (currently); implementations that support only a
subset of Unicode need only a subset of the list, of course:

{{{
(define zeros '(
  #\x0030 ;DIGIT ZERO
  #\x0660 ;ARABIC-INDIC DIGIT ZERO
  #\x06F0 ;EXTENDED ARABIC-INDIC DIGIT ZERO
  #\x07C0 ;NKO DIGIT ZERO
  #\x0966 ;DEVANAGARI DIGIT ZERO
  #\x09E6 ;BENGALI DIGIT ZERO
  #\x0A66 ;GURMUKHI DIGIT ZERO
  #\x0AE6 ;GUJARATI DIGIT ZERO
  #\x0B66 ;ORIYA DIGIT ZERO
  #\x0BE6 ;TAMIL DIGIT ZERO
  #\x0C66 ;TELUGU DIGIT ZERO
  #\x0CE6 ;KANNADA DIGIT ZERO
  #\x0D66 ;MALAYALAM DIGIT ZERO
  #\x0E50 ;THAI DIGIT ZERO
  #\x0ED0 ;LAO DIGIT ZERO
  #\x0F20 ;TIBETAN DIGIT ZERO
  #\x1040 ;MYANMAR DIGIT ZERO
  #\x1090 ;MYANMAR SHAN DIGIT ZERO
  #\x17E0 ;KHMER DIGIT ZERO
  #\x1810 ;MONGOLIAN DIGIT ZERO
  #\x1946 ;LIMBU DIGIT ZERO
  #\x19D0 ;NEW TAI LUE DIGIT ZERO
  #\x1A80 ;TAI THAM HORA DIGIT ZERO
  #\x1A90 ;TAI THAM THAM DIGIT ZERO
  #\x1B50 ;BALINESE DIGIT ZERO
  #\x1BB0 ;SUNDANESE DIGIT ZERO
  #\x1C40 ;LEPCHA DIGIT ZERO
  #\x1C50 ;OL CHIKI DIGIT ZERO
  #\xA620 ;VAI DIGIT ZERO
  #\xA8D0 ;SAURASHTRA DIGIT ZERO
  #\xA900 ;KAYAH LI DIGIT ZERO
  #\xA9D0 ;JAVANESE DIGIT ZERO
  #\xAA50 ;CHAM DIGIT ZERO
  #\xABF0 ;MEETEI MAYEK DIGIT ZERO
  #\xFF10 ;FULLWIDTH DIGIT ZERO
  #\x104A0 ;OSMANYA DIGIT ZERO
  #\x11066 ;BRAHMI DIGIT ZERO
  #\x1D7CE ;MATHEMATICAL BOLD DIGIT ZERO
  #\x1D7D8 ;MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
  #\x1D7E2 ;MATHEMATICAL SANS-SERIF DIGIT ZERO
  #\x1D7EC ;MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
  #\x1D7F6 ;MATHEMATICAL MONOSPACE DIGIT ZERO
))

(define (digit-value ch) (digit-value* ch zeros))

(define (digit-value* ch zeros)
  (if
    (null? zeros)
    #f
    (let*
      ((val (char->integer ch))
       (val0 (char->integer (car zeros)))
       (val9 (+ val0 9)))
        (if
          (and (<= val0 val) (<= val val9))
          (- val val0)
          (digit-value* ch (cdr zeros))))))

}}}

CL provides this as `digit-char-p`, which is its substitute for
`char-numeric?`.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, undecided, no

=== #221 Editorial: Consolidate and clarify formal-parameter conventions ===

Page 4 shows a naming convention for procedure
parameters:

  * obj - any object
  * list - list
  * z - complex

and so on.

There is no notation convention for characters and strings.
It feels a bit funny that the document uses this notation and
then x, x_1, x_2 are used for reals, but then uses "char_1"
and "char_2" for characters.

  * '''Proposals:'''
    * '''abbrev:''' abbreviate `char` => `ch` and `string` => `str`
    * '''list:''' just add `char` and `string` to the list of conventions
  * '''Options:''' abbrev, list, none, undecided
  * '''Default:''' none
  * '''Preferences:''' abbrev, list


=== #222 Rename character ports to textual ports ===

The term "textual port" is R6RS compatible.

  * '''Options:''' textual, character, undecided
  * '''Default:''' character
  * '''Preferences:''' textual, character, undecided

=== #223 Converting current-{input,output}-ports to binary ===

The standard input and output ports which `current-input-port` and
`current-output-port` are initially bound to are not opened
explicitly.  They default to character ports.  Should they be
replaceable with equivalent binary ports using the following
procedures?

  * (standard-input-is-binary!)
  * (standard-output-is-binary!)

It is an error to call either of these if the corresponding
`current-{input,output}-port` is not the original value, or if any I/O
has been performed on them, or if they cannot reasonably be treated as
binary.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' undecided

It would be nice to do so but we need a better interface than this either to generally change port type or why not something like converting it whan read-u8 as soon as is used ?

=== #224 Additional blob I/O ===

See BlobIoShinn, which provides the ability to read and write blobs,
and simple conversions between blobs (interpreted as UTF-8) and
strings.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes

  Absolutely needed

=== #226 Remove property-list file specs from WG1 ===

Getting rid of file-specs was the motivation for the new binary I/O
API we agreed on, but it was not made explicit in the proposal.
Consequently, although all references to `file-spec` have been removed
from the current draft, we should vote on this for completeness.

  * '''Options:''' remove, keep, undecided
  * '''Default:''' remove
  * '''Preferences:''' remove

=== #229 EQV? and NaN ===

For good reasons, `+nan.0` is not `=` to any other number, including
itself.  However, `eqv?` is about "sameness" rather than "equality".

The `same` proposal is that we add two clauses to the definition of
`eqv?`, one saying that if both arguments are `+nan.0`, `eqv?` must
return `#t`, and if one argument is `+nan.0` and the other is not,
`eqv?` must return `#f`.  This is what R6RS specifies.

The `different` proposal is that we add a single clause requiring
`(eqv? +nan.0 x)` to return `#f` for any `x`.  This is the behavior
that results for any R5RS implementation that adds support for +nan.0
as an IEEE float without any special handling for it in `eqv?`.

Note the second clause in the `same` proposal is universally supported
by all implementations with `+nan.0` except for SISC, which appears to
have a bug (see below), so the only thing to decide is the first
clause.

The following 7 implementations return `#t`: chez, gambit, guile,
ikarus, kawa, racket, stklos.

The following 7 implementations return `#f`: bigloo, chibi, chicken,
gauche, larceny, mit-scheme, scheme48.

SigScheme and Scheme 9 don't have +nan.0.  SISC currently has a bug
where `(= nan.0 x)` is true for any `x`.

Since implementations currently disagree on these semantics, it may
make the most sense to leave this `unspecified`.

  * '''Options:''' same, different, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' same, unspecified, undecided, different

=== #230 Reserve module names for current and future standards ===

Do we want to add a clause stating that all module names under the
`(scheme ...)` name are reserved for current and future standards?
Do we want to reserve the `(srfi ...)` names?

Note the name `scheme` may be changed pending the result of #237.

  * '''Options:''' scheme, srfi, both, neither, undecided
  * '''Default:''' no
  * '''Preferences:''' both, scheme, undecided


=== #232 define-values ===

Several implementations provide a `define-values` macro.  This allows
cleanly writing multiple definitions with a shared state for example.

Should we have it on WG1 or not?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes

=== #234 Add EAGER from SRFI 45 ===

`eager`, like `delay`, returns a promise object that can be forced by
`force`, but it evaluates its argument up front (it is a procedure,
not syntax) and stashes it in the promise in such a way that `force`
can access it.

Semantically, writing `(eager expression)` is equivalent to writing
`(let ((value expression)) (delay value))`.

Some debate was given as to how useful `eager` is - generally, if
something is known in advance to be eager you don't want to make it a
promise to begin with.  Use cases should be provided if we want to
include this.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, no, undecided

John's example convinced me that this has its place inside the (scheme lazy) module.

=== #235 Should bytevector constants be self-quoting? ===

They are in R6RS, apparently because they are considered more closely
related to strings rather than vectors.

Note currently vectors are not self-quoting.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes

=== #237 Change "scheme" in module names to "rsn" or "rs11" or something else ===

The term "scheme" is already in use in module names on some Scheme
implementations.  We need to pick something that nobody is using.

The term "rnrs" was used by R6RS, but this was integrated with the
library versioning mechanism.  It therefore may not be suitable, and
either way would cause conflicts with existing R6RS modules.

Feel free to write in a name.

  * '''Options:''' scheme, r7rs, scheme2011, undecided
  * '''Default:''' scheme
  * '''Preferences:''' scheme, r7rs, undecided

Scheme or r7rs are the most natural and expected terms

=== #238 Reserve #! for read directives ===

From Denis Washington:

{{{
Reading chapter 2 of the third draft, I was thinking: now that we have
`#!fold-case` and `#!no-fold-case` and other directives might follow
in WG2, wouldn't it be appropriate for section 2.3 (Other notations)
to define `#!` as generally introducing a "read directive"? That would
encourage implementations to use the same syntax for their own
directives, which helps portability (an implementation could just
ignore unknown directives which might just be used by another for
optimization purposes).
}}}

Note that since the only use we have alters the reader, the `!` is
consistent with the existing convention for `!`.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' no, undecided, yes

=== #240 Rename current-second to current-tai ===

The procedure, as currently spec'd, may return a fraction of a second,
and there should be a mention of TAI in it.  `Current-tai-time` is
redundant, since the T in TAI stands for Time (or ''Temps'').

  * '''Options:''' current-second, current-tai-time, current-tai, undecided
  * '''Default:''' current-second
  * '''Preferences:''' current-second

 current-second is a good naming choice, describing that this is indeed TAI in the function description is enough.


=== #243 Add optional support for -0.0 ===

Implementations should be permitted to distinguish 0.0 from -0.0 in
accordance with IEEE 754.  `0.0` and `-0.0` should be the same to `=`
and friends, but should be distinguishable by `eqv?`.

Mathematically, negative inexact zero represents a number greater than
the largest representable negative inexact number and less than or
equal to 0.  This is different from positive inexact zero, which
represents a number greater than or equal to 0 and less than the
smallest representable positive inexact number.

Vote `yes` to adapt the description of -0.0 from R6RS and include
examples where appropriate.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes

  By conformance to IEEE 754


=== #244 Extended "Overview of Scheme" chapter ===

Denis Washington said (<http://lists.scheme-reports.org/pipermail/scheme-reports/2011-August/001255.html>):

{{{
I would love to see a ticket added about possibly including (some of)
the detailed "Overview of Scheme" chapter from R6RS into the report;
it helps very much to understand the rest of the report and is
invaluable for e.g. students. Would someone from the working group do
this (provided that anyone actually agrees with me)? It would be sad
if this consideration were forgotten.
}}}

The "Overview of Scheme" in R6RS contains a similar introduction as in
the section of the same name in R5RS, followed by a rough tutorial
describing the basic syntax and data types.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes, undecided, no

=== #245 Editorial: Case-folding should refer to UAX ===

In the string case conversion, it mentions the context sensitivity of
Greek sigma: A small final sigma needs to be used when it is at the
end of the word.  However, there's no definition of "word", which can
lead inconsistent behavior among implementations.  We can refer to UAX
#29, as R6RS does.

Vote `uax-29` for the reference, or `unspecified` to leave this up to
the implementation.

  * '''Options:''' uax-29, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' uax-29


=== #248 fill-string and fill-vector: optional start/end arguments? ===

Should we provide the obvious way to fill part of a string or vector?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' yes

=== #254 Behavior of open-output-file on existing files ===

Currently this is unspecified, and different implementations behave
differently.  WG2 will likely provide explicit control for this, but
we may want to specify the default behavior in WG1.

Vote `overwrite` to truncate and overwrite the existing file, or
`error` to require an error be signalled.

  * '''Options:''' overwrite, error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Preferences:''' unspecified

  Relegate control on opening files to WG2.

=== #262 module factoring (scheme io) ===

This is one of several issues raised by ModuleFactoringSummary.

This and the following items ask you to decide whether a current set
of procedures under discussion belongs in the core or a separate
module.  The default is `core` reflecting the fact that R5RS had no
separate modules at all.

Should the basic I/O procedures (not involving file I/O or reading or
writing) be in the core or a separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Preferences:''' separate

Rename (scheme io) to (scheme ports) as it deals with ports management, which are not exclusively used for I/O (as strings ports for example). Don't require it into the base because some implementation don't need this. 


=== #263 module factoring (scheme repl) ===

This is one of several issues raised by ModuleFactoringSummary (see #262).

Should `interaction-environment` be in the core, the REPL module, or
the `eval` module?

  * '''Options:''' core, eval, repl
  * '''Default:''' core
  * '''Preferences:''' repl

=== #264 module factoring (scheme case-lambda) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `case-lambda` be in the core or a separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Preferences:''' separate

  This one has to be in a module: it poorly handles neither optional arguments nor pattern matching. It is the kind of feature we later regret having into the core.

=== #265 module factoring (scheme multiple-values) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `values` and `call-with-values` be in the core or a separate
module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Preferences:''' core

=== #266 module factoring (scheme char normalization) ===

This is one of several issues raised by ModuleFactoringSummary.

Should the Unicode normalization procedures be in the core, the `char`
module, or their own separate module?

  * '''Options:''' core, char, separate
  * '''Default:''' core
  * '''Preferences:''' separate, char

=== #267 module factoring all I/O ===

This is one of several issues raised by ModuleFactoringSummary.

Should we provide an aggregate module for the three (or four) proposed
I/O modules, where `(scheme io)` provides all of:

  * `(scheme io base)`   (if not in the core)
  * `(scheme file)`
  * `(scheme read)`
  * `(scheme write)`

  * '''Options:''' yes, no
  * '''Default:''' no
  * '''Preferences:''' no
  

=== #268 module factoring (scheme parameter) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `make-parameter` and `parameterize` be in the core or their own
separate module?

Note `current-in/output/error-port` are parameters, though they do not
require the parameter API to be useful as is.

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Preferences:''' separate

  These functions allow creation and management of dynamic bindings, let put these in a module so people wanting them know what they are doing. (Moreover as stated above, `current-in/output/error-port` have their own API)

=== #269 module factoring (scheme record) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `define-record-type` be in the core or in its own separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Preferences:''' separate

  As this is a sensible issue, I would prefer not to write SRFI-9 in stone into the core but let open the (scheme record *) namespace for all kind of record implementations (in WG2 maybe).

=== #270 module factoring (scheme char) ===

This is one of several issues raised by ModuleFactoringSummary.

Should the Unicode character case and property utilities be in the
core or their own separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Preferences:''' separate

  Separate and optional as not all Scheme implementations will support Unicode.

=== #231 WG1/WG2 Scheme naming proposal ===

Denis Washington made the following proposal for the names of the
variants of Scheme defined by WG1 and WG2:

  * WG1: "Report on the Algorithmic Language Scheme, Revised 2011" (abbr.: RS11)

  * WG2: "Report on Standard Extensions to the Algorithmic Language Scheme, Revised 2011" (abbr.: RSES11; alternatively, "[...] Commobn Extensions [...]", abbr. RCES11; should probably be "Published 2011" as there is no original document to revise.)

The current draft is written in the same style and layout as all the
drafts through R5RS, and by default has kept the same naming
convention.  Do we want to change the name?

Note the name for WG2 is beyond the scope of this ballot.

  * '''References:'''
    * http://lists.scheme-reports.org/pipermail/scheme-reports/2011-July/001170.html
  * '''Proposals:'''
    * '''r7rs:''' "R7RS" as in the current draft
    * '''2011 :''' "Scheme 2011" as in the proposal above
  * '''Options:''' r7rs, 2011, undecided
  * '''Default:''' r7rs
  * '''Preferences:''' r7rs

Putting the date is a bad idea IMHO, it wrongly makes the language look deprecated some years after.  Just name it something like "Report on the Core Scheme Language" with a subtitle like "2011 edition, 7th revision" (not directly a part of the title).

=== #189 List changes from R6RS ===

An incomplete list of the differences between this language and the
R6RS is available.  Do we want to include this directly into the
document?  Alternately it can go into a separate document, or be
included in the WG2 document.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Preferences:''' undecided

  As the language is now splitted in "little WG1" and "big WG2" languages, does it really make sense to compare WG1 to R6RS rather than WG2 ? Comparing WG1 to R5RS is a better idea IMHO.

=== #227 quasiquote and cycles ===

Some doubts were raised as to whether reader label cycles were allowed
in `quasiquote`.  Since cycles in code are "an error", and
`quasiquote` is just a macro expanding into code, then this case is
also an error (and thus implementation dependent).

Do we want to explicitly state that the result is an error for
`quasiquote`?  Or make a special exception and try to require handling
of some cases for `quasiquote`?  Or make no special note of this since
it's already covered?

Vote `note` to add a note.

  * '''References:'''
    * http://lists.scheme-reports.org/pipermail/scheme-reports/2011-July/001142.html
  * '''Proposals:'''
    * '''note:''' make a note
  * '''Options:''' note, nothing, undecided
  * '''Default:''' nothing
  * '''Preferences:''' note

== WG1 - Reader Syntax ==

=== #214 string/symbol escape sequence confusing ===

From Felix Winkelmann:

{{{
Using "\xX...;" as escape sequence is suboptimal, as it confuses
syntax highlighters. I also have no knowledge of any precedent
of this syntax. Also: how is this sequence handled inside "|...|"?
}}}

Prior to R6RS no implementations to my knowledge used this syntax, and
"\xXX" with a fixed two characters and no trailing semi-colon, as well
as "\x{X...}" were used.  The semi-colon specifically conflicts with
the existing uses of "\xXX", although this can't represent all
characters.  Braces would at least allow for backwards compatibility
with existing code.

Note the escapes for symbols will be the same unless someone proposed
otherwise.

  * '''Proposals:'''
    * '''semi-colon:''' the current draft and R6RS option
    * '''brace:''' "\x{X...}" where the braces are required
    * '''brace-or-semi-colon:''' either of the two above
    * '''fixed-two:''' "\xXX" with two fixed hex-digits (can't support all chars)
    * '''optional-semi-colon:''' use semi-colon as a terminator if present, otherwise only read the first two hex-digits
  * '''Options:''' semi-colon, brace, brace-or-semi-colon, fixed-two, optional-semi-colon, undecided
  * '''Default:''' semi-colon
  * '''Preferences:''' brace, semi-colon

=== #218 infinity/nan syntax ===

The current BNF for symbol syntax is rather cumbersome, having to
account for the fact that +inf.0, -inf.0 and +nan.0 are numbers even
though they do not begin with a numeric prefix.

A simple solution would be to use an alternative such as 0/1, 0/-1 and
0/0, respectively.  These are shorter, more self explanatory, and do
not conflict with the definition of symbol syntax.

Alternately we can just require a numeric prefix on the existing
names.

  * '''Proposals:'''
    * '''r6rs:''' +inf.0, -inf.0, +nan.0
    * '''short:''' 0/1, 0/-1, 0/0
    * '''dotted:''' 0/1.0, 0/-1.0, 0/0.0
    * '''prefix:''' 0+inf, 0-inf, 0+nan
    * '''prefix-dotted:''' 0+inf.0, 0-inf.0, 0+nan.0
  * '''Options:''' r6rs, short, dotted, prefix, prefix-dotted, undecided
  * '''Default:''' r6rs
  * '''Preferences:''' r6rs

=== #219 bring back readable boolean literals ===

Scheme used to use `#!true` and `#!false` before abbreviating to the
unfortunate `#t` and `#f` syntax, which look far too much alike.

We could add these back in as aliases, optionally without the "!" now
that tokens are required to be delimited so there would be no
ambiguity.

Note - this proposal is to add alternate names.  `#t` and `#f` will be
kept however we vote.

  * '''Proposals:'''
    * '''long:''' `#true` and `#false`
    * '''bang-long:''' `#!true` and `#!false`
  * '''Options:''' long, bang-long, none, undecided
  * '''Default:''' none
  * '''Preferences:''' long, none, undecided, bang-long

Alex convinced me that this is more readable (along with keepeing shortened forms for compatibility).

=== #22 mantissa widths and placeholders ===

Previously we voted to keep the R5RS `#` placeholders for "unknown"
digits, and leave out the new R6RS mantissa widths.

Feedback suggests that this feature is never used in R5RS programs.
It was a leftover from R3RS procedures for formatting numbers when
more digits of precision were asked for than were available, and also
commonly used in papers on the subject of formatting numbers.  These
formatters are no longer in the language, and human-written code
rarely if ever takes advantage of the feature, so we may want to
reconsider removing it from the language.

Vote `no` to remove the `#` placeholders.

  * '''Options:''' r5rs, no, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' no

=== #68 "Undefined value" vs. "undefined values" ===

Previously we voted to keep the R5RS semantics of returning a single,
unspecified value for the results of side-effecting expressions.

Some implementors have raised concerns about this.  The R6RS semantics
allow implementations to return an unspecified number of unspecified
values, which allows for the R5RS semantics, as well as for returning
zero values.

The argument is that a non-trivial amount of existing R5RS code
explicitly depends on these expressions returning a single value.

The counter-argument is that using an unspecified value at all is bad
style, and there is likely a large overlap between the programmers who
do so and the implementations which will continue to return a single
value.  The hope is that code using "good style" would be portable,
whereas "bad style" would continue to work on existing implementations
but not be portable to others.

We should reconsider this item.

  * '''Options:''' r5rs, r6rs, undecided
  * '''Default:''' r5rs
  * '''Preferences:''' r6rs, undecided, r5rs

  R6RS phrasing allows flexibility. With it implementations are free to return any useful value or to return no values instead of an unfortunate "undefined" value.
