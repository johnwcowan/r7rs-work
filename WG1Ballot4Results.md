= Notes about Results =

See [[:WG1BallotExplanation|WG1BallotExplanation]].


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

  * '''Options:''' module, library, define-module, define-library
  * '''Default:''' module
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: define-library, define-module
    * [[:WG1BallotGanz|Ganz]]: library
    * [[:WG1BallotGleckler|Gleckler]]: define-library, define-module, module, library
    * [[:WG1BallotMedernach|Medernach]]: r7rs-module, abstraction, library, component, undecided, define-library, define-module, module, package
    * [[:WG1BallotShinn|Shinn]]: define-library
    * [[:WG1BallotSnellPym|SnellPym]]: (define-module define-library), library, module
  * '''Results:''' '''define-library''', define-module, library, module, r7rs-module, abstraction, component, undecided, package
  * '''Ratios:''' 4:0, 4:2, 5:0, 4:1, 4:1, 4:1, 4:1, 5:0
  * '''Rationales:'''

 `Gleckler`::
  I strongly dislike the term library, but R6RS has chosen it. We need to use something different than the `library' form, though, to avoid compatibility problems, so I'm choosing `define-library'.
 `Medernach`::
  Other possible names without clashing: abstraction component r7rs-module Name clash occurs with: module package (Snow) define-module (Gauche)
 `Shinn`::
  Reversed from my earlier decision, we need to avoid conflicts and I haven't seen any better names.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: no, undecided
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotLucier|Lucier]]: yes
    * [[:WG1BallotMedernach|Medernach]]: yes, undecided, no
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', no, undecided
  * '''Ratios:''' 5:2, 5:1
  * '''Rationales:'''

 `Ganz`::
  Best to avoid "should" and "should not" entirely.
 `Gleckler`::
  This is more trouble than it's worth. We haven't had this in all the previous revisions of the standard and we've been just fine.
 `SnellPym`::
  It's a weak yes. I like the idea of defining these terms more tightly.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: centered, no
    * [[:WG1BallotGanz|Ganz]]: centered, undecided
    * [[:WG1BallotGleckler|Gleckler]]: centered, no
    * [[:WG1BallotHsu|Hsu]]: undecided
    * [[:WG1BallotLucier|Lucier]]: remove, no
    * [[:WG1BallotMedernach|Medernach]]: centered, no, undecided, remove
    * [[:WG1BallotShinn|Shinn]]: remove, no
    * [[:WG1BallotSnellPym|SnellPym]]: remove, centered, no
  * '''Results:''' '''centered''', no, undecided, remove
  * '''Ratios:''' 5:2, 5:1, 4:3
  * '''Rationales:'''

 `Gleckler`::
  We should have the complete set, and it's not hard to implement.
 `Hsu`::
  I don't understand these well enough to vote on them yet.
 `Lucier`::
  (1) I don't see the "centered-*" operators as somehow a "completion" of the other division operators. (2) In the small language I'd recommend only the "truncate-*" and "floor-*" operators for two reasons: they are the only division operators that have an established history of use in computer programming and mathematics, and they form a minimal extension of R5RS. (I'm not saying that the other division operators have never been used in mathematics or programming (see CL), but small Scheme is not supposed to be a kitchen-sink language.)
 `Shinn`::
  We need to use and understand all of these operators more before deciding. Some of the newly proposed operators have no known use in existing programs to my knowledge.
 `SnellPym`::
  I used to be for these, but I'm starting to veer towards having a simple set in WG1, and having the full set as a module in WG2 or an SRFI.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: no, undecided
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: no
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: no
  * '''Results:''' '''no''', undecided
  * '''Ratios:''' 7:0
  * '''Rationales:'''

 `Gleckler`::
  As John points out, there are few places where we require signaling an error. This situation doesn't merit special treatment.
 `Medernach`::
  The current situation is that some implementations truncate values to the first one, others signal an error, others reifie values, etc. Hence we cannot reach a consensus on this issue and as such one could not rely on it to write portable robust code using multiple values in a single value context. Ok I agree now that signaling an error is a bit too much.
 `Shinn`::
  No, this is silly.
 `SnellPym`::
  I think this should be allowed behaviour, in fact, with the defined semantics of taking the first value and discarding others; this allows future extension by returning extra values that old code can then easily discard. Returning zero values should be an error, of course, but for implementation convenience, I would be inclined to allow it to return a value that causes an error if it's actually used for anything (eg, allow zero return values to be implemented by returning a special sentinel value; it needn't fail at the binding site and may fail on the first type dispatch).

=== #202 Semi-Editorial: Should we remove the specific syntaxes from the BNF in section 7? ===

These date back to R4RS, when Scheme had a fixed syntax and it made
sense to enumerate it here.  Are they still doing useful work, now
that syntax forms can be changed at will?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: no, undecided
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: no, yes, undecided
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''no''', yes, undecided
  * '''Ratios:''' 5:2, 5:0
  * '''Rationales:'''

 `Ganz`::
  Don't remove. Standard syntax provides better grounding than "floating" nonterminals.
 `Gleckler`::
  Even if they can be changed, it's good to have them enumerated for reference.
 `Medernach`::
  This is intended to describe fundamental Scheme with no macro extensions.
 `Shinn`::
  Drop these, they don't belong when describing a language without fixed syntax.
 `SnellPym`::
  Yes, they should go, or else the syntax is not correct.

=== #212 Let LOAD take an optional environment argument ===

Change `load` to take a second argument which is the environment to
load into.  The default is `(interaction-environment)`.

See also #161.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes, undecided
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotMedernach|Medernach]]: yes, no, undecided
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: no
  * '''Results:''' '''yes''', no, undecided
  * '''Ratios:''' 5:1, 5:0
  * '''Rationales:'''

 `Ganz`::
  I agree with Aaron's comments about also allowing for an evaluator (but one of two arguments).
 `Gleckler`::
  This makes `load' much more useful.
 `Medernach`::
  It is a change worth to consider.
 `SnellPym`::
  This just doesn't feel right to me. Hard to put my finger on exactly why, though.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes, undecided
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: yes, undecided, no
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', undecided, no
  * '''Ratios:''' 6:0, 6:1
  * '''Rationales:'''

 `Gleckler`::
  As John points out, this is essential now for implementations that support Unicode now that we support non-ASCII digits.
 `Shinn`::
  This belongs in the `(scheme char)` module.
 `SnellPym`::
  This is a useful feature, and essential for practical working in international environments, I suspect.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: list, abbrev
    * [[:WG1BallotGanz|Ganz]]: abbrev, list, undecided
    * [[:WG1BallotGleckler|Gleckler]]: list, abbrev
    * [[:WG1BallotLucier|Lucier]]: list, no
    * [[:WG1BallotMedernach|Medernach]]: abbrev, list
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: abbrev, list
  * '''Results:''' '''list''', abbrev, no, undecided
  * '''Ratios:''' 3:3, 6:1, 6:0
  * '''Rationales:'''

 `Gleckler`::
  This will make the document clearer. The abbreviations don't save enough space to be worth doing.
 `Shinn`::
  I think this is self-explanatory.
 `SnellPym`::
  It makes sense to be consistent.

=== #222 Rename character ports to textual ports ===

The term "textual port" is R6RS compatible.

  * '''Options:''' textual, character, undecided
  * '''Default:''' character
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: textual
    * [[:WG1BallotGanz|Ganz]]: character
    * [[:WG1BallotGleckler|Gleckler]]: textual, character
    * [[:WG1BallotLucier|Lucier]]: character
    * [[:WG1BallotMedernach|Medernach]]: textual, character, undecided
    * [[:WG1BallotShinn|Shinn]]: textual
    * [[:WG1BallotSnellPym|SnellPym]]: undecided
  * '''Results:''' '''textual''', character, undecided
  * '''Ratios:''' 4:2, 4:1
  * '''Rationales:'''

 `Gleckler`::
  I prefer character, but there's no reason to be different than R6RS here.
 `Shinn`::
  Bikeshed, go with R6RS.
 `SnellPym`::
  What colour to paint the bikeshed, eh?

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: no
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotMedernach|Medernach]]: undecided
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: no
  * '''Results:''' '''no''', undecided
  * '''Ratios:''' 5:1
  * '''Rationales:'''

 `Gleckler`::
  This feels super kludgy. I'd rather leave it to implementations than specify something like this.
 `Medernach`::
  It would be nice to do so but we need a better interface than this either to generally change port type or why not something like converting it whan read-u8 as soon as is used ?
 `Shinn`::
  This needs more thought.
 `SnellPym`::
  Ugly hack. Standard input and output are defined as character streams for the good of repls and the like. Having them as binary streams should be done with some kind of whole-program pragma/option, which we currently have no mechanism in place for, and IMHO this should leave current-input-port and current-output-port still pointing to character ports (current-output-port can be stderr under POSIX, and current-input-port either exist but start off closed or something like that. Windows systems may be perfectly capable of having binary stdin and stdout while presenting a textual console in a window, for example.

=== #224 Additional blob I/O ===

See BlobIoShinn, which provides the ability to read and write blobs,
and simple conversions between blobs (interpreted as UTF-8) and
strings.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: yes
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', no
  * '''Ratios:''' 6:1
  * '''Rationales:'''

 `Gleckler`::
  Yes, but the names should use "bytevector" instead of "blob."
 `Medernach`::
  Absolutely needed

=== #226 Remove property-list file specs from WG1 ===

Getting rid of file-specs was the motivation for the new binary I/O
API we agreed on, but it was not made explicit in the proposal.
Consequently, although all references to `file-spec` have been removed
from the current draft, we should vote on this for completeness.

  * '''Options:''' remove, keep, undecided
  * '''Default:''' remove
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: remove
    * [[:WG1BallotGleckler|Gleckler]]: remove
    * [[:WG1BallotMedernach|Medernach]]: remove
    * [[:WG1BallotShinn|Shinn]]: remove
    * [[:WG1BallotSnellPym|SnellPym]]: remove
  * '''Results:''' '''remove'''
  * '''Ratios:''' 

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: same, unspecified
    * [[:WG1BallotGanz|Ganz]]: same
    * [[:WG1BallotGleckler|Gleckler]]: same, unspecified
    * [[:WG1BallotLucier|Lucier]]: same*
    * [[:WG1BallotMedernach|Medernach]]: same, unspecified, undecided, different
    * [[:WG1BallotShinn|Shinn]]: unspecified
    * [[:WG1BallotSnellPym|SnellPym]]: unspecified
  * '''Results:''' '''same''', unspecified, same*, undecided, different
  * '''Ratios:''' 4:2, 4:1, 4:0, 4:0
  * '''Rationales:'''

 `Gleckler`::
  Let's match R6RS since this is easy to implement. Otherwise, then since implementation differ so much on the semantics, it would better to leave this unspecified than to specify something different than R6RS.
 `Lucier`::
  *There are possibly many NaNs generated by IEEE-conforming arithmetics; for example Apple used to (and may still) use different NaNs in its arithmetic libraries to indicate which routine generated the initial NaN (which is passed along unchanged in later operations). So +nan.0 may be the textual representation of floating point numbers with different bit patterns. So I would recommend that (let ((x (/ 0. 0.))) (eqv? x x)) => #t but allow (if (and (real? x) (nan? x) (real? y) (nan? y)) (eqv? x y) #t) to return #f. In other words, two NaNs with the same bit patterns are eqv?, and we still have (eq? x y) implies (eqv? x y). This is such a small thing that I think implementors can do it without much trouble and it's time to get it right.
 `Shinn`::
  It is de facto unspecified.

=== #230 Reserve module names for current and future standards ===

Do we want to add a clause stating that all module names under the
`(scheme ...)` name are reserved for current and future standards?
Do we want to reserve the `(srfi ...)` names?

Note the name `scheme` may be changed pending the result of #237.

  * '''Options:''' scheme, srfi, both, neither, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: both, scheme, srfi
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: both, scheme, no
    * [[:WG1BallotMedernach|Medernach]]: both, scheme, undecided
    * [[:WG1BallotShinn|Shinn]]: both
    * [[:WG1BallotSnellPym|SnellPym]]: both
  * '''Results:''' '''both''', scheme, undecided, srfi, no, yes
  * '''Ratios:''' 5:0, 5:0, 5:0, 5:0, 5:1
  * '''Rationales:'''

 `Gleckler`::
  Yes, let's keep the way clear for future implementations and SRFIs. But if we don't reserve for future implementations, we shouldn't reserve for SRFIs.

=== #232 define-values ===

Several implementations provide a `define-values` macro.  This allows
cleanly writing multiple definitions with a shared state for example.

Should we have it on WG1 or not?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: yes
    * [[:WG1BallotMedernach|Medernach]]: yes
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: undecided
  * '''Results:''' '''yes''', no, undecided
  * '''Ratios:''' 5:1, 5:1
  * '''Rationales:'''

 `Ganz`::
  Avoids polluting the global environment.
 `Gleckler`::
  I don't have a strong preference for including it, but since it is useful, its definition is tiny, and it is supported by several implementations, there's no reason not to.
 `Shinn`::
  Enough with the syntactic sugar! Let people write their own, or refer to the large language for common macros.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: yes, no, undecided
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: no
  * '''Results:''' '''yes''', no, undecided
  * '''Ratios:''' 4:3, 4:0
  * '''Rationales:'''

 `Ganz`::
  The point is not whether you know in advance that something is to be eager or not. It is that one should be able to feed into a routine over promises that doesn't know or care.
 `Gleckler`::
  This is a good symmetry.
 `Medernach`::
  John's example convinced me that this has its place inside the (scheme lazy) module.
 `Shinn`::
  I'm not convinced the uses for this are common enough to add it as a procedure instead of just writing `(let ((value expression)) (delay value))`.
 `SnellPym`::
  I have no interesting use cases.

=== #235 Should bytevector constants be self-quoting? ===

They are in R6RS, apparently because they are considered more closely
related to strings rather than vectors.

Note currently vectors are not self-quoting.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: no
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: yes
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', no
  * '''Ratios:''' 4:3
  * '''Rationales:'''

 `Ganz`::
  They seem closer to vectors than strings, and in any case that is what the name implies.
 `Gleckler`::
  Let's not be different from R6RS when it's easy not to be and there aren't good reasons to be.
 `Shinn`::
  There's no reason they shouldn't be.
 `SnellPym`::
  I think vectors ought to be self-quoting, anyway. What other semantics do they have, other than their own value? There is at most an argument that records ought not to be self quoting, purely so they are useless as unquoted literals, so that they can be used as special markers for code transformers that then cannot be confused for anything in the code. Even then, though, such transformers can just create a private disjoint type and be confident they have full control of the appearance of instances of it.

=== #237 Change "scheme" in module names to "rsn" or "rs11" or something else ===

The term "scheme" is already in use in module names on some Scheme
implementations.  We need to pick something that nobody is using.

The term "rnrs" was used by R6RS, but this was integrated with the
library versioning mechanism.  It therefore may not be suitable, and
either way would cause conflicts with existing R6RS modules.

Feel free to write in a name.

  * '''Options:''' scheme, r7rs, scheme2011, undecided
  * '''Default:''' scheme
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: scheme2011
    * [[:WG1BallotGanz|Ganz]]: scheme
    * [[:WG1BallotGleckler|Gleckler]]: scheme, r7rs, undecided
    * [[:WG1BallotMedernach|Medernach]]: scheme, r7rs, undecided
    * [[:WG1BallotShinn|Shinn]]: r7rs, scheme
    * [[:WG1BallotSnellPym|SnellPym]]: r7rs
  * '''Results:''' '''scheme''', r7rs, undecided, scheme2011
  * '''Ratios:''' 3:2, 4:0, 4:1
  * '''Rationales:'''

 `Gleckler`::
  "Scheme" is just the right term. I certainly don't want scheme2011, which needlessly emphasizes the year.
 `Medernach`::
  Scheme or r7rs are the most natural and expected terms
 `Shinn`::
  "r7rs" avoids the issue that we don't have explicit versioning capabilities. When and if we have the same modules unchanged across two or more standards, we can use the "scheme" name.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotLucier|Lucier]]: yes
    * [[:WG1BallotMedernach|Medernach]]: no, undecided, yes
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''no''', yes, undecided
  * '''Ratios:''' 4:3, 4:0
  * '''Rationales:'''

 `Gleckler`::
  As John points out, some implementations already use the "#!" prefix for other purposes.
 `Shinn`::
  This conflicts with other uses of "#!".

=== #240 Rename current-second to current-tai ===

The procedure, as currently spec'd, may return a fraction of a second,
and there should be a mention of TAI in it.  `Current-tai-time` is
redundant, since the T in TAI stands for Time (or ''Temps'').

  * '''Options:''' current-second, current-tai-time, current-tai, undecided
  * '''Default:''' current-second
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: current-tai
    * [[:WG1BallotGanz|Ganz]]: current-tai
    * [[:WG1BallotGleckler|Gleckler]]: current-second, current-tai
    * [[:WG1BallotLucier|Lucier]]: current-second
    * [[:WG1BallotMedernach|Medernach]]: current-second
    * [[:WG1BallotShinn|Shinn]]: current-second
    * [[:WG1BallotSnellPym|SnellPym]]: current-tai
  * '''Results:''' '''current-second''', current-tai
  * '''Ratios:''' 4:3
  * '''Rationales:'''

 `Ganz`::
  current-tai identifies the earliest representable time in a way that current-second does not.
 `Gleckler`::
  There's no reason that `current-second' is incompatible with returning fractional second values. And we certainly don't want the redundant `current-tai-time'.
 `Medernach`::
  current-second is a good naming choice, describing that this is indeed TAI in the function description is enough.
 `SnellPym`::
  Yeah, that sounds sensible. I know somebody who goes by the name of "Tai" anyway, and he might be amused by this.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: undecided
    * [[:WG1BallotMedernach|Medernach]]: yes
    * [[:WG1BallotShinn|Shinn]]: yes
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', undecided
  * '''Ratios:''' 6:1
  * '''Rationales:'''

 `Gleckler`::
  Let's be compatible with R6RS.
 `Lucier`::
  I can't find what is meant in this issue by "the description of -0.0 from R6RS" after searching for all instances of "-0.0" in r6rs.pdf and r6rs-lib.pdf. There are many helpful examples and statements in R6RS about -0.0, but I don't know what is meant here. The description given in the second paragraph above is descriptive, but it is certainly not proscriptive; in other words, if epsilon is the smallest positive inexact number then (and (<= 0 +0.) (< +0. epsilon)) => #t but also (and (<= 0 -0.) (< -0. epsilon)) => #t So I don't know what this ticket means.
 `Medernach`::
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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: yes
    * [[:WG1BallotMedernach|Medernach]]: yes, undecided, no
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', no, undecided
  * '''Ratios:''' 6:1, 6:0, 6:0, 6:0
  * '''Rationales:'''

 `Ganz`::
  I'd leave out examples unless we can be sure of reasonably complete coverage, which may take a lot of space.
 `Gleckler`::
  We should be certainly include the first section of the Overview of Scheme from R6RS, and should include more if possible. I can help adapt the old text.
 `Shinn`::
  This is not a normative part of the report, nor do I feel does it provide a particularly good tutorial, which should be tailored to different types of students. It just presents a watered-down version of the same descriptions provided later in the same, relatively short document.
 `SnellPym`::
  This is a fine idea, and will help people who are not experienced Schemers to read the spec. This can only help us to publicise R7RS in the wider world.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: uax-29
    * [[:WG1BallotGanz|Ganz]]: uax-29
    * [[:WG1BallotGleckler|Gleckler]]: uax-29
    * [[:WG1BallotLucier|Lucier]]: unspecified
    * [[:WG1BallotMedernach|Medernach]]: uax-29
    * [[:WG1BallotShinn|Shinn]]: undecided, uax-29
    * [[:WG1BallotSnellPym|SnellPym]]: uax-29
  * '''Results:''' '''uax-29''', unspecified, undecided
  * '''Ratios:''' 6:1, 5:1
  * '''Rationales:'''

 `Shinn`::
  We can say "_should_ follow UAX-29" here.

=== #248 fill-string and fill-vector: optional start/end arguments? ===

Should we provide the obvious way to fill part of a string or vector?

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: yes
    * [[:WG1BallotMedernach|Medernach]]: yes
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''yes''', no
  * '''Ratios:''' 6:1
  * '''Rationales:'''

 `Gleckler`::
  These are easy to implement, useful, and may be faster if the implementation provides them.
 `Shinn`::
  Fill-string is an abomination, fill-vector belongs in a larger vector library.
 `SnellPym`::
  Much as I hate the imperativity of these, they should be general rather than specific.

=== #254 Behavior of open-output-file on existing files ===

Currently this is unspecified, and different implementations behave
differently.  WG2 will likely provide explicit control for this, but
we may want to specify the default behavior in WG1.

Vote `overwrite` to truncate and overwrite the existing file, or
`error` to require an error be signalled.

  * '''Options:''' overwrite, error, unspecified, undecided
  * '''Default:''' unspecified
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: unspecified
    * [[:WG1BallotGanz|Ganz]]: undecided
    * [[:WG1BallotGleckler|Gleckler]]: unspecified, overwrite
    * [[:WG1BallotMedernach|Medernach]]: unspecified
    * [[:WG1BallotShinn|Shinn]]: overwrite
    * [[:WG1BallotSnellPym|SnellPym]]: overwrite
  * '''Results:''' '''unspecified''', overwrite, undecided
  * '''Ratios:''' 3:2, 3:1
  * '''Rationales:'''

 `Ganz`::
  This seems too important to be left unspecified. The mechanism for explicit control should be in wg1.
 `Gleckler`::
  Don't break existing programs.
 `Medernach`::
  Relegate control on opening files to WG2.
 `Shinn`::
  This seems to be the most common behavior among implementations.
 `SnellPym`::
  It's the sensible and useful thing to do, IMHO.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: core
    * [[:WG1BallotGanz|Ganz]]: core
    * [[:WG1BallotGleckler|Gleckler]]: core
    * [[:WG1BallotLucier|Lucier]]: core
    * [[:WG1BallotMedernach|Medernach]]: separate
    * [[:WG1BallotShinn|Shinn]]: separate
    * [[:WG1BallotSnellPym|SnellPym]]: separate
  * '''Results:''' '''core''', separate
  * '''Ratios:''' 4:3
  * '''Rationales:'''

 `Gleckler`::
  These forms have been part of the language for a long time and are a fundamental idea.
 `Medernach`::
  Rename (scheme io) to (scheme ports) as it deals with ports management, which are not exclusively used for I/O (as strings ports for example). Don't require it into the base because some implementation don't need this.
 `Shinn`::
  The I/O system is controversial, hence the complete (though unpopular) rewrite in R6RS. We should leave room for people to experiment with alternatives without them being second-class.

=== #263 module factoring (scheme repl) ===

This is one of several issues raised by ModuleFactoringSummary (see #262).

Should `interaction-environment` be in the core, the REPL module, or
the `eval` module?

  * '''Options:''' core, eval, repl
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: repl
    * [[:WG1BallotGanz|Ganz]]: eval
    * [[:WG1BallotGleckler|Gleckler]]: repl, core
    * [[:WG1BallotMedernach|Medernach]]: repl
    * [[:WG1BallotShinn|Shinn]]: eval
    * [[:WG1BallotSnellPym|SnellPym]]: repl
  * '''Results:''' '''repl''', eval, core
  * '''Ratios:''' 4:2, 4:0
  * '''Rationales:'''

 `Ganz`::
  It's not really fundamentally tied to repl, anyway. Perhaps rename it 'implementation-environment'.
 `Gleckler`::
  Embedded implementations, for example, may want a way to avoid including that code when they aren't using it.
 `Shinn`::
  `interaction-environment` is an abstract utility, not necessarily restricted to a traditional REPL.

=== #264 module factoring (scheme case-lambda) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `case-lambda` be in the core or a separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: core
    * [[:WG1BallotGanz|Ganz]]: core
    * [[:WG1BallotGleckler|Gleckler]]: separate
    * [[:WG1BallotLucier|Lucier]]: separate
    * [[:WG1BallotMedernach|Medernach]]: separate
    * [[:WG1BallotShinn|Shinn]]: remove, separate
    * [[:WG1BallotSnellPym|SnellPym]]: core
  * '''Results:''' '''separate''', core, remove
  * '''Ratios:''' 4:3, 3:1
  * '''Rationales:'''

 `Ganz`::
  I see no reason to identify whether or not programs use this feature.
 `Gleckler`::
  `Case-lambda' is new, and some implementations may want to define their own extensions and make that clear through loading modules.
 `Medernach`::
  This one has to be in a module: it poorly handles neither optional arguments nor pattern matching. It is the kind of feature we later regret having into the core.
 `Shinn`::
  `case-lambda` is one of the worst things that has ever happened to Scheme. It requires a quadratic explosion in duplicate code to handle the optional parameters case, yet it strictly less powerful than alternatives such as `match-lambda` for more complex cases. It is only useful as a niche form for procedures whose behavior actually changes depending on the number of arguments - an uncommon and debatable type of API, which should not be encouraged.

=== #265 module factoring (scheme multiple-values) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `values` and `call-with-values` be in the core or a separate
module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: core
    * [[:WG1BallotGanz|Ganz]]: core
    * [[:WG1BallotGleckler|Gleckler]]: core
    * [[:WG1BallotLucier|Lucier]]: core
    * [[:WG1BallotMedernach|Medernach]]: core
    * [[:WG1BallotShinn|Shinn]]: separate
    * [[:WG1BallotSnellPym|SnellPym]]: core
  * '''Results:''' '''core''', separate
  * '''Ratios:''' 6:1
  * '''Rationales:'''

 `Ganz`::
  If these are in their own module, so should call-with-current-continuation be, and for that matter mutating forms. I'm not necessarily opposed to that -- it depends on what one thinks the purpose of the factoring is. But we may not want to go down that road.
 `Gleckler`::
  Multiple values have been part of the language for a long time and are a fundamental idea.
 `Shinn`::
  Multiple values are another wart in the language, which unecessarily complicate both the implementation and all user code which deals with HOFs.

=== #266 module factoring (scheme char normalization) ===

This is one of several issues raised by ModuleFactoringSummary.

Should the Unicode normalization procedures be in the core, the `char`
module, or their own separate module?

  * '''Options:''' core, char, separate
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: separate
    * [[:WG1BallotGanz|Ganz]]: char
    * [[:WG1BallotGleckler|Gleckler]]: separate, char
    * [[:WG1BallotLucier|Lucier]]: separate
    * [[:WG1BallotMedernach|Medernach]]: separate, char
    * [[:WG1BallotShinn|Shinn]]: separate, char
    * [[:WG1BallotSnellPym|SnellPym]]: char
  * '''Results:''' '''separate''', char
  * '''Ratios:''' 5:2

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: yes
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotLucier|Lucier]]: yes
    * [[:WG1BallotMedernach|Medernach]]: no
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' '''no''', yes
  * '''Ratios:''' 4:3
  * '''Rationales:'''

 `Ganz`::
  As long as we are providing the hierarchical structure, might as well use it.
 `Gleckler`::
  It's better to be explicit.
 `Shinn`::
  I've changed my mind here. We can leave aggregate modules up to WG2.

=== #268 module factoring (scheme parameter) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `make-parameter` and `parameterize` be in the core or their own
separate module?

Note `current-in/output/error-port` are parameters, though they do not
require the parameter API to be useful as is.

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: core
    * [[:WG1BallotGanz|Ganz]]: core
    * [[:WG1BallotGleckler|Gleckler]]: core
    * [[:WG1BallotLucier|Lucier]]: core
    * [[:WG1BallotMedernach|Medernach]]: separate
    * [[:WG1BallotShinn|Shinn]]: core
    * [[:WG1BallotSnellPym|SnellPym]]: core
  * '''Results:''' '''core''', separate
  * '''Ratios:''' 6:1
  * '''Rationales:'''

 `Ganz`::
  Comments regarding #265 apply.
 `Gleckler`::
  They're so simple and few in number (two) that there's no reason to separate them.
 `Medernach`::
  These functions allow creation and management of dynamic bindings, let put these in a module so people wanting them know what they are doing. (Moreover as stated above, `current-in/output/error-port` have their own API)
 `Shinn`::
  This is intertwined with the core language.

=== #269 module factoring (scheme record) ===

This is one of several issues raised by ModuleFactoringSummary.

Should `define-record-type` be in the core or in its own separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: core
    * [[:WG1BallotGanz|Ganz]]: core
    * [[:WG1BallotGleckler|Gleckler]]: separate
    * [[:WG1BallotLucier|Lucier]]: core
    * [[:WG1BallotMedernach|Medernach]]: separate
    * [[:WG1BallotShinn|Shinn]]: separate
    * [[:WG1BallotSnellPym|SnellPym]]: core
  * '''Results:''' '''core''', separate
  * '''Ratios:''' 4:3
  * '''Rationales:'''

 `Gleckler`::
  Users may want to use more elaborate versions of `define-record-type' and make that clear through loading modules.
 `Medernach`::
  As this is a sensible issue, I would prefer not to write SRFI-9 in stone into the core but let open the (scheme record *) namespace for all kind of record implementations (in WG2 maybe).
 `Shinn`::
  No one thinks SRFI-9 is the one true record system, just that it's the only one we can agree on. We should leave room for other record systems, and not give this one special status.

=== #270 module factoring (scheme char) ===

This is one of several issues raised by ModuleFactoringSummary.

Should the Unicode character case and property utilities be in the
core or their own separate module?

  * '''Options:''' core, separate
  * '''Default:''' core
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: separate
    * [[:WG1BallotGanz|Ganz]]: char
    * [[:WG1BallotGleckler|Gleckler]]: separate
    * [[:WG1BallotLucier|Lucier]]: separate
    * [[:WG1BallotMedernach|Medernach]]: separate
    * [[:WG1BallotShinn|Shinn]]: separate
    * [[:WG1BallotSnellPym|SnellPym]]: separate
  * '''Results:''' '''separate''', char
  * '''Ratios:''' 6:1
  * '''Rationales:'''

 `Gleckler`::
  This is new.
 `Medernach`::
  Separate and optional as not all Scheme implementations will support Unicode.
 `Shinn`::
  These require pretty large tables to load into a small implementation.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: 2011
    * [[:WG1BallotGanz|Ganz]]: r7rs
    * [[:WG1BallotGleckler|Gleckler]]: r7rs, undecided
    * [[:WG1BallotLucier|Lucier]]: 2011, undecided
    * [[:WG1BallotMedernach|Medernach]]: r7rs
    * [[:WG1BallotShinn|Shinn]]: r7rs
    * [[:WG1BallotSnellPym|SnellPym]]: r7rs
  * '''Results:''' '''r7rs''', undecided, 2011
  * '''Ratios:''' 5:1, 5:0
  * '''Rationales:'''

 `Gleckler`::
  Changing the naming convention after all these years is bikeshedding and dropping a fun and respected tradition. Furthermore, we'll have to explain the break over and over again.
 `Medernach`::
  Putting the date is a bad idea IMHO, it wrongly makes the language look deprecated some years after. Just name it something like "Report on the Core Scheme Language" with a subtitle like "2011 edition, 7th revision" (not directly a part of the title).
 `Shinn`::
  I think changing the name at this point would give the misleading impression that the new report somehow deviates from tradition even more so than R6RS.

=== #189 List changes from R6RS ===

An incomplete list of the differences between this language and the
R6RS is available.  Do we want to include this directly into the
document?  Alternately it can go into a separate document, or be
included in the WG2 document.

  * '''Options:''' yes, no, undecided
  * '''Default:''' no
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: yes
    * [[:WG1BallotGanz|Ganz]]: no
    * [[:WG1BallotGleckler|Gleckler]]: yes
    * [[:WG1BallotLucier|Lucier]]: separate
    * [[:WG1BallotMedernach|Medernach]]: undecided
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: yes
  * '''Results:''' ''yes'', no, separate, undecided
  * '''Ratios:''' 3:2, 3:1, 3:1
  * '''Rationales:'''

 `Ganz`::
  Separate document.
 `Gleckler`::
  Let's include this directly in the document. It's important to explain this clearly.
 `Medernach`::
  As the language is now splitted in "little WG1" and "big WG2" languages, does it really make sense to compare WG1 to R6RS rather than WG2 ? Comparing WG1 to R5RS is a better idea IMHO.
 `Shinn`::
  This would be too long, and more properly belongs in WG2.
 `SnellPym`::
  It would make an interesting appendix, and allow us to rationalise what might otherwise seem arbitrary decisions to people who haven't followed the exciting history of the mailing list.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: note
    * [[:WG1BallotGanz|Ganz]]: nothing
    * [[:WG1BallotGleckler|Gleckler]]: note
    * [[:WG1BallotLucier|Lucier]]: note
    * [[:WG1BallotMedernach|Medernach]]: note
    * [[:WG1BallotShinn|Shinn]]: nothing
    * [[:WG1BallotSnellPym|SnellPym]]: note
  * '''Results:''' '''note''', nothing
  * '''Ratios:''' 5:2
  * '''Rationales:'''

 `Ganz`::
  I'm not so sure this is an error. Quoted objects expand to code, but quasiquote operates on a representation of data (with embedded code). If we're not prepared to define reasonable semantics, we should say nothing.
 `Gleckler`::
  If it's confusing, it's better to be explicit.
 `Shinn`::
  If we include error examples for this case, we should include one for quasiquote.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: semi-colon, brace
    * [[:WG1BallotGanz|Ganz]]: semi-colon
    * [[:WG1BallotGleckler|Gleckler]]: semi-colon, undecided
    * [[:WG1BallotMedernach|Medernach]]: brace, semi-colon
    * [[:WG1BallotShinn|Shinn]]: brace
    * [[:WG1BallotSnellPym|SnellPym]]: brace-or-semi-colon
  * '''Results:''' '''semi-colon''', brace, brace-or-semi-colon, undecided
  * '''Ratios:''' 3:2, 4:1, 4:0
  * '''Rationales:'''

 `Ganz`::
  This is too minor an issue to bring in braces.
 `Gleckler`::
  Syntax highlighters can be updated.
 `Shinn`::
  This is unambiguous and doesn't conflict with existing syntax.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: r6rs, prefix-dotted, dotted, prefix, short
    * [[:WG1BallotGanz|Ganz]]: r6rs
    * [[:WG1BallotGleckler|Gleckler]]: r6rs
    * [[:WG1BallotLucier|Lucier]]: r6rs, r6rs, r6rs, ...
    * [[:WG1BallotMedernach|Medernach]]: r6rs
    * [[:WG1BallotShinn|Shinn]]: dotted, r6rs
    * [[:WG1BallotSnellPym|SnellPym]]: dotted, prefix-dotted, short, prefix
  * '''Results:''' '''r6rs''', dotted, prefix-dotted, prefix, short, ...
  * '''Ratios:''' 5:2, 6:1, 6:1, 6:1, 6:0
  * '''Rationales:'''

 `Gleckler`::
  Ugh. Let's not be gratuitously incompatible with R6RS just because the BNF is cumbersome.
 `Lucier`::
  I presume that the short and dotted notation was meant to be I strongly feel that any notation that incorporates unadorned "0" (not "+0." or "-0.") is incredibly misleading, as (/ 1 0) => Error (/ 1.0 0) => Error etc. The R6RS notation is sub-optimal in my opinion, as it does not indicate the bit patterns in a NaN either on input or output, but it seems better than the alternatives.
 `Shinn`::
  This fits well with our definition of symbol syntax, and keeps the BNF simple and free of exceptions.
 `SnellPym`::
  I reckon the dot is important (they're inexact), and forcing a numeric prefix will simplify the rules for symbols in a pleasing manner.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: long, no
    * [[:WG1BallotGleckler|Gleckler]]: no, bang-long, undecided
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: long, no, undecided, bang-long
    * [[:WG1BallotShinn|Shinn]]: long, no
    * [[:WG1BallotSnellPym|SnellPym]]: long, no, bang-long
  * '''Results:''' '''long''', no, bang-long, undecided
  * '''Ratios:''' 4:3, 4:1, 4:1
  * '''Rationales:'''

 `Gleckler`::
  Let's not do this. They've been removed. I don't have trouble distinguishing #t from #f. Anyone who does can define `true' and `false'.
 `Medernach`::
  Alex convinced me that this is more readable (along with keepeing shortened forms for compatibility).
 `Shinn`::
  Most languages use `true` and `false` spelled out, and `#t` and `#f` are maddenlingly difficult to distinguish.
 `SnellPym`::
  `#!...` is unnecessarily verbose and clashes with using `#!` for reader directives. But I like the longer names.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: no
    * [[:WG1BallotGanz|Ganz]]: no
    * [[:WG1BallotGleckler|Gleckler]]: no
    * [[:WG1BallotLucier|Lucier]]: no
    * [[:WG1BallotMedernach|Medernach]]: no
    * [[:WG1BallotShinn|Shinn]]: no
    * [[:WG1BallotSnellPym|SnellPym]]: no
  * '''Results:''' '''no'''
  * '''Ratios:''' 
  * '''Rationales:'''

 `Gleckler`::
  They don't need to be in the standard if no one uses them.
 `SnellPym`::
  Let's be jewel-like.

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
  * '''Voters:''' 
    * [[:WG1BallotCowan|Cowan]]: r5rs
    * [[:WG1BallotGanz|Ganz]]: r6rs
    * [[:WG1BallotGleckler|Gleckler]]: r5rs
    * [[:WG1BallotLucier|Lucier]]: r5rs
    * [[:WG1BallotMedernach|Medernach]]: r6rs, undecided, r5rs
    * [[:WG1BallotShinn|Shinn]]: r5rs
    * [[:WG1BallotSnellPym|SnellPym]]: r6rs
  * '''Results:''' '''r5rs''', r6rs, undecided
  * '''Ratios:''' 4:3, 4:1
  * '''Rationales:'''

 `Ganz`::
  I like the idea of returning zero values.
 `Gleckler`::
  Let's not break existing programs if there isn't widespread agreement.
 `Medernach`::
  R6RS phrasing allows flexibility. With it implementations are free to return any useful value or to return no values instead of an unfortunate "undefined" value.
 `Shinn`::
  This is the de facto standard. Although R6RS loosened the requirements, existing implementations continue to return a single value.
 `SnellPym`::
  I've made my views on this clear before!
