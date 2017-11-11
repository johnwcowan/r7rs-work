# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

# WG1 Ballot Items To Finalize By Mar. 31

## WG1 - Core

### #229 Are NaN values EQV?

We voted that `eqv?` return `#t` if both arguments are any value which
writes as `+nan.0`.  The description of this item was ill-formed and
confusing, as objected to in:

http://lists.scheme-reports.org/pipermail/scheme-reports/2011-September/001507.html

We therefore are re-opening the item, with amended descriptions.

The `different` proposal is that we add a single clause requiring
`(eqv? +nan.0 x)` to return `#f` for any `x`.  This is the behavior
that results for any R5RS implementation that adds support for +nan.0
as an IEEE float without any special handling for it in `eqv?`.

The `unspecified` proposal is to make the results explicitly unspecified,
as specified in R6RS.

The `same` proposal, contrary to both standards, is that we add a clause to
the definition of `eqv?` saying that if both arguments are NaN
values with the same bit pattern, `eqv?` must return `#t`.  Thus `eq?`
implies `eqv?`.  However, if two values both print as `+nan.0` they
may or may not be `eqv?`.  This also requires additional checks for
floating point comparisons.

Testing with `(equal? (/ 0.0 0.0) (/ 0.0 0.0))` to get the same
bit pattern but non-object-identity, we get the following results:

The following 8 implementations return #t: Chez, Gambit, Guile, Ikarus/Vicare, Kawa, Larceny, Racket, STklos.

The following 6 implementations return #f: Bigloo, Chibi, Chicken, Gauche, MIT Scheme, Scheme48.

[SigScheme](SigScheme.md) and Scheme 9 don't have `+nan.0`. SISC currently has a bug
where `(= nan.0 x)` is true for any `x`.


* **Options:** same, different, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Ganz](WG1BallotGanz.md): undecided
* [Gleckler](WG1BallotGleckler.md): same, unspecified
* [Hsu](WG1BallotHsu.md): unspecified
* [Lucier](WG1BallotLucier.md): unspecified
* [Medernach](WG1BallotMedernach.md): same, unspecified
* [Shinn](WG1BallotShinn.md): unspecified, different
* [SnellPym](WG1BallotSnellPym.md): different, unspecified, same
* **Results:** **unspecified**, same, different, undecided
* **Ratios:** 5:2, 6:1, 7:1
* **Rationales:**

`Gleckler`::
> I don't see any reason to differ with R6RS here. It's easy for implementations that follow the "different" option to switch to supporting the "same" option. Furthermore, Bradley Lucier and Will Clinger appear to have thought about it a lot and have come to the same conclusion, so I'm more confident.
`Lucier`::
> I believe that I now understand what this issue is about. First of all, Gambit might return #f or #t on a PowerPC system, depending on how the NaN is computed; I suspect the same might be true for other Schemes. Depending on the computer system, many values that print as +nan.0 may have different bit patterns. So, if (number->string x) => "+nan.0" and (number->string y) =? "+nan.0" and the bit patterns of x and y differ, then I believe that (eqv? x y) => #f On the other hand, eqv? is supposed to be an equivalence relation, so it is reflexive, so (let* ((x (/ 0. 0.)) (y x)) (eqv? x y)) => #t and both x and y satisfy (number->string x) => +nan.0 (number->string y) => +nan.0 So I believe that unspecified is the best option.
`Medernach`::
> [NaNs](NaNs.md) are provided as a diagnostic tool to help understand errors in number computation. As such we need to have a way to distinguish different [NaNs](NaNs.md), therefore the 'same' vote.
`Shinn`::
> We need to handle this directly in the definition of eqv?. As the current definition stands, if specified at all it must be different.

### #275 Support -nan.0 as a synonym for +nan.0

Excluding `-nan.0` was an oversight, and it's gratuitously
incompatible with R6RS as well as current practice.  Racket, Gauche,
Chicken, Guile, Chez, Ikarus, Larceny, Ypsilon, STklos all support
`+nan.0` and `-nan.0` as equivalent forms.  MIT, Bigloo, Scheme48/scsh,
SISC, SCM, Scheme 9 don't support either form.  Only Gambit and Chibi
support `+nan.0` but not `-nan.0`.

STklos prints both `+nan.0` and `-nan.0` as `-nan.0`.

Vote `yes` to allow `-nan.0`, `no` to disallow it.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> If `-nan.0` is not a number, it's an identifier -- but almost all Schemes that handle `+nan.0` at all treat `-nan.0` as a number. We don't want to demand that they allow code treating it as an identifier. Nobody needs to remember -nan.0 except not to use it as an identifier.
`Gleckler`::
> I agree with Alex. There's no point in having this extra identifier. It has no meaning. If the option were option, I'd rather that we replaced "+nan.0" (and "-nan.0") with the identifier "NaN" now that we're case-sensitive. It would be unlikely to conflict with existing code, and it wouldn't be quite as ugly.
`Lucier`::
> Eventually, some Scheme should allow one to see all the bits of a NaN (as this is true for all other floating-point numbers). This is a good step in that direction.
`Medernach`::
> The sign bit of `NaNs` is meaningless in the IEEE Standard 754 floating-point formats, but in order to avoid using it as an identifier we may consider both as synonymous. However I really would prefer using `NaN` instead of `+nan.0` or `-nan.0` (as it is not a number but an indication of failure, to help diagnostic, it is neither signed in general, nor exact nor inexact, isn't it ?)
`Shinn`::
> I don't understand the motivation behind this. NaN can neither be positive nor negative - the "+" is just a hint that the value is numeric (which is no longer true with the symbol extensions that now allow an initial "+"). Thus "-" serves no purpose and does not simplify the syntax - if we allow multiple forms, do we also allow "nan.0", "+nan.", "+nan", "+nan.1", etc.? One is easier to remember than two.

### #278 Shrink division routines to just truncate and floor

Bradley Lucier says:

I don't see the `centered-*` operators as somehow a "completion" of
the other division operators.  In the small language I'd recommend
only the `truncate-*` and `floor-*` operators for two reasons: they
are the only division operators that have an established history of
use in computer programming and mathematics, and they form a minimal
extension of R5RS.  (I'm not saying that the other division operators
have never been used in mathematics or programming (see CL), but small
Scheme is not supposed to be a kitchen-sink language.)

Vote `shrink` to prune to `truncate-*` (R5RS) and `floor-*` (R5RS `modulo`), moving
the extra operators to the large language; `shrink/core` to do the same as `shrink`
but move the remaining operators to the core language; or `keep` to keep all 18
division operators in the small language.

* **Options:** shrink, shrink/core, keep, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): shrink/core, shrink
* [Ganz](WG1BallotGanz.md): no-centered, keep, shrink/core, shrink
* [Gleckler](WG1BallotGleckler.md): keep, shrink/core, shrink
* [Hsu](WG1BallotHsu.md): keep, undecided, shrink/core, shrink
* [Lucier](WG1BallotLucier.md): shrink/core, shrink
* [Medernach](WG1BallotMedernach.md): shrink/core, shrink, undecided, keep
* [Shinn](WG1BallotShinn.md): shrink/core, shrink
* [SnellPym](WG1BallotSnellPym.md): shrink/core, shrink, keep
* **Results:** **shrink/core**, shrink, keep, undecided, no-centered
* **Ratios:** 8:0, 5:3, 7:1, 7:1
* **Rationales:**

`Cowan`::
> I think enough justification exists for each of the six operations. I agree that if we do shrink, we should get rid of the division module.
`Gleckler`::
> I see no new evidence justifying a change from our initial vote. I encourage people to read the cited paper, "The Euclidean definition of the functions div and mod," <http://dl.acm.org/citation.cfm?id=128862>. Here's an excerpt: Indeed, the functions div and mod are very important concepts in discrete mathematics for certain problems in number theory, in computer science for reasoning about number representation systems, in communications engineering for a variety of issues ranging from coding to sampling and multiplexing, and so on. Hence it is unfortunate that the definition of these functions appears to be handled rather casually in the computer science literature and in the design of programming languages, as one might infer from various poor "definition[...](WG1BallotGleckler.md)
`Lucier`::
> I've read Taylor Campbell's reply and I don't find it compelling. To paraphrase one of his arguments somewhat, I don't find "If we have floor-divide we need ceiling-divide" compelling---we've gotten along reasonably well without either of them so far, but we could get floor-divide from R5RS quotient and modulo from (define (floor-divide x y) (quotient (- x (modulo x y)) y) Module is an important operation, there should be a division operator associated with it. I can't say that the other non-R5RS remainder or division operators are "important". I do believe that R7RS small scheme should just complete the three division/remainder operators in R5RS with floor-divide (or whatever the name should be ) and leave the rest to R7RS big scheme.
`Medernach`::
> Relegate additional operators to the large language and put truncate-* and floor-* back to the core.
`Shinn`::
> I agree completely. The operators need more use and individual rationales before they are promoted to the small language. I'm also voting shrink/core to suggest we put them back in the core language, iff we shrink.

### #280 Make vectors self-quoting

Currently vectors are the only type represented by a readable datum
that are neither self-quoting nor meaningful Scheme expressions
(i.e. symbols and lists).  The proposal is to make them
self-quoting as well.

Currently Racket, Gauche, MIT, Guile, Kawa, Chibi, SCM, STklos, Scheme
9, Scheme 7, UMB, VX, Oaklisp treat vectors as self-quoting.

Gambit, Chicken, Bigloo, Scheme48/scsh, SISC, Ikarus, Larceny,
Ypsilon, IronScheme, Mosh, KSi, SigScheme, Elk treat unquoted
vectors as errors.

Vote `yes` to make them self-quoting, `no` to make it an explicit
error, or `unspecified` to leave unspecified as in R5RS.

The only other reasonable alternative semantics for this unspecified
case would be to treat #(...) as (vector ...) (i.e. in contrast to this proposal
to evaluate the contents rather than quoting them).  No known
implementations make this extension, and it is dubious due to the
fact that it makes what appears to be quoted data to be evaluated,
and so is not listed as an option.  The possibility of this extension,
however, could serve as an argument to leave it unspecified.

* **Options:** yes, no, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes, no
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes, unspecified
* [Hsu](WG1BallotHsu.md): undecided, unspecified, yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no, unspecified
* **Results:** **yes**, no, unspecified, undecided
* **Ratios:** 7:1, 6:1, 6:1
* **Rationales:**

`Cowan`::
> I think this will be awkward for the R6RS implementations, since R6RS requires an error to be signalled. Other than that, I'm for this and always have been. I proposed it during the R6RS formal-comment process, and it was rejected with "Generally, Scheme has often favored uniformity over succinctness". Still, what's done is done. I'm still voting "yes".
`Gleckler`::
> Alex assured us that this wouldn't make things like `#(1 ,(+ 2 3)) fail, so I'm voting yes.
`Hsu`::
> I am unsure whether I am willing to say that vectors are inherently not meaningful Scheme expressions. I can imagine an extension that allows one to say something like `#(5 (+ 1 2)) ; => '#(5 3)`. Implicit quoting would disallow this extension and others.
`Medernach`::
> What about square brackets syntax ? This is natural notation for vectors.
`Shinn`::
> This is fairly widely supported and is the only reasonable semantics for unquoted vectors.

### #282 Map and friends should call their procedures in the same dynamic environment

The specifications of `map`, `for-each`, and other procedures that
accept a procedure as an argument and call it, should specify that the
argument procedures will always be called in the dynamic environment
of the call to `map`, `for-each`, etc.

This is an R6RS fix.

Vote `yes` to add the clarification and `no` to leave it out.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): undecided, no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 6:2, 6:1
* **Rationales:**

`Gleckler`::
> This is too obvious to be worth specifying. Furthermore, if we specify it here, what are we implying about other procedures like this?
`Hsu`::
> This is a good clarification and not harmful.
`Shinn`::
> That the semantics are desired is obvious, but I don't see how they could be interpreted otherwise and I'm not convinced it's worth writing this.

### #283 Initial characters in non-ASCII identifiers should exclude digits and combiners

Identifiers beginning with a character of type Nd, Mc, or Me should be
forbidden.  This is an R6RS issue.

Nd is a numeric character, which in the case of ASCII 0-9 is already
forbidden, but currently unspecified for non-ASCII digits.

Mc and Me are enclosing marks and spacing combining marks respectively, which are logically attached to the preceding character.

Vote `yes` to forbid (which would still allow this as an
implementation-dependent extension for either numbers or symbols).

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided, yes
* [Lucier](WG1BallotLucier.md): undecided, yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **yes**, undecided, no
* **Ratios:** 6:2, 7:1
* **Rationales:**

`Gleckler`::
> This is an obvious choice for consistency of implementations that support Unicode.
`Hsu`::
> I do not know enough about how this will affect things to say for sure.
`Shinn`::
> This makes sense - such identifiers would look like numbers which would be misleading.
`SnellPym`::
> I still feel that any character should be legal in a symbol, if not necessarily possible to represent without quoting, and that any symbol should be legal as an identifier!

### #285 R6RS base compatibility: symbol=?

This is equivalent to `eq?` on symbols, and provides R6RS base
compatibility as well as completing the set of type-specific
comparisons.  See also #316.

Vote `yes` to add this procedure.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> This helps with type-inference, although in practice the large language will want to provide some form of `identifier=?`.
`Ganz`::
> I agree with Arthur's comment on uninterred symbols
`Gleckler`::
> I agree that this is needed to complete the set of type-specific comparisons. Since the standard specifically mentions the possibility of uninterned symbols, the description of `symbol=?` should say something about what it means in implementations with uninterned symbols. It would probably be best to say that its behavior is unspecified when either argument is an uninterned symbol. Normally, we wouldn't have to say anything about an extension to the language, but since we already talk about this extension, it's justified.
`Lucier`::
> We need this why?
`Medernach`::
> I feel that non composed types doesn't need type-specific comparisons functions as we have generic 'eq?' or 'eqv?'. This looks unnecessary to provide a trivial composition of `symbol?` and `eq?`.
`Shinn`::
> This helps with type-inference, although in practice the large language will want to provide some form of `identifier=?`.
`SnellPym`::
> Consistency is good.

### #286 Numeric *-valued procedures for R5RS and R6RS-base compatibility

`Real-valued?`, `rational-valued?`, and `integer-valued?` test whether
a given number object can be coerced to the specified type without
loss of numerical accuracy.  They are equivalent to the versions of
`real?`, `rational?`, and `integer?` that exist in R5RS.

Specifically, the behavior of these predicates differs from the
behavior of `real?`, `rational?`, and `integer?` on complex number
objects whose imaginary part is inexact zero.

These procedures provide R6RS base compatibility as well.

* Vote `yes` to add `*-valued` procedures;
* Vote `no` to leave out the `*-valued` procedures;
* Vote `r5rs` to leave them out *and* revert `real?`, `rational?`, and `integer?` to R5RS semantics
* vote `r5rs+strictly` to do what `r5rs` does, and also add `strictly-*?` procedures to provide the R6RS semantics of `real?`, `rational?`, and `integer?`.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): r5rs
* [Ganz](WG1BallotGanz.md): r5rs
* [Gleckler](WG1BallotGleckler.md): r5rs, no, r5rs+strictly
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): r5rs+strictly, r5rs, yes, undecided
* [Shinn](WG1BallotShinn.md): no, undecided
* [SnellPym](WG1BallotSnellPym.md): r5rs+strictly, r5rs, yes, no
* **Results:** **r5rs**, no, r5rs+strictly, yes, undecided
* **Ratios:** 5:2, 3:2, 5:1, 5:1
* **Rationales:**

`Cowan`::
> It's inconsistent to vote for the R6RS-base library without providing these. In addition, the R5RS library can and should export them as real, rational, and complex. This is one of the places where we made a silent change to the semantics of a procedure (silent in the sense that code will behave differently without any warning), and there should be an easy way to recover the old semantics.
`Ganz`::
> The example given is too narrow to support both sets of predicates.
`Gleckler`::
> These names are awful. I'll never be able to remember that `real-valued?' means something different than `real?', and even if I do, I won't remember which one is which. I'm sure others will have the same problem. If we come up with better names, I might be willing to vote yes. After John's edit: The "strictly-*" names don't make things any less confusing, so I'm voting to revert to r5rs or at least to leave out the new names.
`Shinn`::
> If nothing else the names are too confusing - the difference is too small, and I don't think people will be able to keep these straight.

### #287 R6RS base compatibility: assert

`Assert` raises an error if its argument is `#f`.  This provides R6RS
base compatibility.

Vote `basic` to add this syntax.  Vote `optionals` to make `assert` optionally accept, after its
expression argument, a single `message` argument and zero or more `irritant` arguments
in the same manner as the `error` procedure.  Vote `no` in order not to add `assert`.

* **Options:** basic, optionals, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): optionals/library, no, optionals
* [Gleckler](WG1BallotGleckler.md): optionals, no
* [Hsu](WG1BallotHsu.md): optionals, basic
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): optionals, basic, no
* **Results:** **no**, optionals, basic, optionals/library
* **Ratios:** 4:3, 5:2, 5:1
* **Rationales:**

`Cowan`::
> It's inconsistent to vote for r6rs-base without adding this. People who don't like it are free to import the base library without it and define their own.
`Gleckler`::
> We shouldn't include `assert' without making it at least equal to `error' in its ability to describe a problem. If we can't do that, we should wait for implementations to come to agreement rather than specifying something anemic.
`Hsu`::
> Assert is very useful, but much less so without message arguments.
`Medernach`::
> However I agree that 'assert' is popular, it is superfluous in the WG1 language because it adds little to 'error', and WG2 may come with something more powerful like contracts (a la Eiffel as in "Contracts for Higher-Order Functions" and as provided by Racket).
`Shinn`::
> There are numerous assert macros, and it's not clear which is best - in particular, the best ones allow friendlier reporting. This isn't ready for the small language.
`SnellPym`::
> I vote for the optionals as I think it should provide all the functionality of the inner `error`.

### #288 R6RS base compatibility: infinite?

`Infinite?` returns `#t` if its value is a real number, or if its
value is a complex number and either the real or the imaginary part
would return `#t` to `infinite?`.  This provides R6RS base
compatibility, with extensions for complex numbers analogous to that
provided by `finite?` and `nan?`.

This was in the draft at one point, but was never actually voted on,
so the editors removed it.

Vote `yes` to add this procedure.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 8:0
* **Rationales:**

`Gleckler`::
> Sounds reasonable.
`Shinn`::
> I more often want to check `infinite?` than `finite?`.

## WG1 - Numerics

### #290 Proposed square procedure

Bradley Lucier writes (lightly edited):

A `square` primitive is useful in calculating with bignums because
squaring a bignum is generally cheaper than multiplying two different
bignums of the same size. For example, Gambit's runtime checks
trivially whether the two arguments in `(* a b)` are `eq?` before
calling the appropriate algorithm.  Generally, it may be better to be
able to express this primitive directly.

[#He|also points out that given `square` in the small language, we can
have `flsquare` in the large language, though having the
latter doesn't actually require having the former.]]

In addition, there are 20,340 Google hits for [(square x)" ss|scm]("(define).

Vote `yes` to add this procedure.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): undecided
* **Results:** **yes**, undecided
* **Ratios:** 7:1
* **Rationales:**

`Cowan`::
> On the one hand, `(expt x 2)` is the same thing, and should be just as easy to optimize. On the other, we provide `sqrt` as an alias for `(expt x 1/2)` and it seems odd not to have this inverse.
`Gleckler`::
> I buy the argument from symmetry with `sqrt'.
`Shinn`::
> On the one hand, `(expt x 2)` is the same thing, and should be just as easy to optimize. On the other, we provide `sqrt` as an alias for `(expt x 1/2)` and it seems odd not to have this inverse.

## WG1 - Core

### #291 Require an error to be signalled if input files cannot be opened

For `with-input-from-file`, `with-output-to-file`,
`call-with-input-file`, `call-with-output-file`, R5RS just says that
the file should exist.  However, `open-input-file` requires an error
to be signalled if the file cannot be opened, whether because it does
not exist for some other reason like the lack of permissions.  This
inconsistency doesn't seem useful.

The proposal is to change these wrapper procedures to also require an error
to be signalled if the file cannot be opened.  All major Schemes
already implement this.

Vote `yes` to require signalling an error if the files cannot be opened.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 8:0
* **Rationales:**

`Gleckler`::
> Yes, this error shouldn't happen silently. Since implementations already signal it, this change won't be a problem.
`Shinn`::
> I think R5RS implies this behavior, but it's worth making explicit.
`SnellPym`::
> Forcing an explicit error signal means that portable code can know how to handle this error case by catching the signalled condition.

### #292 Add case-insensitive normalization-insensitive comparisons

mdmkolbe writes on Slashdot:

Given that on a system with Unicode, you almost never want to do a
non-normalizing case-insensitive match and that it is hard for a user
to efficiently implement their own normalizing case-insensitive match,
it seems an odd corner of the rectangle to omit.

(end quotation)

Alternatively we could specify that `-ci` procedures always normalize,
or that `-ni` procedures are always case-insensitive, since the
details of the normalization are not exposed anyway.

* **Proposals:**
* **normalize-ci:** specify that *-ni procedures normalize their arguments
* **case-fold-ni:** specify that *-ni procedures case-fold their arguments
* **ci-ni:** add new *-ci-ni procedures that perform both operations
* **none:** leave as-is, although *-ni may still fold
* **remove:** remove the *-ni procedures altogether
* **remove+normalize-ci:** remove *-ni procedures, allow *-ci procedures to normalize
* **Options:** normalize-ci, case-fold-ni, ci-ni, remove, none
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): remove+normalize-ci, remove
* [Ganz](WG1BallotGanz.md): remove+normalize-ci, ci-ni, remove
* [Gleckler](WG1BallotGleckler.md): remove, ci-ni, no, case-fold-ni
* [Medernach](WG1BallotMedernach.md): remove, undecided
* [Shinn](WG1BallotShinn.md): remove, no, ci-ni, case-fold-ni
* [SnellPym](WG1BallotSnellPym.md): normalize-ci, ci-ni, no, remove+normalize-ci, remove, case-fold-ni
* **Results:** **remove**, ci-ni, no, remove+normalize-ci, case-fold-ni, undecided, normalize-ci
* **Ratios:** 4:2, 5:1, 3:3, 6:0, 6:0, 5:1
* **Rationales:**

`Cowan`::
> Alex says: "I think this point suggests that the normalization API doesn't compose well, and needs to be thought out better. I'd rather remove it and provide something more powerful in the large language." I agree, though I'd like to allow *-ci to do implementation-defined normalization, since it basically exists to do human-friendly matching.
`Gleckler`::
> I don't feel confident about Unicode decisions, so my inclination is to remove these procedures if there's any disagreement. If we don't remove them, we should complete the rectangle.
`Shinn`::
> I think this point suggests that the normalization API doesn't compose well, and needs to be thought out better. I'd rather remove it and provide something more powerful in the large language. Otherwise, we should leave it as is.
`SnellPym`::
> I'm not really sure when I'd want to do a NON-normalising string comparison, to be honest, as it would tend to reflect spurious differences due to the details of Unicode encodings. Whether it's case sensitive or not is the only choice I'd like to make.

### #293 Make it an error for <test> values to return other than one value

Currently nothing is said about the <test> of `if`, `cond`, `and`,
`or`, etc. returning zero values or multiple values.  The proposal is
to make this an explicit error.  Remember that this does not mean an error is
*signalled*.

Vote `yes` to make an explicit error.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **no**, yes
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> As the standard already says, "Except for continuations created by the `call-with-values` procedure [...] all continuations take exactly one value. The effect of passing no value or more than one value to continuations that were not created by call-with-values is unspecified." Repeating this information for every continuation that could not be created by `call-with-values` is redundant.
`Gleckler`::
> The standard already makes this clear in a general way. There's no way to repeat that information for conditionals in particular.
`Medernach`::
> This has to be unspecified because many implementations differ on this issue.
`Shinn`::
> As the standard already says, "Except for continuations created by the `call-with-values` procedure [...] all continuations take exactly one value. The effect of passing no value or more than one value to continuations that were not created by call-with-values is unspecified." Repeating this information for every continuation that could not be created by `call-with-values` is redundant.
`SnellPym`::
> I'd like it to be explicitly legal, and that the first value is taken; and this should be reflected consistently everywhere N values are expected, that any extra values are ignored and it's an error only to have less than N.

### #294 Make it an error for the <expression> of a set! to return other than one value

Currently nothing is said about what happens if the <expression> of a
`set!` returns zero values or multiple values.  The proposal is to make this
an explicit error.  Remember that this does not mean an error is
*signalled*.

Vote `yes` to make an explicit error.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **no**, yes
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> Same argument as for #293 above.
`Gleckler`::
> Same argument as for #293: this is already clear, and repeating it would be redundant.
`Shinn`::
> Same argument as for #293 above.
`SnellPym`::
> Same rationale

### #295 Make it an error for <init>s in binding forms to return other than one value

Right now nothing is said.  The proposal is to make this
an explicit error.  Remember that this does not mean an error is
*signalled*.

Vote `yes` to make an explicit error.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **no**, yes
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> Same argument as for #293 above.
`Gleckler`::
> Same argument as for #293: this is already clear, and repeating it would be redundant.
`Hsu`::
> This should be tied to our definition of what happens when multiple values are sent to a single valued context. Specifically, there should be no difference in semantics between the lambda transformation of `let` and normal `let`.
`Shinn`::
> Same argument as for #293 above.
`SnellPym`::
> Likewise

### #297 Removing case-folding flags

The case-folding flags `#!fold-case` and `#!no-fold-case` are the only
reader flags in the draft, however their need is reduced (though not
eliminated) by the library declaration `include-ci`.  Do we still need
flipflop flags to turn case-folding on and off in part of a file?

If we remove these we maintain backwards compatibility with R5RS
library code, however we lose the ability to support R5RS programs or
toggle case-folding in the REPL or data files, etc.

* **Options:** keep, remove, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): keep
* [Ganz](WG1BallotGanz.md): keep
* [Gleckler](WG1BallotGleckler.md): keep
* [Hsu](WG1BallotHsu.md): keep
* [Medernach](WG1BallotMedernach.md): keep
* [Shinn](WG1BallotShinn.md): keep
* [SnellPym](WG1BallotSnellPym.md): remove, keep
* **Results:** **keep**, remove
* **Ratios:** 6:1
* **Rationales:**

`Cowan`::
> We could only remove these in conjunction with a proposal that allows toggling case-folding in the REPL.
`Gleckler`::
> I'm opposed to making Scheme case sensitive, but have lost that argument. However, even R6RS supported these flags at least optionally, and it shouldn't be necessary to construct a module just to load old code that depends on case sensitivity.
`Hsu`::
> These are too convenient to remove and not all cases are adequately handled by `include-ci`.
`Shinn`::
> We could only remove these in conjunction with a proposal that allows toggling case-folding in the REPL.
`SnellPym`::
> I've never liked these - they just seem inelegant.

### #303 "lazy" is a confusing name

[on feedback from Marc Feeley.](Based)

`delay` and `force` were simple balanced concepts, but the
introduction of `lazy` somewhat confuses the issue - when is `delay`
appropriate and when is `lazy`?  A simple solution would be to rename
`lazy` to `delay-force`, indicating it is simply the composition of
`delay` and `force`, and letting people see directly in code the
balance of `delay`s and `force`s.

* **Options:** delay-force, lazy, undecided
* **Default:** lazy
* **Voters:**
* [Cowan](WG1BallotCowan.md): delay-force
* [Ganz](WG1BallotGanz.md): delay-force
* [Gleckler](WG1BallotGleckler.md): delay-force
* [Hsu](WG1BallotHsu.md): undecided, lazy, delay-force
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): delay-force, lazy
* [Shinn](WG1BallotShinn.md): delay-force
* [SnellPym](WG1BallotSnellPym.md): delay-force, lazy
* **Results:** **delay-force**, lazy, undecided
* **Ratios:** 6:1, 6:2
* **Rationales:**

`Cowan`::
> `lazy` is confusing, `delay-force` makes the usage and the relation to existing operators obvious.
`Gleckler`::
> This name makes the purpose clearer.
`Shinn`::
> `lazy` is confusing, `delay-force` makes the usage and the relation to existing operators obvious.
`SnellPym`::
> I like that reasoning.

### #304 symbol literal syntax wastes characters

[on feedback from Marc Feeley.](Based)

Currently symbols can either be delimited with pipes |...|
with optional hex escapes inside, or include hex escapes
directly without the pipes.  This wastes two characters
that were reserved in R5RS, the pipe and the backslash,
when either one by itself would be sufficient to represent
all symbols.  This is especially unfortunate because both
characters are used as extensions in various Schemes -
the pipe being another symbol character in SCSH (to
represent shell-style pipes and C-style operators) and
the backslash used in Gambit's infix syntax.  We should
reconsider if we really need to take up both of these
characters.

We can also consider new sequences, for instance \|...|
with optional hex escapes inside uses only \, has the
readability advantages of |...|, and still leaves room for
other \ escapes since the following | character is required.
However, such new sequences have no existing support
among implementations.

* **Proposals:**
* **delimited-only:** |...| syntax with internal escapes, \ outside is undefined, Gambit-compatible
* **backslash-only:** \xNN; only, with | valid in identifiers, SCSH-compatible
* **both:** both as in the current draft
* **neither:** remove both
* **backslash-delimited:** \|...| syntax with internal escapes
* **Options:** delimited-only, backslash-only, both, neither, backslash-delimited, undecided
* **Default:** both
* **Voters:**
* [Cowan](WG1BallotCowan.md): delimited-only, backslash-only
* [Ganz](WG1BallotGanz.md): delimited-only, backslash-delimited
* [Gleckler](WG1BallotGleckler.md): delimited-only, backslash-delimited, no, backslash-only
* [Hsu](WG1BallotHsu.md): both-nointernal, both
* [Lucier](WG1BallotLucier.md): backslash-delimited, undecided
* [Medernach](WG1BallotMedernach.md): delimited-only
* [Shinn](WG1BallotShinn.md): delimited-only, backslash-only
* [SnellPym](WG1BallotSnellPym.md): delimited-only, backslash-only, both, backslash-delimited, neither
* **Results:** **delimited-only**, backslash-only, backslash-delimited, both, neither, undecided, no, both-nointernal
* **Ratios:** 6:0, 6:1, 6:1, 6:0, 6:1, 6:0, 6:1
* **Rationales:**

`Cowan`::
> Both is a waste - I prefer either-or. Although the uses in SCSH are nice, the |...| syntax is very widely implemented.
`Gleckler`::
> As others have said, the |...| syntax is widely implemented. I don't think the backslash-only syntax is widely implemented.
`Hsu`::
> I like both of these forms, but I think that the composition of the two is a mistake. Specifically, you should not be using internal escapes, and as such, I have added my own option here, to remove the use of internal escapes inside of the pipes.
`Shinn`::
> Both is a waste - I prefer either-or. Although the uses in SCSH are nice, the |...| syntax is very widely implemented.

### #305 Should we move the c...r and c....r procedures into a new library?

They have been required for a long time, but Alex Shinn says:

I definitely think everything but the one and two depth combinations
should be removed from `(scheme base)`.  Their use is generally a code
smell.  People should use destructuring, records, or SRFI-1
`first..tenth` accessors.

Ray Dillinger (Bear) adds:

The historic use of these entities was as accessors for structured
aggregates implemented with cons cells.  In a language that directly
supports records, they have a reduced mission.

Vote `base` to keep all in the base library or `library` to move the 3- and 4-letter accessors to a separate library.

* **Options:** base, library, remove, undecided
* **Default:** base
* **Voters:**
* [Cowan](WG1BallotCowan.md): library, base
* [Ganz](WG1BallotGanz.md): library
* [Gleckler](WG1BallotGleckler.md): library, base
* [Hsu](WG1BallotHsu.md): undecided, base, library, remove
* [Lucier](WG1BallotLucier.md): base, library
* [Medernach](WG1BallotMedernach.md): library, base
* [Shinn](WG1BallotShinn.md): library, base
* [SnellPym](WG1BallotSnellPym.md): library, remove, base
* **Results:** **library**, base, remove, undecided
* **Ratios:** 6:2, 8:0, 7:1
* **Rationales:**

`Cowan`::
> I think removing them is too strong, but would like to be able to trim down the base library of 24 marginal-use procedures.
`Gleckler`::
> They have a long history and are used in lots of code, so we shouldn't remove them. However, moving them to a library is a good idea. What shall we call it?
`Shinn`::
> I think removing them is too strong, but would like to be able to trim down the base library of 24 marginal-use procedures.
`SnellPym`::
> I concur with the submitted arguments.

### #307 "eager" is a confusing name

[on feedback from Marc Feeley](Based)

The `eager` procedure is named particularly unfortunately because it
sounds as though it is in some way paired with `lazy`, and there is
anecdotal evidence it was voted in on this misunderstanding.  In fact,
it is completely unrelated to `lazy`, being just a utility procedure
that has never been seen used in practice.  Perhaps a better name for
it would be `promise` or `make-promise`, since it just creates an
(already computed) promise value.

Vote `eager`, `promise` or `make-promise` to specify the name, or
`remove` to remove this procedure altogether.

* **Options:** eager, promise, make-promise, remove, undecided
* **Default:** eager
* **Voters:**
* [Cowan](WG1BallotCowan.md): promise, make-promise
* [Ganz](WG1BallotGanz.md): make-promise
* [Gleckler](WG1BallotGleckler.md): make-promise, promise, remove
* [Hsu](WG1BallotHsu.md): eager, promise, remove
* [Lucier](WG1BallotLucier.md): remove
* [Medernach](WG1BallotMedernach.md): make-promise, promise, eager
* [Shinn](WG1BallotShinn.md): remove, promise, make-promise
* [SnellPym](WG1BallotSnellPym.md): remove, make-promise, promise, eager
* **Results:** **make-promise**, promise, remove, eager
* **Ratios:** 4:3, 4:4, 6:1
* **Rationales:**

`Gleckler`::
> Names that include the word "promise" are clearer. I prefer `make-promise' to `promise' because, in other `make-foo' vs. `foo' cases, e.g. for lists, strings, and vectors, the `foo' name has been used for multiple arguments of the same type.
`Medernach`::
> This wording is a lot better.
`Shinn`::
> I suspect people voted this in under the false impression it formed a pair of some sort with lazy. The utility is minor so I'd just as soon leave it out, but if we do keep it I'd prefer a self-explanatory name.
`SnellPym`::
> I've never felt the need for this anyway.

### #308 Allow circular lists in LIST-REF for SRFI-1 compatibility

Allow the argument of `list-ref` to be circular.  It is still an error
to use an index >= the length of the list.  None of my test
implementations has a problem with this.

Vote `circular` to explicitly allow circular lists, `error` to add an
"is an error" disclaimer, or `unspecified` to leave as is.

* **Options:** circular, error, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): circular
* [Ganz](WG1BallotGanz.md): unspecified
* [Gleckler](WG1BallotGleckler.md): circular, unspecified
* [Hsu](WG1BallotHsu.md): circular, unspecified
* [Lucier](WG1BallotLucier.md): error
* [Medernach](WG1BallotMedernach.md): unspecified, circular
* [Shinn](WG1BallotShinn.md): circular
* [SnellPym](WG1BallotSnellPym.md): circular, error, unspecified
* **Results:** **circular**, unspecified, error
* **Ratios:** 5:2, 6:1
* **Rationales:**

`Cowan`::
> A de-facto standard - every implementation allows this anyway.
`Ganz`::
> Shouldn't be explicitly allowed unless also allowed for index >= length of list. Making it an error would require performing that check.
`Gleckler`::
> This is what implementations already do. We should certainly not make it an error, so the second choice is clearly "unspecified."
`Shinn`::
> A de-facto standard - every implementation allows this anyway.

### #309 Allow circular lists in MAP and FOR-EACH for SRFI-1 compatibility

Allow circular lists as the list arguments to `map` and `for-each`. If
all arguments are circular, these procedures will not terminate unless
the mapping procedure forces a non-local exit.  The result of `map` is
not circular.  Implementations that stop when the shortest list runs
out and don't make gratuitous tests shouldn't have a problem with
this: R5RS allows, R6RS forbids, and R7RS requires this behavior.

Vote `circular` to explicitly allow circular lists, `error` to add an
"is an error" disclaimer, or `unspecified` to leave as is.
Unspecified leaves open the theoretical extension of returning a new
circular list with the corresponding mapped results.

* **Options:** circular, error, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): circular
* [Ganz](WG1BallotGanz.md): unspecified
* [Gleckler](WG1BallotGleckler.md): circular, unspecified
* [Hsu](WG1BallotHsu.md): unspecified, circular
* [Lucier](WG1BallotLucier.md): error
* [Medernach](WG1BallotMedernach.md): unspecified, circular
* [Shinn](WG1BallotShinn.md): circular
* [SnellPym](WG1BallotSnellPym.md): circular, error, unspecified
* **Results:** *circular*, unspecified, error
* **Ratios:** 4:3, 6:1
* **Rationales:**

`Cowan`::
> The draft R7RS semantics makes this obvious.
`Gleckler`::
> This is useful, cheap, and matches R5RS. I've used an approach like this, for example, when constructing HTML with alternating colors for rows in a table.
`Shinn`::
> The draft R7RS semantics makes this obvious.

### #310 Rationalize start/end/(fill) arguments in sequence procedures

When we approved [CompleteSequenceCowan](CompleteSequenceCowan.md) in ticket #64, we adopted
[#http://srfi.schemers.org/srfi-43/srfi-43.html#vector-fill-bang|SRFI
43]] syntax and semantics for `vector-copy`, meaning that it takes
optional *start, end, fill* arguments.  This is inconsistent with
various other copier procedures in R7RS as inherited from R5RS, as
well as what is provided in SRFI 43 and its relatives
[SRFI 1](http://srfi.schemers.org/srfi-1/srfi-1.html) (for lists) and
[SRFI 13](http://srfi.schemers.org/srfi-13/srfi-13.html) (for strings).
There are four plausible courses of action:

* **Proposals:**
* *nothing* (default):  The only virtue here is that it requires the least thinking and editing.  Several comments have criticized it.
* *r5rs:*  Claw back ``vector-copy`` to just accept the source vector, all of which is to be copied.  This provides self-consistency, consistency with R5RS, and maximum simplicity.  The SRFIs will be provided as R7RS-large packages which will export the more complex and powerful versions.
* *srfi:*  Enhance `vector-fill!`, `vector->list`, `string->list`, `string-copy`, `string-fill!` to support optional *start* and *end* arguments.  This provides some self-consistency, backward compatibility with R5RS, consistency with the SRFIs, and some loss of simplicity.
* *srfi-plus:*  Same as *SRFIs*, but also add optional *start, end, fill* arguments to `list-copy` and optional *fill* argument to `string-copy`.  This provides maximal function, full self-consistency, backward compatibility with R5RS, and backward compatibility with the SRFIs.
* **Options:** nothing, r5rs, srfi, srfi-plus, undecided
* **Default:** nothing
* **Voters:**
* [Cowan](WG1BallotCowan.md): srfi, r5rs, srfi-plus
* [Ganz](WG1BallotGanz.md): srfi-plus, r5rs
* [Gleckler](WG1BallotGleckler.md): srfi-plus, srfi, nothing
* [Hsu](WG1BallotHsu.md): srfi-plus, srfi
* [Lucier](WG1BallotLucier.md): srfi
* [Medernach](WG1BallotMedernach.md): srfi, r5rs, srfi-plus, undecided, nothing
* [Shinn](WG1BallotShinn.md): r5rs, srfi, srfi-plus
* [SnellPym](WG1BallotSnellPym.md): srfi-plus, srfi, r5rs, nothing
* **Results:** **srfi**, srfi-plus, r5rs, nothing, undecided
* **Ratios:** 4:4, 6:2, 7:0, 7:0
* **Rationales:**

`Cowan`::
> Simpler is better in the core - people can always competing SRFIs and utility libraries.
`Gleckler`::
> This isn't expensive for implementations to provide, and there's widespread agreement on what it means. Let's have complete consistency and full power.
`Hsu`::
> The fill arguments are optional and do not make the language more complicated to use, and implementation is not difficult. It increases greatly the usefulness of these functions.
`Shinn`::
> Simpler is better in the core - people can always competing SRFIs and utility libraries.
`SnellPym`::
> I think they are relatively minor additions in terms of standard library code bloat, compared to the code bloat of reimplementing them all to provide the extra functionality *in an extra library on top of the core versions*.

### #311 Remove tail call guarantee for guard clauses

The current draft guarantees the guard clauses (not the body) of a
guard form to be in tail call position, but the need for this is
unclear (who needs an unbounded number of active exceptions), and
there may be worthwhile guard implementations where this is not the
case.

* **Options:** remove, keep, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): remove
* [Ganz](WG1BallotGanz.md): keep
* [Gleckler](WG1BallotGleckler.md): remove
* [Hsu](WG1BallotHsu.md): keep
* [Medernach](WG1BallotMedernach.md): remove
* [Shinn](WG1BallotShinn.md): remove
* [SnellPym](WG1BallotSnellPym.md): remove, keep
* **Results:** **remove**, keep
* **Ratios:** 5:2
* **Rationales:**

`Gleckler`::
> I don't have a strong opinion here, but I buy the argument from lack of need for an unbounded number of active exceptions.
`Hsu`::
> We should not eliminate a good condition like this based on an assumption of the common use case of GUARD. Keeping the tail call guarantee makes GUARD more generally useful, though perhaps marginally so.
`Shinn`::
> A minor detail, but I think mentioning detracts more than it adds from the standard.

### #312 unquoting and identifiers beginning with @

The current draft allows `@` to begin an identifier, which would require
some comment about unquoting, i.e. to distinguish whether `,@foo` is
`(unquote @foo)` or `(unquote-splicing foo)`.

The options are `invalid` (disallow @ at the beginning of an
identifier, as in R5RS), `unquote` to indicate that `,@foo` is `(unquote @foo)`, and
`unquote-splicing` to indicate that `,@foo` is `(unquote-splicing foo)`.

If `unquote-splicing` is chosen, a
note will be added saying that if you want to unquote an identifier beginning with `@` you
need to either insert whitespace or escape the identifier, e.g. either `, @foo`
or `,|@foo|`.

Note that if we don't choose `invalid` then SXML retroactively becomes
valid syntax.

* **Options:** invalid, unquote, unquote-splicing, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unquote-splicing
* [Ganz](WG1BallotGanz.md): invalid, unspecified, undecided
* [Gleckler](WG1BallotGleckler.md): invalid, unquote-splicing
* [Hsu](WG1BallotHsu.md): unquote-splicing, unspecified
* [Lucier](WG1BallotLucier.md): invalid
* [Medernach](WG1BallotMedernach.md): unquote-splicing, invalid
* [Shinn](WG1BallotShinn.md): unquote-splicing
* [SnellPym](WG1BallotSnellPym.md): unquote-splicing, unquote, unspecified, invalid
* **Results:** **unquote-splicing**, invalid, unspecified, unquote, undecided
* **Ratios:** 5:3, 6:1, 6:0, 6:1
* **Rationales:**

`Cowan`::
> This is what all implementations I'm aware of do - `unquote` would be strange and difficult semantics and is not completely speficied here, while `invalid` is undesirable due to SXML.
`Gleckler`::
> Anything other than invalid would be too confusing. If we're not going to do that, let's do what implementations already do, which is the unquote-splicing option.
`Hsu`::
> The unquote-splicing form more naturally maps to the way that people will have been used to seeing this. That is, if I have a ,@foo without @foo being defined anywhere, I expect to get `(unquote-splicing foo)` and not `, @foo` which will give me an error. This stuff is likely to happen before we get to anything that tells us whether an identifier is bound or not, and so this will be confusing for thos who do not use @foo identifiers. Those who do can easily get the behaviour they want, and they are more likely to know what is going on.
`Shinn`::
> This is what all implementations I'm aware of do - `unquote` would be strange and difficult semantics and is not completely speficied here, while `invalid` is undesirable due to SXML.
`SnellPym`::
> I think that the syntactic sugar 'operators' should have higher precedence than symbol syntax, as it's easier to quote a funny symbol than to write out `(unquote-splicing ...)` by hand.

### #315 null character may not be usable in strings

We should probably make (string-set! str n #\null) unspecified.  Note that R7RS implementations can already restrict the set of characters that are allowed in strings.

Vote `yes` to add a clause to this effect, and `no` to leave it as legal.

* **Options:** yes, no, undecided
* **Default:** yes
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **no**, yes, undecided
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> R7RS implementations can already restrict the set of string-chars. I don't see any reason to call out null specially. If your implementation doesn't want to allow it, then don't allow it.
`Gleckler`::
> Since implementations can already prohibit #\null in strings, there's no need to do this. But the broken semantics of C strings shouldn't become part of the Scheme standard.
`Medernach`::
> I see no reason why scheme strings may or may not include #\null in some implementations, but if not you are free to disallow it.
`Shinn`::
> Many implementations can represent the null char, and at the same time use C strings as the underlying string representation, which makes this unspecified.
`SnellPym`::
> Not that I have any love for the null character, but I don't see a good reason to disallow implementations from supporting it if they want to. Somebody might wish to use Scheme to write test suites for string-processing tools, protocols, etc that might not be as null-transparent as they should be, so generating and examining test strings with nulls in would be a required feature then.

### #316 R6RS base compatibility: boolean=?

This is equivalent to `eq?` on booleans, and provides R6RS base
compatibility as well as completing the set of type-specific
comparisons.  See also #285.

Vote `yes` to add these three procedures.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> As in #285, this helps with type-inference.
`Gleckler`::
> It's good to complete the set of type-specific comparisons.
`Shinn`::
> As in #285, this helps with type-inference.
`SnellPym`::
> COnsistency is good.

### #317 escape from with-input-from-file

The draft states for with-input-from-file and with-output-to-file:

> If an escape procedure is used to escape
> from the continuation of these procedures, their
> behavior is implementation-dependent.

but now that we have dynamic-wind there's no particular reason to keep
this restriction, nor is it difficult to implement.

Vote `parameterize` to specify the current-in/output-port are bound
dynamically as with parameterize in these cases, or `unspecified` to
leave unspecified.

* **Options:** parameterize, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): parameterize
* [Ganz](WG1BallotGanz.md): parameterize
* [Gleckler](WG1BallotGleckler.md): parameterize
* [Hsu](WG1BallotHsu.md): parameterize
* [Medernach](WG1BallotMedernach.md): parameterize
* [Shinn](WG1BallotShinn.md): parameterize
* [SnellPym](WG1BallotSnellPym.md): parameterize, unspecified
* **Results:** **parameterize**, unspecified
* **Ratios:** 7:0
* **Rationales:**

`Cowan`::
> This was just an oversight, and was pretty clearly an oversight in R5RS which already had dynamic-wind.
`Gleckler`::
> This seems completely natural. I can't think of a reason that the behavior of these should be different than if they had been defined explicitly in terms of `parameterize'.
`Shinn`::
> This was pretty clearly an oversight in R5RS which already had dynamic-wind.

### #319 Make special treatment of CAPITAL SIGMA optional

Currently we require that if the characters GREEK LETTER CAPITAL
SIGMA, SMALL SIGMA, and SMALL FINAL SIGMA are supported by an
implementation, that a CAPITAL SIGMA in a string passed to
`string-downcase` be changed to SMALL FINAL SIGMA just before a word
break, and SMALL SIGMA otherwise.  Word breaks are defined by UAX #29,
and are no simple matter.  The proposal is to make this behavior optional,
allowing CAPITAL SIGMA to be downcased to SMALL SIGMA in every case.

Vote `yes` to make optional.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): undecided, no
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** *yes*, undecided, no
* **Ratios:** 4:2, 3:2
* **Rationales:**

`Cowan`::
> Alex says: "There are other complexities in case folding, I don't see why we should single out just this case." On the contrary, the rest of case folding for strings is trivial: there are 1027 characters to replace with another character, 88 characters to replace with a sequence of two characters, and 16 characters to replace with a sequence of three characters, all independent of the context. This can be executed in a single loop. By contrast, case-folding CAPITAL SIGMA requires determining whether a word-break follows: it cannot be done without look-ahead. To implement the word-breaking algorithm, one must keep around 13 character classes and implement 17 rules, all to get one character right -- and not even completely reliably: "φιλοσ." is right if t[...](WG1BallotCowan.md)
`Gleckler`::
> If John's argument that case folding sigma is AI-complete is even partially correct, then this makes sense. Furthermore, it is strange to incorporate this kind of language-specific Unicode-ism into the standard.
`Shinn`::
> I think this needs more discussion, but if we're defining string-*case in terms of an AI-complete word-break operator then that's the real problem, not what to do with sigma.
`SnellPym`::
> I know the full Unicode rules are complex, but if we don't support them properly, then it'll be difficult for users to get hold of full Unicode semantics when they need them - they'll have to implement it from scratch. Which means that, in effect, `string-downcase` is unreliable and can't be trusted and you have to use your own implementation if you care.

### #320 Add new cond-expand feature to Appendix B: exact-complex

(In this ticket, "complex" is used for readability; it is synonymous
with "non-real".)

This feature is true in implementations that support complex numbers
such that both the real and the imaginary parts are exact; that is, if
`(eqv? 3+4i 3.0+4.0i)` evaluates to `#f`.  This feature is false if
complex numbers are not supported or if only inexact complex numbers
are supported.  Most of the applications of complex numbers use
inexact numbers, but some applications may require exactness: this
feature allows those applications to fail fast on implementations that
cannot support them.

Existing implementations:

* Exact complex numbers: Racket, MIT, Gambit, Chicken with the `numbers` egg, Scheme48/scsh, Kawa, Chibi, Chez, Vicare, Ypsilon, Mosh, IronScheme, STklos, Wraith
* No exact complex numbers: Gauche, Guile, SISC, SCM, Scheme 7, KSi, UMB, Stalin
* No complex numbers: Chicken without the `numbers` egg, Bigloo, Ikarus, RScheme, Scheme 9, Oaklisp, Elk, VX, Sixx, Sizzle, Dream, Owl Lisp, Psyche

Vote `yes` to add this feature.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 7:0
* **Rationales:**

`Gleckler`::
> Feature identifiers are cheap and useful.
`Shinn`::
> This is handy - it is already being used in the Chibi codebase.
`SnellPym`::
> Failing fast is what cond-expand is for, so let's give it the tools to do the job.

### #321 Add get-features from [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) to R7RS-small

This procedure returns a list of symbols corresponding to the feature
identifiers which the implementation treats as true.  More details at
[EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md).

Vote `yes` to add this procedure.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): no
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): wg2, no, yes
* **Results:** **yes**, wg2, no
* **Ratios:** 5:1, 5:2
* **Rationales:**

`Cowan`::
> This can be useful for debugging and version info.
`Gleckler`::
> I'd like to be able to print the list of features on start-up, for example. This information will certainly be available to the implementation, so it should be made available programmatically. However, this should be called `features' or `feature-list', not `get-features'. The latter sounds like Java.
`Shinn`::
> This can be useful for debugging and version info.
`SnellPym`::
> This is useful, but I have a hunch it shouldn't be required as it might be an expensive operation to do on systems that dynamically load features on demand etc.

### #322 Add [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) (other than get-features) to R7RS-small

[EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) is a library providing *at run time* what
Common Lisp calls environment enquiries such as the name of the OS.
Implementations can currently expose these as `cond-expand` feature
identifiers, but there is no way to determine things like the name of
the implementation at run time so that it can be written to a log
file, for example.

Vote `yes` to add [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) (other than
`get-features`), and `no` to leave out.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes, wg2
* [Hsu](WG1BallotHsu.md): no
* [Medernach](WG1BallotMedernach.md): wg2, no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, wg2, no
* **Results:** **no**, yes, wg2
* **Ratios:** 4:3, 3:3
* **Rationales:**

`Cowan`::
> This needs more use and investigation, and as a library has no advantage being in the small language (as opposed to features). Let's add this to the large language.
`Ganz`::
> Seems good to have.
`Gleckler`::
> This information is easy and cheap for any implementation to provide inexpensively, and is highly useful. However, `implementation-type' should be called `implementation-name'. After all, the description at [EnvironmentEnquiriesCowan](EnvironmentEnquiriesCowan.md) starts "Returns the name [the type](not) of the Scheme implementation."
`Shinn`::
> This needs more use and investigation, and as a library has no advantage being in the small language (as opposed to features). Let's add this to the large language.
`SnellPym`::
> My answer to the previous nonwithstanding, it's good to be able to provide information that the implementation should generally have trivial access to.

### #323 Eliminate some cond-expand feature identifiers

Reduce the standardized `cond-expand` feature identifiers to `r7rs`,
`exact-closed`, `ratio`s, `ieee-float`, and `full-unicode`, plus the
name and name-plus-version of the implementation.  The others can't
affect the behavior of strictly conforming programs, and it's not
clear if they apply to compile time or run time on implementations
that distinguish the two.  See also ticket #320 for `exact-complex`.

Argument against: Keeping them in the standard encourages all
implementations that use them to spell them the same way: `darwin`,
not `macosx`.

Vote `full` to keep the full list as in draft-6, `implementation` to
keep only the implementation features, or `numerics` to keep the list
described above.

* **Options:** full, implementation, numerics
* **Default:** full
* **Voters:**
* [Cowan](WG1BallotCowan.md): full, numerics
* [Ganz](WG1BallotGanz.md): full, numerics
* [Gleckler](WG1BallotGleckler.md): full, numerics
* [Hsu](WG1BallotHsu.md): numerics, implementation
* [Medernach](WG1BallotMedernach.md): full
* [Shinn](WG1BallotShinn.md): full, numerics
* [SnellPym](WG1BallotSnellPym.md): full, (implementation numerics)
* **Results:** **full**, numerics, implementation
* **Ratios:** 6:1, 6:1
* **Rationales:**

`Cowan`::
> Pending a better list, I think many of the features are useful, and are actively in use in several implementations.
`Gleckler`::
> The list is useful. The argument that the other features identifiers can't affect the behavior of strictly conforming programs misses the point. The whole point of those identifiers is dealing with places where implementations differ.
`Shinn`::
> Pending a better list, I think many of the features are useful, and are actively in use in several implementations.

### #259 Remove `(library <name>)` cond-expand features

The `(library <name>)` feature test which is true if the given library
is available (at compile time).  This was used because we voted for
[CondExpandCowan](CondExpandCowan.md), but the original syntax was just `<name>` which is
ambiguous and therefore invalid.  The switch to `(library <name>)` was
added editorially, but not officially voted on.

Vote `keep` to keep and `remove` to remove.

* **Options:** keep, remove, wg2, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): keep
* [Ganz](WG1BallotGanz.md): keep
* [Gleckler](WG1BallotGleckler.md): keep, remove
* [Hsu](WG1BallotHsu.md): keep
* [Lucier](WG1BallotLucier.md): keep
* [Medernach](WG1BallotMedernach.md): keep
* [Shinn](WG1BallotShinn.md): keep
* [SnellPym](WG1BallotSnellPym.md): keep, remove
* **Results:** **keep**, remove
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> I personally don't care if users are forbidden to name libraries `(not ...)`, `(and ...)`, or `(or ...)`, but this is a sensible way to avoid a minor problem.
`Gleckler`::
> This avoids ambiguity. Clashes are unlikely, but that's exactly what makes debugging them difficult when they do happen. This avoids the problem entirely.
`Shinn`::
> A formality - we basically already voted this in.

### #324 allow |\ as escape for | within a |-escaped identifier

Allow `\|` to represent a vertical bar in an identifier enclosed in
vertical bars (the current BNF disallows | anywhere in the escape).

Note this item is nullified if |...| escapes are removed in item #304.

Vote `pipe` to allow just the vertical bar escaped, `string` to allow
the same set of escapes as in string literals (plus pipe), and `none`
to leave as is.

* **Options:** pipe, string, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): string
* [Ganz](WG1BallotGanz.md): string, pipe
* [Gleckler](WG1BallotGleckler.md): string
* [Hsu](WG1BallotHsu.md): no
* [Medernach](WG1BallotMedernach.md): string
* [Shinn](WG1BallotShinn.md): string
* [SnellPym](WG1BallotSnellPym.md): string, pipe, no
* **Results:** **string**, pipe, no
* **Ratios:** 6:0, 6:1
* **Rationales:**

`Cowan`::
> I think this was an oversight.
`Gleckler`::
> Consistency makes things easier to remember, and there's no reason not to be consistent with strings here.
`Hsu`::
> this defeats the purpose of |...| in my opinion.
`Shinn`::
> I think this was an oversight.

### #325 Eliminate bytevector-copy!

`(bytevector-copy! from to)` is equivalent to
`(bytevector-copy-partial! from 0 (bytevector-length) to 0)`.

The proposal is to remove the existing `bytevector-copy!` from the
small language, and rename `bytevector-copy-partial!` to
`bytevector-copy!`, with the order of arguments `to at from start
end`, the same order used in SRFI 43's `vector-copy!`.  Note that SRFI
43 will be part of the large language.

Vote `yes` to eliminate and rename as proposed, and `no` to leave
as-is.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 7:0, 7:1
* **Rationales:**

`Cowan`::
> There were many complaints against the existing API, we should simplify this.
`Gleckler`::
> Yes, let's make this consistent.
`Shinn`::
> There were many complaints against the existing API, we should simplify this.

### #326 Add destructive list-copy!, string-copy!, and vector-copy!

From Per Bothner:

Copying a slice from one vector/string into another is such a
fundamental operation that it should be added, IMO, considering that
it's tedious to write if "by hand", and that a standard library
routine is likely to be much more efficient (especially for strings,
since that avoids the need for boxing and unboxing the characters).
[#JC:|Many implementations represent characters as immediates,
however.]]

One could also argue that "character" operations don't really make
semantic sense in a Unicode world, and so `string-set!` has limited
usefulness.  Thus `string-copy` [start/end arguments](with) and
`string-copy!` are the actual useful "primitive" operations.

JC: These would be the five-argument versions based on the current
`bytevector-copy-partial!`, possibly with renumbering of arguments
depending on the outcome of #325.

Vote `yes` to add these destructive operations as proposed, `nolist` to add `string-copy!` and `vector-copy!` only, or `no` for none of them.

* **Options:** yes, nolist, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): nolist, yes
* [Ganz](WG1BallotGanz.md): yes, nolist
* [Gleckler](WG1BallotGleckler.md): nolist, no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): nolist
* [Medernach](WG1BallotMedernach.md): nolist
* [Shinn](WG1BallotShinn.md): no, vector-only, nolist
* [SnellPym](WG1BallotSnellPym.md): nolist, no, yes
* **Results:** **nolist**, yes, no, vector-only
* **Ratios:** 6:2, 6:1, 6:1
* **Rationales:**

`Cowan`::
> I added `list-copy!` to this ballot for uniformity, but on reflection I agree that it's not that useful, whereas `string-copy!` and `vector-copy!` are.
`Gleckler`::
> I agree with others' comments about `list-copy!`. However, `string-copy!` and `vector-copy!` are quite useful and cheap.
`Shinn`::
> `list-copy!` is very rarely used and usually indicative of a broken algorithm. `string-copy!` is a step away from immutable strings, and terrible performance for utf8 implementations. `vector-copy!` is more useful but probably better left to a general vector library, though I could be convinced to include just it.
`SnellPym`::
> I want cons cells and strings, to be less mutable, but am open to mutability in vectors.

### #327 Specify that read, the program reader, and string->number accept the same syntax

Currently there is no guarantee of this.  Obviously the
`string->number` only applies to the case where the radix is 10 or
specified.

Specifying `same` is problematic in the presence of batch compilation
- the compile-time and runtime may not even support the same numeric
tower.

* **Proposals:**
* *same*: The lexical syntax for numbers accepted by `string->number` and `read`, as well as the corresponding syntax of literal numbers in programs, must be the same.
* *run-time*: The lexical syntax for numbers accepted by `string->number` and `read` must be the same, but the relationship with the the corresponding syntax of literal numbers in programs is unspecified.
* *unspecified*: The relationships between lexical syntax for numbers accepted by `string->number` and `read`, as well as the corresponding syntax of literal numbers in programs, is unspecified.
* **Options:** same, run-time, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): run-time, unspecified
* [Ganz](WG1BallotGanz.md): same
* [Gleckler](WG1BallotGleckler.md): same, run-time
* [Hsu](WG1BallotHsu.md): same, unspecified, run-time
* [Lucier](WG1BallotLucier.md): same, unspecified, undecided
* [Medernach](WG1BallotMedernach.md): run-time, same
* [Shinn](WG1BallotShinn.md): run-time, unspecified
* [SnellPym](WG1BallotSnellPym.md): same, unspecified, run-time
* **Results:** **same**, run-time, unspecified, undecided
* **Ratios:** 5:3, 6:2, 6:0
* **Rationales:**

`Cowan`::
> Basically all we can and should say is that `string->number` with a radix of 10 should behave the same as `read`.
`Ganz`::
> Programs can be data.
`Gleckler`::
> I don't understand why we should support having different numerics tower at compile time and run time. That seems like a recipe for confusion. What implementations make this distinction?
`Shinn`::
> Basically all we can and should say is that `string->number` with a radix of 10 should behave the same as `read`.
`SnellPym`::
> Implementations having different numeric towers and compile and run time, I feel, is already a recipe for pain and suffering, so effort spent pandering to it is probably not well spent!

### #328 names for inexact->exact and exact->inexact

R6RS changed these names to the more sensible exact and inexact.
We need to decide if we want to follow suit, or provide both names,
or write a disclaimer.

Vote `r6rs` for the short names, `r5rs` for the long names, or `both`
for both.

* **Options:** r5rs, r6rs, both, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs, both
* [Ganz](WG1BallotGanz.md): r5rs
* [Gleckler](WG1BallotGleckler.md): r6rs, r5rs
* [Hsu](WG1BallotHsu.md): r6rs
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): r5rs, both, r6rs
* [Shinn](WG1BallotShinn.md): r6rs, both
* [SnellPym](WG1BallotSnellPym.md): r6rs, r5rs, both
* **Results:** **r6rs**, r5rs, both
* **Ratios:** 6:2, 6:1
* **Rationales:**

`Cowan`::
> We have a library system now, we shouldn't be too afraid to clean up names. The `(scheme r5rs)` library can be voted in if we want easy 100% compatibility.
`Ganz`::
> The long names seem more consistent with other function names.
`Gleckler`::
> It's pointless and confusing to have both.
`Lucier`::
> Actually, I'd prefer ->exact and ->inexact.
`Medernach`::
> I like the long names as it shows clearly that it is conversion procedures. Ok, this is kind of bike-shedding.
`Shinn`::
> We have a library system now, we shouldn't be too afraid to clean up names. The `(scheme r5rs)` library can be voted in if we want easy 100% compatibility.
`SnellPym`::
> I prefer the R6RS names.

### #329 Add IEEE compatibility library

The `(scheme ieee)` library exports the standard identifiers of IEEE
1178-1990.  By my current reckoning, those identifiers are as follows:

`- * / + < <= = > >= abs acos and angle append apply asin assoc assq
assv atan begin boolean? call-with-current-continuation car case cdr
ceiling char->integer char-alphabetic? char-ci<? char-ci<=? char-ci=?
char-ci>? char-ci>=? char-downcase char-lower-case? char-numeric?
char-upcase char-upper-case? char-whitespace? char? char<? char<=?
char=? char>? char>=? close-input-port close-output-port complex? cond
cons cos current-input-port current-output-port define denominator
display do eof-object? eq? equal? eqv? even? exact->inexact exact? exp
expt floor for-each gcd if imag-part inexact->exact inexact?
input-port? integer->char integer? lambda lcm length let let* letrec
list list-ref list? log magnitude make-polar make-rectangular
make-string make-vector map max member memq memv min modulo negative?
newline not null? number->string number? numerator odd?
open-input-file open-output-file or output-port? pair? peek-char
positive? procedure? quasiquote quote quotient rational? rationalize
read read-char real-part real? remainder reverse round set-car!
set-cdr! set! sin sqrt string string->number string->symbol
string-append string-ci<? string-ci<=? string-ci=? string-ci>?
string-ci>=? string-length string-ref string-set! string? string<?
string<=? string=? string>? string>=? substring symbol->string symbol?
tan truncate vector vector-length vector-ref vector-set! vector? write
write-char zero?`

As with any library other than `(scheme base)`, implementations SHOULD
(rather than MUST) provide this.

Vote `yes` to add this library.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **no**, yes
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> It's easier to provide `(scheme r5rs)`.
`Gleckler`::
> People don't care much about IEEE Scheme, so we shouldn't force implementations to provide this.
`Shinn`::
> I don't think people would ever want to use this instead of `(scheme r5rs)`.

### #330 Add R5RS compatibility library

The `(scheme r5rs)` library exports the standard identifiers of R5RS
Scheme other than `transcript-{on,off}`.  By my current reckoning, those identifiers are as follows:

`- * / + < <= = > >= abs acos and angle append apply asin assoc assq
assv atan begin boolean? call-with-current-continuation
call-with-values car case cdr ceiling char->integer char-alphabetic?
char-ci<? char-ci<=? char-ci=? char-ci>? char-ci>=? char-downcase
char-lower-case? char-numeric? char-ready? char-upcase
char-upper-case? char-whitespace? char? char<? char<=? char=? char>?
char>=? close-input-port close-output-port complex? cond cons cos
current-input-port current-output-port define define-syntax delay
denominator display do dynamic-wind eof-object? eq? equal? eqv? eval
even? exact->inexact exact? exp expt floor for-each force gcd if
imag-part inexact->exact inexact? input-port? integer->char integer?
interaction-environment lambda lcm length let let-syntax let* letrec
letrec-syntax list list->string list->vector list-ref list-tail list?
load log magnitude make-polar make-rectangular make-string make-vector
map max member memq memv min modulo negative? newline not
null-environment null? number->string number? numerator odd?
open-input-file open-output-file or output-port? pair? peek-char
positive? procedure? quasiquote quote quotient rational? rationalize
read read-char real-part real? remainder reverse round
scheme-report-environment set-car! set-cdr! set! sin sqrt string
string->list string->number string->symbol string-append string-ci<?
string-ci<=? string-ci=? string-ci>? string-ci>=? string-copy
string-fill! string-length string-ref string-set! string? string<?
string<=? string=? string>? string>=? substring symbol->string symbol?
tan truncate values vector vector->list vector-fill! vector-length
vector-ref vector-set! vector? with-input-from-file
with-output-to-file write write-char zero?`

As with any library other than `(scheme base)`, implementations SHOULD
(rather than MUST) provide this.  A disclaimer will be added that the
semantics may not be exactly the same.

Vote `yes` to add this library.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 7:1
* **Rationales:**

`Gleckler`::
> This will make using old programs easier.
`Medernach`::
> For compatibility
`Shinn`::
> This eases backwards-compatibility greatly.

### #331 Add R6RS base compatibility library

The `(scheme r6rs base)` library exports the standard identifiers of
the base library of R6RS.  By my current reckoning, those identifiers
are as follows:

`- * / + < <= = > >= abs acos and angle append apply asin atan begin
boolean? call/cc call-with-current-continuation call-with-values car
case cdr ceiling char? char<? char<=? char=? char>? char>=?
char->integer complex? cond cons cos define define-syntax denominator
dynamic-wind eq? equal? eqv? even? exact exact? exact-integer-sqrt exp
expt finite? floor for-each gcd guard if imag-part import inexact
inexact? integer? integer->char lambda lcm length let let* let*-values
letrec letrec* letrec-syntax let-syntax let-values list list?
list->string list->vector list-ref list-tail log magnitude make-polar
make-rectangular make-string make-vector map max min nan? negative?
not null? number? number->string numerator odd? or pair? positive?
procedure? quasiquote quote rational? rationalize real? real-part
reverse round set! sin sqrt string string? string<? string<=? string=?
string>? string>=? string->list string->number string->symbol
string-append string-copy string-for-each string-length string-ref
substring symbol? symbol->string tan truncate values vector vector?
vector->list vector-fill! vector-for-each vector-length vector-map
vector-ref vector-set! zero?`

As with any library other than `(scheme base)`, implementations SHOULD
(rather than MUST) provide this.  Full compliance will depend on voting for
the procedures `*-valued`, `assert`, `boolean=?`, `symbol=?`.  A disclaimer
will be added that the semantics will not be exactly the same.

Vote `yes` to add this library.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): wg2, yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): wg2, no, undecided, yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **no**, wg2, yes, undecided
* **Ratios:** 5:2, 5:3, 6:0
* **Rationales:**

`Gleckler`::
> This might be reasonable for large Scheme, but one of the points of small Scheme is to avoid having to support R6RS.
`Medernach`::
> Really it makes no sense to provide all of this in WG1, but may be provided in WG2
`Shinn`::
> I don't agree with including all of the R6RS base exports in the small language, and the small language has no obligation to support R6RS - that's the reason we have a large language.

### #332 Allow multiple name pairs in export renaming

Currently, to export `my:foo` and `my:bar` as `foo` and `bar`, one
must write `(export (rename my:foo foo) (rename my:bar bar))`.  This
proposal allows `(export (rename (my:foo foo) (my:bar bar)))`.  This
is incompatible with R6RS, but compatible with the `rename` sub-form
of `import`.

Vote `multiple` to allow multiple renames in one rename clause as with
the import version, `r6rs` to allow the R6RS-compatible syntax in the
current draft, or `both` to allow both forms.

* **Options:** r6rs, multiple, both, undecided
* **Default:** r6rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Ganz](WG1BallotGanz.md): multiple, both
* [Gleckler](WG1BallotGleckler.md): multiple, r6rs
* [Hsu](WG1BallotHsu.md): both
* [Medernach](WG1BallotMedernach.md): r6rs
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): multiple, r6rs, both
* **Results:** **r6rs**, multiple, both
* **Ratios:** 3:3, 5:2
* **Rationales:**

`Gleckler`::
> Why not be compatible with `rename'?
`Shinn`::
> I like the simplicity of `(length export-list)` indicating the number of exports.
`SnellPym`::
> Consistency with import strikes me as preferable.

### #333 Require eof-objects to be disjoint from basic Scheme types

It's already a requirement that an eof-object cannot have an external
representation, which means it cannot be any of the basic types in
Section 3.2 except procedure or port.  This is very improbable, and in
fact none of my 40 test Schemes returns either a procedure or a port.

Doing this would allow `eof-object?` to be added to the list of
disjoint type predicates in Section 3.2.

Vote `yes` to explicitly list the eof-object as a separate disjoint type.

* **Options:**
* **Default:**
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 8:0
* **Rationales:**

`Gleckler`::
> Now that #f and '() are of distinct, we've been moving toward disjoint types in general.
`Shinn`::
> This is a useful guarantee and a de-facto standard.

### #334 Use proper case for the feature identifiers in Appendix B

Specifically R7RS, IEEE-float, full-Unicode, Windows, POSIX, Unix,
Darwin, Linux, BSD, FreeBSD, Solaris, PPC, SPARC, JVM, CLR, LLVM,
ILP32, LP64, ILP64.

Note this is incompatible with existing implementations which provide
these features.  The correct case can often be ambiguous, and it's
easiest to keep everything consistently lower case.

Vote `mixed` for mixed case and `lower` for lower case.

* **Options:** lower, mixed, undecided
* **Default:** lower
* **Voters:**
* [Cowan](WG1BallotCowan.md): lower
* [Ganz](WG1BallotGanz.md): lower
* [Gleckler](WG1BallotGleckler.md): lower
* [Hsu](WG1BallotHsu.md): lower
* [Lucier](WG1BallotLucier.md): lower
* [Medernach](WG1BallotMedernach.md): lower
* [Shinn](WG1BallotShinn.md): lower
* [SnellPym](WG1BallotSnellPym.md): lower, mixed
* **Results:** **lower**, mixed
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> We have enough of a de-facto standard with existing implementation features, and as a general rule I hate mixed-case identifiers. People are inconsistent as to when they use it in different libraries (is it "utf8" or "UTF8"?, etc.), so it's easier to remember if everything is lowercase. It also saves having to touch the shift key.
`Gleckler`::
> While this was my proposal, I'm voting against it based on the argument that some of these feature identifiers are already being used in lower case. It feels illiterate to require case sensitivity but then demand that people use case that doesn't match the natural-language names, but since feature identifiers exist for purely practical purposes, we're struck with this.
`Shinn`::
> We have enough of a de-facto standard with existing implementation features, and as a general rule I hate mixed-case identifiers. People are inconsistent as to when they use it in different libraries (is it "utf8" or "UTF8"?, etc.), so it's easier to remember if everything is lowercase. It also saves having to touch the shift key.

### #335 Specify behavior of default exception handler

If an exception is caught and leaves the current dynamic extent,
obviously the *after* thunk must be run, but an uncaught exception has
no semantics and is basically reverting to "is an error" semantics,
i.e. nasal demon territory.

Possibly we should tighten this up in the standard, i.e. specify that
there is a default exception handler which enters a continuation
outside the extent of the whole program before exiting.

Vote `unwind` to specify that there is a default exception handler
which leaves the current dynamic extent causing a full unwind (and
thus forbidding a debugger), `exit` to specify that (modulo any
diagnostic information) the program must simply exit without
unwinding, or `unspecified` to leave this as is.

* **Options:** unwind, exit, unspecified
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Ganz](WG1BallotGanz.md): unwind, unspecified
* [Gleckler](WG1BallotGleckler.md): unspecified
* [Hsu](WG1BallotHsu.md): unspecified, unwind
* [Medernach](WG1BallotMedernach.md): unspecified
* [Shinn](WG1BallotShinn.md): unspecified
* [SnellPym](WG1BallotSnellPym.md): unwind, exit, unspecified
* **Results:** **unspecified**, unwind, exit
* **Ratios:** 5:2, 6:1
* **Rationales:**

`Cowan`::
> The default behavior in some existing implementations such as Gambit is to drop into a debugger, even in a batch program, so I don't think we can specify this.
`Ganz`::
> The semantics of the inner block should not be dependent on what happens outside afterwards.
`Gleckler`::
> Implementations vary too much in this regard, and it's an area where the context of the program and implementation matter a lot, so we should leave it up the implementers.
`Shinn`::
> The default behavior in some existing implementations such as Gambit is to drop into a debugger, even in a batch program, so I don't think we can specify this.
`SnellPym`::
> `unwind` doesn't forbid a debugger, but merely makes it non-compliant. However, as a feature one turns on in an implementation by requesting it, it would be a perfectly valid and useful noncompliance. After all, other debugging facilities, such as randomly changing mutable state at any point in program execution, are clearly also in violation of the spec. It's disappointing if dynamic-wind is unreliable.

### #344 Should dynamic-wind handlers be invoked from EXIT?

Currently the report is silent about whether dynamic-wind handlers are
invoked when `exit` is called.

The options are the same as in #335 above.

* **Options:** unwind, exit, unspecified
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unwind
* [Ganz](WG1BallotGanz.md): exit, unwind
* [Gleckler](WG1BallotGleckler.md): unspecified
* [Hsu](WG1BallotHsu.md): unwind, unspecified
* [Medernach](WG1BallotMedernach.md): unwind
* [Shinn](WG1BallotShinn.md): unwind
* [SnellPym](WG1BallotSnellPym.md): unwind, exit, unspecified
* **Results:** **unwind**, exit, unspecified
* **Ratios:** 5:1, 6:1
* **Rationales:**

`Cowan`::
> Contrary to #335 above, there is no reason not to unwind here. Once finalizers are supported (maybe in the large language) we'd probably want to require them to be run as well. Note this does mean that `exit` can't be a simple wrapper around the syscall.
`Ganz`::
> Like the name says. At least there should be some function that does this; it could be called 'abort'. But it must be specified.
`Gleckler`::
> I'm uncomfortable specifying what should be done here without knowing what existing implementations do in general. For some people, `exit' means "get out of here immediately." I don't want something that happens in a dynamic-wind handler to prevent the program from exiting, for example, or to delay exit.
`Shinn`::
> Contrary to #335 above, there is no reason not to unwind here. Once finalizers are supported (maybe in the large language) we'd probably want to require them to be run as well. Note this does mean that `exit` can't be a simple wrapper around the syscall.
`SnellPym`::
> Likewise.

### #337 Add eof-object procedure

`eof-object` returns an object which answers `#t` to `eof-object?`.
This procedure is present in R6RS, where it must return the *unique*
end-of-file object; that is not required here.

From Vincent Manis:

This isn't just an attempt to create a vain orthogonality; there are
good reasons why arbitrary code might wish to return an eof
object. For example, a DBMS interface might have a routine that
returns one row, as a list or a vector, at a time; after the last, it
is perfectly reasonable to return an eof object.

An argument against providing this is that the constructor may be
trivially written, as shown [below]. A similar argument could be
applied to `zero?`, `newline`, `quotient`, `remainder`, and `modulo`,
among others. R7RS is not afraid to provide easy-to-implement
procedures in the name of simplicity, orthogonality, or historical
compatibility.  The lack of an eof constructor is worth
remedying.

```
(let* ((p (open-input-string ""))
       (x (read p)))
  (close-port p)
  x)
```

Vote `eof-object` for a procedure of that name, or `none` to not add any such procedure.

* **Options:** eof-object, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): eof-object
* [Ganz](WG1BallotGanz.md): eof-object
* [Gleckler](WG1BallotGleckler.md): eof-object
* [Hsu](WG1BallotHsu.md): eof-object
* [Lucier](WG1BallotLucier.md): eof-object
* [Medernach](WG1BallotMedernach.md): eof-object
* [Shinn](WG1BallotShinn.md): eof-object
* [SnellPym](WG1BallotSnellPym.md): eof-object, no
* **Results:** **eof-object**, no
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> I end up generating this often enough anyway with `(read-char (open-input-string ""))`.
`Ganz`::
> Why isn't this 'make-eof-object'?
`Gleckler`::
> I don't buy Vincent's argument from the DBMS example. After all, a DBMS is not a file, so returning an end-of-file object is a strange choice. #f or '() would be equally valid. However, people do seem to create EOF objects for file-related purposes, so why not make it easy to construct them in a non-klugerous way?
`Shinn`::
> I end up generating this often enough anyway with `(read-char (open-input-string ""))`.

### #339 Restrict identifiers in library names for compatibility with file system restrictions

Currently the identifiers in library names can be any identifier.
Under this proposal, the identifiers must not include any of `| \ ?* <
" : > + [#|]] /` or control characters after escapes are expanded.

If this proposal fails, its content will be included non-normatively
as a *should not*.

Vote `yes` to restrict with *must not*.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no, unspecified
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** yes, *no*, unspecified
* **Ratios:** 4:4, 4:1
* **Rationales:**

`Cowan`::
> R6RS had enough headaches with this and ":", we should avoid the same mistake.
`Ganz`::
> 'Should not' is sufficient.
`Gleckler`::
> Operating-system level naming concerns shouldn't be pushed up to the library level. After all, some R7RS small implementations won't have a file system at all.
`Shinn`::
> R6RS had enough headaches with this and ":", we should avoid the same mistake.

### #340 Include non-normative note about the file-system based implementations of libraries

Libraries do not necessarily have any mapping to files, nor does an
implementation necessarily run on a system with a filesystem, however
for those implementations which do so it may be worth adding such a
note.

A library file contains a single library.  A library named (A1 A2 AN)
is in a file named "A1/A2/AN.sld" ("sld" for "Scheme Library
Definition" or some other standardized file extension), relative to
some "library path".  For portability, library component names should
be integers or lower-case identifiers that avoid certain prohibited
characters.  When a library or top-level imports some other library,
the corresponding file is found in the obvious way.

Alternately, this can be left entirely to WG2 and/or packaging systems
such as Snow.

Vote `yes` to add such a note or `no` to leave it out.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **no**, yes
* **Ratios:** 7:1
* **Rationales:**

`Cowan`::
> This belongs in Snow.
`Gleckler`::
> We shouldn't constrain files to contain only single libraries. Good Scheme code includes lots of small procedures and macros, and small libraries will be common, too. Forcing each into a separate file unnecessarily constrains the programmer's ability to keep similar concepts grouped naturally.
`Shinn`::
> This belongs in Snow.
`SnellPym`::
> It will help portability.

### #341 Permit ambiguous imports of identifiers which are never used

It is currently an error to attempt to import the same identifier from more
than one library into another library or a top-level program, even if the identifier is not
used anywhere in the new library or program.  That requires programmers to make an
arbitrary decision to exclude it from one library or the other.

Vote `yes` to agree with this proposal to require that, within a
single static library (not with the environment procedure where any
identifier may be subsequently used), an implementation must allow
such multiple imports if the identifier is not referenced and does not
occur in a syntax-rules template (which introduces conflicts with
low-level macros introduced by WG2).

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): unspecified, no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): undecided, no
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): yes, undecided
* [Shinn](WG1BallotShinn.md): undecided, no
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **no**, yes, undecided, unspecified
* **Ratios:** 7:1, 5:3, 6:1
* **Rationales:**

`Cowan`::
> Overall I think this is more of a pain than it's worth.
`Gleckler`::
> It's better for programmers to address the possible conflict explicitly.
`Medernach`::
> What is important is that references are non ambiguous, there are no risks if unused identifiers collide. If an identifier with multiple meanings is not used, there is no ambiguity, nor real conflict.
`Shinn`::
> I think this is too difficult to specify and implement.
`SnellPym`::
> This sounds too complex a rule for it to be a good idea, surely?

### #342 Have READ-BYTEVECTOR(!) return 0 at EOF

Currently, `read-bytevector` and `read-bytevector!` return an EOF
object at EOF; otherwise, `read-bytevector` returns a non-empty
bytevector and `read-bytevector!` returns the number of bytes read.
Returning #u8() and 0, respectively, at EOF instead would make the
results always the same type.  This change would introduce the
ambiguity that one would not be able to detect EOF when reading a
bytevector of length 0 (which is to say, not reading any bytes at
all).

Vote `zero` to return #u8() and 0 as in the proposal, and `eof-object`
to return the eof-object as in the current draft.  Vote `zero!` to
make the change only for `read-bytevector!`.

* **Options:** zero, eof-object, undecided
* **Default:** eof-object
* **Voters:**
* [Cowan](WG1BallotCowan.md): eof-object
* [Ganz](WG1BallotGanz.md): eof-object
* [Gleckler](WG1BallotGleckler.md): eof-object
* [Hsu](WG1BallotHsu.md): eof-object
* [Lucier](WG1BallotLucier.md): eof-object
* [Medernach](WG1BallotMedernach.md): eof-object
* [Shinn](WG1BallotShinn.md): eof-object
* [SnellPym](WG1BallotSnellPym.md): eof-object, zero
* **Results:** **eof-object**, zero
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> Returning the eof-object as in the current draft is ubambiguous and consistent with R6RS.
`Gleckler`::
> Returning an EOF object allows one to distinguish the zero-byte case, which is inmportant.
`Medernach`::
> Reading nothing is not the same as stopping reading.
`Shinn`::
> Returning the eof-object as in the current draft is ubambiguous and consistent with R6RS.
`SnellPym`::
> Sentinel values should be very obviously distinct, to help avoid accidents.

### #343 Editorial: divide domain explanations to be split before and after descriptions

All Scheme standards up to and including R6RS and R7RS draft-6 have
consistently placed the full domain at the beginning of each entry.
In most cases the domain consists only of the implicit type
restrictions from the prototype, but in some cases there are
additional domain restrictions that cannot be conveniently included in
the prototype such as the following `map` restrictions:

> It is an error if *proc* does not accept as many arguments as
> there are *lists* and return a single value.

It has been suggested to move this to an appropriate later point in the entry,
to put more emphasis on the initial entry description.  This has the
disadvantage of splitting the domain into two places, which can more
easily cause oversights and make quick domain confirmations difficult.

An alternative is to separate the additional domain restrictions from
the initial description, as a separate short paragraph immediately
following the prototype and possibly de-emphasized by making it smaller.
his would keep the domain in one place and still allow
let the first line of the description stand out prominently in the
initial paragraph.

Vote `start` for the status quo, `start-split` for the separate
de-emphasized option, or `later` to move additional restrictions to a
later point.

* **Options:** start, start-split, later, undecided
* **Default:** start
* **Voters:**
* [Cowan](WG1BallotCowan.md): later, start-split, start, undecided
* [Ganz](WG1BallotGanz.md): start-split, start
* [Gleckler](WG1BallotGleckler.md): start-split, start
* [Hsu](WG1BallotHsu.md): start-split, start
* [Lucier](WG1BallotLucier.md): start
* [Medernach](WG1BallotMedernach.md): start-split, start, undecided, later
* [Shinn](WG1BallotShinn.md): start-split, start, undecided
* [SnellPym](WG1BallotSnellPym.md): start-split, later, start
* **Results:** **start-split**, start, undecided, later
* **Ratios:** 7:1, 7:0, 6:1
* **Rationales:**

`Cowan`::
> I want to keep all domain information in a single place - I often know what a procedure does but need to double check only the domain, and having to jump around to find it is inconvenient.
`Gleckler`::
> Start-split is a nice compromise.
`Shinn`::
> I want to keep all domain information in a single place - I often know what a procedure does but need to double check only the domain, and having to jump around to find it is inconvenient.
`SnellPym`::
> The extra conditions tend to be "edge cases" anyway, that would deserve a thorough reading of the whole thing before dabbling with!

### #345 Should 0.0 and -0.0 be distinct in the sense of EQV?

Currently, the draft report implies that 0.0 and -0.0 must be the same
in the sense of `eqv?`, because `eqv?` defers to `=` for numbers
(with the possible exception of [NaNs](NaNs.md)).

Vote `same` for the status quo, `different` to change to "must be
different", or `unspecified` to change to "may be different".

* **Options:** same, different, unspecified, undecided
* **Default:** same
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified, different, same
* [Ganz](WG1BallotGanz.md): undecided
* [Gleckler](WG1BallotGleckler.md): unspecified
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): unspecified
* [Medernach](WG1BallotMedernach.md): unspecified
* [Shinn](WG1BallotShinn.md): undecided, same
* [SnellPym](WG1BallotSnellPym.md): same, different, unspecified
* **Results:** *unspecified*, undecided, same, different
* **Ratios:** 5:3, 4:2, 4:1
* **Rationales:**

`Cowan`::
> We should neither require nor forbid implementations from distinguishing -0.0.
`Gleckler`::
> Unless we know that most implementations have chosen to do the same thing in this case, we should leave it unspecified.
`Lucier`::
> If there are any Schemes with non-IEEE arithmetic, then this should be unspecified.
`Shinn`::
> This should fall out naturally from the definition of eqv?, which currently makes it clear that they must be the same, though we'll be revisiting this.

### #349 Define exact integers to be at least 24 bits

Currently, R7RS (tracking R5RS) does not constrain the sizes of exact
integers beyond being required to represent the indices of strings,
vectors and bytevectors.

R6RS requires systems to support "practically unlimited" size exact
integers.  It also requires that a subset of these exist, called
*fixnums*, which must support at least the range -2^23^ to 2^23^-1.
(All practical Schemes have larger ranges for their fixnums).
This proposal suggests that we adopt this range as
the minimum range of R7RS exact integers.

The immediate issue here is that a library name may contain
(non-negative) exact integers as well as identifiers in R7RS.  For
such names to be portable, there must be a portable range of exact
integers.

See [FixnumInfo](FixnumInfo.md) to see what 39 existing Schemes do.

Vote `24` to require 24 bits of precision, `16` to require 16 bits of precision,
or `none` to leave this entirely unspecified.

* **Options:** 24, 16, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): 16, 24
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): 16, no
* [Hsu](WG1BallotHsu.md): 24
* [Lucier](WG1BallotLucier.md): 16, no
* [Medernach](WG1BallotMedernach.md): 16, 24, no, undecided
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): 16, 24, no
* **Results:** **no**, undecided, 16, 24
* **Ratios:** 6:0, 6:0, 6:0
* **Rationales:**

`Cowan`::
> Alex says: "R7RS small does not make the ugly 'fixnum' distinction, and I don't see why we should set such arbitrary minimums, or forbid simplistic Scheme implementations on 16-bit machines." All very well, but as long as we allow numbers in module names (basically introduced for the sake of SRFIs), we need to say something about the portability of such values.
`Gleckler`::
> Twenty-four is too many bits to require for tiny implementations. I'm nervous about burdening the smallest implementations with even a sixteen-bit requirement, but such implementations typically already leave out significant language features, so I'm willing to ask for 16 bits.
`Medernach`::
> Integers in library names are often used as a practical solution to manage dependencies (even if this is not completely satisfactory IMHO). We should have at least be safe to make comparisons between versions, so a portable range is necessary.
`Shinn`::
> R7RS small does not make the ugly "fixnum" distinction, and I don't see why we should set such arbitrary minimums, or forbid simplistic Scheme implementations on 16-bit machines.

### #354 mutating exports

We define mutating imports to be an error, however
the standard currently says nothing about what
happens when an exported binding is mutated from
within the library where it's defined.
In many common library implementations there
will be no effect (i.e. the import effectively gets
a copy of the original), whereas in a namespace
based implementation the change will be reflected,
so a conservative approach is to add a note saying
the result is unspecified.

Vote `shared` to force the binding to be shared
and the change reflected everywhere it's imported,
`separate` to force the binding to be separate,
`none` to make no comment, and `unspecified`
or `error` to add a clarification to the standard
to that effect.

* **Options:** shared, separate, none, unspecified, error, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Ganz](WG1BallotGanz.md): shared, error, unspecified
* [Gleckler](WG1BallotGleckler.md): shared, unspecified
* [Hsu](WG1BallotHsu.md): error, shared, no
* [Medernach](WG1BallotMedernach.md): unspecified
* [Shinn](WG1BallotShinn.md): unspecified
* [SnellPym](WG1BallotSnellPym.md): error, unspecified, no, separate, shared
* **Results:** **unspecified**, error, shared, no, separate
* **Ratios:** 4:3, 4:3, 6:1, 6:0
* **Rationales:**

`Cowan`::
> This was the original intention, and leaving it out was an oversight.
`Ganz`::
> I don't see any use for separate bindings, other than potential convenience of implementation.
`Gleckler`::
> I vote "shared" because a library should be able to mutate its own binding, particularly when the programmer is making changes in a REPL. In addition, "shared" makes importing libraries behave as if they had closed over the binding that will be mutated the way that closures do over lexically enclosing bindings. It's easy to share a cell to make this work.
`Hsu`::
> It should be an error that an implementation try to export an assigned variable. This should happen at the time of the library definition IMO.
`Shinn`::
> This was the original intention, and leaving it out was an oversight.

### #358 change epoch of current-second

A formal comment has proposed changing the epoch of current-second to
1970-01-01 00:00:00 TAI rather than 1970-01-01 00:00:10 TAI (00:00:00
UTC).

The actual time systems are independent of an epoch - the epoch is
just convenient for computer systems.

The UTC-centric epoch was chosen (despite the use of TAI time) mostly
because it is used in popular TAI times such as libtai and Olson's
time library.

See http://lists.scheme-reports.org/pipermail/scheme-reports/2012-March/001943.html for more details.

Vote `utc` for the current draft's start-of-1970-in-utc epoch, or
`tai` for the proposed start-of-1970-in-tai epoch.

* **Options:** utc, tai, undecided
* **Default:** utc
* **Voters:**
* [Cowan](WG1BallotCowan.md): tai
* [Ganz](WG1BallotGanz.md): tai
* [Gleckler](WG1BallotGleckler.md): tai
* [Hsu](WG1BallotHsu.md): undecided
* [Medernach](WG1BallotMedernach.md): tai
* [Shinn](WG1BallotShinn.md): undecided, utc
* [SnellPym](WG1BallotSnellPym.md): utc, tai
* **Results:** **tai**, undecided, utc
* **Ratios:** 5:2, 4:2
* **Rationales:**

`Cowan`::
> I originally proposed `TAI - 10`, but on reflection I think people implementing this from scratch are less likely to get it wrong if it's based on `TAI - UTC` (34 seconds at present) rather than `TAI - 10 - UTC` (24 seconds).
`Gleckler`::
> If we're using TAI time, we should use the TAI epoch. The ten-second skew is just random, and leaving it in is just asking for errors in code that is already error-prone for other reasons.
`Shinn`::
> I actually haven't been able to find any libraries which use the TAI epoch.
