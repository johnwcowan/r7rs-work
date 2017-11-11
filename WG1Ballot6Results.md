
# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

# WG1 Ballot Items To Finalize By July 31

## WG1 - Core

### #460 Semantics of `eqv?`

Earlier we voted on #125, #229 and #345 separately without regard to
the formal semantics of `eqv?` from a top level.  We need to first
decide what the definition of `eqv?` is, and consider if there should
be any exception cases as a secondary effect.

The debate is fundamentally one of whether we define `eqv?` in terms
of "operational equivalence" as in R6RS or a simpler rule
(e.g. resolve by `=`) as in earlier standards.

R2RS had the simplest historical rule which was simply to use `=`.

The term "operational equivalence" appears in R3RS but for numbers the
definition is the same as in R4RS and R5RS, which is `=` plus the same
exactness.  This is the `r5rs` option, with the "true" cases written formally as:

```
  The `eqv?` procedure returns #t if:

  (1) obj1 and obj2 are both booleans and are the same according
  to the `boolean=?` procedure.

  (2) obj1 and obj2 are both symbols and are the same symbol
  according to the `symbol=?` procedure.

  (3) obj1 and obj2 are both numbers, have the same exactness, are
  numerically equal (see `=`).

  (4) obj1 and obj2 are both characters and are the same
  character according to the `char=?` procedure.

  (5) obj1 and obj2 are both the empty list.

  (6) obj1 and obj2 are pairs, vectors, bytevectors, records,
  or strings that denote the same location in the store.

  (7) obj1 and obj2 are procedures whose location tags are equal.
```

Note that (7) is an exception case which will be decided
separately in #125.  Furthermore, an exception to make [NaNs](NaNs.md)
unspecified regardless of the semantics here will be decided in
#229.

The `r6rs` vote replaces (3) with the following operational
equivalence semantics:

```
  (3.1) obj1 and obj2 are both exact numbers and are numerically
  equal (see `=`)

  (3.2) obj1 and obj2 are both inexact numbers, are numerically
  equal (see `=`), and yield the same results (in the sense of
  `eqv?`) when passed as arguments to any other procedure that
  can be defined as a finite composition of Scheme’s standard
  arithmetic procedures.
```

where "standard arithmetic procedures" refers arguably to either
11.7 or 11.7.4.3.  For R7RS it would apply to the "Numbers"
section 6.2.6.  R6RS further adds an extra case which is not
applicable because we don't guarantee record-types are first-class
objects:

```
  (8) obj1 and obj2 are record-type descriptors that are
  specified to be `eqv?` in library section on “Procedural
  layer”.
```

The `r6rs/all` option changes (3.2) to a finite composition of
any of the implementations arithmetic procedures.  The intention
is that `decode-float` if provided could distinguish [NaNs](NaNs.md), but
something like `eq?` (which could distinguish the same bit
pattern in different locations) would not be considered
arithmetic and not apply.  This does leave the
definition "arithmetic" open to some interpretation.

In contrast to R6RS, R7RS does not require the full numeric tower.
This means that any definition of operational equivalence would render
many numbers unspecified from the perspective of the standard, yet
users could rely on consistency within their own implementation, and
broad agreement amongst most implementations which provide the full
tower.

Finally, the `same-bits` option replaces (3) with:

```
  (3.1) obj1 and obj2 are both exact numbers and are numerically
  equal (see `=`)

  (3.2) obj1 and obj2 are both inexact real numbers conforming to the
  IEEE 754-2008 standard, and they have the same radix,
  precision, maximum exponent, sign, exponent, and significand as
  described in IEEE 754-2008

  (3.3) obj1 and obj2 are both inexact real numbers, are not implemented using
  IEEE 754-2008, and are numerically equal (see `=`)

  (3.4) obj1 and obj2 are both complex numbers whose real and imaginary
  parts are `eqv?`
```

Keep in mind the semantics of `eqv?` also affect `memv`, `assv` and
`case`.

* **References:**
* [eqv? issues summarized](https://groups.google.com/d/msg/scheme-reports-wg1/BGvDFtD6A1M/5pHmfXHtvEIJ)
* [the history of eqv? on numbers](https://groups.google.com/d/msg/scheme-reports-wg1/2Nv6oIND8HI/Z2HXPQMNFooJ)
* **Options:** r5rs, r6rs, r6rs/all, same-bits
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs/all, r6rs
* [Ganz](WG1BallotGanz.md): same-bits, r5rs, r6rs/all, r6rs
* [Gleckler](WG1BallotGleckler.md): same-bits, r5rs, r6rs, r6rs/all
* [Hsu](WG1BallotHsu.md): r6rs, r6rs/all
* [Lucier](WG1BallotLucier.md): same-bits, r6rs/all, r6rs
* [Medernach](WG1BallotMedernach.md): same-bits, r6rs/all, r6rs, r5rs
* [Radul](WG1BallotRadul.md): same-bits, r5rs
* [Read](WG1BallotRead.md): same-bits, r6rs, r6rs/all, r5rs
* [Shinn](WG1BallotShinn.md): r5rs, r6rs/all, same-bits, r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs, r5rs, (r6rs/all   same-bits)
* **Results:** **same-bits**, r6rs/all, r6rs, r5rs
* **Ratios:** 6:3, 7:3, 6:2
* **Rationales:**

`Cowan`::
> On systems where `0.0` and `-0.0` are mathematically distinct, they should be distinct in the sense of `eqv?` as well, and the same for `1.0+2.0i` and `1.0+2i`.
`Ganz`::
> I sympathize with the motivation of `r6rs` in getting at "true" operational equivalence, but agree with Alex that this eventually becomes inseparable from the machine implementation, and is thus not suitable for its most prominent use. `same-bits` is of course also somewhat implementation-dependent, but in a more controlled way.
`Gleckler`::
> I was impressed by Bradley's arguments that same-bits matches IEEE 754 and is therefore superior. The R6RS semantics are defined too indirectly and are too dependent on the specific combination of numeric features supported by the implementation. The R5RS semantics are clearer. I would argue for an option that is the same as r5rs but distinguishes between positive and negative zero in the same way as we already distinguish between exact and inexact, but that does not appear on the ballot. I don't agree with choosing `r6rs/all` over `r6rs`. Implementation-specific procedures like `decode-float`, the example given above, are specifically designed to expose implementation details, whereas `eqv?` has generally been used to compare numbers arithmetically. If we a[...](WG1BallotGleckler.md)
`Hsu`::
> The R6RS option seems to provide more rigor, in particular in determining actual equivalence, rather than "mathematical equality." The notion provided by R6RS is useful and important, and without it, I do not see a good way to portably provide both `=` and the operational equivalence notions. Let us have the distinctions. You can get back to the R5RS version yourself if you really need it.
`Read`::
> Bradley Lucier provides a good case for 'same-bits'. The notion of sameness of two floats is well-specified in IEEE 754; we should use it.
`Shinn`::
> I don't think the operational semantics of R6RS is well defined, nor is it appropriate when so much is unspecified about the numeric tower. If we are going to use an operational semantics, however, it should reflect the full semantics of the implementation so as to be usable for memoization, so I put r6rs/all before r6rs.
`SnellPym`::
> My gut feeling is that eqv? should be about equivalence, so I like the "operationally indistinguishable" notion; yet I also feel that anything which peeks beneath the hood of the numeric abstraction, such as bitwise comparison or implementation-provided introspection features, should be exempt from that.

### #229 eqv? and NaN

As announced previously this is being re-opened due to incorrect
formulation in the previous ballot, and in response to formal comment
#423.

Regardless of the result of #460, the semantics implies that `eqv?`
return `#f` on comparing any two NaN objects.  It is reasonable to
want to consider any two [NaNs](NaNs.md) as the "same" since they behave the same
under any operation, even though none of the results are `=`.  Moreover,
it is very common to use a shortcut `eq?` pointer comparison before
falling back on general `eqv?` logic.  In deference to this R6RS makes
an exception and allows the result to be unspecified, and we should
consider allowing this exception.

This proposal is only to allow an explicit exception to make
NaN comparisons unspecified, regardless of the semantics.
Vote `no-exception` (or anything other than `unspecified`)
to require NaN comparisons to follow directly from #460.

The default of `unspecified` still holds from the previous invalidated
ballot.

* **Options:** no-exception, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Ganz](WG1BallotGanz.md): unspecified
* [Gleckler](WG1BallotGleckler.md): unspecified
* [Hsu](WG1BallotHsu.md): unspecified
* [Lucier](WG1BallotLucier.md): no-exception
* [Medernach](WG1BallotMedernach.md): unspecified, no-exception, undecided
* [Radul](WG1BallotRadul.md): no-exception
* [Read](WG1BallotRead.md): no-exception, unspecified
* [Shinn](WG1BallotShinn.md): unspecified, different
* [SnellPym](WG1BallotSnellPym.md): no-exception, unspecified
* **Results:** **unspecified**, no-exception, undecided, different
* **Ratios:** 6:4, 8:0, 8:0
* **Rationales:**

`Cowan`::
> This is a marginal case which is not worth prescribing. I would have preferred all [NaNs](NaNs.md) to be `eqv?` to each other, but that's probably not achievable.
`Ganz`::
> I think it is reasonable to consider two of the "same" [NaNs](NaNs.md) to be `eqv?`, and see no reason not to specify this behavior. The specified behavior of `=` on numbers is not relevant to [NaNs](NaNs.md).
`Gleckler`::
> There is not enough agreement about this to make a requirement. NaN's can be addressed specially by code that uses them.
`Hsu`::
> This seems like a reasonable exception to make, and should not hurt in any case.
`Medernach`::
> It will depend on the #460 item result, but personnaly I would prefer to be able to distinguish different [NaNs](NaNs.md) and to identify identical [NaNs](NaNs.md) ("same" vote).
`Radul`::
> As I interpret same-bits, it would actually require eqv? to return #t on two IEEE [NaNs](NaNs.md) that did, in fact, have the same pattern of bits. This case satisfies all rationales I can think of for wanting [NaNs](NaNs.md) to compare eqv?.
`Read`::
> [NaNs](NaNs.md) which may be distinguished are part of the IEEE 754 abstraction; eqv? should distinguish them even if = doesn't.
`Shinn`::
> Mostly because there is so much disagreement about this, and the `eq?` shortcut is so common, I'm OK with this exception.
`SnellPym`::
> At this level of abstraction, a NaN is a NaN; differentiating [NaNs](NaNs.md) is peeking below the abstraction again.

### #125 Allow procedures not to be locations (making EQV? unspecified in some additional cases)

Procedures are another case for contention with `eqv?`.  In R3RS, an
operational equivalence was defined for procedures, and this was
subsequently removed.

R6RS went the other direction and allowed the exact same procedure `x`
to return `#f` for `(eqv? x x)`, and R7RS currently reaffirms this.
The rationale behind this is for compiler optimizations such as
inlining local procedures, that is given:

```
(let ((square (lambda (x) (* x x))))
  (eqv? square square))
```

it is reasonable for a compiler to perform the optimization:

```
(eqv? (lambda (x) (* x x)) (lambda (x) (* x x)))
```

in which case the result would naturally return `#f`.

Vote `yes` to allow the result to be `#f`.

The default of `yes` still holds from the previous invalidated ballot.

* **Options:** yes, no, undecided
* **Default:** yes
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes, undecided, no
* [Radul](WG1BallotRadul.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 6:4, 6:0
* **Rationales:**

`Cowan`::
> Although this means that procedures aren't quite first-class (you can't reliably use `memv` to determine if a procedure can be found in a list), I think we don't want to renege on the license to return `#f` that R6RS provided and that optimizing compilers exploit.
`Ganz`::
> I interpret this as `(eqv? x x)`, where `x` is a variable. As I think was pointed out in the discussion, this example is not much different from: {{{ (let ((cell (cons 3 4))) (eqv? cell cell)) }}} so the results should be consistent. Also, this doesn't seem like a good reason to give up having `eqv?` be an approximation of `eq?`.
`Gleckler`::
> Absolutely not. This would mean that even `eq?` wouldn't work after such an optimization.
`Hsu`::
> Allowing `#f` here allows for optimizations that does not result in the need for an additional pass or more to manage this notion of equivalence. Instead, after the optimization, if things still work out to be `#t`, then that is fine, but otherwise, there is no reason to expect that they should.
`Medernach`::
> The problematic R5RS paragraph is the following (end of section 4.1.4): Each procedure created as the result of evaluating a lambda expression is (conceptually) tagged with a storage location, in order to make `eqv?' and `eq?' work on procedures (see section *note Equivalence predicates::). I have the tendency to think that (eqv? p p) should always return #t whatever p may be, at least when this is so syntactically manifest. However the point here is more about forcing all procedures to have location or not (i.e. deciding "Does lambda is an allocator or not ?"). Inlined functions or unrolled loops are examples of such "beasts". As such optimizations are really worth to have, especially in our functional programming world, I recommend: - either not using (hen[...](WG1BallotMedernach.md)
`Radul`::
> I have argued against this on the list in the past, as being the beginning of a slippery slope that has no end. It occurs to me now that procedures-with-locations are definable from procedures-without-locations if the implementation supports apply-hooks and gensyms (the latter of which I expect that most implementations will have to, in one or another form, if nothing else because cons is generative). This means that a complete, performance-conscious implementation should offer both tagged procedures and untagged procedures, and the question becomes which one should you get when you type "lambda" into the initial environment. I vote for retaining the invariant that for any variable x, (eqv? x x) is true unless one is using an implementation extension (like s[...](WG1BallotRadul.md)
`Shinn`::
> I think this is too surprising and contrary to the spirit of `eqv?`. Compilers can always detect the use of `eqv?` on a procedure value and skip this optimization.

### #393 Defining record equality

Currently, R7RS-small says that when `equal?` is applied to records
that are not `eqv?` (that were constructed by different calls to the
record constructor), the result may be `#t` or `#f` at the
implementation's discretion.  The proposal is to treat records of the same
type like pairs, strings, vectors, and bytevectors: that is, their
contents are recursively compared.

Vote `recursive` to require recursive comparison of the record's
fields, `identity` to return `#t` iff `eqv?` does, and `unspecified`
to leave this open.

Note `equal?` is already required to handle cycles regardless.

* **Options:** recursive, identity, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Ganz](WG1BallotGanz.md): recursive
* [Gleckler](WG1BallotGleckler.md): recursive
* [Hsu](WG1BallotHsu.md): unspecified
* [Lucier](WG1BallotLucier.md): recursive, unspecified
* [Medernach](WG1BallotMedernach.md): identity, unspecified, undecided, recursive
* [Radul](WG1BallotRadul.md): unspecified
* [Read](WG1BallotRead.md): recursive
* [Shinn](WG1BallotShinn.md): unspecified
* [SnellPym](WG1BallotSnellPym.md): recursive, identity, unspecified
* **Results:** **unspecified**, recursive, identity, undecided
* **Ratios:** 5:5, 5:2, 7:0
* **Rationales:**

`Cowan`::
> Existing systems shouldn't have to change their `equal?` to conform to R7RS. What's more, record equality is `eqv?` on some systems by default and not on others.
`Ganz`::
> This is what `equal?` does.
`Gleckler`::
> This is merely making `equal?` on records consistent with `equal?` on other data types.
`Hsu`::
> While I would like to make this recursive, we cannot do so without prejudicing the core against implementations that favor opaque records in some sense. If we had a means of expressing opacity, then this would not be a problem, but since we cannot reasonably talk about opacity, we should leave the effects of opacity in the record system unspecified.
`Medernach`::
> I believe that one may have 2 records containing identical fields and despite that being semantically distincts. (records have "bag" behaviour instead of set behaviour).
`Radul`::
> I don't think WG1 is in a position to solve this problem. If equal? is recursive, it becomes impossible to use records for information hiding or security. If equal? is identity, the common (at least in my code, and in what I understand to be good style) case (of records just being transparent immutable named tuples) breaks. In either case, the essential problem is that if you want the non-default behavior for a particular record type, there is no way to effectively modify equal?, without rewriting it and rewriting every procedure that uses equal? internally (member comes to mind; even if there are no others in the standard, there will be others in libraries). So the "right answer" to this involves some kind of extensible or generic operation system, which we[...](WG1BallotRadul.md)
`Read`::
> Compatible with our expectations of `equal?`
`Shinn`::
> I think recursive is the Right Thing, but appreciate this is very controversial and moreover that it may be difficult for implementations to support.
`SnellPym`::
> I see a desire for both "open" records that are vectors with named slots, and "closed" records that are opaque separations of implementation from interface. I definitely want WG2 Scheme to provide both these options as part of an extended record system, along the related issues of read/write on record objects and record introspection. So, what we're really deciding here is both what limited WG1-only schemes will provide, and what the default will be. On the latter basis, I think that open records are the sensible default as secure sandboxing is a rather specialise case, and I'm undecided on the former basis. So I go for open records on the latter basis alone :-) However, to make it clear, I want the wording such that this is the defined behaviour for records[...](WG1BallotSnellPym.md)

### #306 What does "full Unicode" support mean in Appendix B?

Specifically, does it indicate case-folding and normalization support
for the repertoire of any particular version of Unicode, or any
version greater than 5 or 6 or 6.1, or no particular version?

Full unicode refers to the set of characters available.  Case-folding
and character predicates are required to work according to the Unicode
standard for all supported characters.  The question of which version
of Unicode the property refers to is important.  We could require a
specific version (and all further revisions), or always require the
latest official Unicode standard, in which case an implementation
would not be compliant until it was updated with each new standard.  Alternatively, we could parameterize the feature identifier, so that implementations might claim to support (full-unicode 6), (full-unicode 6.1), etc.

* **Options:** at-least-6, at-least-6.1, latest, parameterize, undecided, unspecified
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): at-least-6
* [Ganz](WG1BallotGanz.md): parameterize, latest, at-least-6.1, at-least-6
* [Gleckler](WG1BallotGleckler.md): at-least-6, at-least-6.1, unspecified
* [Hsu](WG1BallotHsu.md): undecided, at-least-6, at-least-6.1, latest
* [Lucier](WG1BallotLucier.md): unspecified
* [Medernach](WG1BallotMedernach.md): at-least-6, latest
* [Read](WG1BallotRead.md): unspecified
* [Shinn](WG1BallotShinn.md): at-least-6, at-least-6.1
* [SnellPym](WG1BallotSnellPym.md): unspecified, (latest   at-least-6   at-least-6.1)
* **Results:** **at-least-6**, at-least-6.1, unspecified, latest, parameterize, undecided
* **Ratios:** 5:1, 6:3, 5:1, 6:1, 6:1
* **Rationales:**

`Cowan`::
> Requiring the latest major version is reasonable, since R7RS will require at least a refresher release for almost all implementations that adopt it, but there's no reason to huff and pant to keep up with the latest minor version.
`Ganz`::
> `latest` seems easier than having to update the standard in order to get a guarantee that the next version of unicode is supported. But parameterizing the feature identifier with a version number allows for new unicode versions without either implementations becoming non-compliant or the standard becoming out-dated.
`Gleckler`::
> Specifying `latest` is unrealistic. That would mean that existing Scheme implementations would fall out of compliance the moment a new Unicode standard was promulgated. Specifying a point version, e.g. 6.1, is also unrealistic, as the version of Unicode that a particular implementation is reasonably able to support depends so much on underlying facilities provided, for example, by the operating system.
`Shinn`::
> We should tie this report to the latest major version of Unicode. Later standards can consider Unicode 7, etc., and in the meantime implementations can continue to add new characters from minor releases. It's highly unlikely characters will ever be removed, but in such a case Scheme implementations will be required to support old characters, and allowed to support new ones.
`SnellPym`::
> I think an implementation should specify what version of Unicode it supports, but that this should be orthogonal to the Scheme language it supports.

### #458 Remove the formal semantics from the report

There have been a bunch of complaints about the formal semantics: it's
incomplete, it cannot be mechanized with a proof assistant, it doesn't
help either users or implementers very much, and so on.  See in
particular #453.

The semantics have been updated to include `dynamic-wind`, however the
other arguments still hold.

This proposal is to remove it from the report altogether, and to urge
the Steering Committee to create a new WG to produce one, likely in a
"rolling" style with increasingly comprehensive releases, on its own
schedule.  Some members of the current WG have expressed interest in
serving on such a group, and others have expressed their complete lack
of interest, so a new WG seems the best choice if this is done.

Alternately, we can adapt the operational semantics from R6RS.

* **Options:** remove, keep, operational, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): remove
* [Ganz](WG1BallotGanz.md): keep
* [Gleckler](WG1BallotGleckler.md): keep, remove
* [Hsu](WG1BallotHsu.md): remove, operational, keep
* [Lucier](WG1BallotLucier.md): keep, operational, remove, undecided
* [Medernach](WG1BallotMedernach.md): remove
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): keep, operational
* [SnellPym](WG1BallotSnellPym.md): remove, operational, keep
* **Results:** remove, *keep*, operational, undecided
* **Ratios:** 4:4, 5:2, 6:1
* **Rationales:**

`Cowan`::
> The formal semantics broken as written, and even if we patched it up, it would still be less than helpful. We need to let a separate group do a better job.
`Ganz`::
> I agree that the semantics deserve a focused effort and that this is best not attempted within the current WG, so I support urging the Steering Committee to form a new WG for this purpose. However, I am not comfortable with releasing a standards document based only on English descriptions. We should go with what we have and let the new WG improve and replace it, especially since the new semantics is intended to be rolled out iteratively anyway and since a justification for this was to have us not be without a formal semantics. The document can be modified to specify authors involved with that second phase.
`Gleckler`::
> I've never understood the fascination with formal semantics, particularly considering that they're non-normative, i.e. that the English has always taken precedence. We always get the English right, then adjust the formal semantics to match. In all my years of using the RnRS specifications for reference, I have not once referred to the formal semantics in order to understand some point of the language. Nevertheless, the formal semantics do matter a lot to many people in the community. Removing them, even temporarily, will probably cause us to lose support for the draft. I'm afraid that we're going to have to find a way -- some volunteers? -- to bring that part of the document up to date. There's no point in adopting the operational semantics from R6RS since i[...](WG1BallotGleckler.md)
`Hsu`::
> I dislike the idea of having a non-normative semantics that will likely not be used, which must nonetheless take time from the WG to get right. I want a normative semantics done as well as can be, which requires, IMO, a separate group. However, if we do keep things around, we should use the more up to date and partially mechanized operational semantics of R6RS.
`Medernach`::
> Please produce it in a separate forthcoming document, in order for experts to have enough time to do it as well as possible.
`Shinn`::
> The primary objections have been addressed - the semantics are now up-to-date - and this is a piece of Scheme history that should not just be thrown away. We can add alternate formal semantics later if we want, but let's keep this.
`SnellPym`::
> I think keeping a flawed semantics helps nobody.

### #398 Allow repeated keys in `case`

R5RS says it's an error for a key to appear in more than one clause of
`case` (or twice in the same clause, but that's trivial).  R6RS allows
the same key to appear more than one clause, but insists on
left-to-right testing throughout, like `cond`.  The R6RS editors
thought this was better for machine-generated code, though worse for
hand-written code.

The proposal is a compromise: allow keys to appear in more than one clause,
but behave as if the key appeared only in the first (leftmost) clause.
This allows hash-table or other non-left-to-right implementations.

* **Options:** r5rs, r6rs, leftmost, unspecified, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): leftmost, r6rs
* [Ganz](WG1BallotGanz.md): r5rs
* [Gleckler](WG1BallotGleckler.md): leftmost, r6rs
* [Hsu](WG1BallotHsu.md): r6rs, leftmost
* [Lucier](WG1BallotLucier.md): r5rs, leftmost
* [Medernach](WG1BallotMedernach.md): r5rs, leftmost, r6rs, unspecified, undecided
* [Radul](WG1BallotRadul.md): leftmost
* [Read](WG1BallotRead.md): unspecified
* [Shinn](WG1BallotShinn.md): r5rs, unspecified
* [SnellPym](WG1BallotSnellPym.md): r5rs, leftmost, r6rs
* **Results:** **r5rs**, leftmost, r6rs, unspecified, undecided
* **Ratios:** 5:4, 5:3, 5:1, 5:0
* **Rationales:**

`Cowan`::
> `Leftmost` is a reasonable compromise between R5RS and R6RS; the latter overspecifies the mechanism.
`Ganz`::
> Machines can be smart enough to generate each key only once.
`Gleckler`::
> The argument from machine-generated code, e.g. macros, makes perfect sense, but the `leftmost` proposal gives implementations more freedom to achieve the same ends.
`Hsu`::
> This ticket is inaccurate. A hash-table or other non-left to right implementation is perfectly valid in R6RS' world, because you cannot tell the difference between that and a left to right evaluation if the keys are all distinct. If they are not all distinct, then the R6RS is still equivalent to the leftmost option, because the only time that the ordering matters is in duplicate keys. Thus, r6rs and leftmost are equivalent, and in fact, superior to R5RS.
`Medernach`::
> There is a risk that repeated keys are an oversight. Being silent in that case is not recommended.
`Shinn`::
> I would need to see specific examples of generated code where this is useful, otherwise I fear we're just masking errors.

### #85 Blobs, bytevectors, byte-vectors, octet-vectors, or something else?

Following exactly in the footsteps of R6RS we voted for a `blob` API
and then changed the name to `bytevector`.

Formal comment #435 argues that `u8vector` is in more common use, so
this item is being re-opened.  The default is the current draft
`bytevector`, and for any member leaving the preferences are left
blank their votes from ballot 3 will be used.

* **Options:** blob, bytevector, byte-vector, u8vector, octet-vector, undecided
* **Default:** bytevector
* **Voters:**
* [Cowan](WG1BallotCowan.md): blob, bytevector
* [Ganz](WG1BallotGanz.md): bytevector, byte-vector, u8vector
* [Gleckler](WG1BallotGleckler.md): byte-vector, bytevector, u8vector, octet-vector, blob
* [Hsu](WG1BallotHsu.md): bytevector, blob, byte-vector, u8vector
* [Lucier](WG1BallotLucier.md): u8vector, bytevector
* [Medernach](WG1BallotMedernach.md): bytevector, byte-vector
* [Read](WG1BallotRead.md): bytevector, blob
* [Shinn](WG1BallotShinn.md): bytevector, blob, u8vector
* [SnellPym](WG1BallotSnellPym.md): blob, (bytevector byte-vector), u8vector, octet-vector
* **Results:** **bytevector**, blob, byte-vector, u8vector, octet-vector
* **Ratios:** 7:2, 7:1, 8:1, 9:0
* **Rationales:**

`Cowan`::
> Neither `bytevector` nor `u8vector` fully captures the idea here, which is of a collection of unstructured binary information. `Blob` does that nicely. Failing that, at least there is R6RS as precedent for using `bytevector` in this vague way.
`Ganz`::
> bytevector is in common usage, based on a google search. [the gnu list java classes](http://www.gnu.org/software/kawa/api/gnu/lists/ByteVector.html) consider a bytevector to have signed and unsigned subclasses. By this definition, the difference turns on whether, if we were to support constructors `#s8(...)` and `bytevector-s8-set!`, whether these would be considered to operate over a single type of bytevectors, or two different types. If the former, we should use `bytevector`, if the latter `u8vector`. I don't see much purpose for converting the same bit pattern between different interpretations, but parsimony would argue for a single type.
`Gleckler`::
> While "blob" is a widely used term these days, I prefer a properly hyphenated, descriptive term, or at least a descriptive term. I've never liked "blob," even knowing that it abbreviates "Binary Large OBject." Since R7RS is case sensitive, we don't need even more cases where the capitalization of an abbreviation is not matched in the names of Scheme identifiers. Furthermore, it has been ages since "byte" meant anything other than eight bits, so there's no need to choose "u8" or "octet." While Marc's argument that we (and implementers of the widely adopted SRFI 4) use "#u8(" to prefix literal byte vectors is compelling, the idea of byte vectors is to provide the basis for a more complete system in WG2 Scheme that supports reading and writing not only unsigned[...](WG1BallotGleckler.md)
`Medernach`::
> blob or u8vector are IMHO terrible names. byte-vector is not perfect but conveys meaning.
`Shinn`::
> I don't particularly like the name, but it has precedence in R6RS. The name u8vector doesn't work well when accessing by other types, e.g. u8vector-u32-ref sounds too strange.

## WG1 - Library System

### #353 No use before import in libraries

For ease of implementation, the proposal is to make it an error for an
imported identifier to be referenced or defined in a library before
the library declaration that imports it.  This allows strict
left-to-right processing of library declarations, with no need to
delay processing till the end of the library.

Therefore, this would be an error (but still permitted as an extension
in Schemes that can easily provide it):

```
(module
  (begin (define x y))
  (import (library defining y))
```

This would necessitate replacing the penultimate paragraph of section
5.5.1 with:

One possible implementation of libraries is as follows: After all
`cond-expand` library declarations are expanded, a new environment is
constructed for the library consisting of all imported bindings. The
expressions and declarations from all `begin`, `include`, and
`include-ci` declarations are expanded in that environment in the
order in which they occur in the library declaration.  Alternatively,
`cond-expand` and `import` declarations may be processed in left to
right order interspersed with the processing of expressions and
declarations, with the environment growing as imported bindings are
added to it by each `import` declaration.

Vote `yes` to add the restriction, or `no` to leave it out.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): undecided
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 6:2, 6:1
* **Rationales:**

`Cowan`::
> This is how Chicken works, and I think left-to-right processing is a Good Thing whenever possible. Note that this has nothing to do with the fact that the library form is read all at once, and everything to do with how it is processed.
`Ganz`::
> The restriction doesn't seem to do any harm. One could always define multiple libraries with just the desired dependencies.
`Gleckler`::
> I'm not sure why we even allow `begin`, `include`, or `include-ci` before any `import` or `export` form. The proposed restriction is not limiting, and simplifies not only implementing the library system, but also reading code that uses it.
`Hsu`::
> One can arbitrarily re-order clauses in a library, and one should at least read the entire library in to check for valid syntax before one begins processing. All that is required is to re-order the clauses into a correct ordering before processing to enable left-to-right processing. This is trivial and not difficult for any implementation. This restriction is unhelpful verbage with no real use.
`Shinn`::
> I'm worried about interaction with include-library-declarations from #448 where it can be useful to change the order. This may also conflict with requirements such as #441.
`SnellPym`::
> Use before import doesn't make for readable code, and makes for trickier implementation; nobody benefits!

### #359 Limit numbers used in library names

This is a proposal to limit numbers in library names to the range 0 to
32767.  Currently, there is no portable lower bound which all Schemes
can assume as the maximum size of an integer.

Numbers are mostly used for SRFI-based libraries anyway, which are not
likely to reach either limit.

The option `uint15` for the proposal as stated (0 to 32767), `int16`
for -32768 to 32767, int24 for -2^23^ to 2^23^-1, etc.

Vote `unspecified` to make no explicit requirement on the integers
allowed in library names.

* **Options:** uint15, int16, uint16, int24, uint24, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): uint15, int16, uint16, int24, uint24
* [Ganz](WG1BallotGanz.md): unspecified
* [Gleckler](WG1BallotGleckler.md): int16, uint16, unspecified, int24, uint24
* [Hsu](WG1BallotHsu.md): unspecified
* [Lucier](WG1BallotLucier.md): uint15, unspecified
* [Medernach](WG1BallotMedernach.md): unspecified
* [Radul](WG1BallotRadul.md): uint15, uint16, unspecified
* [Read](WG1BallotRead.md): unspecified
* [Shinn](WG1BallotShinn.md): int16
* [SnellPym](WG1BallotSnellPym.md): uint15, (int16 uint16), (int24 uint24)
* **Results:** **unspecified**, uint15, int16, uint16, int24, uint24
* **Ratios:** 5:4, 6:4, 5:4, 7:2, 7:2
* **Rationales:**

`Cowan`::
> I may change this later when/if Alex explains why negative numbers are useful in library names. We haven't had them up to now.
`Ganz`::
> I can't imagine wanting such a large number in a library name, but maybe I'm not very imaginative.
`Gleckler`::
> Rationale (from my earlier ballot on #349): Twenty-four is too many bits to require for tiny implementations. I'm nervous about burdening the smallest implementations with even a sixteen-bit requirement, but such implementations typically already leave out significant language features, so I'm willing to ask for 16 bits.
`Hsu`::
> I just don't feel comfortable making a sudden distinction in fixnums versus bignums in the language.
`Shinn`::
> We should definitely allow negative numbers (for the old R^-1RS and (srfi -1) jokes), and 16 bits seems enough for all common uses.
`SnellPym`::
> I suppose it makes sense to have some contract between implementers and numeric-library authors.

### #441 Make sure a program/library loads any imported libraries at most once

Add the following text to the discussion of library loading:

> Regardless of the number of times that a library is loaded, each
> program or library that imports bindings from a library will receive
> bindings from a single loading of that library, regardless of the
> number of `import` or `cond-expand` declarations in which it appears.

to make it clear that, for example,

```
(import (prefix (foo) 'foo:))
(import (only (foo) bar))
```

will cause `bar` and `foo:bar` to come from the same instantiation of
the library '(foo)'

Vote `yes` to add this requirement.

* **Options:** yes, no, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes, unspecified
* [Hsu](WG1BallotHsu.md): yes, undecided
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): yes
* [Radul](WG1BallotRadul.md): yes, unspecified
* [Read](WG1BallotRead.md): unspecified
* [Shinn](WG1BallotShinn.md): undecided, yes
* [SnellPym](WG1BallotSnellPym.md): yes, unspecified, no
* **Results:** **yes**, unspecified, undecided, no
* **Ratios:** 8:1, 7:2, 8:0
* **Rationales:**

`Cowan`::
> This is not a case R6RS deals with, but I think it's sensible to require all imports from the foo library into the bar library to come from the same instantiation.
`Gleckler`::
> Yes, we don't want people to interpret `import` as a statement causing an import to happen rather than a declaration that one should. However, the wording still isn't clear, and in fact seems to suggest the opposite of its intent. I recommend this instead: Regardless of the number of times that a library is loaded, all the bindings it exports to any loading program or library will come from a single loading of that library, regardless of the number of `import` or `cond-expand` declarations in which it appears.
`Hsu`::
> This is reasonable based on the assumption that it does not preclude loading the library zero or more times, given phases or optimizations.
`Medernach`::
> By the rule of least surprise.
`Read`::
> I want to see some best practices develop before setting anything in stone here.
`SnellPym`::
> Although libraries that mutate their private internal state are a risky programming paradigm, it is occasionally useful for situations where we must manage process-wide resources such as signals. Ensuring that any given libary is instantiated (and initialised) only once is, therefore, useful.

### #402 Add an export-all form.

Add an export-all form to the library declaration that means "export
all identifiers that are defined in the library with begin, include,
and include-ci but none that are imported with import."

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided, no
* [Lucier](WG1BallotLucier.md): no, undecided
* [Medernach](WG1BallotMedernach.md): no
* [Radul](WG1BallotRadul.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, yes, undecided
* **Ratios:** 5:4, 4:1
* **Rationales:**

`Cowan`::
> I think this is very useful for reducing bureaucracy in small libraries. Chicken has it.
`Ganz`::
> Some may choose other ways of hiding definitions, and this can save both keystrokes and maintenance effort.
`Gleckler`::
> This will be a common usage pattern. It's much better than the alternative suggestion, which was to make all identifiers be exported if none are. That violates the principle of least astonishment. I don't understand Alex's argument that this will be hard to implement. If something like that is hard, we're doing something wrong.
`Hsu`::
> Is this even how normal `export-all` functons work? I would normally expect it to export all of its imported bindings too.
`Medernach`::
> Not in the standard does not disallow some implementation to have it, but allow others not to provide it if they think this is an overkill.
`Radul`::
> This is definitely going to be the common case.
`Shinn`::
> I'm concerned we may make the library system too complicated to easily implement. It's also unclear whether `export-all` should export imported identifiers or not, and contrary to what John Cowan says Chicken does not provide this functionality.
`SnellPym`::
> I think it's good discipline to explicitly export stuff. It prevents accidental pollution of namespaces.

### #448 Add library declaration include-library-declarations

The proposed `include-library-declarations` allows a library to
incorporate a file containing arbitrary library declarations, not just
Scheme code (definitions and expressions).  This allows, for example,
the exports of a module to be written directly in the library file,
and its imports in a separate file.

An alternative would be something like `(export-from <library>)` to
export the same bindings as another library.  This does require the
clumsiness of actually defining the identifiers in the other library
if it is abstract.

* **Options:** include-library-declarations, export-from, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): include-library-declarations
* [Ganz](WG1BallotGanz.md): export-from
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): undecided, no, include-library-declarations
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): include-library-declarations, no, undecided, export-from
* [Radul](WG1BallotRadul.md): undecided
* [Shinn](WG1BallotShinn.md): include-library-declarations, no
* [SnellPym](WG1BallotSnellPym.md): include-library-declarations, no, export-from
* **Results:** *include-library-declarations*, no, undecided, export-from
* **Ratios:** 4:2, 4:3, 5:1
* **Rationales:**

`Cowan`::
> Though this name is verbose even by Scheme standards, what it provides is very helpful. In simple implementations, it can be the same as include.
`Ganz`::
> This is a useful feature, and `export-from` seems like the cleaner way to achieve it.
`Gleckler`::
> I don't understand why writing the imports in a separate file is a good idea. I view the imports and exports as part of a "wiring diagram" showing how parts of the system are connected, not a way of declaring an abstract interface. In any case, this is invention, not something that exists in Scheme implementations already, so it doesn't belong in the standard.
`Hsu`::
> There is something that I just don't like about this, and it feels untested and unprecedented. I would prefer to see it implemented and encouraged by implementations. Otherwise, we may find that implementations prefer to use something else for this purpose.
`Medernach`::
> Having a simple way to separate interfaces from implementation is nice.
`Shinn`::
> This is trivial to implement and is a good poor-man's interface implementation.
`SnellPym`::
> export-from is, IMHO, an ugly hack...

### #449 Clarify library loading rules

R7RS currently says:

> Within a program, each imported library is loaded at least once, and,
> if imported by more than one program or library, may possibly be
> loaded additional times.

Richard Kelsey thinks this is too liberal, and proposes:

> Regardless of the number of times that a library is loaded, each
> program or library that imports bindings from a library will receive
> bindings from a single loading of that library, regardless of the
> number of `import` or `cond-expand` forms in which it appears.

Aaron Hsu, however, thinks this is too restrictive, and proposes
(backed up by actual R6RS implementations):

> If a library's definitions are referenced in the expanded form of a
> program or library body, then that library must be loaded before the
> expanded program or library body is evaluated. This rule applies
> transitively.

> Similarly, during the expansion of a library, if a syntax keyword
> imported from a library is needed to expand the library, then the
> imported library must be visited before the expansion of the
> importing library.

* **Proposals:**
* **one:** Kelsey's proposal
* **one-or-more:** current draft
* **zero-or-more:** Hsu's proposal, R6RS status-quo
* **zero-or-one:** Kelsey's proposal with Hsu's relaxation
* **Options:** one, one-or-more, zero-or-one, zero-or-more
* **Default:** one-or-more
* **Voters:**
* [Cowan](WG1BallotCowan.md): zero-or-more
* [Ganz](WG1BallotGanz.md): one, undecided
* [Gleckler](WG1BallotGleckler.md): one, zero-or-one, one-or-more, zero-or-more
* [Hsu](WG1BallotHsu.md): zero-or-one, zero-or-more
* [Lucier](WG1BallotLucier.md):  i have no opinion
* [Medernach](WG1BallotMedernach.md): undecided
* [Radul](WG1BallotRadul.md): one
* [Read](WG1BallotRead.md): zero-or-more
* [Shinn](WG1BallotShinn.md): zero-or-more, one-or-more
* [SnellPym](WG1BallotSnellPym.md): one, zero-or-one, (one-or-more zero-or-more)
* **Results:** *zero-or-more*, one, zero-or-one, one-or-more, undecided, i, have, no, opinion
* **Ratios:** 4:4, 3:3, 4:1, 6:2, 6:1, 6:1, 6:1, 6:1
* **Rationales:**

`Cowan`::
> I think we should defer to R6RS-based experience here. Those who don't care much can probably live with any of these options, but those who care seem to be passionate about it.
`Ganz`::
> My module proposal had allowed for the same module to be loaded multiple times, consuming and producing different bindings each time, and I think was very well-motivated. I'm a bit confused on what additional expressiveness is being offered here. Based on my current understanding, the "exactly one" rule best meets the intuitions of the current module system. I don't see much benefit to avoiding loading libraries whose bindings are not referenced, and significant "surprise" potential in not doing so.
`Gleckler`::
> Our library system is designed to have simple semantics, and divergence from once-and-only-once makes it harder to reason about. If we are going to do something else, let's still make sure that modules aren't loaded more than once.
`Hsu`::
> So far I cannot find a reason to have more than a single load for any given expansion phase, and therefore the zero-or-one seems reasonable, but I still insist that zero must be a valid load count if the library is unused.
`Read`::
> It is useful for libraries that are not actually referred to to not be loaded, and we will leave it to implementations to decide how many loadings are necessary.
`Shinn`::
> Allowing one or many instantiations is a philosophical choice that I think implementations are unlikely to compromise on. In deference to existing R6RS implementations I think zero instantiations is reasonable, assuming the implementation can prove it's safe. This gives better support among implementations, and the work-around to being unable to import for side effects is simply to import and call a function at run time.
`SnellPym`::
> Same argument as #441 above.

## WG1 - Numerics

### #366 Add (log z b) for logarithm of z to the base b

Coverage for this R6RS feature is currently sparse: only Gauche, Chez,
Vicare, Larceny, Ypsilon, Mosh, IronScheme, KSi, RScheme, Rep support
it.  But it is convenient when working in bases other than *e* such
as 10, 2, or 16, and it is just a few extra lines of code, since `(log
z b)` => `(/ (log z) (log b))` for arbitrary complex numbers *z, b*.

Vote `yes` to add the optional second argument from R6RS.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes, no
* [Medernach](WG1BallotMedernach.md): no, undecided, yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): undecided, yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 8:1, 7:2
* **Rationales:**

`Cowan`::
> This is easy for both users and implementers, but implementers should do it once so users don't have to do it more than once (as well as remembering the order of terms in the division!)
`Gleckler`::
> This is simple, widely useful, and conforms to R6RS.
`Lucier`::
> I voted both yes and no because I think it's a good function to have, but (log z b) => (/ (log z) (log b)) is not as complete an implementation as I would like, e.g, I'd prefer (log (expt 2 29) 2) => 29 exactly, and not 29.000000000000004, which the second formula gives on Gambit.
`Read`::
> In engineering, log-base-10 is way more common than log-base-e. You're not doing developers of engineering applications any favors by not providing easy access to the logarithm they're most familiar with.
`Shinn`::
> I've often wanted this when coding.
`SnellPym`::
> For some reason, I'm uncharacteristically soppy about allowing extensions to the numeric procedures...

### #367 Inexact division by exact zero

Draft 6 says that it's an error for an argument of `/` (other than the
first) to be an exact zero.  R6RS, however, says that it's an error
only if *all* the arguments are exact.  In other words, `(/ 2.0 0)`
is an error according to the draft, but in R6RS it returns `+inf.0`
(assuming the implementation supports it).  The proposal is to adopt
the R6RS wording.

Cowan tested `(/ 2.0 0)` in the usual set of Schemes:

* Racket, Gambit, Chicken (with the numbers egg), Guile, Chibi, Elk, Spark report an error.
* Gauche, Bigloo, Scheme48/scsh, Kawa, SISC, Chez, SCM, Ikarus/Vicare, Larceny, Ypsilon, Mosh, IronScheme, NexJ, STklos, RScheme, BDC, UMB, VX return `+inf.0`.
* MIT, scsh, Shoe, TinyScheme, Scheme 7, XLisp, Rep, Schemik, Inlab always report an error when dividing by zero, exact or inexact.
* KSi, Scheme 9 produce incorrect results.
* SigScheme, Dream, Oaklisp, Owl Lisp don't support inexact numbers.

Vote `error` for the current draft semantics that it is an error,
`all-error` for the R6RS semantics that it is only an error if all
arguments are exact, or `unspecified` to make this case unspecified.

* **Options:** error, all-error, unspecified, undecided
* **Default:** error
* **Voters:**
* [Cowan](WG1BallotCowan.md): error
* [Ganz](WG1BallotGanz.md): all-error
* [Gleckler](WG1BallotGleckler.md): error, all-error
* [Hsu](WG1BallotHsu.md): all-error, unspecified, error
* [Lucier](WG1BallotLucier.md): error
* [Medernach](WG1BallotMedernach.md): error, unspecified, all-error, undecided
* [Radul](WG1BallotRadul.md): error, unspecified
* [Read](WG1BallotRead.md): all-error
* [Shinn](WG1BallotShinn.md): error
* [SnellPym](WG1BallotSnellPym.md): error, unspecified, all-error
* **Results:** **error**, all-error, unspecified, undecided
* **Ratios:** 7:3, 7:1, 8:0
* **Rationales:**

`Cowan`::
> The zero is exact and that is what causes the error -- the other arguments are not relevant.
`Ganz`::
> Systems that support `+inf.0` and `-inf.0` as constants and have procedures to test for them should produce them when appropriate.
`Gleckler`::
> Division by zero is still typically a mistake, and catching it as early as possible is a good idea. Until we specify how an implementation goes into IEEE non-signalling mode, we should continue signalling such errors.
`Shinn`::
> The zero is exact and that is what causes the error - the other arguments are not relevant.

### #369 Require that - and / allow an arbitrary number of arguments

R5RS requires that `-` and `/` accept one or two arguments, and labels
support for more than two as "optional".  R6RS requires such support.
The proposal is to require it.

All Schemes in the test suite support more than two arguments except
Scheme48/scsh.  (Owl Lisp does not support variadic procedures of any
kind.)

Vote `require` for required n-ary behavior and `optional` to leave it
optional as in R5RS.  Alternately, vote `forbidden` to make this
always an error in all implementations.

* **Options:** required, optional, forbidden, undecided
* **Default:** optional
* **Voters:**
* [Cowan](WG1BallotCowan.md): required, optional
* [Ganz](WG1BallotGanz.md): required
* [Gleckler](WG1BallotGleckler.md): required, optional
* [Hsu](WG1BallotHsu.md): required, optional
* [Lucier](WG1BallotLucier.md): required
* [Medernach](WG1BallotMedernach.md): required, optional, undecided, forbidden
* [Radul](WG1BallotRadul.md): required
* [Read](WG1BallotRead.md): optional
* [Shinn](WG1BallotShinn.md): required
* [SnellPym](WG1BallotSnellPym.md): required, optional, forbidden
* **Results:** **required**, optional, undecided, forbidden
* **Ratios:** 9:1, 9:0, 9:0
* **Rationales:**

`Cowan`::
> It's silly for `(- 1 2 3)` not to Just Work in all Schemes. Still, `forbidden` would be terrible.
`Ganz`::
> I don't think these are particularly useful, but will go with it for the sake of improving portability.
`Gleckler`::
> It's not hard to make this just work, and almost all Schemes already support it.
`Radul`::
> Since everybody does it anyway, might as well allow code to depend on it. Not that (/ 1 2 3 4) is very intuitive.
`Shinn`::
> This is a de-facto standard, and I dislike "fixing" my code so that it works for Scheme48.
`SnellPym`::
> It's useful, and it does not harm, and it's widely implemented. What's not to love?

### #370 Log of exact and inexact zero

R5RS and draft 6 of R7RS don't say what `(log 0.0)` and `(log 0)`
return.  R6RS requires `-inf.0` and an exception respectively.  The
proposal is to say that `(log 0.0)` returns `-inf.0` on systems that
have `-inf.0`, and that `(log 0)` is an error.

In Racket, Gambit, Chicken (with the numbers egg), Guile, Chibi, Chez,
Ikarus/Vicare, Larceny, Ypsilon, Mosh, IronScheme, STklos, Spark,
`(log 0.0)` returns `-inf.0` and `(log 0)` raises an exception.

Gauche, MIT, Chicken (without the numbers egg), Bigloo, Scheme48/scsh,
Kawa, SISC, SCM, NexJ, KSi, RScheme, XLisp, Rep, VX, SXM, Inlab return
`-inf.0` in both cases.

Elk, UMB, Oaklisp raise an exception in both cases.

Scheme 7 returns the wrong answer in both cases.

SigScheme, Shoe, TinyScheme, Dream, BDC, Owl Lisp don't support `log`.

Scheme 9 apparently goes into an infinite loop in both cases.

Vote `r6rs` for the R6RS behavior of returning `-inf.0` and raising an
error, respectively.  Vote `infinity` to always return `-inf.0`.

* **Options:** r6rs, infinity, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Ganz](WG1BallotGanz.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs, unspecified
* [Hsu](WG1BallotHsu.md): r6rs
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): r6rs, infinity, unspecified, undecided
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs, unspecified, infinity
* **Results:** **r6rs**, unspecified, infinity, undecided
* **Ratios:** 9:0, 9:0, 9:0
* **Rationales:**

`Cowan`::
> R6RS got it right here: the limit of log *x* as *x* -> 0 is negative infinity, which however is not representable within the exact number system.
`Gleckler`::
> Let's catch errors early.
`Hsu`::
> The R6RS option here does not prevent us from returning `-inf.0` because our error state is not that strong.
`Shinn`::
> As in #367, an exact zero for an undefined operation should be an error. An inexact zero should return what the floating point processor returns.

### #407 Dividing exact 0 by an inexact number

This proposal allows `(/ 0 `*x*`)`, where *x* is an inexact
number, to return an exact value.  Currently only Racket, Gambit,
TinyScheme, Sizzle, Spark do this; see [Zero](Zero.md) for details.

Vote `zero` to allow (but not require) this to return exact 0.  Vote
`no-nan` to allow it to return 0 except when `x` is `+nan.0`, where it
would return `+nan.0`.

* **Options:** zero, no-nan, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): zero, no-nan
* [Ganz](WG1BallotGanz.md): no-nan
* [Gleckler](WG1BallotGleckler.md): no-nan, zero, unspecified
* [Hsu](WG1BallotHsu.md): undecided, no-nan
* [Lucier](WG1BallotLucier.md): zero
* [Medernach](WG1BallotMedernach.md): no-nan, zero, unspecified, undecided
* [Radul](WG1BallotRadul.md): zero
* [Read](WG1BallotRead.md): no-nan
* [Shinn](WG1BallotShinn.md): no-nan, unspecified
* [SnellPym](WG1BallotSnellPym.md): no-nan, zero, unspecified
* **Results:** **no-nan**, zero, unspecified, undecided
* **Ratios:** 7:3, 8:0, 7:1
* **Rationales:**

`Cowan`::
> Returning an exact value is good, but propagating NaN is good too. As I've posted, I don't think the NaN exception to the exception is worth fussing about, since it's all optional.
`Ganz`::
> I'd be happy to require exact zero in other cases (and for multiplication by exact 0).
`Gleckler`::
> Zero divided by anything is zero. We know the answer exactly given only an exact numerator, so we should return an exact answer. However, as John points out, a NaN may indicate an earlier error, and we shouldn't require masking that.
`Radul`::
> While this does have the downside of potentially masking a NaN, it only masks one in a computation whose result wasn't going to be used anyway. If the programmer really did not want their [NaNs](NaNs.md) masked, they could have ensured the zero were inexact. In the meantime, this allows an optimization, which can be very important in numeric code because it can chain.
`Shinn`::
> The proposal as-is explicitly breaks the rules for `+nan.0`.

### #410 Infinity vs. NaN in max and min

Currently R7RS says nothing about the value of `(max +inf.0 +nan.0)`
or `(min -inf.0 +nan.0)`.  R6RS required these functions to return the
infinite value, but this was adopted by some but not all R6RS
implementations (see [MaxInfNan](MaxInfNan.md) for details).  R5RS implementations are
also divided.

The proposal is to allow R7RS implementations to provide either value.

Vote `both` to explicitly add a note that either are allowed,
`infinity` to require the infinite value as in R6RS, `nan` to require
returning `+nan.0`, and `unspecified` leave unspecified (i.e. the same
as `both` but without the note).

* **Options:** both, infinity, nan, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): both, unspecified
* [Ganz](WG1BallotGanz.md): nan, unspecified
* [Gleckler](WG1BallotGleckler.md): both, unspecified, nan, infinity
* [Hsu](WG1BallotHsu.md): nan, unspecified, both
* [Lucier](WG1BallotLucier.md): infinity, unspecified
* [Medernach](WG1BallotMedernach.md): both, unspecified, nan, infinity, undecided
* [Radul](WG1BallotRadul.md): unspecified
* [Read](WG1BallotRead.md): unspecified
* [Shinn](WG1BallotShinn.md): both, unspecified, nan
* [SnellPym](WG1BallotSnellPym.md): infinity, both, unspecified, nan
* **Results:** both, *unspecified*, nan, infinity, undecided
* **Ratios:** 5:5, 5:2, 5:2, 6:0
* **Rationales:**

`Cowan`::
> If we are going to allow both (as I think we should and must, the arguments on both sides being equally satisfactory), we should say so.
`Gleckler`::
> I'm changing this for the same reason I changed my answer to #407, i.e. because a NAN may indicate an earlier error and we shouldn't lose the information that there was an error.
`Lucier`::
> I consider Gambit's behavior a bug that I intend to fix (if I remember to do so). It is true that one of the original motivations for introducing NaN objects was to track the location of where anomalies/errors occured. But if an anomaly would not affect the final answer (in other words, if the final answer would be the same no matter the value substituted for the NaN), then that final answer should be returned.
`Read`::
> max and min are undefined over non-numbers (including [NaNs](NaNs.md)).
`Shinn`::
> Implementations are divided and this is a confusing case so it should be documented. Given one or the other I'd say NaN trumps infinity.
`SnellPym`::
> If there's no number that can be larger than +inf.0, then NaN can't make the maximum any more maximal, so infinity is the logical answer for (max +inf.0 +nan.0).

### #395 Infinite and NaN complex numbers

Currently both `infinite?` and `nan?` return `#t` to a complex number
like `+inf.0+nan.0i`.  Is this the Right Thing, or should `infinite?`
only return `#t` if neither part is a NaN?

Note it is reasonable for an implementation to not support partial nan
complex numbers.

Vote `disjoint` to ensure that `infinite?` and `nan?` are disjoint
predicates as in the proposal, or `overlap` to allow the current
behavior.

* **Options:** overlap, disjoint, unspecified, undecided
* **Default:** overlap
* **Voters:**
* [Cowan](WG1BallotCowan.md): overlap
* [Ganz](WG1BallotGanz.md): disjoint, unspecified
* [Gleckler](WG1BallotGleckler.md): overlap, disjoint
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): overlap, unspecified
* [Medernach](WG1BallotMedernach.md): overlap, unspecified, disjoint, undecided
* [Radul](WG1BallotRadul.md): disjoint
* [Read](WG1BallotRead.md): disjoint, overlap
* [Shinn](WG1BallotShinn.md): disjoint
* [SnellPym](WG1BallotSnellPym.md): overlap, unspecified, disjoint
* **Results:** **overlap**, disjoint, unspecified, undecided
* **Ratios:** 5:4, 6:1, 6:1
* **Rationales:**

`Cowan`::
> I don't really care about the R6RS notion that `finite?`, `infinite?`, and `nan?` form a partition (and in R6RS they don't work on complex numbers anyway). Testing against the Scheme suite didn't show much, because most Schemes either don't have complex numbers at all, don't have `infinite?` and/or `nan?`, or don't allow complex numbers as arguments to them. However, Gauche, Chicken with the numbers egg, Vicare, IronScheme agree that `+inf.0+nan.0i` is both infinite and NaN. Spark says it is neither, SXM doesn't allow NaN in a complex value.
`Gleckler`::
> I'm not sure how useful it is to ask whether a complex number is infinite, but the only reasonable interpretation I can see is that if either part is infinite, the complex number is. If we don't agree on that, I can't see any value in leaving this unspecified, so let's make them disjoint.
`Lucier`::
> I think `infinite?' and `nan?' should take only real arguments.
`Read`::
> NaN-ness should be contagious: if any component of a numeric value is NaN, then the whole thing is "not a number". Accordingly complex numbers with one part NaN should be either forbidden, or `nan?` returns `#t` on them but not any predicates that apply to is-a-number numeric values.
`Shinn`::
> I don't think a number can be +nan.0 as well as something else.
`SnellPym`::
> +inf.0+nan.0i is definitely infinite, and definitely not entirely specified, so infinite? and nan? are both valid.

### #364 truncate, floor, ceiling round should return a non-finite argument

Currently R7RS is silent on what truncate, floor, ceiling, and round
do when the argument is `+inf.0`, `-inf.0`, or `+nan.0`. R6RS has them
return the argument, which seems reasonable.

Tests were made for `(round (* 1.0e200 1.0e200))` on a variety of
implementations.

Racket, Gauche, Chicken (with and without the numbers egg), Bigloo,
Guile, Kawa, Chibi, Chez, SCM, Ikarus/Vicare?, Larceny, Ypsilon, Mosh,
[IronScheme](IronScheme.md), NexJ, STklos, KSi, Shoe, BDC, Rep, Schemik, Elk, Spark
all return the argument.

MIT, Gambit, Scheme48/scsh, SISC, Scheme 9, Scheme 7, signal errors.

[SigScheme](SigScheme.md), [TinyScheme](TinyScheme.md), Dream, UMB don't work for one or another
reason.

Oaklisp and Owl Lisp don't do flonums.

XLisp only has fixnums and flonums, and returns the largest or
smallest fixnum as the case may be.

RScheme returns a variety of slightly strange values: (round +inf.0),
for example, is 0, but (round -inf.0) is -inf.0.

Vote `input` to return the input, `error` to specify "it is an error",
and `unspecified` to leave unspecified as in the current draft.

* **Options:** input, error, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified, input, error
* [Ganz](WG1BallotGanz.md): input, unspecified
* [Gleckler](WG1BallotGleckler.md): error, unspecified
* [Hsu](WG1BallotHsu.md): input, unspecified
* [Lucier](WG1BallotLucier.md): error
* [Medernach](WG1BallotMedernach.md): input, unspecified, error, undecided
* [Radul](WG1BallotRadul.md): input
* [Read](WG1BallotRead.md): input
* [Shinn](WG1BallotShinn.md): unspecified
* [SnellPym](WG1BallotSnellPym.md): input, unspecified, error
* **Results:** **input**, unspecified, error, undecided
* **Ratios:** 6:3, 7:2, 7:0
* **Rationales:**

`Cowan`::
> Implementations differ. If we must specify, R6RS wins.
`Gleckler`::
> Let's catch errors early. The whole point of these procedures is to return integers, so returning something that is not an integer makes no sense. If we don't do that, we should leave the result unspecified rather than force implementations to do the wrong thing even if it is compatible with R6RS.
`Lucier`::
> To my mind, the output of round, truncate, floor, and ceiling should be integers, and +inf.0 and nan.0 are not rational, so they are not integers.
`Shinn`::
> I think this is an error, but understand wanting to return the argument for efficiency, simplicity, and closeness to the floating point processor. Since implementations differ we should leave this unspecified.

### #392 Exact positive and non-negative integer predicates

There are two useful subsets of the exact numbers, both of which are
commonly called natural numbers, depending on who's talking.
Logicians, set theorists, and computer scientists include 0, other
mathematicians mostly don't.  This proposal adds the predicates
`exact-positive-integer?` and `exact-non-negative-integer?`, analogous
to `exact-integer?`.  Because of the ambiguity, the name
`natural-number?` is not proposed.

Vote `yes` to add these two procedures.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): no, wg2
* [Hsu](WG1BallotHsu.md): undecided, wg2, yes
* [Lucier](WG1BallotLucier.md): yes, wg2
* [Medernach](WG1BallotMedernach.md): no, wg2, undecided, yes
* [Shinn](WG1BallotShinn.md): no, wg2
* [SnellPym](WG1BallotSnellPym.md): yes, wg2, no
* **Results:** *wg2*, yes, **no**, undecided
* **Ratios:** 4:4, 3:3, 5:1
* **Rationales:**

`Cowan`::
> Natural numbers are an important type, and Scheme should support it directly.
`Gleckler`::
> We don't need more names in WG1.
`Hsu`::
> I am not sure I see the use of these rather than just a combination of predicates...but...well, sure, I am not opposed to them so much.
`Shinn`::
> The motivation for this is unclear, apart from using procedure names as a platform to argue about the definition of natural numbers. The procedures in question are trivial compositions of existing procedures and serve only to bloat the standard.
`SnellPym`::
> I'm still soft on more numeric procedures...

## WG1 - Read/Write

### #380 Is support of TAB as a whitespace character required or not?

2.2 says:

Whitespace characters include the space and newline characters.
(Implementations may provide additional whitespace characters such as
tab or page break.)

However, 7.1.1 has:

<intraline whitespace> -> <space or tab>
<whitespace> -> <intraline whitespace> | <newline> | <return>

So 2.2 implies that supporting tabs is allowed but not required, yet
7.1.1 implies supporting tabs is required.

Vote `required` to require support for tab as a whitespace character
by `read`.  `char-whitespace?` is required to return `#t` for it
regardless.

* **Options:** required, optional, undecided
* **Default:** optional
* **Voters:**
* [Cowan](WG1BallotCowan.md): required
* [Ganz](WG1BallotGanz.md): required
* [Gleckler](WG1BallotGleckler.md): required
* [Hsu](WG1BallotHsu.md): required
* [Lucier](WG1BallotLucier.md): required
* [Medernach](WG1BallotMedernach.md): required, optional, undecided
* [Radul](WG1BallotRadul.md): required
* [Read](WG1BallotRead.md): required
* [Shinn](WG1BallotShinn.md): required
* [SnellPym](WG1BallotSnellPym.md): required, optional
* **Results:** **required**, optional, undecided
* **Ratios:** 10:0, 10:0
* **Rationales:**

`Cowan`::
> All Schemes treat tab as whitespace anyway, though some give it special meaning (completion or whatever) at the REPL.
`Ganz`::
> Behavior of `read` should be consistent with `char-whitespace?`
`Gleckler`::
> How can this be in question? It's just basic ASCII, as is page break, for that matter.
`Read`::
> When have tabs ever not been whitespace?
`Shinn`::
> This is too common to be vague on, and a lot of source code includes tabs.
`SnellPym`::
> I think that anything generally considered whitespace in the implementation character set is fair game. Include Unicode paragraph breaks and NELs and all that malarky that caused such fun with XML 1.1...

### #388 Specify what `display` does with circular lists

Currently we don't specify what `display` does with circular lists.
Should it generate labels like `write`, or loop like `write-simple`,
or leave it unspecified?

* **Options:** labels, loop, unspecified
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): labels
* [Ganz](WG1BallotGanz.md): labels
* [Gleckler](WG1BallotGleckler.md): unspecified, labels
* [Hsu](WG1BallotHsu.md): unspecified
* [Lucier](WG1BallotLucier.md): labels, unspecified
* [Medernach](WG1BallotMedernach.md): labels, unspecified, loop
* [Read](WG1BallotRead.md): labels
* [Shinn](WG1BallotShinn.md): labels
* [SnellPym](WG1BallotSnellPym.md): labels, unspecified, loop
* **Results:** **labels**, unspecified, loop
* **Ratios:** 7:2, 8:0
* **Rationales:**

`Cowan`::
> Display should be safe (shouldn't loop), since it's often used as a debug printer. It might be better if it ignored sharing, but not enough to be worth yet another exception.
`Gleckler`::
> Why are we requiring that basic I/O operations be expensive? I shouldn't have to allocate memory just to print a list.
`Read`::
> `display` is Scheme's `PRINT` statement. It should gracefully handle any input.
`Shinn`::
> Display should be safe as well. We should also provide `write-string` from #425 for efficiency.
`SnellPym`::
> Looping sucks.

### #447 #!fold-case and #!no-fold-case have no final delimiter

The `#!fold-case` and `#!no-fold-case` directives are read as
comments, which means that they are treated as whitespace (section
2.2).  Unlike the other kinds of comments, their final delimiter is
implicit.  This means that `(1#!no-fold-cases)` reads as `(1 s)`.
This seems unfortunate.

* **Proposals:**
* **identifier:** add the formal syntax `<lexical-directive> --> #! <identifier>` and then make the interpretation of `<identifier>` implementation-dependent, except for the standard cases `#!fold-case` and `#!no-fold-case`. (Per Bothner, Richard Kelsey)
* **delimiter:** the directives must be followed by delimiter (John Cowan)
* **comment:** the draft status-quo
* **Options:** identifier, delimiter, comment, undecided
* **Default:** comment
* **Voters:**
* [Cowan](WG1BallotCowan.md): delimiter
* [Ganz](WG1BallotGanz.md): identifier, delimiter
* [Gleckler](WG1BallotGleckler.md): delimiter, identifier
* [Hsu](WG1BallotHsu.md): delimiter, identifier, comment
* [Lucier](WG1BallotLucier.md): delimiter, identifier
* [Medernach](WG1BallotMedernach.md): delimiter, identifier, undecided, comment
* [Read](WG1BallotRead.md): identifier, delimiter
* [Shinn](WG1BallotShinn.md): delimiter, identifier
* [SnellPym](WG1BallotSnellPym.md): delimiter, identifier, comment
* **Results:** **delimiter**, identifier, comment, undecided
* **Ratios:** 7:2, 9:0, 9:0
* **Rationales:**

`Cowan`::
> Most of the time case-folding directives will be delimited by newline or space anyway. I see no point in adding yet another class of magic implementation-dependent things to the standards: implementations can do what they will anyway.
`Gleckler`::
> Requiring a delimiter (presumably including end of file) is simple and consistent. Even if we don't do that, we shouldn't stick with the status quo, which is bizarre.
`Read`::
> The `identifier` choice is what Gambit Scheme uses, which provides useful self-evaluating constants such as `#!eof` and `#!void`. I would be totally cool with standardizing this behavior.
`Shinn`::
> I don't like encouraging additional #! identifiers - requiring a delimiter is simpler and resolves the issue.
`SnellPym`::
> It seems the cleanest.

### #442 write procedure is not backwards compatible

There is concern that the output of `write` cannot be read by non-R7RS
implementations.  This is not a strict requirement, but is reasonable
if using simple sexp-based file/interchange formats.

Specifically, even though there are no cycles in

> `(let ((x (list 2))) (write (list x x)))`

it previously output "((2) (2))" but now outputs "(#0=(2) #0#)".

The WG concern is that R5RS write is unsafe, easily causing infinite
loops, and should therefore not be the default.  Thus we renamed this
"write-simple", requiring programmers to know they are writing a
"simple" data structure up front.

Arguably, there are three procedures desired:

* write-cyclic: uses labels only to avoid cycles
* write-shared: uses labels for all shared structure
* write-simple: won't use labels - it is an error to pass a cyclic structure

although even for `write-shared` people sometimes want to treat
containers such as strings separately.

Note the algorithms for detecting shared structure differ from those
for detecting cycles, so providing both -shared and -cyclic imposes an
additional implementation burden.

* **Proposals:**
* **write+simple:** the current draft status quo
* **write+shared:** change `write` back and add `write-shared` to explicitly handle sharing
* **write+cyclic:** change `write` back and add `write-cyclic` to handle only cycles
* **write+shared+cyclic:** change `write` back and add both `write-shared` and `write-cyclic`
* **write+simple+shared:** `write` handles cycles only, provide `write-simple` and `write-shared` separately
* **Options:** write+simple, write+shared, write+cyclic, write+shared+cyclic, write+simple+shared, unspecified, undecided
* **Default:** write+simple
* **Voters:**
* [Cowan](WG1BallotCowan.md): write+simple+shared, write+simple
* [Ganz](WG1BallotGanz.md): write+simple+shared
* [Gleckler](WG1BallotGleckler.md): write+shared, write+cyclic, write+shared+cyclic, write+simple+shared
* [Hsu](WG1BallotHsu.md): write+simple+shared, write+shared+cyclic, write+simple
* [Lucier](WG1BallotLucier.md): write+shared+cyclic, write+shared, undecided
* [Medernach](WG1BallotMedernach.md): write+simple+shared, write+shared+cyclic, write+shared, write+simple, write+cyclic, unspecified, undecided
* [Radul](WG1BallotRadul.md): write+simple
* [Read](WG1BallotRead.md): write+simple
* [Shinn](WG1BallotShinn.md): write+simple+shared, write+shared, write+simple
* [SnellPym](WG1BallotSnellPym.md): write+simple, write+simple+shared, (write+shared write+cyclic write+shared+cyclic)
* **Results:** **write+simple+shared**, write+simple, write+shared+cyclic, write+shared, write+cyclic, unspecified, undecided
* **Ratios:** 6:3, 6:2, 6:2, 6:1, 7:0, 7:1
* **Rationales:**

`Cowan`::
> I've changed my mind on this one: it's better to have all three.
`Ganz`::
> I don't have patience to try to rate the others in terms of 'least-bad'.
`Gleckler`::
> We shouldn't make such a basic, incompatible change in a core language feature (i.e. `write`) even in the interests of preventing infinite loops. Furthermore, `write` shouldn't have to allocate memory just to print something, particularly considering that the vast majority of uses will have no cycles.
`Hsu`::
> We should have a safe, but backwards compatible `write`, which is `write-cyclic` assuming that it behaves as `write-shared` only when a cycle is detected. However, we also want the other two versions.
`Medernach`::
> Safety is essential and both procedures are useful.
`Radul`::
> If you are trying to use read/write to implement serialization and you have two different implementations of Scheme, one of which writes shared structure by default and the other does not read it, you have two perfectly good options: Either write a procedure that unshares shared structure, or implement reading of shared structure. The latter is harder, but is likely to yield a more compact external representation.
`Shinn`::
> This is backwards compatible and makes the default `write` safe.
`SnellPym`::
> I want a safe write by default; if you want backwards compatability, you shouldn't be trying to send shared structure, which r5rs doesn't support. In effect, you're relying on an implicit "remove all sharing" phase before WRITE in R5RS. I don't like relying on implicit things if I can avoid it!

### #219 Bring back readable boolean literals

Scheme used to use `#!true` and `#!false` before abbreviating to
the `#t` and `#f` syntax.

In draft 4 we added these back in as aliases, without the "!" now
that tokens are required to be delimited so there would be no ambiguity.

Some objections were made to the new syntax which generated
a lot of discussion, so we are re-opening this ticket.  The default
is the previous decision to add `#true` and `#false` as aliases.

The primary objection is that boolean literals are very common,
and this introduces needless incompatibilities with non-R7RS
systems, and potential confusion in documentation.

The counter-argument is that these are more readable and
friendly to beginners, and allow easy visual distinction in long lists
of booleans.  We retain full backwards compatibility and are
under no obligation for non-R7RS systems to be able to run R7RS code.

Note that Racket and Chibi independently adopted this same
syntax unaware of each other.  Chicken also supports this via
its SRFI-38 implementation.

* **References:**
* **Proposals:**
* **long:** #true and #false
* **bang-long:** #!true and #!false
* **Options:** long, bang-long, none, undecided
* **Default:** long
* **Voters:**
* [Cowan](WG1BallotCowan.md): long
* [Ganz](WG1BallotGanz.md): long, no
* [Gleckler](WG1BallotGleckler.md): long, bang-long
* [Hsu](WG1BallotHsu.md): long, no
* [Lucier](WG1BallotLucier.md): no, undecided
* [Medernach](WG1BallotMedernach.md): long
* [Radul](WG1BallotRadul.md): long
* [Read](WG1BallotRead.md): bang-long, no
* [Shinn](WG1BallotShinn.md): long
* [SnellPym](WG1BallotSnellPym.md): long, bang-long, no
* **Results:** **long**, bang-long, no, undecided
* **Ratios:** 8:1, 8:2, 8:1
* **Rationales:**

`Cowan`::
> "I meant what I said and I said what I meant / An elephant's faithful, 100%!" --Horton Hatches The Egg
`Ganz`::
> `#!` is now for directives, which these are not.
`Gleckler`::
> The long form is more readable, I suppose. However, I'm worried that non-R7RS implementations will no longer be able to read values of this most basic type.
`Hsu`::
> It is unlikely that these should be used for anything else, so we might as well alias them.
`Read`::
> `bang-long` is consistent with my answer to #447.
`Shinn`::
> I almost went back on this, but the fact that Racket came up with the same syntax convinced me. There will be few compatibility issues in practice since implementations will likely continue using the short forms for write, and the names really are an improvement.

### #443 Recommend sources of character names

Currently, we allow implementations to provide their own names for
characters, but provide no guidance for them.  There are two plausible
sources: the [#http://unicode.org/Public/UNIDATA/NamesList.txt|names in
the Unicode Standard]], and the [#http://www.w3.org/TR/xml-entity-names/
entity|names specified by W3C]] for use in HTML, MathML, and other
markup standards (ultimately derived from ISO SGML character entity
sets).

The Unicode names are in all upper case and contain significant spaces
and apostrophes as name characters, which would require some mapping
to make valid Scheme identifiers.  The W3C name list is incomplete
though fairly large (currently 2237 names), covering mainly the Greek
and Cyrillic scripts and non-ASCII punctuation and symbols.  It
distinguishes between iota (small) and Iota (capital).

Vote `w3c` for the W3C list, `unicode` to use the Unicode list with
names mapped by converting to lowercase and replacing any
non-identifier character (space and apostrophe) with hyphens.  Vote
`unspecified` to leave the character name extensions entirely up to
the implementation.

* **Options:** w3c, unicode, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified, w3c, unicode
* [Ganz](WG1BallotGanz.md): w3c, unspecified
* [Gleckler](WG1BallotGleckler.md): unspecified, unicode
* [Hsu](WG1BallotHsu.md): unicode, w3c, unspecified
* [Lucier](WG1BallotLucier.md): unspecified, undecided
* [Medernach](WG1BallotMedernach.md): unspecified
* [Read](WG1BallotRead.md): unicode, w3c
* [Shinn](WG1BallotShinn.md): unspecified, unicode
* [SnellPym](WG1BallotSnellPym.md): (w3c unicode), unspecified
* **Results:** **unspecified**, unicode, w3c, undecided
* **Ratios:** 6:3, 5:4, 8:0
* **Rationales:**

`Cowan`::
> Alex is unhappy with the W3C list because it has case-sensitive names (so under case folding some names are shadowed), and I'm unhappy with the Unicode list because it's huuuuuuge and the names are loooooong. Let's say nothing.
`Gleckler`::
> There isn't agreement among implementations on this. If we do specify it, we should specify Unicode to be consistent with all our other support for Unicode, which is the most carefully thought out standard in any case.
`Shinn`::
> I'd rather not specify this in WG1, but if we do the W3C entity names seem arbitrary and ugly, are case-sensitive (in contrast to our existing rule for character names), and most languages adding readable character names these days seem to use Unicode.
`SnellPym`::
> I think we should provide a recommendation, but I'm not sure which. If we go for w3c, we need to make it case-inensitive, which might be tricky for the iota!

## WG1 - Base Library

### #140 Removing `quotient`, `remainder`, `modulo`

With the acceptance of #278, we reduced the set of division operators
to `truncate-*` and `floor-*` and move these into the base library.
Three of these procedures are simply aliases for `quotient`,
`remainder` and `modulo`, so it is worth considering removing the old
names.

Since the old names are in IEEE Scheme we need strong justification
for removing them from (scheme base), and even if we do so they will
remain available in (scheme r5rs).

We have precedence for changing names, but only in the case when the
existing names were both actively misleading and had already been
changed in R6RS.  Specifically, in ticket #328 we replaced the names
`inexact->exact` and `exact->inexact` with the more accurate `exact`
and `inexact`.

Arguably the new division operator names are clearer, but the old
names are not actually misleading.

Vote `yes` to remove the old names from (scheme base), or `no` to
leave them in as aliases.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): no, yes
* [Medernach](WG1BallotMedernach.md): no
* [Radul](WG1BallotRadul.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **no**, yes
* **Ratios:** 8:2
* **Rationales:**

`Cowan`::
> It's silly to have both sets of names in the base library.
`Gleckler`::
> I don't have a strong preference, so I'll go with not breaking existing code.
`Medernach`::
> This would break too much code.
`Shinn`::
> This breaks too much existing code, and isn't actively misleading like `inexact->exact`.
`SnellPym`::
> I'm happy about backwards compatability if they're in (scheme r5rs), and want to keep (scheme base) clean.

### #378 Rename GET-FEATURES to just FEATURES

This is compatible with Chicken, and "more Scheme-like, less
Java-like".  Okay, it's bikeshedding.

* **Options:** features, get-features, undecided
* **Default:** get-features
* **Voters:**
* [Cowan](WG1BallotCowan.md): features
* [Ganz](WG1BallotGanz.md): features
* [Gleckler](WG1BallotGleckler.md): features
* [Hsu](WG1BallotHsu.md): features, get-features, undecided
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): features, get-features, undecided
* [Radul](WG1BallotRadul.md): features
* [Shinn](WG1BallotShinn.md): features
* [SnellPym](WG1BallotSnellPym.md): features, get-features
* **Results:** **features**, get-features, undecided
* **Ratios:** 8:0, 8:1
* **Rationales:**

`Cowan`::
> I don't even know why I made this `get-features` when I proposed it.
`Gleckler`::
> Yes, the "get-" prefix adds nothing here. We should consider removing it from `get-output-string`, `get-output-bytevector`, `get-environment-variable`, and `get-environment-variables` as well.
`Shinn`::
> We have precedence from Chicken, and I don't think any implementation uses get-features.

### #384 Merge `bytevector-copy` and `bytevector-copy-partial`

Under this proposal, the name would be `bytevector-copy` and the
signature would be

> `(bytevector-copy `*bytevector* [#*start*|[*end*]]]`)`

Vote `yes` for this simplification.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes, undecided, no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 9:0, 9:0
* **Rationales:**

`Cowan`::
> This is in conformity with the way we do string and vector copiers now, as well as the destructive `bytevector-copy!`.
`Gleckler`::
> The naming convention "-partial" is neither widely used nor consistent with naming elsewhere in the draft.
`Shinn`::
> The *-partial interfaces are not used in existing implementations.

### #385 Merge `write-bytevector` and `write-bytevector-partial`

One proposal is `port-last` with a signature of:

> `(write-bytevector *bytevector* [#*start*|[*end* [*port*]]]])`

This has the disadvantage of being required to call
`bytevector-length` when writing to a specific port.

Alternately we could do `offsets-last`:

> `(write-bytevector *bytevector* [#*port*|[*start* [*end*]]]])`

which has the disadvantage of separating the bytevector from its
offsets.

Alternately, vote `separate` to keep these as two separate procedures.

* **Options:** port-last, offsets-last, separate, undecided
* **Default:** separate
* **Voters:**
* [Cowan](WG1BallotCowan.md): port-last
* [Ganz](WG1BallotGanz.md): offsets-last
* [Gleckler](WG1BallotGleckler.md): port-last
* [Hsu](WG1BallotHsu.md): offsets-last, port-last, undecided
* [Lucier](WG1BallotLucier.md): separate, port-last
* [Medernach](WG1BallotMedernach.md): port-last, separate, offsets-last, undecided
* [Read](WG1BallotRead.md): offsets-last
* [Shinn](WG1BallotShinn.md): offsets-last, port-last, undecided
* [SnellPym](WG1BallotSnellPym.md): offsets-last, separate, port-last
* **Results:** **offsets-last**, port-last, separate, undecided
* **Ratios:** 5:4, 5:2, 6:0
* **Rationales:**

`Cowan`::
> I think the port is less important than the stop/start; it can be set using the current-output-port parameter anyway.
`Gleckler`::
> Alex says that we should optimize for the most common use case, and I agree, but I believe that that's writing a range of the byte vector, not writing to a different port. After all, as John points out, the latter can be accomplished using a parameter.
`Shinn`::
> We should optimize for the most common use case, and wanting to write the entire bytevector is most common.
`SnellPym`::
> I see no problem with separating the bytevector from its offsets, and it's a pragmatic argument ordering.

### #387 Add start/end arguments to string->vector and vector->string

This is a proposal to add optional start (inclusive) and end
(exclusive) arguments to `string->vector` and `vector->string`.  We
now have start (inclusive) and end (exclusive) arguments for
`string->list` and `vector->list`, but our non-R5RS and non-SRFI
procedures to convert directly between strings and vectors don't
provide these.

Vote `yes` to add these optional arguments.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): no
* [Radul](WG1BallotRadul.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no
* **Ratios:** 8:2
* **Rationales:**

`Cowan`::
> Consistency and completeness.
`Gleckler`::
> Yes, these are useful, and we should be consistent. Alex says that using indexes with strings is a mistake, but I disagree. Arbitrarily indexing into a string and expecting to find a well-formed substring may be a mistake, but it should still be possible to remember safe offsets while creating a string and use them later.
`Shinn`::
> First, using indexes with strings is a mistake. More importantly, these are type coercion utilites, and the case of using indexes seems too rare, and better relegated to a thorough vector or string library, and/or a comprehensions library. Let's keep simple functions simple.
`SnellPym`::
> Viva la consistency!

### #391 Add predicates for R7RS signalled conditions

R7RS requires an error to be signalled (which means an exception is
raised as if by `raise`) in the following circumstances:

1. Trying to open for input or delete a file that does not exist or is otherwise inaccessible.
1. Specifying an argument to `scheme-report-environment` that the implementation doesn't support.  (It must support 7 and may support other values.)
1. An EOF is encountered while `read` is in the middle of a datum.
1. Using `expt` to raise zero to the power of a non-real number (alternatively an arbitrary number may be returned).

This proposal is to provide four standard predicates that identify
these specific conditions, to be used in `guard` clauses or in
`with-exception` handlers as a portable means of detecting these
errors.  Although these predicates may return `#t` on other objects,
if one reports `#t` on an object, the others must report `#f`.
Proposed names are `file-error?`, `scheme-report-error?`,
`read-error?`, and `expt-error?` respectively.

Vote `yes` to add these procedures, or `file-only` to only add the
`file-error?` predicate, and file+read to add the `file-error?` and
`read-error?` predicates.

* **Options:** yes, file-only, file+read, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): file+read, yes, file-only
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes, file+read, file-only
* [Hsu](WG1BallotHsu.md): yes, no, file-only, undecided
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): file+read, file-only, yes, undecided, no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no, file+read, file-only
* [SnellPym](WG1BallotSnellPym.md): yes, no, file-only
* **Results:** **yes**, file+read, file-only, no, undecided
* **Ratios:** 5:3, 6:2, 7:1, 7:1
* **Rationales:**

`Cowan`::
> There are only a few situations where errors must be signalled, and we should be able to identify which one we have in an exception handler. I can't think of any reason for providing `file-error` but not the others.
`Gleckler`::
> I'm disappointed that we have an exception system without a standard taxonomy even in WG1, so this will provide at least some relief. Alex argues that all of these situations can be checked for in advance, but that's not true. For example, checking that a file exists and is accessible before opening it opens one up to a race condition. Also, the only way to determine whether EOF will be found in the middle of reading a datum is to implement a separate parser, at which point `read` becomes useless.
`Medernach`::
> File operations are too common not to have a predicate for detecting file errors. The others are not really needed as one could test for it beforehand.
`Shinn`::
> WG2 will include a full condition hierarchy which will make this redundant. All of these can be checked for in advance.

### #400 Define record? .

We should define the predicate record? so that it's possible to
distinguish instances of record types from all other types.  It should
not be necessary to enumerate all record type predicates in order to
determine whether an object is an instance of a record.

This is Alexey Radul's suggestion.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): no
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): wg2, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): no, undecided, yes
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, yes
* **Results:** **no**, yes, undecided, wg2
* **Ratios:** 7:2, 7:0, 6:1
* **Rationales:**

`Cowan`::
> No, no, a thousand times no. The large language may want to introduce opaque records (records that look like primitive types to the outside world) and this would effectively prevent it. Redefining `record?` within a library wouldn't be enough, as it would permit opaque records to be recognized as such as soon as they were passed out of the library.
`Ganz`::
> This seems even less useful than `procedure?`, which is hardly the most well-recommended scheme primitive. I'm not sure I care if something is a record without more powerful introspection procedures, which might at least allow me to recur through the fields. I can't do anything with it unless I already know what kind of record it is, and that is available through the predicates.
`Gleckler`::
> Why shouldn't one be able to distinguish instances of record types from instances of other types? John says that having `record?` would prevent WG2 Scheme from supporting opaque records because it would be possible to recognize them as instances of record types. So what? What's the harm in being able to recognize them as such? Could a programmer do anything malicious with that information?
`Hsu`::
> This conflicts with opacity. As with equality, if we had a means of discussing opacity in the standard in a meaningful way, then it might be reasonable to put this in here. As it stands, this predicate makes sense only when we can define it in the context of opacity. Without that, it is nonsense; it must return an unspecified value, since we should allow either opaque or open records as the defualt, given that we do not provide a means of creating one or the other explicitly -- this behavior is, of course, nonsense.
`Medernach`::
> Definitely not. The idea is that each record type defines a type by itself, and that one doesn't want to exhibit implementation details. Moreover there is not any functions taking generic records.
`Read`::
> From my standpoint the purpose of records is not to have these things called records, but to usefully extend the Scheme type system with user-created aggregate types. There souldn't be a way to distinguish user-created types from types that are intrinsic to the language, and `record?` would be one way to distinguish them.
`Shinn`::
> This is useless without a record introspection library. We can include it when and if we provide record introspection.
`SnellPym`::
> I see no reason to wonder if an object is an instance of some record type.

### #425 Add read-string, read-string!, write-string procedures to (scheme base)

This was requested by Formal Comment #424.

These procedures would be provided for parallelism with the
byte-vector I/O operations:

|Byte|Character|Bytevector|String|
|read-u8|read-char|read-bytevector(!)|read-string(!)|
|write-u8|write-char|write-bytevector|write-string|

If #385 passes, optional *start* (inclusive) and *end* (exclusive)
index arguments would be added to `write-string`.  Otherwise
`write-partial-string` would be provided.

Vote `yes` to add all three, `immutable` to add only `read-string` and
`write-string`, or `no` to leave them out.

* **Options:** yes, immutable, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes, immutable.
* [Ganz](WG1BallotGanz.md): yes, immutable
* [Gleckler](WG1BallotGleckler.md): yes, immutable
* [Hsu](WG1BallotHsu.md): yes, immutable, no
* [Lucier](WG1BallotLucier.md): immutable, undecided
* [Medernach](WG1BallotMedernach.md): immutable, yes
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): immutable, no
* [SnellPym](WG1BallotSnellPym.md): immutable, yes, no
* **Results:** **immutable**, yes, no, undecided, immutable.
* **Ratios:** 4:4, 7:0, 7:1, 7:1
* **Rationales:**

`Cowan`::
> As soon as we voted in `string-copy!`, the goal of immutable strings died a horrible death. We should have all of these for bulk I/O. Still, `immutable` is better than `no`.
`Gleckler`::
> I'm alarmed that we made it this far without `write-string`.
`Read`::
> How would encodings be handled? Perhaps we should
`Shinn`::
> These are very useful, but for reading into a mutable data-structure you should use bytevectors - strings are not guaranteed to have the requisite size allocated.
`SnellPym`::
> These are quite useful operations, and I have a penchant for immutability.

### #433 full conversion cycle for containers

Marc Feeley proposes it should be possible to convert from any
container type to another, possibly via an intermediary such as

> `(list->B (A->list a))`

proposing specifically "list" be the universally available
intermediary, although "vector" would also be worth considering.

The container types are list, vector, string and bytevector.  String
and bytevector are second-class in that they are not general-purpose
container types, and may raise errors converting from lists or
vectors.

Vote `list` for the proposal to add the following procedures to
complete the cycle:

* list->bytevector
* bytevector->list

Vote `vector` to add the equivalent procedures to allow converting
between any of the types and vectors, specifically the following two
new procedures:

* vector->bytevector
* bytevector->vector

Vote `list+vector` to add both list and vector conversions.

The `latin-1` proposal also adds the Latin-1-centric ideas of string to
bytevector conversion, where each element of the bytevector is
converted to/from a character with char->integer/integer->char.

The `matrix` proposal requires all 4^3^=64 conversions.

* **Options:** matrix, list, vector, list+vector, latin-1, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): list+vector
* [Ganz](WG1BallotGanz.md): vector, matrix
* [Gleckler](WG1BallotGleckler.md): no, list+vector, vector
* [Hsu](WG1BallotHsu.md): vector, list+vector, list, no
* [Lucier](WG1BallotLucier.md): list, list+vector, no
* [Medernach](WG1BallotMedernach.md): no, vector, list+vector, list, undecided, latin-1, matrix
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): no, list, vector, list+vector, latin-1
* [SnellPym](WG1BallotSnellPym.md): vector, list, (list+vector matrix latin-1)
* **Results:** **no**, vector, list+vector, list, undecided, latin-1, matrix
* **Ratios:** 4:3, 3:4, 3:3, 5:1, 5:1, 5:2
* **Rationales:**

`Cowan`::
> Conversions between bytevectors and lists/vectors are sensible, but doing Latin-1 conversions isn't a particularly sensible conversion between strings and bytevectors.
`Ganz`::
> I don't see a lot of justification for `bytevector->list`.
`Gleckler`::
> I don't see the point. These conversions are generally wasteful, especially as simple intermediate values, and direct conversions would be better. However, we shouldn't include the full direct conversions matrix because that's just too large, especially for WG1.
`Medernach`::
> This is more part of a WG2 library.
`Shinn`::
> I'd rather these be part of the general extended bytevector library from wg2. Bytevectors are a second class data structure that can only hold bytes, not a general purpose one like lists or vectors.
`SnellPym`::
> Vectors and bytevectors are more akin. I don't really see a need for a single "universal" intermediary; unnecessary inconsistency is bad, but situations like converting between strings and bytevectors are useful as a chokepoint at which to concentrate the complexity of encoding choices.

### #444 Add vector-append procedure

This is for completeness with `append` and `string-append`.  See #436
for the Formal Comment that triggered this ticket.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided
* [Lucier](WG1BallotLucier.md): yes, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 9:0, 9:0
* **Rationales:**

`Cowan`::
> Consistency and completeness.
`Gleckler`::
> Yes, for consistency and completeness.
`Shinn`::
> Although I disagree with the rationale in the formal comment, this is a generally useful function.
`SnellPym`::
> I'm a sucker for consistency and complete sets of things, too. Is this why my house is overflowing with books?

### #451 Add bytevector-append procedure

This is for consistency with `append`, `string-append`, and
`vector-append` (per ticket #444) procedures.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided
* [Lucier](WG1BallotLucier.md): yes, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 9:0, 9:0
* **Rationales:**

`Cowan`::
> Consistency and completeness.
`Gleckler`::
> Yes, for consistency and completeness.
`Shinn`::
> This is perhaps less useful than `vector-append` but reasonable to include.
`SnellPym`::
> Well, the camel's nose was already in the tent...

### #445 Bidirectional ports and port-open?

Replace `port-open?` with `input-port-open?` and `output-port-open?`,
since a bidirectional port can be closed on one side without the
other.  See Formal Comment #439.

Vote `replace` to replace `port-open?` with just the two new versions,
or `add` to have all three.

* **Options:** replace, add, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): replace
* [Ganz](WG1BallotGanz.md): replace
* [Gleckler](WG1BallotGleckler.md): replace
* [Hsu](WG1BallotHsu.md): undecided, add
* [Lucier](WG1BallotLucier.md): add, replace, no
* [Medernach](WG1BallotMedernach.md): replace
* [Read](WG1BallotRead.md): replace
* [Shinn](WG1BallotShinn.md): replace, add
* [SnellPym](WG1BallotSnellPym.md): replace, add, no
* **Results:** **replace**, add, no, undecided
* **Ratios:** 7:2, 8:0, 8:1
* **Rationales:**

`Cowan`::
> It's better to keep these separate, even if marginally less convenient.
`Ganz`::
> Leaving `port-open?` leads to the question of whether it should require both sides or either side of a bidirectional port to be open. The answer should probably be both, but then you still need to use both the new procedures to determine that a port is closed, or add yet another.
`Gleckler`::
> Otherwise it's unclear what `port-open?` means on a bidirectional port.
`Shinn`::
> Contrary to close-port I think you always know which direction you need when checking if a port is open.

### #450 Eliminate default for fill argument in vector-copy

Marc Feeley writes:

It is a bad idea for the *fill* parameter of `vector-copy` to have a
default. When *fill* is absent, it should be an error when *start*
and *end* are not within the bounds of the sequence. Otherwise, some
index calculation errors (off-by-one on *end*) may go
unnoticed. Moreover, when it is supplied, *fill* should also be used
when *start* is less than 0, for consistency with the case where
*end* is greater to the length of the sequence.

Vote `required` to make the fill parameter required, `error` to make
it an error in the case that fill is absent yet needed, `remove` to
remove the fill parameter and signal a runtime error if end is longer
than the input vector, or `default` for the current status quo.

* **Options:** required, error, remove, default, undecided
* **Default:** default
* **Voters:**
* [Cowan](WG1BallotCowan.md): default
* [Ganz](WG1BallotGanz.md): error
* [Gleckler](WG1BallotGleckler.md): remove, error, required
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): remove, error, default, undecided, required
* [Read](WG1BallotRead.md): remove
* [Shinn](WG1BallotShinn.md): remove, default, error
* [SnellPym](WG1BallotSnellPym.md): error, remove, required, default
* **Results:** **remove**, error, default, undecided, required
* **Ratios:** 4:2, 5:1, 5:2, 5:0
* **Rationales:**

`Cowan`::
> This routine comes from SRFI-43 and should be kept compatible with it.
`Ganz`::
> Marc's suggestions sound convincing to me.
`Gleckler`::
> While allowing the `fill` parameter to have a default value is compatible with SRFI 43, Marc's argument about detecting errors is strong. However, I disagree that the idea of allowing `start` to be negative is somehow more consistent. The obvious application of the `fill` parameter is implementing variable-length data structures out of vectors, extending the original in the process. However, using the name `vector-copy` for this purpose is an awkward overloading. A separate procedure, not defined in WG1 Scheme, should be used for that purpose. I've updated WG1Ballot, so others should adjust their votes if they see fit.
`Medernach`::
> It should be an error to cross vector boundary, lest something terribly wrong may happen unnoticed. As Alex said, the best is IMHO to remove this fill parameter.
`Read`::
> Seems to me like needless complexity.
`Shinn`::
> I think we're conflating the vector-copy and vector-fill! concepts here and should just remove the fill parameter. It should be a runtime error if end is longer than the input vector. Either way, we can't make the final parameter required without making start and end required as well, which is too clumsy.
`SnellPym`::
> I like the reasoning about making off-by-ones into real errors.

### #404 Make handlers take a raise-continuable? argument.

Pass exception handlers a second, Boolean argument that declares
whether the exception is continuable.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Ganz](WG1BallotGanz.md): undecided
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): no, undecided
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): undecided, no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **no**, undecided, yes
* **Ratios:** 5:3, 5:1
* **Rationales:**

`Cowan`::
> Given that Scheme has no way of calling a procedure portably when the number of arguments it accepts is unknown, I have to reluctantly reject this, even though it is clearly the Right Thing. Many handlers will not care, and they should not be required to be retrofitted with a dummy second argument.
`Ganz`::
> Seems like a lot of wires to connect. I can see why this might be useful, but would like to see some examples of programs that rely on this, and decide whether this is really the best way. For example, perhaps `with-exception-handler` can provide separate handlers for the continuable and non-continuable cases.
`Gleckler`::
> I would like to see this, too, but agree that passing it to every handler is overly verbose considering how rarely it would be used. We should address this in WG2 where we will, I hope, have a more powerful exception system.
`Medernach`::
> This is a needed feature, but I am not convinced that this is the correct way to express it ?
`Shinn`::
> This has no precedent, and could be difficult for implementations to support.
`SnellPym`::
> I think this is useful information to know.

### #464 Add optional start and end parameters to utf8->string and string->utf8.

Per ticket 464, add optional start and end arguments to `utf8->string`
and `string->utf8`.

Vote `both` to add optional start and end arguments to both,
`string->utf8` or `utf8->string` to add them to only one procedure, or
`neither` to leave both unchanged.

* **Options:** both, string->utf8, utf8->string, neither
* **Default:** neither
* **Voters:**
* [Cowan](WG1BallotCowan.md): both, utf8->string
* [Ganz](WG1BallotGanz.md): both
* [Gleckler](WG1BallotGleckler.md): both, utf8->string, no
* [Lucier](WG1BallotLucier.md): both
* [Medernach](WG1BallotMedernach.md): both
* [Read](WG1BallotRead.md): both
* [Shinn](WG1BallotShinn.md): both
* [SnellPym](WG1BallotSnellPym.md): both, utf8->string, (string->utf8 neither)
* **Results:** **both**, utf8->string, string->utf8, neither, no
* **Ratios:** 8:0, 8:0, 8:0, 8:0
* **Rationales:**

`Cowan`::
> I think both of these are wins, but converting part of a bytevector into a string without having to copy it first is probably the more important part.
`Ganz`::
> Consistency is good. BTW, I REALLY dislike these names. The bytevector is not utf8, it's just bytes. If anything, the string is utf8. I prefer `bytevector->string`/`string->bytevector`, possibly taking `utf8` as a symbol argument, to allow support for alternative coding. At a minimum, `bytevector->utf8`/`utf8->bytevector` would make more sense.
`Gleckler`::
> As Marijn says, it's useful to be able to avoid extra copying while decoding a string. We should change both for symmetry.
`Shinn`::
> The arguments to utf8->string are necessary for many common idioms of reading binary data and extracting strings from it. The reverse is much less useful, but makes sense for symmetry.
`SnellPym`::
> Consistency!

## WG1 - Optional Libraries

### #373 (exit #t) should be the same as (exit)

See Formal Comment #372 for the argument.  Cowan writes: "I support this proposal.  I
don't support the alternative proposal to just say that any true value
reports success and only #f reports failure, for there is usually only
one kind of success (0 on Posix and Windows, "" on Plan 9, 2 on VMS)
and many kinds of failure."

It is reasonable and convenient to use `#t`/`#f` as generic
success/failure for portable programs, with `(exit)` as a shorthand
for the "normal" completion `(exit #t)`.

Another reasonable extension is fallback for certain success values
that the implementation cannot understand.  Specifically, `0` is
commonly used for success on Posix systems, and the empty string "" as
success on Plan9.  We could require that if the implementation does
not know how to pass these value types (string or number) to the OS,
then it should recognize `0` and `""` as true.  Any value other than
these which cannot be passed to the OS should be treated as a generic
error.  That way, a program written for Posix that alternatively uses
`(exit 0)` and `(exit <n>)` will still work as desired on a Plan9
system, only losing details of the type of failure (and likewise for
Plan9 programs running on Posix).

In either case, unless someone makes a proposal to the contrary,
unknown values should always be treated as generic failure, and never
raise an exception or fail to exit (from #374).

* **Proposals:**
* **boolean:** Only `#t`/`#f` are as described as above, and all other values are passed (as best as possible) to the OS and therefore implementation-defined
* **extended-true:** `#f` is generic failure, `#t` generic success, and `""` and `0` are generic success if not otherwise understood by the OS
* **Options:** boolean, extended-true, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): boolean
* [Ganz](WG1BallotGanz.md): boolean
* [Gleckler](WG1BallotGleckler.md): boolean, unspecified
* [Hsu](WG1BallotHsu.md): boolean
* [Lucier](WG1BallotLucier.md): boolean, unspecified
* [Medernach](WG1BallotMedernach.md): boolean, unspecified, undecided, extended-true
* [Shinn](WG1BallotShinn.md): extended-true, boolean
* [SnellPym](WG1BallotSnellPym.md): boolean, extended-true, unspecified
* **Results:** **boolean**, unspecified, extended-true, undecided
* **Ratios:** 8:0, 7:1, 8:0
* **Rationales:**

`Cowan`::
> `Extended-true` is going too far: 0 means success on many systems, but failure (specifically: failure of unknown origin) on VMS. Portable code should stick with `#t` and `#f`.
`Ganz`::
> Don't add unnecessary OS dependencies.
`Gleckler`::
> Making `(exit #t)` have the same effect as `(exit)` is a no-brainer. I can see no portable reason to treat the empty string and zero specially.
`Shinn`::
> This is the only way to allow programs to exit with meaningful values while remaining portable.

### #375 Add EMERGENCY-EXIT procedure

This procedure provides instant guaranteed process exit without
running `dynamic-wind` thunks.  This is a low-level and dangerous
procedure.

Vote `emergency-exit` to add this procedure, or `no` to leave it out.
If you want to write in an alternate name, be sure to include
`emergency-exit` as a secondary option after it.

* **Options:** emergency-exit, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): exit-immediately, emergency-exit
* [Ganz](WG1BallotGanz.md): abort, emergency-exit
* [Gleckler](WG1BallotGleckler.md): exit-immediately, emergency-exit
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): exit-immediately, emergency-exit, undecided, no
* [Read](WG1BallotRead.md): exit!, emergency-exit
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, emergency-exit
* **Results:** **emergency-exit**, no, exit-immediately, undecided, abort, exit!
* **Ratios:** 5:4, 3:3, 6:0, 5:1, 5:1
* **Rationales:**

`Cowan`::
> Given that `exit` unwinds thunks, this is needed. It should also call `_exit()` on Posix and Windows, so stdio buffers are not flushed either. I agree that `exit-immediately` is a better name.
`Ganz`::
> I think `abort` is standard for this. It is probably not recommended in production code, but is useful for completeness, for education, and potentially for debugging.
`Gleckler`::
> I hate the name, but it makes sense to be able to exit immediately. Frankly, I'd rather that `exit` did this, or that its behavior with regard to `dynamic-wind` was unspecified. It's my impression that people use `exit` when they want their program to stop immediately. They don't want a `dynamic-wind` form to prevent their exit, for example. I suggest the name `exit-immediately`.
`Hsu`::
> This is not a reasonable thing to have in WG1's version of Scheme.
`Medernach`::
> This is occasionally (or unfortunately) needed in critical systems. Arthur's name proposal is certainly better.
`Shinn`::
> Maybe in WG2, but this is too dangerous for general use.
`SnellPym`::
> Looks too dangerous and low-level, IMHO.

### #394 Ditching SCHEME-REPORT-ENVIRONMENT and NULL-ENVIRONMENT

Cowan writes:

"I have reluctantly come to the same conclusion as the R6RS editors:
that in a Scheme with libraries, `scheme-report-environment` and
`null-environment` don't make much sense.  They are not in IEEE Scheme
or R4RS, so there is no formal barrier to removing them.

"Semantically, `scheme-report-environment` holds all the identifiers in
R5RS, excepting any which the implementation doesn't provide, like
`make-rectangular` if it does not have complex numbers.
`Null-environment`, on the other hand, contains only the syntax
keywords with none of the standard procedures: it is not an empty
environment.  R6RS preserves these procedures only in the R5RS
compatibility library, where they expose only R5RS content.

"When adapting the definition to R7RS, I changed
`scheme-report-environment` to contain all the identifiers in all the
standard libraries that the implementation provides, and
`null-environment` all the syntax keywords in those libraries.  This
was the best I thought I could do, but now I think that it provides
very little utility.

"It's possible to construct any specific environment you want by using
the `environment` procedure, which turns a sequence of import-specs
into an environment.  In particular, we now have the `(scheme r5rs)`
library, which essentially provides what
`(scheme-environment-procedure 5)` should provide, and there is no
portable use of any argument other than 5."

Vote `remove` to remove these two procedures entirely, or `move` to
move them from (scheme eval) and provide them only as portability
options in `(scheme r5rs)`, where only the argument 5 is required to
be supported.  Vote `keep` to leave them as-is.

* **Options:** remove, move, keep, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): move, remove
* [Ganz](WG1BallotGanz.md): remove, move, keep
* [Gleckler](WG1BallotGleckler.md): move, remove
* [Hsu](WG1BallotHsu.md): remove, move, keep
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): move, remove, keep, undecided
* [Read](WG1BallotRead.md): move
* [Shinn](WG1BallotShinn.md): move
* [SnellPym](WG1BallotSnellPym.md): move, remove, keep
* **Results:** **move**, remove, keep, undecided
* **Ratios:** 6:2, 8:0, 8:1
* **Rationales:**

`Cowan`::
> These do no harm in the R5RS-compatibility library, but are actively harmful (because misleading) in the `eval` library where they are today.
`Gleckler`::
> I see John's point, but we shouldn't break compatibility with R5RS.
`Shinn`::
> I agree they are less meaningful, but want to preserve `(scheme r5rs)` compatibility as much as possible.

### #413 EVAL accepts DEFINE

The proposal is to require `eval` to accept definitions as well as
expressions, as long as the specified environment is mutable.  See
[EvalDefine](EvalDefine.md) for which Schemes already handle this.

* **Options:** yes, no, unspecified, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): yes, but
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes, no
* [Medernach](WG1BallotMedernach.md): yes, unspecified
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, unspecified, but, no
* **Ratios:** 9:0, 9:0, 9:0
* **Rationales:**

`Cowan`::
> We've got first-class environments now, we should make use of them.
`Ganz`::
> I would expect and prefer all environments to be mutable, at least by default, i.e., `null-environment` and `environment` are procedures that always return a "new" environment with a particular structure that can be modified by `eval`.
`Gleckler`::
> `eval` should accept the full language, including defines.
`Shinn`::
> We've got first-class environments now, we should make use of them.
`SnellPym`::
> I see it as a useful facility for many uses.

### #399 clarify which primitives are allowed to implicitly force

The standard allows the following extension to force:

> Some implementations may implement "implicit forcing," where the
> value of a promise is forced by primitive procedures like `cdr'
> and `+'

We should remove this note or tighten the definition.

A simple definition is any primitive that would require a type-check
can perform implicit forcing.  This would include all type predicates
themselves except for `promise?`.  Note if #405 passes, then in
implementations which support this extension an object could return
`#t` for `promise?` in addition to one other type.

* **Options:** remove, type-check, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): type-check, unspecified
* [Ganz](WG1BallotGanz.md): type-check, unspecified
* [Gleckler](WG1BallotGleckler.md): unspecified
* [Hsu](WG1BallotHsu.md): remove, undecided
* [Lucier](WG1BallotLucier.md): type-check, unspecified
* [Medernach](WG1BallotMedernach.md): remove, unspecified, type-check, undecided
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): unspecified, type-check
* [SnellPym](WG1BallotSnellPym.md): type-check, unspecified, remove
* **Results:** *type-check*, unspecified, remove, undecided
* **Ratios:** 4:3, 5:2, 6:2
* **Rationales:**

`Cowan`::
> The `type-check` rule is sensible and clear.
`Gleckler`::
> I don't see any reason to change this.
`Medernach`::
> Conflating a lazy language with standard Scheme is not a good idea, especially for primitives. One may still provide its own extensions, but no standard code have to rely on this.
`Shinn`::
> I don't like removing this extension, which is still available in chibi-scheme. `type-check` is slightly different from what chibi currently does.

### #405 Make promises first-class

Currently there is no way to inspect an object to see if it's a
promise.  This proposal makes promises first-class by adding a
`promise?` predicate.  It also requires that if the argument to
`make-promise` is a promise, it is returned without rewrapping it, and
that if `force` is given a non-promise argument, it returns it
unchanged.  (These things cannot be provided by the user without a
`promise?` predicate, and are trivial to provide with it.)

Vote `disjoint` to add `promise?` and make it a disjoint type, or
`yes` to add it as a not-necessarily disjoint predicate.

* **Options:** disjoint, yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): disjoint, yes
* [Ganz](WG1BallotGanz.md): yes, no, undecided
* [Gleckler](WG1BallotGleckler.md): disjoint, yes
* [Hsu](WG1BallotHsu.md): yes, disjoint, undecided
* [Lucier](WG1BallotLucier.md): disjoint
* [Medernach](WG1BallotMedernach.md): yes, undecided, disjoint, no
* [Read](WG1BallotRead.md): disjoint
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, (no disjoint)
* **Results:** **yes**, disjoint, undecided, no
* **Ratios:** 5:4, 7:0, 7:0
* **Rationales:**

`Cowan`::
> Promises should be disjoint, specifically disjoint from procedures. See [DisjointProcedures](DisjointProcedures.md) for which are and which are not.
`Ganz`::
> The `disjoint` option seems inconsistent with implicit forcing.
`Gleckler`::
> The `promise?` procedure is useful, as the examples above demonstrate. Once we support `promise?`, keeping them disjoint seems easy, since if one implements them directly as procedures, one can't provide `promise?` without an inefficient table lookup anyway.
`Medernach`::
> We should have a way to check for promises, but not make it a disjoint type. It is perfectly reasonable to implement promises with thunks.
`Shinn`::
> `promise?` is useful, I don't think it needs to be disjoint. One must still be aware that the result of `(delay <expr>)` may not answer true to `promise?` given the existing allowed extensions.

### #462 end of line definition

The definition of read-line allows implementation defined extensions
to the set of end of line sequences. This is arguably too loose, as an
implementation could define "a" as and end of line. On the other hand,
if we do want to leave this in it may make sense to remove "\r", which
is no longer used in any contemporary OS.

Vote `no-extensions` to forbid implementation defined extensions,
`no-return` to remove a single return from the list of required end of
lines, and `none` to leave as-is.

* **Options:** no-extensions, no-return, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): no, no-return
* [Ganz](WG1BallotGanz.md): no-return
* [Gleckler](WG1BallotGleckler.md): no, no-return
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): no, undecided
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no-return, no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, no-return, undecided
* **Ratios:** 7:2, 8:0
* **Rationales:**

`Cowan`::
> The implementation-defined extensions is for the sake of R6RS systems that treat EBCDIC NEL and Unicode LS as line delimiters (a la XML 1.1, both my doing, I fear). Defining "a" as a line ending is a quality of implementation issue; the standard does not need to forbid implementations from being stupid, because nobody will use such an implementation. I agree we could live without `\r`, but it's very standard today in such routines.
`Ganz`::
> I think we can trust that implementations will be reasonable here. They should be left enough leeway to deal with any OS.
`Gleckler`::
> Implementations are going to define extensions, and it's unreasonable to expect them not to.
`Medernach`::
> Not any sensible implementation would do such thing !
`Shinn`::
> Allowing extensions is reasonable, and given that case there is no need to explicitly mention the antiquated "\r".
`SnellPym`::
> Implementations can already do stupid things, like provide only a few bytes of memory and therefore rejecting all nontrivial programs; common sense is enough of a barrier to that.

### #452 provide digit-value support for non-decimal-digits

In ballot 4, in symmetry with the new Unicode definition of
`char-numeric?` and as an analog to CL's `digit-char-p`, we provided
`digit-value`.

An informal comment was made questioning this procedure, and
suggesting if provided at all it be extended to hex digits.

Vote `ascii-hex` to support only the ASCII hex digits a-f,A-F (in
addition to full Unicode numeric digits), `unicode-hex` to support all
Unicode variants of a-f,A-F (need to define formally).

Vote `ascii-radix` or `unicode-radix` to have both `digit-value` and `char-numeric?` take a radix argument, such that `char-numeric?` returns #t and `digit-value` returns the appropriate value for characters representing non-numeric digits of that radix under ASCII or Unicode character encodings, respectively, and for characters representing numeric digits under Unicode.  Implementations are required to support at least the radix values: 2, 8, 10, and 16, and may support others.

Vote `remove` to remove `digit-value` entirely, `remove-radix` to remove `digit-value` entirely, but add the radix argument to `char-numeric?` as described above, or `keep` to keep as is.

* **Options:** ascii-hex, unicode-hex, ascii-radix, unicode-radix, remove, remove-radix, keep, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): keep
* [Ganz](WG1BallotGanz.md): unicode-radix, ascii-radix, remove-radix, keep
* [Gleckler](WG1BallotGleckler.md): remove, ascii-hex
* [Hsu](WG1BallotHsu.md): remove, keep, undecided
* [Lucier](WG1BallotLucier.md): ascii-hex, keep
* [Medernach](WG1BallotMedernach.md): remove, undecided, ascii-hex, keep, unicode-hex
* [Shinn](WG1BallotShinn.md): remove, keep, undecided
* [SnellPym](WG1BallotSnellPym.md): unicode-hex, ascii-hex, keep, remove
* **Results:** remove, ascii-hex, *keep*, undecided, unicode-hex, unicode-radix, ascii-radix, remove-radix
* **Ratios:** 4:2, 4:4, 5:0, 4:1, 5:1, 5:1, 5:1
* **Rationales:**

`Cowan`::
> Allowing the hex digits is an unnecessary wart.
`Ganz`::
> A radix argument allows support for letters a-f as digits without creating inconsistency with `char-numeric?`. It also allows easier access to these results for other numeric systems.
`Gleckler`::
> We've strayed into invention territory here, providing something that existing implementations don't. If we do keep it, it should support ASCII hex for compatibility with `string->number`.
`Medernach`::
> Let WG2 comes up with an Unicode library.
`Shinn`::
> digit-value is paired with char-numeric? which does not handle hex digits. The two of these can serve as a basis for hex-digit or base-N-digit procedures. The -radix variations are already getting too complicated for something in the core language. Since there is contention I'd just as soon move this to more general Unicode property handling in WG2, however.

## WG1 - Non-normative

### #411 Reference implementation

Our charter calls for one or more reference implementations.  As of
today, Chibi is very close to being so.  The proposal is to bless it
as a sample or model implementation, but not technically a reference
implementation -- if it disagrees with the standard, the standard
wins.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Ganz](WG1BallotGanz.md): undecided
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): yes, no, undecided
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 7:0, 7:2
* **Rationales:**

`Cowan`::
> Chibi is the de facto model implementation, and we may as well say so. It's small, easy to understand, and does a lot of things directly in Scheme.
`Ganz`::
> If it disagrees with the standard (as is always possible) one of them needs to be changed, even after we are officially relieved of our duty, so the distinction seems pedantic.
`Gleckler`::
> Chibi is the one implementation that be considered a reference implementation, and we are supposed to provide one, so let's make it official. Thank you very much to Alex for doing the work to make this happen.
`Medernach`::
> Of course. Thanks Alex for such an impressive work !
`Shinn`::
> The important thing is the standard comes first.
`SnellPym`::
> It's easier to have bugs in an implementation than in a specification.

### #463 library naming conventions

We currently use the singular form of data types for library names,
e.g. `(scheme char)` and `(scheme file)`.  R6RS prefers the plural, as
in `(scheme lists)` and `(scheme records)`. We should decide
officially which is preferred.

* **Options:** singular, plural, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): singular
* [Ganz](WG1BallotGanz.md): singular, plural
* [Gleckler](WG1BallotGleckler.md): singular, plural, unspecified
* [Hsu](WG1BallotHsu.md): plural
* [Lucier](WG1BallotLucier.md): plural, unspecified, undecided
* [Medernach](WG1BallotMedernach.md): plural
* [Read](WG1BallotRead.md): singular, unspecified
* [Shinn](WG1BallotShinn.md): plural, singular
* [SnellPym](WG1BallotSnellPym.md): singular
* **Results:** **singular**, plural, unspecified, undecided
* **Ratios:** 5:4, 6:1, 6:1
* **Rationales:**

`Cowan`::
> This is a matter of taste, but I like the singular form and see no reason to change it.
`Ganz`::
> Just personal preference.
`Gleckler`::
> I'll bikeshed for the singular. (I'm assuming that what we're saying here is that we'll change all of the data type names we use, not that we're saying anything about the names people use for purposes other than standard data types.)
`Shinn`::
> Not only R6RS but also several SRFIs, e.g. (srfi 99 records), have started a plural convention.


## WG1 - Late additions

### #465 Add jiffy-modulus to specify when, if ever, current-jiffy wraps

If the value of `current-jiffy` is to be both space-efficient (that is, a fixnum) and reasonably precise (say, microsecond timing), it needs to wrap around: 30-bit fixnums on a 32-bit system will wrap every 17 minutes.  That means an application needs to know what the maximum value is before it wraps back to zero.  The `jiffy-modulus` function returns the maximum value of the current jiffy plus 1.  Alternatively, jiffies can be signed and wrap from (- (jiffy-modulus) 1) to (- (jiffy-modulus)), which is easier for the implementation but harder for the user.

* **Options:** unsigned, signed, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): unsigned
* [Ganz](WG1BallotGanz.md): undecided
* [Gleckler](WG1BallotGleckler.md): undecided
* [Hsu](WG1BallotHsu.md): undecided, unsigned, no
* [Medernach](WG1BallotMedernach.md): undecided
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): unsigned, signed, no
* **Results:** *undecided*, unsigned, **no**, signed
* **Ratios:** 4:2, 4:2, 4:1
* **Rationales:**

`Cowan`::
> Unsigned will roll over faster if the full fixnum range is used, but it makes life easier for the user not to have to deal with negative jiffy values ever.
`Gleckler`::
> I don't know what existing implementations do.
`Hsu`::
> I am not sure that I really follow or understand a lot of these timing functions, and the idea of putting something that is implicitly a fixnum into the system strikes me as a bad design.
`Shinn`::
> As specified, this is broken, since if jiffies are equated with fixnums, and an implementation has no bignum support, then by definition it cannot represent the modulus. A better API is `(maximum-jiffy)` optionally with `(minumum-jiffy)` if negative values are allowed. I still find jiffies difficult to use and avoided them in some recent R7RS time based code.
`SnellPym`::
> I agree that wrapping jiffies are useful (and may be forced upon us by the underlying clock mechanism). However, I would also like some support for `jiffy-modulus` to be infinite, as forcing the jiffy counter to be a fixed-width number is unnecessary. Allowing it to return positive infinity would satiate me, I think!

### #466 case folding of character names

In ticket #11 we voted to make the reader case-sensitive
by default. In ticket #92 we further added the R6RS
#!fold-case and #!no-fold-case reader extensions. In
both cases the terminology was lax and simply referred
to "reader case sensitivity", and all discussion centered
around symbols, although in R6RS character names were
also affected.

Case folding will apply to numeric literals, booleans and
bytevectors regardless, as they do in both R5RS and R6RS.
We need to clarify how character names and the case
folding directives themselves are handled.

The default is `r6rs`, where character names are case
sensitive by default and folded by the `#!fold-case` flag:

​http://www.r6rs.org/final/html/r6rs-app/r6rs-app-Z-H-4.html#node_chap_B

Alternately character names could be made to ignore
the reader directives and always or never fold case.
Never folding case breaks R5RS and earlier compatibility
without any easy workaround.

These same settings apply to the `include-ci` syntax.

* **Proposals:**
* **r6rs:** character names behave like symbols, directives are sensitive
* **r6rs+directives:** like `r6rs` but directives can also be case-folded
* **always-fold:** like `r6rs` but character names and directives always fold case
* **never-fold:** like `r6rs` but character names and directives never fold case
* **Options:** r6rs, r6rs+directives, always-fold, never-fold, undecided
* **Default:** r6rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Ganz](WG1BallotGanz.md): r6rs, r6rs+directives, never-fold, always-fold
* [Gleckler](WG1BallotGleckler.md): r6rs, r6rs+directives
* [Hsu](WG1BallotHsu.md): r6rs, always-fold, r6rs+directives, undecided, never-fold
* [Lucier](WG1BallotLucier.md): r6rs, undecided
* [Medernach](WG1BallotMedernach.md): r6rs, r6rs+directives
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): r6rs, r6rs+directives, always-fold
* [SnellPym](WG1BallotSnellPym.md): always-fold, r6rs+directives, r6rs, never-fold
* **Results:** **r6rs**, r6rs+directives, always-fold, never-fold, undecided
* **Ratios:** 8:1, 8:1, 9:0, 9:0
* **Rationales:**

`Cowan`::
> I see no reason to deviate from R6RS here.
`Ganz`::
> Making case fold directives subject to themselves seems at best weird and at worst paradoxical: I suppose `#!nO-fOLD-cASE` is unspecified but `#!fOLD-cASE` is a valid directive? The very possibility that directives could be interpreted as case-folded forces them to be case-unambiguous anyway, or else the case-folding crowd will have to pick a selected interpretation. I don't think we want to go there. My understanding is that under `r6rs` option nothing prevents an implementation from supporting additional directives with any other case. I see no need to emphasize this possibility, though.
`Gleckler`::
> I see no reason to differ from R6RS here.
`Hsu`::
> It is needless complication to deviate from R6RS in this case.
`Shinn`::
> I can't think of a good reason to diverge from R6RS here.

### #467 Allow eqv? and eq? to return different answers on procedures as well as integers and characters

This proposal stems from [remarks](http://lists.r6rs.org/pipermail/r6rs-discuss/2012-July/006405.html) by Alaric Snell-Pym and Will Clinger on the r6rs public mailing list.  If `eq?` is allowed to return `#f` on two procedures when `eqv?` nevertheless returns `#t`, as is already the case for numbers and characters, then more intelligent implementation-specific procedure comparisons using `eqv?` are possible, while still keeping `eq?` simple enough to inline easily.

Note that this is orthogonal to the question of #460, how `eqv?` works on procedures.  There should be little or no backward-compatibility hit for this change.

* **Proposals:**
* **same:** `eq?` and `eqv?` always return the same on procedures, per R5RS and R6RS
* **different:** `eq?` may return `#f` on procedures even when `eqv?` returns `#t` (but not vice versa)
* **Options:** same, different, undecided
* **Default:** same
* **Voters:**
* [Cowan](WG1BallotCowan.md): different
* [Ganz](WG1BallotGanz.md): same, undecided
* [Gleckler](WG1BallotGleckler.md): different
* [Hsu](WG1BallotHsu.md): undecided, different, same
* [Lucier](WG1BallotLucier.md): different
* [Medernach](WG1BallotMedernach.md): different
* [Radul](WG1BallotRadul.md): undecided
* [Read](WG1BallotRead.md): different
* [Shinn](WG1BallotShinn.md): undecided, different
* [SnellPym](WG1BallotSnellPym.md): different, same
* **Results:** **different**, undecided, same
* **Ratios:** 6:4, 8:1
* **Rationales:**

`Cowan`::
> If it's useful for Will and Larceny, that's good enough for me. This doesn't affect conformance, and is unlikely to affect user code either.
`Gleckler`::
> I will defer to Will's long experience in compiler implementation.
`Hsu`::
> This seems to be useful, but I am cautious about this.
`Radul`::
> It could be reasonable to allow a scenario where eq? compares procedures by location tags and eqv? does something coarser, like alpha renaming. I would, however, strongly object to allowing {{{ (let ((x (lambda ...))) (eq? x x)) }}} to evaluate to #f. I do not see any options on this item that I understand to correspond with this, which is the definition of "further discussion".
`Shinn`::
> I understand the desire to separate these two, but need to think about this more.
