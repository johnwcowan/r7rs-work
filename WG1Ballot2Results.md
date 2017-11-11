# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

# Instructions

* You may list as many of the options as you want in order of preference.
* Options are comma-delimited (ignoring space) and case-insensitive.
* You can pipe-delimit (|) options you want to give equal weight to.
* You may write in your own option if you announce it to the list first.
* You may specify a variant with option/variant, for example `srfi-1/module` to vote for `srfi-1` but clarify it should be in a separate module. Please also include the `srfi-1` option in this case.
* You can write a free-form rationale after the "preferences" line,
* `module` means "yes, but I want it in a separate module",
* `wg2` means "no, but I think it should go in WG2".
* `undecided` means I want to discuss this issue further.
* Abstain on any item by leaving the preferences blank.

# WG1 Ballot Items To Finalize By Jan. 9

## WG1 - Core

### #32 user-defined types

Do we support any means of creating disjoint user-defined types, such
as in SRFI-9, SRFI-99 or the R6RS record system?

* **Proposals:**
* **cowan:** [RecordsCowan](RecordsCowan.md)
* **hsu:** [RecordsArcfide](RecordsArcfide.md)
* **medernach:** [AggregatesMedernach](AggregatesMedernach.md)
* **rush:** [UserAggregatesRush](UserAggregatesRush.md)
* **snellpym:** [UniqueTypesSnellPym](UniqueTypesSnellPym.md)
* **Options:** srfi-9, srfi-57, srfi-99, r6rs, cowan, hsu, medernach, rush, snellpym, none, wg2, undecided
* **Default:** none
* **Voters:** 14: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Radul](WG1BallotRadul.md), [Read](WG1BallotRead.md), [Rush](WG1BallotRush.md), [Russel](WG1BallotRussel.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** **srfi-9**, hsu, cowan, medernach, snellpym, rush, wg2, no, r6rs, undecided, srfi-99, srfi-57, srfi99, snellpym+inheritance, snellpym+mutate, snellpym+inheritance+mutate, snellpym+mutable, srfi-99., medernach+inheritance, snellpym+mutate+inheritance
* **Ratios:** 6:4, 6:3, 4:5, 6:3, 5:5, 9:1, 9:1, 8:1, 9:2, 9:0, 9:0, 9:0, 9:0, 8:1, 8:1, 9:0, 9:0, 9:1, 8:1
* **Rationales:**

`Cowan`::
> I had a lot of trouble with this one. Srfi-9 has lots of existing use and is the nearest thing to a standard we have. On the other hand, my idea is small and can be more efficient than plain vectors, because it doesn't need a bounds check *and* a type check, just a type check.
`Gleckler`::
> I had initially voted for SRFI 99 as my top choice, but I'm now convinced that that's just too complicated a system for core Scheme. We need something more fundamental upon which other systems can be built. SRFI 9 is widely used and is about the simplest syntactic implementation one could hope for. However, it doesn't support inheritance. [RecordsArcfide](RecordsArcfide.md) is also simple and it supports inheritance. It is a syntactic system. [RecordsCowan](RecordsCowan.md) is also simple and also supports inheritance. Furthermore, it's a procedural system, which makes more sense as the fundamental approach for WG1, which should be about nailing down simple, clean design for the core ideas. [UniqueTypesSnellPym](UniqueTypesSnellPym.md) is a good distillation of the core ideas and is also procedural. However, its handling of subtypes (i.e. the requirement to pass <e> and <d> procedures rather than the supertype itself) and the way that fields are declared to be mutable are both awkward. However, I do agree with the premise stated in the Background section, i.e. that we should provide a mechanism on which other, more powerful and more widely adopted record systems can be built. [AggregatesMedernach](AggregatesMedernach.md) is another good distillation of the core ideas. However, constructing its aggregate functions is an all-or-nothing affair; the type switch mechanism seems to require complete destructuring of the record even when not all of the components are necessary, e.g. in the SRFI 9 predicate example; and there is no inheritance. SRFI 99 (ERR5RS Records) is an extension of SRFI 9 that is a rationalization of the R6RS system, so I'm voting for it ahead of the R6RS system. As the description says, "This entire SRFI is compatible with the procedural and inspection layers of the R6RS record system, but offers several worthwhile improvements over the R6RS system." Here is exactly how I came up with the preference order above: cowan > snellpym+inheritance+mutate because <cowan> is simpler and cleaner snellpym+inheritance+mutate > medernach snellpym+inheritance+mutate > snellpym+mutate hsu > srfi-9 because inheritance is desirable medernach > hsu snellpym+inheritance+mutate > hsu snellpym+mutate > hsu because procedural is more fundamental than syntactic srfi-9 > wg2 hsu > wg2 because WG1 Scheme should have some form of record definition facility wg2 > srfi-99 because of the high complexity of SRFI 99, as well as the problems others have reported srfi-99 > r6rs because SRFI 99 is a refinement of R6RS records, designed to solve some of their problems wg2 > r6rs because of the high complexity of R6RS, as well as the problems others have reported r6rs > snellpym because snellpym doesn't support mutation
`Medernach`::
> IMHO Core Scheme should have the ability to create disjoint kind of data, this is the essence of data types. On top of that capability the various kind of records could be build. I don't think much more is needed inside the core, let other features be for WG2. SRFI9 is nice but I am a bit afraid about its real extension potential to make it part of WG1 standard. [UniqueTypesSnellPym](UniqueTypesSnellPym.md) proposal is in the spirit of this, however I dislike to select fields by numbers and the list of flags for mutation looks hairy to me: it means for instance that I have to know the number of fields of the parent type, even worse: adding a field in a parent force to shift all the hierarchy of subtypes, this is too much tight together. Instead of a list of flags my proposal allow to have all fields mutable and controlling mutability by exporting with a module system only what mutators I allow to use. Update: I overtook that [RecordsCowan](RecordsCowan.md) inheritance is based on field numbering. Then there are the same problems than in snellpym+inheritance. However it is a nice and simple proposal but I prefer widespread SRFI-9 over it (no inheritance than number-based inheritance).
`Shinn`::
> I think providing a syntactic interface as the basis for records is more widely supportable and may offer better optimization chances, however the SRFI-99 API is too difficult to remember and [RecordsArcfide](RecordsArcfide.md) seems only partially better, so I prefer the de facto standard SRFI-9 for WG1.

### #50 byte-vectors

Several SRFIs, R6RS, and most Scheme implementations support some sort
of uniform packed integer vectors.  In particular, these are necessary
for efficient binary I/O, and for memory mapping, so WG2 will
certainly want them.

Do we provide a syntax and basic API for these in WG1?

* **Proposals:**
* **r6rs:** [R6RS byte-vectors](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2)
* **cowan:** BlobAPI
* **snellpym:** [BlobsAndSRFI](BlobsAndSRFI.md)4SnellPym
* **Options:** r6rs, cowan, snellpym, wg2, none, undecided
* **Default:** none
* **Voters:** 12: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Radul](WG1BallotRadul.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** **cowan**, r6rs, wg2, snellpym, undecided, no, snellpym/module, cowan/module
* **Ratios:** 8:2, 6:5, 8:1, 8:2, 9:0, 8:1, 8:1
* **Rationales:**

`Gleckler`::
> Reviewing the proposals again and taking a look at other people's votes, I'm now convinced that the <cowan> proposal is better than <r6rs>. That's primarily because <cowan> makes endianness a property of each accessor and mutator procedure rather than a parameter, which means that efficiency through inlining is even easier to achieve. However, I still prefer the term "byte vector" to "blob." We should specify what Unicode procedures are required, if any, when the Scheme implementation doesn't support Unicode. Once again, though, it would be great if people writing proposals specifically justified their design decisions in the text of their proposals, particularly when deviating from existing Scheme standards. That would make voting much easier.
`Harvey`::
> But I'd really like a better name than "blob"!
`Hsu`::
> R6RS has a clumsy interface that is corrected, IMO, by Cowan's proposal.
`Lucier`::
> I prefer "full cowan", what he's recommending for WG2.
`Shinn`::
> I like the idea behind `snellpym` but it needs work - I'd rather give the author time to flesh it out. Failing that, I don't see what differentiates BlobAPI from the R6RS bytevectors.

### #55 Lazy evaluation

R5RS provides a simple mechanism for easy cases of lazy evaluation.
It does not support generalized lazy evaluation, because all built-in
procedures are eager whether they 'need' to be or not.  The relevant
identifiers are `delay` and `force`; they are not present in IEEE
Scheme.  SRFI 45 argues that this pair is insufficient for expressing
common lazy algorithms in a way that preserves tail recursion, and
adds `lazy` (equivalent to `(delay (force ...))`, but acting
atomically) and `eager`.  The semantics of `delay` and `force` remain
downward compatible.

Vote `srfi-45` to add just the bindings `lazy` and `eager` in addition
to `delay` and `force`, not all of the `srfi-45` utilities.  Vote
`none` to remove `delay` and `force` from the standard.

* **Options:** r5rs, srfi-45, none, wg2, undecided
* **Default:** r5rs
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **r5rs**, srfi-45, wg2, undecided, no
* **Ratios:** 5:4, 4:3, 5:2, 5:2
* **Rationales:**

`Cowan`::
> I find the arguments found in SRFI 45 itself compelling: with `lazy`, it becomes possible to mechanically change ordinary eager expressions into lazy ones mechanically. Using `delay` causes a failure of tail recursion.
`Gleckler`::
> As Alex has said in his message [item #55 - I'd rather be lazy than eager with new primitives](https://groups.google.com/d/msg/scheme-reports-wg1/qzAeT7395Sk/fTlT-BYTVysJ), there hasn't been enough discussion of or experience with SRFI 45 to justify its inclusion in WG1 Scheme.

### #57 Random Numbers

Random numbers are useful for a wide variety of applications,
including cryptography, statistics, games, simulations and genetic
programming.  Do we want to provide an interface to random number
generation in WG1 Scheme?

* **Proposals:**
* **srfi-27:** [SRFI-27](http://srfi.schemers.org/srfi-27/srfi-27.html)
* **cl:** [RandomnessCommonLisp](RandomnessCommonLisp.md)
* **cowan:** [RandomCowan](RandomCowan.md)
* **hsu:** [RandomnessArcfide](RandomnessArcfide.md)
* **Options:** srfi-27, cl, cowan, hsu, none, wg2, undecided
* **Default:** none
* **Voters:** 14: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Radul](WG1BallotRadul.md), [Read](WG1BallotRead.md), [Rush](WG1BallotRush.md), [Russel](WG1BallotRussel.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** *wg2*, cl, srfi-27, cowan, **no**, hsu, cl/module, srfi-27/module, cowan/module, undecided, cowan/core, srfi-27-core, srfi-27/core
* **Ratios:** 8:4, 7:4, 8:5, 9:1, 9:3, 8:2, 8:2, 8:2, 9:2, 9:1, 9:1, 10:0
* **Rationales:**

`Cowan`::
> The CL system has been around for quite a while, and seems to suit. The implementation gets to pick an appropriate algorithm for its situation. Srfi-27, with its fixed implementation, makes more sense as a library. I voted against my own idea here.
`Gleckler`::
> [RandomCowan](RandomCowan.md) does not allow control over the seed, so it is of such limited usefulness as to not be worth including. The API defined by SRFI 27 does allow control of the seed, and makes random sources first class, both of which are good ideas. However, the API is awkward, especially `random-source-state-ref` and `random-source-state-set!`. I'd like to see WG2 do a survey of existing implementations and find something better than both of these proposals. The Common Lisp-based proposal is the best developed of all of these.
`Hsu`::
> I tend to think that CL is okay here, but absent that choice, a simpler approach that is flexible is preferable.
`Lucier`::
> I don't think we should be trying to provide randomness for cryptography applications, it's too hard (meaning, I don't understand how to do it ;-). I thought originally that SRFI-27 would be good because it has a good base generator in its default implementation and a good interface for getting random integers and reals. However, [RandomnessCommonLisp](RandomnessCommonLisp.md) provides an interface that may be a better starting point for what we want. I'd like to separate the generation of inexact and integer random numbers, and also to provide (random-source-later-stream random-source [optional-index]) and (random-source-later-substream random-source [optional-index]), which would provide a basis for applications in "statistics, games, simulations, ... genetic programming" and the other simulation-type applications.
`Medernach`::
> This is really a module issue, let people choose among a set the one which fits the best their needs. Standardize names only for helping code reuse. By the way if one need repeatability why not roll their own random stream from a saved persistent table ? (as good old random number tables one could buy for those who knew about it :)
`Radul`::
> If you can't do it right, don't do it at all. Unlike a module system, randomness can be retrofit at user level, so Scheme will not shrivel up and die if we wait for perfection.
`Sussman`::
> Do not introduce anything "not necessarily of very high quality" into the language! Don't do anything that Knuth and Kahan would not approve of! If you have good integers and assignment a user can make his own, so this is not essential.

### #62 Environment Variables

Currently, there is no standard way to communicate with the context
from which a Scheme program was started.  This has become pretty
standardized over time: a list of strings ("command-line arguments")
and a map from strings to strings ("environment variables") on input,
and a small integer or string on output ("exit value").  Scheme should
recognize these realities.

We have `command-line` and `exit` from [ModulesShinn](ModulesShinn.md), so the question
remains if we should add SRFI-98 environment accessors.

* **Options:** srfi-98, none, wg2, undecided
* **Default:** none
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **srfi-98**, srfi-98/module, wg2, no, undecided
* **Ratios:** 6:3, 6:3, 7:2, 9:0
* **Rationales:**

`Cowan`::
> Most programming environments can supply these, and programs should have easy access to them. Putting them in a module makes them optional for small Scheme implementations that can't support them. `Command-line` and `exit` should go in the same module.
`Ganz`::
> Yes, a must-have.
`Gleckler`::
> This shouldn't be included in WG1 Scheme except as a module. Environment variables, while common, are operating system specific. Embedded systems are unlikely to have them.
`Medernach`::
> This should not be enforced, as embedded devices don't need it. Providing it as a module is a good choice.

### #68 "Undefined value" vs. "undefined values"

In R5RS, many procedures and syntax forms return an "undefined value".
In R6RS, the corresponding convention is to return "undefined values",
meaning an undefined number (including zero) of undefined values.  How
shall R7RS go?

Vote `r5rs` for a single undefined value, `r6rs` for zero or more
undefined values, or `zero` for exactly zero values.  Anything other
than `r5rs` would break R5RS (and IEEE) compatibility.

* **Options:** r5rs, r6rs, zero, undecided
* **Default:** r5rs
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **r5rs**, r6rs, zero, undecided
* **Ratios:** 5:3, 6:3, 8:0
* **Rationales:**

`Cowan`::
> R5RS, reluctantly. I really don't think it would in practice break compatibility, because in practice Scheme implementations are okay with handling multiple values in `begin` forms and the equivalent.
`Ganz`::
> This seems much more elegant -- as long as we've got multiple values, use zero of them.
`Medernach`::
> What is the rationale behind this ? Is it to allow partial functions returning an <undefined> value and to propagate it ? Then IMHO the best semantic is to return empty value. Update: I am now convinced that R6RS phrasing is the more flexible option. However I exhort using "(values)" whenever possible.
`Shinn`::
> Too many existing programs expect exactly one value.
`SnellPym`::
> "exactly zero" undefined values is just plain arbitrary and therefore sucks. An undefined number allows for future expansion, compatibly.

### #49 Undefined value API

Assuming a single "undefined" value (dependent on the result of #68),
users sometimes want to test for this value.  If we enforce a unique
undefined value, one approach is to generate this value explicitly to
test against, such as `(void)` provided by some implementations.
Alternately we could provide a test such as `undefined?`.  Either
approach breaks compatibility with existing extensions, and may be
prohibitively difficult to implement for compilers lacking a separate
undefined value type.  Some programmers also consider testing for this
value sign of a broken design.

Vote `generate` for a `(void)` procedure, `test` for `undefined?`, and
`both` for both.

* **Options:** generate, test, both, none, wg2, undecided
* **Default:** none
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, generate, both, test, wg2, undecided
* **Ratios:** 6:3, 6:1, 6:0, 6:0, 6:0
* **Rationales:**

`Cowan`::
> I really don't like this; it encourages people to have such a defined-undefined value, which is semantically bogus.
`Ganz`::
> But hopefully will be irrelevant based on #68.
`Gleckler`::
> Undefined should be undefined. Being able to test for it makes it defined. Implementations should be given freedom to interpret undefined in a way that is appropriate and efficient, not constrained in this strange and contradictory way.
`Hsu`::
> Testing for this value is bad practice. Likewise, there is no reason that this procedure must return a single value. Systems like Chez Scheme normalize to this void object, but the point of `(void)` is to enable one to explicitly make a procedure return unspecified values. This is valid whether or not we have zero, one, or any number of unspecified values returned (#68). It is useful regardless, and I would like to have it regardless of the outcome of #68.
`Medernach`::
> Testing undefined values is strange to say the least... It seems better to have error handling instead when appropriate.
`Rush`::
> This is fairly important to get right. I used to advocate the "both" position, but Ihave since decided that many of the use cases for (void) and undefined? are much better served by using explicit-CPS forms (particularly is searching applications). Hence my "none" vote is a positive vote that none is the right thing, rather than a "Let's not do anything about this" vote.
`Shinn`::
> It's a bug to write programs which rely on this - unspecified is unspecified, and may be anything or even vary per compiler and program and call.
`SnellPym`::
> Undefined values are room for future expansion, not specific null placeholders. I've used Chicken's (void) as a substitute for returning no values (and then relied on it being equal to itself in unit tests that force me to check the return value of procedures called only for side effect...); I'd rather put (values) at the end of side-effect-only procedures and have a test macro for this case, that doesn't compare return values!

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

Options are `equal?` to specify `equal?` must terminate on cyclic
input, `r6rs` to specify R6RS behavior for `list?` and `length`,
`srfi-1` to specify the SRFI-1 semantics (where `length` returns `#f`)
and `equal?+r6rs` or `equal?+srfi-1` are options for both.

* **Proposals:**
* **equal?:** `equal?` is required to do proper checks for cyclic structure to not diverge
* **r6rs:** `list?` should return `#f` and `length` raise an error for cyclic lists
* **srfi-1:** `length` is equivalent to `length+` and returns `#f` for a cyclic list
* **Options:** equal?,r6rs,srfi-1,equal?+r6rs,equal?+srfi-1,no,module,wg2,undecided
* **Voters:** 13: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Radul](WG1BallotRadul.md), [Rush](WG1BallotRush.md), [Russel](WG1BallotRussel.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** **equal?+r6rs**, r6rs, equal?, no, equal?+srfi-1, srfi-1, wg2, module, undecided
* **Ratios:** 5:4, 5:2, 5:3, 3:2, 4:2, 5:2, 5:0, 5:1
* **Rationales:**

`Gleckler`::
> We shouldn't force implementations to be slow in the usual case just to handle the possibility that they might encounter cyclic structures.
`Harvey`::
> I think I must be misunderstanding the issue about EQUAL?. You want to specify that it *must not terminate*? That seems, um, draconian.
`Medernach`::
> Don't let 'length' returns #f, please raise an error instead.
`Rush`::
> anything other than the listed choices is stupid, really.
`Shinn`::
> `equal?` is dangerous to use if it may diverge. It would be reasonable to leave this unspecified, but since we already require shared structures checks for `write` it seems consistent to make the same requirement for `equal?`.

### #69 Dynamic Parameters

Old-fashioned Lisps used dynamic extent of variables.  Although Scheme
has switched to lexical scope, the concept of a dynamic environment
can be useful in special cases.

Instead of special variables, SRFI-39 provides first-class "parameter"
objects with dynamic bindings.  Do we want to provide something
similar?

* **Proposals:**
* **srfi-39:** [SRFI-39](http://srfi.schemers.org/srfi-39/srfi-39.html)
* **cowan:** [ImmutableParametersCowan](ImmutableParametersCowan.md)
* **snellpym:** [ParametersSnellPym](ParametersSnellPym.md)
* **Options:** cowan, snellpym, srfi-39, none, wg2, undecided
* **Default:** none
* **Voters:** 11: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** **cowan**, srfi-39, snellpym, wg2, undecided, no
* **Ratios:** 5:4, 5:4, 6:1, 6:2, 6:1
* **Rationales:**

`Harvey`::
> My instinct is to vote "no" on everything, but I'm swayed by the argument that if we don't do it we'll get some hideous monstrosity foisted on us by wg2. :-)
`Shinn`::
> Explicit mutation is the uncommon case, and I think it's safe to leave the semantics of this unspecified in the presence of threads. It's crucial, however, that `parameterize` be thread-local.

### #70 accessing the system time

Short of a full time and date library, a single procedure

> (current-seconds)

returning the epoch time in seconds, possibly as a real number, would
be useful and is sufficient to implement a full library (though access
to the host system's timezone would be desirable in that case).

Since some systems may not have access to a clock, we could make this
an optional procedure.  Alternately, it could be defined as a simple
counter in such cases, providing an accurate notion of time ordering
but no sense of duration. Finally, it could return `#f` in the absense
of a clock.

* **Proposals:**
* **cowan:** [TimeCowan](TimeCowan.md)
* **required:** `(current-seconds)` must return seconds since epoch
* **optional:** `(current-seconds)` is an optional procedure
* **counter:** `(current-seconds)` may just be a counter, returning 0, 1, 2, ...
* **return-false:** `(current-seconds)` may return `#f` if unsupported
* **Options:** cowan, required, optional, counter, return-false, none, wg2, undecided
* **Default:** none
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **optional**, cowan, return-false, required, wg2, counter, undecided, no, cowan/module
* **Ratios:** 4:3, 4:3, 4:2, 5:1, 6:0, 5:2, 6:1, 6:0
* **Rationales:**

`Medernach`::
> We should make a difference between a date (social convention about naming points in time) and a measure of a time interval. IMHO this has to be optional and inside a module. ( And if this is optional we need of course a standard feature identifier to know whether it is present or not. ) About counter option, I would prefer not current-seconds returns tick instead of seconds : If only ticker is available it may be better to have a "current-tick" function instead in order to know that this is not seconds but ticks.
`Rush`::
> where's the "module" option? My vote is technically complicated here. This is a great wg1 library function, but the way this is worded (and the voting options) seem to less than obviously allow it as such.
`Shinn`::
> This is a *huge* can of worms. POSIX time is simply a bug, and I would rather not have any time API than put it in WG1, but I want time to consider alternatives.

### #109 elapsed time

Should we have functions allowing a program to compute elapsed time,
as distinct from calendar time?

[TimeCowan](TimeCowan.md) contains a proposal.

* **Options:** cowan, yes, no, wg2, undecided
* **Default:** no
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **cowan**, yes, wg2, undecided, no, chronometer/module, cowan/module
* **Ratios:** 4:2, 3:3, 6:0, 6:1, 5:1, 5:1
* **Rationales:**

`Medernach`::
> This has to be optional feature. A timer (or a ticker) is a definitely distinct from calendar time. I don't get the "jiffy" rationale. Something like the Chronometer proposal is more appealing to me because the reference time point is clearly stated.
`Shinn`::
> I don't see the point of this. Time should be handled properly by #70 or not at all.

### #78 Should we have case-lambda?

Should we provide case-lambda as in SRFI 16 and R6RS?  It provides
simple overloading of procedures based on the number of their
arguments, and does not require that optional arguments appear only
after mandatory ones.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **yes**, no, module, wg2, undecided
* **Ratios:** 5:4, 4:4, 5:2, 5:1
* **Rationales:**

`Cowan`::
> Overloading by argument count makes the most sense for a dynamically typed language, and it's just a syntax-rules macro.
`Rush`::
> I would like to see a pattern matching facility in Scheme that doesn't suck. Unfortunately, doing that crrectly depends heavily on getting the user aggregates question correct. I suggest that if there is a strong positive response to this question that the aggregates question be held open as well
`Shinn`::
> `case-lambda` is widely implemented but I think encourages bad style. It's a terrible API when you want optional arguments, and is less expressive than a more general `match-lambda`.

### #82 missing port? procedure

It's not clear whether R5RS requires a PORT? procedure or not.  It's
listed in Section 3.3.2 under Disjointness of Types, but not under
section 6.6.1 under Ports.  R6RS requires it.  Racket, Gauche, MIT
Scheme, Gambit, Chicken, Guile, SISC support it; Scheme48/scsh, Kawa,
and Chibi currently do not.

Shall we require it?

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **yes**, module, wg2, no
* **Ratios:** 9:0, 9:0, 9:0
* **Rationales:**

`Cowan`::
> Yes, do it; what the hell.
`Rush`::
> please let's not be silly
`Shinn`::
> This is trivial to define and often provided.

### #107 port status detection

Currently there's no way to determine whether a port is open or
closed, short of trying to read/write to it and catching an error.
Do we want to add an interface to this?

* **Options:** port-open?, port-closed?, both, no, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **port-open?**, port-closed?, both, wg2, undecided, no
* **Ratios:** 8:1, 8:1, 9:0, 9:0, 9:0
* **Rationales:**

`Cowan`::
> `Port-open?` is positive, and most of our predicates are in the positive: `zero?`, for example, not `non-zero?`.
`Gleckler`::
> There should certainly be some way to test whether a port is opened. I prefer to test the positive case, i.e. whether the port is open.
`Rush`::
> avoiding exceptions is good.
`Shinn`::
> In most programs you should know the lifetime of the port, but in some cases this is necessary. I prefer `port-open?` because it's more common to want to test the "positive" capability of reading/writing before doing so.

### #87 Allow multiple producers in `call-with-values`

In R5RS and R6RS, `call-with-values` takes two arguments, both
procedures.  The first is a *producer* of multiple values; the
second is a *consumer*, to which the multiple values returned by
*producer* are passed as arguments.

A possible extension is to allow multiple *producer* arguments,
flattening all the produced values together, analogous to Common
Lisp's `multiple-value-call`.

Do we add this extension?

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, wg2, yes, undecided
* **Ratios:** 6:0, 6:2, 6:1
* **Rationales:**

`Cowan`::
> My proposal.
`Medernach`::
> I don't see the added value of this.
`Rush`::
> I mean "NO". really. call-with-values should be *eliminated* from the language.
`Shinn`::
> Just because CL has something doesn't mean we should, and I haven't seen any convincing cases where this extension is useful.
`SnellPym`::
> This smacks of bloat to me

### #88 SRFI 87: => in CASE

SRFI-87 extends `case` with a `=>` clauses, analogous to the use of
`=>` in `cond` clauses, which allows you to pass the item actually
matched to a procedure.

Do we add this extension?

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **yes**, no, wg2, undecided
* **Ratios:** 6:3, 6:0, 6:0
* **Rationales:**

`Cowan`::
> Makes a lot of sense to me when you want to make the default do something outside the realm of `case`, like handle lists or vectors.
`Medernach`::
> We trivially have <key> already in lexical scope, don't we ?
`Shinn`::
> Syntactic sugar, rarely needed and easy enough to get around with an extra `let`.
`SnellPym`::
> It makes sense to be consistent with `cond`.

### #89 SRFI 61: COND => with generator and guard

SRFI-61 extends `=>` clauses in `cond` with an optional *guard*
form, such that after the value is generated and found to be true,
it's further checked against the guard.  If the guard returns `#f` the
clause fails and processing proceeds to the next clause, otherwise the
clause is accepted as normal.

Do we add this extension?

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, wg2, yes, undecided
* **Ratios:** 6:1, 7:2, 7:0
* **Rationales:**

`Cowan`::
> This one also makes a lot of sense to me.
`Gleckler`::
> This unnecessarily complicates COND.
`Hsu`::
> This is a good step towards unifying the syntaxes of various dispatch mechanisms, such as syntax-case, match, and cond.
`Medernach`::
> Not for WG1, Ok for WG2 if one wants it.
`Rush`::
> are we going to have a referendum on every SRFI?
`Shinn`::
> `cond` is complicated enough as it is.
`SnellPym`::
> This is kind of nice, but strikes me as a super duper optional extension rather than part of a jewel-like core.

### #90 Multiple values in COND => clauses

Currently, `=>` clauses in `cond` accept a single value from the
*generator* (right-hand side) and pass it to the *receiver*
(left-hand side).  Shall we allow the generator to return multiple
values and pass them to the receiver?  If both this ticket and #89
pass, multiple values would also be allowed for generator/guard `cond`
clauses.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, yes, wg2, undecided
* **Ratios:** 5:4, 5:1, 6:0
* **Rationales:**

`Cowan`::
> I said "Sure, why not?" but Alex convinced me not to, though I don't find all of his rationale convincing.
`Gleckler`::
> This doesn't make any sense. First, it's the left-hand side that is the generator and the right-hand side that is the receiver. And if the left-hand side generates multiple values, which one is tested for truth?
`Lucier`::
> Aren't "left-hand side" and "right-hand side" reversed in the description?
`Rush`::
> multiple-values is bad. the right thing to do is pattern match.
`Shinn`::
> Emphatically no. The whole point of `=>` clauses is they are testing that the value generated is true - if multiple values are generated, which do we test for truth? Any semantics will be unintuitive to some. In addition, this will make `=>` clauses slower in most implementations even if MV aren't used, because you need to account for them and box the result in the general case.
`SnellPym`::
> Multiple values shouldn't be second-class citizens. It's ugly when you can't use the usual niceties of Scheme just because you've broken out into multiple values.

### #91 INCLUDE at the REPL

Should we allow `(include "*filename*")` at the REPL?  This is
distinct from `import` in that it just loads code without altering the
module structure.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **yes**, no, wg2, undecided, wh2
* **Ratios:** 5:4, 5:2, 5:1, 5:0
* **Rationales:**

`Cowan`::
> Absolutely yes. This is a way of bringing chameleon code into the current context without having to mess with the module system. Modules are good; *required* modules are not so good.
`Shinn`::
> I don't see the point of this over `load`. The original ticket refers to `load` handling binary files whereas `include` would not, but in the proposed standard `load` isn't going to handle binary files anyway (beyond implementation-specific extensions).

### #92 Case-folding flags

The default reader in R7RS will default to case-sensitive, but users
may wish to override this in some situations.  R6RS allows at the
top-level #!case-fold and #!no-case-fold read syntax to control the
case-sensitivity of the current file.  Many existing R5RS
implementations, on the other hand, use #ci and #cs, with the
difference that they refer to the next datum only.

Note [PortsCowan](PortsCowan.md) provides a separate means of controlling
case-sensitivity per-port.

Vote `per-datum` for the next-datum-only #ci/#cs syntax.

* **Options:** r6rs, per-datum, none, wg2, undecided
* **Default:** none
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **r6rs**, per-datum, no, wg2, undecided, both
* **Ratios:** 7:2, 9:0, 9:0, 8:1, 8:1
* **Rationales:**

`Cowan`::
> Next-datum-only is annoying. However, `include-ci` would subsume this.
`Ganz`::
> if both, per-datum takes precedence.
`Rush`::
> this will be a WG1/2 compatibility issue. decide it here whichever way.
`Shinn`::
> Per-datum flags can be handled entirely by `read` without any need for mutable state. I also dislike all #! forms (of which there are currently none).

### #116 Source File Character Encoding

The standard currently says nothing about the character encoding
system of source files.  Do we require this to be a fixed encoding
such as UTF-8, use an out-of-band specification like the Emacs (and
Python) `-*- coding: foo -*-` convention, or just leave it
unspecified?

* **Options:** utf-8, emacs, unspecified, undecided
* **Default:** none
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **unspecified**, utf-8, emacs, undecided, no
* **Ratios:** 8:1, 8:1, 9:0, 9:0
* **Rationales:**

`Shinn`::
> The emacs approach is handy but too much of a kludge to go into the small Scheme standard, and I don't want to force utf-8.
`SnellPym`::
> I say "utf-8", but implicitly I assume that this is also bound by the implementation's restriction no available character set, so as not to require "full Unicode", so ASCII-only is Just Fine. Allowing specification of encoding names, like XML/emacs/Python, then requires standardising what encodings are allowed - and if implementations are allowed to support other encodings as well, then interoperability suffers.

### #93 Removing string mutability

R6RS relegated `string-set!` to a module, and many modern languages
tend towards making strings immutable.  Removing entirely, however,
breaks IEEE Scheme compatibility and should only be considered if you
believe mutable strings are fundamentally broken.

Do we remove `string-set!`?  Vote `yes` to remove, `module` to
relegate to a module as in R6RS, or `no` to keep as is.

* **Options:** yes, no, module, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** module, *no*, yes, undecided
* **Ratios:** 4:4, 3:2, 4:0
* **Rationales:**

`Cowan`::
> See [Removing `string-set!` from R7RS small Scheme](https://docs.google.com/View?id=dc46qrdf_21cxkmft28) for my arguments.
`Rush`::
> Scheme is a language with mutable bindings and data structures. deal with it.
`Shinn`::
> I consider mutable strings a design mistake in Scheme, but we need to preserve backwards compatibility so I prefer to discourage their mutation by putting them in a module. This is not just a symbolic gesture (like putting pair mutators in a module), because there are already existing Scheme implementations for which `string-set!` is very expensive.
`SnellPym`::
> Mutating strings gets us into a while characters-versus-graphemes-versus-codepoints Unicode mess. Plus, immutable strings open the doors for some useful efficiency gains.

### #83 Auxiliary Keywords

In R6RS auxiliary keywords (such as `else` in `cond` and `case` forms)
are explicitly exported from the `(rnrs base (6))` library.  Do we
want to bind and export these from the core library?

If `else` is bound in the default module, then it must be imported at
the call site whenever using it in `cond` or it won't match
hygienically.

If `else` is **not** bound in the default module, then it must not
be bound or imported at the call site whenever using it in `cond` or
it won't match hygienically.

Another option is to specify for `cond` and `case` that they match the
`else` identifier literally, ignoring any hygiene.  This breaks
compatibility with R5RS and R6RS.

* **Options:** bound, unbound, unhygienic, undecided
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **unbound**, undecided, bound, unhygienic
* **Ratios:** 4:3, 4:2, 4:1
* **Rationales:**

`Cowan`::
> Binding them is more in line with what people expect.
`Rush`::
> I'm not sure i understand the ramifications here. I think that the ballot is saying that unbound is the R5rs comaptible way, which we've been living with long enough to at least not be surprised by...
`Shinn`::
> `else` is conceptually unbound in the standard env and so should be specified as such (although for `case` it actually makes sense to match unhygienically since no other identifier would be legal).
`SnellPym`::
> I prefer the unhygienic option since it helps avoid confusing errors due to accidental failure to manage `else` properly. Likewise, I prefer unbound to bound as it reduces the window for accidental failure.

### #101 exactness and `eqv?`/`equal?`

In R5RS `eqv?`/`equal?` are in some sense the broadest tests for
equality, comparing structural equality, but also tests for the same
exactness, so that

> {{{(equal? 0 0.0) => #f}}}

whereas

> {{{(= 0 0.0) => #t}}}

Some users consider this confusing, others sometimes want an `equal?`
that behaves like `=` for numbers.

Do we want to change `equal?` and `eqv?` in this way, or add a
separate exactness-agnostic procedure?  Vote `yes` to change,
`equal=?` or `inexact-equal?` for separate procedures of those names
(plus the analogous `eqv=?` or `inexact-eqv?`), or `no` to leave as
is.  Alternately, write in a separate name.

* **Options:** yes, equal=?, inexact-equal?, no, wg2, undecided
* **Default:** no
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, equal=?, inexact-equal?, yes, wg2, undecided
* **Ratios:** 6:2, 6:2, 7:1, 7:1, 6:2
* **Rationales:**

`Cowan`::
> What I really want is to leave `equal?` alone and add `equal=?`, which should do what `equal?` does except for comparing numbers with `=`.
`Hsu`::
> There is no reason to eliminate the extra precision when we have a test `=` that handles the less precise test. We can easily make eqv=? if we want, so let's not muck up the standard with something like this.
`Lucier`::
> Please don't change it. Numerical equality is something different from eqv? and equal? equality. Right now we also have (define x (/ 0. 0.)) (eqv? x x) => #t (at least in many Schemes), yet (= x x) => #f. The needs of numerical = are different from the needs of equal?/eqv?/eq?, and each should be allowed to evolve independently of the other.
`Rush`::
> I would just like to note that inexact-equal? should also have an optional tolerance parameter
`Shinn`::
> These are a common source of confusion, but I don't like breaking backwards compatibility, and think yet another function may prove even more confusing.

### #102 module syntax name

A bikeshed color issue, we need to choose the
actual names for the module syntax for the winner
of #2.

`import`, `export` and `include` are fairly universal
and no alternate is suggested unless someone wants
to write in a proposal.

The enclosing syntax can be called either
`library` as in R6RS, `module` or some other proposal.

* **Options:** library, module, undecided
* **Default:** library
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **module**, library, undecided
* **Ratios:** 4:2, 5:1
* **Rationales:**

`Cowan`::
> I keep vacillating between "We should use `library` for R6RS compatibility" and "Our modules aren't really R6RS-compatible, using `library` is false advertising." Reluctantly I think familiarity wins.
`Hsu`::
> I want `module` saved for a purely syntactic entity that doesn't exist at all at runtime. Library is already the closer match from R6RS, so let's use that.
`SnellPym`::
> Pure stylistic preference. A module is a modular unit of code; a library is (to me) a module meant for sharing between programs. Modules may be used to provide structure within programs, or to make libraries.

### #103 module body syntax name

Similar to #102, we need to choose a name
for the form to include Scheme code directly
in a module form.  This can be `body` as in
the proposal, `begin` or some other name.

* **Options:** body, begin, scheme, code, undecided
* **Default:** body
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **begin**, body, code, scheme, undecided
* **Ratios:** 4:3, 5:2, 6:1, 7:0
* **Rationales:**

`Cowan`::
> I prefer `begin` because it's a begin-block anyway.
`Shinn`::
> `begin` is used in the Scheme48 syntax, but it really doesn't mean the same thing as normal `begin`, and takes up an important binding in the module description language.
`SnellPym`::
> Adding new symbols seems redundant to me.

### #105 case-insensitive module includes

The `include` module form includes files literally
with the default case-sensitivity.  An `include-ci`
form could include files case-insensitively
without resorting to the reader hacks proposed in
#92, allowing existing R5RS libraries to be used
without modification.

* **Options:** yes, no, wg2, undecided
* **Default:** no
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **yes**, undecided, wg2
* **Ratios:** 6:2, 6:0
* **Rationales:**

`Cowan`::
> The best idea yet for mixing case-folding and case-sensitive code.
`Hsu`::
> This can be implemented with a reader parameter and unhygienic macros, but absent those, this makes sense, and it is generally useful.
`Shinn`::
> This is easy to implement and is a nice way of providing backwards compatibility without any reader hacks.

### #106 conditional code selection

Users invariably want some way to conditionally select code depending
on the implementation and/or feature set available. [CondExpandCowan](CondExpandCowan.md)
allows conditional expansion in the style of SRFI-0 within the module language.
[SRFI-0](http://srfi.schemers.org/srfi-0/srfi-0.html) provides
`cond-expand`, [SRFI-103](http://srfi.schemers.org/srfi-103/srfi-103.html)
provides a library naming extension, and numerous other personal hacks exist.

Do we want to include something along these lines in WG1 Scheme?

* **Proposals:**
* **cowan:** [CondExpandCowan](CondExpandCowan.md)
* **srfi-0:** `cond-expand` only defined as a top-level module form
* **srfi-103:** the search path extension used by R6RS implementations
* **Options:** cowan, srfi-0, srfi-103, none, wg2, undecided
* **Default:** none
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **cowan**, srfi-0, wg2, no, undecided, srfi-103
* **Ratios:** 4:2, 5:2, 6:1, 4:2, 6:0
* **Rationales:**

`Hsu`::
> None of these are good enough to put in a Small Scheme. Let WG2 have it.
`Medernach`::
> Definitely needed, however not so sure if it is the right choice.
`Shinn`::
> Something like this is very much needed. The search path extension is a hack, so I choose `cond-expand`, but I don't think a standard set of features belongs in WG1, even if they are optional.

### #108 immutable data interface

R5RS specifies literal data in source code as immutable, but otherwise
provides no way to generate or introspect immutable data.

One proposal is given in [ImmutableData](ImmutableData.md), providing `mutable?`,
`make-immutable` and `immutable->mutable`.

Racket, for which all pairs are immutable in the default language,
needs some way to generate shared and cyclic data structures at
runtime, and provides the `shared` syntax for this.  It also has an
`immutable?` utility as the complement to `mutable?` above.

* **Proposals:**
* **medernach:** [ImmutableData](ImmutableData.md)
* **racket:** `shared`, `immutable?` ([http://docs.racket-lang.org/reference/shared.html])
* **Options:** medernach, racket, no, undecided
* **Default:** no
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** medernach, *no*, undecided, racket, shared
* **Ratios:** 3:3, 2:2, 2:1, 2:0
* **Rationales:**

`Cowan`::
> Too much too soon.
`Gleckler`::
> I agree with John. It's just too early for standardization of ideas like this.
`Hsu`::
> I think this needs more discussion and proofs.
`Medernach`::
> As stated earlier I personnaly think these are orthogonal features and I like both. Now there was not enough discussions about it to be included.
`Shinn`::
> This is a complex topic which hasn't generated much discussion, and is better left to WG2. The [ImmutableData](ImmutableData.md) proposal in particular is underspecified, and needs some discussion of whether it's a shallow or deep copy, how it handles closed variables, inherently mutable data-structures like ports, etc.

### #111 require `equal?` to return `#t` if `eqv?` does

Currently `equal?` is strictly broader than `eqv?` except in the
pathological case of comparing the same circular list with itself, for
which `eqv?` returns true and `equal?` may loop infinitely.  We could
explicitly require `equal?` to check and return `#t` in this case,
which most implementations do as a performance hack anyway.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:** 9: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **yes**, no, undecided
* **Ratios:** 5:3, 5:2
* **Rationales:**

`Cowan`::
> This was my idea.
`Ganz`::
> Only if equal? passes on #54.
`Shinn`::
> This is an ugly special case to patch up something that should be handled by #51. If you're passing potentially circular structures to `equal?` at all in the absense of proper circularity handling you have a bug in your program.

## WG1 - Exceptions

### #18 exception system

R6RS provided a detailed exception system with support for raising and
catching exceptions, using a hierarchy of exception types.

Do we use this, or parts of it, or a new exception system?  The `r6rs`
option is just for the core exception handling.

* **Proposals:**
* **r6rs:** [R6RS Exceptions](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-8.html#node_sec_7.1) - `with-exception-handler`, `guard`, `raise`, `raise-continuable`
* **cowan:** [ExceptionHandlingCowan](ExceptionHandlingCowan.md)
* **Options:** cowan, r6rs, wg2, none, undecided
* **Default:** none
* **Voters:** 11: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Radul](WG1BallotRadul.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** *r6rs*, wg2, no, cowan, undecided, r6rs/module, r6rs/core, cowan/module, cowan/core
* **Ratios:** 5:4, 5:4, 6:2, 4:3, 6:1, 7:0, 7:0, 7:0
* **Rationales:**

`Cowan`::
> I now think that r6rs is better than cowan, now that I understand R6RS better, so I'm voting against my own proposal here.
`Gleckler`::
> While the R6RS exception system is not perfect, I'm happy with it. In WG1, it belongs in a module, not in the core. If we don't agree to use the R6RS system, then I'd rather see WG2 refine it instead of including [ExceptionHandlingCowan](ExceptionHandlingCowan.md) in WG1, since the [ExceptionHandlingCowan](ExceptionHandlingCowan.md) proposal doesn't explain the rationale for its deviations from R6RS. I've studied the mailing list archive and can't find a convincing argument for [ExceptionHandlingCowan](ExceptionHandlingCowan.md), either, so I'm sticking with R6RS or, as a fallback position, WG2. The largest flaw I see with the R6RS condition system is that its condition taxonomy is too coarse and focused on operating-system issues. Compare it with the taxonomy of Gambit or MIT Scheme, for example. (See [ExceptionTaxonomies](ExceptionTaxonomies.md) for details of the condition taxonomies of many Scheme implementations.) However, this ballot item is only for the core exception handling system, not the taxonomy. I wish we would still standardize on some taxonomy rather than none. Without a common exception taxonomy, it's hard to share code.
`Hsu`::
> Better to go with R6RS than yet another system. Either that, or let WG2 have it.
`Medernach`::
> I feel exceptions as inappropriate for WG1: we already have the flexibility of call-with-current-continuation. Moreover existing exceptions taxonomy are difficult to unify adequatly without making some complex, ad hoc, and unfortunate kludge.
`Radul`::
> The only reason I can imagine for wanting exceptions in the core is to specify which conditions various provided procedures (including ERROR) will raise. Leave this to WG2: let them amend the specifications of any WG1 procedures with their behavior in exceptional circumstances.
`Shinn`::
> Exception systems have subtle semantics and we should not specify anything that hasn't even been implemented.

### #19 when to throw an error

R5RS defines many things as "is an error" without any specification of
what happens in that situation.  R6RS goes to the opposite extreme and
specifies as much as possible what exceptions are raised when.

Taking into account the system provided by ticket #18, we need to come
up with guidelines for when exceptions should be raised, and clarify
which R5RS "error" situations should raise an exception or be left
unspecified.

R5RS specifies only 3 situations where an error is required to be
signalled, leaving most situations unspecified as described in
[ErrorSituations](ErrorSituations.md).

* **Options:** r5rs, r6rs, undecided
* **Default:** r5rs
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** *undecided*, **r5rs**, r6rs
* **Ratios:** 4:2, 5:2
* **Rationales:**

`Medernach`::
> Discussions needed.
`Shinn`::
> I think there's probably a good line between r5rs and r6rs here, but no one has drawn it yet, and it's reasonable to stick with the r5rs default.
`SnellPym`::
> I think it's important for portable code to be able to know how to handle various kinds of errors. Most of the errors in [ErrorSituations](ErrorSituations.md) are likely to be programming errors, but even they need to be catchable in systems that host "foreign" code - from sandboxes to "application servers".

## WG1 - I/O

### #28 binary I/O ports

Do we provide any binary input or output ports, and if so how do we
construct them and operate on them?  Can binary and textual operations
be mixed on the different port types?

[PortsCowan](PortsCowan.md) provides binary port operations along with other
extensions.

R6RS provides an entirely new I/O system, as well as a separate
R5RS-compatible I/O system.

The withdrawn SRFI-91 provides yet another I/O system supporting
binary ports.

Note this item as well as #29 and #31 specify semi-orthogonal aspects
of I/O systems which are typically specified together by individual
proposals.  If the same proposal doesn't win for all three, the
aspects will be merged as needed.

* **Proposals:**
* **r6rs:** [R6RS Port I/O](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2)
* **r6rs-simple:** [R6RS Simple I/O](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.3)
* **srfi-91:** [SRFI-91](http://srfi.schemers.org/srfi-91/srfi-91.html)
* **cowan:** [PortsCowan](PortsCowan.md) (subset relevant to binary I/O)
* **Options:** r6rs, r6rs-simple, srfi-91, cowan, none, undecided
* **Default:** none
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** *cowan*, srfi-91, r6rs-simple, undecided, r6rs, no
* **Ratios:** 4:1, 4:1, 4:4, 4:0, 4:1
* **Rationales:**

`Hsu`::
> I have not had enough time to read over the history of these things, and I need more time.
`Shinn`::
> I think it's a mistake to _require_ implementations allow mixing of binary and character data, even if some implementations already do so.

### #29 port encodings

Do we support encoding and decoding text from ports with different
character encoding systems?  Different end-of-line conventions?
Different normalizations?  How are encoding errors handled?

* **Proposals:**
* **r6rs:** [R6RS Port I/O](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2)
* **srfi-91:** [SRFI-91](http://srfi.schemers.org/srfi-91/srfi-91.html)
* **cowan:** [PortsCowan](PortsCowan.md) (subset relevant to port encodings)
* **Options:** r6rs, srfi-91, cowan, none, undecided
* **Default:** none
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** srfi-91, undecided, cowan, *no*, r6rs, r6rs-simple
* **Ratios:** 4:4, 1:3, 3:3, 3:1, 4:0
* **Rationales:**

`Cowan`::
> I've modified cowan here to allow rather than require these facilities.
`Hsu`::
> I need more time for this one.

### #31 custom ports

Do we provide a mechanism for custom ports, on which for instance
string ports could be created?

R6RS as well as a number of Scheme implementations provide custom
ports with various APIs.

* **Proposals:**
* **r6rs:** [R6RS Port I/O](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2)
* **Options:** r6rs, none
* **Default:** none
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, undecided, r6rs
* **Ratios:** 4:2, 5:1
* **Rationales:**

`Cowan`::
> Too messy. A custom port is basically a record of procedures, but there's no principled way to decide what the fields should be.
`Gleckler`::
> Unless we do a comprehensive survey of how existing implementations handle this idea, we shouldn't try to standardize it.
`Hsu`::
> This needs more discussion. I recall hearing some complaints about the R6RS system, but I do think that custom ports are universal enough to warrant consideration.
`Medernach`::
> Discussions and review needed
`Rush`::
> why is there not a wg2 option?

## WG1 - Libraries

### #36 hash-tables

R6RS and SRFI-69 both provide hash-table interfaces.  Do we provide
either of these, or try to provide some primitives on which efficient
hash-tables can be implemented?

* **Options:** r6rs, srfi-69, no, wg2, undecided
* **Default:** no
* **Voters:** 15: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Harvey](WG1BallotHarvey.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Radul](WG1BallotRadul.md), [Read](WG1BallotRead.md), [Rush](WG1BallotRush.md), [Russel](WG1BallotRussel.md), [Shinn](WG1BallotShinn.md), [Shivers](WG1BallotShivers.md), [SnellPym](WG1BallotSnellPym.md), [Sussman](WG1BallotSussman.md)
* **Results:** *wg2*, r6rs, srfi-69, undecided, **no**, module
* **Ratios:** 6:4, 6:4, 6:4, 8:1, 8:2
* **Rationales:**

`Cowan`::
> Reluctantly I think we need to push this off on WG2. This sucks, because I think all languages should have a map datatype.
`Radul`::
> If we're going to specify hash tables, we must allow room for holding the keys and/or values weakly. There must be system support for this from the garbage collector --- it cannot be written in user code.
`Rush`::
> I actually want to say "yes" to primitives, but there doesn't seem to be an option
`Shinn`::
> Exposing the `eq?-hash` function in SRFI-69 is a mistake.
`Sussman`::
> Must include weak structures and ephemerons, because these structures cannot be built with user code.

### #113 directory contents

We've decided to add file-exists? and delete-file,
essential for a large class of scripts, but still
have no way to get a list of files in a directory.
Do we want to provide an interface to this?

* **Proposals:**
*  **cowan:** [DirectoryPortsCowan](DirectoryPortsCowan.md)
*  **directory-files:** return a list of all files in the dir
*  **directory-streams:** [SCSH directory stream interface](http://www.scsh.net/docu/html/man-Z-H-4.html#node_sec_3.3)
* **Options:** directory-files, directory-streams, no, wg2, undecided
* **Default:** no
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** *wg2*, directory-streams, cowan, directory-files, **no**, undecided
* **Ratios:** 4:2, 4:2, 3:3, 5:0, 3:3
* **Rationales:**

`Cowan`::
> I agree with Gleckler that cowan is too complicated, but directory-files is too simple in a world of very lengthy directories, so I go with directory-streams (just `open-d-s`, `read-d-s`, and `close-d-s`).
`Gleckler`::
> The directory-files proposal is the only one that is simple but useful. The cowan proposal is too complicated for such a simple purpose, and conflates ports with directory reading unnecessarily. The directory-streams proposal uses the term "stream" for something other than what is conventionally meant in Scheme usage, and it's overcomplicated, too.

## WG1 - Macros

### #48 let-syntax

`let-syntax` and `letrec-syntax` has known ambiguities in their
behavior. We have the option of altering the semantics to correct this
behavior, defining which behavior we intend, or removing `let-syntax`
entirely.  We could also leave this ambiguity unspecified.

The question of whether or not to introduce a new lexical scope
(i.e. whether internal `define`s are visible outside the `let-syntax`)
is straightforward.

If we don't introduce a new lexical scope, the question arises whether
or not internal `define-syntax` forms are allowed and whether they
apply to the body of the `let-syntax`, forms following the
`let-syntax`, or both.

If internal `define-syntax` applies to the body, we may also wish to
specify what happens when said `define-syntax` redefines an identifier
bound by the enclosing `let-syntax`.  This varies by implementation
and may be difficult for macro expanders to change, so is left
unspecified in the proposals below.

* **Proposals:**
* **hsu:** [LetSyntaxArcfide](LetSyntaxArcfide.md)
* **remove:** remove both of these forms from the standard
* **lexical:** introduces a new lexical contour
* **define:** allows splicing `define`/`begin`
* **syntax:** allows `define-syntax`
* **syntax-body:** allows `define-syntax` only applying to the body
* **syntax-follows:**  allows `define-syntax` only applying to following forms
* **Options:** hsu, remove, lexical, define, syntax, syntax-body, syntax-follows, unspecified, undecided
* **Default:** unspecified
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **lexical**, remove, undecided, hsu, syntax-body, syntax, define, syntax-follows
* **Ratios:** 5:1, 5:1, 4:1, 5:0, 5:1, 5:1, 5:0
* **Rationales:**

`Cowan`::
> R5RS was lexical under the plain meaning of the text, and I think changing that in R6RS was a mistake.
`Hsu`::
> I am voting for [LetSyntaxArcfide](LetSyntaxArcfide.md) with the understanding that it is proposing a new approach to `let-syntax` functionality rather than being a fully finished product. I believe that `let-syntax` is not very useful if it does not splice, and not very correct if it does, so I don`t like having it around.
`Rush`::
> I suspect that let-syntax becomes much less necessary in the presence of a module system.

### #97 syntax-rules special literals

`...` and with the result of #6 also `_` have special meaning in
syntax-rules patterns, so they are not treated as pattern variables by
default.

However their behavior when used in the literals list of
syntax-rules is ambiguous, and simply breaks in most implementations.

Rather than breaking, it makes sense to go ahead and treat
them as normal literals, overriding their special meanings.

In particular, there are many existing R5RS macros which
make use of `_` in the literals and are thus broken outright
by #6. Allowing them as literals fixes these macros.

* **Options:** literal, error, unspecified, undecided
* **Default:** unspecified
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Ganz](WG1BallotGanz.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **literal**, error, unspecified, undecided
* **Ratios:** 8:0, 8:0, 8:0
* **Rationales:**

`Cowan`::
> Makes sense to me.
`Hsu`::
> Getting these with special meanings makes sense, but if an user puts them in the literals list, then we should not surprise them by breaking. However, if we do break, then an error should be the result, and not some silent failure.
`Shinn`::
> This fixes the R5RS macros that the new `_` pattern breaks.

## WG1 - Modules

### #3 module naming convention

We need a naming convention for the core modules and standard
libraries of the new module system.

In R5RS everything is effectively in a single module.  R6RS provides a
much more fine-grained breakdown of modules which could be
retro-fitted to the bindings we include in our standard.

John Cowan has proposed a number of module factorings in items #71,
#72, #73, #74, #75, #76, #77, as well as an I/O module breakdown in
[PortsCowan](PortsCowan.md).

Since the naming and breakdown must be internally consistent I'm
grouping these into a single ballot item.  Members desiring to put
forth a new proposal should specify where all bindings belong, or
specify a subset of the bindings and default the rest to some other
proposal.

Note some ballots specify explicitly whether or not the bindings in
question are intended to be in a module or the core language.  In
these cases we still need to decide to which module they belong.
Where specific votes contradict general factoring proposals, the
specific vote wins.

* **Proposals:**
* **r5rs:** one single module
* **r6rs:**
* **cowan:** #71, #72, #73, #74, #75, #76, #77
* **Options:** r5rs, r6rs, cowan, undecided
* **Default:** r5rs
* **Voters:** 7: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **cowan**, r5rs, undecided, r6rs
* **Ratios:** 4:1, 3:4, 4:1
* **Rationales:**

`SnellPym`::
> I think we need to actually gather a list of what's going in modules, and then make a decision then.

## WG1 - Numerics

### #79 rational-expt

Often a rational-only exponentiation function is useful; that is, a
rational number raised to an integer power.  Should we add this
procedure to the core so that exponentiation is available even if
inexact rationals are not provided or not imported?

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **no**, yes, undecided, module, wg2
* **Ratios:** 5:2, 6:1, 5:1, 5:1
* **Rationales:**

`Cowan`::
> My idea.
`Lucier`::
> I don't see what inexact rationals has to do with expt. Current expt can do the proposed operation without difficulty.
`Medernach`::
> I don't get the point, isn't it already the job of expt ?
`Shinn`::
> I completely fail to see the point of this - is it motivated by some sort of efficiency concern, or perceived module factoring? Either rationale is misguided - `expt` is fine for rational as well as all other numbers.

### #81 What numeric tower variants should be supported?

[NumericTower](NumericTower.md) lists a plausible set of ten from fixnums only to the
full monty.  Which ones should we allow an implementation to provide?
R5RS requires only fixnums large enough to handle string and vector
indexes, while R6RS requires the full numeric tower.

Vote on **the minimum level of support** you want to **require**
(implementations may of course still provide more than this).  I've
included only the most likely options below, write in other options if
needed.

Note quaternions are a fairly rare numeric type, known to be provided
only by extensions to [scm](http://www.ccs.neu.edu/home/dorai/squat/squat.html)
and [chicken](http://wiki.call-cc.org/eggref/4/quaternions), and thus
may be difficult for other implementations to support if required.

* **Proposals:**
* **r5rs:** fixnum (`inexact?` may always be false)
* **inexact-only:** inexact (`exact?` may be the same as `integer?`)
* **inexact:** fixnum, inexact
* **rational:** fixnum, inexact, rational
* **complex:** fixnum, inexact, complex
* **r6rs:** fixnum, inexact, rational, complex
* **quaternion:** fixnum, inexact, rational, complex, quaternion
* **Options:** r5rs, inexact-only, inexact, rational, complex, r6rs, quaternion, undecided
* **Default:** r5rs
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **r5rs**, inexact-only, rational, r6rs, undecided, complex, quaternion
* **Ratios:** 6:1, 6:1, 6:1, 7:1, 7:0, 7:0
* **Rationales:**

`Cowan`::
> None of these satisfy me at all. I wish this had been given as "Which [NumericTower](NumericTower.md) feature switches should be forced to true, forced to false, or undetermined by the standard?"
`Medernach`::
> Why do we have to require anything beyond fixnums ? Some embedded systems lacks floating point arithmetics. That issue aside, why not standardize these features as modules ?
`Rush`::
> I like the idea of having quaternions around, they are essential for a lot of 3d geometry. However they are definitely a wg2 feature!
`Shinn`::
> We have a rich numeric tower, but there's no need to require the whole thing and rule out simple implementations.

### #100 integral division

R5RS provides quotient, modulo and remainder for integral
division. R6RS extended this with div/mod and div0/mod0. A thorough
analysis of possible division operations is provided in
[DivisionRiastradh](DivisionRiastradh.md), which includes a proposal for five separate
division operator pairs.  We need to choose which API we'll provide.

* **Proposals:**
* **riastradh:** [DivisionRiastradh](DivisionRiastradh.md)
* **Options:** r5rs, r6rs, riastradh, undecided
* **Default:** r5rs
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **riastradh**, r5rs, r6rs, undecided, r5rs+div/mod
* **Ratios:** 5:3, 5:0, 4:3, 5:1
* **Rationales:**

`Cowan`::
> Riastradh convinces me.
`Lucier`::
> I am not a fan of div0/mod0 in r6rs, and I don't recommend it for wg1. I would prefer to keep quotient/remainder from r5rs, and add div/mod from r6rs (where modulus is a synonym for mod). I see these operations as number-theoretic operations, so I would prefer that they apply only to integers.
`Rush`::
> what i would really like id [DivisionRiastradh](DivisionRiastradh.md) with the use of multiple values replaced by using pairs. In spite of the complexity, this proposal has the kind of small-scale getting-it-right quality that is very Schemely
`Shinn`::
> The R6RS operations are clearly bad, but I'm unconvinced we need everything provided by [DivisionRiastradh](DivisionRiastradh.md).
`SnellPym`::
> I am drawn, moth-like, to the awesome rigor of Riastradh's proposal, even though it makes my "not jewel-like" glands itch at the same time.

## WG1 - Reader Syntax

### #12 symbol literal extensions

In R5RS, symbols parsed as any sequence of valid symbol characters
that does not begin with a character that can begin a number.  The
three exceptions `+`, `-` and `...` are also provided.  This allows
parsing with only constant lookahead to determine type.

R6RS added additional exceptions for symbols beginning with `->`, a
common idiom, still allowing parsers to determine type with a constant
lookahead.

John Cowan proposes allowing anything that cannot be parsed as a
number to be a valid symbol.  This removes the special case
exceptions, but may require arbitrary lookahead.

Alex Shinn proposes symbols are any sequence of valid symbol
characters that does not have a prefix which is a valid number.  This
removes the special case exceptions, allows constant lookahead, and
allows extensions to number syntax.

* **Proposals:**
* **r5rs:** symbols may not begin with `-`, except for `-` itself
* **r6rs:** symbols may not begin with `-[^>]`
* **cowan:** symbols are anything that doesn't parse as a number
* **shinn:** symbols may not begin with a number prefix
* **Options:** r5rs, r6rs, cowan, shinn, undecided
* **Default:** r5rs
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **shinn**, r6rs, r5rs, cowan
* **Ratios:** 8:0, 7:1, 8:0
* **Rationales:**

`Cowan`::
> I've been convinced by Alex's rationale here: we need a little kick room for extending number syntax in the standard later on.
`Shinn`::
> It's important to leave room for numeric extensions, such as quaternions and units which are already provided by some Schemes. The `cowan` proposal makes this impossible. Ease of parsing, both by computers (not requiring arbitrary lookahead) and by humans (being able to tell if something is a number or symbol at a quick glance) is also a concern which should not be dismissed lightly. Both of these issues are addressed by the `shinn` proposal, which has just as simple a description and removes the special cases of R5RS and R6RS.

### #84 Need to decide on a list of named character escapes

The WG has voted to have a list of character names.

The list in R5RS and the longer list in R6RS are only informative.  I
suggest adopting the R6RS list and making it normative.

* **Proposals:**
* **r5rs:** space, newline
* **r6rs:** [R6RS Characters](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html#node_sec_4.2.6)
* **shinn:** space, tab, newline, return, escape, null, alarm, backspace
* **Options:** r5rs, r6rs, shinn
* **Default:** r5rs
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **shinn**, r6rs, r5rs
* **Ratios:** 4:4, 8:0
* **Rationales:**

`Cowan`::
> My idea.
`Shinn`::
> The list is somewhat arbitrary, and I'd be open to other suggestions. Mostly I consider it important to make characters used for terminal manipulation avaiable, and to kill the vertical tab. The only character I debated on and ultimately left out was formfeed.

### #104 list of mnemonic string escapes

Similar to #84, we need to choose a specific list of mnemonic escapes
like \n and \t to be recognized in strings.

* **Proposals:**
* **r5rs:** `\\`, `\"`
* **r6rs:** [R6RS Strings](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html#node_sec_4.2.7)
* **shinn:** `\\`, `\"`, `\t`, `\n`, `\r`, `\e`, `\a`, `\b`
* **Options:** r5rs, r6rs, shinn
* **Default:** r5rs
* **Voters:** 8: [Cowan](WG1BallotCowan.md), [Gleckler](WG1BallotGleckler.md), [Hsu](WG1BallotHsu.md), [Lucier](WG1BallotLucier.md), [Medernach](WG1BallotMedernach.md), [Rush](WG1BallotRush.md), [Shinn](WG1BallotShinn.md), [SnellPym](WG1BallotSnellPym.md)
* **Results:** **shinn**, r6rs, r5rs
* **Ratios:** 4:4, 8:0
* **Rationales:**

`Cowan`::
> My idea.
`Shinn`::
> These should be consitent with #84.
