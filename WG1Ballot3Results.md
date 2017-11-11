# WG1 Ballot 3 Results from July 1st

# Notes about Results

See [WG1BallotExplanation](WG1BallotExplanation.md).

# Previous Undecided and Re-opened Ballot Items

### #32 user-defined types

Do we support any means of creating disjoint user-defined types, such
as in SRFI-9, SRFI-99 or the R6RS record system?

WG1 voted **srfi-9** before.  New arguments against filter
constructors were raised, so the ticket was re-opened.

* **References:**
* https://groups.google.com/d/topic/scheme-reports-wg1/BX2F10MO6_k/discussion
* **Proposals:**
* **cowan:** [RecordsCowan](RecordsCowan.md)
* **gleckler:** [RecordsGleckler](RecordsGleckler.md), which is just [RecordsCowan](RecordsCowan.md) plus [RecordsArcfide](RecordsArcfide.md)
* **hsu:** [RecordsArcfide](RecordsArcfide.md)
* **medernach:** [AggregatesMedernach](AggregatesMedernach.md)
* **rush:** [UserAggregatesRush](UserAggregatesRush.md)
* **snellpym:** [UniqueTypesSnellPym](UniqueTypesSnellPym.md)
* **Options:** srfi-9, srfi-57, srfi-99, r6rs, cowan, hsu, medernach, rush, snellpym, none, wg2, undecided
* **Default:** srfi-9
* **Voters:**
* [Cowan](WG1BallotCowan.md): srfi-9
* [Gleckler](WG1BallotGleckler.md): gleckler, cowan, snellpym+inheritance+mutate, (medernach   snellpym+mutate), hsu, srfi-9, wg2, srfi-99, r6rs, snellpym
* [Hsu](WG1BallotHsu.md): hsu, gleckler, wg2, no, srfi-9
* [Lucier](WG1BallotLucier.md): srfi-9
* [Medernach](WG1BallotMedernach.md): medernach, snellpym, hsu, rush, srfi-9, cowan, wg2, srfi99, r6rs, snellpym+inheritance, snellpym+mutate, snellpym+inheritance+mutate, srfi-57, no, undecided
* [Read](WG1BallotRead.md): srfi-9
* [Shinn](WG1BallotShinn.md): srfi-9, hsu, r6rs, srfi-99, srfi-57, rush, snellpym, cowan, medernach, wg2
* [SnellPym](WG1BallotSnellPym.md): gleckler, (snellpym cowan), hsu, medernach, rush
* **Results:** hsu, *srfi-9*, gleckler, snellpym, cowan, medernach, rush, wg2, r6rs, snellpym+mutate, snellpym+inheritance+mutate, srfi-99, srfi99, snellpym+inheritance, srfi-57, no, undecided
* **Ratios:** 4:4, 3:2, 3:2, 3:2, 3:2, 5:0, 5:0, 5:0, 4:1, 4:1, 5:0, 5:0, 5:0, 5:0, 5:0, 5:0
* **Rationales:**

`Cowan`::
> The most important thing about SRFI 9 is how pervasive it is (26 out of 30 implementations I track at http://tinyurl.com/scheme-s5 support it). Yes, it's messy. Yes, we should keep it.
`Gleckler`::
> I had initially voted for SRFI 99 as my top choice, but I'm now convinced that that's just too complicated a system for core Scheme. We need something more fundamental upon which other systems can be built. However, I don't want to give up convenient syntax, so I've added a new choice, [RecordsGleckler](RecordsGleckler.md), which is simple combination of [RecordsCowan](RecordsCowan.md) and [RecordsArcfide](RecordsArcfide.md). That way, we get both. SRFI 9 is widely used and is about the simplest syntactic implementation one could hope for. It doesn't support inheritance. As Aaron Hsu has pointed out, it has problems with filtering constructors. [RecordsArcfide](RecordsArcfide.md) is also simple and, while an earlier version supported inheritance, this one doesn't. It is a syntactic system. It uses the name `define-disjoint-type'. It is car[...](WG1BallotGleckler.md)
`Hsu`::
> SRFI-9 is flawed by the filtering constructor and a lack of extensibility in its syntax, but represents a good minimal set of features that are found everywhere.
`Medernach`::
> Please notice that there was a Condorcet's paradox on this item in the last ballot: as such I think that we need to be open-minded and have space for alternatives record systems, instead having "one record system to bind them all". I would put widespread srfi-9 into a module named (scheme record srfi-9) and let doors open for others (scheme record ...).
`Read`::
> I'm a big fan of unique types since with unique types and vectors you can build records. However, SRFI-9 is so widespread and so relied upon by the R5RS community that it's worth it not to turn our backs on those users. Standards should be about reflecting the best of common practice, not handing down new untested practice.
`Shinn`::
> This is what we voted on before, it's extremely widely supported, and I remain unconvinced of the alternatives. Filtering constructors can be a useful convenience (and optimization) in some circumstances.

### #28 Binary I/O ports

Do we provide any binary input or output ports, and if so how do we
construct them and operate on them?  Can binary and textual operations
be mixed on the different port types?

[BinaryPortsCowan](BinaryPortsCowan.md) provided binary port operations, being a mild
revision of the relevant parts of [PortsCowan](PortsCowan.md).  It has been removed
by Cowan in favor of [PortsShinn](PortsShinn.md).

[PortsShinn](PortsShinn.md) provides binary port operations, with similar operations to
[BinaryPortsCowan](BinaryPortsCowan.md) but keeping the binary/textual ports disjoint.

R6RS provides an entirely new I/O system, as well as a separate
R5RS-compatible I/O system.

The withdrawn SRFI-91 provides yet another I/O system supporting
binary ports.

Note this item as well as #29 and #31 specify semi-orthogonal aspects
of I/O systems which are typically specified together by individual
proposals.  If the same proposal doesn't win for all three, the
aspects will be merged as needed.

WG1 voted weakly in favor of [PortsCowan](PortsCowan.md) before.

* **Proposals:**
* **r6rs:** [R6RS Port I/O](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.2)
* **r6rs-simple:** [R6RS Simple I/O](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html#node_sec_8.3)
* **srfi-91:** [SRFI-91](http://srfi.schemers.org/srfi-91/srfi-91.html)
* **shinn:** [PortsShinn](PortsShinn.md)
* **Options:** r6rs, r6rs-simple, srfi-91, cowan, shinn, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): shinn
* [Gleckler](WG1BallotGleckler.md): shinn, r6rs-simple, undecided
* [Hsu](WG1BallotHsu.md): undecided, shinn, cowan, r6rs-simple
* [Lucier](WG1BallotLucier.md): srfi-91, r6rs-simple, undecided
* [Medernach](WG1BallotMedernach.md): undecided
* [Read](WG1BallotRead.md): srfi-91
* [Shinn](WG1BallotShinn.md): shinn, no, undecided
* [SnellPym](WG1BallotSnellPym.md): shinn
* **Results:** **shinn**, undecided, srfi-91, r6rs-simple, no, cowan
* **Ratios:** 4:3, 5:2, 5:1, 5:0, 5:0
* **Rationales:**

`Hsu`::
> I think we still need more time on this. R6RS' biggest trouble is its I/O system, and I don't want the same mistake being made again. We need more effort into this domain first by more people.
`Read`::
> The Gambit port system is a working, tested implementation generally regarded as a win.

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

WG1 voted **unbound** previously.  New issues were brought up on the
list so the ticket was re-opened.

* **References:**
* [wiki:Keywords]
* http://lists.scheme-reports.org/pipermail/scheme-reports/2011-April/thread.html
* **Options:** bound, unbound, unhygienic, undecided
* **Default:** unbound
* **Voters:**
* [Cowan](WG1BallotCowan.md): bound
* [Gleckler](WG1BallotGleckler.md): undecided
* [Hsu](WG1BallotHsu.md): bound
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): undecided
* [Read](WG1BallotRead.md): bound
* [Shinn](WG1BallotShinn.md): bound, unbound
* [SnellPym](WG1BallotSnellPym.md): bound, unbound, unhygienic
* **Results:** **bound**, undecided, unbound, unhygienic
* **Ratios:** 5:3, 5:0, 5:0
* **Rationales:**

`Cowan`::
> The arguments for binding this convince me.
`Gleckler`::
> I just don't understand the issues here well, so I'll leave the debate to others for now.
`Hsu`::
> While there are advantages to being unbound, unbound is still too limited and the advantages do not outweight the disadvantages. Bound has a few disadvantages, but we gain much more flexibility and the advantages just outweigh the disadvantages. Unbound limits what you can do, while Bound presents only some minor inconvenience that sometimes occurs, but that is easily worked around in ways that aren't hacky.
`Shinn`::
> I'm reversing my former preference on this. Keywords in Scheme are broken, but we can't break hygiene and we need to decide whether they are bound or not in this standard, pending future improvements. Leaving keywords unbound allows multiple unrelated macros to use the same keywords, but Chez breaks in this case. Fortunately, for the keywords used in the standard, all macros can refer to the same bindings, so I recommend we bind them and perhaps recommend third-party modules to _not_ bind their keywords.

### #3 module naming convention

We need a naming convention for the core modules and standard
libraries of the new module system.

The existing break down is based on John Cowan's earlier proposal of
factorings in items #71, #72, #73, #74, #75, #76, #77, as well as an
I/O module breakdown in [PortsCowan](PortsCowan.md).  There have been various tickets
proposing changing this, so we are re-opening the ticket.

* **Proposals:**
* **draft-1:** the first draft
* **r5rs:** one single module
* **r6rs:** no proposal yet
* **cowan:** [ModuleFactoringCowan](ModuleFactoringCowan.md)
* **gleckler:** [ModuleFactoringGleckler](ModuleFactoringGleckler.md)
* **shinn:** [ModuleFactoringShinn](ModuleFactoringShinn.md)
* **medernach:** [ModuleFactoringMedernach](ModuleFactoringMedernach.md)
* **Options:** draft-1, r5rs, r6rs, cowan, shinn, medernach, undecided
* **Default:** draft-1
* **Voters:**
* [Cowan](WG1BallotCowan.md): cowan
* [Gleckler](WG1BallotGleckler.md): gleckler, cowan, shinn, r6rs, r5rs
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): gleckler
* [Medernach](WG1BallotMedernach.md): medernach, gleckler, shinn, cowan, undecided
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): medernach, shinn, gleckler, cowan, draft-1
* [SnellPym](WG1BallotSnellPym.md): cowan, shinn, medernach, r5rs
* **Results:** *medernach*, gleckler, cowan, shinn, undecided, r5rs, draft-1, r6rs
* **Ratios:** 3:2, 2:3, 2:2, 3:2, 3:1, 3:0, 3:1
* **Rationales:**

`Cowan`::
**Note:** The `(scheme io)` module in the draft was never voted on and doesn't belong there.
`Gleckler`::
> My own proposal, [ModuleFactoringGleckler](ModuleFactoringGleckler.md), is a simple combination of [ModuleFactoringCowan](ModuleFactoringCowan.md) and [ModuleFactoringShinn](ModuleFactoringShinn.md).
`Hsu`::
> I have not had enough time to look this over.
`Shinn`::
> I prefer Medernach's proposal to my own here.

# New Ballot Items

## WG1 - Core

### #85 Blobs, bytevectors, byte-vectors, octet-vectors, or something else?

Now that we have blobs, we have to decide what to call them.  R6RS
uses bytevector, SRFI-4 and SRFI-68 uses u8vector, while the original
WG1 proposal used blob (which is therefore the default).

* **Options:** blob, bytevector, byte-vector, u8vector, octet-vector, undecided
* **Default:** blob
* **Voters:**
* [Cowan](WG1BallotCowan.md): blob
* [Gleckler](WG1BallotGleckler.md): byte-vector, octet-vector, blob, bytevector, u8vector
* [Hsu](WG1BallotHsu.md): bytevector, u8vector, octet-vector, byte-vector, blob
* [Lucier](WG1BallotLucier.md): bytevector, u8vector
* [Medernach](WG1BallotMedernach.md): bytevector, byte-vector, blob, octet-vector, u8vector, undecided
* [Read](WG1BallotRead.md): bytevector, blob
* [Shinn](WG1BallotShinn.md): bytevector, blob, u8vector
* [SnellPym](WG1BallotSnellPym.md): blob, u8vector, (bytevector byte-vector octet-vector)
* **Results:** **bytevector**, blob, u8vector, byte-vector, octet-vector, undecided
* **Ratios:** 5:3, 6:1, 5:1, 5:1, 7:0
* **Rationales:**

`Cowan`::
> I call them blobs because I see them as objects whose size is measured in bytes, not specialized vectors of 8-bit unsigned integers. It's easy to layer such an interpretation, along with many other interpretations, on top of blobs, but blobs should themselves remain simple for WG1.
`Gleckler`::
> While "blob" is a widely used term these days, I prefer a properly hyphenated, descriptive term.
`Hsu`::
> Blob doesn't give enough information to the reader and feels strange, despite its terseness. Bytevector very accurately describes the data structure we are dealing with. No matter how you decide to treat those bytes, we are still dealing with a vector of bytes, not a vector of 3 bits or something else. I disagree that there is confusion on how to use a bytevector just because the word "byte" appears in the name. On the other hand, blob gives no hints on how to use the structure at all, or even what sort of things might be available.
`Read`::
> I am partial to "bytevector" myself. Failing that the Erlang term for these things is "a binary". I can understand where that might cause confusion, so "blob" is probably as good an alternative as any.
`Shinn`::
> This is pure bikeshedding, and the charter makes it clear we should prefer the R6RS option when there is no good reason otherwise.
`SnellPym`::
> I think it's important to keep blobs distinct from typed arrays. A blob is a region of memory; u8vectors are a particular interpration thereof. Blobs may be measured in bytes, but that's just due to the granularity of access - they aren't necessarily composed of independent u8s (they might be composed of u16s, or floats, or a complex structure). However, if we have to conflate them, I'd rather go with SRFI-4 names.

### #118 Simple literals must be explicitly delimited.

In R5RS syntax such as `#t#f` is left unspecified - some readers may
parse this as the true literal followed by false.  R6RS requires
identifiers, characters, booleans, number objects, and `.` to be
terminated with a "delimiter" or by the end of input.

* **References:**
* http://scheme-punks.org/wiki/index.php?title=ERR5RS:Lexical_Syntax
* http://lists.r6rs.org/pipermail/r6rs-discuss/2007-June/002649.html
* **Options:** delimited, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): delimited
* [Gleckler](WG1BallotGleckler.md): delimited, unspecified
* [Hsu](WG1BallotHsu.md): delimited, unspecified
* [Lucier](WG1BallotLucier.md): delimited
* [Medernach](WG1BallotMedernach.md): delimited
* [Read](WG1BallotRead.md): delimited
* [Shinn](WG1BallotShinn.md): delimited, unspecified
* [SnellPym](WG1BallotSnellPym.md): delimited, unspecified
* **Results:** **delimited**, unspecified
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> Allowing `#true` to mean `#t rue` is just silly. If implementations want to accept `#true`, they should be able to do so.
`Hsu`::
> I prefer to create less ambiguity here.
`Read`::
> What are we, 8-bit kiddies packing code into Microsoft BASIC? FORI=1TO10? Delimiters are there for a reason. We should make use of them.
`Shinn`::
> This is an ambiguity in R5RS that has no benefit and should be clarified.

### #119 Whether to treat # as a delimiter.

In R5RS `foo#f` is a valid identifier, whereas R6RS requires `#` to
act as a delimiter, so that this would parse as the identifier `foo`
followed by the false literal.

* **Options:**  delimiter, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): delimiter
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, delimiter
* **Results:** **no**, delimiter
* **Ratios:** 7:1
* **Rationales:**

`Cowan`::
> Backward compatibility rules here. Chicken depends heavily on identifiers with embedded `#`s.
`Hsu`::
> We have ways such as with the vertical bar of enclosing data, so there is no reason to make something like # confusing to the reader. `|foo#f|` is clear, while `foo#f` is not, so let's make # a delimiter.
`Read`::
> You would break Gambit's namespace separator!!!
`Shinn`::
> Several implementations make use of this for extended identifier syntax.

### #123 Extend unquote and unquote-splicing to multiple arguments

This is a change also made by R6RS (and CL).

* **References:**
* http://lists.scheme-reports.org/pipermail/scheme-reports/2011-April/000448.html
* http://www.rhinocerus.net/forum/lang-scheme/98742-quasiquote-syntax-rules-macro.html
* http://www.mail-archive.com/guile-user@gnu.org/msg03899.html
* **Options:** multiple, single, undecided
* **Default:** single
* **Voters:**
* [Cowan](WG1BallotCowan.md): single
* [Gleckler](WG1BallotGleckler.md): multiple
* [Hsu](WG1BallotHsu.md): multiple
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): undecided
* [Read](WG1BallotRead.md): single
* [Shinn](WG1BallotShinn.md): single
* [SnellPym](WG1BallotSnellPym.md): multiple
* **Results:** **single**, multiple, undecided, r6rs
* **Ratios:** 3:3, 3:1, 3:1
* **Rationales:**

`Cowan`::
> I'm not convinced this is useful enough.
`Gleckler`::
> One of the threads above indicates that Alan Bawden approves of this change. I defer to Alan's infinite wisdom in all things related to macros.
`Hsu`::
> Convincing arguments have been made as to why this is a good thing on the mailing list, and there are too many unsolved issues if we leave it as single.
`Shinn`::
> I'm unconvinced. The use cases seem rare (especially since WG1 has no low-level macros), and the semantics not obvious.

### #124 Nested quasiquote semantics

* **References:**
* http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.17
* http://lists.nongnu.org/archive/html/chicken-hackers/2010-12/msg00008.html
* **Proposals:**
* **r5rs:** unspecified
* **r6rs:** strict and multiple (implies multiple for #123)
* **chicken:** strict at level 0 (option 2 in second reference)
* **strict:** strict at all levels (R6RS with single for #123)
* **Options:** r5rs, r6rs, chicken, strict, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs, r5rs
* [Hsu](WG1BallotHsu.md): r6rs, strict
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): r6rs
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): strict, chicken, r5rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, strict, chicken, r5rs
* **Ratios:** 7:1, 7:1, 7:1
* **Rationales:**

`Cowan`::
> R6RS is better defined, and we should accept it. The vagueness in R5RS helps nobody.
`Hsu`::
> R6RS makes things much easier to work with in this regard. We should adopt this. I have encountered situations where this makes a great deal of sense, especially in macro writing.

### #125 Allow procedures not to be locations (making EQV? unspecified in some additional cases)

This is a change also made by R6RS.

* **Options:** r6rs, r5rs, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): r6rs
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): r5rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, r5rs
* **Ratios:** 7:1
* **Rationales:**

`Cowan`::
> Treating procedures as locations distinguishable by `eq?` is a hack nobody ought to depend on.
`Hsu`::
> This gives more flexibility to the implementation when dealing with quasiquote.
`Read`::
> What if a procedure gets inlined at every call site? What is its location then?
`Shinn`::
> We need a better rationale. Do any implementations do this, and why?

### #126 Partly specify the mutability of the values of quasiquote structures

This is a change also made by R6RS, specifically:

> A quasiquote expression may return either fresh, mutable objects or literal structure
> for any structure that is constructed at run time during the evaluation of the expression.
> Portions that do not need to be rebuilt are always literal

* **Options:** r6rs, r5rs, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs
* [Medernach](WG1BallotMedernach.md): undecided
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, undecided
* **Ratios:** 5:2
* **Rationales:**

`Cowan`::
> This is a useful provision and doesn't constrain anyone much.
`Hsu`::
> R6RS is an improvement in this regard.
`Shinn`::
> This is vague and could be worded better, but is probably better than saying nothing.

### #127 Specify the dynamic environment of the *before* and *after* procedures of dynamic-wind

R5RS is slightly ambiguous, saying

> BEFORE is called whenever execution enters the dynamic extent of the
> call to THUNK and AFTER is called whenever it exits that dynamic
> extent.

without saying clearly whether *before* and *after* themselves are
called before or after the dynamic extent is entered or exited.

* **Proposals:**
* **outside:** called outside the dynamic extent (R6RS)
* **inside:** called inside the dynamic extent
* **unspecified:** R5RS
* **Options:** outside, inside, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): outside
* [Gleckler](WG1BallotGleckler.md): outside
* [Hsu](WG1BallotHsu.md): outside, inside, unspecified
* [Lucier](WG1BallotLucier.md): outside
* [Medernach](WG1BallotMedernach.md): outside
* [Read](WG1BallotRead.md): outside
* [Shinn](WG1BallotShinn.md): outside
* [SnellPym](WG1BallotSnellPym.md): outside, inside, unspecified
* **Results:** **outside**, inside, unspecified
* **Ratios:** 8:0, 8:0
* **Rationales:**

`Cowan`::
> I think *outside* was the intention of the R5RS authors and this should be clarified.
`Hsu`::
> Making them outside is how I always think of these, and it makes more sense syntactically, visually, and in terms of programming convenience. Either way, we should specify one or the other, and not leave this unspecified.
`Read`::
> Outside makes the most sense. The procedures *before* and *after* are commonly used to set up and tear down, respectively, a context for the dynamic extent.
`Shinn`::
> This is most likely what was meant by R5RS.

### #135 let-values and let*-values

These R6RS procedures were part of #77 (modularization of multiple
values), but were never explicitly voted up or down by WG1, so I'm
opening a new ticket for them.

* **Options:** yes, no, module, wg2, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes, module, wg2
* [Hsu](WG1BallotHsu.md): yes, wg2, module
* [Lucier](WG1BallotLucier.md): yes, module
* [Medernach](WG1BallotMedernach.md): yes, wg2
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, wg2, module, no
* **Ratios:** 7:0, 7:0, 7:1
* **Rationales:**

`Cowan`::
> Some implementations can optimize these over calls to `call-with-values`, and they are much more convenient most of the time.
`Hsu`::
> These are simple and very useful constructs that are in common use throughout. They are an excellent addition to the multiple values abstraction and should certainly be included.
`Medernach`::
> Yes, however define-values are really more important IMHO.
`Read`::
> This is like the very handy `receive` of SRFI 8. Keep.
`Shinn`::
> No, these are minor utility macros, and arguably `receive` is better for most cases - they should be in a WG2 module.

### #137 Current-seconds semantics still open

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

[TimeCowan](TimeCowan.md) is equivalent to the `posix-integer` option, and in addition
changes the name to `current-posix-second`.

* **Proposals:**
* **cowan:** [TimeCowan](TimeCowan.md)
* **posix-integer:** POSIX time as an exact integer value
* **posix-flonum:** POSIX time as an inexact real value
* **tai-integer:** TAI time as an exact integer value
* **tai-flonum:** TAI time as an inexact real value
* **Options:** cowan, unspecified, undecided, none
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): cowan
* [Gleckler](WG1BallotGleckler.md): tai-integer, posix-integer, cowan, tai-flonum, posix-flonum
* [Hsu](WG1BallotHsu.md): tai-flonum, tai-integer, cowan, posix-flonum, posix-integer
* [Lucier](WG1BallotLucier.md): tai-flonum, posix-flonum
* [Medernach](WG1BallotMedernach.md): cowan
* [Read](WG1BallotRead.md): cowan
* [Shinn](WG1BallotShinn.md): tai-flonum, tai-integer, unspecified
* [SnellPym](WG1BallotSnellPym.md): tai-flonum, posix-flonum, tai-integer, posix-integer
* **Results:** **tai-flonum**, tai-integer, cowan, posix-flonum, posix-integer, unspecified
* **Ratios:** 4:1, 4:4, 5:0, 4:1, 5:0
* **Rationales:**

`Cowan`::
> That is, accept the [TimeCowan](TimeCowan.md) view as noted above.
`Gleckler`::
> Having a floating point representation seems even worse than using the POSIX representation. I don't want floating point error in my time.
`Hsu`::
> We should make it clear when we are dealing in POSIX, and otherwise we should avoid bugs, even if they are common. Floating points are more useful, in this case.
`Read`::
> That is, accept the [TimeCowan](TimeCowan.md) view as noted above.
`Shinn`::
> POSIX is just broken, too horribly to defend. It can neither reliably be used to measure real time (duration) because it shifts, nor can it be used to measure absolute time because it is ambiguous. If we don't settle on TAI we can only leave this unspecified.
`SnellPym`::
> I prefer TAI over POSIX as I hate being bug-compatible with past mistakes. However, regarding the potential implementation difficulties of TAI on a POSIX underlying system, I'd like to shove that under the unavoidable carpet of "Yeah, but implementation accuracy isn't particularly well defined" - so although I'd like `current-seconds` to monotonically and continuously increase about once a second, the implementation is quite free to return a time counter initially seeded from a POSIX clock without knowing how many leap seconds are in there, and just be inaccurate by a few seconds. As it might simply be wrong anyway due to a bad clock.

### #147 Allow literal file spec lists in include and include-ci

This could allow implementation-specific extensions to support files
don't have character-string names.  On the other hand, such names
probably shouldn't be used as source files, and there are other ways
to support this.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided, no, yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> The issue here is that not all valid filenames are valid character strings: in Posix, filenames are sequences of 8-bit units excluding 0x00 and 0x2F, and on Windows they are sequences of 16-bit units with more exclusions. Those don't necessarily translate one-to-one UTF-8 and UTF-16 respectively, or any other particular encoding. The language of file specs used by `open-*-file` and friends allow implementation-specific extensions to cover these problems; `include(-ci)` should allow the same, as should `load`.
`Hsu`::
> We should not prohibit extensions to `include`, but we should only define string based names.
`Read`::
> The Right Thing would be to make file paths specialized objects disjoint from strings, blobs, or anything else. That sounds like a job for WG2. We should go with whatever works.
`Shinn`::
> The file spec list isn't needed here, and can always be added later anyway.
`SnellPym`::
> The way we specify filenames should be consistent across Scheme.

### #148 Allow include-ci at top level

Currently `include-ci` is allowed as a module declaration but not at top level,
as `include` is.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no
* **Ratios:** 7:1
* **Rationales:**

`Cowan`::
> That is, allow it and remove the restriction. The point of `include-ci` is to bring in files of R5RS code that uses case typographically rather than orthographically.
`Hsu`::
> This is an oversight.
`Read`::
> No reason not to allow `include-ci` anywhere `include` is allowed.
`Shinn`::
> `include` and `include-ci` are both module forms. Including them in a local scope is bad style.

### #149 blob ports

We've voted to add string ports, which are character ports backed by
Scheme strings.  Since we have blobs another potential extension is
blob ports, which binary ports backed by blobs.  These are described
in [PortsCowan](PortsCowan.md), but it's unclear if they were specifically voted for or
against in the previous ballot.

* **Options:** cowan, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): cowan
* [Gleckler](WG1BallotGleckler.md): cowan
* [Hsu](WG1BallotHsu.md): cowan
* [Lucier](WG1BallotLucier.md): cowan
* [Medernach](WG1BallotMedernach.md): cowan, undecided
* [Read](WG1BallotRead.md): cowan
* [Shinn](WG1BallotShinn.md): cowan
* [SnellPym](WG1BallotSnellPym.md): cowan
* **Results:** **cowan**, undecided
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> Blob ports allow the construction and deconstruction of blobs in the same way that string ports allow the construction and deconstruction of strings, and should be provided for the same reasons.
`Hsu`::
> We want a way to get a port abstraction on blobs.
`Read`::
> There's no reason not to provide these.
`Shinn`::
> It's bloat, but it's pretty fundamental.

### #150 cond-expand at top level

Currently `cond-expand` is only valid as a module declaration.  Should
we allow it at top level in a program?

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no
* **Ratios:** 5:3
* **Rationales:**

`Cowan`::
> Scripts need the ability to decided which modules to load (or which code to include) based on either the presence of other modules or specific criteria about the implementation, just as modules do.
`Hsu`::
> `cond-expand` is a bad and flawed construct that should not be made available to the user. Its appearance in a module declaration is somewhat forgivable as a static construct, but it's use at anything other than inside of a module declaration is, IMO, very bad. We should not encourage its use.
`Shinn`::
> The purpose of `cond-expand` is to act statically, even before macro expansion.

### #153 Renaming blob procedures

The blob procedures don't follow the same system as the rest.  I
propose these changes:

```
copy-blob => blob-copy
copy-blob! => blob-copy!
partial-blob => blob-copy-partial
copy-partial-blob! -> blob-copy-partial!
```

Note this is modulo the choice of "blob" or "bytevector"
or whichever.

* **Options:** new, original, remove, undecided
* **Default:** original
* **Voters:**
* [Cowan](WG1BallotCowan.md): new
* [Gleckler](WG1BallotGleckler.md): new
* [Hsu](WG1BallotHsu.md): new, original
* [Lucier](WG1BallotLucier.md): new
* [Medernach](WG1BallotMedernach.md): new
* [Read](WG1BallotRead.md): new
* [Shinn](WG1BallotShinn.md): new
* [SnellPym](WG1BallotSnellPym.md): new, original, remove
* **Results:** **new**, original, remove
* **Ratios:** 8:0, 8:0
* **Rationales:**

`Cowan`::
> The existence of the old names just shows that I didn't consider blobs and [CompleteSequenceCowan](CompleteSequenceCowan.md) in the same breath. Let's fix that while we can.
`Hsu`::
> Consistent naming is important here, and the hierarchical naming policy makes much more sense.
`Read`::
> The new names are more consistent.
`Shinn`::
> New, but using `bytevector` (or whichever wins #85).

### #154 Physical newline in a string equivalent to \n (that is, U+000A)

R5RS leaves this situation undefined, but R6RS, CL, and most languages
that allow it (C does not) treat physical newline and escaped newline
as equivalent, even if the local representation of line endings is
\r\n or U+0085 or what not.  Another possibility is to treat string literals
broken across lines as errors.

* **Options:** unix, local, error, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unix
* [Gleckler](WG1BallotGleckler.md): unix
* [Hsu](WG1BallotHsu.md): unix, unspecified
* [Medernach](WG1BallotMedernach.md): unix
* [Read](WG1BallotRead.md): unix
* [Shinn](WG1BallotShinn.md): unix
* [SnellPym](WG1BallotSnellPym.md): unix, error, unspecified, local
* **Results:** **unix**, error, unspecified, local
* **Ratios:** 7:0, 7:0, 7:0
* **Rationales:**

`Cowan`::
**Note:** There is nothing Unix about this except that Unixen have the same convention. As noted, many other languages prescribe it.
`Gleckler`::
> What happens when people use ASCII correctly (unlike UNIX) and break lines with \r\n? I hope that \r gets the same treatment as \n. I originally voted for an error in this case, but it's just too useful to be able to include line breaks in strings, so I'm changing my vote.
`Hsu`::
> We should not allow the ambiguity that a string may contain different data when loaded on two different operating systems, so the treatment of newlines should be transparent. If an user wants to actually have a string with a CRLF line ending, then we can configure this at a port or output level. We should not do this at the reader level. This is also in keeping with standard practice in other languages, as the ticket mentions. Deviating from this is just confusing.
`Read`::
> `\n` is a pretty universal line terminator. Even Windows which uses `\r\n` handles it more gracefully than it did in the past.
`Shinn`::
> A literal newline should be the same as \n.

### #155 Make recursively defined code an explicit error

Allowing examples like these will make code-walkers (including
compilers and interpreters) excessively complicated:

#1=(begin (display #\x) . #1#)

(lambda #2=(a b c #2#) ...)

(+ . #3=(1 2 3 . #3#))

* **Options:** error, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): error
* [Gleckler](WG1BallotGleckler.md): error
* [Hsu](WG1BallotHsu.md): unspecified
* [Lucier](WG1BallotLucier.md): error
* [Medernach](WG1BallotMedernach.md): error
* [Read](WG1BallotRead.md): error
* [Shinn](WG1BallotShinn.md): error
* [SnellPym](WG1BallotSnellPym.md): unspecified, error
* **Results:** **error**, unspecified
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> This does not mean that the error must be *signalled* (raised). It just means that Scheme programmer shouldn't be allowed to depend on these tricks working, for some value of "working".
`Gleckler`::
> I agree with Alex and John that this needn't be a "signaling" error.
`Hsu`::
> Making this an error makes things more complicated, not less, and it makes life more difficult on naive implementations. Leave this unspecified.
`Read`::
> Anyone who actually writes code like the above is probably wearing a trollface. I don't see any reason to accommodate such pathological cases.
`Shinn`::
> It's worth mentioning this is an error (not necessarily signalling).

### #156 Replace "an error is signalled" with "an implementation-dependent object is raised as if by `raise`"

The following situations are described in draft 1 (and R5RS) with "an
error is signalled":

1. The *file-spec* given to `call-with-input-file`,
> `call-with-output-file`, `open-input-file`, or `open-output-file`
> represents a file that cannot be opened.

2. An end of file is read from a port by `read` after the beginning
> of an object's external representation, but the external
> representation is incomplete and therefore not parsable.

I propose that in both cases the implementation be required to raise
an exception as if by applying `raise` (that is, non-continuably) to
an implementation-defined object, which means it can be caught by the
R7RS exception system.  Note that there is no requirement to create a
fresh object.

* **Options:** signal, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): signal
* [Gleckler](WG1BallotGleckler.md): signal
* [Hsu](WG1BallotHsu.md): signal
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): signal
* [Read](WG1BallotRead.md): signal
* [Shinn](WG1BallotShinn.md): signal
* [SnellPym](WG1BallotSnellPym.md): signal, unspecified
* **Results:** **signal**, unspecified, undecided
* **Ratios:** 7:0, 7:1
* **Rationales:**

`Cowan`::
> I am not in favor of forcing Schemes to signal exceptions where they were not required to do so in R5RS. But in those few cases where implementations already must signal an exception, they should signal it so that it can be caught using the standard exception catcher. The overhead of doing so should be nearly nil.
`Hsu`::
> Regardless of what is raised, the exception must be capturable by the exception system. I think it makes more sense to put this at the beginning and continue to use "an error is signalled," but with a more precise explanation of what this means.
`Read`::
> I thought signalling an error *meant* raising an exception in implementations that provided exceptions?
`Shinn`::
> I think this is already implied by the semantics.

### #162 Remove DELAY and FORCE altogether

They are present in R4RS and R5RS, but not IEEE Scheme (which is our
baseline).  There are problems with a straightforward implementation
that SRFI 45 fixes, but we voted down SRFI 45.  Given that, we should
consider removing them from the standard altogether.  (Of course this
does not mean compliant implementations can't provide them, it just
means they won't be in a standard module.)

Since the inconsistency was raised and people are going so far as
to remove these, we can entertain votes for SRFI-45's `lazy` again.

* **Options:** remove, keep, lazy, undecided
* **Default:** keep
* **Voters:**
* [Cowan](WG1BallotCowan.md): lazy, remove
* [Gleckler](WG1BallotGleckler.md): remove, lazy, keep
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): keep, lazy
* [Medernach](WG1BallotMedernach.md): lazy, remove
* [Read](WG1BallotRead.md): remove
* [Shinn](WG1BallotShinn.md): lazy, keep
* [SnellPym](WG1BallotSnellPym.md): remove, keep, lazy
* **Results:** *lazy*, remove, keep, undecided
* **Ratios:** 4:3, 4:2, 6:1
* **Rationales:**

`Cowan`::
> Alex added lazy (i.e. SRFI 45) back to the ballot, so I voted for it. Failing that, I really don't want to standardize something that's known to be broken in existing non-SRFI-45 implementations. They don't really provide lazy Scheme, and those who want actual Lazy Scheme know where to get it.
`Hsu`::
> I think this warrants more work before a final vote.
`Read`::
> If you want SRFI-45, you know where to find it. The implementation is straightforward and doesn't rely on anything not in Scheme besides BOX and UNBOX, which could be easily implemented with unique types or records.
`Shinn`::
> I'm reversing my decision on this. It's not clear that `lazy` is the best solution, but it's the best we have and solves a known problem. On the other hand, `delay` and `force` work fine for a wide variety of problems and removing them and breaking R5RS compatibility is extreme.

### #164 Meaning of char-numeric?

The current draft, like R6RS, defines `char-numeric?` according to the
nonexistent Unicode Numeric property.  That has to be fixed.  Options:

1. **Any.** `char-numeric?` returns `#t` if the character's
> Numeric_Type property value is other than `None`.  This means that
> many hanzi are both alphabetic and numeric.

2. (Omitted, because it does not preserve IEEE Scheme)

3. **ASCII.** Define `char-numeric?` to return `#t` only for ASCII
0, 1, 2, 3, 4, 5, 6, 7, 8, and 9.  This retains compatibility witht
> R5RS, and we can still use `char-numeric?` to parse numbers, and
> safely use `(- (char->integer c) (char->integer #\0))` to obtain the
> digit value the character represents.  (Note: R5RS programs that use
> `char-numeric?` to parse numbers will break if we adopt the current
> draft's definition of `char-numeric?`).  Gauche, Gambit, and Chicken
> (without the utf8 egg) work like this.

4. **Digit.** Define `char-numeric?` as equivalent to the
> Numeric_Digit property (general category value of Nd).  Guile 2.0,
> Kawa, Larceny, Ypsilon, Mosh, and [IronScheme](IronScheme.md) work like this.

5. **Number.** Define `char-numeric?` as equivalent to the Number
> property (general category values of Nd, Nl, No).  Scheme48, Chez,
> and Ikarus work like this.

* **Options:** any, number, digit, ascii, undecided
* **Default:** ascii
* **Voters:**
* [Cowan](WG1BallotCowan.md): digit
* [Gleckler](WG1BallotGleckler.md): ascii, digit
* [Hsu](WG1BallotHsu.md): number, digit, ascii, any
* [Medernach](WG1BallotMedernach.md): digit, number
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): digit, number, ascii, any
* [SnellPym](WG1BallotSnellPym.md): digit, (any number), ascii
* **Results:** **digit**, number, ascii, any, undecided
* **Ratios:** 5:1, 5:1, 6:0, 6:1
* **Rationales:**

`Cowan`::
> There is no advantage to allowing U+0660 (Arabic-style 0) to be called non-numeric. Of course, there is no requirement for an application to support this character, but if it does support it, it should tell the truth about it, just as Arabic letters should be reported as alphabetic. On the other hand, letter-based numbers, fractions, Roman numerals don't really fit the traditional profile of numeric characters, which is why the expansive *number* and *any* choices don't seem appropriate.
`Gleckler`::
> Changing this will break too many programs. Programs that are Unicode-aware can handle this themselves.
`Hsu`::
> Number seems like the better and more general choice.
`Read`::
> My position is that the standard should specify that `char-numeric?` should return `#t` for 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9; and whatever other characters the implementation chooses to recognize as digits. The Arabic numerals are fairly widespread throughout the global programming community, and I don't think including other Unicode characters considered digits is necessarily; contrariwise I don't want to exclude them either. Let WG2 sort this mess out.
`Shinn`::
> I agree with John's reasoning on this, and second it with `number` because I think that's what the R6RS intended, and would prefer to be inclusive here.

### #166 Add predicate and accessors for error objects

(Email from Vincent Manis)

Problem: It's impossible to write a portable error handler that writes
out the *message* and *irritants* that were passed to `error`.

This comes about because `error` creates an "implementation-defined
object". I would assume that this hides the whole exception class
hierarchy a WG2 implementation might provide. Since the *message*
and *irritants* arguments to `error` are presumably living in this
implementation-defined object, it should be simple enough to provide
accessors to extract them, so that the above "portable error handler"
can be written.

Suggestion: Add the following procedures:

`(error-object? `*object*`)`

Returns `#t` if *object* is something created by `error`, `#f`
otherwise. Any constraints on type disjointness are up to the
implementation.

`(error-object-message `*object*`)`

Returns the message of *object*.

`(error-object-irritants `*object*`)`

Returns a list of the irritants of *object*.

* **Options:** manis, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): manis
* [Gleckler](WG1BallotGleckler.md): no
* [Lucier](WG1BallotLucier.md): manis
* [Medernach](WG1BallotMedernach.md): manis
* [Read](WG1BallotRead.md): manis
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): manis
* **Results:** **manis**, no
* **Ratios:** 5:2
* **Rationales:**

`Cowan`::
> I understand Vincent to mean that `error-object?` returns `#t` if the object *could* have been created by `error`, not that it necessarily was. So what is guaranteed is that objects raised by `error` satisfy `error-object?`, not that nothing else satisfies it.
`Gleckler`::
> I would have voted for manis, but I would prefer names like `error-foo' instead of `error-object-foo', which is redundant and verbose. I don't feel strongly enough to write a proposal about this, so I'd rather leave this to WG2.
`Read`::
> Any time you have a compound object, you should be able to get at the bits that interest the consumers of the object. Consumers of error objects are almost certainly interested in the message and irritants.
`Shinn`::
> Leave this up to WG2.

### #167 Add constructor for error objects

(Email from Vincent Manis)

Problem: Raising arbitrary objects as exceptions has been found to be
nasty in some other languages (Python and C++ in particular).

This one is a tad speculative, but I'm reluctant to encourage people
to write things like `(raise 4)`, because of course it doesn't respect
any module boundaries. I think the intent with the descriptions of
`raise` and `raise-continuable` was to allow exception hierarchies to
be added in WG2 without constraining them here. I would suggest adding
a new procedure:

`(make-error-object `*message*` `*obj* ...`)`

to creates the implementation-defined object `error` is supposed to
create, and adding a sentence to the `raise` and `raise-continuable`
entries that says "The effect of applying this procedure to an object
not created via `make-error-object` is implementation-defined." This
allows WG2 to do what it wants regarding exception objects, and to
limit the types of exception objects allowed, without breaking
anything in WG1. `Error` can be defined as:

```
 (define (error message . objs)
   (raise (apply make-error-object message objs)))
```


* **Options:** manis, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Lucier](WG1BallotLucier.md): manis
* [Medernach](WG1BallotMedernach.md): manis
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): manis
* **Results:** **no**, manis
* **Ratios:** 4:3
* **Rationales:**

`Cowan`::
> I agree that developers should document what sort of condition objects their APIs raise, and that raising numbers is a Bad Thing. But the fact is that existing code and implementations raise all sorts of things as condition objects: in SRFI 12 (which Chicken uses), they are lists. Not everything can be stuffed into the message-plus-irritants paradigm.
`Read`::
> I am not sure what the right thing here is. I think we should err on the side of what people are currently doing with Scheme.
`Shinn`::
> Meangingless without #166.

### #169 Add standard-*-port procedures

These return the initial values of the corresponding `current-*-port`
procedures, and can be used to access the implementation-provided
standard input, output, and error streams.

* **Options:** r6rs, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): r6rs
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, r6rs
* **Results:** **no**, r6rs
* **Ratios:** 5:3
* **Rationales:**

`Cowan`::
> As Alex says, these are tricky to get right, and anyway the description here does *not* match R6RS, which requires the `standard-*-port` procedures to return *fresh* ports. Let's not go there.
`Hsu`::
> These are very common and should be provided.
`Read`::
> Thanks to the ubiquity of C, just about everybody has stdin, stdout, and stderr. Even in the pathological Scheme-on-a-toaster case, all but the most meager 8-bit operating systems have some notion of standard I/O channels. to which program output and logs may be written and from which user input may be read. If it's a problem we can always make these optional.
`Shinn`::
> There's no need, and the semantics can get messy. I'd need to see a strong rationale and use cases to be convinced.
`SnellPym`::
> I wince at the sandboxing implications. Code run with `current-*-port` parameterised shouldn't be able to get hold of the standard ports again. If you want to keep the original values, do so explicitly in your own code.

### #171 Duplicate identifiers in define-record-type

What happens if `define-record-type` is specified with two fields that
have the same `accessor` identifiers provided for both fields?  More
generally, we need to say what happens when any two identifiers are
non-unique.

This ticket deals specifically with the situation where two
identifiers (accessors or mutators) of two field clauses in a
`define-record-type` form are identical. This is not meant to address
field names and what happens or what it means if the field names are
symbolically equivalent but lexically distinct.

* **Options:** error, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): error
* [Gleckler](WG1BallotGleckler.md): error
* [Hsu](WG1BallotHsu.md): error
* [Lucier](WG1BallotLucier.md): error
* [Medernach](WG1BallotMedernach.md): error
* [Read](WG1BallotRead.md): error
* [Shinn](WG1BallotShinn.md): error
* [SnellPym](WG1BallotSnellPym.md): error, unspecified
* **Results:** **error**, unspecified
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> This makes no sense; it ought to be an error (that is, no conformant Scheme program should rely on what happens in such a case).
`Hsu`::
> This situation doesn't make sense, so it should be an error.
`Read`::
> Clearly a record spec with two fields having identical names is erroneous. The programmer ought to be smart enough to sufficiently disambiguate the fields.
`Shinn`::
> It is an error (not necessarily signalled).

### #173 Unifying BEGINs

In R5RS, there are three kinds of BEGINs:

1) All subforms are expressions; this can be used wherever an
expression can be used.  (4.2.3)

2) All subforms are definitions; this can be used wherever an internal
definition can be used.  (5.2.2)

3) Subforms can be definitions or expressions intermixed in any order;
this can be used only at top level.  (In R7RS we extend this to module
top level as well).  (5.1)

In particular,

```
(define (x)
 (define y 32)
 (begin
   (define z 45)
   (set! y z))
 y)
```

is not licensed by any of these provisions, and consequently is not
valid R5RS Scheme.  Nevertheless, all of my usual Schemes accept the
above definition except Scheme48/scsh and SSCM -- actually, SSCM fails
when you invoke x rather than when you define it.  So I'm proposing
that we unify them for R7RS.

* **Options:** cowan, r5rs, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r5rs
* [Gleckler](WG1BallotGleckler.md): r5rs
* [Hsu](WG1BallotHsu.md): cowan
* [Lucier](WG1BallotLucier.md): cowan
* [Medernach](WG1BallotMedernach.md): r5rs
* [Read](WG1BallotRead.md): r5rs
* [Shinn](WG1BallotShinn.md): r5rs
* [SnellPym](WG1BallotSnellPym.md): cowan, r5rs
* **Results:** **r5rs**, cowan
* **Ratios:** 5:3
* **Rationales:**

`Cowan`::
> I'm no longer sure I believe my own reasoning here. Since such a `begin` can only be used at the boundary between definitions and expressions, there's little sense in a macro returning it, as such a macro could only be used there either.
`Hsu`::
> Having three different begins and two of which are not splicing while one is, but can only appear at the top-level, is confusing and not in the spirit of Scheme.
`Read`::
> I am really not sure what to do here.

### #174 Safe uses of multiple values

Currently, uses of `values` where the values are discarded anyway is
illegal, but all the usual Schemes except SCM and SSCM accept them (I
tested with `begin`).  Should we go with something close to the R6RS
wording?

"The continuations of all non-final expressions within a sequence of
expressions, such as in `lambda`, `begin`, `let`, `let*`, `letrec`,
`letrec*`, `case`, and `cond` forms, take an arbitrary number of
values."

The definition of `begin` would need to change too:

```
(define-syntax begin
  (syntax-rules ()
    ((begin exp)
     exp)
    ((begin exp1 exp2 ...)
     (call-with-values
         (lambda () exp1)
       (lambda args
         (begin exp2 ...))))))
```

* **Options:** safe-values, r5rs, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): safe-values
* [Gleckler](WG1BallotGleckler.md): safe-values
* [Hsu](WG1BallotHsu.md): safe-values
* [Lucier](WG1BallotLucier.md): safe-values
* [Medernach](WG1BallotMedernach.md): safe-values
* [Read](WG1BallotRead.md): safe-values
* [Shinn](WG1BallotShinn.md): safe-values
* [SnellPym](WG1BallotSnellPym.md): safe-values, r5rs
* **Results:** **safe-values**, r5rs
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> What makes obvious sense, and what almost all implementations permit anyway, should be permitted by the standard. That is, programmers should be able to rely with confidence on its working, rather than just hoping they don't stumble across an implementation where it happens not to work.
`Hsu`::
> Most everyone does this, it is pretty much a de facto standard and we should respect it.
`Read`::
> The values are going to be thrown away anyway. Who cares how many there are?
`Shinn`::
> As John points out, this is almost de-facto anyway.

### #45 Record-let syntax and semantics

```
(record-let <record-data> ((<variable> <field>) ...)
  <body>)
```

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


* **Options:** record-let, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): record-let, undecided
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, undecided, record-let
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> This is actually very nice; in particular, it is hygienic, unlike `with` statements in many languages.. However, it is primitive syntax (syntax-rules cannot implement it unless it has low-level access to the fields of a record) and it is a novelty. Let's push this to WG2.
`Gleckler`::
> This should be in WG2.
`Hsu`::
> This is underspecified at the moment and should be cleaned up, but I think it should be in here, as it is a common thing to want to do.
`Medernach`::
> Let push this to WG2 features.
`Shinn`::
> Leave this to WG2.
`SnellPym`::
> This can just be a macro that expands: `(record-let foo ((a foo-a) (b foo-b)) (list a b))` into `(let ((a (foo-a foo)) (b (foo-b foo))) (list a b))`, can't it? I see no need for magic low-level access to record internals, which would unhappily restrict it to actual records anyway (so I'm a bit queasy about the name). `(record-let foo ((a car) (b cdr)) ...)` would be useful in its own right.

### #172 Multiple returns from `map`

R6RS specifies that `map` does not mutate previous results if there
are multiple returns from map. Should we include this language?

* **Options:** r6rs, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs
* [Medernach](WG1BallotMedernach.md): r6rs
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, undecided
* **Ratios:** 6:1
* **Rationales:**

`Cowan`::
**Rationale**: This prevents an implementation based on directly mutating a string, vector, or list, but I think the results are better.
`Hsu`::
> This provides a measure of stability to `map` that we would not otherwise have.
`Shinn`::
> We can't protect programmers from mutating the map themselves, but map should not mutate the results itself.

### #178 Shadowing with internal definitions

From Andre Von Tonder:

On p 19, some shadowing problems that would break lexical scope are
declared to be errors.  However, I believe there are other examples
that shold be errors that are not covered by the report.
>
In R6RS a more general criterion was used - please see R6RS for
details.
>
Here is an example that does not violate the WG1 report but should be
an error becasue it violates lexical scoping.  It does not violate the
WG1 criterion because the meaning of x is not needed to determine
whether (foo x p ) is a definition.

```
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
```

Here is another example that WG1 allows but that would cause violation
of lexical scoping, because the macro would be evaluated first and
treat ... as a placeholder in a region where it is shadowed to be the
variable bound to 1:

```
    (let ()
      (define-syntax list-macro
        (syntax-rules ()
          ((_ x ...) (list x ...))))
      (define ... 1)    ;; This shadows ... in previously expanded macro
                        ;; body and will be a violation of lexical scoping
      (list-macro 1 2)) ;; if the last line evaluates to (1 2)
```

OTOH, it is unclear to me if WG1 allows this or not.

```
    (let ((x #f))
      (let-syntax ((foo (syntax-rules (x)
                          ((_ x y) (define y 'outer))
                          ((_ _ y) (define y 'inner)))))
        (let ()
          (define x #f)
          (foo x p)
          p)))
```

* **Options:** r6rs, r5rs, tonder, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs, undecided
* [Lucier](WG1BallotLucier.md): tonder
* [Medernach](WG1BallotMedernach.md): tonder
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): undecided
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, undecided, tonder
* **Ratios:** 4:2, 4:2
* **Rationales:**

`Cowan`::
> I'm more willing to believe that the R6RS people got this right than that we did.
`Hsu`::
> I prefer the R6RS language here, as it scales well and is unambiguous.

## WG1 - Modules

### #112 REPL redefinitions

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

| From/To       | import | import syntax | define | define-syntax |
| import        |   ?    |       ?       |   ?    |       ?       |
| import syntax |   ?    |       ?       |   ?    |       ?       |
| define        |   ?    |       ?       |   ?    |       ?       |
| define-syntax |   ?    |       ?       |   ?    |       ?       |

Not all 64 combinations necessarily make sense.  The default from R5RS
is "unspecified", which means all 16 values are unspecified.  Note in
most implementations there is no such thing as a "reference" to
existing syntax, since macros are expanded once, but this is not the
case for SCM or Wraith Scheme.

* **Proposals:**
* **override:** override for all 16 values (non-syntax to syntax can break closure references)
* **preserve:** preserve for all 16 values (must always create a new definition, not mutate, contrary to most implementations)
* **common:** most common behavior among implementations - override, except preserve for non-syntax to syntax
* **simple:** override, except unspecified for non-syntax to syntax
* **dynamic:** override, except unspecified for syntax to anything (compatible with SCM/Wraith)
* **Options:** override, preserve, common, dynamic, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): common
* [Gleckler](WG1BallotGleckler.md): unspecified, common
* [Hsu](WG1BallotHsu.md): common, simple, unspecified, undecided
* [Medernach](WG1BallotMedernach.md): common, unspecified
* [Read](WG1BallotRead.md): common
* [Shinn](WG1BallotShinn.md): common, simple, dynamic
* [SnellPym](WG1BallotSnellPym.md): unspecified
* **Results:** **common**, unspecified, simple, dynamic, undecided
* **Ratios:** 5:2, 6:0, 6:0, 6:0
* **Rationales:**

`Cowan`::
> The common behavior is provided by all implementaions except SCM and Wraith. We should standardize it.
`Hsu`::
> We should go with the common case, which I think is overwhelmingly chosen and used. Either that or we can leave things unspecified, but this results in an ambiguous REPL, which I thought the WG1 was trying to avoid as much as possible.
`Shinn`::
> This is largely a de facto standard.
`SnellPym`::
> I think REPL semantics should be left quite open to implementation variation. Standardising code written in files is what's needed for portability!

### #132 Imports override previous imports?

The current draft describes importing different bindings for the same
identifier as "an error."  R6RS explicitly requires this to signal an
error.  Do we want to change this?

This ticket refers only to modules - the top-level semantics are
decided in ticket #112.

* **Options:** override, preserve, error, unspecified, undecided
* **Default:** error
* **Voters:**
* [Cowan](WG1BallotCowan.md): error
* [Gleckler](WG1BallotGleckler.md): error
* [Hsu](WG1BallotHsu.md): error, undecided
* [Medernach](WG1BallotMedernach.md): error
* [Read](WG1BallotRead.md): error
* [Shinn](WG1BallotShinn.md): error
* [SnellPym](WG1BallotSnellPym.md): error, override, unspecified, preserve
* **Results:** **error**, override, unspecified, preserve, undecided
* **Ratios:** 7:0, 7:0, 7:0, 7:0
* **Rationales:**

`Cowan`::
> This means that to override you have to declare the things overridden, which is probably good. Silent overriding is very ugly, as is silent non-overriding. High-quality implementations should make this a static error.
`Hsu`::
> Two imports into the same scope is essentially equivalent to two definitions with the same identifier as the target binding. Imports at the same level should be imported to the same scope, rather than introducing some new, implied scope.
`Read`::
> One binding per identifier makes the most sense for a module. Modules have a slightly different semantics than the top level, in that they are treated more like bags of bindings than like sequences of things to be evaluated.
`Shinn`::
> Modules should be strict, and allowing implicit conflict resolution is asking for trouble.

### #160 Interleaving of imports and code in a module

Given

```
   (module (name)
     (begin c1 ...)
     (import (A))
     (begin c2 ...)
     (import (B))
     (begin c3 ...))
```

the intention, reference implementation, and specification from
Scheme48 on which the syntax was based say that all imports establish
the initial environment and then the code is expanded in order, but
interleaving the imports is conceivable.

* **Options:** shinn, interleave, unspecified, undecided
* **Default:** shinn
* **Voters:**
* [Cowan](WG1BallotCowan.md): shinn
* [Gleckler](WG1BallotGleckler.md): shinn
* [Hsu](WG1BallotHsu.md): shinn, undecided, interleave
* [Lucier](WG1BallotLucier.md): unspecified
* [Medernach](WG1BallotMedernach.md): undecided
* [Read](WG1BallotRead.md): shinn
* [Shinn](WG1BallotShinn.md): shinn
* [SnellPym](WG1BallotSnellPym.md): shinn, interleave, unspecified
* **Results:** **shinn**, undecided, interleave, unspecified
* **Ratios:** 6:1, 6:0, 6:1
* **Rationales:**

`Cowan`::
> Conceivable but not worthwhile.
`Hsu`::
> Interleaving complicates the semantics. We should keep with imports as being at the top level.
`Medernach`::
> Is it really beneficial ? Is it because of macro expanding to module forms ? Will the gain outweight the confusion ?
`Read`::
> Conceivable but not worthwhile.

### #163 Allow modules at the REPL?

Should users be allowed to enter a `module` form at the REPL?

Note that there are actually many varying approaches to generating
moduls at runtime, and Scheme48 and Chibi use an out-of-band REPL
operation to create new modules, leaving the `module` binding open.

* **Options:** yes, no, unspecified, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, unspecified, no
* [Medernach](WG1BallotMedernach.md): unspecified
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): unspecified, yes
* **Results:** **no**, unspecified, yes, undecided
* **Ratios:** 3:3, 3:3, 3:1
* **Rationales:**

`Cowan`::
> Given the static nature of our modules, I can't see doing this. Modules are for programming in the medium to large scale, or for making libraries.
`Gleckler`::
> It should be possible to experiment with module forms at the REPL. Otherwise, we're losing the dynamic nature of the language bit by bit.
`Hsu`::
> This is something that is very convenient to do, and can be implemented even on implementations that leave the module binding open.
`Medernach`::
> Implementations should have full freedom there.
`Read`::
> Leaning towards no here, with the caveat that it doesn't preclude the REPL from providing the functionality via other means (e.g., a language external to Scheme in which such things may be declared).
`Shinn`::
> The whole point of the module system is that it's static. Dynamic module definition should be an implementation-defined extension (and/or a module in WG2).
`SnellPym`::
> See my earlier point about REPLs being implementation-specific.

### #141 What are the semantics of modules with respect to separate compilation?

[ModulesShinn](ModulesShinn.md) says that the bodies of libraries are evaluated
before any of the bodies of the importing library; does that include,
eg, "at compile time" rather than at "run time"?  It's not clear.

* **Options:** compile-time, unspecified, undecided
* **Default:** undecided
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Gleckler](WG1BallotGleckler.md): unspecified
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): compile-time
* [Medernach](WG1BallotMedernach.md): unspecified
* [Read](WG1BallotRead.md): unspecified
* [Shinn](WG1BallotShinn.md): unspecified
* [SnellPym](WG1BallotSnellPym.md): unspecified
* **Results:** **unspecified**, compile-time, undecided
* **Ratios:** 6:1, 6:1
* **Rationales:**

`Cowan`::
> What happens at compile-time stays at compile-time.
`Hsu`::
> I think we need to introduce a clearer concept of what happens at compile time and what happens at run time, and understand the differences here. As such, I do not think that we should finalize this vote yet.
`Read`::
> As Ren Ho谷k said, "That's just it. We don't know." Best to let implementations decide.
`Shinn`::
> This is not even specified by R6RS - we should't touch it.
`SnellPym`::
> Compile time and run time are difficult to precisely define, without limiting yourself to certain classes of implementations.

### #158 mutating imports

Currently the semantics of calling set! or define
on an imported binding is undefined.  Do we
want to specifically make this an error?

* **Options:** error, allowed, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): error
* [Gleckler](WG1BallotGleckler.md): error
* [Hsu](WG1BallotHsu.md): error, undecided, unspecified
* [Medernach](WG1BallotMedernach.md): error
* [Read](WG1BallotRead.md): error
* [Shinn](WG1BallotShinn.md): error
* [SnellPym](WG1BallotSnellPym.md): error, unspecified, allowed
* **Results:** **error**, unspecified, allowed, undecided
* **Ratios:** 7:0, 7:0, 7:0
* **Rationales:**

`Cowan`::
> Yes, for the same reasons as #132.
`Hsu`::
> Immutable imports is something that we definitely want to have, and is something that many implementations use. We should ensure, at least, that we maintain compatibility with those systems that use such semantics.
`Read`::
> Modules are different beasts than scripts or REPL expressions.
`SnellPym`::
> Immutability of imported bindings is necessary for mutability analysis in the presence of separate compilation - which is a very important optimisation in Scheme (as it lets you avoid a LOT of indirection).

### #159 base environments

What is the base environment provided by the repl,
scripts, and the result of (scheme-report-environment 7)?

The intention was the base script environment was empty,
scheme-report-environment was (scheme base), and repls
were an implementation-defined superset thereof, but there
are other options and we need to clarify this.

* **Options:** shinn, undecided
* **shinn:** intention as described above
* **Default:** shinn
* **Voters:**
* [Cowan](WG1BallotCowan.md): shinn
* [Gleckler](WG1BallotGleckler.md): shinn
* [Hsu](WG1BallotHsu.md): undecided, shinn
* [Medernach](WG1BallotMedernach.md): shinn
* [Read](WG1BallotRead.md): shinn
* [Shinn](WG1BallotShinn.md): shinn
* [SnellPym](WG1BallotSnellPym.md): shinn
* **Results:** **shinn**, undecided
* **Ratios:** 6:1
* **Rationales:**

`Cowan`::
> I don't see anything better than this.
`Hsu`::
> We should have more discussion on this.
`Read`::
> Shinn's proposal works for me.

### #161 module argument to eval

It would be useful to allow modules as an argument to eval in addition
to environments.  This could be done with a special syntax, or just
the module name as a list.

R6RS provides a procedure `environment` which just
takes a list that looks like an import spec an generates
the corresponding environment.

* **Options:** r6rs, none, undecided
* **Default:** r6rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs, undecided, no
* [Medernach](WG1BallotMedernach.md): r6rs
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, undecided, no
* **Ratios:** 7:0, 7:0
* **Rationales:**

`Cowan`::
> Actually, it takes any number of such arguments. This allows `eval` to do its work against anything specifiable as a module, whether it includes the base or not.
`Hsu`::
> R6RS' `environment` is extremely useful and works well.
`Read`::
> That sounds like a good idea.
`Shinn`::
> This is too convenient to leave out, so long as you have `eval`.

### #139 `exit`

The ballot statement for #62 said we had voted for `exit` when we
voted for [ModulesShinn](ModulesShinn.md), but that page doesn't mention `exit`.  So we
need to vote on it.

* **Options:** yes, no, undecided
* **Default:** yes
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 7:0, 7:0
* **Rationales:**

`Cowan`::
> Yes. Every implementation provides it.
`Hsu`::
> Very useful, yes.
`Read`::
> No reason not to, except to call it "quit".
`Shinn`::
> But we need to be careful specifying unwinding effects here.

### #144 strip prefix on import

I'm thinking that for importing code that defines its external symbols
as `foo:this`, `foo:that`, and `foo:tother`, there should be a type of
import clause that strips a specified prefix from imported symbols.
This is equivalent to renaming on import or renaming on export, but
less painful, in the same way as the `prefix` import clause does.

Specific proposal: `(strip-prefix <import-set> <prefix-identifier>)`.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, yes, undecided
* **Ratios:** 4:3, 4:2
* **Rationales:**

`Cowan`::
> This is especially nice if you are importing numeric libraries: you can use `+ - * /` with non-standard definitions, but if straightforwardly imported they will not collide with standard `+ - * /`.
`Hsu`::
> This is very useful, and a common form among implementation module systems.
`Shinn`::
> This is really messy to implement, and modules should provide their exports with no prefix to begin with.
`SnellPym`::
> Why not just fail to put the prefix on in the first place?

## WG1 - I/O

### #133 Provide read-line

This is an R6RS procedure that was part of [PortsCowan](PortsCowan.md), but never
explicitly voted up or down by WG1.  It reads a single line up to a
line delimiter from a given port (the current input by default) and
discards the line delimiter.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 8:0, 8:0
* **Rationales:**

`Cowan`::
> Hand-rolled implementations will tend to handle only the line delimiter used by default in the programmer's environment, which does not generalize. In particular, it's now common to have text files with different types of line endings in the same environment.
`Hsu`::
> very common and very useful.
`Read`::
> Everybody should have `read-line`. It's a primitive in lots of languages.
`SnellPym`::
> This is just far too useful in practice...

### #170 Add with-error-to-file procedure

Since we now have `current-error-port`, arguably we should have
`with-error-to-file` for completeness.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **no**, yes, undecided
* **Ratios:** 4:3, 4:1
* **Rationales:**

`Cowan`::
> I see no point in making it easy to redirect the standard error.
`Hsu`::
> Useful, and it rounds things out.
`Read`::
> This would make a good log-capture facility, for instance.
`Shinn`::
> And also `call-with-error-file`? I think the completeness argument is being taken too far in this case.

### #176 Are string ports exclusively character ports?

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

* **Options:** character-only, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): unspecified
* [Gleckler](WG1BallotGleckler.md): character-only
* [Hsu](WG1BallotHsu.md): character-only, undecided, unspecified
* [Medernach](WG1BallotMedernach.md): unspecified
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): character-only
* [SnellPym](WG1BallotSnellPym.md): unspecified
* **Results:** character-only, *unspecified*, undecided
* **Ratios:** 3:3, 3:1
* **Rationales:**

`Cowan`::
> I see no reason to block an extension in this area.
`Hsu`::
> We don't want to confuse binary and character ports. Strings should only have characters (code points) as their underlying subelement, and we should not confuse that with byte vectors or the like.
`Shinn`::
> I think we need a clear separation of binary and character ports.

### #177 Distinguish file and string ports?

Should there exist predicates that identify string and file ports?

* **Options:** string-port?, file-port?, both, neither, undecided
* **Default:** neither
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): both
* [Hsu](WG1BallotHsu.md): both, no, string-port?, file-port?, undecided
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no, both
* **Results:** **no**, both, string-port?, file-port?, undecided
* **Ratios:** 5:2, 6:0, 6:0, 6:0
* **Rationales:**

`Cowan`::
> I don't see the value of this.
`Gleckler`::
> These are trivial to implement and useful for debugging.
`Hsu`::
> It makes sense to have them, and we should have both or neither rather than only one or the other, but some systems don't provide these.
`Read`::
> Should be implementation-defined.
`Shinn`::
> It's not clear what this would be used for. In WG2, it can be useful in conjunction with other extensions such as file-descriptor handling, but that's for WG2.

### #131 Output procedures return value

Output procedures (display, write, newline) currently return
unspecified value, do we wish to make them return something (like in
case of an error) or not?

Need proposals.

* **Options:** r5rs, undecided
* **Default:**
* **Voters:**
* [Cowan](WG1BallotCowan.md): r5rs
* [Gleckler](WG1BallotGleckler.md): r5rs
* [Hsu](WG1BallotHsu.md): r5rs, undecided
* [Lucier](WG1BallotLucier.md): r5rs
* [Medernach](WG1BallotMedernach.md): r5rs
* [Read](WG1BallotRead.md): r5rs
* [Shinn](WG1BallotShinn.md): r5rs
* [SnellPym](WG1BallotSnellPym.md): r5rs
* **Results:** **r5rs**, undecided
* **Ratios:** 8:0
* **Rationales:**

`Cowan`::
> There's not much you can do if these fail.
`Gleckler`::
> What is this, C?
`Hsu`::
> We should stick with the unspecified error, there is no good reason to change this.
`Read`::
> No reason to break the expectations of thousands of Scheme programmers.
`Shinn`::
> It "is an error" to read/write from/to closed ports. If anything, this should signal an error, not return a value.

### #134 Provide flush-output-port

This is an R6RS procedure that was part of [PortsCowan](PortsCowan.md), but never
explicitly voted up or down by WG1.  It flushes implementation output
buffers on the specified port, the current output port by default.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes, no
* **Results:** **yes**, no, undecided
* **Ratios:** 8:0, 8:0
* **Rationales:**

`Cowan`::
> Very helpful in TTY-style interactions such as student programs often use.
`Hsu`::
> This is an extremely useful procedure and is crucial in some applications.
`Medernach`::
> Definitely
`Read`::
> Having control of when output gets flushed is pretty essential for interactive programs or programs that communicate over a network.
`Shinn`::
> Yes, this is essential.

## WG1 - Numerics

### #117 Real numbers have imaginary part #e0

In R6RS, a complex number with imaginary part 0 is only real if the
imaginary part is an exact 0.  In R5RS, this was not true, and the
requirement was simply that `(zero? (imag-part Z))` be true.

* **Options:** exact-only, any-zero, unspecified, undecided
* **Default:** any-zero
* **Voters:**
* [Cowan](WG1BallotCowan.md): exact-only
* [Gleckler](WG1BallotGleckler.md): exact-only
* [Hsu](WG1BallotHsu.md): exact-only, any-zero, unspecified, undecided
* [Lucier](WG1BallotLucier.md): exact-only
* [Medernach](WG1BallotMedernach.md): exact-only
* [Read](WG1BallotRead.md): exact-only
* [Shinn](WG1BallotShinn.md): any-zero
* [SnellPym](WG1BallotSnellPym.md): any-zero
* **Results:** **exact-only**, any-zero, unspecified, undecided
* **Ratios:** 6:2, 6:0, 6:0
* **Rationales:**

`Cowan`::
> An inexact zero may or may not be zero: it can be any small real number whose absolute value lies between zero and the smallest positive real number representable as an inexact number. Numbers with inexact zeros are still complex, not real. This mildly breaks backward compatibility, but it does so in the direction of correctness.
`Gleckler`::
> Correctness trumps backwards compatibility, particularly with numerics.
`Hsu`::
> I prefer the exact form.
`Lucier`::
> Comments: This has many implications in how one defines elementary functions, etc., and I think it's important to treat numbers with an inexact zero imaginary part as non-real
`Shinn`::
> This is a compatibility issue, and there are many implementations with no exact complex numbers.

### #120 Define the semantics of the transcendental functions more fully

R6RS has an extended description of the transcendental functions.  Do
we want to include this?

TODO: explain the exact diff, why it is desirable, and whether any
reasonable alternatives are possible.

* **References:**
* http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.7.3.2
* **Options:** r6rs, r5rs, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs, undecided, r5rs
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): r6rs
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): undecided, r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, undecided, r5rs
* **Ratios:** 7:1, 8:0
* **Rationales:**

`Cowan`::
> What it amounts to is that IEEE 754 semantics should be followed. We can just say that. In particular, R5RS has nothing to say about infinities and [NaNs](NaNs.md), but we should for the benefit of implementations that support them (most).
`Hsu`::
> We should clear this up and try to improve the descriptions of these things where we can, but we should still discuss exactly what we want to include and how/why.
`Lucier`::
> Scheme and Common Lisp have a long history of trying to do the "right thing" with the (elementary) transcendental functions. The description of R6RS is short and clear, mathematically, and I see no reason not to adopt it.
`Read`::
> R6RS semantics is preferable where it is more clear/complete.
`Shinn`::
> Probably R6RS, I need to double check this.

### #121 The semantics of expt for zero bases has been refined

This is a change also made by R6RS.

R5RS says:

> Returns z1 raised to the power z2. For z1 /= 0, z1^z2^ = e^z2^ log z1; 0^z^ is 1 if z = 0 and 0 otherwise.

R6RS says:

> Returns z1 raised to the power z2. For nonzero z1, this is e^z2^ log z1. 0.0^z^ is 1.0 if z = 0.0, and 0.0 if (real-part z) is positive. For other cases in which the first argument is zero, either an exception is raised [...] or an unspecified number object is returned.


* **Options:** r6rs, r5rs, undecided
* **Default:** r5rs
* **Voters:**
* [Cowan](WG1BallotCowan.md): r6rs
* [Gleckler](WG1BallotGleckler.md): r6rs
* [Hsu](WG1BallotHsu.md): r6rs, undecided, r5rs
* [Lucier](WG1BallotLucier.md): r6rs
* [Medernach](WG1BallotMedernach.md): undecided
* [Read](WG1BallotRead.md): r6rs
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): r6rs
* **Results:** **r6rs**, undecided, r5rs
* **Ratios:** 7:1, 7:0
* **Rationales:**

`Hsu`::
> I prefer the more detailed description.
`Lucier`::
> Comment: This is not the complete R6RS description and examples. I prefer the complete description
`Medernach`::
> Strictly speaking z1^z2^ has no values at z1 = 0.0 and z2 = 0.0 because it is not continuous there.

### #122 Make infinity, NaN, and -0.0 semantics (when supported) consistent with IEEE 754

R5RS does not explicitly describe these values.  We have to decide
whether to require that, if an implementation provides any of these
values, they must be consistent with IEEE 754.

R6RS both requires these values and requires they be consistent with
IEEE 754.

* **Options:** ieee-754, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): ieee-754
* [Gleckler](WG1BallotGleckler.md): ieee-754
* [Hsu](WG1BallotHsu.md): ieee-754, undecided, unspecified
* [Lucier](WG1BallotLucier.md): unspecified, ieee-654
* [Medernach](WG1BallotMedernach.md): ieee-754, unspecified
* [Read](WG1BallotRead.md): ieee-754
* [Shinn](WG1BallotShinn.md): ieee-754
* [SnellPym](WG1BallotSnellPym.md): unspecified, ieee-754
* **Results:** **ieee-754**, unspecified, ieee-654, undecided
* **Ratios:** 6:2, 7:1, 7:0
* **Rationales:**

`Cowan`::
**Note:** But only if supported by the implementation.
`Hsu`::
> I don't think there is a good reason to deviate here, but I also think that we could benefit from some more discussion on this if there are good examples to why deviating from ieee-754 improves things.
`Medernach`::
> Only for implementations providing it.
`Read`::
**Note:** I think everybody uses IEEE 754 now. I shouldn't say that given the software systems I've seen, but the cases where it's not used are ancient and pathological.
`Shinn`::
> It doesn't seem reasonable to allow implementations to support these but use different semantics.
`SnellPym`::
> IEEE 754 is a good standard, but I think conforming with it should be optional for implementations

### #175 Control of significant digits or decimal places in NUMBER->STRING

Vincent Manis pleads for a way to write numbers with a specified precision:

http://lists.scheme-reports.org/pipermail/scheme-reports/2011-May/000709.html

I (Alaric Snell-Pym) wondered if this should be done via
NUMBER->STRING or via an optional extra argument to ROUND etc
specifying a precision, as a number like `0.01` to get two decimal
places. How to provide significant figures rather than DP without
introducing a base-10 dependency is left as an exercise to the reader
(as is the task of deciding if I'm mad for not wanting a base-10
dependency)

* **Options:** manis, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): no, undecided, manis
* [Lucier](WG1BallotLucier.md): undecided
* [Medernach](WG1BallotMedernach.md): manis
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): manis
* **Results:** **no**, manis, undecided
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> Allowing rounded results in `number->string` mixes concerns.
`Hsu`::
> This is better tackled by more complete formatting procedures or some additional rounding operation on strings. We should not clutter `number->string` to provide this specific functionality.
`Read`::
> This is useful (e.g., business apps) but should be part of a more comprehensive number-formatting library.
`Shinn`::
> This is a nice convenience, but should really be part of a general formatting library (which WG2 will have two of).

### #138 [DivisionRiastradh](DivisionRiastradh.md) domain

Zero as a divisor aside, what should the domain of the proposed
procedures be?

1. Any real numbers?
1. Integers only?
1. Exact integers only?

* **Options:** reals, integers, exact-integers
* **Default:**
* **Voters:**
* [Cowan](WG1BallotCowan.md): integers
* [Gleckler](WG1BallotGleckler.md): integers
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): exact-integers
* [Medernach](WG1BallotMedernach.md): reals
* [Read](WG1BallotRead.md): integers
* [Shinn](WG1BallotShinn.md): integers
* [SnellPym](WG1BallotSnellPym.md): integers, exact-integers
* **Results:** **integers**, exact-integers, reals, undecided
* **Ratios:** 5:1, 5:1, 5:1
* **Rationales:**

`Cowan`::
> I think division of non-integers is messy.
`Hsu`::
> I want to see more discussion on this.
`Medernach`::
> One may want "modulo 2pi" with it for instance.
`Shinn`::
> I'd need to see a good use case for non-integral dividends and remainers.

### #217 [DivisionRiastradh](DivisionRiastradh.md) exactness preservation

What about exactness preservation?

1. Not exactness preserving
1. Exactness preserving unless the implementation can prove that an inexact argument can't affect the result (as in the case of an exact zero dividend and an inexact divisor)
1. Exactness preserving in all cases

* **Options:** not-exactness-preserving, exactness-preserving, exactness-preserving-unless
* **Default:**
* **Voters:**
* [Cowan](WG1BallotCowan.md): exactness-preserving-unless
* [Gleckler](WG1BallotGleckler.md): exactness-preserving-unless
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): exactness-preserving-unless
* [Medernach](WG1BallotMedernach.md): exactness-preserving-unless, not-exactness-preserving
* [Read](WG1BallotRead.md): exactness-preserving-unless
* [Shinn](WG1BallotShinn.md): exactness-preserving-unless
* [SnellPym](WG1BallotSnellPym.md): exactness-preserving-unless
* **Results:** **exactness-preserving-unless**, not-exactness-preserving, undecided
* **Ratios:** 7:0, 7:1
* **Rationales:**

`Cowan`::
> This is consistent with everything else.
`Hsu`::
> I want to see more discussion on this before I make my decision.
`Shinn`::
> It seems reasonable to allow that optimization.

### #140 Removing `quotient`, `remainder`, `modulo`

Are we removing the IEEE Scheme functions `quotient`, `remainder`, and
`modulo` from WG1 Scheme?  If so, we need a special justification, due
to the charter text:

> Existing features of IEEE Scheme may be removed only if a strong
> case can be made that they are fundamentally flawed. Insofar as
> practical, the language should be backwards compatible with the IEEE
> standard, the R5RS standard, and an appropriate subset of the R6RS
> standard.

Here's what [DivisionRiastradh](DivisionRiastradh.md) says:

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

* **Options:** yes, no, module, undecided
* **Default:** yes
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): module, yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes, yes, yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, module, undecided, no
* **Ratios:** 7:1, 8:0, 8:0
* **Rationales:**

`Cowan`::
> On reflection, every implementation will go on supporting them anyway, if just to keep old code working. Consistent with my "modules = optional" views, I see no reason to have a module here. There should be discouraging language in the report, though.
`Hsu`::
> I think we can reasonably relegate these, but I'm hesitant to remove them completely, since they tend to be very commonly used. More discussion is a good thing here.
`Medernach`::
> Please keep it, too much code rely on it and there are natural names.
`Read`::
> We should not suddenly take away what many Scheme programmers rely on.
`Shinn`::
> Too big a change.
`SnellPym`::
> I'd love to see them removed to make Scheme more consistent, but in hindsight, it's a petty incompatibility with existing code.

### #151 Extend `finite?` and `nan?` to non-real values

R6RS specifies the domain of `finite?` and `nan?` as the real numbers
only.  I propose that `finite?` return `#t` on a non-real value iff
both the real part and the imaginary part are finite and not `+nan.0`,
and that `nan?` return `#t` on a non-real value iff either the real or
the imaginary part is `+nan.0`.

* **Proposals:**
* **cowan:** the above description
* **Options:** cowan, unspecified, undecided
* **Default:** unspecified
* **Voters:**
* [Cowan](WG1BallotCowan.md): cowan
* [Gleckler](WG1BallotGleckler.md): cowan
* [Hsu](WG1BallotHsu.md): undecided, cowan, unspecified
* [Lucier](WG1BallotLucier.md): cowan
* [Medernach](WG1BallotMedernach.md): cowan
* [Read](WG1BallotRead.md): cowan
* [Shinn](WG1BallotShinn.md): cowan
* [SnellPym](WG1BallotSnellPym.md): cowan
* **Results:** **cowan**, undecided, unspecified
* **Ratios:** 7:1, 8:0
* **Rationales:**

`Cowan`::
> That is, what's proposed there. This is what people who do complex flonum programming need.
`Hsu`::
> I don't know the ramifications of this, and I would like more discussion on this.
`Shinn`::
> This seems reasonable assuming the implementations have such numbers.

### #152 exact-integer-sqrt inconsistent with multiple values module

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

* **Options:** values-in-core, return-list, return-pair, return-root-only, new-module, remove, nothing, undecided
* **Default:** nothing
* **Voters:**
* [Cowan](WG1BallotCowan.md): values-in-core
* [Gleckler](WG1BallotGleckler.md): values-in-core, remove, return-root-only, return-list, return-pair, new-module
* [Hsu](WG1BallotHsu.md): values-in-core, new-module, nothing, undecided, return-root-only, remove, return-pair, return-list
* [Lucier](WG1BallotLucier.md): values-in-core, return-root-only
* [Medernach](WG1BallotMedernach.md): new-module, return-root-only, values-in-core, remove, undecided, return-list, return-pair, nothing
* [Read](WG1BallotRead.md): values-in-core
* [Shinn](WG1BallotShinn.md): remove, return-root-only, return-list, return-pair, new-module, values-in-core
* [SnellPym](WG1BallotSnellPym.md): values-in-core, new-module, (return-list return-pair), return-root-only, (remove nothing)
* **Results:** **values-in-core**, new-module, return-root-only, remove, return-list, return-pair, nothing, undecided
* **Ratios:** 6:2, 6:2, 7:1, 7:1, 7:1, 8:0, 8:0
* **Rationales:**

`Cowan`::
> Multiple values are cheap to provide poorly for implementations that don't care, and can be provided well with some effort by implementations that do. No need to make them optional, therefore. Doing so also removes an extra dependency from WG2 packages that want to return multiple values.
`Hsu`::
> I don't want to have any procedure in the language that uses lists/pairs as intermediate container constructs, as I am offended (in some ways) by that sort of conflation of the use of lists. Values should be in the core.
`Medernach`::
> Why not a module containing tricky arithmetic operations ?
`Read`::
> Multiple values is the cleanest way to do this. Promote to core.

### #180 Make case and cond clauses into bodies

Andy Wingo suggests: make the clauses in `case` and `cond` forms
(without `=>`, naturally) be BODY instances, to allow them to have
definitions.  It is well defined AFAIK, and costs nothing.

The counter-argument is that it doesn't "look" like the sort of place
definitions are allowed.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **no**, yes, undecided
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> There is nothing in Scheme like ((= x y) (define z ...) (+ z x y)) except at top level. Let's not go there.
`Gleckler`::
> Ugly, and let's not extend the language beyond common practice in trivial areas like this.
`Hsu`::
> There is no reason not to do this. If you don't like the style, then don't use it, but a standards body should not be in charge of dictating good style.
`Read`::
> Oh no you di'int.
`Shinn`::
> This is confusing.

### #181 Add WHEN and UNLESS to the base module

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 5:2, 5:0
* **Rationales:**

`Cowan`::
> These are good things, and there is no reason to keep them separated out as if they were untouchables.
`Hsu`::
> These are very useful and help to discourage one-armed `if`s.
`Read`::
> These sound like Perl or Ruby constructs. Scheme could use a dose of the brevity and convenience those languages provide. (Thought not too much...)
`Shinn`::
> If anything, WG2.

### #182 Add WHILE and UNTIL

These trivial syntaxes add familiarity for new Scheme programmers
coming from other languages, as will almost always be the case.  LOOP
is too big and named-LET too alien.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): no, undecided, yes
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, yes, undecided
* **Ratios:** 5:2, 5:0
* **Rationales:**

`Cowan`::
> Yes, I know we are not supposed to encourage mutability. But we have a lot of it already, particularly when dealing with ports.
`Gleckler`::
> These go against the grain of the language. Are they even widely supported?
`Hsu`::
> They are rarely if ever used among non-beginner Scheme programmers, and I know of very few teachers who would encourage its use. Thus, they don't provide enough usefulness to the general Scheme community to warrant standardization.
`Read`::
> They are convenient. But what about DO?
`Shinn`::
> Emphatically not - this actively encourages non-functional programming, and proper loop macros are more flexible anyway.
`SnellPym`::
> named-let is a good thing to learn!

### #183 Escaped newline removes following whitespace?

Andy Wingo suggests the R6RS handling of escaped embedded newlines:

```
    "asdadf \
    asdfadf"
```

in R6RS has the same meaning as "asdf asdfadf".  It allows you to
nicely indent strings that you need to line-break for width.  I
suggest that the production

```
   \ NEWLINE WHITESPACE*
```

within string literals be elided.

Note an alternate method for handling embedded strings with nice
indentation is scribble syntax.

We voted on various string syntaxes previously but did not
specifically propose this R6RS extension.  We should have a rationale
if we don't follow it.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 5:2, 5:0
* **Rationales:**

`Cowan`::
> Cheap and useful.
`Hsu`::
> This is really useful, but the scribble syntax is also very useful, and we should consider this as a separate ticket.
`Read`::
> Shell scripts and C use it all the time.
`Shinn`::
> This is ugly and confusing, and unnecessary if embedded newlines are specified.

### #184 Require CHAR=?, STRING=? etc. to accept arbitrary numbers of arguments?

R5RS makes a point of specifying that supporting more than two
arguments is optional.  (Everything not explicitly mentioned is
optional, so this may have significance.)  R6RS requires accepting 2
or more arguments.  Currently Racket, Gambit, Guile, Chez, Ikarus,
Larceny, Ypsilon, Mosh, and Scheme 9 support the feature, whereas
Gauche, MIT, Chicken, Bigloo, Scheme48/scsh, Kawa, SISC, Chibi,
STklos, and SSCM don't.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided, yes, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 5:2, 6:1
* **Rationales:**

`Cowan`::
> I think the R5RS implementations should be bootstrapped into the future here. It also meets the consistency guideline in the charter.
`Hsu`::
> I don't understand why we would limit this. It seems like an strange limitation to enforce on users. I would like to see more rationale on why we might want to vote no, but otherwise I tihnk they should accept multiple numbers of arguments.

### #185 Add sixth "centered" division operator

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

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): undecided
* [Lucier](WG1BallotLucier.md): no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, undecided
* **Ratios:** 6:2
* **Rationales:**

`Cowan`::
> I'm not convinced they are useful except maybe to do bignums with, in which case it's easy to define them. **Note:** There are rationales for most of the division operators at [Riastradh's original proposal](http://mumble.net/~campbell/tmp/division.txt).
`Hsu`::
> I don't understand these well enough to vote on them yet.
`Shinn`::
> There are too many division operators already. What are they all used for? We need to revisit this.

### #195 Editorial: proposed rewording for `begin`

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

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 8:0, 8:0
* **Rationales:**

`Cowan`::
> I think this is a documentation issue only: the documentation Andy complained abuot is just for expression-`begin`.
`Hsu`::
> I think we ought to have a splicing `begin` form, so I don't know what else would differ between R6RS and R7RS in this regard.
`Shinn`::
> We can revisit the language, though R5RS doesn't correspond exactly to R6RS here.

### #198 Make it an error for a procedure mapped by MAP and friends to mutate the result list/string/vector

This is possibly difficult to enforce, and can break existing R5RS
programs written in very bad style.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 6:1, 6:0
* **Rationales:**

`Cowan`::
> I don't see how it can break anything, because no R5RS program can count on `map` using a mutable-result implementation anyway. `Vector-map` and `string-map` are more likely to.
`Hsu`::
> We don't have to enforce this, but it does let people write things in a way that they can get away with being slightly lazy. Making it an error is a good thing in this case.
`Medernach`::
> No, but please put a warning about doing it in the report as "shooting yourself in the foot".
`Read`::
> You don't want to modify a list you're mapping over. Not EVER. Map is a functor from scheme objects onto lists of scheme objects. It says nothing about HOW the function is to be mapped; if the list is mutated mid-map, you don't know what the results will be. "It is an error" means the behavior could be undefined, which is definitely the case here.
`Shinn`::
> But I don't expect many implementations will actually signal an error in this case.

### #199 Make it an error for a procedure mapped by MAP and friends to return more than once

This is possibly difficult to enforce, and can break existing R5RS
programs.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): undecided, yes, no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 4:3, 3:1
* **Rationales:**

`Cowan`::
> "Difficult to enforce" is irrelevant; "is an error" means no Scheme programmer should rely on the results of it.
`Hsu`::
> I would like to know the ramifications of this.
`Medernach`::
> Multiple returns is one Scheme essential feature, let not restrict it, there is no reason here.
`Read`::
> See above.
`Shinn`::
> Returning more than once is not inherently a bad thing, and if anything we should rely on #172. There's simply no reason to make this an error.

### #200 Completing the blob procedures

Add `blob`, `blob-map`, `blob-for-each`, and blob conversion functions
to and from lists/vectors/strings.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): undecided, yes, no
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, yes, undecided
* **Ratios:** 5:2, 5:1
* **Rationales:**

`Cowan`::
> Too much for WG1.
`Hsu`::
> Ratione: I know that these would be useful, but I don't know what they would look like, and they should be in the spirit of blobs and not ad hoc.
`Medernach`::
> Unappropriate for blobs.
`Shinn`::
> Too much - we'll want a blob library in WG2 anyway.
`SnellPym`::
> Blob's don't have an internal structure to map or for-each over. Sure, we can measure positions in them in bytes, but that's distinct from saying they're *made* of bytes.

### #205 Roll partial-blob-copy(!) into blob-copy(!)

... with extra arguments.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): no
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **no**, yes, undecided
* **Ratios:** 5:3, 5:1
* **Rationales:**

`Cowan`::
> We did not roll `substring` into `string-copy` with additional arguments; the only reason not to use `subblob` as a name is its risibility.
`Hsu`::
> This simplifies the name space and should make things easier. I like this sort of interface better than remembering another name.
`Shinn`::
> These are redundant.

### #206 Provide read-syntax for blobs

R6RS provides a `#vu8(...)` read-syntax for bytevectors.  SRFI-4 uses
`#u8(...)`.

* **Options:** r6rs, srfi-4, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): r6rs, srfi-4
* [Hsu](WG1BallotHsu.md): r6rs, srfi-4, undecided, no
* [Lucier](WG1BallotLucier.md): srfi-4
* [Medernach](WG1BallotMedernach.md): no
* [Read](WG1BallotRead.md): no
* [Shinn](WG1BallotShinn.md): r6rs
* [SnellPym](WG1BallotSnellPym.md): srfi-4, no, r6rs
* **Results:** *srfi-4*, no, r6rs, undecided
* **Ratios:** 4:3, 2:3, 4:0
* **Rationales:**

`Cowan`::
> I can't see anyone writing these by hand. They might add a little efficiency compared to a `blob` procedure (analogous to `string`, `vector`, and `list`).
`Hsu`::
> We should have one, regardless of what we pick, and I prefer the more descriptive (slightly) R6RS version more than the SRFI-4 version.
`Read`::
> Ugly. Not likely to be used in code.
`Shinn`::
> Bikeshedding, R6RS wins.

### #207 Editorial: Polar complex numbers are inexact

Add a note saying that `1@2` and `(make-polar 1 2)` MAY evaluate to an
inexact complex number.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): undecided, yes, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 6:1, 7:0
* **Rationales:**

`Cowan`::
> This is obviously an oversight: exact values passed to `make-polar` aren't going to come out exact from `make-rectangular`, and essentially all Schemes store complex numbers as rectangulars internally. (The [Pure language](http://pure-lang.googlegroups.com) supports both representations internally, but I can't see going there for standard -Scheme.)
`Hsu`::
> I haven't thought enough about this.
`Read`::
> If you must do computing with exact polar numbers, you know where to find the records/uniqueness types.
`Shinn`::
> This makes sense.

### #208 Is | a valid identifier?

The grammar in 7.1.1 allows `|` as an <identifier>. However, page 5
suggests the `|...|` form is only for convenience (e.g. `|foo bar|` is
equivalent to `foo\x20;bar`). There's no way to normalise `|` to
anything without the vertical bars that's a valid identifier. Was that
intentional, or should the rule be

```
<vertical bar> <symbol element>+ <vertical bar>
```

Vote `remove` to remove the `|...|` syntax altogether.

* **Options:** remove, empty-valid, empty-invalid, undecided
* **Default:** empty-valid
* **Voters:**
* [Cowan](WG1BallotCowan.md): empty-valid
* [Gleckler](WG1BallotGleckler.md): empty-invalid, empty-valid, remove
* [Hsu](WG1BallotHsu.md): empty-valid, empty-invalid, undecided, removed
* [Lucier](WG1BallotLucier.md): empty-invalid
* [Medernach](WG1BallotMedernach.md): empty-valid
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): remove, empty-valid
* [SnellPym](WG1BallotSnellPym.md): empty-valid
* **Results:** **empty-valid**, empty-invalid, remove, undecided, removed
* **Ratios:** 5:2, 5:1, 6:1, 6:0
* **Rationales:**

`Cowan`::
> Not sure why this is supposed to be the default. | is convenient on occasion, and allows `string->symbol` to accept any string.
`Hsu`::
> There's no reason to limit the bar syntax when | is a perfectly good identifier. We should remove language that suggests it is only for convenience.
`SnellPym`::
> What else would (string->symbol "") return?

### #191 Include CLOSE-PORT ?

Should we include `close-port`, as a generic version of
`close-input-port` and `close-output-port`?

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): no
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): undecided
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** *yes*, no, undecided
* **Ratios:** 4:3, 4:1
* **Rationales:**

`Cowan`::
> I could see adding this to a Scheme that has input/output ports to close both sides of the port, but the WG1 standard doesn't have them. Just as a generic procedure, no.
`Gleckler`::
> It seems silly to omit this since it's so useful and common. I note that the title of this ballot item takes advantage of case-insensitivity.
`Hsu`::
> It's useful and general, but I'm not attached to it.
`Shinn`::
> You generally know whether it's an input or output port.

### #188 Clarify wording of `and` and `or` definitions

The definitions of `and` and `or` may be slightly confusing. Reword
them to be more clear. One possible hiccup is that the current
language permits the return of different false values, while a clearer
wording may preclude this.

R6RS provides a clearer definition that does not provide wiggle room
for multiple false values. Should we use that?

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): no
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, no, undecided
* **Ratios:** 7:1, 7:0
* **Rationales:**

`Cowan`::
> I know why Guile needs multiple false values (to handle Emacs Lisp as well as Scheme), but I'd rather see such a system saying it overrides the Standard in this respect.
`Gleckler`::
> I don't see any reason we should have to vote on whether to make things clearer. However, I don't see any reason to worry about permitting different false values.
`Hsu`::
> We should clarify this wording specifically to eliminate this ambiguity about false values.
`Read`::
> I've seen where multiple falses can lead us in Python. I don't want to go there.
`Shinn`::
> I don't see why we need to go out of our way to forbid multiple false values if an implementation has them.

### #187 Clarify duplicate bindings in `let*`

The language of the standard could clarify that duplicate bindings are
permitted in the clauses of a `let*`.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): yes
* **Results:** **yes**, undecided, no
* **Ratios:** 7:0, 7:0
* **Rationales:**

`Cowan`::
> Why not?
`Hsu`::
> Arguably this is not necessary, but it does help people who are trying to read the standard. This is an issue that has cropped up from time to time so maybe it makes sense to include language about it in the standard, even at the expense of a bit of verbosity.
`Read`::
> Given the sequential nature of `let*`, it makes more sense than for `let` or `letrec`.
`Shinn`::
> Meh.

### #215 initial value argument to make-blob

`make-blob` should either have an initial value argument, or rationale
why it is inconsistent with `make-vector` and `make-string`.

Vote `yes` for an initial value argument.

* **Options:** yes, no, undecided
* **Default:** no
* **Voters:**
* [Cowan](WG1BallotCowan.md): yes
* [Gleckler](WG1BallotGleckler.md): yes
* [Hsu](WG1BallotHsu.md): yes, undecided, no
* [Lucier](WG1BallotLucier.md): yes
* [Medernach](WG1BallotMedernach.md): yes
* [Read](WG1BallotRead.md): yes
* [Shinn](WG1BallotShinn.md): yes
* [SnellPym](WG1BallotSnellPym.md): no
* **Results:** **yes**, no, undecided
* **Ratios:** 7:1, 7:0
* **Rationales:**

`Cowan`::
> As noted, I don't see blobs as specialized vectors. Still, I suppose initialization to all zeros couldn't hurt. Okay.
`Hsu`::
> I assume that this will be an optional argument, so, yes.
`Read`::
> Consistency is good.
`Shinn`::
> This is pretty useful, especially without `blob-fill!`.
`SnellPym`::
> BLobs don't have an internal structure to "fill". Let new blobs be all zeroes, as there's no boundaries in a string of zero bits...

### #216 Controlling use of reader labels on output

There are cases when one does not want to output reader labels for
shared structure, such as when you don't care (and want the output to
be more legible), or when you know that the time or space requirements
to construct the table will be too large.

We could offer a parameter to control this, or have a separate
procedure (e.g. `write/simple`) which doesn't use the reader labels.

Finer grained control may also let use specify a predicate for which
values are interesting (e.g. never use labels for strings), or only
use labels for cycles, etc.

* **Options:** parameter, write/simple, none, undecided
* **Default:** none
* **Voters:**
* [Cowan](WG1BallotCowan.md): write/simple
* [Gleckler](WG1BallotGleckler.md): write/simple, parameter
* [Hsu](WG1BallotHsu.md): undecided, no, parameter, write/simple
* [Medernach](WG1BallotMedernach.md): write/simple
* [Read](WG1BallotRead.md): write/simple
* [Shinn](WG1BallotShinn.md): write/simple
* [SnellPym](WG1BallotSnellPym.md): write/symbol, no, parameter
* **Results:** **write/simple**, no, parameter, write/symbol, undecided
* **Ratios:** 5:2, 5:2, 6:1, 5:1
* **Rationales:**

`Cowan`::
> I think a separate procedure is marginally better than a parameter.
`Gleckler`::
> However, I don't like the name. I'd prefer `write-simple', `write-simply', or `write-without-reader-labels'.
`Hsu`::
> Is this is common use? What would this look like? I want more discussion on this, but otherwise, I am not sure it makes sense, so I'm going with none after undecided.
`Medernach`::
> However write-simple would be a better name (Ok, this is bikeshed)
`Read`::
> A separate procedure makes sense, and is easy for the programmer to comprehend.
`Shinn`::
> This control is necessary.
`SnellPym`::
> Putting too much of this stuff into parameters means it's easy to assume wrongly about how `write` will behave, forgetting that some caller might tinker with some parameter and BREAK YOU.
