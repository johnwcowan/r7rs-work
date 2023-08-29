These are the portable libraries Committee B will be voting on.

Free docket names: Bia Dike Gaia Leto Nyx Orion Xenos Zephyrus; Kronos used but dropped.


# Docket Index

The following order is not necessarily the voting order.

 * [Red: data structures](#user-content-red-docket-data-structures)
 * [Tangerine: numerics](#user-content-tangerine-docket-numerics)
 * [Hypnos: SRFIs](#user-content-hypnos-docket-portable-srfis)
 * [Eos: not SRFIs](#user-content-eos-docket-portable-not-srfis)
 * [Orange: SRFIs](#user-content-orange-docket-portable-srfis)
 * [Urania: not SRFIs, no implementations](#user-content-urania-docket-portable-not-srfis-no-implementation-yet)
 * [Selene: not SRFIs, no implementations](#user-content-selene-docket-portable-not-srfis)
 * [Pan: not SRFIs, no implementations](#user-content-pan-docket-portable-not-srfis)
 
# Red Docket (data structures)

* Quick summary:
  * comprehensive list library
  * comprehensive vector library
  * sorting
  * hash sets (including character sets) and bags
  * hashtables
  * immutable pairs
  * random=-access pairs
  * immutable deque
  * procedural generators
  * lazy lists based on generators or promises
  * mutable boxes
  * ephemerons
  * comparator type class
* Already voted on.
  See [RedEdition](RedEdition.md).

# Tangerine Docket (numerics)
* Quick summary:
  * immutable mappings, hash-based and ordered
  * regular expressions
  * accumulators (inverse of generators)
  * six kinds of integer division
  * bitwise integer ops
  * fixnum library
  * flonum library
  * homogeneous vectors of various types (bitvectors also provide
    bitwise ops)
  * combinator formatting
  
* Already voted on.
  See [TangerineEdition](TangerineEdition.md).

# Hypnos Docket (SRFIs)

Transducers: [SRFI 171](https://srfi.schemers.org/srfi-171/srfi-171.html)

Multiple values: [SRFI 210](https://srfi.schemers.org/srfi-210/srfi-210.html)

Nested `define`:  [SRFI 219](https://srfi.schemers.org/srfi-219/srfi-219.html)

`cond` guards: [SRFI 61](https://srfi.schemers.org/srfi-61/srfi-61.html)

`receive`: [SRFI 8](https://srfi.schemers.org/srfi-8/srfi-8.html)

`rec`: [SRFI 31](https://srfi.schemers.org/srfi-31/srfi-31.html)

`Cut/cute`:  [SRFI 26](https://srfi.schemers.org/srfi-26/srfi-26.html)

Curried procedures: [SRFI 232](https://srfi.schemers.org/srfi-232/srfi-232.html)

Binary search: [SRFI 223](https://srfi.schemers.org/srfi-223/srfi-223.html)

Maybe/Either: [SRFI 189](https://srfi.schemers.org/srfi-189/srfi-189.html)

Environment monad: [SRFI 165](https://srfi.schemers.org/srfi-165/srfi-165.html)

Flexvectors: [SRFI 214](https://srfi.schemers.org/srfi-214/srfi-214.html)

Unifiable boxes: [SRFI 161](https://srfi.schemers.org/srfi-161/srfi-161.html)

Sorted sets: [SRFI 153](https://srfi.schemers.org/srfi-153/srfi-153.html)


# Orange Docket (SRFIs)

## Numeric types, operations, and data structures

Random numbers: [SRFI 27](https://srfi.schemers.org/srfi-27/srfi-27.html),
plus [SRFI 194](https://srfi.schemers.org/srfi-194/srfi-194.html)

Integer sets:  [SRFI 217](https://srfi.schemers.org/srfi-217/srfi-217.html)

Integer mappings:  [SRFI 224](https://srfi.schemers.org/srfi-224/srfi-224.html)

Ranges:  [SRFI 196](https://srfi.schemers.org/srfi-196/srfi-196.html)

Homogeneous vectors: [SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html)

Bitvectors: [SRFI 178](https://srfi.schemers.org/srfi-178/srfi-178.html)

Bytestrings (lexical syntax: [SRFI 207](https://srfi.schemers.org/srfi-207/srfi-207.html)

Multidimensional arrays: [SRFI 231](https://srfi.schemers.org/srfi-231/srfi-231.html) 
or [SRFI 164](https://srfi.schemers.org/srfi-164/srfi-164.html)

## Other

Enumerations: [SRFI 209](https://srfi.schemers.org/srfi-209/srfi-209.html)

Compound objects: [SRFI 222](https://srfi.schemers.org/srfi-222/srfi-222.html)

Yet more generator extensions: [SRFI 162](https://srfi.schemers.org/srfi-162/srfi-162.html)

Coroutine generators: [SRFI 190](https://srfi.schemers.org/srfi-190/srfi-190.html)

Generic combinator procedures: [SRFI 235](https://srfi.schemers.org/srfi-235/srfi-235.html)

Generic dictionary operations: [SRFI 225](https://srfi.schemers.org/srfi-225/srfi-225.html)

Generic sequence operations: [SequencesCowan](SequencesCowan.md)

Generic set/bag operations: [SetsCowan](SetsCowan.md)

INI files: [SRFI 233](https://srfi.schemers.org/srfi-233/srfi-233.html)

# Eos Docket (not SRFIs, implementations available)

Matching: Withdrawn [SRFI 204](https://srfi.schemers.org/srfi-204/srfi-204.html) bis (eliminate non-portable match clauses)

Port operations: [PortOperationsCowan](PortOperationsCowan.md) [Implementation](https://gitlab.com/dpk/presrfis/-/blob/master/io-utils/port-operations.scm)

Modify macros: [Srfi17ExtensionsCowan](Srfi17ExtensionsCowan.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/Srfi17ExtensionsCowan)

Loops: [SRFI 42](https://srfi.schemers.org/srfi-42/srfi-42) or [Riastradh's foof-loop](http://mumble.net/~campbell/scheme/foof-loop.txt) or [Chibi loop](http://synthcode.com/scheme/chibi/lib/chibi/loop.html), Implementation: all.  Summarized at [EagerComprehensions](EagerComprehensions.md)

Assumptions/Assertions/Warnings: [SRFI 145](https://srfi.schemers.org/srfi-145/srfi-145.html)
and
[AssertionsWarnings](https://github.com/johnwcowan/r7rs-work/blob/master/AssertionsWarnings.md), [Implementation](https://github.com/arvyy/r7rs-work/tree/master/AssertionsWarnings).
or [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.14).
[R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_750), R6RS with optional message and irritants plus R6RS warnings

`while`, `until`, `do-times`: [SimpleIterationCL](SimpleIterationCL.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/SimpleIterationCL).

`let-list`, `let-vector`: [MacrosAlexandria](MacrosAlexandria.md). [Partial Implementation](https://github.com/arvyy/r7rs-work/tree/master/MacrosAlexandria)

`if*` with arbitrarily many arguments: [Daphne Preston-Kendal's rationale](http://dpk.io/r7rs/naryif-20130406). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/IfKendal)

`and-let*`: [SRFI 2 using define-macro](https://srfi.schemers.org/srfi-2/srfi-2.html), [using syntax-rules](http://okmij.org/ftp/Scheme/lib/myenv-chez.scm)
[using explicit renaming](https://code.call-cc.org/cgi-bin/gitweb.cgi?p=chicken-core.git;a=blob;f=chicken-syntax.scm;h=e3c2feb47c1437d44aefa0d879e04dc28a0cbc61;hb=HEAD),
[using syntax-case](http://git.savannah.gnu.org/cgit/guile.git/tree/module/ice-9/and-let-star.scm)), or
[SRFI 202 using syntax-case](https://srfi.schemers.org/srfi-202/srfi-202.html)

Testing: [SRFI 64](https://srfi.schemers.org/srfi-64/srfi-64.html)
or [ChibiChickenTest](http://wiki.call-cc.org/eggref/5/test)
or [SRFI 78](https://srfi.schemers.org/srfi-78/srfi-78.html).  Implementations: all.

String interpolation: [StringInterpolateCowan](StringInterpolateCowan.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/StringInterpolateCowan)

Pathname objects: [PathnamesPython](PathnamesPython.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/PathnamesPython)

Skip lists:  No writeup yet.  [Implementation](https://mumble.net/~campbell/scheme/skip-list.scm)

# Urania Docket (not SRFIs, no implementations)

JSON: [SRFI 180](https://srfi.schemers.org/srfi-180/srfi-180.html),
`(schemepunk json)` [code](https://github.com/ar-nelson/schemepunk#schemepunk-json)
and [data](https://github.com/ar-nelson/schemepunk/blob/master/json.sld), or
trivial `(json-read `*stream* [ *max-chars* ]`)` and `(json-write `*stream obj*`)`.

Trees: [Trees](https://github.com/pre-srfi/trees/blob/master/spec.md).  [Implementation](https://github.com/pre-srfi/trees/tree/master).

Enumeration maps: [EnumMappingsCowan](EnumMappingsCowan.md)

Twinjo I/O: [Twinjo](https://github.com/s-expressions/twinjo/blob/master/spec/Twinjo.md) + 
[Twinjo procedures](https://github.com/s-expressions/twinjo/blob/master/spec/TwinjoLib.md) +
[Lisp Serialization Conventions](https://docs.google.com/spreadsheets/d/1V-7E5d3fLON5DrVeHkVvp9h5SRgcteOgnPl8KvWTA3M/edit#gid=0)

Levenshtein distance: [LevenshteinDistanceGauche](https://practical-scheme.net/gauche/man/gauche-refe/Levenshtein-edit-distance.html#Levenshtein-edit-distance)

Bimaps: [Bimaps](Bimaps.md)

List mutation: [ListSurgeryCowan](ListSurgeryCowan.md)

Language tag searching: [BCP 47](https://tools.ietf.org/html/bcp47).  Procedures take a tag and a dictionary of tagged objects, and return either an alist (ordered choice) or a single key-value pair.

Relations: [RelationsCowan](RelationsCowan.md)

Combinations: [CombinationsGauche](https://htmlpreview.github.io/?https://github.com/johnwcowan/r7rs-work/blob/master/CombinationsGauche.html)

URI objects: [UrisCowan](UrisCowan.md)

Unicode character database: [UcdCowan](UcdCowan.md), [AdvancedUcdCowan](AdvancedUcdCowan.md)

Environment: [SRFI 112](https://srfi.schemers.org/srfi-112/srfi-112.html) with [MiscEnvironmentSchudy](MiscEnvironmentSchudy.md)

CSV, DSV: [DataFormatsCowan](DataFormatsCowan.md)

Binary heap: [BinaryHeapsCowan](BinaryHeapsCowan.md)

Dijkstra arrays (deques) [DijkstraArrays](DijkstraArrays.md)

Immutable vectors: [FectorsPrice](https://github.com/ijp/fectors)


# Selene Docket (not SRFIs, no implementations)

`Record-let`: [#45](https://small.r7rs.org/ticket/45/)

Property lists to bindings: [LetSettingsKendal](LetSettingsKendal.md)

C-style structures: [ByteStructuresTaylanUB](https://github.com/TaylanUB/scheme-bytestructures), Implementation.
[StructuresCowan](StructuresCowan.md)

Predicate generic functions: [GenericsChibi](http://synthcode.com/scheme/chibi/lib/chibi/generic.html) (needs extension for subtyping), [FastGeneric](http://wiki.call-cc.org/eggref/5/fast-generic) Implementations: all.

Streaming regular expressions: [PragmaticParsingBaker](http://home.pipeline.com/~hbaker1/Prag-Parse.html)

CLI utilities: [SRFI 37](https://srfi.schemers.org/srfi-37/srfi-37.html),
[Optimism](http://wiki.call-cc.org/eggref/5/optimism)
[ArgsChicken](http://wiki.call-cc.org/eggref/5/args),
[AppChibi](http://synthcode.com/scheme/chibi/lib/chibi/app.html) +
[ConfigChibi](http://synthcode.com/scheme/chibi/lib/chibi/config.html),
[ArgParsePython](https://docs.python.org/3/library/argparse.html).
Implementation: all.


# Pan Docket (not SRFIs, no implementations).

Timespecs: 
Date and time arithmetic: [TimeAdvancedCowan](TimeAdvancedCowan.md),
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html)

Date-time parser: [Hato date parser](https://code.google.com/p/hato/source/browse/hato-date.scm),
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html)

Date-time formatter: [TimeFormattingCowan](https://github.com/johnwcowan/r7rs-work/blob/master/TimeFormattingCowan.md),
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html), other libs

Channels: [PigeonHolesChicken](http://wiki.call-cc.org/eggref/5/pigeon-hole),
[GochanChicken](http://wiki.call-cc.org/eggref/5/gochan).  Implementations: all.

Memoization: [Memoize](Memoize.md) (not a proposal yet), [Racket](http://planet.racket-lang.org/display.ss?package=memoize.plt&owner=dherman), [Haskell](http://hackage.haskell.org/package/memoize-0.1/docs/Data-Function-Memoize.html)

Message digests: [MessageDigests](MessageDigests.md)

Monads, applicative functors, and functors:  [ContextsCowan](ContextsCowan.md)

Character-cell terminals: [TerminalsCowan](TerminalsCowan.md)

# String library

Strings: [SRFI 152](https://srfi.schemers.org/srfi-152/srfi-152.html) (index-based),
[SRFI 130](https://srfi.schemers.org/srfi-130/srfi-130.html) (cursor-based), [pre-SRFI 135x](https://htmlpreview.github.io/?https://raw.githubusercontent.com/johnwcowan/r7rs-work/master/srfi-135x.html)
