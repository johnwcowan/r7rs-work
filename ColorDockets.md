For other dockets see [WG2Dockets](WG2Dockets.md).

Free docket names: Bia Dike Gaia Leto Nyx Orion Xenos Zephyrus; Kronos used but dropped.


# Docket Index

The following order is not necessarily the voting order.

 * [Red: data structures](#user-content-red-docket-data-structures)
 * [Tangerine: numerics](#user-content-tangerine-docket-numerics)
 * [Yellow: macros, SRFIs](#user-content-yellow-docket-macros-srfis)
 * [Hypnos: portable, SRFIs](#user-content-hypnos-docket-portable-srfis)
 * [Orange: portable, SRFIs](#user-content-orange-docket-portable-srfis)
 * [Urania: portable, not SRFIs, no implementations](#user-content-urania-docket-portable-not-srfis-no-implementation-yet)
 * [Eos: portable, not SRFIs](#user-content-eos-docket-portable-not-srfis)
 * [Morpheus: not portable, SRFIs](#user-content-morpheus-docket-not-portable-srfis)
 * [Ares: not portable, SRFIs](#user-content-ares-docket-not-portable-srfis)
 * [Selene: portable, not SRFIs](#user-content-selene-docket-portable-not-srfis)
 * [Pan: portable, not SRFIs](#user-content-pan-docket-portable-not-srfis)
 * [Indigo: features of dubious utility](#user-content-indigo-docket-stuff-of-dubious-utility)

# Red Docket (data structures)

* Quick summary:
  * comprehensive list library
  * comprehensive vector library
  * sorting
  * sets (including character sets) and bags
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

# Yellow Docket (macros)

Multiple values: [SRFI 210](https://srfi.schemers.org/srfi-210/srfi-210.html)

Nested `define`:  [SRFI 219](https://srfi.schemers.org/srfi-219/srfi-219.html)

Syntax-case: [R6RS syntax-case](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html)
 
Rejected: Explicit renaming: [simple explicit renaming](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Explicit-Renaming.html#Explicit-Renaming) [explicit renaming interoperable with syntax-case](https://srfi.schemers.org/srfi-211/srfi-211.html)

Identifier syntax: [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_796)

Syntax parameters: [SRFI 139](https://srfi.schemers.org/srfi-139/srfi-139.html)

Splicing `let(rec)-syntax`: [SRFI 188](https://srfi.schemers.org/srfi-188/srfi-188.html)

Identifier aliasing: [SRFI 212](https://srfi.schemers.org/srfi-212/srfi-212.html)

Identifier properties: [SRFI 213](https://srfi.schemers.org/srfi-213/srfi-213.html)

## Syntax-rules macros

`cond` guards: [SRFI 61](https://srfi.schemers.org/srfi-61/srfi-61.html)

`receive`: [SRFI 8](https://srfi.schemers.org/srfi-8/srfi-8.html)
fsr
`rec`: [SRFI 31](https://srfi.schemers.org/srfi-31/srfi-31.html)

`Cut/cute`:  [SRFI 26](https://srfi.schemers.org/srfi-26/srfi-26.html)

Rejected: `let` extensions: [SRFI 5](https://srfi.schemers.org/srfi-5/srfi-5.html)

Rejected: Syntax combiners for binary functions: [SRFI 156](https://srfi.schemers.org/srfi-156/srfi-156.html)

# Hypnos Docket (portable, SRFIs)

Binary search: [SRFI 223](https://srfi.schemers.org/srfi-223/srfi-223.html)

Maybe/Either: [SRFI 189](https://srfi.schemers.org/srfi-189/srfi-189.html)

Environment monad: [SRFI 165](https://srfi.schemers.org/srfi-165/srfi-165.html)

ASCII character library: [SRFI 175](https://srfi.schemers.org/srfi-175/srfi-175.html)

Flexvectors: [SRFI 214](https://srfi.schemers.org/srfi-214/srfi-214.html)

Unifiable boxes: [SRFI 161](https://srfi.schemers.org/srfi-161/srfi-161.html)

Formatting: [SRFI 166](https://srfi.schemers.org/srfi-166/srfi-166.html) (combinators),
[SRFI 48](https://srfi.schemers.org/srfi-48/srfi-48.html) (format strings).

JSON: [SRFI 180](https://srfi.schemers.org/srfi-180/srfi-180.html),
`(schemepunk json)` [code](https://github.com/ar-nelson/schemepunk#schemepunk-json)
and [data](https://github.com/ar-nelson/schemepunk/blob/master/json.sld).

Hooks: [SRFI 173](https://srfi.schemers.org/srfi-173/srfi-173.html)

# Orange Docket (portable, SRFIs)

## Numeric types, operations, and data structures

Random numbers: [SRFI 27](https://srfi.schemers.org/srfi-27/srfi-27.html),
plus [SRFI 194](https://srfi.schemers.org/srfi-194/srfi-194.html)

Comparator sublibraries: [SRFI 162](https://srfi.schemers.org/srfi-162/srfi-162.html) plus
[SRFI 228](https://srfi.schemers.org/srfi-228/srfi-228.html)
(integrated into `(scheme comparators)`

Integer sets:  [SRFI 217](https://srfi.schemers.org/srfi-217/srfi-217.html)

Integer mappings:  [SRFI 224](https://srfi.schemers.org/srfi-224/srfi-224.html)

Ranges:  [SRFI 196](https://srfi.schemers.org/srfi-196/srfi-196.html)

Homogeneous vectors: [SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html)

Bitvectors: [SRFI 178](https://srfi.schemers.org/srfi-178/srfi-178.html)

Bytestrings: [SRFI 207](https://srfi.schemers.org/srfi-207/srfi-207.html)

Multidimensional arrays: [SRFI 179](https://srfi.schemers.org/srfi-179/srfi-179.html) 
or [SRFI 164](https://srfi.schemers.org/srfi-164/srfi-164.html)

ASCII character library: [SRFI 175](https://srfi.schemers.org/srfi-175/srfi-175.html)

## Other

Enumerations: [SRFI 209](https://srfi.schemers.org/srfi-209/srfi-209.html)

Compound objects: [SRFI 222](https://srfi.schemers.org/srfi-222/srfi-222.html)

Yet more generator extensions: [SRFI 162](https://srfi.schemers.org/srfi-162/srfi-162.html)

Coroutine generators: [SRFI 190](https://srfi.schemers.org/srfi-190/srfi-190.html)

Generic dictionary operations: [SRFI 225](https://srfi.schemers.org/srfi-225/srfi-225.html)

# Urania Docket (portable, not SRFIs, no implementations)

Lexical macros: [LexmacsCowan](LexmacsCowan.md)

List mutation: [ListSurgeryCowan](ListSurgeryCowan.md)

Enumeration maps: [EnumMappingsCowan](EnumMappingsCowan.md)

Twinjo I/O: [Twinjo](https://github.com/s-expressions/twinjo/blob/master/spec/Twinjo.md) + 
[Twinjo procedures](https://github.com/s-expressions/twinjo/blob/master/spec/TwinjoLib.md) +
[Lisp Serialization Conventions](https://docs.google.com/spreadsheets/d/1V-7E5d3fLON5DrVeHkVvp9h5SRgcteOgnPl8KvWTA3M/edit#gid=0)

Port operations: [PortOperationsCowan](PortOperationsCowan.md)

Lists as sets (extension to SRFI 1): [LSetExtensions](LSetExtensions.md)

[StructuresCowan](StructuresCowan.md)

Levenshtein distance: [LevenshteinDistanceGauche](https://practical-scheme.net/gauche/man/gauche-refe/Levenshtein-edit-distance.html#Levenshtein-edit-distance)

Sorted dictionaries: [SortedDictionaries](SortedDictionaries.md)

Bimaps: [Bimaps](Bimaps.md)

Language tag searching: [BCP 47](https://tools.ietf.org/html/bcp47).  Procedures take a tag and a dictionary of tagged objects, and return either an alist (ordered choice) or a single key-value pair.

Pure delay/force: [PureDelayedGloria](PureDelayedGloria.md)

Relations: [RelationsCowan](RelationsCowan.md)

Combinations: [CombinationsGauche](https://htmlpreview.github.io/?https://github.com/johnwcowan/r7rs-work/blob/master/CombinationsGauche.html)

Time types: [SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html) and/or [TimeAdvancedCowan](TimeAdvancedCowan.md) with [TimePeriodsCowan](TimePeriodsCowan.md)

URI objects: [UrisCowan](UrisCowan.md)

Unicode character database: [UcdCowan](UcdCowan.md), [AdvancedUcdCowan](AdvancedUcdCowan.md)

Environment: [SRFI 112](https://srfi.schemers.org/srfi-112/srfi-112.html) with [MiscEnvironmentSchudy](MiscEnvironmentSchudy.md)

CSV, DSV: [DataFormatsCowan](DataFormatsCowan.md)

Binary heap: [BinaryHeapsCowan](BinaryHeapsCowan.md)

Writing filters: [FileFilterGauche](https://practical-scheme.net/gauche/man/gauche-refe/Filtering-file-content.html#Filtering-file-content)

SXML serializer: [SxmlSerializerGauche](https://practical-scheme.net/gauche/man/gauche-refe/Serializing-XML-and-HTML-from-SXML.html#Serializing-XML-and-HTML-from-SXML)

[Racket/Gerbil syntax-case extensions](https://gitlab.com/dpk/presrfis/-/blob/master/syntax-case-extensions.md)

Dijkstra arrays (deques) [DijkstraArrays](DijkstraArrays.md)

Immutable vectors: [FectorsPrice](https://github.com/ijp/fectors)

# Eos Docket (portable, not SRFIs, implementations available)

Generic combinator procedures: [CombinatorsCowan](CombinatorsCowan.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/CombinatorsCowan)

Modify macros: [Srfi17ExtensionsCowan](Srfi17ExtensionsCowan.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/Srfi17ExtensionsCowan)

Loops: [SRFI 42](https://srfi.schemers.org/srfi-42/srfi-42) or [Riastradh's foof-loop](http://mumble.net/~campbell/scheme/foof-loop.txt) or [Chibi loop](http://synthcode.com/scheme/chibi/lib/chibi/loop.html), Implementation: all.  Summarized at [EagerComprehensions](EagerComprehensions.md)

Assumptions/Assertions/Warnings: [SRFI 145](https://srfi.schemers.org/srfi-145/srfi-145.html)
and
[AssertionsWarnings](https://github.com/johnwcowan/r7rs-work/blob/master/AssertionsWarnings.md), [Implementation](https://github.com/arvyy/r7rs-work/tree/master/AssertionsWarnings).
or [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.14).
[R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_750), R6RS with optional message and irritants plus R6RS warnings

`while`, `until`, `do-times`: [SimpleIterationCL](SimpleIterationCL.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/SimpleIterationCL).

lambda*: [BeyondCurryingHemann](BeyondCurryingHemann.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/BeyondCurryingHemann)

`let-list`, `let-vector`: [MacrosAlexandria](MacrosAlexandria.md). [Partial Implementation](https://github.com/arvyy/r7rs-work/tree/master/MacrosAlexandria)

`if*` with arbitrarily many arguments: [Daphne Preston-Kendal's rationale](http://dpk.io/r7rs/naryif-20130406). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/IfKendal)

# Morpheus Docket (not portable, SRFIs)

Finalizers: [FinalizersCowan](FinalizersCowan.md)

R4RS macro system ("syntax-case without `syntax-case`"): [R4RS](https://people.csail.mit.edu/jaffer/r4rs_12.html#SEC77); see also [SRFI 211](https://people.csail.mit.edu/jaffer/r4rs_12.html#SEC77).

Setter functions: [SRFI 17](https://srfi.schemers.org/srfi-17/srfi-17.html)

Custom macro transformers: [SRFI 147](https://srfi.schemers.org/srfi-147/srfi-147.html)

Syntax-rules extensions: [SRFI 149](https://srfi.schemers.org/srfi-149/srfi-149.html)

Binding auxiliary syntax: [SRFI 206](https://srfi.schemers.org/srfi-206/srfi-206.html)

Core syntactic extensions: [SRFI 201](https://srfi.schemers.org/srfi-201/srfi-201.html)

`and-let*`: [SRFI 2 using define-macro](https://srfi.schemers.org/srfi-2/srfi-2.html), [using syntax-rules](http://okmij.org/ftp/Scheme/lib/myenv-chez.scm)
[using explicit renaming](https://code.call-cc.org/cgi-bin/gitweb.cgi?p=chicken-core.git;a=blob;f=chicken-syntax.scm;h=e3c2feb47c1437d44aefa0d879e04dc28a0cbc61;hb=HEAD),
[using syntax-case](http://git.savannah.gnu.org/cgit/guile.git/tree/module/ice-9/and-let-star.scm)), or
[SRFI 202 using syntax-case](https://srfi.schemers.org/srfi-202/srfi-202.html)

Continuation marks:  [SRFI 226](https://srfi.schemers.org/srfi-226/srfi-226.html) (comprehensive) or [SRFI 157](https://srfi.schemers.org/srfi-157/srfi-157.html)

Extended exact numbers: [SRFI 73](https://srfi.schemers.org/srfi-73/srfi-73.html)
or [ExtendedRationalsCowan](ExtendedRationalsCowan.md)

Posix environment: [SRFI 170](https://htmlpreview.github.io/?https://github.com/johnwcowan/r7rs-work/blob/master/srfi-170.html)

Coroutine generator syntax: [SRFI 190](https://srfi.schemers.org/srfi-190/srfi-190.html)

Timers: [SRFI 120](https://srfi.schemers.org/srfi-120/srfi-120.html)

Custom I/O ports and transcoded ports: [SRFI 181](https://srfi.schemers.org/srfi-181/srfi-181.html).

Port positioning: [SRFI 192](https://srfi.schemers.org/srfi-192/srfi-192.html).

Sockets: [SRFI 106](https://srfi.schemers.org/srfi-106/srfi-106.html)

Threads: [SRFI 18](https://srfi.schemers.org/srfi-18/srfi-18.html)
or [SRFI 226](https://srfi.schemers.org/srfi-226/srfi-226.html) (comprehensive)

# Ares Docket (not portable, not SRFIs, no implementations)

Datagram channels (UDP sockets): [DatagramChannelsCowan](DatagramChannelsCowan.md)

Mutable environments: [EnvironmentsMIT](https://htmlpreview.github.io/?https://github.com/johnwcowan/r7rs-work/blob/master/EnvironmentsMIT.html)

Access to the REPL: [ReplCowan](ReplCowan.md)

Library declarations: [LibraryDeclarationsCowan](LibraryDeclarationsCowan.md)

Interfaces: [InterfacesCowan](InterfacesCowan.md)

Process control: [ProcessesCowan](ProcessesCowan.md)

Delimited continuations: [SRFI 226](https://srfi.schemers.org/srfi-226/srfi-226.html) (comprehensive) or [Racket](https://docs.racket-lang.org/reference/cont.html),
[Guile](https://www.gnu.org/software/guile/manual/html_node/Prompt-Primitives.html),
[Scheme48/Kali](https://github.com/tonyg/kali-scheme/blob/master/scheme/misc/shift-reset.scm),
[Gauche](https://practical-scheme.net/gauche/man/gauche-refe/Partial-continuations.html),
[Chicken](http://wiki.call-cc.org/eggref/4/F-operator)

Port type detector: [#177](https://small.r7rs.org/ticket/177/)

Chronometers: [Chronometer](Chronometer.md)

Character-cell terminals: [TerminalsCowan](TerminalsCowan.md).  Portable with SRFI 205.

Graphics canvas: [GraphicsCanvas](GraphicsCanvas.md)

Multiple-language support: [MultipleLanguages](MultipleLanguages.md)

Simple SQL database interface: [SimpleSqlCowan](SimpleSqlCowan.md)

Syslog interface: [SyslogCowan](SyslogCowan.md)

# Selene Docket (portable, not SRFIs, no implementations)

`Record-let`: [#45](https://small.r7rs.org/ticket/45/)

Property lists to bindings: [LetSettingsKendal](LetSettingsKendal.md)

Optional arguments (other than by `case-lambda`):

[SRFI 227](https://srfi.schemers.org/srfi-227/srfi-227.html)
or [OptionalsRiastradh](http://mumble.net/~campbell/proposals/optional.text).
[Implementation](https://github.com/arvyy/r7rs-work/tree/master/OptionalsRiastradh),
or [(chibi optional)](http://snow-fort.org/s/gmail.com/alexshinn/chibi/optional/0.7.3/index.html). 
Multiple values passed through => in `cond`: see [#90](https://small.r7rs.org/ticket/90/)

Named parameters:  [SRFI 177](https://srfi.schemers.org/srfi-177/srfi-177.html) bis (dpk's),
[SRFI 89](https://srfi.schemers.org/srfi-89/srfi-89.html),
or [(chibi optional)](http://snow-fort.org/s/gmail.com/alexshinn/chibi/optional/0.7.3/index.html)(portable)

Matching: SRFI 204 bis

Strings: [SRFI 152](https://srfi.schemers.org/srfi-152/srfi-152.html) (index-based),
[SRFI 130](https://srfi.schemers.org/srfi-130/srfi-130.html) (cursor-based), [pre-SRFI 135x](https://htmlpreview.github.io/?https://raw.githubusercontent.com/johnwcowan/r7rs-work/master/srfi-135x.html)

String interpolation: [StringInterpolateCowan](StringInterpolateCowan.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/StringInterpolateCowan)

C-style structures: [ByteStructuresTaylanUB](https://github.com/TaylanUB/scheme-bytestructures), Implementation.
[StructuresCowan](StructuresCowan.md), see Urania

Predicate generic functions: [GenericsChibi](http://synthcode.com/scheme/chibi/lib/chibi/generic.html) (needs extension for subtyping), [FastGeneric](http://wiki.call-cc.org/eggref/5/fast-generic) Implementations: all.

Timespecs:  [SRFI 174](https://srfi.schemers.org/srfi-174/srfi-174.html) or
[SRFI 174 bis](TimeObjects.md)

Conditions: [ConditionsCowan](ConditionsCowan.md)

Topological sort: [TopologicalSortGauche](https://practical-scheme.net/gauche/man/gauche-refe/Topological-sort.html#Topological-sort). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/TopologicalSortGauche)

Streaming regular expressions: [PragmaticParsingBaker](http://home.pipeline.com/~hbaker1/Prag-Parse.html)

CLI utilities: [SRFI 37](https://srfi.schemers.org/srfi-37/srfi-37.html),
[Optimism](http://wiki.call-cc.org/eggref/5/optimism)
[ArgsChicken](http://wiki.call-cc.org/eggref/5/args),
[AppChibi](http://synthcode.com/scheme/chibi/lib/chibi/app.html) +
[ConfigChibi](http://synthcode.com/scheme/chibi/lib/chibi/config.html),
[ArgParsePython](https://docs.python.org/3/library/argparse.html).
Implementation: all.

Generic combinator procedures: [CombinatorsCowan](CombinatorsCowan.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/CombinatorsCowan)

Parallel promises: [ParallelPromisesCowan](ParallelPromisesCowan.md).  Portable implementation in pre-SRFI.

Pathname objects: [PathnamesPython](PathnamesPython.md). [Implementation](https://github.com/arvyy/r7rs-work/tree/master/PathnamesPython)

INI files: [IniFilesCowan](IniFilesCowan.md) [Implementation](https://github.com/arvyy/r7rs-work/tree/master/IniFilesCowan)

Unicode string normalization: [StringNormalizationCowan](StringNormalizationCowan.md).  R6RS implementations.

# Pan Docket (portable, not SRFIs, no implementations).

Date and time arithmetic: [TimeAdvancedCowan](TimeAdvancedCowan.md),
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html)

Date-time parser: [Hato date parser](https://code.google.com/p/hato/source/browse/hato-date.scm),
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html)

Date-time formatter: [TimeFormattingCowan](https://github.com/johnwcowan/r7rs-work/blob/master/TimeFormattingCowan.md),
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html), other libs

Regular expressions over s-expressions:  [SerexPalmer](http://inamidst.com/lisp/serex)

Procedural record types: [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html#node_sec_6.3)
[SRFI 99](https://srfi.schemers.org/srfi/srfi-99.html),
[AnonymousRecordsCowan](AnonymousRecordsCowan.md),
[RidiculouslySimpleRecordsCowan](RecordsCowan.md),  
[UniqueTypesCowan](UniqueTypesCowan.md)

Channels: [PigeonHolesChicken](http://wiki.call-cc.org/eggref/5/pigeon-hole),
[GochanChicken](http://wiki.call-cc.org/eggref/5/gochan).  Implementations: all.

Applicable record instances: [R6RS formal comment](http://www.r6rs.org/formal-comments/comment-6.txt)

Memoization: [Memoize](Memoize.md) (not a proposal yet), [Racket](http://planet.racket-lang.org/display.ss?package=memoize.plt&owner=dherman), [Haskell](http://hackage.haskell.org/package/memoize-0.1/docs/Data-Function-Memoize.html)

Message digests: [MessageDigests](MessageDigests.md)

Monads, applicative functors, and functors:  [ContextsCowan](ContextsCowan.md)

Testing: [SRFI 64](https://srfi.schemers.org/srfi-64/srfi-64.html)
or [ChibiChickenTest](http://wiki.call-cc.org/eggref/5/test)
or [SRFI 78](https://srfi.schemers.org/srfi-78/srfi-78.html).  Implementations: all.

Adjustable-size strings: portable [SRFI 185](https://srfi.schemers.org/srfi-185/srfi-185.html)
or easily retrofittable [SRFI 118](https://srfi.schemers.org/srfi-118/srfi-118.html)]

# Indigo Docket (stuff of dubious utility)

Internationalization of strings: [GettextCowan](GettextCowan.md),
[JavaScript Intl](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl).

S-expressions for SQL: [AgnosticSql](AgnosticSql.md)

Substitute/transform: [SubstituteCowan](SubstituteCowan.md)

[MiscAlexandria](MiscAlexandria.md). [Partial Implementation](https://github.com/arvyy/r7rs-work/tree/master/MacrosAlexandria)

R6RS compatibility: whole libraries or cherry-picked procedures

Macro expander(s) available at run time:

Association lists: [AssociationListsCowan](AssociationListsCowan.md)

Edit buffers: [BuffersCowan](BuffersCowan.md)

Immutable cycles: [CyclesMedernach](CyclesMedernach.md)

Packages and rich symbols: [PackageSymbolsCowan](PackageSymbolsCowan.md)

Doubly linked lists: [DoublyLinkedListsCowan](DoublyLinkedListsCowan.md)

Prime number library: [PrimesGauche](https://htmlpreview.github.io/?https://github.com/johnwcowan/r7rs-work/blob/master/PrimesGauche.html)

Descriptive statistics:  [TallyCowan](TallyCowan.md)

μXml: [MicroXMLCowan](MicroXmlCowan.md)

JavaScript objects: [JsoCowan](JsoCowan.md)

Intervals: [IntervalsCowan](IntervalsCowan.md)

Ring buffers: [RingBuffersGauche](https://practical-scheme.net/gauche/man/gauche-refe/Ring-buffer.html#Ring-buffer)

Tagbody: [Tagbody](Tagbody.md)
