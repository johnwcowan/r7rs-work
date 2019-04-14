For other dockets see [WG2Dockets](WG2Dockets.md).

# Red Docket (data structures)

Already voted on.
See [RedEdition](RedEdition.md).

# Tangerine Docket (numerics)

Already voted on.
See [TangerineEdition](TangerineEdition.md).

# Orange Docket

**Numeric types and operations**

Random numbers: [SRFI 27](http://srfi.schemers.org/srfi-27/srfi-27.html),
plus [AdvancedRandomGauche](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/AdvancedRandomGauche.html)
or [RandomnessCommonLisp](RandomnessCommonLisp.md)
or [RandomnessElf](https://regmedia.co.uk/2018/10/01/roig_paper.pdf)

Natural number predicates (from WG1): `exact-positive-integer?` and `exact-non-negative-integer?`.

Float and NaN dissector API (sign, quiet/signaling status, and integer tag): [FloatCLMedernach](FloatCLMedernach.md)

R6RS versions of `real?`, `rational?`, `integer?`: these are false if the imaginary part is an inexact zero; the issue here is what to name them to avoid confusion

Binary numeric I/O: [BinaryIoCowan](BinaryIoCowan.md)

Combinator sublibrary: [SRFI 162](http://srfi.schemers.org/srfi-162/srfi-162.html)

Logistic functions: [LogisticRiastradh](LogisticRiastradh.md)

**Numeric and semi-numeric data structures**

C-style structs:  [ByteStructuresTaylanub](https://github.com/TaylanUB/scheme-bytestructures).

Integer sets:  [IntegerSetsCowan](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/IntegerSetsCowan.html)

Descriptive statistics:  [TallyCowan](TallyCowan.md)

Ranges:  [RangesCowan](RangesCowan.md)

Bit and boolean vectors: [Bvectors.html]
(https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/Bvectors.html)

Bytestrings: [BytestringsCowan](BytestringsCowan.md)

**Enumerations**

Enumerations: [EnumsCowan](EnumsCowan.md)

Enumeration sets and maps: [EnumContainersCowan](EnumContainersCowan.md)

**Other**

Combinations: [CombinationsGauche](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/CombinationsGauche.html)

Number to string with Unicode: [NumberStringUnicode](NumberStringUnicode.md)

# Amber Docket (syntax)

Low-level macros:
[explicit renaming](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Explicit-Renaming.html),
[implicit renaming](https://wiki.call-cc.org/explicit-renaming-macros#implicit-renaming-macros),
[syntactic closures](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Syntactic-Closures.html),
[R6RS syntax-case](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html),
[SRFI 72](http://srfi.schemers.org/srfi-72/srfi-72.html).

More syntax-rules extensions: [SRFI 149](http://srfi.schemers.org/srfi-149/srfi-149.html)

Custom macro transformers: [SRFI 147](http://srfi.schemers.org/srfi-147/srfi-147.html) [not portable]

Syntax parameters: [SRFI 139](http://srfi.schemers.org/srfi-139/srfi-139.html)

`cond` guards: [SRFI 61](http://srfi.schemers.org/srfi-61/srfi-61.html)

`and-let*`: [SRFI 2](http://srfi.schemers.org/srfi-2/srfi-2.html)

Generalized `set!`: [SRFI 17](http://srfi.schemers.org/srfi-17/srfi-17.html), possibly plus [Srfi17ExtensionsCowan](Srfi17ExtensionsCowan.md)

`receive`: [SRFI 8](http://srfi.schemers.org/srfi-8/srfi-8.html)

`rec`: [SRFI 31](http://srfi.schemers.org/srfi-31/srfi-31.html)

`Cut/cute`:  [SRFI 26](http://srfi.schemers.org/srfi-26/srfi-26.html)

Loops: [SRFI 42](http://srfi.schemers.org/srfi-42/srfi-42) or [Riastradh's foof-loop](http://mumble.net/~campbell/scheme/foof-loop.txt) or [Chibi loop](http://synthcode.com/scheme/chibi/lib/chibi/loop.html), summarized at [EagerComprehensions](EagerComprehensions.md)

Generic accessors/mutators: [SRFI 123](http://srfi.schemers.org/srfi-123/srfi-123.html)

Custom macro transformers: [SRFI 147](http://srfi.schemers.org/srfi-147/srfi-147.html)

Syntax-rules extensions: [SRFI 149](http://srfi.schemers.org/srfi-149/srfi-149.html)

Assumptions: [SRFI 145](http://srfi.schemers.org/srfi-145/srfi-145.html)

`let` extensions: [SRFI 5](http://srfi.schemers.org/srfi-5/srfi-5.html)

Eager syntax rules: [SRFI 148](http://srfi.schemers.org/srfi-148/srfi-148.html)

Syntax-rules extensions: [SRFI 149](http://srfi.schemers.org/srfi-149/srfi-149.html)

Syntax combiners for binary functions: [SRFI 156](https://srfi.schemers.org/srfi-156/srfi-156.html)
# Yellow Docket (syntax)

Matching:  [(chibi match)](http://snow-fort.org/s/gmail.com/alexshinn/chibi/match/0.7.3/index.html)

Combinators:  [CombinatorsCowan](CombinatorsCowan.md)

Lambda*: [BeyondCurryingHemann](BeyondCurryingHemann.md)

Named parameters:  [SRFI 89](http://srfi.schemers.org/srfi-89/srfi-89.html)  
or [(chibi optional)](http://snow-fort.org/s/gmail.com/alexshinn/chibi/optional/0.7.3/index.html)

Conditions: [ConditionsCowan](ConditionsCowan.md)

Restarts:  [RestartsCowan](RestartsCowan.md)

Multiple values passed through => in `cond`: see #90

Optional arguments (other than by `case-lambda`): [OptionalsRiastradh](http://mumble.net/~campbell/proposals/optional.text)  
or [(chibi optional)](http://snow-fort.org/s/gmail.com/alexshinn/chibi/optional/0.7.3/index.html)

`Record-let`: #45

`if` with arbitrarily many arguments: [David Kendal's rationale](http://dpk.io/r7rs/naryif-20130406)

Lexical macros: [LexmacsCowan](LexmacsCowan.md)

# Green Docket (non-portable)

File I/O: [FilesAdvancedCowan](FilesAdvancedCowan.md) plus [SettingsListsCowan](SettingsListsCowan.md)
and maybe [LetSettingsKendal](LetSettingsKendal.md)

Threads: [SRFI 18](http://srfi.schemers.org/srfi-18/srfi-18.html)
or [FuturesCowan](FuturesCowan.md) (simplified)

Real-time threads: [SRFI 21](http://srfi.schemers.org/srfi-21/srfi-21.html)

Sockets: [SRFI 106](http://srfi.schemers.org/srfi-106/srfi-106.html)
or [NetworkPortsCowan](NetworkPortsCowan.md) with [NetworkEndpointsCowan](NetworkEndpointsCowan.md)

Datagram channels (UDP sockets): [DatagramChannelsCowan](DatagramChannelsCowan.md)

Timers: [SRFI 120](http://srfi.schemers.org/srfi-120/srfi-120.html)

Mutable environments: [MutableEnvironmentsCurtisCowan](MutableEnvironmentsCurtisCowan.md)

Simple POSIX: [SimplePosixCowan](SimplePosixCowan.md)

Access to the REPL: [ReplCowan](ReplCowan.md)

Library declarations: [LibraryDeclarationsCowan](LibraryDeclarationsCowan.md)

Interfaces: [InterfacesCowan](InterfacesCowan.md)

Process ports: [ProcessPortsCowan](ProcessPortsCowan.md)

File system directories (reading):
[SCSH directory stream interface](http://www.scsh.net/docu/html/man-Z-H-4.html#node_sec_3.3),
[DirectoriesCowan](DirectoriesCowan.md),
`directory-files` to return a list of all files in the dir (in WG1 vote order)

System commands: [SystemCommandCowan](SystemCommandCowan.md)

Pure delay/force: [PureDelayedGloria](PureDelayedGloria.md)

Finalizers: [FinalizersCowan](FinalizersCowan.md)

Delimited continuations: [Racket](https://docs.racket-lang.org/reference/cont.html),
[Guile](https://www.gnu.org/software/guile/manual/html_node/Prompt-Primitives.html),
[Scheme48/Kali](https://github.com/tonyg/kali-scheme/blob/master/scheme/misc/shift-reset.scm),
[Gauche](https://practical-scheme.net/gauche/man/gauche-refe/Partial-continuations.html),
[Chicken](http://wiki.call-cc.org/eggref/4/F-operator)

Continuation marks:  [SRFI 157](http://srfi.schemers.org/srfi-157/srfi-157.html)

Extended exact numbers: [SRFI 73](http://srfi.schemers.org/srfi-73/srfi-73.html)
or [ExtendedRationalsCowan](ExtendedRationalsCowan.md)

Adjustable strings: [SRFI 118](http://srfi.schemers.org/srfi-118/srfi-118.html) (basic)
or [SRFI 140](http://srfi.schemers.org/srfi-140/srfi-140.html) (mutable/immutable)

Mutable environments for `eval`:  [MutableEnvironmentsCurtisCowan](MutableEnvironmentsCurtisCowan.md)

Mutexes, condition variables: [SRFI 18](http://srfi.schemers.org/srfi-18/srfi-18.html)

Port type detector: see ticket #177

Applicable record instances: [R6RS formal comment](http://www.r6rs.org/formal-comments/comment-6.txt)

Internationalization of strings: [GettextCowan](GettextCowan.md)

Standard condition predicates [StandardConditionPredicates](StandardConditionPredicates.md)

Chronometers: [Chronometer](Chronometer.md)

# Aqua Docket (portable but complex things).

Port operations: [PortOperationsCowan](PortOperationsCowan.md)

Time types: [SRFI 19](http://srfi.schemers.org/srfi-19/srfi-19.html) and/or [TimeAdvancedCowan](TimeAdvancedCowan.md) with [TimePeriodsCowan](TimePeriodsCowan.md)

Character conversion: [CharacterConversionCowan](CharacterConversionCowan.md)

Parallel promises: [ParallelPromisesCowan](ParallelPromisesCowan.md)

Pathname objects: [PathnamesPython](PathnamesPython.md)

URI objects: [UrisGauche](http://practical-scheme.net/gauche/man/gauche-refe/URI-parsing-and-construction.html#URI-parsing-and-construction)  
or Chicken [uri-generic](http://wiki.call-cc.org/eggref/5/uri-generic) +  
[uri-common](http://wiki.call-cc.org/eggref/5/uri-common).

Unicode character database: [UcdCowan](UcdCowan.md), [AdvancedUcdCowan](AdvancedUcdCowan.md)

Environment: [SRFI 112](http://srfi.schemers.org/srfi-112/srfi-112.html) with [MiscEnvironmentSchudy](MiscEnvironmentSchudy.md)

Trees: [TreesCowan](TreesCowan.md)

JSON, CSV, DSV: [DataFormatsCowan](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/JsoCowan.html)

Unicode string normalization: [StringNormalizationCowan](StringNormalizationCowan.md)

Binary heap: [BinaryHeapsCowan](BinaryHeapsCowan.md)

Date and time arithmetic: [TimeAdvancedCowan](TimeAdvancedCowan.md) plus [TimePeriodsCowan](TimePeriodsCowan.md),
[SRFI 19](http://srfi.schemers.org/srfi-19/srfi-19.html)

Date-time parser: [Hato date parser](https://code.google.com/p/hato/source/browse/hato-date.scm), [SRFI 19](http://srfi.schemers.org/srfi-19/srfi-19.html)

Regular expressions over s-expressions:  [SerexPalmer](http://inamidst.com/lisp/serex)

Procedural record types: [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html#node_sec_6.3)
[SRFI 99](http://srfi.schemers.org/srfi/srfi-99.html),
[AnonymousRecordsCowan](AnonymousRecordsCowan.md),
[RidiculouslySimpleRecordsCowan](RecordsCowan.md)

# Blue Docket (portable but advanced things).

Compound objects: [CompoundObjectsCowan](CompoundObjectsCowan.md)

Memoization: [Memoize](Memoize.md) (not a proposal yet), [Racket](http://planet.racket-lang.org/display.ss?package=memoize.plt&owner=dherman), [Haskell](http://hackage.haskell.org/package/memoize-0.1/docs/Data-Function-Memoize.html)

Message digests (CRC, MD5, SHA1, SHA2):

Assertions: [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_750), R6RS with optional message and irritants.

Predicate generic functions: [GenericsChibi](http://synthcode.com/scheme/chibi/lib/chibi/generic.html) (needs extension for subtyping)

Maybe/Either: [MaybeEither](MaybeEither.md)

Monad etc.:  [ContextsCowan](ContextsCowan.md)

String normalization [StringNormalization](StringNormalization.md)

Unique types: [UniqueTypesCowan](UniqueTypesCowan.md)

Testing: [SRFI 64](http://srfi.schemers.org/srfi-64/srfi-64.html)
or [ChibiChickenTest](http://wiki.call-cc.org/eggref/5/test)
or [SRFI 78](http://srfi.schemers.org/srfi-78/srfi-78.html)

Command-line arguments: [SRFI 37](http://srfi.schemers.org/srfi-37/srfi-37.html)
or [ArgsChicken](http://wiki.call-cc.org/eggref/4/args)

Unique types: [SRFI 137](http://srfi.schemers.org/srfi-137/srfi-137.html)

Unifiable boxes: [SRFI 161](http://srfi.schemers.org/srfi-161/srfi-161.html)

Channels: [PigeonHolesChicken](http://wiki.call-cc.org/eggref/5/pigeon-hole),
[GochanChicken](http://wiki.call-cc.org/eggref/5/gochan)

Immutable vectors: [FectorsPrice](https://github.com/ijp/fectors)

First-class dynamic extents: [SRFI 154](http://srfi.schemers.org/srfi-154/srfi-154.html)

Promises: [SRFI 155](http://srfi.schemers.org/srfi-155/srfi-155.html)



# Indigo Docket (stuff of dubious utility)

Substitute/transform: [SubstituteCowan](SubstituteCowan.md)

More multiple values: [MultipleValuesCowan](MultipleValuesCowan.md)

[MiscAlexandria](MiscAlexandria.md)

R6RS compatibility: whole libraries or cherry-picked procedures

Custom I/O ports: [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html)

Macro expander(s) available at run time:

Association lists: [AssociationListsCowan](AssociationListsCowan.md)

Edit buffers: [BuffersCowan](BuffersCowan.md)

Immutable cycles: [CyclesMedernach](CyclesMedernach.md)

Packages and rich symbols: [PackageSymbolsCowan](PackageSymbolsCowan.md)

Doubly linked lists: [DoublyLinkedListsCowan](DoublyLinkedListsCowan.md)

Prime number library: [PrimesGauche](https://htmlpreview.github.io/?https://bitbucket.org/cowan/r7rs-wg1-infra/raw/default/PrimesGauche.html)

μXml: [MicroXMLCowan](MicroXmlCowan.md)

