I (John Cowan) have extracted the following formal and informal objections to draft 9 of R7RS-small from votes cast in the R7RS-small plebiscite (and a few other email messages), and filed a ticket for each: the tickets are linked via their numbers.

Almost all of the tickets take the following form: an attributed quotation from one or more ballots or emails (I have consolidated the complaints into a single ticket if I thought they were substantially the same), followed by the names of people who substantially agreed but did not seem worth quoting, followed by my comments.  When my comments are factual in nature (by intent), they appear as part of the description; if they involve personal opinions, they appear as ticket comments.  In all cases the ticket titles are my own responsibility. 

* #478: Procedure equivalence should return.
* #479: Character names like 'xbeef' can be read two ways
* #480: Mapper output can share storage with input(s)
* #481: Draft is gratuitously incompatible with R6RS
* #482: Module system is pointlessly inflexible
* #483: Top-level semantics unspecified
* #484: The draft avoids making hard decisions
* #485: Many decisions were based on a tacit requirement to ignore R6RS
* #486: The draft is a step backwards from R6RS
* #487: The draft is not true to the spirit of Scheme
* #488: The draft is a move away from unity
* #489: The draft has little to offer over R5RS
* #490: The draft has little to offer over R6RS
* #491: Library versions, a pedagogically important feature, are not provided
* #492: Assertions are not provided
* #493: Full Unicode support is not portable
* #494: Including/loading doesn't belong in Scheme
* #495: The draft lacks many important libraries
* #496: Catastrophic failure should not be acceptable
* #497: The Scheme community is broken
* #498: Does the world need a better R5RS?
* #499: The draft is less expressive than (rnrs base 6)
* #500: "The ""stack winding dance"" of guard clauses"
* #501: Mutable strings are still provided
* #502: Call/cc should go
* #503: Setters should be allowed to return zero values
* #504: The treatment of sequences is still flawed
* #505: Internal definitions other than by define are problematic
* #506: Process issue: Late changes were not reviewable
* #507: R6RS/R7RS incompatibility of bytevector-copy! procedure
* #508: Library syntax should have been a subset of R6RS
* #509: Problematic interactions between dynamic-wind and exceptions
* #510: SRFI 9 records shouldn't be universal
* #511: Parameters shouldn't be in the core
* #512: Parameters should be used instead of proliferating names
* #513: Error should have been R6RS-compatible
* #514: New module syntax is ugly and unnecessary
* #515: Cond-expand doesn't scale and is hard to use
* #516: The lack of a fully-featured macro system requires sub-par constructs
* #517: The draft doesn't support user extensibility
* #518: The R7RS-small text should have been founded on the R6RS text
* #519: Toy Schemes shouldn't be the measure of what belongs in the standard
* #520: A library with no export declarations should export everything
* #521: Make library contents be at top level
* #522: WG1 was forbidden to remove restrictions
* #523: R7RS-large should have preceded R7RS-small
* #524: The draft's text-handling procedures are wrong in a Unicode world
* #525: Adding Unicode is too big a change from R5RS
* #526: #true and #false considered gratuitous
* #527: Block comment syntax considered unsightly
* #528: #!(no-)fold-case considered ugly
* #529: Read-line permits DoS attacks
* #530: The draft is too conservative in its changes to R5RS
* #531: Eq? should not be called an equivalence predicate
* #532: Delaying multiple values shouldn't be undefined
* #533: Define-library doesn't bind a name
* #534: Include at the library level should include declarations, not code
* #535: Libraries should have at most one begin
* #536: R6RS and R7RS libraries should be made closer
* #537: Minimal interface to sockets, concurrency, and hierarchical file systems
* #538: Falling off a cond-expand should be unspecified behavior

Here is an author index, specifying which tickets each person's ballot or email is referred to in.  See PlebisciteIndex for links to the actual ballots.  Links to non-ballots are inside the tickets.

* Barzilay: #485
* Bex: #525, #526, #527, #528, #529
* Bothner: #507, #508
* Clinger: #531
* Dillinger: #502, #522, #523, #524
* Heidkamp: #502
* Hsu: #503, #510, #512, #513, #514, #515, #516, #517, #518, #519
* Kato: #486
* Manis: #478
* Medernach: #503, #509, #510, #511
* Montague: #533, #534, #535, #536
* More: #520, #521
* Piitulainen: #478, #504, #505, #538
* Radul: #479, #480
* Smyles: #506
* Snell-Pym: #503
* Sperber: #486
* Stone: #491, #492, #493, #494, #495
* Sussman: #478
* Tobin-Hochstadt: #481, #482, #483, #484
* Unknown: #530
* Watson: #478
* Weinholt: #496, #497
* Wingo: #498, #499, #500, #501, #502, #503
* Wise: #486, #487, #488, #489, #490
* Wittenberger: #532
* Wortman: #537