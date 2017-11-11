## WG2 Standard Docket

This is a list of work items that WG2 has voted to work on, and proposals for those work items.  See [WG2Dockets](WG2Dockets.md) for other dockets.

Applicable record instances: [R6RS formal comment](http://www.r6rs.org/formal-comments/comment-6.txt)

Assertions: [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_750), R6RS with optional message and irritants.

Binary heap: [BinaryHeapsCowan](BinaryHeapsCowan.md)

Container conversion: See [BytevectorsCowan](BytevectorsCowan.md), [NumericVectorsCowan](NumericVectorsCowan.md), and [WG1 ballot options](http://trac.sacrideo.us/wg/wiki/WG1Ballot6Results#a433fullconversioncycleforcontainers)

Continuation API:  [Feeley's paper](http://repository.readscheme.org/ftp/papers/sw2001/feeley.pdf) (PDF)

Date and time arithmetic: [TimeAdvancedCowan](TimeAdvancedCowan.md) plus [TimePeriodsCowan](TimePeriodsCowan.md), [SRFI 19](http://srfi.schemers.org/srfi-19/srfi-19.html)

Date-time parser: [Hato date parser](https://code.google.com/p/hato/source/browse/hato-date.scm), [SRFI 19](http://srfi.schemers.org/srfi-19/srfi-19.html)

Evaluator arguments to procedures like `load`: see #277

File system directories (reading): [SCSH directory stream interface](http://www.scsh.net/docu/html/man-Z-H-4.html#node_sec_3.3), [DirectoryPortsCowan](DirectoryPortsCowan.md), `directory-files` to return a list of all files in the dir (in WG1 vote order) [Yellow]

File system directories (creation, removal): [DirectoriesCowan](DirectoriesCowan.md)

Futures:  [Racket API](http://docs.racket-lang.org/reference/futures.html)

`if` with arbitrarily many arguments: [David Kendal's rationale](http://dpk.io/r7rs/naryif-20130406)

Library declarations: [LibraryDeclarationsCowan](LibraryDeclarationsCowan.md)

Mailboxes, channels, synchronized queues: [Scheme48 from Concurrent ML](http://www.s48.org/1.9/manual/manual-Z-H-8.html#node_sec_7.8)

`make-error-object`: constructs error object without raising it

Memoization: [wiki:Memoize] (not a proposal yet), [Racket](http://planet.racket-lang.org/display.ss?package=memoize.plt&owner=dherman), [Haskell](http://hackage.haskell.org/package/memoize-0.1/docs/Data-Function-Memoize.html)

Message digests (CRC, MD5, SHA1, SHA2):

Multiple values passed through => in `cond`: see #90

Mutable environments for `eval`:  [MutableEnvironmentsCurtisCowan](MutableEnvironmentsCurtisCowan.md)

Mutexes, condition variables: [SRFI 18](http://srfi.schemers.org/srfi-18/srfi-18.html)

Optional arguments (other than by `case-lambda`): [OptionalsRiastradh](http://mumble.net/~campbell/proposals/optional.text)

Procedure arity inspection: [SRFI 102](http://srfi.schemers.org/srfi-102/srfi-102.html), [Dybvig's proposal](http://srfi.schemers.org/srfi-102/mail-archive/msg00011.html)

Processes (system, popen, etc): [SystemCommandCowan](SystemCommandCowan.md) + [ProcessPortsCowan](ProcessPortsCowan.md)

Raw strings:  <<, """...""", [SRFI 109](http://srfi.schemers.org/srfi-109/srfi-109.html)

Record introspection: [SRFI 99](http://srfi.schemers.org/srfi-99/srfi-99.html)

`Record-let`: #45

REPL facilities: [ReplCowan](ReplCowan.md)

Shift and reset: [Scheme48](https://github.com/tonyg/pgg/blob/master/shift-reset.scm), [Racket](http://docs.racket-lang.org/reference/cont.html)

Symbol library:  [SymbolsCowan](SymbolsCowan.md)

`syntax-case` (voted down, but restored by popular demand): [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html)

TCP protocol: [NetworkPortsCowan](NetworkPortsCowan.md)

Thread-local storage: [Java](http://docs.oracle.com/javase/6/docs/api/java/lang/ThreadLocal.html)

Threads:  [ThreadsCowan](ThreadsCowan.md), [SRFI 18](http://srfi.schemers.org/srfi-18/srfi-18.html)

UDP protocol:  [DatagramChannelsCowan](DatagramChannelsCowan.md)

Undefined value API: see #49

Unicode character database: [UcdCowan](UcdCowan.md)

Unicode normalization: [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-2.html#node_idx_58), `string-ni=?` and friends (from earlier R7RS-small drafts).
