== WG2 Standard Docket ==

This is a list of work items that WG2 has voted to work on, and proposals for those work items.  See [[:WG2Dockets|WG2Dockets]] for other dockets.

Applicable record instances: [[http://www.r6rs.org/formal-comments/comment-6.txt|R6RS formal comment]]

Assertions: [[http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_750|R6RS]], R6RS with optional message and irritants.

Binary heap: BinaryHeapsCowan

Container conversion: See BytevectorsCowan, NumericVectorsCowan, and [[http://trac.sacrideo.us/wg/wiki/WG1Ballot6Results#a433fullconversioncycleforcontainers|WG1 ballot options]]

Continuation API:  [[http://repository.readscheme.org/ftp/papers/sw2001/feeley.pdf|Feeley's paper]] (PDF)

Date and time arithmetic: TimeAdvancedCowan plus TimePeriodsCowan, [[http://srfi.schemers.org/srfi-19/srfi-19.html|SRFI 19]]

Date-time parser: [[https://code.google.com/p/hato/source/browse/hato-date.scm|Hato date parser]], [[http://srfi.schemers.org/srfi-19/srfi-19.html|SRFI 19]]

Evaluator arguments to procedures like `load`: see #277

File system directories (reading): [[http://www.scsh.net/docu/html/man-Z-H-4.html#node_sec_3.3|SCSH directory stream interface]], DirectoryPortsCowan, `directory-files` to return a list of all files in the dir (in WG1 vote order) [Yellow]

File system directories (creation, removal): DirectoriesCowan

Futures:  [[http://docs.racket-lang.org/reference/futures.html|Racket API]]

`if` with arbitrarily many arguments: [[http://dpk.io/r7rs/naryif-20130406|David Kendal's rationale]]

Library declarations: LibraryDeclarationsCowan

Mailboxes, channels, synchronized queues: [[http://www.s48.org/1.9/manual/manual-Z-H-8.html#node_sec_7.8|Scheme48 from Concurrent ML]]

`make-error-object`: constructs error object without raising it

Memoization: [wiki:Memoize] (not a proposal yet), [[http://planet.racket-lang.org/display.ss?package=memoize.plt&owner=dherman|Racket]], [[http://hackage.haskell.org/package/memoize-0.1/docs/Data-Function-Memoize.html|Haskell]]

Message digests (CRC, MD5, SHA1, SHA2):

Multiple values passed through => in `cond`: see #90 

Mutable environments for `eval`:  MutableEnvironmentsCurtisCowan

Mutexes, condition variables: [[http://srfi.schemers.org/srfi-18/srfi-18.html|SRFI 18]]

Optional arguments (other than by `case-lambda`): [[http://mumble.net/~campbell/proposals/optional.text|OptionalsRiastradh]]

Procedure arity inspection: [[http://srfi.schemers.org/srfi-102/srfi-102.html|SRFI 102]], [[http://srfi.schemers.org/srfi-102/mail-archive/msg00011.html|Dybvig's proposal]]

Processes (system, popen, etc): SystemCommandCowan + ProcessPortsCowan

Raw strings:  <<, """...""", [[http://srfi.schemers.org/srfi-109/srfi-109.html|SRFI 109]]

Record introspection: [[http://srfi.schemers.org/srfi-99/srfi-99.html|SRFI 99]]

`Record-let`: #45

REPL facilities: ReplCowan

Shift and reset: [[https://github.com/tonyg/pgg/blob/master/shift-reset.scm|Scheme48]], [[http://docs.racket-lang.org/reference/cont.html|Racket]]

Symbol library:  SymbolsCowan

`syntax-case` (voted down, but restored by popular demand): [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html|R6RS]]

TCP protocol: NetworkPortsCowan

Thread-local storage: [[http://docs.oracle.com/javase/6/docs/api/java/lang/ThreadLocal.html|Java]]

Threads:  ThreadsCowan, [[http://srfi.schemers.org/srfi-18/srfi-18.html|SRFI 18]]

UDP protocol:  DatagramChannelsCowan

Undefined value API: see #49

Unicode character database: UcdCowan

Unicode normalization: [[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-2.html#node_idx_58|R6RS]], `string-ni=?` and friends (from earlier R7RS-small drafts).
