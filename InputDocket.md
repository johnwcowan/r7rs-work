## WG2 Input Docket

This docket contains work items that need to be voted on for possible inclusion into R7RS-large.  See [WG2Dockets](WG2Dockets.md) for other dockets.

Bidirectional hash maps:

Binary I/O helpers: procedures relevant to reading and writing low-level binary formats, such as
* 8/16/32/64 bit signed/unsigned integers in little-endian/big-endian/network-endian order
* null terminated ASCII strings
* non-byte-aligned bit fields
* seeking to arbitrary positions
* reading/writing packed representations of Scheme records
(see [SRFI 56](http://srfi.schemers.org/srfi-56/srfi-56.html))

Custom I/O ports: [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html)

Default exception handler controls:

Features, allowing libraries to add:

Greek lambda syntax: define syntax equivalent to "lambda" whose identifier is a single Unicode U+03BB character (http://www.fileformat.info/info/unicode/char/3bb/index.htm)

Identifier-syntax: [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.19)

R6RS versions of `real?`, `rational?`, `integer?`: these are false if the imaginary part is an inexact zero; the issue here is what to name them to avoid confusion

Interrupts, timers, signals:

Jiffy cleanup: [Arcane Sentiment blog post](http://arcanesentiment.blogspot.com/2012/07/current-jiffy-is-not-usable-portably.html)

Macro expander(s) available at run time:

Maximal and minimal elements of lists and vectors:

`number->string` variant with control for significant digits: [Vincent Manis proposal](http://lists.scheme-reports.org/pipermail/scheme-reports/2011-May/000709.html), R6RS

Observable objects: [Java](http://docs.oracle.com/javase/6/docs/api/java/util/Observable.html#clearChanged())

Port type detector: see #177

R6RS compatibility: whole libraries or cherry-picked procedures:

Record external representations: [SRFI 108](http://srfi.schemers.org/srfi-108/srfi-108.html)

`standard-*-port` routines:  [R6RS](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-9.html)

Tree library: at least `tree=?` and `tree-copy`, probably Common Lisp [SUBLIS and NSUBLIS](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_sublis.htm), fold, unfold, map ...

Thread-local storage:

Unquote with multiple arguments:  see #123

User-specified syntax-transformers:

Convenience package(s) for commonly desired sets of bindings:
* (scheme r7rs-small) - all bindings in the small language
* (scheme r7rs-red) - the Red Edition's bindings

