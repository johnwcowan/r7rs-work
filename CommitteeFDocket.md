For other dockets see [WG2Dockets](WG2Dockets.md).

## Already in [FASCICLES](https://dpk.io/temp/FASCICLES)

Syntax-case: [R6RS syntax-case](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html)
 
Identifier syntax: [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_796)

Syntax parameters: [SRFI 139](https://srfi.schemers.org/srfi-139/srfi-139.html)

Splicing `let(rec)-syntax`: [SRFI 188](https://srfi.schemers.org/srfi-188/srfi-188.html)

Identifier aliasing: [SRFI 212](https://srfi.schemers.org/srfi-212/srfi-212.html)

Identifier properties: [SRFI 213](https://srfi.schemers.org/srfi-213/srfi-213.html)

## Lexical syntax

Homogeneous vectors (base, lexical syntax): [SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html)

Bitvectors (base, lexical syntax): [SRFI 178](https://srfi.schemers.org/srfi-178/srfi-178.html)

Bytestrings (lexical syntax): [SRFI 207](https://srfi.schemers.org/srfi-207/srfi-207.html)

Multidimensional arrays (base, lexical syntax): [SRFI 179](https://srfi.schemers.org/srfi-179/srfi-179.html) 
or [SRFI 164](https://srfi.schemers.org/srfi-164/srfi-164.html)

Compound objects: [SRFI 222](https://srfi.schemers.org/srfi-222/srfi-222.html)

tAssumptions/Assertions/Warnings: [SRFI 145](https://srfi.schemers.org/srfi-145/srfi-145.html)
and
[AssertionsWarnings](https://github.com/johnwcowan/r7rs-work/blob/master/AssertionsWarnings.md), [Implementation](https://github.com/arvyy/r7rs-work/tree/master/AssertionsWarnings).
or [R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.14).
[R6RS](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_750), R6RS with optional message and irritants plus R6RS warnings

Guardians: Chez

R4RS macro system ("syntax-case without `syntax-case`"): [R4RS](https://people.csail.mit.edu/jaffer/r4rs_12.html#SEC77); see also [SRFI 211](https://people.csail.mit.edu/jaffer/r4rs_12.html#SEC77).

Syntax-rules extensions: [SRFI 149](https://srfi.schemers.org/srfi-149/srfi-149.html)

Binding auxiliary syntax: [SRFI 206](https://srfi.schemers.org/srfi-206/srfi-206.html)

Core syntactic extensions: [SRFI 201](https://srfi.schemers.org/srfi-201/srfi-201.html)

Continuation marks:  [SRFI 226](https://srfi.schemers.org/srfi-226/srfi-226.html) (comprehensive) or [SRFI 157](https://srfi.schemers.org/srfi-157/srfi-157.html)

Extended exact numbers: [SRFI 73](https://srfi.schemers.org/srfi-73/srfi-73.html)
or [ExtendedRationalsCowan](ExtendedRationalsCowan.md)

Mutable environments: [EnvironmentsMIT](https://htmlpreview.github.io/?https://github.com/johnwcowan/r7rs-work/blob/master/EnvironmentsMIT.html)

Library declarations: [LibraryDeclarationsCowan](LibraryDeclarationsCowan.md)

Interfaces: [InterfacesCowan](InterfacesCowan.md)

Delimited continuations: [SRFI 226](https://srfi.schemers.org/srfi-226/srfi-226.html) (comprehensive) or [Racket](https://docs.racket-lang.org/reference/cont.html),
[Guile](https://www.gnu.org/software/guile/manual/html_node/Prompt-Primitives.html),
[Scheme48/Kali](https://github.com/tonyg/kali-scheme/blob/master/scheme/misc/shift-reset.scm),
[Gauche](https://practical-scheme.net/gauche/man/gauche-refe/Partial-continuations.html),
[Chicken](http://wiki.call-cc.org/eggref/4/F-operator)

Multiple-language support: [MultipleLanguages](MultipleLanguages.md)

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

Parallel promises: [ParallelPromisesCowan](ParallelPromisesCowan.md).  Portable implementation in pre-SRFI.

Channels: [PigeonHolesChicken](http://wiki.call-cc.org/eggref/5/pigeon-hole),
[GochanChicken](http://wiki.call-cc.org/eggref/5/gochan).  Implementations: all.

Applicable record instances: [R6RS formal comment](http://www.r6rs.org/formal-comments/comment-6.txt)

Adjustable-size strings: portable [SRFI 185](https://srfi.schemers.org/srfi-185/srfi-185.html)
or easily retrofittable [SRFI 118](https://srfi.schemers.org/srfi-118/srfi-118.html)]
