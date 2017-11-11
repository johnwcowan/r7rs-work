The WG1 charter says "Self consistency is an important objective, which may require adding new features."  Scheme has three sequence types: lists, strings, and vectors, but the support for them is not consistent.  Lists have the most support, strings and vectors have much less, and inconsistently so.  I propose providing the following procedures, some of which are R5RS, some R6RS, some in various SRFIs, and some new:

|Type of procedure|Lists|Strings|Vectors|
|Basic constructor|`make-list` (SRFI 1)|`make-string`|`make-vector`|
|Variadic constructor|`list`|`string`|`vector`|
|Copy constructor|`list-copy` (SRFI 1)|`string-copy`|`vector-copy` (SRFI 43)|
|Mutating copier|`list-copy!` (SRFI 1)|`string-copy!` (SRFI 13)|`vector-copy!` (SRFI 43)|
|Basic predicate|`list?`|`string?`|`vector?`|
|Sequence length|`length`|`string-length`|`vector-length`|
|Element access|`list-ref`|`string-ref`|`vector-ref`|
|Element mutator|`list-set!` (proposed)|`string-set!`|`vector-set!`|
|Map function|`map`|`string-map` (SRFI 13 extended)|`vector-map` (R6RS)|
|Map side effects|`for-each`|`string-for-each` (proposed)|`vector-for-each` (R6RS)|
|Append|`append`|`string-append`|`vector-append` (SRFI 43)|
|Fill|---|`string-fill!`|`vector-fill!`|
|Convert to list|---|`string->list`|`vector->list`|
|Convert to string|`list->string`|---|`vector->string` (proposed)|
|Convert to vector|`list->vector`|`string->vector` (proposed)|---|


In summary: 45 procedures, 27 in R5RS, 2 in R6RS, 2 in SRFI 1, 2 in SRFI 43, 1 in SRFI-13 but extended to take multiple strings, 4 novel but obvious.

I further propose that the "map function" and "map side effects" groups should be specified to implicitly truncate all sequence arguments to the length of the shortest sequence.
