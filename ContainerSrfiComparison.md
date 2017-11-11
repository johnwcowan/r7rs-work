This is a face-to-face comparison of three container SRFIs, [SRFI 1](http://srfi.schemers.org/srfi-1/srfi-1.html) for lists, [SRFI 13](http://srfi.schemers.org/srfi-13/srfi-13.html) for strings, and [SRFI 43](http://srfi.schemers.org/srfi-43/srfi-43.html) for vectors.  Procedures present in the small language have mostly been removed unless they have corresponding procedures in other SRFIs that aren't part of the small language, as I assume that they won't be separately part of the large language.  The order of functions is basically the order of SRFI-1, the most comprehensive library: the functions are grouped as constructors, predicates, selectors, miscellaneous, filtering, partitioning, searching, and deleting.

The following sections and subsections are omitted here because they aren't likely to have counterparts in other containers:

* association lists (SRFI 1)
* set operations on lists (SRFI 1)
* trimming and padding (SRFI 13)
* alphabetic case mapping (SRFI 13)
* low-level procedures (SRFI 13)

|Function|List library|String library|Vector library|
|||
|Specified elements plus existing container|`cons*`|-|-|
|Elements constructed by applying proc to integers|`list-tabulate`|`string-tabulate`|-|
|Elements are integers|`iota`|-|-|
|Copy container in reverse element order|-|-|`vector-reverse-copy`|
|||
|Empty container predicate|`null?`|`string-null?`|`vector-empty?`|
|Elementwise equality|`list=`|-|`vector=`|
|Does one container subsume another?|-|`string-contains`(`-ci`)|-|
|Specified element|`first` .. `tenth`|-|-|
|||
|Initial elements|`take`(`!`)|`string-take`|-|
|All but initial elements|`drop`|`string-drop`|-|
|Final elements|`take-right`(`!`)|`string-take-right`|-|
|All but final elements|`drop-right`|`string-drop-right`|-|
|Initial and final elements|`split-at`(`!`)|-|-|
|Last element|`last`|-|-|
|||
|Inequality comparison|-|`string`(`-ci`)`-<>`|-|
|Comparison|-|`string-compare`(`-ci`)|-|
|Hashing|-|`string-hash`(`-ci`)|-|
|Prefix equality|-|`string-prefix`(`-ci`)`?`|-|
|Suffix equality|-|`string-suffix`(`-ci`)`?`|-|
|Shared prefix length|-|`string-prefix-length`(`-ci`)|-|
|Shared suffix length|-|`string-suffix-length`(`-ci`)|-|
|||
|Concatenate containers in a list|`concatenate`(`!`)|`string-concatenate`|`vector-concatenate`|
|Concatenate with separator|-|`string-join`|-|
|Concatenate in reverse|-|`string-concatenate-reverse`|-|
|||
|Reverse container|`reverse`(`!`)|`string-reverse`(`!`)|`vector-reverse!`|
|Append reversed container|`append-reverse`(`!`)|-|-|
|Zip up input to make container of containers|`zip`|-|-|
|Unzip container of containers|`unzip1` .. `unzip5`|-|-|
|Swap elements|-|-|`vector-swap!`|
|Reverse conversion|-|`reverse-list->string`|-|
|Reverse conversion|-|`reverse-vector->list`|-|
|Reverse conversion|-|`reverse-list->vector`|-|
|Count of elements satisfying pred|`count`|`string-count`†|`vector-count`|
|||
|Fundamental deconstructor|`fold`|`string-fold`|`vector-fold`|
|Fundamental deconstructor from right|`fold-right`|`string-fold-right`|`vector-fold-right`|
|Fundamental constructor|`unfold`|`string-unfold`|`vector-unfold`|
|Fundamental constructor from right|`unfold-right`|`string-unfold-right`|`vector-unfold-right`|
|Simplified constructor|`reduce`|-|-|
|Simplified constructor from right|`reduce-right`|-|-|
|Append results of mapping|`append-map`(`!`)|-|-|
|Linear-update map|`map!` |`string-map!` |`vector-map!`|
|Map saving only true values|`filter-map`|-|-|
|Map guaranteeing order|`map-in-order`|-|-|
|For-each with element and index|-|`string-for-each-index`|-|
|||
|Replicated copy|-|`xsubstring`|-|
|Linear-update replicated copy|-|`string-xcopy!`|-|
|||
|Container with elements matching pred|`filter`(`!`)|`string-filter`†|-|
|Two containers with elements matching and not|`partition`(`!`)|-|-|
|Container with elements not matching pred|`remove`(`!`)|`string-delete`†|-|
|||
|Find first element satisfying pred|`find`|-|-|
|Return true if any elements satisfy|`any`|`string-any`†|`vector-any`|
|Return true if every element satisfies|`every`|`string-every`†|`vector-every`|
|Index of first element satisfying pred|`list-index`|`string-index`†|`vector-index`|
|Index of last element satisfying pred|-|`string-index-right`†|`vector-index-right`|
|Find first element not satisfying pred|-|`string-skip`†|`vector-skip`|
|Find last element not satisfying pred|-|`string-skip-right`†|`vector-skip-right`|
|Initial elements satisfying pred|`take-while`(`!`)|-|-|
|Final elements satisfying pred|`drop-while`|-|-|
|Initial elements satisfying pred plus final ones|`span`(`!`)|-|-|
|Initial elements not satisfying pred plus final ones|`break`(`!`)|-|-|
|Binary search|-|-|`vector-binary-search`|
|||
|Remove elements by identity|`delete`(`!`)|`string-delete`†|-|
|Remove duplicate elements|`delete-duplicates`(`!`)|-|-|

## List oddities

`xcons`, `circular-list`, `proper-list?`, `circular-list?`, `dotted-list?`, `not-pair?`, `null-list?`, `car+cdr`, `last-pair`, `append!`, `pair-fold`, `pair-fold-right`, `pair-for-each`, `find-tail`, the alist procedures, and the list-set procedures.

In SRFI 1, `length` corresponds to R5RS `length`, whereas `length+` corresponds to R7RS `length`.

## String oddities

`substring/shared`, `string-concatenate/shared`, `string-append/shared`, `string-concatenate-reverse/shared`, `string-replace`, `string-tokenize`, the trimming and padding procedures, the case mapping procedures, and the low-level procedures.

## Footnotes

†Polymorphic procedure: accepts character, character set, or predicate

## Maximalist proposal

The *maximalist proposal* is that this is for R7RS-large, and all the empty boxes under "Vector Library" should be filled with procedures.  The "List Library" and "String Library" procedures already correspond to the extent reasonable.  A proposal for those procedures can be found at [VectorsCowan](VectorsCowan.md).

In addition, provide SRFI 1 and R5RS `length` as `simple-length`.

