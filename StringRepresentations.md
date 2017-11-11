## Introduction

Historically in most older programming languages strings were simple
arrays of characters by default - there was no need for more than 8
bits per character - so R5RS and older Scheme implementations made no
specific statement to this effect, only implying it by providing the
primitives `string-ref` and `string-set!` as the primary interface to
iterating over strings.

With the advent of Unicode and other sets of characters too large to
fit into a single byte, different languages provided different
approaches to representing strings in memory.  Java adopted Unicode
early on when 2 bytes per character were enough, and with the advent
of supplementary characters, Java provided a separate UTF-16-based API,
while maintaining the existing UCS-2 API; most programs and programmers
just ignore the issue.

[SBCL uses a mixed representation](http://sbcl-internals.cliki.net/Unicode)
of flat1 and flat4 as discussed below.  The C family forked their
strings into `char*` and `wchar_t*`, typically encoding UTF-8 in the
former and UTF-16 in the latter (glibc being an exception which uses
UCS-4 for wide characters).  Neither of these offer O(1) access, but C
APIs work mostly with string pointers, rather than indices, so this
isn't a concern in practice.

R6RS added an explicit requirement to the effect that strings should
provide O(1) access on `string-ref`.  Ticket #27 raises the question
of whether we want to include this requirement in WG1.  Effectively,
it's a matter of whether straightforward implementations based on
encodings such as UTF-8 and UTF-16 should be allowed.

*The question of which string representation to use does NOT simply boil down to one of a time-space trade-off.*
Some of the alternate O(1) time access strategies are more compact
than UTF-8 when using non-ASCII text, and UTF-8 has properties that
make it faster than the alternatives for many tasks.  We need to
consider the full range of uses of strings in Scheme, the trade-offs
involved, and decide whether non-constant time representations are worth
supporting.

## Representative Representations

The following representations are adapted from
https://trac.ccs.neu.edu/trac/larceny/wiki/StringRepresentations, with
several additions:

* flat1 - A packed array of characters with 1 byte per character.
> Works for ASCII, ISO-8859-* or any other 8-bit encoding, but not
> for Unicode.  The only option listed without Unicode support,
> included for comparison purposes only.

* flat4 - A packed array of characters with 4 bytes per character.
> In the Unicode case this is also called UCS-4.  Simple and allows
> the fastest random access, it also uses the most memory.  A
> variation used in Larceny is to store boxed characters, which
> saves a single shift instruction on `string-ref`.

* flat3 - A packed array of characters with 3 bytes per character.
> An encoding Scheme similar to this is used by Emacs (and SBCL?).
> Although more compact than flat4, usage is complicated by the
> fact that most processors can't directly access 3 bytes at a
> time, or even 4 bytes on unaligned boundaries.

* flat21 - Similar to flat3, we encode a bit-vector with 21 bits
> per character, i.e. roughly 2.5 bytes instead of the 3 required
> for flat3, at the expense of some additional instructions
> required for decoding.

* record4 - A record with a slot for a flat4.  This allows
> dynamically changing the size of the string, or with the inclusion
> of offset/length slots allows for shared substrings.

* record2 - A record with a slot for an array of characters with 2
> bytes per character, plus a table for supplementary characters
> requiring more than 2 bytes.

* record1 - A record with a slot for an array of characters with 1
> byte per character, plus a table for supplementary characters
> requiring more than 1 byte.

* utf16 - A byte-vector representing the string encoded in the
> UTF-16 format.  Useful for inter-operating with foreign libraries
> that use UTF-16, and the most compact format when working with
> non-ASCII text, it does not provide O(1) access on `string-ref`.

* utf8 - A byte-vector representing the string encoded in the UTF-8
> format.  Useful for inter-operating with foreign libraries that use
> UTF-8, and when working with file and network encodings which are
> predominantly UTF-8.  It's the most compact format when working
> with mostly ASCII text, but does not provide O(1) access on
> `string-ref`.

## Record Representations

The record2 and record1 representations are not commonly known and
require further explanation.  A record2 is a record that initially has a
slot holding a UCS-2 encoded array as follows:

```
  +--------+
  | header |
  |        |
  | base   |--- | 0xNNNN | 0xNNNN | ... | 0xNNNN |   (2*N bytes)
  |        |
  | suppl  |--- #f
  +--------+
```

`string-ref` and `string-set!` use this array as normal with O(1) access and
a compact 2 bytes per character.  The first time a supplementary
character is inserted, we allocate the supplementary array:

```
  +--------+
  | header |
  |        |
  | base   |--- | 0xNNNN | 0xD800-0xDBFF | ... | 0xNNNN |   (2*N bytes)
  |        |
  | suppl  |--- |        | 0xDC00-0xDFFF | ... |        |   (2*N bytes)
  +--------+
```

Here the first surrogate pair goes in the base array, and the second in
the supplementary array.  When `string-ref` sees a surrogate pair in the
base array, it fetches the high surrogate from the same index in the
suppl array.  This preserves guaranteed O(1) access.

The record1 case follows the same idea, but stores 7-bit ASCII in the
base array.

```
  +--------+
  | header |
  |        |
  | base   |--- | 0xNN | 0xNN | ... | 0xNN |   (N bytes)
  |        |
  | suppl  |--- #f
  +--------+
```

If a non-ASCII character is inserted, we set the high bit in the base
character, and take the lower 5 bits from the base character plus 16
bits from the suppl array to cover the 21 bits needed for Unicode.

```
  +--------+
  | header |
  |        |
  | base   |--- | 0xNN | 0b1000xxxx    | ... | 0xNN |   (N bytes)
  |        |
  | suppl  |--- |      | 0xNNNN 0xNNNN | ... |      |   (2*N bytes)
  +--------+
```

This again guarantees O(1) access.

## Mixed Representations

One optimization strategy is to use different types of strings
internally, so that an ASCII-only string would be a record1, but instead
of using a supplementary table convert it to a record2 if a non-ASCII
character is inserted, or a record4 if a supplementary character is
inserted.  This maintains O(1) access and makes efficient use of memory.
The disadvantage is the overhead of dispatching on the string types, and
the normalization problem - the same string may exist as both a record1
and record2, but to verify it is string=? you need to convert one to the
other.  Conversion is a very expensive string operation, yet depending
on the program the number of such on-the-fly conversions is unbounded.
The strings could be kept normalized, but then the same problem occurs
for string-ci=?, and other operations that need to simultaneously
traverse both strings in sequence.

## Optimizations

One obvious optimization is to set a flag as to whether or not the
string has non-ASCII chars in the utf8 case, or supplementary chars in
the utf16 case, and use the O(1) fast-path when possible.  This would
not be reliable for utf8, but for utf16 would give O(1) access a high
percentage of the time.  Unfortunately, in the uncommon case where a
supplementary character turns up you would experience an O(n)
slowdown, making your algorithms unreliable.

Also with utf8 and utf16 it is possible to cache the offset of the
last index accessed (or some constant number thereof) for O(1) access
in common cases.  This adds a little overhead, does not help with all
cases, and `string-set!` may still need to resize the string for O(n)
time.  The resizing is unlikely in the case of utf16, but can be
assumed to be relatively common with utf8.

A more general extension of this caching would be to use a balanced
tree (either generated up front or on demand) mapping indexes to
offsets into the utf8 or utf16 byte-vectors, for O(lg(n)) access.
This could be kept to a certain granularity, only recording the
offsets for the beginning index of each chunk of some size, thus
keeping the tree structure to a manageable size.  In particular, in
the utf16 tree case, the index would only need to keep track of ranges
with supplementary characters, which would make if O(1) in practice
with graceful degradation.  The overhead of the tree-traversal,
however, is unlikely to be inlined by compilers making `string-ref` a
utility function, and on `string-set!` the tree would likely need to
be regenerated.

The record implementations suffer from nasty surprising growth in
space when their base arrays aren't sufficient.  It's possible to
alleviate this by using hash-tables or tree lookups for the non-base
characters, adding overhead and making decoding the non-base case
likely too complicated for inlining.

## Space Efficiency

The following table gives the running times of the string accessors and
the space usage for an all ASCII string versus a string of all non-ASCII
text.  N is the number of characters in the string, and h is "header"
overhead of a single heap object.

| representation | `string-ref` | `string-set!` | ASCII    | non-ASCII  | growth  |
| flat1          | O(1)       | O(1)        | N+h      | --         | --      |
| flat3          | O(1)       | O(1)        | 3*N+h    | 3*N+h      | --      |
| flat21         | O(1)       | O(1)        | 2.625*N+h | 2.625*N+h | --      |
| flat4          | O(1)       | O(1)        | 4*N+h    | 4*N+h      | --      |
| record1        | O(1)       | O(1)        | N+2*h    | 3*N+3*h    | x3      |
| record2        | O(1)       | O(1)        | 2*N+2*h  | 4*N+3*h    | x2      |
| record4        | O(1)       | O(1)        | 4*N+2*h  | 4*N+2*h    | --      |
| utf16          | O(n)       | O(n)        | 2*N+h    | 2*N+h      | gradual |
| utf8           | O(n)       | O(n)        | N+h      | 3*N+h      | gradual |
| utf8 + tree    | O(lg(n))   | O(n)        | N+lg(N)h | 3*N+lg(N)h | gradual |
| utf16 + tree   | O(1)       | O(n)        | 2*N+2*h  | 2*N+2*h    | gradual |

For the non-ASCII column we assume an Asian script with a small
constant number of supplementary characters.  This requires 3 bytes
for utf8 - European scripts would be in between the ASCII and
non-ASCII columns for utf8 (requiring an average of just under 2 bytes
per char), and Cuneiform would require a full 2x the space for utf16
and 4x for utf8.  So the non-ASCII column is in a sense the worst-case
commonly expected case.  If, however, you were working exclusively
with Cuneiform you would experience worst behavior, so this technique
should be avoided if you're afraid of ancient Pharaohs rising from the
dead to seek vengeance on you.

The growth column refers to the change in growth as the characters
change from ASCII to non-ASCII.  For record1 and record2 there is a
jump, tripling or doubling in size the first time a wider character is
inserted.  For utf8 and utf16 there is a gradual pay-as-you-go increase
on each character.

## Multi-Processing

The question arises in a multi-processing environment of whether locking
is required for any operation that may re-allocate or move memory,
i.e. `string-set!` for any representation other than flat4.  The alternate
opinion is that locking is the programmer's responsibility.  At the
moment there is no threading library being proposed for WG1, but this
should count as a potential point in favor of flat4.

## Common Algorithms and Usage Patterns

Rather than just looking at the performance of `string-ref`, we want
to consider the overall performance implications for all common string
operations.  We assume the following basic algorithms are optimized at
the primitive level, i.e. they can look under the hood and perform any
tricks needed for the particular representations.

These could all use further study, although comparative benchmarking
is difficult.

* string comparison - Basic `string=?` and other comparisons
> should be as fast as possible.  The fastest implementations will
> be those with simple data structures compacted into the smallest
> contiguous block of memory, so either utf8 or utf16 win here
> depending on the distribution of characters.  Multiple potential
> string representations fair the worst here, unless strings are
> always kept normalized.

* string search - A more general comparison, we often want to be
> able to search for one string inside another.  In practice tight
> simple loops can beat algorithmically faster approaches here.
> Considering Boyer-Moore or KMP string searches, the inner loop
> consists of a character table lookup, however for the full range
> of Unicode such a character lookup can't be reasonably done in
> constant time - the best bet is a tree with O(lg(m)) in the
> inner loop, or a hash-table with constant expected case bu O(m)
> worst case.  However, with UTF-8 it's possible to search at the
> byte level, and any match is still valid because UTF-8
> substrings can't be shifted to invalid boundaries (that is to
> say strstr(3) is a perfectly legitimate way to search for one
> UTF-8 string inside another).  A byte lookup table is small and
> fast, so UTF-8 technically comes out asymptotically fastest
> here.  There are various constant factors involved though, so
> more extensive benchmarking is in order.

* regular expressions - Modern regular expressions are
> tremendously complicated, and have many trade-offs, but when you
> absolutely need speed you want a DFA.  Thus this is what most
> grep implementations use because they may be matching against
> gigabytes of data.  For similar reasons to the string search
> case (which is just a special case regular expression) the inner
> loop wants fast table lookups, so once again UTF-8 matching
> bytes will be the fastest approach.  This is why Google's latest
> RE library works with UTF-8, using the same byte-level tricks
> [IrRegex](IrRegex.md) and presumably other libraries have been using for
> years.

* parsing - Here you only parse strings sequentially, and are
> often likely to use a string port, so random access is not an
> issue.  The only thing that really matters is the overhead of
> reading a single character, for which flat4 clearly wins.
> Moreover, if the parsing loop has many separate reads and peeks
> (as is common), the overhead of inlining any especially
> complicated character decoding will result in code bloat.  On
> the other hand, UTF-8 can generally be parsed a single byte at a
> time much faster with the right API.

* rendering - Probably benefits most from the same implementations
> that do parsing well, the actual rendering is likely to dwarf
> any string processing overhead.

* string concatenation - Here you either want the most compact
> implementation, or an implementation that allows sharing
> substrings.

## Alternate APIs

Need to fill these out in more detail, though this is likely more of a
WG2 task, and even then only if we don't require O(1) access time.

* string pointers (as in C)

* string cursor objects (as in http://mumble.net/~campbell/proposals/new-text.txt)

* string ports

* shared substrings

* higher-order string APIs

* sub-codepoint level access
