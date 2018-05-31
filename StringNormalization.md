# String Normalization

## Introduction

There are four Unicode normalization forms defined in
[UAX #15](http://unicode.org/reports/tr15/), corresponding to two types
of character equivalence.

The first, and most important, is canonical equivalence.  This is
equivalence between sequences of codepoints which represent the same
abstract character.  This includes:

* pre-composed characters and their separate base+combining forms
* pre-composed Hangul and their Jamo sequences
* unified singleton characters (e.g. mapping the Ohm sign to the Greek capital Omega by which it is represented)
* canonical ordering for combining marks when multiple combining marks are used

The second character equivalence is compatibility equivalence.
Compatibility equivalence is where two characters represent the same
abstract character but differ in appearance or behavior.  Differences
include:

* Font variants - cursive, bold, mathematical
* Breaking differences - different hyphen and space types
* Circled characters
* Width, size, rotated - variations common in Japanese scripts
* Superscripts and subscripts - 2**5 becomes "2"+"5"
* Squared characters - Japanese words written in a single character square
* Fractions - representing the single 1/2 character as "1"+"/"+"2"

These two equivalences can be combined to provide four normalization
forms:

* NFD - canonical decomposition
* NFC - canonical decomposition, followed by canonical composition
* NFKD - compatibility decomposition
* NFKC - compatibility decomposition, followed by canonical composition

Order matters, since the canonical and compatibility normalizations
are not commutative.  Normalizing by performing canonical composition
followed by compatibility decomposition is not a defined form in the
standard.

Note that neither of these equivalences ignore control characters, nor
do they attempt to unify characters which look alike, such as Latin
and Cyrillic "o", so spoofing is a security issue regardless of
Unicode normalization.

## Problems with Normalization

The problem inherent in a character set as complex as Unicode is the
"same" (by any of the definitions above) character can have multiple
representations.  Thus the same conceptual word may be represented
with different sets of codepoints, and not match as a search term or
database key or filename, etc.  For example, Googling the same word
with different forms for NFC and NFKC will return a different set of
results, which makes no sense.  Since web search is inherently an
approximate art to begin with, this is perhaps not directly perceived
as a "bug" by users, though certainly their experience would be
improved if searching gave the same results.

Googling for [unicode normalization bug](http://www.google.com/search?q=unicode+normalization+bug),
on the other hand, turns up a large number of hits for interoperability
problems with filenames, and with systems sharing data between
multiple users such as wikis.

A large part of the problem is that whereas Windows and Linux tend to
default to NFC normalization, and Mac OS X input tends to produce NFC,
the Mac HFS filesystem automatically normalizes to NFD (as well as
doing full Unicode case-folding).  But even if this were not the case
problems would arise, just less predictably.

## Approaches to Normalization

R6RS provided four separate procedures to convert explicitly between
the four Unicode normalization forms.  This is the obvious choice, and
is what most other languages do.  Whenever normalization matters (yes,
every programmer working with any kind of text needs to understand
normalization and when it is necessary), you need to convert all input
to a chosen normalization form.  Or more likely ignore it until you
get a rare but inevitable normalization bug, then go back to your
design and figure out where all the relevant inputs are.  But it's
what everyone else does, so at least we won't get laughed at for this
choice.

Another option is to leave all strings in their original form and just
compare them in a normalization-insensitive manner, e.g. with
`string-ni=?'.  If you want to hash with these you'll also need
`string-hash-ni', and if you want to sort strings or use them in
search trees you'll need `string-ni<?'.  Searching will require
`string-contains-ni', though this could (very inefficiantly) be built
using `string-ni=?'.  In general, anything that needs to be
normalization independent needs to be built specially.  And you get a
lot of duplicated work.  And the programmer still needs to remember to
actually _use_ this API where appropriate instead of `string=?' and
the like.

How can we make things easier for the programmer?  One approach is to
globally represent all strings in the same normalization form in
memory.  This is mostly a matter of normalizing on port input, but
also involves some simple checks on endpoints when concatenating
strings.  String mutation is more complicated as well, though I think
strings should be immutable anyway.  The advantage is this is all done
once, at the implementation level.  The programmer never needs to
worry about normalization, and can just compare with `string=?' and
`string<?' like before (so existing code works too).  Normalization
then becomes an encoding issue, only of concern when working with an
external system or format that expects a normalization form different
from your own.

Automated normalization is not a complete silver bullet though.  The
fundamental problem is that we're working with codepoints when
conceptually most of the time we want to be working with graphemes.
The problem can be seen when searching for a string ending in a base
character in a document which has that same string followed by a
combining character (in any of the normal forms this is possible).
Even when both are in the same normal form the search will return
success, but the string doesn't actually match.  If we were comparing
grapheme by grapheme, on the other hand, it would correctly ignore the
partial match.  And assuming graphemes are all internally normalized
this would provide the same benefits of automatically normalized
strings.  So using graphemes instead of codepoints (or layered over
codepoints) as our basic unit of strings looks like a promising way to
simplify the programmer's life.

Of course, with the exception of the first option none of these have
been specified formally, much less implemented, much less tested and
used in real applications.  But they deserve consideration.
Unfortunately, the first option, by exposing direct control over
codepoint sequences, makes it impossible to implement either of the
last two options, so it seems premature to standardize on this in WG1.

A practical compromise would be to provide a single `string-normalize'
procedure, which converts a string to a system-specific normalization
form.  In an ASCII-only implementation, or an auto-normalizing
implementation this could be the identify function.  Other
implementations would simply choose a single preferred form.  Then
programmers writing portable code could code just as they do in the
first option, with the caveat that the normalization form has been
chosen for them.  This procedure would be sufficient to implement the
API described in the second option, though more optimized versions
could be provided by the implementation.  Control of explicit
normalization forms would be a matter for I/O and/or byte-vectors,
probably in a separate WG2 module.  And APIs that make the
programmer's life easier would at least be possible.
