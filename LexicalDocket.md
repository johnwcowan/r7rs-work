This is a docket for lexical-syntax proposals.
SRFIs 30, 38, 62 are incorporated into R7RS-small.
For other dockets, see [WG2Dockets](WG2Dockets.md).

Raw strings have been mentioned, but no SRFI yet.

This is a list of the SRFIs in final state that extend the R7RS lexical syntax.

SRFIs 4 and 160 provide lexical syntax for homogeneous numerical vectors
of the form `#u8 list`, `#s8 list`, `#f32 list`, etc. 
PLT, Gauche, Gambit, Chicken, Bigloo, Guile, Kawa, Scheme48, STklos, RScheme provide this SRFI.
A close variant of this syntax, using `#vu8` for unsigned byte vectors, is required by R6RS.

SRFI 10 provides `#,(tag datum ...)`, which causes a procedure bound to `keyword` to be applied
to `args` at read time.  This is similar to, but not the same as, CL `#,` syntax.
Gauche, Chicken, Guile, SISC, STklos, RScheme support this SRFI.

SRFI 49 provides indentation-sensitive syntax.
There is no known support for this anywhere except in the SRFI itself,
which is written in Guile.

SRFI 58 provides an `#nA datum` for multi-dimensional arrays,
specifying optional dimensions and type.
This SRFI is supported only in SCM,
though Chicken provides a non-SRFI egg that supports the same syntax.

SRFI 58 also specifies the CL-compatible use of a number following `#` to give
the length of a vector in advance.  In addition to SCM, Racket supports this.

SRFI 88 provides self-evaluating keywords of the form `foo:`.
This is DSSSL-compatible.
Gambit, Chicken, Guile, Bigloo, STklos, S7 support this SRFI.

Racket, Kawa, Chicken, Guile support keywords of the form `#:foo`.
These are self-evaluating in Chicken and Guile but are not expressions in Racket and Kawa
unless quoted, being usable only in keyword procedure declarations and calls.

Gauche, Bigloo, STklos, S7
support self-evaluating CL-compatible keywords of the form `foo:`.

(Chez uses `#:foo` to represent an uninterned symbol rather than a keyword.)

SRFI 105 provides curly infix expressions.

SRFI 107 provides XML syntax literals.

SRFI 108 provides named quasi-literals (analogous to quasiquotations)

SRFI 109 provides extended string literals.

SRFI 110 provides sweet-expressions.

SRFI 119 provides WISP, another form of indentation-sensitive syntax.

SRFI 135 recommends `«`...`»` as an external representation of texts.
A possible alternative would be `#"`...`"`.

SRFI 163 proposes another syntax for arrays, consisting of `#` followed
by a type tag (which is `a` for heterogeneous arrays) followed by an optional rank
followed by the contents of the array as a nested list.
Dimensions are inferred from the rank and the structure of the nested list.
Only Kawa supports this SRFI.

SRFI 169 proposes that a single `_` be allowed between any two digits of a number.
This would be done for readability and without any effect on the meaning.

SRFI 178 provides support for and recommends the use of CL-compatible
notation for bitvectors, `#*` followed by a sequence of `1`s and `0`s.

C-compatible base-16 inexact number notation
flagged by the use of "p" rather than "e" in exponential notation:

To be accepted by `string->number` and produced by `number->string`
when the radix argument is set  to 16; also to be accepted by `read` and the lexical syntax.

SRFI 207 defines `#u8"..."` for string-notated bytevectors.

====

This is a list of `#` constructions that are not yet defined anywhere:  

`#$ #% #? #@ #[ #^ #_ #` #~ #{ #+ #- #. #/`
`#g #h #j #k #l #m #n #p #q #r #w #y #z`
