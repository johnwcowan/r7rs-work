This is a docket for lexical-syntax proposals.
SRFIs 30, 38, 62 are incorporated into R7RS.
For other dockets, see [WG2Dockets](WG2Dockets.md).

This is a list of the SRFIs in final state that extend the R7RS lexical syntax.

SRFI 4 provides lexical syntax for homogeneous numerical vectors of the form `#u8 list`, `#s8 list`, `#f32 list`, etc..  PLT, Gauche, Gambit, Chicken, Bigloo, Guile, Kawa, Scheme48, STklos, RScheme provide this SRFI.  A close variant of this syntax, `#vu8` for unsigned byte vectors is provided in R6RS, which means that IronScheme, Ikarus, Larceny/R6RS, Ypsilon, and Mosh also support it.

SRFI 10 provides `#,(tag datum ...)`, which causes a procedure bound to `keyword` to be applied to `args` at read time.  This is similar to, but not the same as, CL `#,` syntax.  Gauche, Chicken, Guile, SISC, STklos, RScheme support this SRFI.

SRFI 49 provides indentation-sensitive syntax.  There is no known support for this anywhere except in the SRFI itself, which is written in Guile.

SRFI 58 provides CL-compatible `#nA datum` for multi-dimensional general arrays.  This SRFI is supported only in SCM, though Chicken provides a non-SRFI egg that supports the same syntax.

SRFI 88 provides self-evaluating keywords, similar to those in CL but with trailing colons.  This is DSSSL-compatible.  Chicken, Guile, Kawa, STklos, S7 support this SRFI.

SRFI 105 provides curly infix expressions.

SRFI 107 provides XML syntax literals.

SRFI 108 provides named quasi-literals (analogous to quasiquotations)

SRFI 109 provides extended string literals.

SRFI 110 provides sweet-expressions.

SRFI 119 provides WISP syntax.

