### Facts

This is a list of the SRFIs in final state that extend the R5RS lexical syntax.

SRFI 4 provides lexical syntax for homogeneous numerical vectors of the form `#u8 list`, `#s8 list`, `#f32 list`, etc..  PLT, Gauche, Gambit, Chicken, Bigloo, Guile, Kawa, Scheme48, STklos, RScheme provide this SRFI.  A close variant of this syntax, `#vu8` for unsigned byte vectors is provided in R6RS, which means that IronScheme, Ikarus, Larceny/R6RS, Ypsilon, and Mosh also support it.

SRFI 10 provides `#,(tag datum ...)`, which causes a procedure bound to `keyword` to be applied to `args` at read time.  This is similar to, but not the same as, CL `#,` syntax.  Gauche, Chicken, Guile, SISC, STklos, RScheme support this SRFI.

SRFI 30 provides `#|...|#` nested comments.  PLT, Gauche, MIT, Chicken, scsh, Kawa, SISC, Larceny/R5RS, STklos, RScheme, S7 support this SRFI.  It is also part of R6RS, which means that IronScheme, Ikarus, Larceny/R6RS, Ypsilon, and Mosh also support it.

SRFI 49 provides indentation-sensitive syntax.  There is no known support for this anywhere except in the SRFI itself, which is written in Guile.

SRFI 58 provides CL-compatible `#nA datum` for multi-dimensional general arrays.  This SRFI is supported only in SCM, though Chicken provides a non-SRFI egg that supports the same syntax.

SRFI 62 provides `#;` to comment out a single datum.  PLT, Gauche, MIT, Chicken, Kawa, SISC, Chibi, Larceny/R5RS, and Chibi support it.  It is also part of R6RS, which means that IronScheme, Ikarus, Larceny/R6RS, Ypsilon, and Mosh also support it.

SRFI 88 provides self-evaluating keywords, similar to those in CL but with trailing colons.  This is DSSSL-compatible.  Chicken, Guile, Kawa, STklos, S7 support this SRFI.


### John Cowan's opinions

Provide both kinds of comments.

Leave holes in the lexical syntax for homogeneous vectors and general arrays (say that #a and #v are reserved).

Do not provide SRFI 10, because of the phasing problem.  Binding the tag to its procedure happens at run time, which is too late to affect load time.

Consider keywords.  They're very nice.

Ignore SRFI 49.
