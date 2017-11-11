I examined the results of `(integer->char 256)`, the first Unicode character outside the 8-bit range; `(integer->char 65536)`, the first Unicode character outside the Basic Multilingual Plane; `(integer->char 128)`, the first non-ASCII character; and `(integer->char 0)`, the ASCII NUL character.  I then attempted to wrap each character in a string.  In all cases where it was possible to create such a string, it correctly had a length of 1.

I also checked that `(integer->char 65)` was the same as `#\A`; it was in all Schemes that support `integer->char`.  R5RS and earlier standards don't require that `integer->char` is an ASCII mapping, but in fact it always is.

## Characters

Full support:  Racket, Gauche, Gambit, MIT, Chicken, Scheme48/scsh, Guile, Kawa, Chez, Vicare, Larceny, Ypsilon, Mosh, STklos, KSi, SigScheme, XLisp, BDC, Foment, Owl Lisp, Chibi, Sagittarius

Characters 0-65535 only:  RScheme, SISC, IronScheme, NexJ, JScheme

Characters 0-255 only:  Bigloo, SCM, Shoe, TinyScheme, Scheme 9, S7, UMB, Elk, SXM, Sizzle, Inlab

No `integer->char` procedure: Rep, Schemik, Llava, FemtoLisp, Dfsch

## Strings

Full support:  Racket, Gauche, Gambit, Chicken, Scheme48/scsh, Guile, Kawa, Chez, Vicare, Larceny, Ypsilon, Mosh, STklos, KSi, SigScheme, XLisp, BDC, Foment, Owl Lisp, Chibi, Sagittarius

Characters 0-65535 only: SISC, IronScheme, NexJ, JScheme

Characters 0-255 only:  MIT (but MIT Scheme has UTF8 and "wide" strings, which do support full Unicode), Bigloo, RScheme, SCM, Shoe, TinyScheme, Scheme 9, S7, UMB, Elk, SXM, Sizzle, Inlab

No `integer->char` procedure: Rep, Schemik, Llava, FemtoLisp, Dfsch

## Identifiers

I tried to define the identifier π with the value 3.141592653 and then evaluate it.

Successful:  Racket, Gauche, Gambit, Chicken, Bigloo, Scheme48/scsh, Guile, Kawa, SISC, SCM, Chez, Vicare, Larceny, Ypsilon, Mosh, IronScheme, NexJ, JScheme, STklos, KSi, SigScheme, Shoe, TinyScheme, RScheme, S7, BDC, UMB, Elk, Llava, Sizzle, FemtoLisp, Dfsch, Inlab, Foment, Owl Lisp, Chibi, Sagittarius

Unsuccessful: MIT, Chez (can't enter π at the REPL), Scheme 9, XLisp, Rep, Schemik, SXM

This test often succeeds because many Schemes treat anything that is not numeric or a delimiter as an identifier, even if they only understand 8-bit encodings.
