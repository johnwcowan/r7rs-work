I tested how Schemes deal with `string->symbol` when the string contains characters that can only appear in an escaped symbol.  All Schemes in my test suite (except Rep, Schemik, and Oaklisp, which don't support `string->symbol`) return a symbol from `(string->symbol "foo)(bar")` rather than a syntax error.

An interesting difference was the way in which the resulting symbol was printed by the REPL.

Racket, Gauche, MIT, Gambit, Chicken, SISC, Chibi, STklos, Sagittarius printed the symbol name as `|foo)(bar|`.

Guile, Chez, !Icarus/Vicare, Larceny, Ypsilon, KSi printed it as `foo\x29;\x28;bar`, R6RS-style.

SCM, Shoe, Elk printed it as `foo\)\(bar`.

Bigloo, Scheme48/scsh, Kawa, Mosh, !IronScheme, NexJ, !SigScheme, !TinyScheme, Scheme 9, Dream, RScheme, S7, BDC, XLisp, UMB, VX, Owl Lisp displayed the symbol as `foo)(bar` (this is arguably a bug in Mosh and !IronScheme, which are R6RS implementations).

Note that this list does not match the list of implementations which ''accept'' vertical bars, hex escapes, and backslashes.