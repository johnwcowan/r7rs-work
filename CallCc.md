The alternative name `call/cc` is available in Racket*, Gauche, Gambit, Chicken, Bigloo, Guile*, Kawa, SISC, Chibi+, Chez*, Vicare*, Larceny*, Ypsilon*, Mosh*, IronScheme*, NexJ, STKlos, KSi, SigScheme, TinyScheme, S7, XLisp, XLisp, Rep, Schemik, Elk, UMB, Oaklisp, Llava, SXM, Sizzle, Spark, Inlab, Owl Lisp, Scheme 9, Sagittarius.

It is not available in MIT, Scheme48/scsh, SCM, Shoe, Dream, RScheme, BDC, VX, FemtoLisp, Dfsch.

[*] Required by R6RS

[+] Required by R7RS-small

The following Schemes that support `call/cc` return different error messages from `(call-with-current-continuation 32)` and `(call/cc 32)`, implying that they are distinct procedures: Chicken, Bigloo, Guile*, Chez, Vicare, Ypsilon*, XLisp, Oaklisp*.

[*] Error messages differ only in a hex address for the procedure or opcode
