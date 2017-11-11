Asked to load a file containing the text `(values 1 2 3)`, Racket, Scheme48/scsh, Guile, RScheme return `1 2 3` (3 values).

Chicken, Kawa, SISC, Chibi, Chez, Ikarus/Vicare, Larceny, Ypsilon, SigScheme return their standard
undefined-value object.

Gauche returns `#t 2 3` (3 values).

Bigloo returns `1 0` (2 values).

MIT, Gambit, XLisp, Sagittarius return various objects (1 value).

SCM doesn't like multiple values at the top level.

Mosh, IronScheme, KSI, TinyScheme, Scheme 9, Elk, UMB, VX, Oaklisp, Owl Lisp either don't support values or don't support load.
