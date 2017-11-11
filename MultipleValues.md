### Where a single value is expected

I asked my usual suite of Schemes to evaluate `(+ 1 (values 2 3))`.

Racket, MIT,
Gambit, Scheme48/scsh, Kawa, SISC, Chibi, SCM, Chez, Ikarus/Vicare, Ypsilon,
IronScheme, KSi, SigScheme, Shoe, SXM, Sizzle, Spark, Femtolisp, Owl Lisp
all report errors, either of the form "Single
value expected, multiple values received", or else a low-level error
reflecting how multiple values are represented.

Gauche, Chicken,
Bigloo, Guile, Larceny, Mosh, NexJ, STklos, S7, XLisp, Dfsch, Sagittarius reduce the multiple values to a
single value.  Of these, Guile and NexJ treat zero values passed to a single-valued continuation as an error, Bigloo transforms it to 0, XLisp and Dfsch as the empty list, and the rest as their implementation-specific "undefined" value.

TinyScheme, Scheme 9, Dream, Rep, Schemik, Elk, UMB, VX, Oaklisp, Inlab don't support `values`.

### Embedded in a begin block

Here I'm testing `(begin (values 1 2) 3)` and `(begin (values) 3)`.  Of the Schemes above that support multiple values, only SCM, SigScheme, Shoe, and Owl Lisp report an error; the rest return 3.
