This page shows how different Schemes implement the `string-titlecase` procedure when the character to be titlecased has a multi-character titlecase mapping.  The string-titlecase routine may come from SRFI-13, from R6RS, or may be a Scheme extension.  The example used is `ﬂoo`, which begins with a `fl` ligature character.

The Unicodely correct way of titlecasing this string is to treat the ligature the same as `fl`, in which case the result is `Floo`.  However, by the strict letter of R6RS, the `ﬂ` character is passed to `char-titlecase`, which in this case will return its argument unchanged.  What is more, if the `ﬂ` character is not even seen as a letter, then the result will be `ﬂOo`.

Result is `Floo`: Racket, Chicken (with the utf8 egg), Larceny

Result is `ﬂoo`: Scheme48/scsh, Guile, Kawa, SCM, Ypsilon, Mosh, STklos, Dfsch, Sagittarius

Result is `ﬂOo`: Gauche, plain Chicken, Chez, Vicare, Sizzle

No `string-titlecase` procedure that I could find: MIT, Gambit, Bigloo, JScheme, KSi, Sigscheme, Shoe MiniScheme, TinyScheme, Scheme 9, S7, XLisp, Rep, Schemik, UMB, Elk, Llava, SXi, FemtoLisp, LMU, Inlab, Foment, Picrin, Owl Lisp, Chibi

Failure of some other kind: IronScheme, NexJ, RScheme, Unlikely, SIOD, BD
