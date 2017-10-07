= Fixnum ranges for 32-bit Schemes =

This is the range of fixnums in bits for the 32-bit builds of 39 Schemes.   For this purpose, an exact integer ''n'' is a fixnum if ''n'' satisfies the implementation's `fixnum?` procedure (marked by an asterisk) or if there is no such procedure, then if `(eq? `''n''` `''n''`)` returns `#t`.

||31 bits||Racket*, Chicken*, Ypsilon*, Elk||
||30 bits||Gambit* , Scheme48/scsh, Guile, Chibi*, Chez*, SCM, Ikarus*/Vicare*, Larceny*, Ypsilon*, Mosh*, !IronScheme* (but see below), STklos*, RScheme*, Oaklisp*||
||29 bits||Gauche*, Bigloo*, !SigScheme||
||26 bits||MIT*||
||16 bits||Owl Lisp*||
||9 bits||S7||
||No fixnums (even `(eq? 0 0)` is `#f`)||SISC, KSi, !TinyScheme, Scheme 9, Dream, BDC, UMB||
||Apparently unbounded fixnums||Shoe, Schemik, VX||
||Answer not easily determined||NexJ, XLisp, rep||

In Kawa and !IronScheme all exact integers are boxed, but a short range is preallocated, so that `eq?` is satisfied: -100 to 1024 for Kawa, -100 to 999 for !IronScheme.

= Fixnum ranges for 64-bit Schemes =

These were computed using later versions of many of the above Schemes, so they are not directly comparable.

||62 bits||Racket*, Chicken*, Ypsilon*||
||61 bits||Gauche*, Gambit*, Scheme48/scsh, Guile, Chibi*, SCM, STklos*, Sagittarius*||
||60 bits||Chez, Vicare*||
||59 bits||!SigScheme||
||56 bits||MIT*||
||31 bits||Elk, !IronScheme* (but see above)||
||29 bits||Larceny*, Mosh*, RScheme*||
||16 bits||Owl Lisp*||
||11 bits||S7||
||8 bits||BDC||
||No fixnums (even `(eq? 0 0)` is `#f`)||SISC, KSi, !TinyScheme, Scheme 9, BDC||
||Apparently unbounded fixnums||Shoe, Schemik, NexJ||
||Answer not easily determined||Bigloo*, UMB, XLisp, rep||