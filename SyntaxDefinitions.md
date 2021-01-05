These tests show the type of syntax definitions the Schemes in the suite understand.
The following Schemes do not support syntax extension of any (known) kind,
and are excluded below:
NexJ, JScheme, KSi, Shoe, TinyScheme, BDC, XLisp, Schemik, UMB, Llava, FemtoLisp,
Oaklisp, Inlab

## Define-macro

`Define-macro` is a low-level bare macro definition, analogous to Common Lisp `defmacro`.
I tested `(define-macro x (lambda (y) y))` followed by `(x (+ 3 4))`,
expecting it to return 7 on Schemes that support `define-macro` and an error otherwise.
`Define-macro` is not part of any Scheme standard.

Supported: Gambit, Bigloo, Guile, Kawa, SISC, Ypsilon, SigScheme, S7\[\*],
Scheme 9, STklos, RScheme, Rep, Elk, Dfsch\[\*], Picrin

Not supported: Racket, Gauche, MIT, Chicken, Scheme48/scsh, SCM,
Chez, Vicare, Larceny, Mosh, IronScheme, SXM, Sagittarius,
Foment, Owl Lisp, Chibi, Sizzle[*]

Supported as the only kind of macros: S7, RScheme, Rep, Elk, Dfsch

Supported as the only kind of low-level macros: Scheme 9 

[*] S7 and Dfsch accept the `define`-style syntax `(define-macro (x y) y)` only.
The Sizzle documentation claims to do the same, but it didn't work for me.

## Define-syntax

`Define-syntax` is a wrapper for any of the following syntax transformations.
Other such wrappers are `let-syntax`, `let*-syntax`, and `letrec-syntax`,
which define local macros.
The following additional Schemes do not support `define-syntax` and are excluded below:
SigScheme, S7, Rep, Elk, Sizzle, Dfsch.
RScheme accepts only a non-standard form of `define-syntax` and is also excluded below.


### Syntax-rules

`Syntax-rules` is a syntax transformer used with `define-syntax` and its relatives.
It was described but not standardized in R4RS;
with minor extensions, it is part of the R5RS, R6RS, and R7RS-small standards.
I tested `(define-syntax x (syntax-rules () ((x y) y)))`,
which is essentially equivalent to the `define-macro` macro used above, but with hygiene.

Supported: Racket, Gauche, MIT, Gambit (with the `-:s` switch), Chicken, Bigloo,
Scheme48/scsh, Guile, Kawa, SISC, SCM (with the `-r5 -m` switches),
Chez, Vicare, Larceny, Ypsilon, Mosh, IronScheme, STklos, Scheme 9,
SXM, Sagittarius, Foment, Picrin, Owl Lisp, Chibi

Not supported: (none)

Supported as the only kind of hygienic macros: Bigloo 

### Syntax-case

`Syntax-case` is a hybrid low/high-level macro system that is standardized in R6RS.
The test used here is `(define-syntax x (lambda (x) (syntax-case x () ((x y) (syntax y)))))`,
which is exactly equivalent to the `syntax-rules` macro used above.

Supported: Racket, Gambit, Guile, Kawa, SISC, Chez, Vicare, Larceny, Ypsilon, Mosh,
IronScheme, SXM, Sagittarius, Chibi

Not supported:  Gauche, MIT, Chicken, Bigloo, SCM, STklos, Scheme 9,
Foment, Picrin, Owl Lisp

Supports both `syntax-case` and another low-level hygienic macro system:
Larceny (ER), Sagittarius (ER), Chibi (ER)

### Explicit renaming

Explicit renaming is a low-level macro system supported by
Gauche, MIT, Chicken, Scheme48/scsh, Sagittarius, Picrin, Chibi,
Larceny (with different syntax).

Supported as the only kind of low-level hygienic macros: Scheme48/scsh

Supported without `syntax-case` support: Gauche, MIT, Chicken, Scheme48/scsh, Picrin

### Implicit renaming

Implicit renaming is a low-level macro system supported by Chicken, Picrin.

Supported as the only kind of low-level hygienic macros: (none)

### Syntactic closures

Syntactic closures is a low-level macro system supported by MIT, Picrin, Chibi.

Supported as the only kind of low-level hygienic macros: (none)

### Identifier-syntax

Identifier syntax specifies an identifier which macroexpands to an expression.
It can also specify the expansion of a `set!` expression that specifies a
procedure call rather than a variable, such as `(set! (car x) y)`.

Supported by all implementations that support `syntax-case`, but no others.

