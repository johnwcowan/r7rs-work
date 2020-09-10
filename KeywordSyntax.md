Keywords are self-evaluating identifiers (except in Racket, see below)
that look like `:foo`, `foo:`, or `#:foo`, depending on the Scheme implementation:

*  Gauche, Sagittarius (and Common Lisp) support `:foo`.

*  Gambit (and DSSSL, SRFI 88) support `foo:`.

*  Bigloo, STklos support both `foo:` and `:foo` and treats them as the same (in the sense of `eqv?`).

*  S7 supports both `foo:` and `:foo` and treats them as distinct (in the sense of `eqv?`).

*  Chicken always supports `#:foo` always and `foo:` by default, but can be
   switched to use `:foo` instead of `foo:`
   by setting a parameter or a startup option.
   All formats are the same in the sense of `eqv?`.

*  Guile supports `#:foo`.

*  MIT, Scheme48/scsh, SISC, Chez, SCM, Ikarus, Larceny, Mosh, Scheme 9, SCM, SXM, VSCM, Chibi
   don't support any of them, but of course `'foo:` and `':foo` are valid quoted symbols.

*  Racket, Kawa are a special case: they support `#:foo` in function calls,
   but keywords are not expressions (though `'#:foo` is an expression).

Note: Chez and CL use `#:` not for keywords but for uninterned symbols.

If we adopt the `:foo` style, Gauche, Bigloo, S7, STklos will work out of the box,
and Chicken will support it with an option.

If we adopt the `foo:` style, Gambit, Chicken, STklos, Bigloo, S7 will work out of the box,
and Guile, Kawa will support it with an option.

If we adopt the `#:foo` style, Chicken, Guile, S7, and to some extent Racket and Kawa
will work out of the box.

## Tokens consisting only of colons

How is the datum `:` (one colon) read?

* As a **keyword** with the name `""` in Common Lisp, Gauche,
  Sagittarius, STklos.

* As a **symbol** with the name `":"` (one colon) in s7, Bigloo,
  Chicken, Gambit, s7.

How is the datum `::` (two colons) read?

* As a **keyword** with the name `""` in Common Lisp.

* As a **keyword** with the name `":"` (one colon) in Chicken, Gambit,
  Gauche, s7, Sagittarius, STklos.

* As a **symbol** with the name `"::"` (two colons) in Bigloo.

How is the datum `:::` (three colons) read?

* As a **keyword** with the name `"::"` (two colons) in Chicken,
  Gambit, Gauche, s7, Sagittarius, STklos.

* As a **symbol** with the name `":::"` (three colons) in Bigloo.

* Causes a syntax error in Common Lisp.

## Keywords and vertical bar notation

TODO

## Are keywords symbols?

Keyword objects are symbols in the `"KEYWORD"` package in Common Lisp.
**symbol-name** drops the `:` or `::` prefix.

Keyword objects are symbols such that **symbol->string** keeps the `:`
prefix in Gauche.

Keyword objects are symbols such that **symbol->string** keeps the `:`
prefix **or suffix**, remembering which one you used, in s7.

Keyword objects are not symbols in Bigloo, Chicken, Gambit,
Sagittarius, STklos.
