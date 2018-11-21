Keywords are self-evaluating identifiers (except in Racket, see below)
that look like `:foo`, `foo:`, or `#:foo`, depending on the Scheme implementation:

*  Gauche (and Common Lisp) support `:foo`.

*  Gambit (and DSSSL, SRFI 88) support `foo:`.

*  Bigloo, STklos support both `foo:` and `:foo` and treats them as the same (in the sense of `eqv?`).

*  S7 supports both `foo:` and `:foo` and treats them as distinct (in the sense of `eqv?`).

*  Kawa supports `#:foo` always, and also allows `foo:` depending on a command-line switch.

*  Chicken, Guile support `#:foo` always, and also allows either `foo:` or `:foo` depending on the setting of a parameter or a startup option.

*  MIT, Scheme48/scsh, SISC, Chez, SCM, Ikarus, Larceny, Mosh, Scheme 9, SSCM, SXM, VSCM, Chibi don't support any of them, but of course `'foo:` and `:foo` are valid quoted symbols.

*  Racket is a special case: it supports `#:foo` in function calls, but keywords are not expressions (though `'#:foo` is an expression).

If we adopt the `:foo` style, Gauche, Bigloo, S7, STklos will work out of the box,
and Chicken and Guile will support it with an option.

If we adopt the `foo:` style, Gambit, Chicken, STklos, Bigloo, S7 will work out of the box,
and Guile, Kawa will support it with an option.

If we adopt the `#:foo` style, Chicken, Bigloo, Guile, S7, Kawa and to some extent Racket will work out of the box.
