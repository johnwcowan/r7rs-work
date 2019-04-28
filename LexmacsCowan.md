## Lexical macros

This SRFI provides ways to serialize and deserialize Scheme values containing arbitrary objects
that don't have any standard representation as S-expressions, specifically lists whose car is a reserved
symbol.  For example, a URI object whose
content is "http://example.com/foo.html" could be represented externally as
`(.uri "http://example.com/foo.html")`.  By convention, these special symbols
begin with periods.

## Procedures

`(make-lexenv)`

Creates a newly allocated empty lexical environment.

`(add-to-lexenv! `*lexenv symbol expander predicate unexpander*`)`

Adds a new entry to *lexenv*, which says that during expansion lists
whose cars are *symbol* are passed to *expander* to convert them
to internal format,
and during unexpansion objects which satisfy *predicate* are passed
to *unexpander* to convert them to external format.

`(lexmacs-expand `*object* *lexenv*`)`

Recursively expands *object*, looking for lists whose cars are defined in *lexenv*
and replacing the lists with their internal representations.  Returns a copy
of *object* with all possible expansions; the copy may share structure with *object*.
Sublists are expanded before their parents are; non-lists are unchanged.

`(lexmacs-unexpand `*object* *lexenv*`)`

Recursively unexpands *object*, looking for objects that satisfy
predicates defined in *lexenv* and replacing them with their external
representations.  Returns a copy of *object* with all such objects
unexpanded; the copy may share structure with *object*.

`(lexmacs-read `*lexenv* [ *port* ]`)`

Reads an external representation from *port*, whose default is
the value of `(current-input-port)` as if with `read`,
expands it against `lexenv`, and returns it.

`(lexmacs-write `*lexenv obj* [ *port* ]`)`

Unexpands *obj* against *lexenv* and writes it as if with `write`
to *port*, whose default value is `(current-output-port)`.

`(lexmacs-eval `*obj lexenv env*`)`

Expands *obj* against *lexenv* and passes the result, along with
*env* (an R7RS-small environment specifier) to `eval` and returns
the result.



