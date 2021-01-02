## Lexical macros

This SRFI provides ways to help serialize and deserialize
Scheme values containing arbitrary objects
that don't have any standard representation as S-expressions
by replacing them with lists whose cars are
symbols understood by the procedures below.
The process of replacing such objects by lists is
called *externalization*; the inverse is called *internalization*.
Once a data structure
has been externalized, it may be safely output with `write` and read back in with `read`.
There is no requirement that externalizing
and internalizing are exact converses; in particular, externalizing may discard
certain information.

By convention, the symbols used in externalized forms begin with slash, to minimize
conflicts with existing names.
For example, a SRFI 113 set containing the first five positive exact integers might be
externalized as `(/set 1 2 3 4 5)`.  Internalizing this might create a set
using the SRFI 128 default comparator rather than any more specific comparator that the
original set had used.  This SRFI does not define any specific internalizers or
externalizers.  (Maybe it should.)

## Issues

1) We can externalize a list of sets, but we cannot externalize a vector or set of sets,
because `lexmacs-externalize` doesn't know how to traverse vectors or sets.  This probably
must be solved by a `foldable` typeclass.

## Procedures

`(make-lexenv)`

Creates a newly allocated empty lexical macro environment.

`(add-to-lexenv! `*lexenv symbol internalizer predicate externalizer*`)`

Adds a new entry to *lexenv*, which says that during internalization, lists
whose cars are *symbol* are passed to *internalizer* to convert them
to internal format,
and during externalization, objects which satisfy *predicate* are passed
to *externalizer* to convert them to external format.  The intention
is that objects produced by *internalizer* satisfy *predicate* and that
objects produced by *externalizer* are lists whose car is *symbol*, but
this is not enforced.

Specifying *symbol* and *internalizer* as `#f` provides one-way
externalization, and specifying *predicate* and *externalizer* as `#f` provides
one-way internalization.

`(lexmacs-internalize `*object* *lexenv*`)`

Recursively expands *object*, looking for lists whose cars are defined in *lexenv*
and replacing the lists with their internalized representations.  Returns a copy
of *object* with all possible expansions; the copy may share structure with *object*.
Sublists are internalized before their parents are; non-lists are unchanged.

`(lexmacs-externalize `*object* *lexenv*`)`

Recursively externalizes *object*, examining it and its sub-objects
(only lists and vectors are examined)
for objects that satisfy a predicate defined in *lexenv*
and replacing them with their externalized
representations.  Returns a copy of *object* with all such objects
externalized; the copy may share structure with *object*.

`(lexmacs-read `*lexenv* [ *port* ]`)`

Reads an external representation from *port*, whose default is
the value of `(current-input-port)` as if with `read`,
internalizes it against `lexenv`, and returns it.

`(lexmacs-write `*lexenv obj* [ *port* ]`)`

Externalizes *obj* against *lexenv* and writes it as if with `write`
to *port*, whose default value is `(current-output-port)`.

`(lexmacs-eval `*obj lexenv env*`)`

Internalizes *obj* against *lexenv* and passes the result along with
*env* to `eval`, returning the result.



