## Abstract

A context is an actual or virtual container for zero or more ordinary Scheme objects
such that the objects can be manipulated within the container without removing
them from it.  There are many data types in Scheme that can serve as contexts,
most obviously lists and vectors. A Maybe object from
[SRFI 189](https://srfi.schemers.org/srfi-189/srfi-189.html)
is also a context holding
either values or nothing, and an Either (from the same SRFI) is also treated as a
context which holds values if it is a Right but holds nothing if it is a Left.  Note that
holding no values is not the same as holding nothing.

This SRFI contains no references to category theory.

## Issues

Because  it is an error if the argument of a Scheme
procedure produces zero values or more than one value,
it is unclear how to launch a chain of context procedures
starting with multiple values.

## Rationale

This SRFI provides procedures which operate on containers generically.
This is a different kind of polymorphism from that provided by object-oriented
systems or generic functions in other Lisps.
Instead of using type discrimination on procedure arguments to determine how
generic operations are to be performed,
a *context instance object* must be passed to the generic procedure
as its first argument.
This object bundles up to six basic procedures in terms of which all
other procedures on contexts are defined.
Context instance objects are analogous to SRFI 128 comparators, which bundle up to
four procedures that are used to compare two objects of the same type, and in terms of
which all other comparisons are defined.

There are three classes of context types dealt with by this SRFI, known as functors,
idioms, and monads.  Every monad is an idiom and every idiom is a functor.
Consequently, all the procedures may be applied to any object of a functor type, but
the ones beginning with `idiom-` can be applied to only objects of idiom types, and
the ones beginning with `monad-` can be applied only to objects of monad types.
The final section gives context instance objects for various standard Scheme types.

("Idiom" is a synonym for "applicative functor".
It is less clear but shorter, an important consideration in a monomorphic language.)

Objects in a context can be *unwrapped* to produce the same objects
(in the sense of `eqv?`) out of the context;
objects out of a context can be *wrapped* to produce the same objects in the context.
How this is done, or if it can be done at all, depends on the context instance.

Except as noted, all procedures accept a context instance object
and return an object in that context.

## Specification

Names of arguments:

*c* is a context instance object.

*cobj* is an object in a context, whereas *obj* is an object out of a context.
Note that whether an object is in or out of a context depends on the relevant
context instance object.

*proc* is a pure procedure, accepting an *obj* (or more than one as noted)
and returning an *obj*.

*mproc* is a monadic procedure, accepting an *obj* and returning a *cobj*.
Monadic procedures are also known as Kleisli arrows.

*cproc* is a pure procedure which is wrapped in a context.

## Constructor

`(make-context `*plist*`)`

Returns a context instance object containing the basic procedures that must be supported.
The *plist* is a list alternating between symbols and procedure objects.
As such, it may conveniently be constructed with a backquote.

The six recognized symbols are `map`, `sequence`, `pure`, `apply`, `bind`,
and `join`, roughly corresponding to the similarly named Scheme procedures defined below.
Any unspecified procedures will be given default implementations based on the
provided procedures whenever this is possible.  The defaulting rules
are specified here:

  *  `(monad-join c)` is `(monad-bind values)`, where `values` is Scheme's identity function.

  *  `(monad-bind mobj f)` is `(monad-join (functor-map f mobj))`.

  *  `(functor-map f mobj)` is `(idiom-apply (idiom-pure f) x)`.
  
  * TBD apply in terms of sequence and vice versa

A context instance object is a functor instance object when the `map` procedure is either
provided or defaulted.

A context instance object is an idiom instance object when the `sequence`, `pure`, and
`apply` procedures are either provided or defaulted.

A context instance object is a monad instance object when all six procedures are either provided
or defaulted.

## Accessors

`(context-map-procedure `*c*`)`  
`(context-sequence-procedure  `*c*`)`  
`(context-pure-procedure `*c*`)`  
`(context-apply-procedure `*c*`)`  
`(context-bind-procedure `*c*`)`  
`(context-join-procedure`*c*`)`

These procedures return the corresponding
procedure stored in the context instance *c*,
or `#f` if the procedure is not provided
and cannot be defaulted.

## Context procedures

`(functor-map `*c proc cobj*`)`

Unwraps zero or more values from *cobj*, applies *proc* to each of them, rewraps
the results in the context *c*, and returns the result.

`(idiom-sequence `*c list*`)`

Takes a list whose elements are objects in the specified context, unwraps them,
puts them in another list, and wraps the list in the context.

Since lists are idioms, this effectively swaps the order in which
two idioms are applied, but the outer idiom must be "traversable",
a notion not defined in this SRFI.  Implementations may, however,
extend the type of the second argument.  For example, if a vector
is supported as the second argument, the result will be a vector in the context.

`(idiom-pure `*c obj*`)`

Wraps *obj* in the context *c* and returns the result.

`(idiom-apply `*c cproc cobj* ...`)`

Applies *cproc* to the values of the *cobjs*,
and returns the results wrapped in the context *c*.

`(monad-bind `*c cobj mproc1 mproc2* ...`)`

With three arguments, `monad-bind` takes *cobj* and unwraps it from the
context *c* and applies it to *mproc*, which transforms it
and returns the results wrapped in the context.

With additional *mproc* arguments, the results of the first *mproc* are
unwrapped and passed to the next *mproc*, and this is repeated until
there are no more *mprocs*.

`(monad-join `*c cobj*`)`

The values wrapped in *cobj* are themselves objects in the context *c*.  The values in
these cobjs are wrapped in the context and returned, thus stripping off one
layer of context.

`(functor-as `*c cobj obj*`)`

Unwraps *cobj*, throws the result away, wraps *obj* and returns it.

`(idiom-compose `*c1 c2*`)`

Returns an idiom instance representing the composition of *c1* and *c2*
in that order.  Because monads do not compose, any definitions of
`join` and `bind` are suppressed.

`(monad-then `*c mproc cobj*`)`

Unwraps *cobj* and applies *mproc* to it, then discards the result and
returns *cobj*.

`(functor-cons-left `*c cobj obj*`)`

Conses *obj* with each of the unwrapped *cobj* values, and returns
the pairs wrapped in the context *c*.

`(functor-cons-right `*c cobj obj*`)`

Conses each of the unwrapped *cobj* values with *obj*, and returns
the pairs wrapped in the context *c*.

`(functor-product `*c proc cobj*`)`

Unwraps the values of *cobj*, applies *proc* to them individually, and
returns pairs consed from the value and the result of application
wrapped in the context *c*.

`(monad-product `*c cobj1 cobj2*`)`

Conses each value of *cobj1* with each value of *cobj2* and
returns the pairs wrapped in the context *c*.

`(idiom-product-left `*c mproc cobj1 cobj2*`)`

Unwraps the values of the *cobjs* in the context *c*;
then passes the values of
*cobj1* to *mproc*, and returns the result, which is already
wrapped.

`(idiom-product-right `*c mproc cobj1 cobj2*`)`

Unwraps the values of the *cobjs* in the context *c*; then passes the values of
*cobj2* to *mproc*, and returns the result, which is already
wrapped.

`(monad-if `*c mobj *mproc1 mproc2*`)`

Unwraps *mobj* in the context *c*.  If the result
is true, *mproc1* (which must accept a single argument) is invoked
on it and its results are returned; otherwise, *mproc2* (which must
not require arguments) is invoked and its results are returned.
In either case, the result is already wrapped.

`(functor-lift `*c proc*`)`

Returns a procedure that unwraps its
argument in the context *c*, calls *proc*, and returns the wrapped result.
Note that, exceptionally, `functor-lift` does not itself return a
wrapped result.

Other procedures TBD.

## Monad instances

Lists, vectors, 

## Idiom instances

## Functor instances

