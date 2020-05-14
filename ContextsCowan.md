## Abstract

A context is an actual or virtual container for zero or more ordinary Scheme objects
such that the objects can be manipulated within the container without removing
them from it.  There are many data types in Scheme that can serve as contexts,
most obviously lists and vectors. A Maybe
[SRFI 189](http://srfi.schemers.org/srfi-189/srfi-189.html)
is also a context holding
zero or one values, and an Either (from the same SRFI) is also treated as a
context which holds a value if it is a Right but no value if it is a Left.

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

Objects in a context can be unwrapped to produce the same objects
(in the sense of `eqv?`) out of the context;
objects out of a context can be wrapped to produce the same objects in the context.
How this is done, or if it can be done at all, depends on the context instance.

Except as noted, all procedures return a context.

## Specification

Names of arguments:

*cobj* is an object in a context, whereas *obj* is an object out of a context.
Note that whether an object is in or out of a context depends on the context.

*proc* is a pure procedure, accepting an *obj* (or more than one as noted)
and returning an *obj*.

*mproc* is a monadic procedure, accepting an *obj* and returning a *cobj*.
Monadic procedures are also known as Kleisli arrows.

*cproc* is a pure procedure which is in a context.

## Constructor

`(make-context `*plist*`)`

Returns a context instance object containing the basic procedures that must be supported.
The *plist* is a list alternating between symbols and procedure objects.
As such, it may conveniently be constructed with a backquote.

The six recognized symbols are `map`, `sequence`, `pure`, `apply`, `bind`,
and `join`, corresponding to the similarly named Scheme procedures defined below.
Any unspecified procedures will be given default implementations based on the
provided procedures whenever this is possible.  The defaulting rules
are roughly specified here:

  *  `(monad-join c)` is `(monad-bind c id)`, where `id` is the identity function.

  *  `(monad-bind mobj f)` is `(monad-join (functor-map f mobj))`.

  *  `(functor-map f mobj)` is `(idiom-apply (idiom-pure f) x)`.
  
  * TBD apply in terms of sequence and vice versa

A context instance object is a functor instance object when the `map` procedure is either
provided or defaulted.

A context instance object is an idiom instance object when the`sequence`, `pure`, and
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

procedure stored in the context instance, or `#f` if the procedure is not provided
and cannot be defaulted.

## Context procedures

`(functor-map `proc cobj*`)`

Unwraps the objects from *cobj*, applies *proc* to each of them, rewraps them
in the context, and returns the result.

`(idiom-sequence `list*`)`

Takes a list whose elements are objects in the context, unwraps them,
puts them in a list, and wraps the list in the context.

Since lists are idioms, this effectively swaps the order in which
two idioms are applied, but the outer idiom must be "traversable",
a notion not defined in this SRFI.  Implementations may, however,
extend the type of the second argument.  For example, if a vector
is supported, the result will be a vector in the context.

`(idiom-pure `obj*`)`

Wraps *obj* in the context and returns the result.

`(idiom-apply `cproc cobj* ...`)`

Applies *cproc* to the values of the *cobjs*,
and returns the results wrapped in the context.

`(monad-bind `cobj mproc1 mproc2* ...`)`

With three arguments, `monad-bind` takes *cobj* and unwraps it from the
context and applies it to *mproc*, which transforms it
and returns the results wrapped in the context.

With additional *mproc* arguments, the result of the first *mproc* is
unwrapped and passed to the next *mproc*, and this is repeated until
there are no more *mprocs*.

`(monad-join `cobj*`)`

The values wrapped in *cobj* are themselves objects in the context.  The values in
these cobjs are wrapped in the context and returned, thus stripping off one
layer of context.

`(functor-as `cobj obj*`)`

Unwraps *cobj*, throws the result away, wraps *obj* and returns it.

`(idiom-compose `*c1 c2*`)`

Returns an idiom instance representing the composition of *c1* and *c2*
in that order.  Because monads do not compose, any definitions of
`join` and `bind` are suppressed.

`(monad-and-then `mproc cobj*`)`

Unwraps *cobj* and applies *mproc* to it, then discards the result and
returns *cobj*.

`(functor-cons-left `cobj obj*`)`

Conses *obj* with each of the unwrapped *cobj* values, and returns
the pairs wrapped in the context.

`(functor-cons-right `cobj obj*`)`

Conses each of the unwrapped *cobj* values with *obj*, and returns
the pairs wrapped in the context.

`(functor-product `*proc cobj*`)`

Unwraps the values of *cobj*, applies *proc* to them individually, and
returns pairs consed from the value and the result of application
wrapped in the context.

`(monad-product `cobj1 cobj2*`)`

Conses each value of *cobj1* with each value of *cobj2* and
returns the pairs wrapped in the context.

`(idiom-product-left `mproc cobj1 cobj2*`)`

Unwraps the values of the *cobjs*; then passes the values of
*cobj1* to *mproc*, and returns the result, which is already
wrapped.

`(idiom-product-right `mproc cobj1 cobj2*`)`

Unwraps the values of the *cobjs*; then passes the values of
*cobj2* to *mproc*, and returns the result, which is already
wrapped.

`(monad-if `*mobj* *mproc1* *mproc2*`)`

Takes a boolean wrapped in the context and unwraps it.  If it
is true, *mproc1* (which must not require arguments) is invoked
and its result is returned; otherwise, *mproc2* (which also must
not require arguments) is invoked and its result is returned.
In either case, the result is already wrapped.

`(functor-lift `proc*`)`

Returns a procedure that unwraps its
argument, calls *proc*, and returns the wrapped result.
Note that, exceptionally, `functor-lift` does not itself return a
wrapped result.

Other procedures TBD.

## Monad instances

## Idiom instances

## Functor instances

