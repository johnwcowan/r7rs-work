## Abstract

A context is a container for zero or more ordinary Scheme objects
such that the objects can be manipulated within the container without removing
them from it.  There are many data types in Scheme that can serve as contexts,
most obviously lists and vectors. A Maybe (SRFI ???) is also a container holding
zero or one values, and an Either (from the same SRFI) is also treated as a
container which holds a value if it is a Right but no value if it is a Left.

This SRFI provides procedures which operate on containers generically.  In order
to know how to perform these operations, a context instance object must be passed
which holds up to six basic procedures in terms of which all the others are defined.
All procedures accept a context instance as the first argument, and
except as noted,
all procedures return a context.

There are three classes of context types dealt with by this SRFI, known as functors,
idioms, and monads.  Every monad is an idiom and every idiom is a functor.
Consequently, all the procedures may be applied to any object of a functor type, but
the ones beginning with `idiom-` can be applied to only objects of idiom types, and
the ones beginning with `monad-` can be applied only to objects of monad types.
The final section gives context instance objects for various standard Scheme types.

("Idiom" is a synonym for "applicative functor".
It is less clear but shorter, an important consideration in a monomorphic language.)

Objects in a context can be unwrapped, to produce the same objects out of the context;
objecta out of a context can be wrapped to produce the same objects in the context.
How this is done, or if it can be done at all, depends on the context instance.

## Constructor

`(make-context `*plist*`)`

Returns a context instance containing the basic procedures that must be supported.
The *plist* is a list alternating between names and procedure objects.
As such, it may conveniently be constructed with a backquote.

The six recognized names are `map`, `sequence`, `pure`, `apply`, `bind`,
and `join`, corresponding to the similarly named Scheme procedures defined below.
Any unspecified procedures will be given default implementations based ona
provided procedures whenever this is possible.  The exact defaulting rules
are TBD.

A context instance is a functor instance when the `map` procedure is either
provided or defaulted.

A context instance is an idiom instance when the`sequence`, `pure`, and
`apply` procedures are either provided or defaulted.

A context instance is a monad instance when all six procedures are either provided
or defaulted.

## Accessors

```
context-map-procedure 
context-sequence-procedure 
context-pure-procedure 
context-apply-procedure 
context-bind-procedure 
context-join-procedure
```

All take a context instance as the only argument and return the provided or defaulted
procedure stored in the context instance, or `#f` if the procedure is not provided
and cannot be defaulted.

## Procedures

Names of arguments:

*c* is a context instance.

*cobj* is an object in a context, whereas *obj* is an object out of a context.
Note that whether an object is in or out of a context depends on *c*.

*proc* is a pure procedure, accepting an *obj* (or more than one as noted)
and returning an *obj*.

*mproc* is a monadic procedure, accepting an *obj* and returning a *cobj*.
Monadic procedures are also known as Kleisli arrows.

*cproc* is a pure procedure which is in a context.

`(functor-map `*c proc cobj*`)`

Unwraps the objects from *cobj*, applies *proc* to each of them, rewraps them
in the context, and returns the result.

`(idiom-sequence `*c list*`)`

Takes a list whose elements are objects in the context, unwraps them,
puts them in a list, and wraps the list in the context.

Since lists are idioms, this effectively swaps the order in which
two idioms are applied, but the outer idiom must be "traversable",
a notion not defined in this SRFI.  Implementations may, however,
extend the type of the second argument.  For example, if a vector
is supported, the result will be a vector in the context.

`(idiom-pure `*c obj*`)`

Wraps *obj* in the context and returns the result.

`(idiom-apply `*c cproc cobj* ...`)`

Applies *cproc* to the values of the *cobjs*,
and returns the results wrapped in the context.

`(monad-bind `*c cobj mproc1 mproc2* ...`)`

TODO

`(monad-join `*c cobj*`)`

The values in *cobj* are themselves context objects.  The values in
these cobjs are wrapped in the context and returned.

`(functor-as `*c mobj obj*`)`

Unwraps *mobj*, throws the result away, wraps *obj* and returns it.

`(idiom-compose `*c1 c2*`)`

Returns an idiom instance representing the composition of *c1* and *c2*
in that order.  Because monads do not compose, any definitions of
`join` and `bind` are suppressed.

`(monad-and-then `*c mproc cobj*`)`

Unwraps *cobj* and applies *mproc* to it, then discards the result and
returns *cobj*.

`(functor-cons-left `*c cobj obj*`)`

Conses *obj* with the unwrapped *cobj* values, and returns
the pairs wrapped in the context.

`(functor-cons-right `*c cobj obj*`)`

Conses the unwrapped *cobj* values with *obj*, and returns
the pairs wrapped in the context.

`(functor-product `*c *cobj* *proc*`)`

Unwraps the values in *cobj*, applies *proc* to them, and
returns pairs consed from the value and the result of application
wrapped in the context.

`(monad-product `*c cobj1 cobj2*`)`

Conses each value of *cobj1* with each value of *cobj2* and
returns the pairs wrapped in the context.

`(idiom-product-left `*c mproc cobj1 cobj2*`)`

Unwraps the values of the *cobjs*; then passes the values of
*cobj1* to *mproc*, and returns the result, which is already
wrapped.

`(idiom-product-right `*c mproc cobj1 cobj2*`)`

Unwraps the values of the *cobjs*; then passes the values of
*cobj2* to *mproc*, and returns the result, which is already
wrapped.

`(monad-if `*c *mobj* *mproc1* *mproc2*`)`

Takes a boolean wrapped in the context and unwraps it.  If it
is true, *mproc1* (which must not require arguments) is invoked
and its result is returned; otherwise, *mproc2* (which also must
not require arguments) is invoked and its result is returned.
In either case, the result is already wrapped.

`(functor-lift `*c proc*`)`

Returns a procedure that unwraps its
argument, calls *proc*, and returns the wrapped result.
Note that, exceptionally, `functor-lift` does not itself return a
wrapped result.

Other procedures TBD.

## Monad instances

## Idiom instances

## Functor instances

