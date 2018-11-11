# INCOMPLETE, DO NOT REVIEW

A single object type supports monad, idiom, and functor instances, as every monad is an idiom
and every idiom is a functor.  "Context" is a general term used in this SRFI for all three.
("Idiom" is a synonym for "applicative functor".
It is less clear but shorter, important in a monomorphic language.)
Objects in a context can be unwrapped, to produce the same objects out of the context;
objecta out of a context can be wrapped to produce the same objects in the context.
How this is done, or if it can be done at all, depends on the context instance.

## Constructor

`(make-context `*plist*`)`

Returns a context instance containing the basic procedures that must be supported.
The *plist* is a list alternating between names and procedure objects.
As such, it may conveniently be constructed with a backquote.

The five recognized names are `map`, `pure`, `apply`, `bind`,
and `join`, corresponding to the similarly named Scheme procedures defined below.
Any unspecified procedures will be given default implementations based ona
provided procedures whenever this is possible.  The exact defaulting rules
are TBD.

A context instance is a functor instance when the `map` procedure is either
provided or defaulted.

A context instance is an idiom instance when the `pure` and
`apply` procedures are either provided or defaulted.

A context instance is a monad instance when all five procedures are either provided
or defaulted.

## Accessors

```
context-map-procedure 
context-pure-procedure 
context-apply-procedure 
context-bind-procedure 
context-join-procedure
```

All take a context instance as the only argument and return the provided or defaulted
procedure stored in the context instance, or `#f` if the procedure is not provided
and cannot be defaulted.

## Procedures

Procedures have names matching `functor-*` if they are applicable to any functor,
and likewise for `idiom-*` and `monad-*`.

Names of arguments:

*c* is a context instance.

*cobj* is an object in a context, whereas *obj* is an object out of a context.
Note that whether an object is in or out of a context depends on the context.

*proc* is a pure procedure, accepting an *obj* and returning an *obj*.

*mproc* is a monadic procedure, accepting an *obj* and returning a *cobj*.
Monadic procedures are also known as Kleisli arrows.

*cproc* is a pure procedure which is in a context.

others TBD

`(functor-map `*c proc cobj*`)`

Unwraps the objects from *cobj*, applies *proc* to each of them, rewraps them
in the context, and returns the result.

`(idiom-pure `*c obj*`)`

Wraps *obj* in the context and returns the result.

`(idiom-apply `*c cproc cobj* ...`)`

Applies *cproc* to the values of the *cobjs*,
and returns the results wrapped in the context.

`(monad-bind `*c mproc cobj* ...`)`

Unwraps the *cobjs* and applies *mproc* to their values,
and returns the results wrapped in the context.

`(monad-join `*c cobj*`)`

The values in *cobj* are themselves context objects.  The values in
these cobjs are wrapped in the context and returned.

`(functor-as `*c mobj obj*`)`

Unwraps *mobj*, throws the result away, wraps *obj* and returns it.

`(idiom-compose `*c1 c2*`)`

Returns an idiom instance representing the composition of *c1* and *c2*
in that order.  Because monads do not compose, any definitions of
`join` and `bind` are ignored.

`(monad-and-then `*c mproc cobj*`)`

Unwraps *cobj* and applies *mproc* to it, then discards the result and
returns *cobj*.

`(idiom-replicate `*c n cobj*`)`

TBD

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

`(