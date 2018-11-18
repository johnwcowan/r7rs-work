## Abstract

This SRFI defines two unique container types known as Maybe and Either,
both of which contain only a single object known as their payload.
A Maybe object is either a Just object or the unique object Nothing
(which has no payload); an Either object is either
a Right object or a Left object.  Maybe represents the concept of an
optional value; Either represents the concept of a value which is
either correct (Right) or an error (Left).

Note that the terms Maybe, Just, Nothing, Either, Right, and Left
are capitalized in this SRFI so as not to be confused from their
ordinary use as English words.

## Rationale

It is common for Scheme procedures that can either succeed or fail
to return their value on success and `#f` on failure.  However, if
the procedure is able to return any value on success, there is no
way to distinguish between a successful return of `#f` and failure.
What is more, it is easy for the programmer to write code in which
success is assumed and the special case of `#f` is not handled
correctly, perhaps causing a dynamic type error.

By returning a Maybe instead, a procedure can unambiguously distinguish
between success, which returns a Just object, and failure, which
returns Nothing.  Furthermore, the returned value cannot be further
processed without removing it from its Just container except by
procedures that are Maybe-aware; a number wrapped in a Just is not
a number and has to be unwrapped to be used as a number.

Either is closely related to Maybe, and Right is closely related to Just.
However, a Left container is capable of containing an object which indicates
*why* a procedure returning an Either failed, whereas Nothing indicates
only a failure.  This use of Left and Right is merely conventional, but the
Either-accepting procedures in this SRFI treat Left and Right asymmetrically;
specifically, the `either-raise` procedure unwraps a Right, but raises the
payload of a Left.  It is also possible to use Left and Right simply as two
distinguishable types of container, or to interchange the roles of Left and
Right with `either-swap`.

## Specification

We speak of unwrapping a container when we extract its payload, and wrapping
a value in a container when we create the container with the value as its'
payload.  These containers are immutable.  Note that Nothing is not a container.

The following terms are used for the arguments:

*obj*: Any Scheme object.

*maybe*: A Maybe object.

*either*: An Either object.

*failure*: A procedure of no arguments to be tail-called.

*success*: A procedure of one argument to be tail-called.

*default*: Any Scheme object.

*pred*: A predicate procedure that accepts a single argument
and returns a true or false value.

*proc*: A procedure that accepts a single argument and returns
a single value.  In this SRFI, the procedure neither accepts nor
returns a value in a container.

*mproc*: A procedure that accepts a single argument not wrapped
in a container and returns a value that is wrapped in a container.

*list*: A Scheme list.

*producer*: A procedure that accepts no arguments and returns
no values or one value.  If it returns more than one, the other
values are ignored.

### Constructors

`(just `*obj*`)`

Monadic pure.  Returns *obj* wrapped in a Just.

`(nothing)`

Returns the unique Nothing object.

`(right `*obj*`)`

Monadic pure.  Returns *obj* wrapped in a Right.

`(left `*obj*`)`

Returns *obj* wrapped in a Left.

### Predicates

`(just? `*obj*`)`  
`(nothing? `*obj*`)`  
`(right? `*obj*`)`  
`(left? `*obj*`)`

Returns `#t` if *obj* is a Just, Nothing, Left, or Right
respectively, and `#f` otherwise.

`(maybe? `*obj*`)`

Returns `#t` if *obj* is a Maybe (that is, either a Just or Nothing)
and `#f` otherwise.

`(either? `*obj*`)`

Returns `#t` if *obj* is an Either (that is, either a Right or a Left)
and `#f` otherwise.

`(maybe= `*pred maybe1 maybe2*`)`

Returns `#t` if *maybe1* and *maybe2* are both Nothing, or if they
are both Justs and their payloads are equal in the sense of *pred*,
and `#f` otherwise.

`(either= `*pred either1 either2*`)`

Returns `#t` if *either1* and *either2* are both Lefts or both Rights
and their payloads are equal in the sense of *pred*,
and `#f` otherwise.

### Accessors

`(maybe-ref `*maybe* [*failure* [*success*] ]`)`

If *maybe* is a Just, invokes the procedure *success*
on its payload and returns the result.  Otherwise, it
invokes the procedure *failure* on no arguments and
returns the result.

`(either-ref `*either* [*failure* [*success*] ]`)`

If *either* is a Right, invokes the procedure *success*
on its payload and returns the result.  Otherwise, it
invokes the procedure *failure* on 
the payload of the Left and returns the result.
This is the only direct way to extract the payload
of a Left.

`(maybe-ref/default `*maybe default*`)`

If *maybe* is a Just, returns its payload; otherwise
returns *default*.

`(either-ref/default `*maybe default*`)`

If *either* is a Right, returns its payload; otherwise
returns *default*.

### Join and bind

`(maybe-join `*maybe*`)`

Monadic join.  If *maybe* is a Just whose payload is a Maybe,
returns that Maybe; otherwise return *maybe*.

`(either-join `*either*`)`

Monadic join.  If *either* is a Right whose payload is an Either,
returns that Either; otherwise return *either*.

`(maybe-bind `*maybe mproc1 mproc2* ...`)`  
`(either-bind `*either mproc1 mproc2* ...`)`  

Monadic bind.  If *maybe* is Nothing / a Left, it is returned at once.
If it is a Just/Right, its payload is applied to *mproc1*, which returns
a Maybe/Either.
This algorithm is repeated with the result of each *mproc*
instead of *maybe/either* until the *mprocs* are exhausted,
and the final result is then returned.

### Sequence operations

These procedures treat Maybes (and in some cases Eithers)
as a sequence of length 0 or 1.

`(maybe-length `*maybe*`)`  
`(either-length `*either*`)`

Return 1 if *maybe/either* is a Just/Right, and 0 otherwise.

`(maybe-contains? `*pred maybe obj*`)`  
`(either-contains? `*pred either obj*`)`

If *maybe/either* is a Just/Right and its payload
is the same as *obj* in the sense of *pred*,
return `#t`; otherwise, return `#f`.

`(maybe-delete `*pred maybe obj*`)`

If *maybe* is a Just and its payload is the same as *obj*
in the sense of *pred*, returns Nothing; otherwise returns *maybe*.

`(maybe-filter `*pred maybe*`)`  
`(maybe-remove `*pred maybe*`)`

If *maybe* is a Just and its payload
satisfies / does not satisfy *pred*,
return *maybe*; otherwise, return Nothing.

### Conversion

`(maybe->either `*maybe*`)`

If *maybe* is a Just, returns a Right with the same payload
in the sense of `eqv?`; otherwise returns a Left of Nothing.

`(either->maybe `*either*`)`

If *either* is a Right, returns a Just with the same payload; otherwise
in the sense of `eqv?`; otherwise returns Nothing.

`(maybe->scheme `*maybe*`)`

If *maybe* is a Just, returns its payload; otherwise returns `#f`.
This converts a Maybe to the usual Scheme protocol of returning a
true object for success or `#f` for failure.

`(scheme->maybe `*obj*`)`

If *obj* is #f, return Nothing; otherwise, return a Just whose
payload is *obj*.
This converts the usual Scheme protocol of returning
a true object for success or `#f` for failure to a Maybe.

`(list->maybe `*list*`)`  
`(list->either `*list*`)`

If *list* is the empty list, return Nothing / a Left of Nothing;
otherwise, return a Just / Right containing the first element
of *list*.

`(maybe->list `*maybe*`)`  
`(either->list `*either*`)`

If *maybe/either* is a Right/Just, return a list whose only
element is the payload; otherwise return the empty list.

`(maybe-values) `*maybe*`)`

If *maybe* is a Just, returns its payload; otherwise returns no values.

`(values->maybe `*producer*`)`

Invokes *producer* with no arguments.
If one or more values is returned, returns the first value wrapped in a Just;
if no values are returned, returns Nothing.

### Map, fold and unfold

`(maybe-map `*proc maybe*`)`  
`(either-map `*proc either*`)`

Monadic map.  If *maybe/either* is a Just/Right, applies the payload
to *proc* and wraps the returned value as a Just/Right; otherwise
returns *maybe/either*.

`(maybe-fold `*cons nil maybe*`)`  
`(either-fold `*cons nil either*`)`

If *maybe/either* is a Just/Right, *cons* is invoked on its
payload and *nil* and the result returned; otherwise, *nil* is returned.

`(maybe-unfold `*stop? mapper successor maybe*`)`  
`(either-unfold `*stop? mapper successor either*`)`

If *stop?* returns true on *seed*, a Nothing / a Left of Nothing is returned;
otherwise, *mapper* is applied to the payload of *maybe/either* and the
result is wrapped in a Right/Just and returned.
The *successor* argument is not used and may be anything;
it is required in order to preserve the standard protocol for Scheme unfold procedures.

### Miscellaneous

`(either-swap `*either*`)`

If *either* is a Left, return a Right with the same payload in the sense of `eqv?`,
and vice versa.

`(either-raise `*either*`)`

If *either* is a Right, its payload is returned; otherwise, the payload is raised
as an exception.
