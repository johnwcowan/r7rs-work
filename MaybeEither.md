## Abstract

This SRFI defines two unique immutable container types
known as Maybe and Either,
both of which can contain only a single object known as their payload.
A Maybe object is either a Just object or the unique object Nothing
(which has no payload); an Either object is either
a Right object or a Left object.  Maybe represents the concept of an
optional value; Either represents the concept of a value which is
either correct (Right) or an error (Left).

Note that the terms Maybe, Just, Nothing, Either, Right, and Left
are capitalized in this SRFI so as not to be confused with their
ordinary use as English words.  Thus "returns Nothing" means
"returns the unique Nothing object"; "returns nothing" could be
interpreted as "returns no values".

## Rationale

It is common for Scheme procedures that can either succeed or fail
to return their value on success and `#f` on failure.  However, if
the procedure is able to return any value on success, there is no
way to distinguish between a successful return of `#f` and failure.
What is more, it is easy for the programmer to write code in which
success is assumed and the special case of `#f` is not handled
correctly; thus when using a procedure which returns a number or `#f`, like
`string->number`, the programmer may assume it will always return a number,
thus causing a dynamic type error when it does not.

By returning a Maybe instead, a procedure can unambiguously distinguish
between success, which returns a Just object, and failure, which
returns Nothing.  Furthermore, the returned value cannot be further
processed without removing it from its Just container except by
procedures that are Maybe-aware; a number wrapped in a Just is not
a number and has to be unwrapped to be used as a number.

Either is closely related to Maybe, and Right is closely related to Just.
However, a Left object is a container for an object which indicates
*why* a procedure returning an Either failed, whereas Nothing indicates
only a failure.  This use of Left and Right is merely conventional, but the
Either-accepting procedures in this SRFI treat Left and Right asymmetrically;
specifically, a Left is considered empty by the join, bind, and sequence
procedures, and the `either-ref` procedure by default unwraps a Right
but raises the payload of a Left as an exception.
It is also possible to use Left and Right simply as two
distinguishable types of container, or to interchange the roles of Left and
Right with the special constructor `either-swap`.

## Specification

We speak of unwrapping a container when we extract its payload, and wrapping
a value in a container when we create the container with the value as its
payload.

The following names are used for the arguments:

*obj, default*: Any Scheme object.

*maybe*: A Maybe object.

*either*: An Either object.

*proc, failure, success*: A procedure.

*pred*: A predicate that accepts a single argument.

*equal*: An equivalence predicate that accepts two arguments.

*proc*: A procedure that accepts a single argument and returns
a single value.  In this SRFI, the procedure neither accepts nor
returns a value in a container.

*mproc*: A procedure that accepts a single argument not wrapped
in a container and returns a value that is wrapped in a container.

*list*: A Scheme list.

*producer*: A procedure that accepts no arguments and returns
zero, one, or two values.

### Constructors

`(just `*obj*`)`

Monadic pure.  Returns *obj* wrapped in a Just.

`(nothing)`

Returns the unique Nothing object.

`(right `*obj*`)`

Monadic pure.  Returns *obj* wrapped in a Right.

`(left `*obj*`)`

Returns *obj* wrapped in a Left.

`(either-swap `*either*`)`

If *either* is a Left, return a Right with the same payload (in the sense of `eqv?`),
and vice versa.

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

`(maybe= `*equal maybe1 maybe2*`)`

Returns `#t` if *maybe1* and *maybe2* are both Nothing, or if they
are both Justs and their payloads are the same in the sense of *equal*,
and `#f` otherwise.

`(either= `*equal either1 either2*`)`

Returns `#t` if *either1* and *either2* are both Lefts or both Rights
and their payloads are equal in the sense of *equal*,
and `#f` otherwise.

### Accessors

`(maybe-ref `*maybe* [*failure* [*success*] ]`)`

If *maybe* is a Just, invokes the procedure *success*
on its payload and returns the result.  Otherwise, it
invokes the procedure *failure* on no arguments and
returns the result.
The default value of *failure* is a procedure that
signals an error; the default value of *success*
is the identity procedure.

`(either-ref `*either* [*failure* [*success*] ]`)`

If *either* is a Right, invokes the procedure *success*
on its payload and returns the result.  Otherwise, it
invokes the procedure *failure* on 
the payload of the Left and returns the result.
The default value of *failure* is the procedure `raise`;
the default value of *success* is the identity procedure.

Note that this is the only direct way to extract the payload
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
returns that payload; otherwise returns *maybe*.  Thus
`(maybe-join (just (just `*x*`))` returns `(just `*x*`)` and
`(maybe-join (just (nothing))` returns Nothing.

`(either-join `*either*`)`

Monadic join.  If *either* is a Right whose payload is an Either,
returns that payload; otherwise return *either*.

`(maybe-bind `*maybe mproc1 mproc2* ...`)`  
`(either-bind `*either mproc1 mproc2* ...`)`  

Monadic bind.  If *maybe* is Nothing / a Left, it is returned at once
without invoking any more *mprocs*.
If it is a Just/Right, *mproc1* is applied to its payload, returning
a Maybe/Either.

The algorithm above is repeated with the result of each *mproc*
instead of *maybe/either* until the *mprocs* are exhausted,
and the final result is then returned.

### Sequence operations

These procedures treat Maybes (and in some cases Eithers)
as a sequence of length 0 or 1.

`(maybe-length `*maybe*`)`  
`(either-length `*either*`)`

Return 1 if *maybe/either* is a Just/Right, and 0 otherwise.

`(maybe-contains? `*equal maybe obj*`)`  
`(either-contains? `*equal either obj*`)`

If *maybe/either* is a Just/Right and its payload
is the same as *obj* in the sense of *equal*,
return `#t`; otherwise, return `#f`.

`(maybe-filter `*pred maybe*`)`  
`(maybe-remove `*pred maybe*`)`

If *maybe* is a Just and its payload
satisfies / does not satisfy *pred*,
return *maybe*; otherwise, returns Nothing.

### Conversion

`(maybe->either `*maybe*`)`

If *maybe* is a Just, returns a Right with the same payload
in the sense of `eqv?`; otherwise returns a Left
whose payload is Nothing.

`(either->maybe `*either*`)`

If *either* is a Right, returns a Just with the same payload
in the sense of `eqv?`; otherwise returns Nothing.

`(list->maybe `*list*`)`  
`(list->either `*list*`)`

If *list* is the empty list, return Nothing / a Left whose payload is Nothing;
otherwise, return a Just / Right whose payload is the first element
of *list*.

`(maybe->list `*maybe*`)`  
`(either->list `*either*`)`

If *maybe/either* is a Right/Just, return a list whose only
element is the payload; otherwise return the empty list.

`(maybe->lisp `*maybe*`)`

If *maybe* is a Just, returns its payload; otherwise returns `#f`.
This converts a Maybe to the usual Lisp and Scheme protocol of returning a
true object for success or `#f` for failure.

`(lisp->maybe `*obj*`)`

If *obj* is #f, return Nothing; otherwise, return a Just whose
payload is *obj*.
This converts the usual Lisp and Scheme protocol of returning
a true object for success or `#f` for failure to a Maybe.

`(maybe->values) `*maybe*`)`

If *maybe* is a Just, returns its payload; otherwise returns no values.

`(maybe->two-values) `*maybe*`)`

If *maybe* is a Just, returns two values, its payload and `#t`;
otherwise returns two values, both `#f`.  (This protocol is
more often used in Common Lisp, where additional values are
automatically discarded if the continuation expects only one.)

`(values->maybe `*producer*`)`

This procedure is the inverse of both `maybe->values` and
`maybe->two-values`.
It invokes *producer* with no arguments.
If no values are returned, Nothing is returned.
If one value is returned, the value is wrapped in a Just and returned.
If two values are returned and the second value is true,
the first value is wrapped in a Just and returned;
but if the second value is false, Nothing
is returned.
It is an error if *producer* returns more than two values.

`(either->values) `*maybe*`)`

If *either* is a Right, returns its payload; otherwise returns no values.

`(either->two-values) `*either*`)`

If *either* is a Right, returns two values, its payload and `#t`;
otherwise returns two values, its payload and `#f`.  (This protocol is
more often used in Common Lisp, where additional values are
automatically discarded if the continuation expects only one.)

`(values->either `*producer*`)`

This procedure is the inverse of both `either->values` and
`either->two-values`.
It invokes *producer* with no arguments.
If no values are returned, Left of Nothing is returned.
If one value is returned, the value is wrapped in a Right and returned.
If two values are returned and the second value is true,
the first value is wrapped in a Right and returned;
but if the second value is false, the first value is
wrapped in a Left and returned.
It is an error if *producer* returns more than two values.

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

If *stop?* returns true on *maybe/either*, a Nothing / a Left of Nothing is returned;
otherwise, *mapper* is applied to the payload of *maybe/either*,
wrapped in a Just/Right, and returned.
The *successor* argument is not used and may be anything;
it is required in order to preserve the standard protocol for Scheme unfold procedures.

### Trivalent logic

These macros and procedures provide trivalent
logic in the style of SQL, with
Nothing playing the role of NULL.  For the purposes of this section,
an object counts as true if it is neither `#f` nor Nothing.

The difference between the `tri-and` macro and the `tri-conjunction`
procedure (and likewise for `tri-or` and `tri-disjunction`
and for `tri-merge` and `tri-merger`)
is that the macros provide Lisp-style semantics, evaluating
only just enough of their arguments, whereas the procedures take all
of their arguments into account and so provide SQL-style semantics.
For example, `(tri-and #f (nothing))` will
return `#f`, because its second argument is never evaluated,
but `(tri-conjunction #f (nothing))` will return Nothing.

`(tri-not `*obj*`)`

Returns `#t` if *obj* is false, `#f` if *obj* is true, and Nothing
if *obj* is Nothing.

`(tri-and `<expr> ...`)` [syntax]

The <expr>s are evaluated from left to right.
If any value is false or Nothing, it is returned immediately,
and the remaining <expr>s are not evaluated.
If all values are true, the last such value is returned.
If there are no <expr>s, `#t` is returned.

`(tri-or `<expr> ...`)` [syntax]

The <expr>s are evaluated from left to right.
If any value is true or Nothing, it is returned immediately,
and the remaining <expr>s are not evaluated.
If all values are false or
there are no <expr>s, `#f` is returned.

`(tri-merge `<expr> ...`)` [syntax]

The <expr>s are evaluated from left to right.
If any value is true or false, it is returned immediately,
and the remaining <expr>s are not evaluated.
If all values are Nothing or
there are no <expr>s, Nothing is returned.

`(tri-conjunction `*obj* ...`)`

If all *objs* are true, `#t` is returned.
If any *obj* is false or Nothing, then
the first such *obj* is returned.
If there are no arguments, `#t` is returned.

`(tri-disjunction `*obj* ...`)`

If all *objs* are false, `#f` is returned.
If any *obj* is true or Nothing, then
the first such *obj* is returned.
If there are no arguments, `#f` is returned.

`(tri-merger `*obj* ...`)`

If any *objs* are true or false,
then the first such *obj* is returned.
If all *objs* are Nothing, then
Nothing is returned.
If there are no arguments, Nothing is returned.

## Acknowledgements

The Maybe and Either types and their procedures are based on Scala's Option
and Either types, though the name "Maybe" comes from Haskell.
(I think "Maybe" is catchier than "Option", which ultimately comes from ML.)
The trivalent logic is based on Chicken's `sql-null` egg.


