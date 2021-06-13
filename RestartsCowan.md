# Restarting conditions

## Author

Taylor Campbell and John Cowan

## Abstract

When an exceptional situation is encountered by a program, it may
signal this and pass control to a condition handler.  The signaller and
handler are two different parts of a system, between which there is a
barrier of abstraction.  In most systems, the signaller sends
information in the form of an object known as a *condition* about the
situation that it encountered.  In order to recover gracefully and
flexibly from exceptional situations, however, the signaller must
provide ways by which the handler can restart the computation, some of
which require extra input.  Often, the decision of which method of
recovery to choose is left up to a physical user, who may be prompted
for the input needed to recover.  This SRFI proposes a simple mechanism
called *restarters* to encapsulate the information necessary to restart
a computation with associated interactive prompters.

## Issues

None so far.

## Rationale

An effective and flexible system for gracefully handling and recovering
from exceptional situations is a necessity for any large software
system.  Most condition or exception systems provide only a one-way
flow of information from the signaller to the handler, however: they
offer no way to communicate possible methods of recovery to the
signaller.  Common Lisp, MIT Scheme, and Dylan are almost
unique in this regard: they provide comprehensive facilities for
restarting exceptional situations.  This proposal is considerably
simpler than Common Lisp's or Dylan's restart systems, more in the
spirit of Scheme, however.

One important feature for a restart system to provide is interactivity.
Though purely programmatic condition recovery is useful, it is well-
acknowledged by the designers and users of Common Lisp's condition
system that the ability to _interactively_ choose a method of recovery
for a condition: this ability, built-in to the Common Lisp language, is
one of the primary reasons for the power of Common Lisp development and
debugging environments.  Though much of the baggage in Common Lisp's
restart system was deemed unnecessary for this proposal, interactivity
is one crucial feature that was included.
In this SRFI, the interactivity is provided by an *interactor procedure*
that by default is provided by the implementation, but can be overridden
by the user.

## Specification

A *restarter* is an object of a new disjoint type with three fields:

  *  *tag* - an identifying symbol
  *  *description* - a list of strings that describes the method of recovery
  and the values, if any, needed for recovery
  *  *invoker* - a procedure that actually performs the recovery;
  the number of arguments it expects is equal to the length of *description* minus 1.
  
Restarters can be available in one of two ways.  They can be *ambient restarters*,
each of which is available during some dynamic extent of the program, or they can
be the value of some variable or part of some data structure.

### Procedures

`(make-restarter `*tag description invoker*`)` -> *restarter*

Returns a restarter with the specified tag, description, and invoker.

The *tag* argument may be a symbol or `#f` if the restarter has no tag.

The *description* argument is a list whose car is a string that
describes the effect of invoking the restarter.
By convention it is a complete sentence in a natural language using
the standard punctuation conventions of that language.  It may also be
a question or a command.  The cdr of *description* is a list of
strings that describe the values to be passed to the invoker in
the same natural language: they may be phrases or whole sentences.

`(restarter? `*obj*`)` -> *boolean*

Returns `#t` if *obj* is a restarter and `#f` otherwise.

`(restarter-tag `*restarter*`)` -> *symbol* or `#f`  
`(restarter-description `*restarter*`)` -> *list-of-strings*

Returns the tag / description of *restarter*. It is an
error to mutate *list-of-strings* or any of its members.

`(restart `*restarter arg* ...`)` -> *values* (may not return)

Invokes the invoker procedure of *restarter* on the *args*, and
returns however many values the invoker does.  If the invoker does not
return, `restart` does not return either.

`(ambient-restarters)`

Returns the current list of ambient restarters created by
`make-restarter` and established by `with-restarter`.
It is an error to mutate this list.
`Ambient-restarters` is normally a SRFI 39 / R7RS
parameter, but directly modifying it with
`parameterize` should be avoided.

`(with-restarter `*restarters thunk*`)`

Establishes *restarters*, which may be a single restarter
or a list, as ambient restarters.  It is an error if any
of the restarters have the same tag.
Then `with-restarter` invokes *thunk* with no arguments, after which
the restarters are disestablished and `with-restarter`
returns whatever *thunk* returns.

`(find-restarter `*tag restarters*`)` -> *restarter* or `#f`

Searches *restarters* for a restarter whose tag is the same
(in the sense of `eqv?`) as *tag*, except that restarters
whose tag is `#f` are ignored.  The *restarters* argument
may be a single restarter, a list of restarters, or a
[SRFI  222](https://srfi.schemers.org/srfi-222/srfi-222.html)
compound object.  If no such restarter is found in *restarters*,
the value of `(ambient-restarters)` is searched instead.
Failing that as well, `#f` is returned.

`(collect-restarters `*restarters*`)`

The argument *restarters* has the same semantics as
the *restarters* argument of `find-restarter`.  All the
restarters in *restarters* and `(ambient-restarters)` are
collected into a list, excluding those whose tag is `#f` and
those whose tag is the same (in the sense of `eqv?`)
as a restarter already on the list.  Returns the list.

`interactor`

A SRFI 39 / R7RS parameter whose value is an interactor
procedure.  The contract of such a procedure is as follows:

It accepts one argument, a list of restarters.
The tags and the car of the
descriptions of the restarters are made available to the user.
The user is then allowed to choose one of the tags.  
Then the remaining strings in the description of the chosen
restarter are made available to the user, and the user is
allowed to specify a value corresponding to each string.

The interactor then calls `restart` on the restarter and
the user's values and returns whatever `restart` returns.

The sample interactor outputs the tags and description strings
with `display` and reads the values using `read`.

`(restart-interactively `*restarters*`)` -> *values* (may not return)

Equivalent to `((interactor) (collect-restarters `*restarters*`))`

### Standard restart tags

There are several tags that, by convention, hold to particular
behaviour protocols.  These tags are simply symbols.

`abort`

  Completely aborts the computation, usually returning to some sort of
  initial user input such as a REPL.  The invoker of an `abort` restarter
  accepts zero arguments, is typically an ambient restarter, and normally does not
  return.

`ignore`

  Ignores the condition and proceeds.  The invoker of an `ignore` restarter
  accepts zero arguments, is typically an ambient restarter, and normally
  returns an unspecified value.

`retry`

  Simply retries a whole computation from a certain point, with no
  explicitly altered inputs.  Some implicit environmental changes are
  expected to have taken place.  The invoker of a `retry` restarter
  accepts zero arguments, is typically *not* an ambient restarter, and normally
  returns an unspecified value.

`use-value`

  Retries a computation with a given input value substituted for some
  invalid value in the original input.  The invoker of a `use-value` restarter
  accepts at least one argument, the new value(s) to substitute.  It is
  typically *not* an ambient restarter, and normally returns an unspecified value.

`store-value`

  These restarters are in every respect like `use-value` restarters
  except that they are meant to store the input value somewhere, so
  that the old one is completely replaced, rather than just using the
  input value temporarily and possibly accidentally reusing the old
  value and signaling another error.
  
## Additional recommendations

It is highly recommended that Scheme systems integrate restarters into
their condition systems and debuggers.  This can be achieved by
using SRFI 222 compound objects to represent conditions, with
restarters as members to represent suitable recovery strategies
among the subobjects.
