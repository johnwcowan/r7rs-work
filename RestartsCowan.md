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

A *restarter*is an object of a new disjoint type with three fields:

  *  tag - an identifying symbol
  *  description - a list of strings string that describes the method of recovery
  *  invoker - a procedure that actually performs the recovery; the number of arguments it expects is equal to the length of *description* minus 1.
  
Restarters can be available in one of two ways.  They can be *ambient restarters*,
each of which is available during some dynamic extent of the program, or they can
be the value of some variable or part of some data structure.

### Procedures

`(make-restarter `*tag description invoker*`)` -> *restarter*

Returns a restarter with the specified tag, description, and invoker.

The *tag* argument may be a symbol or `#f` if the restarter has no tag.

The *description* argument is a list whose car is a string that
by convention is a complete sentence in a natural language using
the standard punctuation conventions of that language.  It may be
a question or a command.  The cdr of *description* is a list of
strings that describe the values to be passed to the invoker in
the same natural language: they may be phrases or whole sentences.

`(restarter? `*obj*`)` -> *boolean*

Returns `#t` if *obj* is a restarter and `#f` otherwise.

`(restarter-tag `*restarter*`)` -> *symbol* or `#f`  
`(restarter-description `*restarter*`)` -> *list-of-strings*

Returns the tag / description of *restarter*.

`(with-restarter `*restarter thunk*`)`

Establishes *restarter* as an ambient restarter
and invokes *thunk* with no arguments, after which
*restarter* is disestablished and `with-restarter`
returns whatever *thunk* returns.

`(ambient-restarters)`

Returns the current list of ambient restarters created by
`make-restarter` and established by `with-restarter`.
*Ambient-restarters* is normally a SRFI 39 / R7RS
parameter, but directly parameterizing it with
`parameter` should be avoided.

`interactor`

A SRFI 39 / R7RS parameter whose value is an interactor
procedure.  The contract of an interactor is as follows:

It accepts one argument *restarters* which is the same as
the *restarters* argument of `find-restarter`.  All the
restarters in *restarters* and `(ambient-restarters)` are
collected into a sequence, excluding those whose tag is `#f`.
All restarters whose tags are the same (in the sense of `eqv?`)
as a restarter earlier in the sequence are also excluded.

The interactor then displays the tags and the car of the
description of the associated restarters to the user, and
allows the user to choose one tag, which represents a specific
means of recovery.  The remaining strings of the chosen
restarter's descriptor are then displayed and the user is
allowed to specify a value corresponding to each string.

The interactor then returns a list whose first element is
the chosen restarter; the remaining elements are the values
collected by the interactor.

`(find-restarter `*tag restarters*`)` -> *restarter* or `#f`

Searches *restarters* for a restarter whose tag is the same
(in the sense of `eqv?`) as *tag*, except that restarters
whose tag is `#f` are ignored.  The *restarters* argument
may be a single restarter, a list of restarters, or a
[SRFI FIXME](http://srfi.schemers.org/srfi-FIXME/srfi-FIXME.html)
compound object.  If no such restarter is found in *restarters*,
the value of `(ambient-restarters)` is searched instead.

`(restart `*restarter arg* ...`)` -> *values* (may not return)

Invokes the invoker procedure of *restarter* on the *args*, and
returns however many values the invoker.  If the invoker does not
return, `restart` does not return either.

`(restart-interactively `*restarters*`)` -> *values* (may not return)

Invokes `(interactor)` on *restarters*.  The car of the resulting
list is the restarter to be invoked; the cdr are the arguments to
invoke it on.  Whatever the invoker returns, `restart-interactively`
returns.  If the invoker does not return, `restart-interactively`
does not return either.

### Standard restart tags

There are several tags that, by convention, hold to particular
behaviour protocols.  These tags are simply symbols.

`abort`

  Completely aborts the computation, usually returning to some sort of
  initial user input, like a REPL.  ABORT restarters' invokers accept
  zero arguments, are typically ambient restarters, and normally do not
  return.

`ignore`

  Ignores the condition and proceeds.  IGNORE restarters' invokers
  accept zero arguments, are typically ambient restarters, and normally
  return an unspecified value.

`retry`

  Simply retries a whole computation from a certain point, with no
  explicitly altered inputs.  Some implicit environmental changes are
  expected to have taken place.  RETRY restarters' invokers accept zero
  arguments, are typically not ambient restarters, and normally
  return an unspecified value

`use-value`

  Retries a computation with a given input value substituted for some
  invalid value in the original input.  USE-VALUE restarters' invokers
  accept at least one argument, the new value(s) to substitute, are
  typically not ambient restarters, and normally return an unspecified value.

`store-value`

  These restarters are in every respect like `use-value` restarters
  except that they are meant to store the input value somewhere, so
  that the old one is completely replaced, rather than just using the
  input value temporarily and possibly accidentally reusing the old
  value and signaling another error.

## Additional recommendations

It is highly recommended that Scheme systems integrate restarters into
their condition systems and debuggers.  This can be achieved by
using SRFI FIXME compound objects to represent conditions and
including restarters to represent suitable recovery strategies
among the subobjects.

their debuggers.  To facilitate effective interactors, they should also
include convenient methods for prompting the user; for instance, MIT
Scheme provides PROMPT-FOR-EVALUATED-EXPRESSION and a n2umber of other
similar useful facilities.
