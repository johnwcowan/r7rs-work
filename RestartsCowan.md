# Restarting conditions

## Authors

Taylor Campbell (text), John Cowan (text, shepherd),
Wolfgang Corcoran-Mathe (revised implementation),
Arvydas Silanskas (initial implementation)

## Abstract

When an exceptional situation is encountered by a program, it may create
a *condition* object describing the situation and then
signal the condition and pass control to a condition handler.  The signaler and
handler are two different parts of a system, between which there is a
barrier of abstraction. In order to recover gracefully and
flexibly from exceptional situations, however, the signaler can
provide multiple ways by which the handler can restart the computation, some of
which may require extra input.  Often, the decision of which method of
recovery to choose is left up to a human user, who may be prompted
for the input needed to recover.  This SRFI proposes a simple mechanism
called *restarters* to encapsulate the information necessary to restart
a computation with associated interactive prompters.

## Issues

Issue 1: There should be a macro to create ambient restarters and evaluate a body
with them available, loosely analogous to `guard`.  It is not obvious how best to specify
the descriptor information for these restarters so that they can be used interactively.


## Rationale

An effective and flexible system for gracefully handling and recovering
from exceptional situations is a necessity for any large software
system.  Most condition or exception systems provide only a one-way
flow of information from the signaler to the handler, however: they
offer no way to communicate possible methods of recovery to the
signaler.  Common Lisp, MIT Scheme, and Dylan are almost
unique in this regard: they provide comprehensive facilities for
restarting exceptional situations.  This proposal is considerably
simpler than Common Lisp's or Dylan's restart systems, more in the
spirit of Scheme, however.  It is historically related to MIT Scheme's system.

One important feature for a restart system to provide is interactivity.
Though purely programmatic condition recovery is useful, it is
well-acknowledged by the designers and users of Common Lisp's condition
system that the ability to _interactively_ choose a method of recovery
for a condition: this ability, built-in to the Common Lisp language, is
one of the primary reasons for the power of Common Lisp development and
debugging environments.  Though much of the baggage in Common Lisp's
restart system was deemed unnecessary for this proposal, interactivity
is one crucial feature that was included.
In this SRFI, the interactivity is provided by an *interactor procedure*
that by default is provided by the implementation, but can be overridden
by the user. 

One major difference between the CL and Scheme condition systems is
that when a CL handler exits to its caller, the next outer handler is invoked,
whereas when a Scheme handler exits, either the code that raised the condition
is resumed (if `raise-continuably` was used), or another error is signaled
(if `raise` was used); the condition must be re-raised in order to give
the next outer handler control.  Therefore, in CL the only way for the signaler to
regain control is through a restart, but in Scheme restarts are just one way
of handling returns from exceptions.

## Specification

A *restarter* is an object of a new disjoint type with three fields:

  *  *tag* - a symbol identifying this restarter
  *  *description* - a list of strings that describes the method of recovery
     and the values, if any, needed for recovery
  *  *invoker* - a procedure that actually performs the recovery;
     the number of arguments it expects is equal to the length of *description* minus 1.
  
Restarters can be available in one of two ways.  They can be *ambient restarters*,
each of which is available during some dynamic extent of the program, or they can
be the value of some variable or part of some data structure.

### Procedures

`(make-restarter `*tag description invoker*`)` → *restarter*

Returns a restarter with the specified tag, description, and invoker.

The *tag* argument is a symbol.

The *description* argument is a list whose car is a string that
describes the effect of invoking the restarter.
By convention it is a complete sentence in a natural language using
the standard punctuation conventions of that language.  It may also be
a question or a command.  The cdr of *description* is a list of
strings that describe the values to be passed to the invoker in
the same natural language: they may be phrases or whole sentences.

The *invoker* argument is a recovery procedure.

Examples of creating restarters to help recover from division by zero:

```
(define return-zero-restarter
  (make-restarter 'return-zero
   '("Return zero.")
   (lambda () 0)))

(define return-numerator-restarter
  (make-restarter 'return-numerator
    '("Return the numerator.")
    (lambda () numerator)))

(define return-value-restarter
  (make-restarter return-value
    '("Return a specified value" "The value to return")))
```

`(restarter? `*obj*`)` → *boolean*

Returns `#t` if *obj* is a restarter and `#f` otherwise.

Examples:
```
(restarter? return-zero-restarter) → #t
(restarter? 20) → #f
```

`(restarter-tag `*restarter*`)` → *tag*  
`(restarter-description `*restarter*`)` → *list-of-strings*

Returns the tag / description of *restarter*. It is an
error to mutate *list-of-strings* or any of its members.

Access fields of `return-denominator-restarter`:

```
(restarter-tag return-denominator-restarter) → return-denominator
(restarter-description return-denominator-restarter) → ("Return the numerator.")
```

`(restart `*restarter arg* ...`)` → *values* (may not return)

Invokes the invoker procedure of *restarter* on the *args*, and
returns however many values the invoker does.  If the invoker does not
return, `restart` does not return either.

Invoking the `return-zero` restarter:

```
(restart return-zero) → 0
```

`(ambient-restarters)`

Returns the current list of ambient restarters created by
`make-restarter` and established by `with-restarter`.
It is an error to mutate this list.
`Ambient-restarters` is normally a SRFI 39 / R7RS
parameter, but directly modifying it with
`parameterize` should be avoided.

`(with-restarters `*restarters thunk*`)`

Establishes *restarters*, which may be a single restarter,
a list of restarters, or a SRFI 222 compound object, as ambient restarters
on top of the existing ambient restarters.
It is an error if any of the restarters specified by *restarters* have the same tag.
Then `with-restarter` invokes *thunk* with no arguments, after which
the restarters are disestablished and `with-restarter`
returns whatever *thunk* returns.

`(find-restarter `*tag restarters*`)` → *restarter* or `#f`

Searches *restarters* for a restarter whose tag is the same
(in the sense of `symbol=?`) as *tag*.  The *restarters* argument
may be a single restarter, a list of restarters, or a
[SRFI  222](https://srfi.schemers.org/srfi-222/srfi-222.html)
compound object.  If no such restarter is found in *restarters*,
the value of `(ambient-restarters)` is searched instead.
Failing that as well, `#f` is returned.

`(collect-restarters `*restarters*`)`

The argument *restarters* has the same semantics as
the *restarters* argument of `find-restarter`.  All available
restarters are collected into a list which is then returned, 
giving the restarters in *restarters* priority
over the restarters in `(ambient-restarters)`, but excluding
those whose tag is the same (in the sense of `symbol=?`)
as a higher-priority restarter already on the list.
Note that if *restarters* is a list, earlier elements in the list
take priority over later ones, and the same is true for subobjects
in a compound object.

`interactor`

A SRFI 39 / R7RS parameter whose value is an interactor
procedure.  The general contract of such a procedure is as follows:

It accepts one argument, a list of restarters.
The tags and the cars of the
descriptions of the restarters are made available to the user.
The user is then allowed to choose one of the tags.  
Then the remaining strings in the description of the chosen
restarter are made available to the user, and the user is
allowed to specify a value corresponding to each string.

The interactor then calls `restart` on the restarter and
the user's values and returns whatever `restart` returns.

The sample interactor outputs the tags and description strings
with `display` and reads the chosen tag and values using `read`.
Here is a possible example of the interaction:

```
The following actions are available:
return-zero: Return zero.
return-numerator: Return the numerator.
use-value: Choose a value to return.
abort: Abort the computation.

Which action do you choose:  use-value
What is the value you wish to use? 32
```

In this case the restarter will return 32.

`(restart-interactively `*restarters*`)` → *values* (may not return)

Equivalent to `((interactor) (collect-restarters `*restarters*`))`

### Standard restart tags

The following tags by convention hold to particular
behaviour protocols:

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
  accepts zero arguments, may or may not be an ambient restarter, and normally
  returns an unspecified value.

`use-value`

  Retries a computation with a given input value substituted for some
  invalid value in the original input.  The invoker of a `use-value` restarter
  accepts at least one argument, the new value(s) to substitute.  It is
  may or may not be an ambient restarter, and normally returns an unspecified value.

`store-value`

  These restarter tag is in every respect like `use-value` restarters
  except that it is meant to store the input value somewhere, so
  that the old one is completely replaced, rather than just using the
  input value temporarily and possibly accidentally reusing the old
  value later and signaling another error.
  
## Additional recommendations

It is highly recommended that Scheme systems integrate restarters into
their condition systems and debuggers.  This can be achieved by
using SRFI 222 compound objects to represent conditions, with
restarters as members to represent suitable recovery strategies
among the subobjects.
