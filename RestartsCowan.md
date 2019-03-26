SRFI o                                                  -*- outline -*-

* Title

Restarting conditions

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
  *  invoker - a procedure that actually performs the recovery

### Constructor

A field is allocated in the dynamic environment for a list of currently
available restarters in a particular dynamic context.  This list can be
obtained with the CURRENT-RESTARTERS procedure; the WITH-RESTARTER
procedure pushes a restarter onto the list for a particular dynamic
extent, as do a number of other wrappers around it.

There is no mechanism specified for associating restarters with a
condition, for which Common Lisp provides WITH-CONDITION-RESTARTS,
because there is no standard condition system.  An extra-SRFI extension
to SRFI 35 is recommended, however, for those Scheme systems that
support it.

** Procedures specified

(MAKE-RESTARTER tag description invoker interactor) -> restarter
(RESTARTER? obj) -> boolean
(RESTARTER-TAG restarter) -> object
(RESTARTER-DESCRIPTION restarter) -> string
  MAKE-RESTARTER constructs a restarter; RESTARTER? is the restarter
  disjoint type predicate; and RESTARTER-TAG & RESTARTER-DESCRIPTION
  access the tag & description fields, respectively, of restarters.

(RESTART restarter arg ...) -> values (may not return)
(RESTART-INTERACTIVELY restarter) -> values (may not return)
  RESTART calls a restarter's invoker  procedure with the given
  arguments.  RESTART-INTERACTIVELY calls a restarter's interactor
  procedure with zero arguments, and then passes the values it returned
  to the restarter's invoker.  The RESTARTER argument may be a
  restarter or any object such that there is a restarter in the list of
  current restarters whose tag is the object; that restarter's invoker
  or interactor is used.

(CURRENT-RESTARTERS) -> list
(WITH-RESTARTER restarter thunk) -> values
(FIND-RESTARTER tag [list]) -> restarter or #F
  CURRENT-RESTARTERS returns a list of the current restarters in the
  nested order in which they were successively pushed.  It is an error
  to modify this list.  WITH-RESTARTER calls THUNK, for whose dynamic
  extent RESTARTER is pushed onto the list of current restarters, and
  returns the values returned by THUNK.  FIND-RESTARTER searches in the
  current restarter list, or LIST if it is supplied, for the most
  recently pushed restarter whose tag is equal to the given tag in the
  sense of EQV?.  If there is no such restarter in the list,
  FIND-RESTARTER returns #F.

(CALL-WITH-RESTARTER tag description invoker receiver) -> values
(CALL-WITH-INTERACTIVE-RESTARTER tag description invoker interactor
    receiver) -> values
  Convenient wrappers around MAKE-RESTARTER and WITH-RESTARTER: these
  create a restarter with the given tag, description, invoker &
  interactor (or, in CALL-WITH-RESTARTER, #F for the interactor) and
  pass the restarter to RECEIVER, for whose dynamic extent it is added
  to the list of current restarters.  These return the values returned
  by RECEIVER.

(WITH-EXITING-RESTARTER tag description thunk) -> values
(CALL-WITH-EXITING-RESTARTER tag description receiver) -> values
  WITH-EXITING-RESTARTER creates an interactive restarter with the
  given tag & description, whose invoker returns zero values to the
  call to WITH-EXITING-RESTARTER and whose interactor returns zero
  values; it then calls THUNK, for whose dynamic extent it is added to
  the list of current restarters.  CALL-WITH-EXITING-RESTARTER does
  similarly, but it also passes the restarter to RECEIVER.

** Standard restart tags

There are several tags that, by convention, hold to particular
behaviour protocols.  These tags are simply symbols.

ABORT
  Completely aborts the computation, usually returning to some sort of
  initial user input, like a REPL.  ABORT restarters' invokers accept
  zero arguments; if interactive, their interactors typically simply
  return zero values for the invoker.

IGNORE
  Ignores the condition and proceeds.  IGNORE restarters' invokers
  accept zero arguments; if interactive, their interactors typically
  simply return zero values for the invoker.

RETRY
  Simply retries a whole computation from a certain point, with no
  explicitly altered inputs.  Some implicit environmental changes are
  expected to have taken place.  RETRY restarters' invokers accept zero
  arguments, and their interactors typically simply return zero values
  for the invoker.

USE-VALUE
  Retries a computation with a given input value substituted for some
  invalid value in the original input.  USE-VALUE restarters' invokers
  accept one argument, the new value to substitute; if interactive,
  their interactors prompt the user for the new value to substitute.

STORE-VALUE
  STORE-VALUE restarters are in every respect like USE-VALUE restarters
  except that they are meant to store the input value somewhere, so
  that the old one is completely replaced, rather than just use the
  input value temporarily and possibly accidentally reuse the old
  value and signal another error.

** Extra-SRFI recommendations (NOT specified)

It is highly recommended that Scheme systems integrate restarters in
their debuggers.  To facilitate effective interactors, they should also
include convenient methods for prompting the user; for instance, MIT
Scheme provides PROMPT-FOR-EVALUATED-EXPRESSION and a number of other
similar useful facilities.

Scheme systems should also integrate the restart system into their
native condition systems, particularly to allow restarters to be
associated with conditions.  If this is done, then FIND-RESTARTER's
optional second argument should also be able to be a condition through
whose associated restarter list to search.

In Schemes that support SRFI 35, this specific extension is suggested:

  (define-condition-type &restartable &condition
    restartable-condition?
    (restarters condition-restarters))

  (define (make-restartable-condition restarters . components)
    (apply make-compound-condition
           (make-condition &restartable 'restarters restarters)
           components))

Some syntactic sugar may also be in order for constructing restartable
conditions; for example,

  (RESTARTABLE-CONDITION <restarter-clauses>
    (<condition-type> (<field-tag> <value>) ...)
    ...)

which is like CONDITION, but which adds restarters based on the list of
restarter clauses, each element of which is of either the form
(<restarter-expression>) or the form (<tag> <description> <invoker>
[<interactor>]).

* Examples

(insert examples of code using the restart system)

** Debugger interaction example

Here is an example of a hypothetical Scheme implementation that
integrates restarting with its interactive debugger.  The ,RESTART
debugger command with zero arguments enumerates all of the available
restarters and their respective indices; with one argument, a restarter
index, it invokes the restarter with that index interactively.  This
example demonstrates restarting CAR with two of the standard restart
classes.  (Note that this does not use any of the restart system API:
it is merely an example of how a practical, interactive debugger may be
integrated with the restart system.)

> (+ 1 (car #t))
Error: invalid argument -- expected pair
       #{Procedure CAR}
       #t
debug> ,restart
  1 (ABORT) Abort to the top-level REPL.
  2 (USE-VALUE) Retry the call to CAR with a replaced argument.
debug> ,restart 2
  Replacement argument to CAR: '(5 . 3)
6
> (- 15 (cdr '#(a b c)))
Error: invalid argument -- expected pair
       #{Procedure CDR}
       #(A B C)
debug> ,restart
  1 (ABORT) Abort to the top-level REPL.
  2 (USE-VALUE) Retry the call to CDR with a replaced argument.
debug> ,restart 1
Top level
> (+ 1 (* 3 frob) (/ 4 frob))
Error: unbound variable
       FROB
debug> ,restart
  1 (ABORT) Abort to the top-level REPL.
  2 (USE-VALUE) Specify a value to use in the place of FROB.
  3 (STORE-VALUE) Specify a value to define FROB to and use.
debug> ,restart 2
  Value to use in the place of FROB: 2
Error: unbound variable
       FROB
debug> ,abort
> (+ 1 (* 3 frob) (/ 4 frob))
Error: unbound variable
       FROB
debug> ,restart 3
  Value to define FROB to: 2
9
> 

* Implementation

A reference implementation is available in restart.scm.  It uses SRFI 9
to define the restarter record type and SRFI 39 to work with the
dynamic environment.  On the last page, it uses SRFI 35 to implement
the extra-SRFI recommendations for SRFI 35, but this is not an integral
component of the implementation.  Finally, it uses SRFI 23 to signal
simple errors.

* References

[1] Common Lisp: the Language, 2nd Edition
    Guy L. Steele, Jr.
    Digital Press, Maynard, Mass., 1990

[2] MIT Scheme Reference Manual
    Available on the web at
     <http://www.gnu.org/software/mit-scheme/documentation/scheme.html>

* Copyright

Copyright (C) 2005 Taylor Campbell.  All rights reserved.

Don't distribute this.  I'm not liable; if stuff goes wrong, it's all
your fault, *nyah nyah*.  This copyright notice will change in the real
SRFI document to something reasonable (in particular, the official SRFI
copyright notice, surprise surprise).