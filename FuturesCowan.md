## Futures

This SRFI describes *futures* as the basic unit of Scheme concurrency (as
opposed to parallelism, for which see [ParallelPromisesCowan](ParallelPromisesCowan.md)).

Futures are analogous to [SRFI 18](http://srfi.schemers.org/srfi-18/srfi-18.html) threads,
and can easily be built on top of them, in which case SRFi 18 threads are the same
objects as this SRFI's futures.  However, are more modern in style and hopefully
easier to use.  Each future is represented to other futures, including itself, by a
unique *future object*, a member of a disjoint type.

## FIXME

Monadic `bind` and `and-then` needed.

## Future states

* A *running* future is one that is currently executing. There can be more than one future running in parallel on a multiprocessor machine.

* A *runnable* future is one that is ready to execute or running.

* A future is *blocked* if it is waiting for something to happen.

* A *new* future is one that has not yet become runnable.

* A *terminated* future is one that can no longer become runnable (but *deadlocked* futures are not considered terminated).

The only valid transitions between future states are from new to runnable,
between runnable and blocked, and from any state to terminated:

```
                         unblock
       start            <-------
  NEW -------> RUNNABLE -------> BLOCKED
    \             |      block  /
     \            v            /
      +-----> TERMINATED <----+
```

## Default fairness

In various situations the scheduler must select one future to run or to unblock
from a set of blocked futures . The constraints on the selection process determine the scheduler's
*fairness*. Typically the selection depends on the order in which futures become
runnable or blocked and on some *priority* attached to them.

Because we do not wish to preclude extensions to this SRFI (see below)
that require specific fairness
constraints, there are no fairness constraints imposed.
Implementations should document whatever fairness constraints they provide.

## Memory coherency

Read and write operations on the store (such as reading and writing a variable,
an element of a vector or a string) are not required to be atomic.
It is an error for a future to write a location in the store
while some other thread reads or writes that same location.
It is the responsibility of the application to avoid write/read and write/write races.

Concurrent reads and writes to ports are allowed.
It is the responsibility of the implementation to serialize accesses
to a given port using the appropriate synchronization primitives.

## Interactions with `dynamic-wind`

When the scheduler stops the execution of a running future F1
(whether because it blocked, expired its quantum, was terminated, etc)
and then resumes the execution of a different running future F2, there is in a sense
a transfer of control between F1's current continuation and the continuation of F2.
This transfer of control by the scheduler does not cause any
`dynamic-wind` before and after thunks to be called.
It is only when a future itself transfers control to a continuation
that `dynamic-wind` before and after thunks are called.

## Future-specific variables

Each future (but not the main program) is associated with a number of
*future-specific variables*.  These are named by a symbol and associated
with a single value, which can be written and read by the associated
future.  It is not possible to read or write a future-specific variable
from outside the future.

## Procedures

`(current-future)`

Returns the future object which represents the currently running future.  When called
from the main program (not in any future), a special object called the *primordial
object* is returned instead.  Unless otherwise noted, the procedures of this
SRFI are inapplicable to the primordial object.

`(future? `*obj*`)`

Returns `#t` if *obj* is a future object (including the primordial
object), otherwise returns `#f`.

`(future `*proc arg* ...`)`

Creates a new future, initializes it, starts it, and returns the
corresponding future object. The execution consists of applying
the *args* to *proc*, with a continuation that causes
the result of *proc* to be stored inside the future object
along with an indication of normal termination, abandon
any communication resources the future has acquired, and terminate.
It is an error if *proc* returns other than one value.
The dynamic-wind stack of the invocation of *proc* is initially empty.

The new future inherits its dynamic environment from the currently
running future, or from the main program if there is no currently
running future.  However, the initial exception handler of the
future is a procedure which causes the argument of the handler
to be stored inside the future object
along with an indication of abnormal termination, abandon
any communication resources the future has acquired, and terminate.
The `dynamic-wind` stack of the new future is empty.

`(yield!)`

The current future, or the main program if there is no current future,
exits the running state as if its quantum had expired.
Returns an unspecified value.

`(sleep-for! `*jiffy-count*`)`

The current future, or the main program if there is no current future,
blocks until the value of `(current-jiffy)`
is greater than or equal to its value when `thread-sleep-for!` was
invoked plus *jiffy-count*.
Returns an unspecified value.

`(sleep-until! `*jiffy*`)`

The current future blocks until the value of `(current-jiffy)` is
is greater than or equal to *jiffy*.
Returns an unspecified value.

`(await `*future*`)`

The current future, or the main program if there is no current futures,
blocks until the future represented by *future* terminates (normally or not).
It is an error to pass the primordial object.
or until the timeout is reached if *timeout* is supplied.

If *future* terminated normally, its stored value is returned as the value of
`await`.  If *future* terminated abnormally, the stored condition object is
raised as if by `raise`.

`(await-for `*thread jiffy-count*`)`

Behaves the same as `await`, with the following difference:
If *future* is still runnable
when the value of `(current-jiffy)`
is greater than or equal to its value when `await-for` was
invoked plus *jiffy-count*, then *future* is terminated
and an error satisfying `timeout-exception?` is signaled.

`(await-until `*thread jiffy*`)`

Behaves the same as `await`, with the following difference:
If *future* is still runnable
when the value of `(current-jiffy)`
is greater than or equal to *jiffy*,
then *future* is terminated
and an error satisfying `timeout-exception?` is signaled.

`(future-ref `*symbol*`)`

Returns the value of the future-specific variable named *symbol*.
It is an error to attempt to get the value of a future-specific
variable that has not been set by `future-set!`.  The main
program does not have future-specific variables.

`(future-set! `*symbol value*`)`

Sets the value of the future-specific variable named *symbol* to *value*
and returns an unspecified value.  The main
program does not have future-specific variables.

`(future-abandon! `*future*`)`

Instructs *future* to abandon execution.  Note that this only happens
if *future* is cooperating by calling `future-abandoned?` periodically.
It is an error if *future* is the primordial object.

`(future-abandoned?)`

Checks whether the current future has been instructed to abandon execution.
If so, the procedure returns `#t`, otherwise `#f`.  If `#t` is returned,
the current future should take steps to abandon its execution either by
returning normally or by raising a condition.  It is an error for the
main program to call this procedure.

`(timeout-exception? `*obj*`)`

Returns `#t` if *obj* is an object raised when a future times out,
and `#f` otherwise.

## Prioritized futures 

Prioritized futures are a feature of this SRFI that specific
Scheme implementations may or may not provide.  Unlike the general
explanation of fairness above, this feature provides specific concept of fairness.
It's built on top of
[SRFI 21](http://srfi.schemers.org/srfi-21/srfi-21.html),
which is a superset of SRFI 18.

The fairness specified by this feature requires
a notion of time ordering, i.e.
"event A occurred before event B".
For the purpose of establishing time ordering,
the system may use a clock with a discrete,
possibly variable, resolution (a *tick*).
Events occuring in a given tick
can be considered to be simultaneous
(i.e. if event A occurred before event B in real time,
then the system can claim that
event A occured before event B
or, if the events fall within the same tick,
that they occured at the same time).

Each future has three priorities which affect fairness;
the *base priority*, the *boosted priority*,
and the *effective priority*.

The base priority is the value contained in the base priority field
(which is set with the `future-base-priority-set!` procedure).
A future's boosted flag field contains a boolean
that affects its boosted priority.
When the boosted flag field is false, the boosted priority is equal
to the base priority, otherwise the boosted priority is equal
to the base priority plus the value contained
in the future's priority boost field.
(which is set with the `future-priority-boost-set!` procedure).

The boosted flag field is set to false
when a future is created, when its quantum expires,
and when `future-yield!` is called.
The boosted flag field is set to true when a future blocks.
By carefully choosing the base priority and priority boost
it is possible to set up an interactive future
so that it has good I/O response time without
being a CPU hog when it performs long computations.

The effective priority of a future F
is equal to the maximum of F's boosted priority
and the effective priority of all the futures
that are blocked by F.
This *priority inheritance* avoids
priority inversion problems that would prevent
a blocked high-priority future blocked
at the entry of a critical section to progress
because a low priority future
inside the critical section is preempted
for an arbitrary long time by a medium priority future.

A future expires its quantum
(which is set with the `future-set-quantum!` procedure)
when an amount of time
equal to its quantum has elapsed
since it entered the running state and did not block,
terminate or call `future-yield!`.
At that point the future exits the running state
to allow other futures to run.
A future's quantum is thus an indication
of the rate of progress of the future
relative to the other futures of the same priority.
Moreover, the resolution of the timer
measuring the running time
may cause a certain deviation from the quantum,
so a future's quantum
should only be viewed as an approximation of the time it can run
before yielding to another future.

Futures blocked on a given mutex or condition variable
will unblock in an order which is consistent
with decreasing priority and increasing blocking time
(i.e. the highest priority future unblocks first,
and among equal priority future the one that blocked
first unblocks first).

## Prioritized-future procedures

`(future-base-priority `*future*`)` 

Returns a real number which corresponds to the base priority of *future*.

`(future-base-priority-set! `*future priority*`)`

Changes the base priority of *future* to *priority*. 
It is an error if the priority is not a real number. 
Returns an unspecified value.

`(future-priority-boost `*future*`)`

Returns a real number which corresponds to the priority boost of *future*.

`(future-priority-boost-set! `*future priority-boost*`)`

Changes the priority boost of *future* to *priority-boost*.
It is an error if *priority-boost* is not a non-negative real number.
Returns an unspecified value.

`(future-quantum `*future*`)`

Returns a real number which corresponds to the quantum of *future*.

`(future-quantum-set! `*future quantum*`)`

Changes the quantum of *future* to *quantum*.
It is an error if *quantum* is not a non-negative real number.
A value of zero selects the smallest quantum supported by the implementation.
Returns an unspecified value.
