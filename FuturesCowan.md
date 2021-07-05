## Futures

This SRFI describes *futures* as the basic unit of Scheme concurrency (as
opposed to parallelism, for which see [ParallelPromisesCowan](ParallelPromisesCowan.md)).

Futures are analogous to [SRFI 18](https://srfi.schemers.org/srfi-18/srfi-18.html) threads,
and can easily be built using SRFI 18 threads directly.
However, it is also possible to have multiple futures
sharing the same thread in a thread pool, and this choice
depends on how heavyweight SRFI 18 threads are
on a particular implementation.

Compared to threads,
futures are more modern in style and hopefully
easier to use.  Each future is represented to other futures, including itself, by a
unique *future object*, a member of a disjoint type.

It is recognized that the term *future* is already used in a variety of ways.

## States of futures

* A *running* future is one that is currently executing.
  There can be more than one future running in parallel.

* A *runnable* future is one that is ready to execute or running.

* A future is *blocked* if it is waiting for something to happen.

* A *new* future is one that has not yet become runnable.

* A *terminated* future is one that can no longer become runnable
  (but *deadlocked* futures are not considered terminated).

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
while some other future reads or writes that same location.
It is the responsibility of the application to avoid write/read and write/write races.

Concurrent reads and writes to ports are allowed.
It is the responsibility of the implementation to serialize accesses
to a given port using the appropriate synchronization primitives, if any.

## Interactions with dynamic-wind

When the scheduler stops the execution of a running future F1
(whether because it blocked, expired its quantum, was terminated, etc)
and then resumes the execution of a different running future F2, there is in a sense
a transfer of control between F1's current continuation and the continuation of F2.
This transfer of control by the scheduler does not cause any
`dynamic-wind` before and after thunks to be called.
It is only when a future itself transfers control to a continuation
that `dynamic-wind` before and after thunks are called.

It is an error for one future to call a continuation created by another future
if `dynamic-wind` is involved.

## Promises as futures

Unless otherwise specified, the procedures of this SRFI
accept promises created with the R[567]RS syntax
`delay` or the R7RS procedure `make-promise` as the equivalent
of futures.  Unlike futures, promises are not started when created; they are
evaluated when waited for.  The `make-promises` procedure is the monadic
pure procedure for futures.

## Future-specific variables

Each future (but not the main program or a promise) is associated with a number of
*future-specific variables*.  These are named by a symbol and associated
with a single value, which can be written and read by the associated
future.  It is an error to try to read or write a future-specific variable
from outside the future.

## Blocking I/O

When a future invokes a blocking I/O operation,
the implementation must ensure that only that specific future is blocked and
that all other futures continue to run.

When the main program invokes a blocking I/O operation
and at least one future is executing,
the implementation must ensure that only the main program is blocked and
that all futures continue to run.

An implementation may depart from this requirement but must document its exact limitations.

## The quantum of a future

A future expires its quantum
when an amount of time
equal to its quantum has elapsed
since it entered the running state,
provided that it did not block,
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

## Futures and parameters

The following requirement is copied from R7RS-small,
and applies to any implementation that supports both R7RS-small and this SRFI:

> If an implementation supports multiple threads of execution,
> then `parameterize` must not change the associated
> values of any parameters in any thread other than the current thread
> and threads created inside its body.

However, if a provision is made for *mutating* as opposed to binding a
parameter, then the mutation may be visible only in the current thread
(as in Racket) or in all threads (as in Gambit).  Because of this
inconsistency, no means of mutation is specified in R7RS-small.

The traditional means is to pass a single argument to the procedure.
This argument is then passed through the parameter's *converter*
procedure and stored in the parameter as its current value.
Implementations may provide mutation in this way, or in some other way,
or not at all.

## Restrictions compared to SRFI 18

Futures do not have names.

There is no analogue of `thread-terminate!` or of the non-standard procedures
`thread-suspend!` and `thread-resume!`, because they are not only deadlock-prone
but may result in objects being exposed in an inconsistent state, allowing
arbitrary behavior.

The "specific" field is not visible; it is preempted in implementations of this
SRFI on top of SRFI 18.

Mutexes and condition variables are not visible.

Time objects (unrelated to the time objects of SRFI 19)
are also not visible, though all of their functionality that is relevant
to this SRFI is provided by other means.

## Procedures

### Constructors and initializers

`(current-future)`

Returns the future object which represents the currently running future.  When called
from the main program (not in any future), a unique object called the *primordial
object* is returned instead.  Unless otherwise noted, the procedures of this
SRFI are inapplicable to the primordial object.

The value is the same whether or not `current-future` is being invoked from within a promise.

`(future? `*obj*`)`

Returns `#t` if *obj* is a future object (including the primordial
object) or a promise, otherwise returns `#f`.

`(make-future `[*runner*] *proc arg* ...`)`

Creates a new future, initializes it with *proc* and *args*, and returns the
corresponding future object.  If the *runner* argument is provided, the
new future is registered with the specified future-runner (see below).

`(future-start! `*future*`)`

Executes *future* by applying
the saved *args* to *proc*, with a continuation that causes
the result(s) of *proc* to be stored inside the future object
along with an indication of normal termination,
any communication resources the future has acquired to be
abandoned, and then terminates it.

The new future inherits its dynamic environment from the currently
running future, or from the main program if there is no currently
running future.  However, the initial exception handler of the
future is a procedure which causes the argument of the handler
to be stored inside the future object
along with an indication of abnormal termination.  The future
then abandons any communication resources it has acquired, and terminates.

The `dynamic-wind` stack of the new future is empty.

`(future `*expr*`)`  [syntax]

Equivalent to `(make-future (lambda () `*expr*`))`, but can be optimized
by a compiler.

### Sleeping

`(future-yield!)`

The current future, or the main program if there is no current future,
exits the running state as if its quantum had expired.
Returns an unspecified value.

`(future-sleep-for! `*jiffy-count*`)`

The current future, or the main program if there is no current future,
blocks until the value of `(current-jiffy)`
is greater than or equal to *jiffy-count* plus the value of `(current-jiffy)`
when `future-sleep-for!` was invoked.  Returns an unspecified value.

`(future-sleep-until! `*jiffy*`)`

The current future , or the main program if there is no current future,
blocks until the value of `(current-jiffy)` is greater than or equal to *jiffy*.
Returns an unspecified value.

### Waiting

`(future-wait `*future*`)`

The current future, or the main program if there is no current future,
blocks until the future represented by *future* terminates (normally or not).
or until the timeout is reached if *timeout* is supplied.

If *future* terminated normally, its stored value is returned as the value of
`future-wait`.  If *future* terminated abnormally, the stored condition object is
raised as if by `raise`.

If *future* is a promise, the promise is forced with `force` and the result
returned as a normal termination.

It is an error to call `future-wait` on the primordial object or to call
it more than once on the same object.

`(future-wait-for `*future jiffy-count*`)`

Behaves the same as `future-wait`, with the following difference:
If *future* is still runnable
when the value of `(current-jiffy)`
is greater than or equal to its value when `wait-for` was
invoked plus *jiffy-count*, then *future* is terminated
and an error satisfying `timeout-exception?` is signaled.

`(future-wait-until `*future jiffy*`)`

Behaves the same as `future-wait`, with the following difference:
If *future* is still runnable
when the value of `(current-jiffy)`
is greater than or equal to *jiffy*,
then *future* is terminated
and an error satisfying `timeout-exception?` is signaled.

### Future-runners

`(with-future-runner `*proc*`)`

Creates a future-runner, an opaque object
with which newly created futures can be registered.
The procedure *proc* is invoked on the future-runner
so that futures created within *proc*
can be registered with it (see `make-future`).
The purpose of `with-future-runner` is to keep the creation
and destruction of futures within the lexical scope of *proc*.

When *proc* returns, `with-future-runner` waits
for all registered futures to terminate normally or abnormally,
and then returns a list of the futures in arbitrary order.
It is then possible to use `future-wait` on any of these
futures to extract their results.

### Future-specific variables

`(future-ref `*symbol*`)`

Returns the value of the future-specific variable named *symbol*.
It is an error to attempt to get the value of a future-specific
variable that has not been set by `future-set!`.  The main
program does not have future-specific variables.

`(future-set! `*symbol value*`)`

Sets the value of the future-specific variable named *symbol* to *value*
and returns an unspecified value.  The main
program does not have future-specific variables.

### Abandonment

`(future-abandon! `*future*`)`

Instructs *future* to abandon execution.  Note that this only happens
if *future* is cooperating by calling `future-abandoned?` periodically.
If *future* is a promise, `future-abandon!` has no effect.
It is an error if *future* is the primordial object.

`(future-abandoned?)`

Checks whether the current future has been instructed to abandon execution.
If so, the procedure returns `#t`, otherwise `#f`.  If `#t` is returned,
the current future should take steps to abandon its execution either by
returning normally or by raising a condition.  It is an error for the
main program to call this procedure.

## Quasi-monadic procedures

`(future-map `*proc future*`)`

Returns a future that behaves as follows: when waited for, it first waits
for *future*, then invokes *proc* on its result and returns whatever *proc* returns.
If *future* raises an exception, `future-map` passes it through
without invoking *proc*.

`(future-bind `*obj future1 future2* ...`)`

Returns a future *f* that behaves as follows: it passes *obj* as an argument
to *future1* and awaits its completion, then passes the result to
*future2* and waits for its completion, and so on until there are no
more *futures*.  When waited for, *f* returns the result of the last
*future*.  This is monadic bind, and is useful for pipelining.

`(future-and-then `*obj future1 future2* ...`)`

The same as `future-bind` except that the *futures* other than the
first don't require an argument and the results of all *futures* are discarded.
This is useful for sequencing side effects.  This is monadic
sequence, and is useful for sequencing.

### Exceptions

`(future-timeout-exception? `*obj*`)`

Returns `#t` if *obj* is an object raised when a future times out,
and `#f` otherwise.

## Implementation

This is a high-level description of how to implement this SRFI on top of SRFI 18 or SRFI 21
such that futures and threads correspond one to one:

The `current-future` procedure is just `current-thread`.

The `future?` procedure returns `#t` if either `promise?` or `thread?` return `#t`.

The `future` procedure constructs a procedure that applies `proc` to `args`,
uses `make-thread` to instantiate a thread that invokes that procedure,
initializes the thread's "specific" field as explained below,
starts the thread, and returns it.

The `future-yield!` procedure is just `thread-yield!`.

The `future-sleep-for!` procedure is just `thread-sleep!`.

The `future-sleep-until!` procedure constructs a SRFI 18 time object
from its *jiffy* argument and calls `thread-sleep!`.

The `future-wait` procedure calls `thread-join!` with a timeout of `#f`.

The `future-wait-for` procedure is just `thread-join!`

The `future-wait-until` procedure constructs a SRFI 18 time object
from its *jiffy* argument and calls `thread-join!`

The `future-ref!`, `future-set!`, `future-abandon`, and `future-abandoned?`
procedures make use of the "specific" field of the underlying thread.
This is initialized by `future` to an object with two slots (a pair will do).

The first slot is the abandon flag which is initially `#f`
and which `future-abandoned?` checks.
Any thread may set this flag to `#t`, so that the thread will abandon execution
when it next calls `future-abandoned?`.  This is normally done by raising an
exception.  No mutual exclusion is needed, since the flag only undergoes a
one-way transition from `#f` to `#t` and it does not matter which thread does this
or how many times it is done.
All that is required is that the action of mutating the slot is atomic.

The second slot contains a lookup table
(such as an alist or hash table) that maps the symbols naming future-specific variables
into their current values.  Because only the current thread has access to this table,
no locking is required.

Unfortunately, because the primordial future's "specific" field is not reliably initialized,
these mechanisms are not available to it.

The `future-timeout-exception?` procedure is just `join-timeout-exception?`.

The implementations of `future-map`, `future-bind` and `future-and-then`
should be obvious from their descriptions.  A future group can be a wrapper
around a list so that new futures can be added at any time.

