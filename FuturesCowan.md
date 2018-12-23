## Futures

This SRFI describes *futures* as the basic unit of Scheme concurrency (as
opposed to parallelism, for which see [ParallelPromisesCowan](ParallelPromisesCowan.md)).

Futures are analogous to [SRFI 18](http://srfi.schemers.org/srfi-18/srfi-18.html) threads,
and can easily be built on top of them.  However, are more modern in style and hopefully
easier to use.  Each future is represented to other futures, including itself, by a
unique *future object*, a member of a disjoint type.

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

## Fairness

In various situations the scheduler must select one future to run or to unblock
from a set of blocked futures . The constraints on the selection process determine the scheduler's
*fairness*. Typically the selection depends on the order in which futures become
runnable or blocked and on some *priority* attached to them.

Because we do not wish to preclude extensions to this SRFI that require specific fairness
constraints, there are no fairness constraints imposed. Implementations should document
whatever fairness constraints they provide.

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

## Procedures

`(current-future)`

Returns the future object which represents the currently running future.  When called
from the main program (not in any future), a special object called the *primordial
object* is returned instead.  Unless otherwise noted, the procedures of this
SRFI are inapplicable to the primordial object.

`(future? `*obj*`)`

Returns `#t` if *obj* is a future object (including the primordial
object), otherwise returns `#f`.

`(future `*proc args* ...`)`

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
is greater than or equal to its value when `await-for!` was
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

Returns the value of the thread-local variable named *symbol*.
It is an error to attempt to get the value of a thread-local
variable that has not been set by `future-set!`.  The main
program does not have thread-local variables.

`(future-set! `*symbol value*`)`

Sets the value of the thread-local variable named *symbol* to *value*
and returns an unspecified value.  The main
program does not have thread-local variables.

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

Returns `#t` if *obj* is an object raised when a thread times out,
and `#f` otherwise.
