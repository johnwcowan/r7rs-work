## Threads

This is a simple threads proposal based on SRFI-18, but eliminating `thread-terminate!`, which has dodgy semantics: it does not give the thread any chance to recover.  Its Java equivalent, `Thread.destroy()`, is deeply deprecated for the same reason.

## Thread states

* A *running* thread is a thread that is currently executing. There can be more than one running thread on a multiprocessor machine.

* A *runnable* thread is a thread that is ready to execute or running.

* A thread is *blocked* if it is waiting for something to happen.

* A *new* thread is a thread that has not yet become runnable. A new thread becomes runnable when it is started.

* A *terminated* thread is a thread that can no longer become runnable (but *deadlocked* threads are not considered terminated).

The only valid transitions between the thread states are from new to runnable, between runnable and blocked, and from any state to terminated:

```
                         unblock
       start            <-------
  NEW -------> RUNNABLE -------> BLOCKED
    \             |      block  /
     \            v            /
      +-----> TERMINATED <----+
```

## Fairness

In various situations the scheduler must select one thread to run or to unblock from a set of threads . The constraints on the selection process determine the scheduler's *fairness*. Typically the selection depends on the order in which threads become runnable or blocked and on some "priority" attached to the threads.

Because we do not wish to preclude extensions to this package that require specific fairness constraints, there are no fairness constraints imposed. Implementations SHOULD document the fairness constraints they provide.

## Memory coherency

Read and write operations on the store (such as reading and writing a variable, an element of a vector or a string) are not required to be atomic. It is an error for a thread to write a location in the store while some other thread reads or writes that same location. It is the responsibility of the application to avoid write/read and write/write races through appropriate uses of the synchronization primitives.

Concurrent reads and writes to ports are allowed. It is the responsibility of the implementation to serialize accesses to a given port using the appropriate synchronization primitives.

## Interactions with `dynamic-wind`

When the scheduler stops the execution of a running thread T1 (whether because it blocked, expired its quantum, was terminated, etc) and then resumes the execution of a thread T2, there is in a sense a transfer of control between T1's current continuation and the continuation of T2. This transfer of control by the scheduler does not cause any `dynamic-wind` before and after thunks to be called. It is only when a thread itself transfers control to a continuation that dynamic-wind before and after thunks are called.

## The primordial thread

The execution of a program is initially under the control of a single thread known as the *primordial thread*. The primordial thread has an unspecified name, specific field, dynamic environment, dynamic-wind stack, and exception handler. All threads are terminated when the primordial thread terminates (normally or not).

## Thread procedures

`(current-thread)`

Returns the current thread.

`(thread? `*obj*`)`

Returns `#t` if *obj* is a thread, otherwise returns `#f`.

`(make-thread `*thunk*` `[*name*]`)`

Constructs and returns a new thread.  *Thunk* is a procedure returning one value; *name* can be any Scheme object.

A thread has the following fields: *name*, *specific*, *end-result*, *end-condition*, and *resource-list*, a list of communication resources it owns.   The first four fields can contain any Scheme object, and default to an unspecified value.  The *name* field is set from the optional *name* argument: it is an arbitrary Scheme object which identifies the thread (useful for debugging).

This thread is not automatically made runnable (the procedure `thread-start!` must be used to start it).  A thread's execution consists of a call to *thunk* with a continuation that causes the thread to store the value of *thunk* in its *end-result* field, abandon all resources in *resource-list*, and finally terminate. The dynamic-wind stack of the initial continuation is empty.

The thread inherits the dynamic environment from the current thread, except that the exception handler is bound to a procedure which causes the thread to store in its *end-condition* field a FIXME object, abandon all resources in *resource-list*, and finally terminate.


`(thread-name `*thread*`)`

Returns the content of the *name* field  of *thread*.

`(thread-specific `*thread*`)`

Returns the content of the *specific* field of *thread*.

`(thread-specific-set! `*thread*` `*obj*`)`

Sets *specific* field of *thread* to *obj*.  Returns an unspecified value.

`(thread-start! `*thread*`)`

Makes *thread* (which must be a new thread) runnable.  Returns *thread*.  Thread creation and thread activation are separated in order to avoid the race condition that would occur if the created thread tries to examine a data structure in which the current thread stores the created thread.

`(thread-yield!)`

The current thread exits the running state as if its quantum had expired. Returns an unspecified value.

`(thread-sleep! `*timeout*`)`

The current thread waits until the value of `(elapsed-time)` is greater than or equal to *timeout*. This blocks the thread only if *timeout* represents a point in the future.  Returns an unspecified value.

`(thread-join! `*thread* [#*timeout*|[*timeout-result*]]]`)`

The current thread waits until *thread* terminates (normally or not) or until the timeout is reached if *timeout* is supplied. If *timeout* is reached, returns *timeout-result* if it is supplied, otherwise a FIXME exception is raised. If *thread* terminated normally, the content of its *end-result* field is returned, otherwise the content of the *end-condition* field is raised.
