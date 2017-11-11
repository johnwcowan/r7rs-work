# Newsflash

I'm throwing my chips in with John! Please see [ExceptionHandlingCowan](ExceptionHandlingCowan.md). The below is for historical reference now.

# Background

Being able to define a means of handling signalled conditions is fundamental to reliable programming, that can recover from error cases.

Yes, we can make all primitives return sentinel values in the event of unusual conditions, and expect users to explicitly test for them, but that creates a lot of unpleasant "paperwork" (although and-let* helps!), and the resulting error handling paths are difficult to exhaustively test.

I think we should standardise condition signalling and handling, rather than allowing different libraries to evolve in the wild, because it's important to have one standard way to handle all signalled conditions, in order to allow code written by multiple authors to be composed.

So a library might expose a "retry up to five times, then ask the user if we should try and more, and abort if not" procedure (that calls a supplied action thunk as the action to be retried), and it can reliably handle conditions signalled by the thunk, rather than needing to cater for a plethora of different condition systems, and without taking the approach of using a dynamic-wind to catch *all* nonlocal exits from the thunk (which would interact badly with tools like (amb)).

However, the interface given here is deliberately low-level, as prettier condition-handling systems should be developed on top of it as user libraries; which will, nonetheless, be able to reliably handle each other's conditions.

# The Proposal

A current condition handler is defined as a dynamically scoped value (and is thread-local in the presence of threads; eg it has wikiLParametersSnellPym parameter semantics).

The initial value of the handler is a system-specific procedure known as the 'root condition handler' that generally invokes a top-level exit continuation for the whole program, or the current thread (if some concept of threading exists), possibly displaying useful debugging information to some interested parties in some implementation-specific way; implementations are permitted to provide a debugger that interactively lets the programmer perform actions other than invoking the top-level exit continuation, perhaps invoking retries or invoking other arbitrary continuations, but only under interactive control; unattended systems must always invoke the top-level exit continuation.

* `(signal <datum>)`

This procedure applies the current condition handler to the datum, with a continuation that immediately invokes the root condition handler with the datum `(condition-handler-returned <return value of condition handler>)`. Condition handlers are not permitted to return; `signal` never invokes its continuation, as it is intended to be called in dire circumstances. However, error handling systems may be constructed on top of it that might invoke their continuations. The root condition handler is explicitly specified, as this case is considered an internal error of the condition system.

* `(abort <datum>)`

This procedure applies the root condition handler to the datum.

I've been convinced by others that it's probably going to be useless; I included it purely because I felt uneasy about having a root condition handler that's otherwise inaccessible. However, unless somebody argues for its continued existence, I'll remove it in due course.

* `(error . <args>)`

This is a shorthand for `(raise (cons 'error <args>))`, and complies with both SRFI-23 and R6RS versions of `error`. However, depending on whether SRFI-23 or R6RS `error` conventions are chosen for WG1, appropriate argument type checking may be added to specialise this definition somewhat.

* `(current-condition-handler)`

This procedure returns the current condition handler.

* `(with-condition-handler <handler> <thunk>)`

This procedure applies the thunk within a dynamic environment in which the current condition handler is a procedure that will apply the supplied handler procedure to the condition handler's argument.

Both the thunk and the supplied handler are given the continuation of `with-condition-handler` itself.

The supplied handler is executed in the dynamic environment of the `signal` that signals the condition, except that the condition handler which was in effect when `with-condition-handler` was called is reinstated for the dynamic extent of the handler, so if it signals any conditions without explicitly changing to a different dynamic environment through applying continuations, they will be passed to the previous handler, not the same one.

The previous revision of this proposal had multiple `with-condition-handler` variants, which let the handler choose to execute things in the dynamic scope of the `signal` or of the `with-condition-handler`; however, I now feel that should be handled by more complex error-handling mechanisms built upon this simple core.

* Passing conditions up

If a condition handler cannot help with the supplied condition datum, then it should re-signal the datum (which will, unless the handler has altered the dynamic environment, invoke the condition handler that was in effect outside the `with-condition-handler`); and pass it the same condition datum, or perhaps (in some cases) a new condition datum if it sees fit to rewrite the condition datum.

It should invoke the outer condition handler within the dynamic scope of the initial `raise`, so that condition handlers which examine the dynamic scope to obtain a "stack trace" or other such debugging aids obtain the correct information.

* Run-time errors

Any error case in primitives defined by this standard shall be `signal`ed as a suitable datum, generally of the form `(<type symbol> <details...>)`; the final report will need to specify the form of the signal for each run-time error situation. Advanced implementations are entitled to statically identify cases that will always fail at run-time, and it is an implementation option to refuse the program in some way, or to execute it but to then always signal the condition when that code path is taken.

* Condition values

Some exception systems provide a detailed type system for conditions, generally allowing for inheritance (so that more specific classes of condition may be specified by handlers), and/or allowing for conditions to have "display behaviour" attached so that they can be converted into a human-readable message.

This proposal defines no such system, but implies that system-raised conditions should be of the form `(<type symbol> <args>...)`; hierarchial structure can be expressed by having a subtype symbol as the first argument, repeated as necessary (so we might have `(io file ...)` errors, `(io socket ...)`, and so on).

I'm open to ideas for better conventions!

More complex condition type systems should then recognise that pattern as a "system condition", optionally mapping the type symbol into some more complex hierarchy, and providing a default "display behaviour" along the lines of "An internal error has occurred".
