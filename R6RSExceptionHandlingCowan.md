## Exceptions

This is a proposal for basic exception-handling and exception-raising constructs.

The exception system allows the program, when it detects an exceptional situation, to pass control to an *exception handler*, and to dynamically establish such exception handlers. Exception handlers are one-argument procedures that determine the action the program takes when an exceptional situation is signalled.  Exception handlers are always invoked with an object (which may be of any type) describing the exceptional situation.  The system implicitly maintains a *current exception handler*.

The program raises an exception by invoking the current exception handler, passing it an object encapsulating information about the exception. Any procedure accepting one argument may serve as an exception handler and any object (called a *condition object*) may be used to represent an exception.

The system maintains the current exception handler as part of the dynamic environment of the program.

When a program begins its execution, the current exception handler is expected to handle all exceptions by interrupting execution, reporting that an exception has been raised, and displaying information about the condition object that was provided. The handler may then exit, or may provide a choice of other options.  Interpretation of these expectations necessarily depends upon the nature of the system in which programs are executed, but the intent is that users perceive the raising of an exception as a controlled escape from the situation that raised the exception, not as a crash.

## Procedures and syntax forms

`(with-exception-handler `*handler*` `*thunk*`)`

*Handler* must be a procedure and should accept one argument. *Thunk* must be a procedure that accepts zero arguments. The `with-exception-handler` procedure returns the results of invoking *thunk*. *Handler* is installed as the current exception handler for the dynamic extent (as determined by `dynamic-wind`) of the invocation of *thunk*.

Implementation responsibilities: The implementation must check the restrictions on handler to the extent performed by applying it as described when it is called as a result of a call to `raise` or `raise-continuable`. An implementation may check whether *handler* is an appropriate argument before applying it.

`(guard (`*variable*` . `*cond clauses*`) `*body*`)`

Syntax: Each *cond clause* is as in the specification of `cond`.

Semantics: Evaluating a guard form evaluates *body* with an exception handler that binds the raised object to *variable* and within the scope of that binding evaluates the clauses as if they were the clauses of a `cond` expression. That implicit `cond` expression is evaluated with the continuation and dynamic environment of the guard expression.

If the test of every *cond clause* evaluates to `#f` and there is no `else` clause, then `raise` is re-invoked on the raised object within the dynamic environment of the original call to `raise`, except that the current exception handler is that of the guard expression.

The final expression in a *cond clause* is in a tail context if the `guard` expression itself is.

`(raise `*obj*`)`

Invokes the current exception handler on *obj*.  The handler is called with a continuation whose dynamic environment is that of the call to `raise`, except that the current exception handler is the one that was in place when the handler being called was installed. If the handler returns, an exception is raised non-continuably in the same dynamic environment as the handler.

`(raise-continuable `*obj*`)`

Invokes the current exception handler on *obj*.  The handler is called with a continuation that is equivalent to the continuation of the call to `raise-continuable`, with these two exceptions:

1) the current exception handler is the one that was in place when the handler being called was installed,

2) if the handler being called returns, then it will again become the current exception handler. If the handler returns, the values it returns become the values returned by the call to `raise-continuable`.

## Error procedures

`(error `*message-string*` . `*irritants*`)`

Constructs an *error condition* and raises it as if using `raise`.  The error condition may be of any Scheme type; the *message-string* and *irritants* must be retrievable from it.  The *message-string* is intended to be a human-readable report of the error.  This is SRFI 23 `error`.

`(error-in `*source*` `*message-string*` . `*irritants*`)`

Constructs an *error condition* and raises it as if using `raise`.  The error condition may be of any Scheme type; the *source*, *message-string* and *irritants* must be retrievable from it.  The *source* is a string or symbol describing the procedure or operation that detected the error; the *message-string* is intended to be a human-readable report of the error.  This is R6RS `error`.

`(error-message `*obj*`)`

Returns the message-string from *obj* if *obj* is an error condition, and `#f` otherwise.

`(error-irritants `*obj*`)`

Returns the irritants from *obj* if *obj* is an error condition, and `#f` otherwise.

`(error-source `*obj*`)`

Returns the source from *obj* if *obj* is an error condition, and `#f` otherwise.


