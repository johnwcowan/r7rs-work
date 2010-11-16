''' I've replaced this with [[:R6RSExceptionHandlingCowan|R6RSExceptionHandlingCowan]].'''''

= Design =

When a situation calling for exceptional handling is
detected either by the implementation or by a
user program, a representation of that situation called a ''condition''
is constructed, and the exception is
announced by an action called ''signaling
the condition''.  This action allows a dynamically established ''condition handler''
an opportunity to resolve the problem.  At any given time, only one handler
is active.  Note that in this proposal, any object may be a condition; the term is not
reserved for a particular type of Scheme object.

When a condition is signaled, the active handler is called with one
argument, a condition which represents the situation.  The
handler function will execute in the dynamic environment of the call to
`signal`, except that the current condition handler
becomes the ''enclosing handler'', which is the handler that was current at the point that the
current handler function was established as the active handler.  This means that
a handler is not expected to handle conditions signaled within its dynamic extent.

If a handler returns normally, any values it returns are discarded.
The enclosing handler is invoked on the condition instead, and this
operation may be repeated until there are no more handlers.
Escaping from handler control is performed by invoking a captured
continuation.  For example, a handler may return to the point where it was
signaled by invoking a continuation stored inside the condition.  Alternatively,
it may return to the point where it was bound by invoking a
continuation in a variable that is lexically visible to the handler.
In any case, the appropriate current handler will be restored as part of the
dynamic environment.

The outermost condition handler is implementation-defined.  It generally invokes
a top-level exit continuation for the whole program, or the current
thread (if some concept of threading exists), possibly displaying
useful debugging information to some interested parties in some
implementation-specific way.  Implementations may provide
an interactive debugger that lets the programmer perform actions other
than invoking the top-level exit continuation, perhaps invoking retries
or other arbitrary continuations.

See StandardConditionPredicatesCowan for standard constructors, predicates and accessors useful
with implementation-defined conditions.

= Procedures =

 * `(current-condition-handler)`

Returns the current condition handler.

 * `(with-condition-handler `''handler''` `''thunk''`)`

Applies ''thunk'' within a dynamic environment in
which  ``handler`` is the current condition handler.  The supplied handler is executed
in the dynamic environment of the call to `signal` that signals the condition,
except that the condition handler which was in effect when `with-condition-handler` was called is
reinstated for the dynamic extent of the handler.  So if it signals any conditions without
explicitly changing to a different dynamic environment through applying continuations,
they will be passed to the previous handler, not the same one.

  * `(signal `''object''`)`

Signals the condition ''object''.  Never returns normally.

  * `(error-in `''who''` `''message''`. ` ''irritants''`)`

Constructs a condition and signals it.  Never returns normally.
This procedure is compatible with R6RS `error`.

The `who` argument must be a string or a symbol that describes the procedure or operation that detected the exception. The ''message'' argument must be a string that describes the exceptional situation. The ''irritants'' should be the arguments to the operation that detected the operation, but may be any objects relevant to the exception.

  * `(error `''message''`. ` ''irritants''`)`

Constructs a condition and signals it.  Never returns normally.
This procedure is compatible with SRFI 23.

The ''message'' argument must be a string that describes the exceptional situation. The ''irritants'' should be the arguments to the operation that detected the operation, but may be any objects relevant to the exception.

= Comments and Positions = 

Rather than write out my own exception proposal, I will just put my comments here. This system is very close to R6RS, but does not provide good reasons for deviating from R6RS' in the ways that it does. In the name of backwards compatibility, I see no good reason to not use R6RS' system over this one, given that the system would only change names and introduce only two extra names. This gains backwards compatibility with the most widely implemented exception system (across implementations, if not in total code base), doesn't lose simplicity, gains usability and convenience, maintains expressiveness, and makes life at least no more difficult for those trying to port code from non-R6RS systems. - Aaron W. Hsu (arcfide)

= Thanks =

Thanks to Taylor Campbell, Alaric Snell-Pym as the author of
ErrorsSnellPym, Kent Pitman as the author of the ISLisp specification,
and the editors of R6RS and the ANSI CL standard,
from all of whom I have derived inspiration and stolen descriptions.  They bear no responsibility
for infelicities in this proposal.

