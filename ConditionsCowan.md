## Conditions

A condition object encapsulates information about an exceptional situation.
Typically the rest of the system is notified about a condition
using the `raise` and `raise-continuable` procedures and their relatives.
Conditions are logically independent of the exception system, however:
conditions may be used for any purpose, and any object may be passed to the exception system.

The design of this condition system attempts to assume as little as possible
about any existing implementation-specific condition system.
In particular, there is no specified relationship between conditions and records,
as there is in R6RS, nor is there any notion of subtyping required by the system.
There are condition types for convenience in dispatching,
but they are just symbols and in general entail nothing about what information is encapsulated,
as different implementations will provide different kinds of information
when creating an implementation-specified condition.

However, it is designed to interoperate smoothly with condition objects that are
[compound objects](CompoundObjectsCowan.md) in such a way that the API defined in this
SRFI will behave correctly when applied to compound objects.  For that reason, there are
no constructors for conditions as such; construct a compound object instead.

A *native condition object* is a Scheme object that any underlying native condition system
can create.

## Predicates

`(condition? `*obj*`)`

Returns `#t` if *obj* is a compound object or a native condition object
created by the native condition system, and `#f` otherwise.

`(condition-of-type? `*obj* *sym*`)`

If *obj* is a compound object, returns `#t` if `((make-compound-type-properties `*sym*`) `*obj*`)`
would return true, and `#f` otherwise.

If *obj* is a native condition object, returns an implementation-specified boolean.

## Accessors

`(condition-types `*condition*`)`

Returns the list of types to which *condition* belongs.  It is an error to attempt to mutate this list.

If *obj* is a compound object, returns a list of the cars of the type subobjects of *obj*.

If *obj* is a native condition object, returns an implementation-specified list of symbols.

`(condition-properties `*condition sym*`)`

Returns an alist of the properties of *condition* that are associated with the type *sym*.
It is an error to attempt to mutate this alist.

If *obj* is a compound object, returns what `(make-compound-type-properties `*sym obj*`)` returns.

If *obj* is a native condition object, returns an implementation-specific alist.


## R7RS-small interoperation

`(error-object? `*obj*`)`

`(file-error? `*obj*`)`

`(read-error? `*obj*`)`

These R7RS-small predicates are extended to return `#t` on a compound object
that would return true to `(condition-properties `*obj sym*`)`,
where *sym* is `simple`, `file`, or `read` respectively.

`(error-message `*error*`)`

`(error-irritants `*error*`)`

If *error* is a compound object,
these R7RS-small procedures are extended to return the value associated with
the key `message` / `irritants` of the first type subobject whose car is `simple`,
or the empty list / string if there is no such key.

## Standard condition types

The following condition types are standardized.
Conditions of each type may be created by the implementation in the specified situations
as well as any analogous situations.
The only constraint is that the implementation must not raise a condition of a specified type
unless that type of external situation is in fact present.
The list is intended to be comprehensive but not complete: it draws on R6RS, Java, and other sources.

|Type|Explanation|
|---|---|
|`already-exists`|file already exists|
|`arithmetic`|arithmetic error|
|`arity`|too many or too few arguments|
|`assert`|error created by `assert`|
|`bignum`|exact number too large to represent|
|`circular-list`|circular lists not supported|
|`closed`|I/O operation on closed port|
|`concurrency`|invalid concurrent modification|
|`continuation`|escape procedure invoked when not supported|
|`conversion`|attempted impossible conversion|
|`deadlock`|scheduler deadlock|
|`divide`|division by exact zero|
|`domain`|argument has wrong type or value|
|`encoding`|encoding or decoding error|
|`eof`|EOF inside a lexical construct|
|`file`|file-related error|
|`filename`|mangled filename|
|`fixnum`|sufficiently small exact integer expected|
|`immutability`|modifying immutable data|
|`implementation-restriction`|the implementation has insufficient resources though the program is correct|
|`improper-list`|improper lists not supported|
|`input`|input error|
|`invalid-position`|invalid file position|
|`lexical`|lexical syntax error|
|`low-level-error`|problem with the implementation|
|`match`|unsatisfied pattern match|
|`memory`|out of memory|
|`network`|socket or network error|
|`no-infinities`|implementation does not support infinities|
|`no-nans`|implementation does not support NaNs|
|`non-continuable`|continuing from an exception raised by `raise`|
|`nonexistent`|reference to something that does not exist|
|`not-found`|file not found|
|`os`|operating system reported error|
|`output`|output error|
|`protection`|file protection error|
|`range`|violation of start-end conditions|
|`read`|textual error during reading|
|`scheduler`|task scheduler error|
|`security`|security violation|
|`simple`|error created by `error`|
|`state`|invalid state event|
|`syntax`|Scheme syntax error|
|`termination`|thread termination|
|`timeout`|operation timed out|
|`type`|wrong type|
|`uncaught`|uncaught exception|
|`undefined`|getting the value of a variable that has not been defined|
|`unsupported`|unsupported operation|
|`value`|wrong value|
|`version-skew`|mismatched versions of code|

## Standard keys

|Key|Explanation|
|---|---|
|`message`|human-readable description string|
|`irritants`|list of problematic arguments|
|`who`|an object reporting a problem with another object|
|`what`|an object which has a problem|
|`position`|the position in `what` at which the problem occurred|
|`subcondition`|a condition embedded in this one which has more details|
