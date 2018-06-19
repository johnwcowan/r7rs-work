## Conditions

This is a WG2 proposal for condition objects.  A condition object encapsulates information about an
exceptional situation.  Typically the rest of the system is notified about a condition using the `raise` and `raise-continuable` procedures and their relatives.  Conditions are logically independent of the exception system, however:  conditions may be used for any purpose, and any object may be passed to the exception system.

The design of this condition system attempts to assume as little as possible about any existing implementation-specific condition system.  In particular, there is no specified relationship between conditions and records, as there is in R6RS, nor is there any notion of subtyping required by the system.  There are condition types for convenience in dispatching, but they are just symbols and in general entail nothing about what information is encapsulated, as different implementations will provide different kinds of information when creating an implementation-specified condition.

Within the above constraints, I have attempted to make the names as compatible as possible with [the R6RS condition system](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-8.html#node_sec_7.2) and its predecessors [SRFI 35](http://srfi.schemers.org/srfi-35/srfi-35.html) and [SRFI 36](http://srfi.schemers.org/srfi-36/srfi-36.html), and with [SRFI 12](http://srfi.schemers.org/srfi-12/srfi-12.html).

A condition is said to be "belong to type *sym*" if (a) it was created by a call to `make-condition` that was passed the symbol `sym` as one of its condition types, or (b) it belongs to an implementation-defined set (possibly empty) of conditions of type *sym*.  This allows implementation-dependent condition objects to participate in this condition system.

## Constructors

`(make-condition `*symlist* ( *sym* *obj* ) ...`)`

Returns a newly allocated condition which belongs to the types whose names are given in `symlist`.  The remainder of the arguments are alternating property names (which are symbols) and values (which can be any object) that specify the information encapsulated by this condition.  It is an error if the value associated with the property name `message` (if it exists) is not a string; it is also an error if the value associated with the property name `irritants` (if it exists) is not a list.

`(alist->condition `*symlist alist*`)`

Returns a newly allocated condition which belongs to the types whose names are given in `symlist`.  Its properties and their values are specified by *alist*.

## Predicates

`(condition? `*obj*`)`

Returns `#t` if *obj* is a condition of any type, and `#f` otherwise.

`(condition-of-type? `*obj* *symlist*`)`

Returns `#t` if *obj* is a condition belonging to any of the types specified by *symlist*, and `#f` otherwise.

`(<type>-error? `*obj*`)`

Returns `#t` if *obj* is a condition belonging to type `<type>` from the list below, and `#f` otherwise.

## Accessors

`(condition-types `*condition*`)`

Returns the list of types to which *condition* belongs.  It is an error to attempt to mutate this list.

`(condition-properties `*condition*`)`

Returns the list of property names associated with this condition.  It is an error to attempt to mutate this list.

`(condition-ref `*condition sym* \[ *default* ] `)`

Returns the property value associated with the property named *sym* of *condition*.  If it has no such property, returns *default*.  If *default* is not specified, returns `#f`.

`(condition-predicate `*symlist*`)`

Returns a predicate which will return `#t` if applied to a condition belonging to any of the types specified in *symlist*, and `#f` otherwise.

`(condition-accessor `*sym* \[ *default* ]`)`

Returns an accessor which will return the value of *sym* if applied to a condition object and *default* otherwise.  If *default* is not specified, the accessor will return `#f`.

Returns the value of the property named `<property-name>` (from the standard list of property names below) of *condition*, or *default* if it has no such property, or `#f` if no default is specified.

`(condition->alist `*condition*`)`

Return the properties and values of *condition* in the form of an alist.


## Specific predicates

`(error-object? `*obj*`)`

Returns `#t` if *obj* is a condition belonging to type `simple`, and `#f` otherwise.  Such conditions are normally created only by user code.  *Part of the small language, but shown here for completeness.*

`(file-error? `*obj*`)`

Returns `#t` if *obj* is a condition belonging to type `file`, and `#f` otherwise.  Such conditions may be created by the implementation if there is an error related to file operations; in particular, the inability to open a file for input.  *Part of the small language, but shown here for completeness.*

`(read-error? `*obj*`)`

Returns `#t` if *obj* is a condition belonging to type `read`, and `#f` otherwise.  Such conditions may be created by the implementation if there is an error related to `read`, such as a lexical syntax error in the input.  *Part of the small language, but shown here for completeness.*

`(syntax-error? `*obj*`)`

Returns `#t` if *obj* is a condition belonging to type `syntax`, and `#f` otherwise.  Such conditions may be created by the implementation if program code is syntactically ill-formed.  When such a condition is raised, it may or may not be possible for the exception system to catch it.  *Part of the small language, but shown here for completeness.*

`(implementation-restriction? `*obj*`)`

Returns `#t` if *obj* is a condition belonging to type `implementation-restriction`, and `#f` otherwise.  Such conditions may be created by the implementation if one of its restrictions is exceeded, such as consuming too much memory or trying to compute an exact number too large to represent.

## Standard condition types

The following condition types are standardized.  Conditions of each type may be created by the implementation in the specified situations as well as any analogous situations.  The only constraint is that the implementation must not raise a condition of a specified type unless that type of external situation is in fact present.  The list is intended to be comprehensive but not complete: it draws on R6RS, Java, and other sources.

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

## Standard property names

|Property|Explanation|
|---|---|
|`message`|human-readable description string|
|`irritants`|list of problematic arguments|
|`who`|an object reporting a problem with another object|
|`what`|an object which has a problem|
|`position`|the position in `what` at which the problem occurred|
|`subcondition`|a condition embedded in this one which has more details|
