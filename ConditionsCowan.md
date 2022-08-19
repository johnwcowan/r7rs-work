## Conditions

A condition object encapsulates information about an exceptional situation.
Typically the rest of the system is notified about a condition
using the `raise` and `raise-continuable` procedures and their relatives.
Conditions are logically independent of the exception system, however:
conditions may be used for any purpose,
and any object may be passed to the exception system.

The design of this condition system attempts to assume as little as possible
about any existing implementation-specific condition system.
In particular, there is no specified relationship between conditions and records,
as there is in R6RS, nor is there any notion of subtyping required by the system.
There are condition types for convenience in dispatching,
but they are just symbols and in general entail nothing
about what information is encapsulated,
as different implementations will provide different kinds of information
when creating an implementation-specified condition.

However, it is designed to interoperate smoothly with condition objects that are\
either R6RS simple conditions, R6RS compound conditions, or SRFI 222
[compound objects](https://srfi.schemers.org/srfi-222/srfi-222.html)
in such a way that the API defined in this
SRFI will behave correctly when applied to any of these conditions.
For that reason, there are no constructors for conditions as such;
construct a compound object instead.

A *native condition object* is a Scheme object that any underlying non-R6RS condition system
can create.

## Predicates

`(condition? `*obj*`)`

Returns `#t` if *obj* is an R6RS simple or compound condition,
a compound object, or a native condition object
created by the native condition system, and `#f` otherwise.

`(condition-of-type? `*obj* *sym*`)`

Returns what `(memq `*sym* `(condition-types `*obj*`))` returns
(see below).

## Accessors

`(condition-types `*obj*`)`

Returns the list of types to which *condition* belongs.
It is an error to attempt to mutate this list,
which is constructed as follows:

* If *obj* is an R6RS simple condition, then
  the list of rtd objects starting with
  `(record-rtd `*obj*`)` and found by repeated
  application of `rtd-parent` is constructed, and
  then the result of mapping a procedure returning
  the record name as a symbol (but without the initial `&`)
  over the list is returned.
* If *obj* is an R6RS compound condition,
  then the concatenation of the results of
  mapping `condition-types` over the value of
  `(simple-conditions `*obj*`)` is returned.
* If *obj* is a non-R6RS native condition object,
  returns an implementation-specified type name,
  a symbol.
* Otherwise, returns what
  `(filter symbol? (compound-subobjects `*obj*`)` returns.

`(condition-properties `*condition sym*`)`

If *obj* is a compound object, returns what
`(make-compound-type-properties `*sym obj*`)` returns.

If *obj* is a native condition object,
returns an implementation-specific alist.

## R7RS-small interoperation

`(error-object? `*obj*`)`

`(file-error? `*obj*`)`

`(read-error? `*obj*`)`

The above R7RS-small predicates are extended to return `#t` on a compound object
that would return true to `(condition-of-type `*obj sym*`)`,
where *sym* is `simple`, `file`, or `read` respectively.

`(error-object-message `*error*`)`

* If *error* is an R6RS condition of type `&message`,
  the R6RS procedure `condition-message` is invoked and its value returned.
  Otherwise, `#f` is returned.
  
* If *error* is a native condition object with an associated message,
  the message is returned.

* Otherwise, the subobjects of *error* are examined, and 
  any pairs whose car is `message`
  and whose cdr is a string are merged into the result in an
  implementation-dependent way.

`(error-object-irritants `*error*`)`

* If *error* is an R6RS condition of type `&irritants`,
  the R6RS procedure `condition-irritants` is invoked and its value returned.
  Otherwise, `#f` is returned.

* If *error* is a native condition object with an associated list of irritants,
  the list is returned.
  
* If *error* is a compound object, its subobjects are examined, and 
  any pairs whose car is `irritants`
  and whose cdr is a list are merged into the result in an
  implementation-dependent way.

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
|`assertion-violation`|error created by `assert`|
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
|`error`|general error|`
|`eof`|EOF inside a lexical construct|
|`file`|file-related error|
|`fixnum`|sufficiently small exact integer expected|
|`immutability`|modifying immutable data|
|`implementation-restriction`|the implementation has insufficient resources though the program is correct|
|`improper-list`|improper lists not supported|
|`invalid-position`|invalid file position|
|`lexical`|lexical syntax violation|
|`i/o`|general I/O error|
|`i/o-file-already-exists`|file already exists|
|`i/o-file-does-not-exist`|file already exists|
|`i/o-file-is-read-only`|file is not writable|
|`i/o-file-protection`|file protection error|
|`i/o-filename`|mangled filename|
|`i/o-invalid-position`|the position in a port at which the problem occurred|
|`i/o-port`|error has associated port|
|`i/o-read`|input error|
|`i/o-write`|input error|
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
|`range`|violation of start-end conditions|
|`read`|textual error during reading|
|`scheduler`|task scheduler error|
|`security`|security violation|
|`serious`|a serious condition|
|`simple`|error created by `error`|
|`state`|invalid state event|
|`syntax`|syntax error|`
|`termination`|thread termination|
|`timeout`|operation timed out|
|`type`|wrong type|
|`uncaught`|uncaught exception|
|`undefined`|getting the value of a variable that has not been defined|
|`unsupported`|unsupported operation|
|`value`|wrong value|
|`version-skew`|mismatched versions of code|
|`violation`|violation of a standard|
|`warning`|warning|
|`who`|error containing the name of the invoking procedure|
