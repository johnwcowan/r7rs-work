## Abstract

This library supports the Twinjo serialization format.

## Procedures

`(twinjo-read-text `*proc* [*port*])  
`(twinjo-read-binary `*proc* [*port*])

Reads an Twinjo Text or Binary value from *port*, by default `(current-input-port)`.
If the external representation of an object is read whose type is unknown,
*proc* is called with three arguments:

 * a symbol representing a non-hex tag in Twinjo Text,
   or `#f` otherwise
 * a number representing a hex tag in Twinjo Text
   or Twinjo Binary,
   or #f in Twinjo Text if the tag is non-hex.
 * a number, string, symbol, bytevector, list,
   or `#f` if the code/tag is stand-alone
   
The value returned by *proc* is inserted
into the result in place of the unknown representation.
Alternatively, *proc* can signal an error satisfying `twinjo-error?`.

`(twinjo-write-text `*proc* [*port*])  
`(twinjo-write-binary `*proc* [*port*])

Writes an Twinjo Text or Binary value to *port*,
by default to `(current-output-port)`.
If an object is to be written whose representation is unknown,
*proc* is called with the object, and is expected to return three values:

 * a symbol representing a non-hex tag
   or `#f` if there is none
 * a number representing a Twinjo Text hex tag
   or Twinjo Binary type code,
   or `#f` if there is none
 * a number, string, symbol, bytevector, list,
   or `#f` if the code/tag is stand-alone
   
The values returned by *proc* are used to format
the representation of the object.
Alternatively, *proc* can signal an error satisfying `twinjo-error?`.

`(twinjo-error `*message irritant ...*`)`

Constructs and raises an exception that satisfies `twinjo-error?`
and allows access to *message* (a string) and *irritants*.

`(twinjo-error? `*obj*`)`

Returns `#t` if *obj* is a condition object created by
`twinjo-error` or any other implementation-specified object.

`(twinjo-message *twinjo-error*`)

Returns the message associated with *twinjo-error*.

`(twinjo-irritants *twinjo-error*`)

Returns the list of irritants associated with *twinjo-error*.

`max-byte-object`  
`max-compound-object`  
`max-nesting-depth`

These parameters are set to constraint the sizes of
objects read by `twinjo-read-text` and `twinjo-read-binary`.
It is an error if their values are not exact non-negative integers.
If the length of a byte object exceeds `(max-byte-object)`,
or a compound object is found to have more than
`(max-compound-object)` subobjects,
or an object is nested more than `(max-nesting-depth)`,
an error satisfying `twinjo-error` is signaled.
The initial values of these parameters are implementation-specified.
Note that there is no value meaning "unlimited".
