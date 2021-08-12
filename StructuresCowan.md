## Abstract

This library is used to convert between a bytevector (which is byte-for-byte
equivalent to a C object) and a Scheme object.  Conversion is done using a
*schema*, which is an S-expression that specifies the mapping.

## Issues

 1. How do we represent C unions in the schema?
    

## Procedures

`(make-struct-packer `*schema*`)`

Returns a procedure that takes an object
(see [Schema](#Schema) for permitted objects)
and packs the object into a newly allocated bytevector,
which is returned.

Raises an error statisfying `pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-packer! `*schema*`)`

Returns a procedure that takes an object
(see [Schema](#Schema) for permitted object types)
a bytevector, and an optional offset (default is 0),
and packs the object into the bytevector
starting at the offset, returning the number of bytes packed.

Raises an error statisfying `pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-writer `*schema*`)`

Returns a procedure that takes an object
(see [Schema](#Schema) for allowed object types)
and a binary output port,
and packs the object onto the port.

Raises an error satisfying `pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-unpacker `*schema*`)`

Returns a procedure that takes a bytevector
and an optional offset (default is 0),
and unpacks the bytevector
starting at the offset.
The returned value is the object.

Raises an error statisfying `pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-reader `*schema*`)`

Returns a procedure that takes a binary input port,
reads the appropriate number of bytes,
and unpacks them into an object, which is returned.

Raises an error statisfying `pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(packing-error? `*obj*`)`  
`(pack-schema-error? `*obj*`)`

Returns `#t` if *obj* is an appropriate condition object
or `#f` otherwise.

## Syntax

`(struct-packer ` *schema*`)`  
`(struct-packer! ` *schema*`)`  
`(struct-writer ` *schema*`)`  
`(struct-unpacker ` *schema*`)`  
`(struct-reader ` *schema*`)`

These macros are equivalent to the corresponding procedures
beginning with `make-`, except that it is an error unless
the schemas are literals or quasiquoted literals.
They are provided so that
an implementation can compile a constant schema into appropriate code.
However, they may also expand directly into procedures. 

## Schema

This is a recursive definition of a schema.
Schemas can be created by quotation or quasiquotation.

`(constant `*bytevector-or-string*`)`

Matches nothing on the Scheme object side
with the bytes of *bytevector-or-string* on the bytevector side.
If a string is provided, it is an error unless it is ASCII-only
and is equivalent to the corresponding bytevector.

`(filler `*size*`)`

Matches nothing on the Scheme object side
and *size* arbitrary bytes on the bytevector side.

`u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c64 c128` (native order)  
`      u16-be s16-be u32-be s32-be u64-be s64-be f32-be f64-be c64-be c128-be` (big-endian order)  
`      u16-le s16-le u32-le s32-le u64-le s64-le f32-le f64-le c64-le c128-le` (little-endian order)

Matches a single number of appropriate type, range, precision, and endianness.

`(array `*length schema*`)`

Matches a Scheme vector and a C array.

`(u8 `*length*`)`  
`...`  
`(c128 `*length*`)`

Matches a [SRFI 4](https://srfi.schemers.org/srfi-4/srfi-4.html)
or [SRFI 160](https://srfi.schemers.org/srfi-160/srfi-160.html)
vector of appropriate type, range, and precision.
If a particular homogeneous vector type is not available,
matches a heterogeneous vector instead.

`(string `*size encoding*`)`

*Packing*: Matches a string of specified size in bytes.
*Unpacking*: Matches a string of specified size in bytes,
or a bytevector if the bytes cannot be decoded into a string
using the *encoding* argument, a symbol.

The following encodings are standard:
`ascii`, `latin-1`, `utf-8`, `utf-16`, `utf-16le`, utf-16be`.
Other encodings may be provided by the implementation.

`(struct `*schema schema* ...`)`

Matches a heterogeneous list to a C `struct`.'  Note that
the C names of the fields are not represented here,
though they can be carried along by `label` schemas.

`(label `*symbol schema*`)`

Matches whatever *schema* matches, but provides a label
which can be used for purposes outside the scope of this SRFI.



