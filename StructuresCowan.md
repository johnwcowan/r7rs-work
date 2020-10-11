## Specification

This library is used to convert between a bytevector (which is byte-for-byte
equivalent to a C object) and a Scheme object.  Conversion is done using a
*schema*, which is an S-expression that specifies the mapping.



## Procedures

`(make-struct-packer `*schema*`)`

Returns a procedure that packs an object
(see [Schema](#Schema) for allowed object types)
into a bytevector.
Schema is a Scheme value.

Raises an error statisfying `*pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-unpacker `*schema*`)`

Returns a procedure that unpacks a bytevector
(see [Schema](#Schema) for allowed object types)
into an object.
Schema is a Scheme value.

Raises an error statisfying `*pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the bytevector being unpacked.
do not match the schema or its components.

`(packing-error? `*obj*`)`  
`(pack-schema-error? `*obj*`)`

Returns `#t` if *obj* is an appropriate condition object
or `#f` otherwise.

## Syntax

`(struct-packer ` <schema>`)`

Returns a procedure that packs an object
(see [Schema](#Schema) for allowed object types)
into a bytevector.
Schema is a constant S-expression.

Raises an error statisfying `*pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the object being unpacked or any of its components
do not match the schema or its components.

`(struct-unpacker ` <schema>`)`

Returns a procedure that unpacks a bytevector
(see [Schema](#Schema) for allowed object types)
into an object.
Schema is a constant S-expression.

Raises an error statisfying `*pack-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `pack-error?`
if the bytevector being unpacked.
does not match the schema or its components.

## Schema

This is a recursive definition of a schema.
Schemas can be created by quotation or quasiquotation.

`(constant `*bytevector*`)`

Matches nothing on the Scheme object side
with the bytes of *bytevector* on the bytevector side.

`(fill `*size*`)`

Matches nothing on the Scheme object side
and *size* arbitrary bytes on the bytevector side.

```
u8 s8
u16 u16-be u16-le
u16 u16-be u16-le
u32 u32-be u32-le
u32 u32-be u32-le
u64 u64-be u64-le
u64 u64-be u64-le
f32 f32-be f32-le
f64 f64-be f64-le
c64 c64-be c64-le
c128 c128-be c128-le
```

Matches a single number of appropriate type, range, and endianism.

`(array `*length schema*`)`

Matches a Scheme vector and a C array.

`(u8 `*length*`)`
`...`
`(c128-le `*length*`)`

Matches a [SRFI 160](http://srfi.schemers.org/srfi-160/srfi-160.html)
vector of appropriate type, range, and endianism.

`(string `*size encoding*`)`

*Packing*: Matches a string of specified size in bytes.
*Unpacking*: Matches a string of specified size in bytes,
or a wrapped-bytevector if the bytes cannot be decoded
using the *encoding* argument, a symbol.

`(struct `*name schema name schema* ...`)`

Matches an alist which maps structure tags to associated objects
and a C structure.

`(union `*name schema name schema* ...`)`

Matches an alist which maps union tags to associated objects
and a C union.

**Issue**: How do we specify which union tag to use when packing?

