## Abstract

This library is used to convert between a bytevector (which is byte-for-byte
equivalent to a C object) and a Scheme object.  Conversion is done
all at once using a
*schema*, which is an S-expression that specifies the mapping.
If the schema specifies C unions, it is also necessary to specify a list
of tags (symbols) representing which member of every union is to be used.

## Issues

None at present.    

## Procedures

`(struct-schema? `*obj*`)` => *boolean*

Returns `#t` if *obj* is a valid schema
and `#f` otherwise.

`(struct-schema-length *schema*`)` => *exact integer*

Returns the number of bytes described by *schema*
as an exact integer.

## Packing procedures

`(make-struct-packer `*schema*`)` => `(`*proc obj* [*taglist*]`)` => *bytevector*

Returns a procedure *proc* that takes *obj*
(see [Schema](#Schema) for permitted objects)
and packs it into a newly allocated bytevector,
which is returned.

Raises an error statisfying `struct-schema-error?`
if *schema* is uninterpretable.

The procedure *proc*
raises an error satisfying `struct-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-packer! `*schema*`)` => `(`*proc! obj bytevector* [*offset*] [*taglist*]`)` => *unspecified*

Returns a procedure that takes an object
(see [Schema](#Schema) for permitted object types)
a bytevector, and an optional offset (default is 0),
and packs the object into the bytevector
starting at the offset.
Returns an unspecified value.

Raises an error statisfying `struct-schema-error?`
if *schema* is uninterpretable.

The returned procedure *proc!*
raises an error satisfying `struct-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-writer `*schema*`)` => `(`*proc obj binary-output-port* [*taglist*]`)` => *unspecified*

Returns a procedure that takes an object
(see [Schema](#Schema) for allowed object types)
and a binary output port,
and packs the object onto the port.
Returns an unspecified value.

Raises an error satisfying `struct-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `struct-error?`
if the object being packed
or any of its components
do not match the schema or its components.

## Unpacking procedures

`(make-struct-unpacker `*schema*`)` => `(`*proc* *bytevector [*offset*] [*taglist*]`)` => *obj*

Returns a procedure that takes a bytevector
and an optional offset (default is 0),
and unpacks the bytevector
starting at the offset.
The returned value is the unpacked object.

Raises an error statisfying `struct-schema-error?`
if *schema* is uninterpretable.

The returned procedure *proc*
raises an error satisfying `struct-error?`
if the object being packed
or any of its components
do not match the schema or its components.

`(make-struct-reader `*schema*`)` => `(`*port* [*taglist*]`)` => *obj*

Returns a procedure that takes a binary input port,
reads the appropriate number of bytes,
and unpacks them into an object, which is returned.

Raises an error statisfying `struct-schema-error?`
if *schema* is uninterpretable.

The returned procedure
raises an error satisfying `struct-error?`
if the object being packed
or any of its components
do not match the schema or its components.

### Exceptions

`(struct-error? `*obj*`)` => *boolean*  
`(struct-schema-error? `*obj*`)` => *boolean*

Returns `#t` if *obj* is an appropriate condition object
as described above or `#f` otherwise.

`(struct-union-exception? `*obj*`)` => *boolean*  
`(struct-union-tag `*struct-union-exception*`)` => *object*  
`(struct-union-schemas `*struct-union-exception*`)` => *list*

See below for the `tagged-union` schema pattern.

## Syntax

`(struct-packer ` *schema*`)`  
`(struct-packer! ` *schema*`)`  
`(struct-writer ` *schema*`)`  
`(struct-unpacker ` *schema*`)`  
`(struct-reader ` *schema*`)`

These macros are equivalent to the corresponding procedures
beginning with `make-`, except that it is an error if
the schemas are not literals.  They are provided so that
an implementation can compile a constant schema into appropriate code.
However, they may also expand directly into their `make-` equivalents. 

## Schema

This is a recursive definition of a schema.
Schemas can be created by quotation or quasiquotation.

`(constant `*bytevector-or-string*`)`

Matches nothing on the Scheme object side
with the bytes of *bytevector-or-string* on the bytevector side.
If a string is provided, it is an error unless it is ASCII-only
and is equivalent to the corresponding bytevector.

`(pad `*size*`)`

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

`(struct `*schema* ...`)`

Matches a heterogeneous list to a C `struct`.'  Note that
the C names of the fields are not represented here,
though they can be carried along by `label` schemas.

`(union (`*tag schema*`)` ...`)`

Equivalent to one of the *schemas* as specified by the *taglist*.
The tag of the first union in depth-first order
corresponds to the first tag in
the list, and so on until both the tags and the unions are exhausted.

For example, the schema
```
(union (a i32)
       (b (struct
            i32
            (union
               (c i32)
               (d f32)))))
```

It is an error unless all the schemas return the same
value to `schema-length`.

`(label `*tag schema*`)`

Matches whatever *schema* matches, but provides a tag
which can be used for purposes outside the scope of this SRFI.
