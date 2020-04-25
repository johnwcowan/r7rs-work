## Syntax

(struct-packer schema)

Returns proc that packs an object (see Schema for allowed object types) into a bytevector.
Schema is an s-expression.

(struct-unpacker schema)

Returns proc that unpacks a bytevector into an object.
Schema is an s-expression.


## Procedures

(make-struct-packer schema)

Returns proc that packs an object (see Schema for allowed object types) into a bytevector.

(struct-unpacker schema)

Returns proc that unpacks a bytevector into an object.

# Schema

(fill size)

Returns nothing, just skips bytes

(array size descriptor)

Returns vector

(struct (name descriptor) ...)

Returns alist or other dictionary

(union (name descriptor) ...)

Returns alist or other dictionary

u8-c128{le,be,}

Returns number

(u8-c128{le,be,} length)

Returns SRFI 160 vector

(string size {ascii,latin-1,utf-8,utf-16,utf-16be,utf-16le}

Returns string or special condition object

replace

Replace an invalid character

wrap

Wrap a bytevector into a condition object if string contains invalid characters.

## Issues

dynamic fields (dependent types)?
