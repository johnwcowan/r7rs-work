## Procedures

(make-struct-packer schema)

returns proc that packs an object (see Schema for allowed object types) into a bytevector

(make-struct-unpacker schema)

returns proc that unpacks a bytevector into an object.

# Schema

(array size descriptor)

returns vector

(struct (name descriptor) ...)

returns alist or other dictionary

(union (name descriptor) ...)

returns alist or other dictionary

u8-c128{le,be,}

returns number

(u8-c128{le,be,} length)

returns SRFI 160 vector

(string size {ascii,latin-1,utf-8,utf-16,utf-16be,utf-16le}

returns string or special condition object

replace

replace an invalid character

wrap

wrap a bytevector into a condition object if string contains invalid characters.

## Issues

dynamic fields (dependent types)?
